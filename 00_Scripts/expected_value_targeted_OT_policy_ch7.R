# EVALUATION: EXPECTED VALUE OF POLICY CHANGE ----
# TARGETED OVERTIME POLICY ----

# 1. Setup ----

# Load Libraries 

library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)


# Load Data
path_train            <- "00_Data/telco_train.xlsx"
path_test             <- "00_Data/telco_test.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl       <- read_excel(path_train, sheet = 1)
test_raw_tbl        <- read_excel(path_test, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

# Processing Pipeline
source("00_Scripts/data_processing_pipeline.R")
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl  <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)

# ML Preprocessing Recipe 
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
    step_zv(all_predictors()) %>%
    step_num2factor(JobLevel, StockOptionLevel) %>%
    prep()

recipe_obj

train_tbl <- bake(recipe_obj, newdata = train_readable_tbl)
test_tbl  <- bake(recipe_obj, newdata = test_readable_tbl)

# 2. Models ----

h2o.init()

# Replace this with your model!!! (or rerun h2o.automl)
automl_leader <- h2o.loadModel("04_Modeling/h2o_models/StackedEnsemble_BestOfFamily_0_AutoML_20180503_035824")

automl_leader


# 3. Primer: Working With Threshold & Rates ----

performance_h2o <- automl_leader %>%
    h2o.performance(newdata = as.h2o(test_tbl))

performance_h2o %>%
    h2o.confusionMatrix()

rates_by_threshold_tbl <- performance_h2o %>%
    h2o.metric() %>%
    as.tibble()

rates_by_threshold_tbl %>% glimpse()

rates_by_threshold_tbl %>%
    select(threshold, f1, tnr:tpr)

rates_by_threshold_tbl %>%
    select(threshold, f1, tnr:tpr) %>%
    filter(f1 == max(f1)) %>%
    slice(1)

rates_by_threshold_tbl %>%
    select(threshold, f1, tnr:tpr) %>%
    gather(key = "key", value = "value", tnr:tpr, factor_key = TRUE) %>%
    mutate(key = fct_reorder2(key, threshold, value)) %>%
    ggplot(aes(threshold, value, color = key)) +
    geom_point() +
    geom_smooth() +
    theme_tq() +
    scale_color_tq() +
    theme(legend.position = "right") +
    labs(
        title = "Expected Rates",
        y = "Value", x = "Threshold"
    )


# 4. Expected Value ----

# 4.1 Calculating Expected Value With OT ----

source("00_Scripts/assess_attrition.R")

predictions_with_OT_tbl <- automl_leader %>%
    h2o.predict(newdata = as.h2o(test_tbl)) %>%
    as.tibble() %>%
    bind_cols(
        test_tbl %>%
            select(EmployeeNumber, MonthlyIncome, OverTime)
    )

predictions_with_OT_tbl


ev_with_OT_tbl <- predictions_with_OT_tbl %>%
    mutate(
        attrition_cost = calculate_attrition_cost(
            n = 1,
            salary = MonthlyIncome * 12,
            net_revenue_per_employee = 250000
        )
    ) %>%
    mutate(
        cost_of_policy_change = 0 
    ) %>%
    mutate(
        expected_attrition_cost = 
            Yes * (attrition_cost + cost_of_policy_change) +
            No *  (cost_of_policy_change)
    )

ev_with_OT_tbl

total_ev_with_OT_tbl <- ev_with_OT_tbl %>%
    summarise(
        total_expected_attrition_cost_0 = sum(expected_attrition_cost)
    )

total_ev_with_OT_tbl



# 4.2 Calculating Expected Value With Targeted OT ----


max_f1_tbl <- rates_by_threshold_tbl %>%
    select(threshold, f1, tnr:tpr) %>%
    filter(f1 == max(f1)) %>%
    slice(1)

max_f1_tbl

tnr <- max_f1_tbl$tnr
fnr <- max_f1_tbl$fnr
fpr <- max_f1_tbl$fpr
tpr <- max_f1_tbl$tpr

threshold <- max_f1_tbl$threshold

test_targeted_OT_tbl <- test_tbl %>%
    add_column(Yes = predictions_with_OT_tbl$Yes) %>%
    mutate(
        OverTime = case_when(
            Yes >= threshold ~ factor("No", levels = levels(test_tbl$OverTime)),
            TRUE ~ OverTime
        )
    ) %>%
    select(-Yes)

test_targeted_OT_tbl

predictions_targeted_OT_tbl <- automl_leader %>%
    h2o.predict(newdata = as.h2o(test_targeted_OT_tbl)) %>%
    as.tibble() %>%
    bind_cols(
        test_tbl %>%
            select(EmployeeNumber, MonthlyIncome, OverTime),
        test_targeted_OT_tbl %>%
            select(OverTime)
    ) %>%
    rename(
        OverTime_0 = OverTime,
        OverTime_1 = OverTime1
    )

predictions_targeted_OT_tbl


avg_overtime_pct <- 0.10

ev_targeted_OT_tbl <- predictions_targeted_OT_tbl %>%
    mutate(
        attrition_cost = calculate_attrition_cost(
            n = 1,
            salary = MonthlyIncome * 12,
            net_revenue_per_employee = 250000
        )
    ) %>%
    mutate(
        cost_of_policy_change = case_when(
            OverTime_0 == "Yes" & OverTime_1 == "No" ~ attrition_cost * avg_overtime_pct,
            TRUE ~ 0
        )
    ) %>%
    mutate(
        cb_tn = cost_of_policy_change,
        cb_fp = cost_of_policy_change,
        cb_tp = cost_of_policy_change + attrition_cost,
        cb_fn = cost_of_policy_change + attrition_cost,
        expected_attrition_cost = 
            Yes * (tpr*cb_tp + fnr*cb_fn) +
            No *  (tnr*cb_tn + fpr*cb_fp)
    ) 

ev_targeted_OT_tbl

total_ev_targeted_OT_tbl <- ev_targeted_OT_tbl %>%
    summarize(
        total_expected_attrition_cost_1 = sum(expected_attrition_cost)
    )

total_ev_targeted_OT_tbl 

# 4.3 Savings Calculation ----

savings_tbl <- bind_cols(
    total_ev_with_OT_tbl,
    total_ev_targeted_OT_tbl
) %>%
    mutate(
        savings = total_expected_attrition_cost_0 - total_expected_attrition_cost_1,
        pct_savings = savings / total_expected_attrition_cost_0
    )

savings_tbl



