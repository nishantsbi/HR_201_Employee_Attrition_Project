# HOMEWORK 1 ----

# Libraries ----
library(tidyverse)
library(tidyquant)
library(readxl)
library(forcats)
library(stringr)

# Source Scripts ----
source("00_Scripts/assess_attrition.R")

# Data ----
path_train     <- "00_Data/telco_train.xlsx"
train_raw_tbl  <- read_excel(path_train, sheet = 1)

dept_jobrole_tbl <- train_raw_tbl %>%
    select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)

kpi_industry_turnover_pct <- 0.088

# Productivity Cost by Role ----

productivity_cost_by_role_tbl <- read_excel("00_Data/productivity_cost_by_role.xlsx")

# Q1: Which Job Role has the highest total cost of attrition? ----
dept_jobrole_tbl %>%
  count(Department, JobRole, Attrition) %>%
  group_by(Department, JobRole) %>%
  mutate(pct = n/sum(n)) %>%
  filter(Attrition == "Yes") %>%
  left_join(productivity_cost_by_role_tbl, by = c("Department", "JobRole")) %>%
  mutate(cost_of_attrition = calculate_attrition_cost(salary = Salary_Average,
                                                      net_revenue_per_employee = Revenue_Average,
                                                      n = n)) %>%
  arrange(desc(cost_of_attrition)) %>%
  ungroup()%>%
  top_n(1)



# Q2: What is the total cost of attrition for the Research & Development: Research Scientist job role? ----
dept_jobrole_tbl %>%
  count(Department, JobRole, Attrition) %>%
  group_by(Department, JobRole) %>%
  mutate(pct = n/sum(n)) %>%
  filter(Attrition == "Yes") %>%
  left_join(productivity_cost_by_role_tbl, by = c("Department", "JobRole")) %>%
  mutate(cost_of_attrition = calculate_attrition_cost(salary = Salary_Average,
                                                      net_revenue_per_employee = Revenue_Average,
                                                      n = n)) %>%
  filter(JobRole == "Research Scientist") %>%
  select(cost_of_attrition)


# Q3: What percentage do the top four Job Roles account for in terms of the total cost of attrition? ----
# First type of solution
dept_jobrole_tbl %>%
  count(Department, JobRole, Attrition) %>%
  group_by(Department, JobRole) %>%
  mutate(pct = n/sum(n)) %>%
  filter(Attrition == "Yes") %>%
  left_join(productivity_cost_by_role_tbl, by = c("Department", "JobRole")) %>%
  mutate(cost_of_attrition = calculate_attrition_cost(salary = Salary_Average,
                                                      net_revenue_per_employee = Revenue_Average,
                                                      n = n)) %>%
  arrange(desc(cost_of_attrition)) %>%
  ungroup() %>%
  mutate(row_num = row_number()) %>%
  mutate(is_top_4 = case_when(row_num <= 4 ~ "Yes", TRUE ~ "No")) %>%
  group_by(is_top_4) %>%
  summarise(total_attrition_cost = sum(cost_of_attrition)) %>%
  mutate(total_attrition_costs_pct = total_attrition_cost/sum(total_attrition_cost))

# Second type of solution
dept_jobrole_tbl %>%
  count(Department, JobRole, Attrition) %>%
  group_by(Department, JobRole) %>%
  mutate(pct = n/sum(n)) %>%
  filter(Attrition == "Yes") %>%
  left_join(productivity_cost_by_role_tbl, by = c("Department", "JobRole")) %>%
  mutate(cost_of_attrition = calculate_attrition_cost(salary = Salary_Average,
                                                      net_revenue_per_employee = Revenue_Average,
                                                      n = n)) %>%
  arrange(desc(cost_of_attrition)) %>%
  ungroup() %>%
  mutate(cum_attrition = cumsum(cost_of_attrition)) %>%
  mutate(cum_pct = cum_attrition/sum(cost_of_attrition))

# Q4. Which Department has the highest total cost of attrition? ----
dept_jobrole_tbl %>%
  count(Department, JobRole, Attrition) %>%
  group_by(Department, JobRole) %>%
  mutate(pct = n/sum(n)) %>%
  filter(Attrition == "Yes") %>%
  left_join(productivity_cost_by_role_tbl, by = c("Department", "JobRole")) %>%
  mutate(cost_of_attrition = calculate_attrition_cost(salary = Salary_Average,
                                                      net_revenue_per_employee = Revenue_Average,
                                                      n = n)) %>%
  group_by(Department) %>%
  summarise(sum = sum(cost_of_attrition))


# Q5: What percentage does the top Department account for in terms of the total cost of attrition? ----
# First type of solution
dept_jobrole_tbl %>%
  count(Department, JobRole, Attrition) %>%
  group_by(Department, JobRole) %>%
  mutate(pct = n/sum(n)) %>%
  filter(Attrition == "Yes") %>%
  left_join(productivity_cost_by_role_tbl, by = c("Department", "JobRole")) %>%
  mutate(cost_of_attrition = calculate_attrition_cost(salary = Salary_Average,
                                                      net_revenue_per_employee = Revenue_Average,
                                                      n = n)) %>%
  arrange(desc(cost_of_attrition)) %>%
  ungroup() %>%
  group_by(Department) %>%
  summarise(dept_cost_of_attrition = sum(cost_of_attrition)) %>%
  arrange(desc(dept_cost_of_attrition)) %>%
  mutate(row_num = row_number()) %>%
  mutate(is_top_1 = case_when(row_num <= 1 ~ "Yes", TRUE ~ "No")) %>%
  group_by(is_top_1) %>%
  summarise(total_attrition_cost = sum(dept_cost_of_attrition)) %>%
  mutate(total_attrition_costs_pct = total_attrition_cost/sum(total_attrition_cost))

