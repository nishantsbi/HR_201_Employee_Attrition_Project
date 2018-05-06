# DATA UNDERSTANDING ----

# Libraries
library(tidyverse)
library(tidyquant)
library(readxl)
library(skimr)
library(GGally)

# Load data
path_train <- "./00_Data/telco_train.xlsx"
path_data_definitions <- "./00_Data/telco_data_definitions.xlsx"

train_raw_tbl <- read_excel(path_train, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

glimpse(train_raw_tbl)
# Feature categories:
# Descriptive (age, gender...)
# Employment features (department, job role)
# Compensation features
# Survey results
# Performance data
# Work-life features (overtime, travel...)
# Training & education
# Time-based features (time in the company, time from last promotion...)

# Exploratory data analysis (EDA) ----

# Step 1: Data summarization (skimr) ----
skim(train_raw_tbl)

# Character type of data
train_raw_tbl %>%
  select_if(is.character) %>%
  glimpse()

train_raw_tbl %>%
  select_if(is.character) %>%
  map(unique)

train_raw_tbl %>%
  select_if(is.character) %>%
  map(~ table(.) %>% prop.table())

# Numeric type of data
train_raw_tbl %>%
  select_if(is.numeric) %>%
  map_df(~ unique(.) %>% length()) %>%
  gather() %>%
  arrange(value) %>%
  filter(value <= 10)

# Step 2: Visual data analysis (GGally) ----