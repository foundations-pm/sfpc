# Data Cleaning Script for No Wrong Doors RCT DR1 ----

# Paths  ----
user_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(user_directory,'Foundations/High-SFPC-Impact - ')

# Data and output paths
data_path = paste0(sharepoint_path, 'Data/')
output_path = paste0(sharepoint_path, 'QA/')

# Working directory
wd = paste0(user_directory, "Documents/sfpc/nwd/")

# Libraries ----
{ source(paste0(wd, "config.R")) }

# Functions ----
{ source(paste0(wd, "functions.R"))}

# EDA Workplan ----

# 1 variable class + transform to correct one if needed
# 2 descriptives per classes:
# 2.1 categorical = frequency by categories of value, percent 
# 2.2 continuous = minx, max, mean, sd, IQR, nb missing, percent missing
# 2.3 dates = min, max, number missing, percent missing 

# EDA DR1 ----

## 1. Variable class ----

# Check variable class
summary(DR1_data_cleaned)

# few cols to change as numeric
# one col to change as date

# Assigning correct class
DR1_data_cleaned <- mutate(
  DR1_data_cleaned,
  across(.cols = any_of(c(colnames(DR1_data_cleaned)[-c(1:3)])),
         .fns = as.numeric))

## 2. Describe DR1 ----
dr1_desc_tb_cont = describe(DR1_data_cleaned) 
# describe is a bespoke function you can find in the function.R script

# By LA
dr1_desc_tb_cont_la = DR1_data_cleaned %>%
  group_by(local_authority) %>%
  describe(., group = "local_authority")

# By LA and month of return
dr1_desc_tb_cont_la_month = DR1_data_cleaned %>%
  group_by(local_authority, month_return) %>%
  describe(., group = c("local_authority",
                        "month_return"))

## 3. Data cleaning ----
# No data cleaning needed - all good to go 

# EDA DR2 ----

## 1. Variable class ----

# Check variable class
lapply(DR2_cleaned_list, summary)

# Date vars:
# DOB (%Y-%m) 
# Ref: Referral date 
# CPP: CP plan start date
# CLA: Date period of care commenced

# Cat vars: 
# Ref: referral NFA
# gender, ethnicity, 
# disabled status, UASC, FSM, PPE

# Cont vars: 
# nb of previous CP plans

# To discard before analysis:
# child_id, referral_id_or_case_id
# = specific analysis for these 

# Assign correct variable class
DR2_cleaned_list = lapply(
  
  names(DR2_cleaned_list), function(name){
    
    print(paste0('Dataset cleaned: ', name))
    
    date_cols = colnames(DR2_cleaned_list[[name]])[grepl(
      'date', colnames(DR2_cleaned_list[[name]]), fixed=T)]
    
    print(paste0("Date column(s): ",
                 str_flatten(date_cols, collapse = " ")))
    
    # Treat Rochdale separately because of different date format in raw file
    rochdale_nov20 = DR2_cleaned_list[[name]] %>%
      dplyr::filter(
        local_authority == 'rochdale' &
          month_return == 'nov20') %>%
      dplyr::mutate(
        number_of_previous_child_protection_plans = as.numeric(
          number_of_previous_child_protection_plans),
        year_and_month_of_birth_of_the_child = as.Date(
          paste0('01/', year_and_month_of_birth_of_the_child),
          format = "%d/%m/%Y"),
        across(.cols = any_of(date_cols),
               .fns = as.Date))
    
    
    other_dr2 = DR2_cleaned_list[[name]] %>% 
      dplyr::filter(
        local_authority != 'rochdale' |
          month_return != 'nov20') %>%
      dplyr::mutate(
        year_and_month_of_birth_of_the_child = as.Date(
          paste0('01/', year_and_month_of_birth_of_the_child),
          format = "%d/%m/%Y"),
        across(
          .cols = any_of(
            c(date_cols, "number_of_previous_child_protection_plans")),
          .fns = as.numeric)) %>%
      dplyr::mutate(
        across(
          .cols = any_of(date_cols),
          .fns = ~ as.Date(.x, origin = "1899-12-30")))
    
    DR2_cleaned_list[[name]] = dplyr::bind_rows(
      other_dr2, rochdale_nov20)
    
    return(DR2_cleaned_list[[name]])
    
  })

names(DR2_cleaned_list) = c("referrals", "cpp", "cla")

## 2. Describe DR2 ----

var_class = c("numeric", "categorical", "date")
vars_to_exclude = c("child_id", "referral_id_or_case_id")

### Single var description ----

dr2_single_var_description = list()

for(class in var_class){
  
  dr2_single_var_description[[class]] = lapply(
    DR2_cleaned_list, 
    describe, 
    class = class)
}

### Grouped var description ----
# 1 - Group by LA 
dr2_grouped_var_description_1 = list()

for(class in var_class){
  
  group = 'local_authority'
  
  dr2_grouped_var_description_1[[class]] = lapply(
    DR2_cleaned_list, function(data) {
      data %>% 
        dplyr::select(-any_of(vars_to_exclude)) %>%
        dplyr::group_by(across(any_of(group))) %>%
        describe(., class = class, group = group)})
}

# 2- Group by LA and month of return
dr2_grouped_var_description_2 = list()

for(class in var_class){
  
  group = c('local_authority', 'month_return')
  
  dr2_grouped_var_description_2[[class]] = lapply(
    DR2_cleaned_list, function(data) {
      data %>%
        dplyr::select(-any_of(vars_to_exclude)) %>%
        dplyr::group_by(across(any_of(group))) %>%
        describe(., class = class, group = group)})
}

## 3. Data cleaning ----

# No data cleaning needed - all good to go 

# EDA DR3 ----

## 1. Variable class ----
## 2. Describe class ----
## 2. Data cleaning ----