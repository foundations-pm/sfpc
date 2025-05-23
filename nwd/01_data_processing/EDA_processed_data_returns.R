# Data Cleaning Script for No Wrong Doors RCT DR1 ----

# Paths  ----
user_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(user_directory,'Foundations/High-SFPC-Impact - ')

# Data and output paths
data_path = paste0(sharepoint_path, 
                   'QA/pre_processing/', 'pre_processed_data/')

output_path = paste0(sharepoint_path, 'QA/processing/')

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
summary(DR1_streamlined)

# few cols to change as numeric
# one col to change as date

## 2. Describe DR1 ----
dr1_desc_tb_cont = describe(DR1_streamlined) 
dr1_desc_tb_date = describe(DR1_streamlined, class = "date") 
# describe is a bespoke function you can find in the function.R script

# By LA
dr1_desc_tb_cont_la = DR1_streamlined %>%
  group_by(local_authority) %>%
  describe(., group = "local_authority") %>%
  filter(local_authority != 'leicester') %>%
  arrange(covariate)

dr1_desc_tb_date_la = DR1_streamlined %>%
  group_by(local_authority) %>%
  describe(., class = "date",
           group = "local_authority") %>%
  filter(local_authority != 'leicester')

# By LA and month of return
dr1_desc_tb_cont_la_return = DR1_streamlined %>%
  group_by(local_authority, month_return) %>%
  describe(., group = c("local_authority",
                        "month_return")) %>%
  filter(local_authority != 'leicester')

# Save tables 
writexl::write_xlsx(
  dr1_desc_tb_cont_la, 
  path = paste0(
    sharepoint_path,
    "QA/outputs/descriptives/",
    "DR1 distribution/dr1_distribution.xlsx"))

# EDA DR2 ----

# Read data
dr2_path = paste0(data_path, "DR2")
setwd(dr2_path)

DR2_file_paths <- list.files(pattern="*.xlsx",
                             full.names=TRUE)

DR2_pre_proccessed_list = lapply(
  DR2_file_paths, function(.file_paths){
  
    readxl::read_excel(.file_paths) }) # this function can be found in the functions.R script

names(DR2_pre_proccessed_list) = c("cla", "cpp", "referrals")

## 1. Variable class ----

# Check variable class
lapply(DR2_pre_proccessed_list, summary)

## 2. Describe DR2 ----

var_class = c("numeric", "categorical", "date")
vars_to_exclude = c("child_id", "referral_id_or_case_id")

### Sample description ----

# Number unique children 
# Number unique referrals in period 

lapply(
  DR2_pre_proccessed_list, function(data){
    
    print(paste0('Number of unique children: ',
                 length(unique(data[['child_id']]))))
    
    print(
      paste0(
        'Number of unique referrals for CLA/ referrals for CPP/referrals: ',
        length(unique(data[['referral_id_or_case_id']]))))
    
  })

# Derive children in sample
test = DR2_pre_proccessed_list[['referrals']] %>%
  group_by(child_id) %>%
  arrange(referral_date) %>% 
  mutate(referral_number = dense_rank(referral_date)) %>%
  relocate(local_authority, month_return, referral_id_or_case_id,
           child_id, referral_date, referral_number) %>%
  arrange(local_authority, child_id, desc(referral_number))

### Single var description ----

dr2_single_var_description = list()

for(class in var_class){
  
  dr2_single_var_description[[class]] = lapply(
    DR2_pre_proccessed_list, 
    describe, 
    class = class)
}

### Grouped var description ----
# 1 - Group by LA 
dr2_grouped_var_description_1 = list()

for(class in var_class){
  
  group = 'local_authority'
  
  dr2_grouped_var_description_1[[class]] = lapply(
    DR2_pre_proccessed_list, function(data) {
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
    DR2_pre_proccessed_list, function(data) {
      data %>%
        dplyr::select(-any_of(vars_to_exclude)) %>%
        dplyr::group_by(across(any_of(group))) %>%
        describe(., class = class, group = group)})
}


# EDA DR3 ----

# Read data
dr3_path = paste0(data_path, "DR3")
setwd(dr3_path)

DR3_file_paths <- list.files(pattern="*.xlsx",
                             full.names=TRUE)

DR3_pre_proccessed_list = lapply(
  DR3_file_paths, function(.file_paths){
    
    readxl::read_excel(.file_paths) }) # this function can be found in the functions.R script

names(DR3_pre_proccessed_list) = c("care_ep", "cin", "cla", 'neet')

## 1. Variable class ----
lapply(DR3_pre_proccessed_list, summary)

# Date vars:
# Care ep: date_period_of_care_commenced, date_episode_commenced,
# date_episode_ceased, date_period_of_care_ended 
# CIN: child_referral_date, cin_closure_date  
# CLA: date_period_of_care_commenced
# NEET: period_of_care_end_date_date_left_care 

# Rest of data is categorical for all populations
# Need to discard Child ID and referral ID for analyses

# To discard before analysis:
# child_id, referral_id        
# = specific analysis for these 

# Check all date vars are in same format across all pre_proccessed records
lapply(
  DR3_pre_proccessed_list, function(data){ 

    data %>% 
      select(local_authority, 
             any_of(contains('date'))) %>%
      drop_na() %>%
      group_by(local_authority) %>%
      filter(row_number()==1)
        
  })

# Count sum nas in date cols to check date transformation has not changed
lapply(DR3_pre_proccessed_list,
       function(x) sapply(
         x, function(x){ 
           
           x = replace(x, x =="NULL", NA)
           
           sum(is.na(x)) }))

# TEMPORARY CHANGE - waiting for Norfolk data ----
cleaned_data = lapply(
  
  names(DR3_pre_proccessed_list), function(name){
    
    print(paste0('Dataset cleaned: ', name))
    
    date_cols = colnames(DR3_pre_proccessed_list[[name]])[grepl(
      'date', colnames(DR3_pre_proccessed_list[[name]]), fixed=T)]
    
    print(paste0("Date column(s): ",
                 str_flatten(date_cols, collapse = " ")))
    
    data = DR3_pre_proccessed_list[[name]] %>%
      dplyr::mutate(
        across(
          .cols = any_of(date_cols),
          .fns = as.numeric)) %>%
      dplyr::mutate(
        across(
          .cols = any_of(date_cols),
          .fns = ~ as.Date(.x, origin = "1899-12-30"))) 
    
    return(data) })

# Check missing
lapply(cleaned_data,
       function(x) sapply(x, function(x) sum(is.na(x))))

# Make sure the order is the same
DR3_pre_proccessed_list[1:4] = cleaned_data

# END TEMPORARY CHANGE ----

# Change variable class

# Start with care_ep because of variation in LAs in how dates are coded
#date_cols = DR3_pre_proccessed_list[['care_ep']] %>%
#  select(contains('date')) %>%
#  colnames()

#norfolk_care_ep = DR3_pre_proccessed_list[['care_ep']] %>% 
#  dplyr::filter(local_authority == 'norfolk') %>%
#  dplyr::mutate(
#    across(.cols = any_of(date_cols),
#           .fns = as.numeric)) %>%
#  dplyr::mutate(
#    across(.cols = any_of(date_cols),
#           .fns = ~ as.Date(.x, origin = "1899-12-30"))) 

#care_ep_other_las = DR3_pre_proccessed_list[['care_ep']] %>% 
#  dplyr::filter(local_authority != 'norfolk') %>%
#  dplyr::mutate(
#    across(
#      .cols = any_of(date_cols),
#      .fns = ~ as.Date(.x, format = '%Y-%m-%d'))) 

#DR3_pre_proccessed_list[['care_ep']] = bind_rows(
#  care_ep_other_las, norfolk_care_ep)

# Now deal with other populations: cin, cla, neet
#DR3_sans_care_ep = lapply(
  
#  names(DR3_pre_proccessed_list)[-1], function(name){
    
#    print(paste0('Dataset cleaned: ', name))
    
#    date_cols = colnames(DR3_pre_proccessed_list[[name]])[grepl(
#      'date', colnames(DR3_pre_proccessed_list[[name]]), fixed=T)]
    
#    print(paste0("Date column(s): ",
#                 str_flatten(date_cols, collapse = " ")))

#    DR3_sans_care_ep = DR3_pre_proccessed_list[[name]] %>%
#      dplyr::mutate(
#        across(
#          .cols = any_of(date_cols),
#          .fns = as.numeric)) %>%
#      dplyr::mutate(
#        across(
#          .cols = any_of(date_cols),
#          .fns = ~ as.Date(.x, origin = "1899-12-30"))) 

#    return(DR3_sans_care_ep) })

# Make sure the order is the same
#DR3_pre_proccessed_list[2:4] = DR3_sans_care_ep

## 2. Describe class ----
# Assign correct variable class

# In DR3 there are only categorical or date variables
var_class = c("categorical", "date")
vars_to_exclude = c("child_id", "referral_id", "referral_id_or_case_id")

### Single var description ----

dr3_single_var_description = list()

for(class in var_class){
  
  dr3_single_var_description[[class]] = lapply(
    
    DR3_pre_proccessed_list, 
    
    function(data) {
      
      data %>% 
        dplyr::select(-any_of(vars_to_exclude)) %>%
        describe(class = class)
    })
}

### Grouped var description ----
# 1 - Group by LA 
dr3_grouped_var_description_1 = list()

for(class in var_class){
  
  group = 'local_authority'
  
  dr3_grouped_var_description_1[[class]] = lapply(
    DR3_pre_proccessed_list, function(data) {
      data %>% 
        dplyr::select(-any_of(vars_to_exclude)) %>%
        dplyr::group_by(across(any_of(group))) %>%
        describe(., class = class, group = group)})
}

## 3. Data cleaning ----

# SAVE PROCESSED DATA ----

# Save processed data

## DR1 ----
writexl::write_xlsx(
  DR1_streamlined, 
  path = paste0(output_path,
                "processed_data/DR1/",
                "DR1_processed.xlsx"))

## DR2 ----
lapply(names(DR2_pre_proccessed_list), function(name){
  
  writexl::write_xlsx(
    DR2_pre_proccessed_list[[name]],
    path = paste0(output_path,
                  "processed_data/DR2/",
                  "DR2_processed_", name, ".xlsx")) })

## DR3 ----
lapply(names(DR3_pre_proccessed_list), function(name){
  
  writexl::write_xlsx(
    DR3_pre_proccessed_list[[name]],
    path = paste0(output_path,
                  "processed_data/DR3/",
                  "DR3_processed_", name, ".xlsx")) })
