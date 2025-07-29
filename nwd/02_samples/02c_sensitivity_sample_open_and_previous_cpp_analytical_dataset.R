# Primary outcome GLM analysis for No Wrong Door RCT  ----

# Set up  ----

#Paths
user_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(user_directory,'Foundations/High-SFPC-Impact - ')

# Data and output paths

# where the primary outcome dataset is
data_path = paste0(sharepoint_path, 'QA/outputs/') 

# where to save final output list
output_path = paste0(sharepoint_path, 'QA/outputs/model_outputs/primary_analyses/')

# where to save individual model/output files 
working_folder = paste0(output_path, 'working_folder/')

# where to save individual sensitivity checks / files
#sensitiviy_checks_folder = paste0(output_path, 'sensitivity_analyses/')

# Dates
date = format(Sys.Date(),"%Y/%m/%d") # date format to save within dataframes

file_date = format(Sys.Date(),"_%Y%b%d") # date format to save files

dir_date = format(Sys.Date(),"%B %Y") # date format to create directories

# Set up folders to save output findings 
# in a neat an organised manner 
# Save individual files in a new directory
# Named after the month when the analyses were conducted

if(!dir.exists(file.path(paste0(working_folder, dir_date)))){
  
  dir.create(file.path(working_folder, dir_date)) # create new dir
  paste0("Creating new directory: '", dir_date,"'")# confirm new dir is created
  
} else { 
  
  cat(
    crayon::green(
      crayon::bold(
        paste0("Directory '", dir_date, "' already exists."))))
  
} # confirms dir already exists

# Working directory
wd = paste0(user_directory, "Documents/sfpc/nwd/")

# Libraries 
{ source(paste0(wd, "config.R")) }

# Functions
{ source(paste0(wd, "functions.R"))}

#01 GLM model - Complete case ------------------------------------------------------------------

## Workplan -------------------------------------------------------------------

# GLM for spec 1: fully specified
# GLM for spec 2: simpler spec
# GLM for complete case, missing indicator
# GLM for imputed data 

# Pipeline: 
#1 Fit models, parameters: formula, data
#2 Diagnostics, parameters: formula, model fit 
#3 Tidy and raw tables, parameters: formula, model fit 
#4 save outputs: 
#4.1 append to list: tidy, raw, diagnostics 
#4.2 save individual files: tidy, raw, diagnostics 

## Load data -----------------------------------------------------------------------

# 1 load data 
previous_cpp_data <- readRDS(file = paste0(
  sharepoint_path, 'QA/outputs/',
  'primary_analysis_analytical_dataset_V2.Rds'))

# Remove children with CP plan > 0
previous_cpp_data  = dplyr::filter(
  previous_cpp_data, 
  number_of_previous_child_protection_plans != '0')

open_cpp_data <- readRDS(file = paste0(
  data_path, 'sensitivity_analysis_open_cp_analytical_dataset_V2.Rds'))

# 2 Combine open and previous CP ---------------------------------------------------
open_and_previous_cp_data = dplyr::bind_rows(
  previous_cpp_data,
  open_cpp_data)

# Unique child IDs 
nrow(open_and_previous_cp_data)
length(unique(open_and_previous_cp_data$child_id))

# Keep unique child IDs and collapse rows for duplicate Child IDs
open_and_previous_cp_data = open_and_previous_cp_data %>%
  dplyr::group_by(child_id) %>%
  dplyr::summarise(across(.cols = everything(),
                          .fns = ~ dplyr::first(na.omit(.))))

# 3 Data cleaning -------------------------------------------------------------------

# Keep age at referral OR CP if no referral date
open_and_previous_cp_data = open_and_previous_cp_data %>% 
  dplyr::mutate(
  age_at_referral_or_cp_start = ifelse(
    is.na(referral_date), age_at_cp_start, age_at_referral),
  age_at_referral_or_cp_start_cat = case_when(
    is.na(referral_date) ~ factor(age_at_cp_start_cat), 
    TRUE ~ factor(age_at_referral_cat))) %>%
  dplyr::select(-age_at_cp_start, -age_at_cp_start_cat,
                -age_at_referral, -age_at_referral_cat) %>%
  dplyr::relocate(c(age_at_referral_or_cp_start, age_at_referral_or_cp_start_cat),
                  .after = year_and_month_of_birth_of_the_child)

# 3 Save data -----------------------------------------------------------------------

saveRDS(open_and_previous_cp_data, file = paste0(
  data_path,"sensitivity_analysis_open_and_previous_cp_analytical_dataset_V1.Rds")) 


writexl::write_xlsx(
  open_and_previous_cp_data,
  path = paste0(
    data_path,
    "sensitivity_analysis_open_and_previous_cp_analytical_dataset_V1.xlsx"))

