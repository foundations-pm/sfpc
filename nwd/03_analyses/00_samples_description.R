# Primary outcome GLMER analysis for No Wrong Door RCT ----

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

#00 Cohort descriptives -----------------------------------------------------------------

## Load data --------------------------------------------------------------------

# 1 load data 
main_data <- readRDS(file = paste0(
  data_path,
  'primary_analysis_analytical_dataset_V2.Rds'))

# Open CP sample
alternative_sample_1_data <- readRDS(file = paste0(
  data_path, 'sensitivity_analysis_open_cp_analytical_dataset_V2.Rds'))

# Previous CP sample
alternative_sample_2_data = dplyr::filter(
  main_data, 
  number_of_previous_child_protection_plans != '0')

## Descriptive table for report -------------------------------------------------

covariates = c(
  'local_authority',
  'wedge',
  'age_at_referral_cat',
  'gender',
  'ethnicity_agg',
  'disabled_status',
  'number_of_previous_child_protection_plans',
  'unaccompanied_asylum_seeker')

### Main sample -----

main_data = dplyr::select(main_data, covariates)

covariate_table =  main_data %>% 
  dplyr::group_by(local_authority, wedge) %>%
  describe(class = 'categorical', 
           group = c('local_authority', 'wedge')) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    count = ifelse(count < 5, '[z]', count),
    count_freq = paste0(count,' (', freq, '%)')) %>%
  dplyr::select(-count, -freq) %>%
  tidyr::pivot_wider(names_from = 'local_authority',
                     values_from = count_freq) %>%
  dplyr::arrange(wedge) %>%
  dplyr::mutate(
    across(
      .cols = c('rochdale', 'warrington', 'redcar', 'norfolk'),
      .fns = ~ ifelse(is.na(.), 'None - not applicable', .))) %>%
  dplyr::select(wedge, covariate, levels, 
                rochdale, warrington, norfolk, redcar)

### Open CP sample -----

alternative_sample_1_data = dplyr::select(
  alternative_sample_1_data, any_of(
    c(covariates, 'age_at_cp_start'))) %>%
  dplyr::select(-wedge) %>%
  dplyr::mutate(age_at_cp_start = factor(
    age_at_cp_start, levels = c('12', '13', '14', '15', '16', '17'))) 

covariate_table = alternative_sample_1_data %>% 
  dplyr::group_by(local_authority) %>%
  describe(class = 'categorical', 
           group = c('local_authority')) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    count = ifelse(count < 5, '[z]', count),
    count_freq = paste0(count,' (', freq, '%)')) %>%
  dplyr::select(-count, -freq) %>%
  tidyr::pivot_wider(names_from = 'local_authority',
                     values_from = count_freq) %>%
  dplyr::mutate(
    across(
      .cols = c('rochdale', 'warrington', 'redcar', 'norfolk'),
      .fns = ~ ifelse(is.na(.), 'None - not applicable', .))) %>%
  dplyr::select(covariate, levels, 
                rochdale, warrington, norfolk, redcar) %>%
  dplyr::arrange(covariate)

# Save table 
setwd(paste0(output_path, 'sensitivity_analyses/descriptives'))

write.xlsx(covariate_table,
           'open_cp_sample_covariate_description.xlsx')


### Previous CP sample -----

alternative_sample_2_data = dplyr::select(
  alternative_sample_2_data, any_of(covariates)) %>%
  dplyr::select(-wedge)

covariate_table = alternative_sample_2_data %>% 
  dplyr::group_by(local_authority) %>%
  describe(class = 'categorical', 
           group = c('local_authority')) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    count = ifelse(count < 5, '[z]', count),
    count_freq = paste0(count,' (', freq, '%)')) %>%
  dplyr::select(-count, -freq) %>%
  tidyr::pivot_wider(names_from = 'local_authority',
                     values_from = count_freq) %>%
  dplyr::mutate(
    across(
      .cols = c('rochdale', 'warrington', 'redcar', 'norfolk'),
      .fns = ~ ifelse(is.na(.), 'None - not applicable', .))) %>%
  dplyr::select(covariate, levels, 
                rochdale, warrington, norfolk, redcar) %>%
  dplyr::arrange(covariate)

