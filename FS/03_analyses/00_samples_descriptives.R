#------------------------------------------------#
#
# FAMILY SAFEGUARDING: STEPPED WEDGE ANALYSES ----
#
#------------------------------------------------#

## ANALYSIS: PRIMARY OUTCOME ANALYTICAL DATASET ----

r_directory = 'C:/Users/PerrineMachuel/'

# Libraries 
{ source(paste0(r_directory, "Documents/SFPC/FS/config.R")) }

# Functions 
{ source(paste0(r_directory, "Documents/SFPC/FS/functions.R"))}

## Set-up  ----
sharepoint_path = paste0(r_directory,'Foundations/High-SFPC-Impact - Family Safeguarding')

# Data and output paths
data_path = paste0(sharepoint_path, '/Datasets/analytical_datasets')
output_path = paste0(sharepoint_path, '/Outputs/Primary analyses/Sample descriptives')

# Working directory
wd = paste0(r_directory, "Documents/SFPC/FS/")

# Dates 
date = format(Sys.Date(),"%Y/%m/%d") # date format to save within dataframes

file_date = format(Sys.Date(),"_%Y%b%d") # date format to save files

dir_date = format(Sys.Date(),"%B %Y") # date format to create directories

# Sample descriptives -----------------------------------------------------------------

## Load data --------------------------------------------------------------------

# Main sample: children referred to CSC during trial period
setwd(data_path)

data = readRDS(file = paste0(
  data_path, "/primary_outcome_sample_analytical_dataset.Rds")) 

## Descriptive table for report -------------------------------------------------

covariates = c(
  'local_authority',
  'wedge',
  'age_at_referral_clean',
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