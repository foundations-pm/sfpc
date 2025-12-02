#------------------------------------------------#
#
# FAMILY SAFEGUARDING: STEPPED WEDGE ANALYSES ----
#
#------------------------------------------------#

## ANALYSIS: SAMPLE DESCRIPTIVES ----

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
  data_path, "/primary_outcome_sample_analytical_dataset_final.Rds")) 

## Descriptive table for report -------------------------------------------------

covariates = c(
  'local_authority',
  'wedge',
  'intervention_group',
  'cla_status',
  'referral_no_further_action_clean',
  'unborn_flag',
  'age_at_referral_final',
  'gender_final',
  'ethnicity_final',
  'disability_status_clean',
  'number_of_previous_cpp_clean',
  'uasc_clean',
  'age_at_referral_numeric_final',
  'prop_cyp_eligible_and_claiming_for_fsm_out_of_all_pupils',
  'prop_white_british')

### Main sample: Children Referred to CSC -----

desc_data = data %>%
  dplyr::mutate(
    intervention_group = factor(as.character(intervention_group), levels = c('0', '1')),
    cla_status = factor(as.character(cla_status), levels = c('0', '1'))) %>%
  dplyr::select(covariates)

la_covariate_desc_table =  desc_data %>% 
  dplyr::group_by(local_authority) %>%
  describe(class = 'categorical', group = c('local_authority')) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    #count = ifelse(count < 5, '[z]', count),
    count_percent = paste0(count,' (', freq*100, '%)')) %>%
  dplyr::select(-count, -freq) %>%
  tidyr::pivot_wider(
    names_from = 'local_authority',values_from = count_percent) %>%
  dplyr::mutate(
    across(
      .cols = c('walsall', 'lancashire', 'telford', 'wandsworth', 'swindon'),
      .fns = ~ ifelse(is.na(.), 'None - not applicable', .))) %>%
  dplyr::select(
    covariate, levels,walsall, lancashire, telford, wandsworth, swindon)

trial_period_covariate_desc_table =  desc_data %>% 
  dplyr::group_by(local_authority, wedge) %>%
  describe(class = 'categorical', 
           group = c('local_authority', 'wedge')) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    #count = ifelse(count < 5, '[z]', count),
    count_percent = paste0(count,' (', freq*100, '%)')) %>%
  dplyr::select(-count, -freq) %>%
  tidyr::pivot_wider(names_from = 'local_authority',
                     values_from = count_percent) %>%
  dplyr::arrange(wedge) %>%
  dplyr::mutate(
    across(
      .cols = c('walsall', 'lancashire', 'telford', 'wandsworth', 'swindon'),
      .fns = ~ ifelse(is.na(.), 'None - not applicable', .))) %>%
  dplyr::select(wedge, covariate, levels, 
                walsall, lancashire, telford, wandsworth, swindon)

## Save tables 
setwd(output_path)

write_xlsx(
  la_covariate_desc_table,
  paste0('primary_outcome_main_sample_descriptives_by_la_raw', file_date, '.xlsx'))

write_xlsx(
  trial_period_covariate_desc_table,
  paste0('primary_outcome_main_sample_descriptives_by_trial_period_raw', file_date, '.xlsx'))
