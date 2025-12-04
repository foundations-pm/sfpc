#------------------------------------------------#
#
# FAMILY SAFEGUARDING: STEPPED WEDGE ANALYSES ----
#
#------------------------------------------------#

# ANALYSIS: SAMPLE DESCRIPTIVES ----

r_directory = 'C:/Users/PerrineMachuel/'

# Libraries 
{ source(paste0(r_directory, "Documents/SFPC/FS/config.R")) }

# Functions 
{ source(paste0(r_directory, "Documents/SFPC/FS/functions.R"))}

# Set-up  ----
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

## Main sample -----

### Load data --------------------------------------------------------------------

# Main sample: children referred to CSC during trial period
setwd(data_path)

data = readRDS(file = paste0(
  data_path, "/primary_outcome_sample_analytical_dataset_final.Rds")) 

### Descriptives -------------------------------------------------

#### Baseline characteristics ----
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
  dplyr::select(-count, -freq)

la_covariate_desc_table = la_covariate_desc_table %>%
  tidyr::pivot_wider(
    names_from = 'local_authority',values_from = count_percent) %>%
  dplyr::mutate(
    across(
      .cols = c('walsall', 'lancashire', 'telford', 'wandsworth', 'swindon'),
      .fns = ~ ifelse(is.na(.), 'None - not applicable', .))) %>%
  dplyr::select(
    covariate, levels, 
    walsall, lancashire, telford, wandsworth, swindon)

trial_period_covariate_desc_table =  desc_data %>% 
  dplyr::group_by(local_authority, wedge) %>%
  describe(class = 'categorical', group = c('local_authority', 'wedge')) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    #count = ifelse(count < 5, '[z]', count),
    count_percent = paste0(count,' (', freq*100, '%)')) %>%
  dplyr::select(-count, -freq) 

trial_period_covariate_desc_table =  trial_period_covariate_desc_table %>% 
  tidyr::pivot_wider(
    names_from = 'local_authority', values_from = count_percent) %>%
  dplyr::arrange(wedge) %>%
  dplyr::mutate(
    across(
      .cols = c('walsall', 'lancashire', 'telford', 'wandsworth', 'swindon'),
      .fns = ~ ifelse(is.na(.), 'None - not applicable', .))) %>%
  dplyr::select(
    wedge, covariate, levels, 
    walsall, lancashire, telford, wandsworth, swindon)

#### Intervention group distribution ------

int_group_distr_table = desc_data %>%
  dplyr::group_by(intervention_group, local_authority) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    intervention_group = ifelse(
      intervention_group == 1, 'Intervention condition','Control condition'),
    percent = round(count/sum(count)*100, 1),
    count_and_percent = paste0(count, "(", percent ,"%)")) %>%
  dplyr::select(-count, -percent)

int_group_distr_table = int_group_distr_table %>%
  tidyr::pivot_wider(
    names_from = 'intervention_group', values_from = count_and_percent)

#### Baseline equivalence ------

covariates = c(
  'referral_no_further_action_clean',
  'unborn_flag',
  'age_at_referral_final',
  'gender_final',
  'ethnicity_final',
  'disability_status_clean',
  'number_of_previous_cpp_clean',
  'uasc_clean',
  'prop_white_british')

# create table
baseline_equivalence_int_group <- tableone::CreateTableOne(
  vars = covariates,
  strata = "intervention_group", 
  data = desc_data, test = FALSE)

# extract SMDs
smd_equivalence_table = print(baseline_equivalence_int_group, smd = TRUE) 
# looking like really good baseline equivalence

# create plot
formula = paste0("intervention_group ~ ", paste(covariates, collapse = ' + '))

cobalt::bal.tab(
  as.formula(formula), 
  data = desc_data,
  stats = 'm',
  binary = 'std',
  abs = TRUE)

cobalt::love.plot(
  as.formula(formula), 
  data = desc_data,
  stats = 'm',
  binary = 'std',
  abs = TRUE)

#### Outcome distribution ------

outcome_distr_table = desc_data %>%
  dplyr::group_by(local_authority, cla_status, intervention_group) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::group_by(local_authority, intervention_group) %>%
  dplyr::mutate(
    cla_status = ifelse(
      cla_status == 1, 'Children looked-after within 18 months','Children not looked-after'),
    intervention_group = ifelse(
      intervention_group == 1, 'Intervention','Control'),
    percent = round(count/sum(count)*100, 1),
    count_and_percent = paste0(count, "(", percent ,"%)")) %>%
  dplyr::ungroup() %>%
  dplyr::select(-count, -percent)

outcome_distr_table = outcome_distr_table %>%
  tidyr::pivot_wider(
    names_from = 'cla_status', values_from = count_and_percent) %>%
  dplyr::arrange(local_authority)

#### Participant flow -----

n_by_period = desc_data %>%
  dplyr::group_by(local_authority, wedge) %>%
  dplyr::summarise(N = n()) %>%
  dplyr::mutate(stat = 'N') %>%
  tidyr::pivot_wider(
    names_from = 'wedge', values_from = N) 

ny1_by_period = desc_data %>%
  dplyr::filter(cla_status == 1) %>%
  dplyr::group_by(local_authority, wedge) %>%
  dplyr::summarise(Ny1 = n()) %>%
  dplyr::mutate(stat = 'Ny1') %>%
  tidyr::pivot_wider(
    names_from = 'wedge', values_from = Ny1) 

py1_by_period = desc_data %>%
  dplyr::group_by(local_authority, wedge, cla_status) %>%
  dplyr::summarise(N = n()) %>%
  dplyr::group_by(local_authority, wedge) %>%
  dplyr::mutate(
    stat = 'py1',
    py1 = round(N/sum(N), 3)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(cla_status == 1) %>%
  dplyr::select(-N, -cla_status) %>%
  tidyr::pivot_wider(
    names_from = 'wedge', values_from = py1) 

participant_flow_table = bind_rows(
  n_by_period, ny1_by_period, py1_by_period)

participant_flow_table = participant_flow_table %>%
  dplyr::arrange(local_authority)

### Save outputs ----
setwd(output_path)

write_xlsx(
  la_covariate_desc_table,
  paste0('primary_outcome_main_sample_descriptives_by_la_raw', file_date, '.xlsx'))

write_xlsx(
  trial_period_covariate_desc_table,
  paste0('primary_outcome_main_sample_descriptives_by_trial_period_raw', file_date, '.xlsx'))
