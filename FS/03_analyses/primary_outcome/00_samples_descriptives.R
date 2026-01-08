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
output_path = paste0(sharepoint_path, '/Outputs/Primary analyses/1. Sample descriptives')

# Working directory
wd = paste0(r_directory, "Documents/SFPC/FS/")

# Dates 
date = format(Sys.Date(),"%Y/%m/%d") # date format to save within dataframes

file_date = format(Sys.Date(),"_%Y%b%d") # date format to save files

dir_date = format(Sys.Date(),"%B %Y") # date format to create directories

## 1. Main sample -------------------------------------------------------------------

### Load data --------------------------------------------------

# Main sample: children referred to CSC during trial period
setwd(data_path)

data = readRDS(file = paste0(
  data_path, "/primary_outcome_sample_analytical_dataset_final.Rds")) 

### Set-up ----
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

# Overall description 
overall_desc =  desc_data %>% 
  describe(class = 'categorical') %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    #count = ifelse(`count` < 5, '[z]', `count`),
    count_percent = paste0(`count`,' (', freq*100, '%)')) %>%
  dplyr::select(-`count`, -freq)

overall_desc =  desc_data %>% 
  describe(class = 'numeric') %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    #count = ifelse(`count` < 5, '[z]', `count`),
    count_percent = paste0(`count`,' (', freq*100, '%)')) %>%
  dplyr::select(-`count`, -freq)

### Table 1 -----------------------------------------------------------------------
table_1 =  desc_data %>%
  dplyr::group_by(local_authority, intervention_group) %>%
  dplyr::summarise(count =  n()) %>%
  dplyr::mutate(freq = round(`count`/sum(`count`), 2)) %>%
  dplyr::mutate(
    count = ifelse(`count` < 5, '[z]', `count`),
    count_percent = paste0(`count`,' (', freq*100, '%)')) %>%
  dplyr::ungroup() %>%
  dplyr::select(-`count`, -freq) %>%
  tidyr::pivot_wider(
    names_from = intervention_group, 
    values_from = count_percent
    ) %>% 
  dplyr::rename(
  `Local authority` = local_authority,
  `Control condition (count and percentage within LA)` = `0`,
  `Intervention condition (count and percentage within LA)` = `1`
  )

# Save table 
setwd(paste0(output_path, '/1. Main sample/Report descriptive tables'))
writexl::write_xlsx(table_1, 'table_1.xlsx')

### Table 2 ----
table_2 =  desc_data %>%
  dplyr::group_by(local_authority, intervention_group, cla_status) %>%
  dplyr::summarise(count =  n()) %>%
  tidyr::pivot_wider(
    names_from = cla_status, 
    values_from = `count`
  ) %>% 
  dplyr::mutate(
    `Total N` = sum(`0`, `1`),
    `Percent looked after` = paste0(round(`1`/`Total N`, 3)*100, '%')) %>%
  dplyr::rename(
    `Local authority` = local_authority,
    `Intervention group` = intervention_group,
    `Children looked-after within 18 months  (count)` = `1`,
    `Children not looked after (count)` = `0`
  )

# Save table 
setwd(paste0(output_path, '/1. Main sample/Report descriptive tables'))
writexl::write_xlsx(table_2, 'table_2.xlsx')

### Table 3 ----
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

table_3 = participant_flow_table %>%
  dplyr::arrange(local_authority)

# Save table 
setwd(paste0(output_path, '/1. Main sample/Report descriptive tables'))
writexl::write_xlsx(table_3, 'table_3.xlsx')

### Table 4 ----
desc_data %>% 
  dplyr::group_by(intervention_group) %>%
  dplyr::summarise(n())

desc_data %>% 
  dplyr::group_by(intervention_group, cla_status) %>%
  dplyr::summarise(n())

### Appendix tables ----

#### Table 1.1 baseline cat ----

desc_data %>% 
  dplyr::group_by(local_authority) %>% 
  dplyr::summarise(n())

appendix_tb_1_cat =  desc_data %>% 
  dplyr::group_by(local_authority) %>%
  describe(class = 'categorical', group = c('local_authority')) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    #count = ifelse(`count` < 5, '[z]', `count`),
    count_percent = paste0(`count`,' (', freq*100, '%)')) %>%
  dplyr::select(-`count`, -freq) %>%
  tidyr::pivot_wider(
    names_from = 'local_authority', values_from = 'count_percent') %>%
  dplyr::mutate(
    across(
      .cols = c('walsall', 'lancashire', 'telford', 'wandsworth', 'swindon'),
      .fns = ~ ifelse(is.na(.), 'None - not applicable', .)))   %>%
  dplyr::rename(`Walsall (n = 5,465)` = walsall,
                `Lancashire (n = 10,943)` = lancashire,      
                `Telford & Wrekin (n = 2,828)` = telford,
                `Wandsworth (n = 3,832)` = wandsworth,
                `Swindon (n = 4,338)` = swindon
                )

# Save table 
setwd(paste0(output_path, '/1. Main sample/Report descriptive tables'))
writexl::write_xlsx(appendix_tb_1_cat, 'appendix_table_1_cat.xlsx')

#### Table 1.2 baseline cont ----

# Numeric vars
appendix_tb_1_cont =  desc_data %>% 
  dplyr::select(-age_at_referral_numeric_final) %>%
  dplyr::group_by(local_authority) %>%
  describe(class = 'numeric', group = c('local_authority')) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(count_percent_missing = paste0(number_missing, " (", percent_missing, "%)")) %>%
  dplyr::relocate(c('covariate','local_authority','min', 'max', 'mean', 'sd', 'count_percent_missing')) %>%
  dplyr::select(-cv,-median,-contains('percentile'), -iqr)

# Save table 
setwd(paste0(output_path, '/1. Main sample/Report descriptive tables'))
writexl::write_xlsx(appendix_tb_1_cont, 'appendix_table_1_cont.xlsx')

#### Table 1.3 baseline equivalence ----
covariates = c(
  #'referral_no_further_action_clean',
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
smd_equivalence_table = as.data.frame(smd_equivalence_table)
smd_equivalence_table = tibble::rownames_to_column(
  smd_equivalence_table, "Covariate")
# looking like really good baseline equivalence

# Save table
setwd(paste0(output_path, '/1. Main sample/Report descriptive tables'))
writexl::write_xlsx(
  smd_equivalence_table, 
  'appendix_table_1_baseline_equivalence.xlsx')

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

#### Table 2,3,4,5,6 ----------

desc_data %>% 
  dplyr::group_by(local_authority, wedge) %>% 
  dplyr::summarise(n()) %>%
  View()

appendix_tb_234567 =  desc_data %>% 
  dplyr::group_by(local_authority, wedge) %>%
  describe(class = 'categorical', group = c('local_authority', 'wedge')) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    count = 5*round(`count`/5),
    count = ifelse(`count` < 5, '[z]', `count`),
    count_percent = paste0(`count`,' (', freq*100, '%)')) %>%
  dplyr::select(-`count`, -freq) %>%
  tidyr::pivot_wider(
    names_from = 'local_authority', values_from = 'count_percent') %>%
  dplyr::mutate(
    across(
      .cols = c('walsall', 'lancashire', 'telford', 'wandsworth', 'swindon'),
      .fns = ~ ifelse(is.na(.), 'None - not applicable', .)))   %>%
  dplyr::mutate(
    levels = case_when(
      is.na(levels) ~ 'Missing',
      TRUE ~ levels)
    ) %>%
  dplyr::rename(`Walsall (n = )` = walsall,
                `Lancashire (n = )` = lancashire,      
                `Telford & Wrekin (n = )` = telford,
                `Wandsworth (n = )` = wandsworth,
                `Swindon (n = )` = swindon
  ) %>%
  dplyr::arrange(wedge, covariate)

# Save table 
setwd(paste0(output_path, '/1. Main sample/Report descriptive tables'))
writexl::write_xlsx(appendix_tb_234567, 'appendix_table_234567.xlsx')


## 2. Chidren with risk factors -------------------------------------------------------------------

### Load data --------------------------------------------------

# Alternative sample: children whose parents had a specific risk factor identified at the end of assessment 
# Risk factors = durg or alcohol misuse, mental health issues, or DA/DVA
setwd(data_path)

data = readRDS(
  paste0('primary_outcome_sample_children_with_risk_factors',
         '_1B_2B_3A_3B_3C_4B_dataset.rds'
  )
)

nrow(data)

### Set-up ----
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

# Overall description 
#overall_desc =  desc_data %>% 
#  describe(class = 'categorical') %>%
#  dplyr::ungroup() %>%
#  dplyr::mutate(
#    #count = ifelse(`count` < 5, '[z]', `count`),
#    count_percent = paste0(`count`,' (', freq*100, '%)')) %>%
#  dplyr::select(-`count`, -freq)

#overall_desc =  desc_data %>% 
#  describe(class = 'numeric') %>%
#  dplyr::ungroup() %>%
#  dplyr::mutate(
#    #count = ifelse(`count` < 5, '[z]', `count`),
#    count_percent = paste0(`count`,' (', freq*100, '%)')) %>%
#  dplyr::select(-`count`, -freq)

### Table 1 -----------------------------------------------------------------------
table_1 =  desc_data %>%
  dplyr::group_by(local_authority, intervention_group) %>%
  dplyr::summarise(count =  n()) %>%
  dplyr::mutate(freq = round(`count`/sum(`count`), 2)) %>%
  dplyr::mutate(
    count = ifelse(`count` < 5, '[z]', `count`),
    count_percent = paste0(`count`,' (', freq*100, '%)')) %>%
  dplyr::ungroup() %>%
  dplyr::select(-`count`, -freq) %>%
  tidyr::pivot_wider(
    names_from = intervention_group, 
    values_from = count_percent
  ) %>% 
  dplyr::rename(
    `Local authority` = local_authority,
    `Control condition (count and percentage within LA)` = `0`,
    `Intervention condition (count and percentage within LA)` = `1`
  )

# Save table 
setwd(paste0(output_path, '/2. Alternative samples/Report descriptive tables'))
writexl::write_xlsx(table_1, 'children_with_risk_factors_table_1.xlsx')

### Table 2 ----
table_2 =  desc_data %>%
  dplyr::group_by(local_authority, intervention_group, cla_status) %>%
  dplyr::summarise(count =  n()) %>%
  tidyr::pivot_wider(
    names_from = cla_status, 
    values_from = `count`
  ) %>% 
  dplyr::mutate(
    `Total N` = sum(`0`, `1`),
    `Percent looked after` = paste0(round(`1`/`Total N`, 3)*100, '%')) %>%
  dplyr::rename(
    `Local authority` = local_authority,
    `Intervention group` = intervention_group,
    `Children looked-after within 18 months  (count)` = `1`,
    `Children not looked after (count)` = `0`
  )

# Save table 
setwd(paste0(output_path, '/2. Alternative samples/Report descriptive tables'))
writexl::write_xlsx(table_2, 'children_with_risk_factors_table_2.xlsx')

### Table 3 ----
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

table_3 = participant_flow_table %>%
  dplyr::arrange(local_authority)

# Save table 
setwd(paste0(output_path, '/2. Alternative samples/Report descriptive tables'))
writexl::write_xlsx(table_3, 'children_with_risk_factors_table_3.xlsx')

### Table 4 ----
desc_data %>% 
  dplyr::group_by(intervention_group) %>%
  dplyr::summarise(n())

desc_data %>% 
  dplyr::group_by(intervention_group, cla_status) %>%
  dplyr::summarise(n())


### Appendix tables ----

#### Table 1.1 baseline cat ----

desc_data %>% 
  dplyr::group_by(local_authority) %>% 
  dplyr::summarise(n())

appendix_tb_1_cat =  desc_data %>% 
  dplyr::group_by(local_authority) %>%
  describe(class = 'categorical', group = c('local_authority')) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    #count = ifelse(`count` < 5, '[z]', `count`),
    #count = 5*round(`count`/5),
    count = ifelse(`count` < 5, '[z]', `count`),
    count_percent = paste0(`count`,' (', freq*100, '%)')) %>%
  dplyr::select(-`count`, -freq) %>%
  tidyr::pivot_wider(
    names_from = 'local_authority', values_from = 'count_percent') %>%
  dplyr::mutate(
    across(
      .cols = c('walsall', 'lancashire', 'telford', 'wandsworth', 'swindon'),
      .fns = ~ ifelse(is.na(.), 'None - not applicable', .)))   %>%
  dplyr::rename(`Walsall (n = 5,465)` = walsall,
                `Lancashire (n = 10,943)` = lancashire,      
                `Telford & Wrekin (n = 2,828)` = telford,
                `Wandsworth (n = 3,832)` = wandsworth,
                `Swindon (n = 4,338)` = swindon
  )

# Save table 
setwd(paste0(output_path, '/2. Alternative samples/Report descriptive tables'))
writexl::write_xlsx(appendix_tb_1_cat, 'children_with_risk_factors_appendix_table_1_cat.xlsx')

#### Table 1.2 baseline cont ----

# Numeric vars
appendix_tb_1_cont =  desc_data %>% 
  dplyr::select(-age_at_referral_numeric_final) %>%
  dplyr::group_by(local_authority) %>%
  describe(class = 'numeric', group = c('local_authority')) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(count_percent_missing = paste0(number_missing, " (", percent_missing, "%)")) %>%
  dplyr::relocate(c('covariate','local_authority','min', 'max', 'mean', 'sd', 'count_percent_missing')) %>%
  dplyr::select(-cv,-median,-contains('percentile'), -iqr)

# Save table 
setwd(paste0(output_path, '/2. Alternative samples/Report descriptive tables'))
writexl::write_xlsx(appendix_tb_1_cont, 'children_with_risk_factors_appendix_table_1_cont.xlsx')

#### Table 1.3 baseline equivalence ----
covariates = c(
  #'referral_no_further_action_clean',
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
smd_equivalence_table = as.data.frame(smd_equivalence_table)
smd_equivalence_table = tibble::rownames_to_column(
  smd_equivalence_table, "Covariate")
# looking like really good baseline equivalence

# Save table
setwd(paste0(output_path, '/2. Alternative samples/Report descriptive tables'))
writexl::write_xlsx(
  smd_equivalence_table, 
  'children_with_risk_factors_appendix_table_1_baseline_equivalence.xlsx')

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

#### Table 2,3,4,5,6 ----------

desc_data %>% 
  dplyr::group_by(local_authority, wedge) %>% 
  dplyr::summarise(n()) %>%
  View()

appendix_tb_234567 =  desc_data %>% 
  dplyr::group_by(local_authority, wedge) %>%
  describe(class = 'categorical', group = c('local_authority', 'wedge')) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    count = 5*round(`count`/5),
    count = ifelse(`count` < 5, '[z]', `count`),
    count_percent = paste0(`count`,' (', freq*100, '%)')) %>%
  dplyr::select(-`count`, -freq) %>%
  tidyr::pivot_wider(
    names_from = 'local_authority', values_from = 'count_percent') %>%
  dplyr::mutate(
    across(
      .cols = c('walsall', 'lancashire', 'telford', 'wandsworth', 'swindon'),
      .fns = ~ ifelse(is.na(.), 'None - not applicable', .)))   %>%
  dplyr::mutate(
    levels = case_when(
      is.na(levels) ~ 'Missing',
      TRUE ~ levels)
  ) %>%
  dplyr::rename(`Walsall (n = )` = walsall,
                `Lancashire (n = )` = lancashire,      
                `Telford & Wrekin (n = )` = telford,
                `Wandsworth (n = )` = wandsworth,
                `Swindon (n = )` = swindon
  ) %>%
  dplyr::arrange(wedge, covariate)

# Save table 
setwd(paste0(output_path, '/2. Alternative samples/Report descriptive tables'))
writexl::write_xlsx(appendix_tb_1_cont, 'children_with_risk_factors_appendix_table_234567.xlsx')


## 3. Born children only -------------------------------------------------------------------

### Load data --------------------------------------------------

# Alternative sample: children who were born during 
# their first referral during the trial
setwd(data_path)

data = readRDS(
  paste0('primary_outcome_sample_born_only_children.rds'
  )
)
nrow(data)

### Set-up ----
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

# Overall description 
overall_desc =  desc_data %>% 
  describe(class = 'categorical') %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    #count = ifelse(`count` < 5, '[z]', `count`),
    count_percent = paste0(`count`,' (', freq*100, '%)')) %>%
  dplyr::select(-`count`, -freq)

overall_desc =  desc_data %>% 
  describe(class = 'numeric') %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    #count = ifelse(`count` < 5, '[z]', `count`),
    count_percent = paste0(`count`,' (', freq*100, '%)')) %>%
  dplyr::select(-`count`, -freq)

### Table 1 -----------------------------------------------------------------------
table_1 =  desc_data %>%
  dplyr::group_by(local_authority, intervention_group) %>%
  dplyr::summarise(count =  n()) %>%
  dplyr::mutate(freq = round(`count`/sum(`count`), 2)) %>%
  dplyr::mutate(
    count = ifelse(`count` < 5, '[z]', `count`),
    count_percent = paste0(`count`,' (', freq*100, '%)')) %>%
  dplyr::ungroup() %>%
  dplyr::select(-`count`, -freq) %>%
  tidyr::pivot_wider(
    names_from = intervention_group, 
    values_from = count_percent
  ) %>% 
  dplyr::rename(
    `Local authority` = local_authority,
    `Control condition (count and percentage within LA)` = `0`,
    `Intervention condition (count and percentage within LA)` = `1`
  )

# Save table 
setwd(paste0(output_path, '/2. Alternative samples/Report descriptive tables'))
writexl::write_xlsx(table_1, 'born_only_children_table_1.xlsx')

### Table 2 ----
table_2 =  desc_data %>%
  dplyr::group_by(local_authority, intervention_group, cla_status) %>%
  dplyr::summarise(count =  n()) %>%
  tidyr::pivot_wider(
    names_from = cla_status, 
    values_from = `count`
  ) %>% 
  dplyr::mutate(
    `Total N` = sum(`0`, `1`),
    `Percent looked after` = paste0(round(`1`/`Total N`, 3)*100, '%')) %>%
  dplyr::rename(
    `Local authority` = local_authority,
    `Intervention group` = intervention_group,
    `Children looked-after within 18 months  (count)` = `1`,
    `Children not looked after (count)` = `0`
  )

# Save table 
setwd(paste0(output_path, '/2. Alternative samples/Report descriptive tables'))
writexl::write_xlsx(table_2, 'born_only_children_table_2.xlsx')

### Table 3 ----
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

table_3 = participant_flow_table %>%
  dplyr::arrange(local_authority)

# Save table 
setwd(paste0(output_path, '/2. Alternative samples/Report descriptive tables'))
writexl::write_xlsx(table_3, 'born_only_children_table_3.xlsx')

### Table 4 ----
desc_data %>% 
  dplyr::group_by(intervention_group) %>%
  dplyr::summarise(n())

desc_data %>% 
  dplyr::group_by(intervention_group, cla_status) %>%
  dplyr::summarise(n())

### Appendix tables ----

#### Table 1.1 baseline cat ----

desc_data %>% 
  dplyr::group_by(local_authority) %>% 
  dplyr::summarise(n())

appendix_tb_1_cat =  desc_data %>% 
  dplyr::group_by(local_authority) %>%
  describe(class = 'categorical', group = c('local_authority')) %>%
  dplyr::mutate(
    #count = ifelse(`count` < 5, '[z]', `count`),
    count = 5*round(`count`/5)
    ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    count = ifelse(`count` < 5, '[z]', `count`),
    count_percent = paste0(`count`,' (', freq*100, '%)')
    ) %>%
  dplyr::select(-`count`, -freq) %>%
  tidyr::pivot_wider(
    names_from = 'local_authority', values_from = 'count_percent') %>%
  dplyr::mutate(
    across(
      .cols = c('walsall', 'lancashire', 'telford', 'wandsworth', 'swindon'),
      .fns = ~ ifelse(is.na(.), 'None - not applicable', .)))   %>%
  dplyr::rename(`Walsall (n = 5,465)` = walsall,
                `Lancashire (n = 10,943)` = lancashire,      
                `Telford & Wrekin (n = 2,828)` = telford,
                `Wandsworth (n = 3,832)` = wandsworth,
                `Swindon (n = 4,338)` = swindon
  )


# Save table 
setwd(paste0(output_path, '/2. Alternative samples/Report descriptive tables'))
writexl::write_xlsx(appendix_tb_1_cat, 'born_only_children_appendix_table_1_cat.xlsx')

#### Table 1.2 baseline cont ----

# Numeric vars
appendix_tb_1_cont =  desc_data %>% 
  dplyr::select(-age_at_referral_numeric_final) %>%
  dplyr::group_by(local_authority) %>%
  describe(class = 'numeric', group = c('local_authority')) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(count_percent_missing = paste0(number_missing, " (", percent_missing, "%)")) %>%
  dplyr::relocate(c('covariate','local_authority','min', 'max', 'mean', 'sd', 'count_percent_missing')) %>%
  dplyr::select(-cv,-median,-contains('percentile'), -iqr)

# Save table 
setwd(paste0(output_path, '/2. Alternative samples/Report descriptive tables'))
writexl::write_xlsx(appendix_tb_1_cont, 'born_only_children_appendix_table_1_cont.xlsx')

#### Table 1.3 baseline equivalence ----
covariates = c(
  #'referral_no_further_action_clean',
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
smd_equivalence_table = as.data.frame(smd_equivalence_table)
smd_equivalence_table = tibble::rownames_to_column(
  smd_equivalence_table, "Covariate")
# looking like really good baseline equivalence

# Save table
setwd(paste0(output_path, '/2. Alternative samples/Report descriptive tables'))
writexl::write_xlsx(
  smd_equivalence_table, 
  'born_only_children_appendix_table_1_baseline_equivalence.xlsx')

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

#### Table 2,3,4,5,6 ----------

desc_data %>% 
  dplyr::group_by(local_authority, wedge) %>% 
  dplyr::summarise(n()) %>%
  View()

appendix_tb_234567 =  desc_data %>% 
  dplyr::group_by(local_authority, wedge) %>%
  describe(class = 'categorical', group = c('local_authority', 'wedge')) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    count = 5*round(`count`/5),
    count = ifelse(`count` < 5, '[z]', `count`),
    count_percent = paste0(`count`,' (', freq*100, '%)')) %>%
  dplyr::select(-`count`, -freq) %>%
  tidyr::pivot_wider(
    names_from = 'local_authority', values_from = 'count_percent') %>%
  dplyr::mutate(
    across(
      .cols = c('walsall', 'lancashire', 'telford', 'wandsworth', 'swindon'),
      .fns = ~ ifelse(is.na(.), 'None - not applicable', .)))   %>%
  dplyr::mutate(
    levels = case_when(
      is.na(levels) ~ 'Missing',
      TRUE ~ levels)
  ) %>%
  dplyr::rename(`Walsall (n = )` = walsall,
                `Lancashire (n = )` = lancashire,      
                `Telford & Wrekin (n = )` = telford,
                `Wandsworth (n = )` = wandsworth,
                `Swindon (n = )` = swindon
  ) %>%
  dplyr::arrange(wedge, covariate)

# Save table 
setwd(paste0(output_path, '/2. Alternative samples/Report descriptive tables'))
writexl::write_xlsx(appendix_tb_1_cont, 'born_only_children_appendix_table_234567.xlsx')
