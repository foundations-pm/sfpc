#------------------------------------------------#
#
# FAMILY SAFEGUARDING: STEPPED WEDGE ANALYSES ----
#
#------------------------------------------------#

# ANALYSIS: PRIMARY OUTCOME MODEL FITTING ----

r_directory = 'C:/Users/PerrineMachuel/'

# Libraries 
{ source(paste0(r_directory, "Documents/SFPC/FS/config.R")) }

# Functions 
{ source(paste0(r_directory, "Documents/SFPC/FS/functions.R"))}

# Set-up  ----
sharepoint_path = paste0(r_directory,'Foundations/High-SFPC-Impact - Family Safeguarding')

# Data and output paths
data_path = paste0(sharepoint_path, '/Datasets/analytical_datasets')
output_path = paste0(sharepoint_path, '/Outputs/Primary analyses')

# Working directory
wd = paste0(r_directory, "Documents/SFPC/FS/")

# Dates 
date = format(Sys.Date(),"%Y/%m/%d") # date format to save within dataframes

file_date = format(Sys.Date(),"_%Y%b%d") # date format to save files

dir_date = format(Sys.Date(),"%B %Y") # date format to create directories

# Set up folder with %m/%Y to store analyses 
if(!dir.exists(file.path(paste0(output_path, '/', dir_date)))){
  
  dir.create(file.path(paste0(output_path, '/', dir_date))) # create new dir
  paste0("Creating new directory: '", dir_date,"'")# confirm new dir is created
  
} else { 
  
  cat(
    crayon::green(
      crayon::bold(
        paste0("Directory '", dir_date, "' already exists."))))
  
} # confirms dir already exists

## Load data --------------------------------------------------------------------

# Main sample: children referred to CSC during trial period
setwd(data_path)

data = readRDS(file = paste0(
  data_path, "/primary_outcome_sample_analytical_dataset_final.Rds")) 

# Main sample: imputed data (m = 10)
setwd(data_path)

file.info(
  list.files(
    full.names=T))[,1, drop=F]

load(paste0("main_sample_m10_imputation/",
            "main_sample_m10_imputation.Rdata"))

imputed_data_m10 = mi.res 

rm(mi.res)

# Number of iteration used to impute datasets
iteration_number = 10

## S1: Factors identified at the start of the assessment ------------------------

### 0. Formula ----
demographics = paste(
  'age_at_referral_final',
  'gender_final',
  'ethnicity_final',
  'disability_status_clean',
  'number_of_previous_cpp_clean',
  'uasc_clean',
  sep = ' + ')

re = " + (1 | local_authority)"

cluster_indicator = c(
  " + prop_white_british")

formula = paste0( # fully-specified, per protocol
  "cla_status ~ intervention_group + wedge + ", # FE for trt + time effects
  demographics, # adjust for person level demographics
  str_flatten(cluster_indicator), # adjust for time-varying cluster level indicators
  re
) # RE intercept 4 clusters

print(formula)

### 1. Missing indicator analyses ----

#### 1.1 Derive data ----
factor_cols = data %>% dplyr::select(any_of(contains('factors_'))) %>% colnames()

# Create a flag for risk factors (drug/alcohol abuse, DA/DVA, mental health)
s1_data = dplyr::mutate(
  data, 
  risk_factors_identified_post_assessment = case_when(
    if_any(factor_cols, ~ .x == 'Yes') ~ 'Yes',
    TRUE ~ 'No')
  )

# Select children with presence of risk factor only
s1_data = dplyr::filter(
  s1_data, 
  risk_factors_identified_post_assessment == 'Yes')

# Check sample size
nrow(s1_data) # 10,901 children

# Save dataset for descriptives
setwd(data_path)

saveRDS(
  s1_data, 
  'primary_outcome_sample_children_with_risk_factors_1B_2B_3A_3B_3C_4B_dataset.rds'
  )

#### 1.2 Fit model ----

analysis_type = 'Children with risk factors - Missing indicator - GLMER'

# Missing indicator data
# Impute missing cases 
tictoc::tic()

s1_data = dplyr::mutate(
  s1_data,
  across(
    .cols = c('gender_final','ethnicity_final', 'disability_status_clean', 'uasc_clean'),
    .fns = ~ ifelse(is.na(.x), 'Missing', .x)
  )
) 

tictoc::toc()

# Fit mixed model

analysis_type = 'Children with risk factors - Missing indicator - GLMER'

s1_glmer = lme4::glmer(
  as.formula(formula), 
  data = s1_data, 
  family = binomial)

s1_glmer_summary = summary(s1_glmer)

#### 1.3 Diagnostics ----
s1_glmer_ss_table = get_optimisers_warning_messages(
  glmer_model_fit = s1_glmer,
  formula = s1_glmer_formula,
  analysis_type = analysis_type)

# VIF table 
s1_glmer_vif_table = get_vif_table(
  model_fit = s1_glmer,
  formula = s1_glmer_formula,
  analysis_type = analysis_type)

# Performance & fit indicators: AIC, BIC, R2...
s1_glmer_performance_table = get_performance_table(
  s1_glmer,
  formula = s1_glmer_formula,
  analysis_type = analysis_type)

s1_glmer_diagnostics_table = dplyr::left_join(
  s1_glmer_ss_table,
  s1_glmer_performance_table,
  by = c('analysis_type', 'formula', 'date'))

#### 1.4 Tidy -----
# Get raw results: GLMER
s1_glmer_raw = get_raw_estimates(
  summary_model_fit = s1_glmer_summary,
  analysis_type = analysis_type,
  formula = s1_glmer_formula,
  date = date)

# Get tidy results: GLMER
s1_glmer_tidy = get_tidy_estimates(
  model_fit = s1_glmer, 
  analysis_type = analysis_type,
  formula = s1_glmer_formula,
  date = date)

#### 1.5 Save outputs ----

###### List ----
# Working directory to save outputs table 

setwd(output_path)
analysis_type = 'Children with risk factors - Missing indicator - GLMER'

##### Tidy: Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'tidy_output_list.xlsx')

append_results(
  output_file = output_file,
  table_1_to_append = s1_glmer_tidy,
  save_to = 'tidy_output_list.xlsx') 

##### Raw: Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'raw_output_list.xlsx')

append_results(
  output_file = output_file,
  table_1_to_append = s1_glmer_raw,
  save_to = 'raw_output_list.xlsx') 

##### Diagnostics 
# Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'diagnostics_list.xlsx')

append_results(output_file = output_file,
               table_1_to_append = s1_glmer_diagnostics_table,
               table_2_to_append = s1_glmer_vif_table,
               is_multisheet_workbook = TRUE,
               save_to = 'diagnostics_list.xlsx')

###### Individual files ----
# Save/export raw & tidy estimates into excel file & into folder with monthly date
setwd(paste0(output_path, '3. Sensitivity analysis/', dir_date)) # Month folder 

#### Tidy and raw
analysis_type = 'Children with risk factors - Missing indicator - GLMER'

writexl::write_xlsx(
  s1_glmer_raw, 
  paste0(
    "raw_",
    janitor::make_clean_names(analysis_type), # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
    file_date, ".xlsx"))

writexl::write_xlsx(
  s1_glmer_tidy, 
  paste0(
    "tidy_", 
    janitor::make_clean_names(analysis_type), # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
    file_date, ".xlsx"))

#### Diagnostics
wb = openxlsx::createWorkbook()

diagnostics_list = list(
  'complete_case_glmer_performance' = s1_glmer_diagnostics_table,
  'complete_case_glmer_vif' = s1_glmer_vif_table)

# Add tables to different worksheets based on list's name
lapply(names(diagnostics_list), function(name){
  
  openxlsx::addWorksheet(wb, name)
  writeData(wb, name, diagnostics_list[[name]])
  
})

openxlsx::saveWorkbook(
  wb, paste0(
    'diagnostics_cp_0_sample_complete_case_glmer', 
    file_date , '.xlsx'),
  overwrite = TRUE)

### 2. Multiple imputation ----

#### 2.1 Derive data ----
factor_cols = data %>% 
  dplyr::select(any_of(contains('factors_'))) %>% colnames()

data = dplyr::mutate(
  data, 
  risk_factors_identified_post_assessment = case_when(
    if_any(factor_cols, ~ .x == 'Yes') ~ 'Yes',
    TRUE ~ 'No')
)

s1_data = dplyr::filter(
  data, 
  risk_factors_identified_post_assessment == 'Yes')

nrow(s1_data) # 10,901

#### 2.2 Fit model ----

#### 2.3 Diagnostics ----

#### 2.4 Tidy model ----

#### 2.5 Save outputs ----


## S1: without unborn -----------------------------------------------------------

# Check sample characteristics
covariates = c(
  'local_authority',
  'wedge',
  'intervention_group',
  'cla_status',
  'age_at_referral_final',
  'gender_final',
  'ethnicity_final',
  'disabled_status_clean',
  'uasc_clean',
  'number_of_previous_cpp_clean')

# Check NAs
map(s1_data, ~sum(is.na(.)))

# General sample distribution
s1_data_desc = s1_data %>%
  dplyr::select(any_of(covariates)) %>%
  dplyr::mutate(
    intervention_group = as.character(intervention_group),
    cla_status = as.character(cla_status)) 

sample_desc_table = s1_data_desc %>%
  describe(class = 'categorical') 

# Describing sample characteristics by LA
sample_desc_la_table = s1_data_desc %>%
  dplyr::group_by(local_authority) %>%
  describe(class = 'categorical',
           group = 'local_authority')

# Describing characteristics of those who were referred 
# during control vs intervention periods 
sample_desc_exposure_table = s1_data_desc %>%
  dplyr::group_by(intervention_group) %>%
  describe(class = 'categorical',
           group = 'intervention_group') 

# Describing characteristics of those who became looked after within 18 months 
sample_desc_outcome_table = s1_data_desc %>%
  dplyr::select(any_of(covariates)) %>%
  dplyr::mutate(intervention_group = as.character(intervention_group),
                cla_status = as.character(cla_status)) %>%
  dplyr::group_by(cla_status) %>%
  describe(class = 'categorical',
           group = 'cla_status') 

# Outcome distribution by wedge
prop_outcome_by_wedge = s1_data_desc %>%
  dplyr::group_by(wedge, cla_status) %>% 
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(wedge) %>%
  dplyr::mutate(freq = count/sum(count)) 

# Baseline outcome distribution by wedge by LA
prop_outcome_by_la_wedge = s1_data_desc %>%
  dplyr::group_by(local_authority, wedge, cla_status) %>% 
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(local_authority, wedge) %>%
  dplyr::mutate(freq = count/sum(count)) %>%
  dplyr::arrange(cla_status, local_authority, wedge)

sample_desc_list = list(
  'Sample description' = sample_desc_table,
  'Sample description by LA' = sample_desc_la_table,
  'Treatment group' = sample_desc_exposure_table,
  'Outcome group' = sample_desc_outcome_table,
  'Outcome freq by period' = prop_outcome_by_wedge,
  'Outcome freq by period and LA' = prop_outcome_by_la_wedge)

sample_desc_list = lapply(
  sample_desc_list, function(table){
    
    table%>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        freq = round(freq,2),
        count = 5*round(`count`/5),
        count = ifelse(count <= 5, '[z]', count))
    
  })

# Save descriptives
setwd(paste0(output_path, '/descriptives'))

# Save outputs
wb = openxlsx::createWorkbook()

# Add tables to different worksheets based on list's name
lapply(names(sample_desc_list), function(name){
  
  openxlsx::addWorksheet(wb, name)
  writeData(wb,
            name,  
            sample_desc_list[[name]])
  
})

openxlsx::saveWorkbook(wb, 'previous_cp_sample_description.xlsx',
                       overwrite = TRUE)


