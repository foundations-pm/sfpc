# Sensitivity analysis for No Wrong Doors RCT: primary outcome ----

# Set up  ----

#Paths
user_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(user_directory,'Foundations/High-SFPC-Impact - ')

# Data and output paths

# where the primary outcome dataset is
data_path = paste0(sharepoint_path, 'QA/outputs/') 

# where to save final output list
output_path = paste0(
  sharepoint_path, 'QA/outputs/model_outputs/',
  'primary_analyses/sensitivity_analyses/')

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

# Note on output storage ---------------------------------------------------------------

# Individual model files to be saved in Month-Year (%B %Y format) folder 
# Output files to be created to store all findings from sensitivity analyses in folder sensitivity_analyses

# Generic formula -------------------------------------------------------------------------------

setwd(output_path)

# Sensitivity analyses ------------------------------------------------------------------

## S1: Children with CP > 0 ------------

### Data --------------------------------------------

# 1 load data 
data <- readRDS(file = paste0(
  data_path, 'primary_analysis_analytical_dataset_V2.Rds'))

# Remove children with CP plan > 0
s1_data = dplyr::filter(
  data, 
  number_of_previous_child_protection_plans != '0')

### Cohort description -----------

# Check sample characteristics
covariates = c(
  'local_authority',
  'wedge',
  'treatment_group',
  'cla_status',
  'age_at_referral_cat',
  'gender',
  'ethnicity_agg',
  'disabled_status',
  'unaccompanied_asylum_seeker',
  'referral_no_further_action',
  'number_of_previous_child_protection_plans')

# General cohort distribution
s1_data_desc = s1_data %>%
  dplyr::select(any_of(covariates)) %>%
  dplyr::mutate(treatment_group = as.character(treatment_group),
                cla_status = as.character(cla_status)) 

cohort_desc_table = s1_data_desc %>%
  describe(class = 'categorical') 

# Describing cohort characteristics by LA
cohort_desc_la_table = s1_data_desc %>%
  dplyr::group_by(local_authority) %>%
  describe(class = 'categorical',
           group = 'local_authority')

# Describing characteristics of those who were referred 
# during control vs intervention periods 
cohort_desc_exposure_table = s1_data_desc %>%
  dplyr::group_by(treatment_group) %>%
  describe(class = 'categorical',
           group = 'treatment_group') 

# Describing characteristics of those who became looked after within 18 months 
cohort_desc_outcome_table = s1_data_desc %>%
  dplyr::select(any_of(covariates)) %>%
  dplyr::mutate(treatment_group = as.character(treatment_group),
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

cohort_desc_list = list(
  'Cohort description' = cohort_desc_table,
  'Cohort description by LA' = cohort_desc_la_table,
  'Treatment group' = cohort_desc_exposure_table,
  'Outcome group' = cohort_desc_outcome_table,
  'Outcome freq by period' = prop_outcome_by_wedge,
  'Outcome freq by period and LA' = prop_outcome_by_la_wedge)

cohort_desc_list = lapply(
  cohort_desc_list, function(table){
    
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
lapply(names(cohort_desc_list), function(name){
  
  openxlsx::addWorksheet(wb, name)
  writeData(wb,
    name,  
    cohort_desc_list[[name]])
  
})

openxlsx::saveWorkbook(wb, 'previous_cp_cohort_description.xlsx',
                       overwrite = TRUE)

# Check corr of CPP with outcome
#stats::chisq.test(s1_data_desc$cla_status, 
#                  s1_data_desc$number_of_previous_child_protection_plans)

#CramerV(s1_data_desc$cla_status, 
#        s1_data_desc$number_of_previous_child_protection_plans)

### Formula --------------------------------
demographics = paste('age_at_referral_cat',
                     'gender',
                     'ethnicity_agg',
                     'disabled_status',
                     #'unaccompanied_asylum_seeker', # no UASC in CP > 1
                     'number_of_previous_child_protection_plans',
                     sep = " + ")

cluster_indicator = str_flatten(
  c(" + prop_white_british"))

re = " + (1 | local_authority)"

s1_glmer_formula = paste0(
  "cla_status ~ treatment_group + wedge + ", # FE for trt + time effects
  demographics, # adjust for person level demographics
  cluster_indicator, # adjust for time-varying cluster level indicators
  re
) # RE intercept 4 clusters

s1_glm_formula = paste0(
  "cla_status ~ treatment_group + wedge + local_authority + ", # FE for trt + time effects
  demographics, # adjust for person level demographics
  cluster_indicator#, # adjust for time-varying cluster level indicators
  #re
) # RE intercept 4 clusters

###S1.A Fit model: GLMER -------------------------------------
analysis_type = 'CP > 0 sample - Complete Case - GLMER'

# Fit mixed model
s1_glmer = lme4::glmer(
  as.formula(s1_glmer_formula), 
  data = s1_data, 
  family = binomial)

s1_glmer_summary = summary(s1_glmer)

##### Diagnostics ----
s1_glmer_ss_table = get_optimisers_warning_messages(
  glmer_model_fit = s1_glmer,
  formula = s1_glmer_formula,
  analysis_type = analysis_type)

# Check performance & goodness of fit
#performance::check_model(s2_glmer)

# Check ICC
s1_icc =  performance::icc(s1_glmer)
print(s1_icc)

# Check VIF
# Quick look
performance::check_collinearity(s1_glmer) 
car::vif(s1_glmer)

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

##### Tidy -----
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

#### Save GLMER outputs -----

###### List ----
# Working directory to save outputs table 
setwd(
  paste0(sharepoint_path, 
         'QA/outputs/model_outputs/',
         'primary_analyses/'))

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
setwd(paste0(output_path, 'working_folder/', dir_date)) # Month folder 

#### Tidy and raw
analysis_type = 'CP > 0 sample - Complete Case - GLMER'

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

###S1.B Fit model: GLM -------------------------------------
analysis_type = 'CP > 0 sample - Complete Case - CR3 Robust SE GLM'

# fit logistic GLM model to compare
s1_glm = stats::glm(
  as.formula(s1_glm_formula),
  data = s1_data, 
  family = binomial(link = "logit"))

s1_glm_summary = summary(s1_glm)

##### Robust SEs ----
# Get robust SEs 
# https://davegiles.blogspot.com/2013/05/robust-standard-errors-for-nonlinear.html

# Complete case glm
tictoc::tic()

s1_glm_robust_se = get_robust_se(
  model_fit = s1_glm,
  data = s1_data,
  cluster = 'local_authority')

tictoc::toc()

##### Diagnostics ----
#s1_ss_table = get_optimisers_warning_messages(
#  glmer_model_fit = s2_glmer,
#  analysis_type = 'Open CP plan cohort')

# Check performance & goodness of fit
performance::check_model(s1_glm)

# Check ICC - no ICC for GLM
#s2_glm_icc =  performance::icc(s1_glm)
#print(s2_glm_icc)

# Check warning messages
#tools::assertWarning(
#  stats::update(s1_glm, 
#                singular.ok=FALSE), verbose=TRUE)

# did not work 

# Check VIF
# Quick look
performance::check_collinearity(s1_glm) 
car::vif(s1_glm)

# VIF table 
s1_glm_vif_table = get_vif_table(
  model_fit = s1_glm,
  formula = s1_glm_formula,
  analysis_type = analysis_type)

# Performance & fit indicators: AIC, BIC, R2...
s1_glm_performance_table = get_performance_table(
  s1_glm,
  formula = s1_glm_formula,
  analysis_type = analysis_type)


##### Tidy -----

# Tidy results
# Get tidy results: GLM
s1_glm_tidy = get_tidy_estimates(
  model_fit = s1_glm, 
  analysis_type = analysis_type,
  formula = s1_glm_formula,
  date = date)

# Get raw results: GLM
s1_glm_raw = get_raw_estimates(
  summary_model_fit = s1_glm_summary,
  analysis_type = analysis_type,
  formula = s1_glm_formula,
  date = date)

###S1.C Save all S1 outputs -----------------------------------------

##### Save to list ----
setwd(paste0(data_path, 'model_outputs/'))

############ Tidy
#name_of_the_output_file = 'tidy_output_list.xlsx'

#output_file = str_subset( # find if file exists in directory
#  list.files(), 
#  name_of_the_output_file)

lapply(list(s1_glmer_tidy, s1_glm_tidy),
       function(df) {
         
         append_results(
           output_file = 'tidy_output_list.xlsx',
           table_1_to_append = df,
           save_to = 'tidy_output_list.xlsx')
         
       })

######## Raw
lapply(list(s1_glmer_tidy, s1_glm_tidy),
       function(df) {
         
  append_results(
  output_file = 'raw_output_list.xlsx',
  table_1_to_append = df,
  save_to = 'raw_output_list.xlsx') 
         
         })

######## Diagnostics 

diagnostics_table = dplyr::bind_rows(
  s1_glmer_diagnostics_table,
  s1_glm_performance_table)

vif_table = dplyr::bind_rows(
  s1_glmer_vif_table,
  s1_glm_vif_table)

# Working directory to save diagnostics table 
#setwd(paste0(data_path, 'model_outputs/'))

# Append and/or save table
name_of_the_output_file = 'diagnostics_list.xlsx'

output_file = str_subset( # find if file exists in directory
  list.files(), 
  name_of_the_output_file)

append_results(output_file = 'diagnostics_list.xlsx',
               table_1_to_append = diagnostics_table,
               table_2_to_append = vif_table,
               is_multisheet_workbook = TRUE,
               save_to = 'diagnostics_list.xlsx')

##### Save individual results ----

# Set directory 
cohort_folder = 'Previous CP record cohort'

setwd(paste0(output_path, dir_date, "/", cohort_folder))

######### Tidy

writexl::write_xlsx(
  s1_glmer_tidy, 
  paste0("tidy_S1_Children_CPP_glmer_model", # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
         file_date, ".xlsx"))

writexl::write_xlsx(
  s1_glm_tidy, 
  paste0("tidy_S1_Children_CPP_glm_model", # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
         file_date, ".xlsx"))

######### Raw
setwd(paste0(output_path, dir_date, "/", cohort_folder))

writexl::write_xlsx(
  s1_glmer_raw,
  paste0("raw_S1_Children_CPP_glmer_model", # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
    file_date, ".xlsx"))

writexl::write_xlsx(
  s1_glm_raw,
  paste0("raw_S1_Children_CPP_glm_model", # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
    file_date, ".xlsx"))

######### Diagnostics 
setwd(paste0(output_path, dir_date, "/", cohort_folder))

# Save outputs
wb = openxlsx::createWorkbook()

diagnostics_list = list(
  'cp_cohort_glmer_performance' = s1_glmer_diagnostics_table,
  'cp_cohort_glmer_vif' = s1_glmer_vif_table,
  'cp_cohort_glm_performance' = s1_glm_performance_table,
  'cp_cohort_glm_vif' = s1_glm_vif_table)

# Add tables to different worksheets based on list's name
lapply(names(diagnostics_list), function(name){
  
  openxlsx::addWorksheet(wb, name)
  writeData(wb, name, diagnostics_list[[name]])
  
})

openxlsx::saveWorkbook(
  wb, 'previous_cp_cohort_diagnostics.xlsx',
  overwrite = TRUE)

##S2: Children with an open CP only -------------------------------------------------------------------------------

### Data --------------------------------------------------------
# 1 load data 
setwd(data_path)

s2_data <- readRDS(file = paste0(
  data_path, 'sensitivity_analysis_analytical_dataset_V2.Rds'))

### Cohort description -----------

covariates = c(
  'local_authority',
  'wedge',
  'treatment_group',
  'cla_status',
  'age_at_cp_start',
  'gender',
  'ethnicity_agg',
  'disabled_status',
  'unaccompanied_asylum_seeker',
  'number_of_previous_child_protection_plans',
  'referral_no_further_action',
  'prop_white_british') # to check prop white british derivation

# Prop white british TBC

# General cohort distribution
s2_data_desc = s2_data %>%
  dplyr::select(any_of(covariates)) %>%
  dplyr::mutate(treatment_group = as.character(treatment_group),
                cla_status = as.character(cla_status)) 

cohort_desc_table = s2_data_desc %>%
  describe(class = 'categorical') 

# Describing cohort characteristics by LA
cohort_desc_la_table = s2_data_desc %>%
  dplyr::group_by(local_authority) %>%
  describe(class = 'categorical',
           group = 'local_authority')

# Describing characteristics of those who had an open CP plan 
# during control vs intervention periods 
cohort_desc_exposure_table = s2_data_desc %>%
  dplyr::group_by(treatment_group) %>%
  describe(class = 'categorical',
           group = 'treatment_group') 

# Describing characteristics of those who became looked after within 18 months 
cohort_desc_outcome_table = s2_data_desc %>%
  dplyr::select(any_of(covariates)) %>%
  dplyr::mutate(treatment_group = as.character(treatment_group),
                cla_status = as.character(cla_status)) %>%
  dplyr::group_by(cla_status) %>%
  describe(class = 'categorical',
           group = 'cla_status') 

# Outcome distribution by wedge
prop_outcome_by_wedge = s2_data_desc %>%
  dplyr::group_by(wedge, cla_status) %>% 
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(wedge) %>%
  dplyr::mutate(freq = count/sum(count)) 

# Baseline outcome distribution by wedge by LA
prop_outcome_by_la_wedge = s2_data_desc %>%
  dplyr::group_by(local_authority, wedge, cla_status) %>% 
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(local_authority, wedge) %>%
  dplyr::mutate(freq = count/sum(count)) %>%
  dplyr::arrange(cla_status, local_authority, wedge)

cohort_desc_list = list(
  'Cohort description' = cohort_desc_table,
  'Cohort description by LA' = cohort_desc_la_table,
  'Treatment group' = cohort_desc_exposure_table,
  'Outcome group' = cohort_desc_outcome_table,
  'Outcome freq by period' = prop_outcome_by_wedge,
  'Outcome freq by period and LA' = prop_outcome_by_la_wedge)

cohort_desc_list = lapply(
  cohort_desc_list, function(table){
    
    table%>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        freq = round(freq,2),
        count = 5*round(`count`/5),
        count = ifelse(count <= 5, '[z]', count))
    
  })

# Save descriptives

# Set directory 
cohort_folder = 'Open CP cohort'

if(!dir.exists(
  file.path(paste0(main_dir, sub_dir, '/', cohort_folder)))){
   
     dir.create(file.path(paste0(main_dir, sub_dir, '/', cohort_folder))) # create new dir
     paste0("Creating new directory: '", cohort_folder,"'")# confirm new dir is created
     
}

setwd(paste0(main_dir, sub_dir, "/", cohort_folder))

# Save outputs
wb = openxlsx::createWorkbook()

# Add tables to different worksheets based on list's name
lapply(names(cohort_desc_list), function(name){
  
  openxlsx::addWorksheet(wb, name)
  writeData(wb, name, cohort_desc_list[[name]])
       
       })

openxlsx::saveWorkbook(wb, 'open_cp_cohort_description.xlsx')

### Formula ----------------------
demographics = paste('age_at_cp_start_cat',
                     'gender',
                     'ethnicity_agg',
                     'disabled_status',
                     #'unaccompanied_asylum_seeker',
                     'number_of_previous_child_protection_plans',
                     sep = " + ")

cluster_indicator = str_flatten(
  c(" + prop_white_british"))

re = " + (1 | local_authority)"

s2_glmer_formula = paste0(
  "cla_status ~ treatment_group + wedge + ", # FE for trt + time effects
  demographics, # adjust for person level demographics
  cluster_indicator, # adjust for time-varying cluster level indicators
  re
) # RE intercept 4 clusters

s2_glm_formula = paste0(
  "cla_status ~ treatment_group + wedge + local_authority + ", # FE for trt + time effects
  demographics, # adjust for person level demographics
  cluster_indicator#, # adjust for time-varying cluster level indicators
  #re
) # RE intercept 4 clusters

###S2.A Fit model: GLMER -------------------------------------

analysis_type = 'Open CP plan sample - Complete case - GLMER'

# Fit mixed model:
s2_glmer = lme4::glmer(
  as.formula(s2_glmer_formula), 
  data = s2_data, 
  family = binomial)

s2_glmer_summary = summary(s2_glmer)

# Remove 17 yo to see 
#s2_glmer_wo_17_yo = lme4::glmer(
#  as.formula(s2_glmer_formula),
#  data = s2_data[s2_data$age_at_cp_start_cat != '17',], 
#  family = binomial)

##### Diagnostics ----
s2_glmer_ss_table = get_optimisers_warning_messages(
  glmer_model_fit = s2_glmer,
  formula = s2_glmer_formula,
  analysis_type = analysis_type)

# Check performance & goodness of fit
#performance::check_model(s2_glmer)

# Check ICC
s2_icc =  performance::icc(s2_glmer)
print(s2_icc)

# Check VIF
# Quick look
performance::check_collinearity(s2_glmer) 
car::vif(s2_glmer)

# VIF table 
s2_glmer_vif_table = get_vif_table(
  model_fit = s2_glmer,
  formula = s2_glmer_formula,
  analysis_type = analysis_type)

# Performance & fit indicators: AIC, BIC, R2...
s2_glmer_performance_table = get_performance_table(
  s2_glmer,
  formula = s2_glmer_formula,
  analysis_type = analysis_type)

s2_glmer_diagnostics_table = dplyr::left_join(
  s2_glmer_ss_table,
  s2_glmer_performance_table,
  by = c('analysis_type', 'formula', 'date'))

##### Tidy -----
analysis_type = 'Open CP plan sample - Complete case - GLMER'

# Get raw results: GLMER
s2_glmer_raw = get_raw_estimates(
  summary_model_fit = s2_glmer_summary,
  analysis_type = analysis_type,
  formula = s2_glmer_formula,
  date = date)

# Get tidy results: GLMER
s2_glmer_tidy = get_tidy_estimates(
  model_fit = s2_glmer, 
  analysis_type = analysis_type,
  formula = s2_glmer_formula,
  date = date)

#### Save GLMER outputs -----
###### List ----
# Working directory to save outputs table 
setwd(
  paste0(sharepoint_path, 
         'QA/outputs/model_outputs/',
         'primary_analyses/'))

##### Tidy: Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'tidy_output_list.xlsx')

append_results(
  output_file = output_file,
  table_1_to_append = s2_glmer_tidy,
  save_to = 'tidy_output_list.xlsx') 

##### Raw: Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'raw_output_list.xlsx')

append_results(
  output_file = output_file,
  table_1_to_append = s2_glmer_raw,
  save_to = 'raw_output_list.xlsx') 

##### Diagnostics 
# Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'diagnostics_list.xlsx')

append_results(output_file = output_file,
               table_1_to_append = s2_glmer_diagnostics_table,
               table_2_to_append = s2_glmer_vif_table,
               is_multisheet_workbook = TRUE,
               save_to = 'diagnostics_list.xlsx')

###### Individual files ----
# Save/export raw & tidy estimates into excel file & into folder with monthly date
setwd(paste0(output_path, 'working_folder/', dir_date)) # Month folder 

#### Tidy and raw
analysis_type = 'Open CP Plan sample - Complete Case - GLMER'

writexl::write_xlsx(
  s2_glmer_raw, 
  paste0(
    "raw_",
    janitor::make_clean_names(analysis_type), # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
    file_date, ".xlsx"))

writexl::write_xlsx(
  s2_glmer_tidy, 
  paste0(
    "tidy_", 
    janitor::make_clean_names(analysis_type), # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
    file_date, ".xlsx"))

#### Diagnostics
wb = openxlsx::createWorkbook()

diagnostics_list = list(
  'complete_case_glmer_performance' = s2_glmer_diagnostics_table,
  'complete_case_glmer_vif' = s2_glmer_vif_table)

# Add tables to different worksheets based on list's name
lapply(names(diagnostics_list), function(name){
  
  openxlsx::addWorksheet(wb, name)
  writeData(wb, name, diagnostics_list[[name]])
  
})

openxlsx::saveWorkbook(
  wb, paste0(
    'diagnostics_open_cp_sample_complete_case_glmer', 
    file_date , '.xlsx'),
  overwrite = TRUE)


###S2.B Fit model: GLM -------------------------------------

analysis_type = 'Open CP plan sample - Complete case - CR3 Robust SE GLM'

# Fit logistic GLM model to compare
s2_glm = stats::glm(
  as.formula(s2_glm_formula),
  data = s2_data, 
  family = binomial(link = "logit"))

s2_glm_summary = summary(s2_glm)

#### Diagnostics ----
#s2_ss_table = get_optimisers_warning_messages(
#  glmer_model_fit = s2_glmer,
#  analysis_type = 'Open CP plan cohort')

# Check performance & goodness of fit
performance::check_model(s2_glm)

# Check ICC - no ICC for GLM
#s2_glm_icc =  performance::icc(s2_glm)
#print(s2_glm_icc)

# Check warning messages
#tools::assertError(
#  update(s2_glm, 
#         singular.ok=FALSE), verbose=interactive())

# Check VIF
# Quick look
performance::check_collinearity(s2_glm) 
car::vif(s2_glm)

# VIF table 
s2_glm_vif_table = get_vif_table(
  model_fit = s2_glm,
  formula = s2_glm_formula,
  analysis_type = analysis_type)

# Performance & fit indicators: AIC, BIC, R2...
s2_glm_performance_table = get_performance_table(
  s2_glm,
  formula = s2_glm_formula,
  analysis_type = analysis_type)

#### Robust SEs ----

# Get robust SEs 
# https://davegiles.blogspot.com/2013/05/robust-standard-errors-for-nonlinear.html

# Cluster-robust covariance matrix
#clustered_se <- clubSandwich::vcovCR(
#  s2_glm, 
#  cluster = s2_data$local_authority, 
#  type = 'CR3')

# Calculate confidence intervals
#confint_robust <- lmtest::coefci(
#  s2_glm, 
#  vcov = clustered_se,
#  test = 'naive.t',
#  conf.int = TRUE)

# Convert to a data frame for easier handling
#confint_robust <- as.data.frame(confint_robust)
#colnames(confint_robust) <- c("conf.low", "conf.high") # Rename columns

# Extract coefficient estimates
#coef_estimates <- coef(s2_glm)

# Combine estimates and CIs
#results <- data.frame(
#  term = names(coef_estimates),                      # Variable names
#  odds_ratio = exp(coef_estimates),                  # Odds ratio (exp of coefficient)
#  conf.low = exp(confint_robust$conf.low),           # Lower bound of CI on OR scale
#  conf.high = exp(confint_robust$conf.high)          # Upper bound of CI on OR scale
#)

# Print the results
#print(results)

#### Tidy -----

analysis_type = 'Open CP plan cohort - GLM'

# Tidy results
# Get tidy results: GLM
s2_glm_tidy = get_tidy_estimates(
  model_fit = s2_glm, 
  analysis_type = analysis_type,
  formula = s2_glm_formula,
  date = date)

# Get raw results: GLM
s2_glm_raw = get_raw_estimates(
  summary_model_fit = s2_glm_summary,
  analysis_type = analysis_type,
  formula = s2_glm_formula,
  date = date)

###S2.C Save all S2 outputs -----------------------------------------

##### Save to list ----
setwd(paste0(data_path, 'model_outputs/'))

############ Tidy
#name_of_the_output_file = 'tidy_output_list.xlsx'

#output_file = str_subset( # find if file exists in directory
#  list.files(), 
#  name_of_the_output_file)

lapply(list(s2_glmer_tidy, s2_glm_tidy),
       function(df) {
         
         append_results(
           output_file = 'tidy_output_list.xlsx',
           table_1_to_append = df,
           save_to = 'tidy_output_list.xlsx')
         
       })

######## Raw
#name_of_the_output_file = 'raw_output_list.xlsx'

#output_file = str_subset( # find if file exists in directory
#  list.files(), 
#  name_of_the_output_file)


lapply(list(s2_glmer_tidy, s2_glm_tidy),
       function(df) {
         
         append_results(
           output_file = 'raw_output_list.xlsx',
           table_1_to_append = df,
           save_to = 'raw_output_list.xlsx') 
         
       })

######## Diagnostics 

diagnostics_table = dplyr::bind_rows(
  s2_glmer_diagnostics_table,
  s2_glm_performance_table)

vif_table = dplyr::bind_rows(
  s2_glmer_vif_table,
  s2_glm_vif_table)

# Working directory to save diagnostics table 
setwd(paste0(data_path, 'model_outputs/'))

# Append and/or save table
name_of_the_output_file = 'diagnostics_list.xlsx'

output_file = str_subset( # find if file exists in directory
  list.files(), 
  name_of_the_output_file)

append_results(output_file = 'diagnostics_list.xlsx',
               table_1_to_append = diagnostics_table,
               table_2_to_append = vif_table,
               is_multisheet_workbook = TRUE,
               save_to = 'diagnostics_list.xlsx')

##### Save individual results ----

# Set directory 
cohort_folder = 'Open CP cohort'

######### Tidy
setwd(paste0(output_path, dir_date, "/", cohort_folder))

writexl::write_xlsx(
  s2_glmer_tidy, 
  paste0("tidy_S2_Open_CP_Cohort_glmer_model", # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
         file_date, ".xlsx"))

writexl::write_xlsx(
  s2_glm_tidy, 
  paste0("tidy_S2_Open_CP_Cohort_glm_model", # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
         file_date, ".xlsx"))

######### Raw
setwd(paste0(output_path, dir_date, "/", cohort_folder))

writexl::write_xlsx(
  s2_glmer_raw,
  paste0("raw_S2_Open_CP_Cohort_glmer_model", # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
         file_date, ".xlsx"))

writexl::write_xlsx(
  s2_glm_raw,
  paste0("raw_S2_Open_CP_Cohort_glm_model", # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
         file_date, ".xlsx"))

######### Diagnostics 
setwd(paste0(output_path, dir_date, "/", cohort_folder))

# Save outputs
wb = openxlsx::createWorkbook()

diagnostics_list = list(
  'cp_cohort_glmer_performance' = s2_glmer_diagnostics_table,
  'cp_cohort_glmer_vif' = s2_glmer_vif_table,
  'cp_cohort_glm_performance' = s2_glm_performance_table,
  'cp_cohort_glm_vif' = s2_glm_vif_table)

# Add tables to different worksheets based on list's name
lapply(names(diagnostics_list), function(name){
  
  openxlsx::addWorksheet(wb, name)
  writeData(wb, name, diagnostics_list[[name]])
  
})

openxlsx::saveWorkbook(
  wb, 'open_cp_cohort_diagnostics.xlsx',
  overwrite = TRUE)
