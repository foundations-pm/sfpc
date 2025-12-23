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
main_output_path = paste0(sharepoint_path, '/Outputs/Primary analyses')
secondary_output_path = paste0(main_output_path, '/3. Sensitivity analysis')

# Working directory
wd = paste0(r_directory, "Documents/SFPC/FS/")

# Dates 
date = format(Sys.Date(),"%Y/%m/%d") # date format to save within dataframes

file_date = format(Sys.Date(),"_%Y%b%d") # date format to save files

dir_date = format(Sys.Date(),"%B %Y") # date format to create directories

# Set up folder with %m/%Y to store analyses 
if(!dir.exists(file.path(paste0(secondary_output_path, '/', dir_date)))){
  
  dir.create(file.path(paste0(secondary_output_path, '/', dir_date))) # create new dir
  paste0("Creating new directory: '", dir_date,"'")# confirm new dir is created
  
} else { 
  
  cat(
    crayon::green(
      crayon::bold(
        paste0("Directory '", dir_date, "' already exists."))))
  
} # confirms dir already exists

# Formula for all models ----
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

# S1: Factors identified at the start of the assessment ------------------------

## 1. Missing indicator analyses -----------------------------------------------

### 1.1 Load data -----------------

# Main sample: children referred to CSC during trial period
setwd(data_path)

data = readRDS(file = paste0(
  data_path, "/primary_outcome_sample_analytical_dataset_final.Rds")) 

### 1.2 Derive data ----
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
  paste0('primary_outcome_sample_children_with_risk_factors',
         '_1B_2B_3A_3B_3C_4B_dataset.rds'
  )
)

### 1.3 Fit model ----
setwd(data_path)

s1_data = readRDS(
  paste0('primary_outcome_sample_children_with_risk_factors',
         '_1B_2B_3A_3B_3C_4B_dataset.rds'
  )
)

analysis_type = 'Children with risk factors - Missing indicator - GLMER'

# Missing indicator data
# Impute missing cases 
s1_data = dplyr::mutate(
  s1_data,
  across(
    .cols = c('gender_final','ethnicity_final', 'disability_status_clean', 'uasc_clean'),
    .fns = ~ ifelse(is.na(.x), 'Missing', .x)
  )
) 

# Fit mixed model
analysis_type = 'Children with risk factors - Missing indicator - GLMER'

s1_glmer = lme4::glmer(
  as.formula(formula), 
  data = s1_data, 
  family = binomial #,
  #nAGQ= 0
  )

s1_glmer_summary = summary(s1_glmer)

### 1.4 Diagnostics ----
s1_glmer_ss_table = get_optimisers_warning_messages(
  glmer_model_fit = s1_glmer,
  formula = formula,
  analysis_type = analysis_type)

# VIF table 
s1_glmer_vif_table = get_vif_table(
  model_fit = s1_glmer,
  formula = formula,
  analysis_type = analysis_type)

# Performance & fit indicators: AIC, BIC, R2...
s1_glmer_performance_table = get_performance_table(
  s1_glmer,
  formula = formula,
  analysis_type = analysis_type)

s1_glmer_diagnostics_table = dplyr::left_join(
  s1_glmer_ss_table,
  s1_glmer_performance_table,
  by = c('analysis_type', 'formula', 'date'))

### 1.5 Tidy -----
# Get raw results: GLMER
s1_glmer_raw = get_raw_estimates(
  summary_model_fit = s1_glmer_summary,
  analysis_type = analysis_type,
  formula = formula,
  date = date)

# Get tidy results: GLMER
s1_glmer_tidy = get_tidy_estimates(
  model_fit = s1_glmer, 
  analysis_type = analysis_type,
  formula = formula,
  date = date)

### 1.6 Save outputs ----

###### List ----
# Working directory to save outputs table 

setwd(main_output_path)
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
setwd(paste0(secondary_output_path, '/', dir_date)) # Month folder 

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
    'diagnostics_',
    janitor::make_clean_names(analysis_type), 
    file_date , '.xlsx'),
  overwrite = TRUE)

### 2. Multiple imputation -----------------------------------------------------------

#### 2.1 Load data ----------------

# Main sample: children referred to CSC during trial period
setwd(data_path)

data = readRDS(file = paste0(
  data_path, "/primary_outcome_sample_analytical_dataset_final.Rds")) 

# Main sample: imputed data (m = 10)
setwd(paste0(data_path, '/main_sample_primary_outcome_imputed_data'))

file.info(
  list.files(
    full.names=T))[,1, drop=F]

load(paste0("main_sample_m10_imputation/",
            "main_sample_m10_imputation.Rdata"))

imputed_data_m10 = mi.res 

rm(mi.res)

# Number of iteration used to impute datasets
iteration_number = 10

#### 2.2 Derive data ----

factor_cols_df = data %>% 
  dplyr::select(any_of(contains('factors_'))) %>%
  dplyr::mutate(.id = row_number())
  
all_imp_data = mice::complete(imputed_data_m10, action = 'long', include = TRUE)

all_imp_data_transformed = all_imp_data %>%
  dplyr::left_join(factor_cols_df, by = ".id") %>%
  dplyr::mutate(
    risk_factors_identified_post_assessment =
      if_else(if_any(starts_with("factors_"), ~ .x == "Yes"), "Yes", "No")
  ) %>%
  dplyr::filter(risk_factors_identified_post_assessment == "Yes") %>%
  dplyr::select(-contains('factors'))

s1_imp_data = as.mids(all_imp_data_transformed)

rm(
  imputed_data_m10, all_imp_data, 
  all_imp_data_transformed, data, factor_cols_df
  )


#### 2.3 Fit model ----
analysis_type = 'Children with risk factors - Imputed m10 - GLMER'

# Fit model on imputed datasets with m= 10 and m=20
# Fitting models:
tictoc::tic()

s1_imp_model = with( 
  s1_imp_data, 
  lme4::glmer(
    as.formula(formula), 
    family = binomial #, 
    #nAGQ = 0
    )
) 

tictoc::toc()

# Save model fit 
setwd(
  paste0(secondary_output_path, '/', dir_date, '/R Objects')
  )  

saveRDS(
  s1_imp_model,
  file= paste0(
    "glmer_fit_", 
    str_replace(
      janitor::make_clean_names(analysis_type),
      '_glmer', ''
      ),
    ".RData")
  )

# Pool results & summary
s1_pooled_results <- mice::pool(s1_imp_model) # pool results
s1_summary = summary(s1_pooled_results) 

#### 2.4 Diagnostics ----
s1_glmer_ss_table = get_optimisers_warning_messages(
  glmer_model_fit = s1_imp_model[['analyses']][[1]],
  formula = formula,
  analysis_type = analysis_type)

# VIF table 
s1_glmer_vif_table = get_vif_table(
  model_fit = s1_imp_model[['analyses']][[1]],
  formula = formula,
  analysis_type = analysis_type)

# Performance & fit indicators: AIC, BIC, R2...
s1_glmer_performance_table = get_performance_table(
  s1_imp_model[['analyses']][[1]],
  formula = formula,
  analysis_type = analysis_type)

s1_glmer_diagnostics_table = dplyr::left_join(
  s1_glmer_ss_table,
  s1_glmer_performance_table,
  by = c('analysis_type', 'formula', 'date'))

#### 2.5 Tidy -----
# Tidy results into dataframes 

#1 Raw model estimates
s1_glmer_raw <- s1_summary %>%
      dplyr::mutate(
        date = date,
        analysis_type = analysis_type,
        formula = formula) %>%
      dplyr::relocate(date, analysis_type, formula)

#2 Tidy model estimates 
s1_glmer_tidy = get_tidy_estimates(
  model_fit = s1_pooled_results,
  analysis_type = analysis_type,
  formula = formula,
  date = date) 

#### 2.6 Save outputs ----

###### List ----
# Working directory to save outputs table 
setwd(main_output_path)
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
setwd(paste0(secondary_output_path, '/', dir_date)) # Month folder 

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
    'diagnostics_',
    janitor::make_clean_names(analysis_type), 
    file_date , '.xlsx'),
  overwrite = TRUE)


# ------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------ #

team
# S2: Born-only children -------------------------------------------------------

## 1. Missing indicator analyses -----------------------------------------------

### 1.1 Load data -----------------

# Main sample: children referred to CSC during trial period
setwd(data_path)

data = readRDS(file = paste0(
  data_path, "/primary_outcome_sample_analytical_dataset_final.Rds")) 

### 1.2 Derive data ----

# Create a flag for risk factors (drug/alcohol abuse, DA/DVA, mental health)
s2_data = dplyr::filter(
  data, 
  unborn_flag == 'born')

nrow(s2_data) # 25,001

# Save dataset for descriptives
setwd(data_path)

saveRDS(
  s2_data, 
  paste0('primary_outcome_sample_born_only_children.rds'
  )
)

### 1.3 Fit model ----
setwd(data_path)

s2_data = readRDS(
  paste0('primary_outcome_sample_born_only_children.rds'
  )
)

analysis_type = 'Born-only children - Missing indicator - GLMER'

# Missing indicator data
# Impute missing cases 
s2_data = dplyr::mutate(
  s2_data,
  across(
    .cols = c('gender_final','ethnicity_final', 'disability_status_clean', 'uasc_clean'),
    .fns = ~ ifelse(is.na(.x), 'Missing', .x)
  )
) 

# Fit mixed model
analysis_type = 'Born-only children - Missing indicator - GLMER'

s2_glmer = lme4::glmer(
  as.formula(formula), 
  data = s2_data, 
  family = binomial #,
  #nAGQ= 0
  )

s2_glmer_summary = summary(s2_glmer)

### 1.4 Diagnostics ----
s2_glmer_ss_table = get_optimisers_warning_messages(
  glmer_model_fit = s2_glmer,
  formula = formula,
  analysis_type = analysis_type)

# VIF table 
s2_glmer_vif_table = get_vif_table(
  model_fit = s2_glmer,
  formula = formula,
  analysis_type = analysis_type)

# Performance & fit indicators: AIC, BIC, R2...
s2_glmer_performance_table = get_performance_table(
  s2_glmer,
  formula = formula,
  analysis_type = analysis_type)

s2_glmer_diagnostics_table = dplyr::left_join(
  s2_glmer_ss_table,
  s2_glmer_performance_table,
  by = c('analysis_type', 'formula', 'date'))

### 1.5 Tidy -----
# Get raw results: GLMER
s2_glmer_raw = get_raw_estimates(
  summary_model_fit = s2_glmer_summary,
  analysis_type = analysis_type,
  formula = formula,
  date = date)

# Get tidy results: GLMER
s2_glmer_tidy = get_tidy_estimates(
  model_fit = s2_glmer, 
  analysis_type = analysis_type,
  formula = formula,
  date = date)

### 1.6 Save outputs ----

###### List ----
# Working directory to save outputs table 

setwd(main_output_path)
analysis_type = 'Born-only children - Missing indicator - GLMER'

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
setwd(paste0(secondary_output_path, '/', dir_date)) # Month folder 

#### Tidy and raw
analysis_type = 'Born-only children - Missing indicator - GLMER'

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
    'diagnostics_',
    janitor::make_clean_names(analysis_type), 
    file_date , '.xlsx'),
  overwrite = TRUE)

### 2. Multiple imputation -----------------------------------------------------------

#### 2.1 Load data ----------------

# Main sample: imputed data (m = 10)
setwd(paste0(data_path, '/main_sample_primary_outcome_imputed_data'))

file.info(
  list.files(
    full.names=T))[,1, drop=F]

load(paste0("main_sample_m10_imputation/",
            "main_sample_m10_imputation.Rdata"))

imputed_data_m10 = mi.res 

rm(mi.res)

# Number of iteration used to impute datasets
iteration_number = 10

#### 2.2 Derive data ----

all_imp_data = mice::complete(imputed_data_m10, action = 'long', include = TRUE)

all_imp_data_transformed = dplyr::filter(
    all_imp_data, 
    unborn_flag == 'born')

s1_imp_data = as.mids(all_imp_data_transformed)

rm(imputed_data_m10, all_imp_data, 
   all_imp_data_transformed
)

#### 2.3 Fit model ----
analysis_type = 'Born-only children - Imputed m10 - GLMER'

# Fit model on imputed datasets with m= 10 and m=20
# Fitting models:
tictoc::tic()

s1_imp_model = with( 
  s1_imp_data, 
  lme4::glmer(
    as.formula(formula), 
    family = binomial #, 
    #nAGQ = 0
    )
) 

tictoc::toc()

# Save model fit 
setwd(
  paste0(secondary_output_path, '/', dir_date, '/R Objects')
)  

saveRDS(
  s1_imp_model,
  file= paste0(
    "glmer_fit_", 
    str_replace(
      janitor::make_clean_names(analysis_type),
      '_glmer', ''
    ),
    ".RData")
)

# Pool results & summary
s1_pooled_results <- mice::pool(s1_imp_model) # pool results
s1_summary = summary(s1_pooled_results) 

#### 2.4 Diagnostics ----
s2_glmer_ss_table = get_optimisers_warning_messages(
  glmer_model_fit = s1_imp_model[['analyses']][[1]],
  formula = formula,
  analysis_type = analysis_type)

# VIF table 
s2_glmer_vif_table = get_vif_table(
  model_fit = s1_imp_model[['analyses']][[1]],
  formula = formula,
  analysis_type = analysis_type)

# Performance & fit indicators: AIC, BIC, R2...
s2_glmer_performance_table = get_performance_table(
  s1_imp_model[['analyses']][[1]],
  formula = formula,
  analysis_type = analysis_type)

s2_glmer_diagnostics_table = dplyr::left_join(
  s2_glmer_ss_table,
  s2_glmer_performance_table,
  by = c('analysis_type', 'formula', 'date'))

#### 2.5 Tidy -----
# Tidy results into dataframes 

#1 Raw model estimates
s2_glmer_raw <- s1_summary %>%
  dplyr::mutate(
    date = date,
    analysis_type = analysis_type,
    formula = formula) %>%
  dplyr::relocate(date, analysis_type, formula)

#2 Tidy model estimates 
s2_glmer_tidy = get_tidy_estimates(
  model_fit = s1_pooled_results,
  analysis_type = analysis_type,
  formula = formula,
  date = date) 

#### 2.6 Save outputs ----

###### List ----
# Working directory to save outputs table 
setwd(main_output_path)
analysis_type = 'Born-only children - Missing indicator - GLMER'

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
setwd(paste0(secondary_output_path, '/', dir_date)) # Month folder 

#### Tidy and raw
analysis_type = 'Born-only children - Missing indicator - GLMER'

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
    'diagnostics_',
    janitor::make_clean_names(analysis_type), 
    file_date , '.xlsx'),
  overwrite = TRUE)