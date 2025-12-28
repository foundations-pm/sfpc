
#------------------------------------------------#
#
# FAMILY SAFEGUARDING: STEPPED WEDGE ANALYSES ----
#
#------------------------------------------------#

# ANALYSIS: EXPLORATION ANALYSES ----

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
  "cla_status ~ intervention_group * readiness + wedge + ", # FE for trt + time effects
  demographics, # adjust for person level demographics
  str_flatten(cluster_indicator), # adjust for time-varying cluster level indicators
  re
) # RE intercept 4 clusters

print(formula)

## 1. Missing indicator analyses -----------------------------------------------

### 1.1 Load data -----------------

# Alternative sample: children who were born during their first referral during the trial
setwd(data_path)

data = readRDS(file = paste0(
  data_path, "/primary_outcome_sample_analytical_dataset_final.Rds")) 

### 1.2 Derive data ----

# Create a flag for risk factors (drug/alcohol abuse, DA/DVA, mental health)
data = dplyr::mutate(
  data, 
  readiness = case_when(
    local_authority %in% c('walsall', 'lancashire') ~ 'High readiness',
    TRUE ~ 'Low readiness'
    ),
  readiness = factor(readiness, levels = c('High readiness', 'Low readiness'))
  )

nrow(data) # 27,406

### 1.3 Fit model ----
analysis_type = 'Readiness * intervention - Missing indicator - GLMER'

# Missing indicator data
# Impute missing cases 
e1_data = dplyr::mutate(
  data,
  across(
    .cols = c('gender_final','ethnicity_final', 'disability_status_clean', 'uasc_clean'),
    .fns = ~ ifelse(is.na(.x), 'Missing', .x)
  )
) 

# Fit mixed model
analysis_type = 'Readiness * intervention - Missing indicator - GLMER'

tictoc::tic()

e1_glmer = lme4::glmer(
  as.formula(formula), 
  data = e1_data, 
  family = binomial #,
  #nAGQ= 0
)

tictoc::toc()

# Save model fit 
setwd(
  paste0(secondary_output_path, '/', dir_date, '/R Objects')
)  

saveRDS(
  e1_glmer,
  file= paste0(
    "glmer_fit_", 
    str_replace(
      janitor::make_clean_names(analysis_type),
      '_glmer', ''
    ),
    ".RData")
)

e1_glmer_summary = summary(e1_glmer)

### 1.4 Diagnostics ----
e1_glmer_ss_table = get_optimisers_warning_messages(
  glmer_model_fit = e1_glmer,
  formula = formula,
  analysis_type = analysis_type)

# VIF table 
e1_glmer_vif_table = get_vif_table(
  model_fit = e1_glmer,
  formula = formula,
  analysis_type = analysis_type)

# Performance & fit indicators: AIC, BIC, R2...
e1_glmer_performance_table = get_performance_table(
  e1_glmer,
  formula = formula,
  analysis_type = analysis_type)

e1_glmer_diagnostics_table = dplyr::left_join(
  e1_glmer_ss_table,
  e1_glmer_performance_table,
  by = c('analysis_type', 'formula', 'date'))

### 1.5 Tidy -----
# Get raw results: GLMER
e1_glmer_raw = get_raw_estimates(
  summary_model_fit = e1_glmer_summary,
  analysis_type = analysis_type,
  formula = formula,
  date = date)

# Get tidy results: GLMER
e1_glmer_tidy = get_tidy_estimates(
  model_fit = e1_glmer, 
  analysis_type = analysis_type,
  formula = formula,
  date = date)

### 1.6 Save outputs ----

###### List ----
# Working directory to save outputs table 

setwd(main_output_path)
analysis_type = 'Readiness * intervention - Missing indicator - GLMER'

##### Tidy: Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'tidy_output_list.xlsx')

append_results(
  output_file = output_file,
  table_1_to_append = e1_glmer_tidy,
  save_to = 'tidy_output_list.xlsx') 

##### Raw: Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'raw_output_list.xlsx')

append_results(
  output_file = output_file,
  table_1_to_append = e1_glmer_raw,
  save_to = 'raw_output_list.xlsx') 

##### Diagnostics 
# Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'diagnostics_list.xlsx')

append_results(output_file = output_file,
               table_1_to_append = e1_glmer_diagnostics_table,
               table_2_to_append = e1_glmer_vif_table,
               is_multisheet_workbook = TRUE,
               save_to = 'diagnostics_list.xlsx')

###### Individual files ----
# Save/export raw & tidy estimates into excel file & into folder with monthly date
setwd(paste0(secondary_output_path, '/', dir_date)) # Month folder 

#### Tidy and raw
analysis_type = 'Readiness * intervention - Missing indicator - GLMER'

writexl::write_xlsx(
  e1_glmer_raw, 
  paste0(
    "raw_",
    janitor::make_clean_names(analysis_type), # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
    file_date, ".xlsx"))

writexl::write_xlsx(
  e1_glmer_tidy, 
  paste0(
    "tidy_", 
    janitor::make_clean_names(analysis_type), # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
    file_date, ".xlsx"))

#### Diagnostics
wb = openxlsx::createWorkbook()

diagnostics_list = list(
  'complete_case_glmer_performance' = e1_glmer_diagnostics_table,
  'complete_case_glmer_vif' = e1_glmer_vif_table)

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

all_imp_data_transformed = dplyr::mutate(
  all_imp_data, 
  readiness = case_when(
    local_authority %in% c('walsall', 'lancashire') ~ 'High readiness',
    TRUE ~ 'Low readiness'
  ),
  readiness = factor(
    readiness, levels = c('High readiness', 'Low readiness')
  )
)

e1_imp_data = as.mids(all_imp_data_transformed)

rm(imputed_data_m10, all_imp_data, 
   all_imp_data_transformed
)

#### 2.3 Fit model ----
analysis_type = 'Readiness * intervention - Imputed m10 - GLMER'

# Fit model on imputed datasets with m= 10 and m=20
# Fitting models:
tictoc::tic()

e1_imp_model = with( 
  e1_imp_data, 
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
  e1_imp_model,
  file= paste0(
    "glmer_fit_", 
    str_replace(
      janitor::make_clean_names(analysis_type),
      '_glmer', ''
    ),
    ".RData")
)

# Pool results & summary
e1_pooled_results <- mice::pool(e1_imp_model) # pool results
e1_summary = summary(e1_pooled_results) 

#### 2.4 Diagnostics ----
e1_glmer_ss_table = get_optimisers_warning_messages(
  glmer_model_fit = e1_imp_model[['analyses']][[1]],
  formula = formula,
  analysis_type = analysis_type)

# VIF table 
e1_glmer_vif_table = get_vif_table(
  model_fit = e1_imp_model[['analyses']][[1]],
  formula = formula,
  analysis_type = analysis_type)

# Performance & fit indicators: AIC, BIC, R2...
e1_glmer_performance_table = get_performance_table(
  e1_imp_model[['analyses']][[1]],
  formula = formula,
  analysis_type = analysis_type)

e1_glmer_diagnostics_table = dplyr::left_join(
  e1_glmer_ss_table,
  e1_glmer_performance_table,
  by = c('analysis_type', 'formula', 'date'))

#### 2.5 Tidy -----
# Tidy results into dataframes 

#1 Raw model estimates
e1_glmer_raw <- e1_summary %>%
  dplyr::mutate(
    date = date,
    analysis_type = analysis_type,
    formula = formula) %>%
  dplyr::relocate(date, analysis_type, formula)

#2 Tidy model estimates 
e1_glmer_tidy = get_tidy_estimates(
  model_fit = e1_pooled_results,
  analysis_type = analysis_type,
  formula = formula,
  date = date) 

#### 2.6 Save outputs ----

###### List ----
# Working directory to save outputs table 
setwd(main_output_path)
analysis_type = 'Readiness * intervention - Imputed m10 - GLMER'

##### Tidy: Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'tidy_output_list.xlsx')

append_results(
  output_file = output_file,
  table_1_to_append = e1_glmer_tidy,
  save_to = 'tidy_output_list.xlsx') 

##### Raw: Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'raw_output_list.xlsx')

append_results(
  output_file = output_file,
  table_1_to_append = e1_glmer_raw,
  save_to = 'raw_output_list.xlsx') 

##### Diagnostics 
# Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'diagnostics_list.xlsx')

append_results(output_file = output_file,
               table_1_to_append = e1_glmer_diagnostics_table,
               table_2_to_append = e1_glmer_vif_table,
               is_multisheet_workbook = TRUE,
               save_to = 'diagnostics_list.xlsx')

###### Individual files ----
# Save/export raw & tidy estimates into excel file & into folder with monthly date
setwd(paste0(secondary_output_path, '/', dir_date)) # Month folder 

#### Tidy and raw
analysis_type = 'Readiness * intervention - Imputed m10 - GLMER'

writexl::write_xlsx(
  e1_glmer_raw, 
  paste0(
    "raw_",
    janitor::make_clean_names(analysis_type), # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
    file_date, ".xlsx"))

writexl::write_xlsx(
  e1_glmer_tidy, 
  paste0(
    "tidy_", 
    janitor::make_clean_names(analysis_type), # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
    file_date, ".xlsx"))

#### Diagnostics
wb = openxlsx::createWorkbook()

diagnostics_list = list(
  'complete_case_glmer_performance' = e1_glmer_diagnostics_table,
  'complete_case_glmer_vif' = e1_glmer_vif_table)

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