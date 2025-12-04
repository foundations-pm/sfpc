#------------------------------------------------#
#
# FAMILY SAFEGUARDING: STEPPED WEDGE ANALYSES ----
#
#------------------------------------------------#

# ANALYSIS: PRIMARY OUTCOME MODEL FITTING: GLM ----

r_directory = 'C:/Users/PerrineMachuel/'

# Libraries 
{ source(paste0(r_directory, "Documents/SFPC/FS/config.R")) }

# Functions 
{ source(paste0(r_directory, "Documents/SFPC/FS/functions.R"))}

# Set-up  ----
sharepoint_path = paste0(r_directory,'Foundations/High-SFPC-Impact - Family Safeguarding')

# Data and output paths
data_path = paste0(sharepoint_path, '/Datasets/analytical_datasets')
output_path = paste0(sharepoint_path, '/Outputs/Primary analyses/Main analysis')

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

## Fit GLM ---------------------------------------------------------------------

### Set-up --------------------------------------------------------------------

# Prep formula 
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
  "cla_status ~ intervention_group + wedge + local_authority + ", # FE for trt + time effects
  demographics, # adjust for person level demographics
  str_flatten(cluster_indicator)#, # adjust for time-varying cluster level indicators
  #re
) # RE intercept 4 clusters

### Fit model -----------------------------------------------------------------

# Complete case pipeline
m1_glm = stats::glm(
  as.formula(formula),
  data = data, 
  family = binomial(link = "logit"))

# Raw summary
m1_glm_summary = summary(m1_glm)

### Robust SEs -------
# Get robust SEs 
# https://davegiles.blogspot.com/2013/05/robust-standard-errors-for-nonlinear.html

# Complete case glm
tictoc::tic()

m1_glm_robust_se = get_robust_se(
  model_fit = m1_glm,
  data = data,
  cluster = 'local_authority',
  method = 'CR2',
  test = 'Satterthwaite')

tictoc::toc()

### Diagnostics ----

# Check performance & goodness of fit
# performance::check_model(s1_glm)

# Check warning messages
#tools::assertWarning(
#  stats::update(s1_glm, 
#                singular.ok=FALSE), verbose=TRUE)
# did not work 
analysis_type = 'Primary sample - Complete Case - CR2 Satterthwaite Robust SE GLM'

# GLM complete case
# VIF table 
m1_glm_vif_table = get_vif_table(
  model_fit = m1_glm,
  formula = formula,
  analysis_type = analysis_type)

# Performance & fit indicators: AIC, BIC, R2...
m1_glm_performance_table = get_performance_table(
  m1_glm,
  formula = formula,
  analysis_type = analysis_type)

### Tidy -----

analysis_type = 'Primary sample - Complete Case - CR2 Satterthwaite Robust SE GLM'

# Tidy results
# Get tidy results: GLM
m1_glm_tidy = m1_glm_robust_se %>%
  dplyr::mutate(
    date = date,
    analysis_type = analysis_type,
    formula = formula) %>%
  dplyr::relocate(date, analysis_type, formula)

# Get raw results: GLM
m1_glm_raw = get_raw_estimates(
  summary_model_fit = m1_glm_summary,
  analysis_type = analysis_type,
  formula = formula,
  date = date)

### Save outputs -----

#### List ----
# Append latest results to existing findings on Sharepoint
# And save these results back into Sharepoint

# Working directory to save diagnostics table 
setwd(paste0(sharepoint_path, '/Outputs/Primary analyses')) # Model outputs

##### Tidy: Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'tidy_output_list.xlsx')

append_results(
  output_file = output_file,
  table_1_to_append = m1_glm_tidy,
  save_to = 'tidy_output_list.xlsx') 

##### Raw: Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'raw_output_list.xlsx')

append_results(
  output_file = output_file,
  table_1_to_append = m1_glm_raw,
  save_to = 'raw_output_list.xlsx') 

##### Diagnostics 
# Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'diagnostics_list.xlsx')

append_results(output_file = output_file,
               table_1_to_append = m1_glm_performance_table,
               table_2_to_append = m1_glm_vif_table,
               is_multisheet_workbook = TRUE,
               save_to = 'diagnostics_list.xlsx')

#### Individual files ----
# Save/export raw & tidy estimates into excel file & into folder with monthly date
setwd(paste0(output_path, '/', dir_date))

#### Tidy and raw'
writexl::write_xlsx(
  m1_glm_tidy, 
  paste0(
    'tidy_', 
    janitor::make_clean_names(analysis_type), # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
    file_date, ".xlsx"))

writexl::write_xlsx(
  m1_glm_raw, 
  paste0(
    'raw_', 
    janitor::make_clean_names(analysis_type), # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
    file_date, ".xlsx"))

### Diagnostics
wb = openxlsx::createWorkbook()

diagnostics_list = list(
  'cr2_robust_se_glm_performance' = m1_glm_performance_table,
  'cr2_robust_se_glm_vif' = m1_glm_vif_table)

# Add tables to different worksheets based on list's name
lapply(names(diagnostics_list), function(name){
  
  openxlsx::addWorksheet(wb, name)
  writeData(wb, name, diagnostics_list[[name]])
  
})

openxlsx::saveWorkbook(
  wb, paste0(
    'diagnostics_primary_sample_complete_case_c2_satt_robust_se_glm', 
    file_date , '.xlsx'),
  overwrite = TRUE)
