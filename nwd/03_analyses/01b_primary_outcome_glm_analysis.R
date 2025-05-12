# Primary outcome analysis for No Wrong Doors RCT DR1 ----

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

#01 GLM model - Complete case ------------------------------------------------------------------

## Workplan -------------------------------------------------------------------

# GLM for spec 1: fully specified
# GLM for spec 2: simpler spec
# GLM for complete case, missing indicator
# GLM for imputed data 

# Pipeline: 
#1 Fit models, parameters: formula, data
#2 Diagnostics, parameters: formula, model fit 
#3 Tidy and raw tables, parameters: formula, model fit 
#4 save outputs: 
#4.1 append to list: tidy, raw, diagnostics 
#4.2 save individual files: tidy, raw, diagnostics 

## Load data -----------------------------------------------------------------------

# 1 load data 
data <- readRDS(file = paste0(
  sharepoint_path, 'QA/outputs/',
  'primary_analysis_analytical_dataset_V2.Rds'))

# Missing indicator data
#missing_indicator_data = dplyr::mutate(
#  data,
#  ethnicity_agg = ifelse(
#    is.na(ethnicity_agg), 'Missing', ethnicity_agg))

#complete_case_data_list = list(
#  'complete_case' = data,
#  'missing_indicator' = missing_indicator_data)

## Formula --------------------------------------------------------------------
setwd(output_path)

# Prep formula 
demographics = paste('age_at_referral_cat',
                     'gender',
                     'ethnicity_agg',
                     'disabled_status',
                     'unaccompanied_asylum_seeker',
                     'number_of_previous_child_protection_plans',
                     #'referral_no_further_action', # not part of the EP
                     sep = " + ")

cluster_indicator = c(
  " + prop_white_british",
  " + turnover_rate_fte",
  " + population_0_to_17")

glm_formula_1 = paste0(
  "cla_status ~ treatment_group + wedge + local_authority + ", # FE for trt + time effects
  demographics, # adjust for person level demographics
  str_flatten(cluster_indicator)#, # adjust for time-varying cluster level indicators
  #re
) # RE intercept 4 clusters

glm_formula_2 = paste0(
  "cla_status ~ treatment_group + wedge + local_authority + ", # FE for trt + time effects
  demographics, # adjust for person level demographics
  cluster_indicator[1]#, # adjust for time-varying cluster level indicators
  #re
) # RE intercept 4 clusters

glm_formula_list = list(
  glm_formula_1,
  glm_formula_2)

formula = glm_formula_list[[1]]

## Fit models -----------------------------------------------------------------

# Complete case pipeline
m1_glm = stats::glm(
  as.formula(formula),
  data = data, 
  family = binomial(link = "logit"))

# Raw summary
m1_glm_summary = summary(m1_glm)

## Robust SEs ----
# Get robust SEs 
# https://davegiles.blogspot.com/2013/05/robust-standard-errors-for-nonlinear.html

# Complete case glm
tictoc::tic()

m1_glm_robust_se = get_robust_se(
  model_fit = m1_glm,
  data = data,
  cluster = 'local_authority')

tictoc::toc()

## Diagnostics ----

# Check performance & goodness of fit
# performance::check_model(s1_glm)

# Check warning messages
#tools::assertWarning(
#  stats::update(s1_glm, 
#                singular.ok=FALSE), verbose=TRUE)
# did not work 

# GLM complete case
# VIF table 
#m1_glm_vif_table = get_vif_table(
#  model_fit = m1_glm,
#  formula = formula,
#  analysis_type = analysis_type)

# Performance & fit indicators: AIC, BIC, R2...
#m1_glm_performance_table = get_performance_table(
#  m1_glm,
#  formula = formula,
#  analysis_type = analysis_type)


## Tidy -----

analysis_type = 'Primary sample - Complete Case - CR3 Robust SE GLM'

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

## Save outputs -----

#### List ----
# Append latest results to existing findings on Sharepoint
# And save these results back into Sharepoint

# Working directory to save diagnostics table 
setwd(output_path) # Model outputs

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
#output_file = str_subset( # find if file exists in directory
#  list.files(), 
#  'diagnostics_list.xlsx')

#append_results(output_file = output_file,
#               table_1_to_append = m1_glm_diagnostics_table,
#               table_2_to_append = m1_glm_vif_table,
#               is_multisheet_workbook = TRUE,
#               save_to = 'diagnostics_list.xlsx')

#### Individual files ----
# Save/export raw & tidy estimates into excel file & into folder with monthly date
setwd(paste0(output_path, 'working_folder/', dir_date))

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

#02 GLM model - Imputed data ------------------------------------------------------------------

## Workplan -------------------------------------------------------------------

# GLM for spec 1: fully specified
# GLM for spec 2: simpler spec
# GLM for complete case, missing indicator
# GLM for imputed data 

# Pipeline: 
#1 Fit models, parameters: formula, data
#2 Diagnostics, parameters: formula, model fit 
#3 Tidy and raw tables, parameters: formula, model fit 
#4 save outputs: 
#4.1 append to list: tidy, raw, diagnostics 
#4.2 save individual files: tidy, raw, diagnostics 

## Load data -----------------------------------------------------------------------

# 2 load imputed data
# Read data
setwd(paste0(
  sharepoint_path,
  'QA/outputs/datasets/imputed_datasets/'))

file.info(
  list.files(
    "Norfolk_binary_single_level_m5_imputation/",
    full.names=T))[,1, drop=F]

load(paste0("Norfolk_binary_single_level_m5_imputation/",
            "Norfolk_binary_single_level_m5_imputation.Rdata"))

imputed_data_m5 = mi.res 

imputed_data_m10 = load(
  paste0("Norfolk_binary_single_level_m10_imputation/",
         "Norfolk_binary_single_level_m10_imputation.Rdata"))

imputed_data_m10 = mi.res 

rm(mi.res)

# Number of iteration used to impute datasets
iteration_number = 100

imputed_data_list = list(
  'imputed_m5' = imputed_data_m5,
  'imputed_m10' = imputed_data_m10)

## Formula --------------------------------------------------------------------
setwd(output_path)

# Prep formula 
demographics = paste('age_at_referral_cat',
                     'gender',
                     'ethnicity_agg',
                     'disabled_status',
                     'unaccompanied_asylum_seeker',
                     'number_of_previous_child_protection_plans',
                     #'referral_no_further_action', # not part of the EP
                     sep = " + ")

cluster_indicator = c(
  " + prop_white_british",
  " + turnover_rate_fte",
  " + population_0_to_17")

glm_formula_1 = paste0(
  "cla_status ~ treatment_group + wedge + local_authority + ", # FE for trt + time effects
  demographics, # adjust for person level demographics
  str_flatten(cluster_indicator)#, # adjust for time-varying cluster level indicators
  #re
) # RE intercept 4 clusters

glm_formula_2 = paste0(
  "cla_status ~ treatment_group + wedge + local_authority + ", # FE for trt + time effects
  demographics, # adjust for person level demographics
  cluster_indicator[1]#, # adjust for time-varying cluster level indicators
  #re
) # RE intercept 4 clusters

glm_formula_list = list(
  glm_formula_1,
  glm_formula_2)

formula = glm_formula_list[[2]]

## Fit models -----------------------------------------------------------------

#analysis_type = 'Main cohort - Imputed m5 - GLM'

# fit logistic GLM model on imputed data 
# Fitting models:
m2_glm_list = lapply( # Creates a list of model objects
  c("imputed_data_m5", "imputed_data_m10"),  # models are fitted to both datasets
  function(dataset){
    
    print(paste0(
      'Fitting model for: ', dataset))
    
    df = get(dataset)
    
    # Fit model 
    with( 
      df, 
      stats::glm(
        as.formula(formula),
        family = binomial(link = "logit")))
    
  })

# check difference when using non-robust standard SE
#m2_glm_summary = summary(m2_glm_list[[1]])
#m2_glm_pooled_results = mice::pool(m2_glm_list[[1]]) 
#m2_glm_tidy = broom.mixed::tidy(m2_glm_list[[1]],
#  conf.int=TRUE, 
#  exponentiate=TRUE,
#  effects=c("fixed"))

## Robust SE ----

# Function to get robust SEs for each imputed data 
# Then pooling estimates together
# Robust SE

tictoc::tic()

m2_robust_glm_pooled_fit = pool_glm_with_robust_se(
  imputed_data = imputed_data_m5,
  formula = formula,
  family = binomial(link = "logit"),
  cluster = 'local_authority')

tictoc::toc()

# Raw summary
m2_robust_glm_summary = summary(
  m2_robust_glm_pooled_fit,
  exponentiate = TRUE, # exp doesn't work with base R summary - exp() in tidy below
  conf.int = TRUE)

## Tidy ----
analysis_type = 'Primary sample - Imputed M5 - CR3 Robust SE GLM'

# to save non-exponentiated estimates 
m2_robust_glm_raw = m2_robust_glm_summary %>%
  dplyr::mutate(
    date = date,
    analysis_type = analysis_type,
    formula = formula,
    term = rownames(.)) %>%
  dplyr::rename('estimate' = 'results',
                'std.error' = 'se',
                'statistic' = 't',
                'p.value' = 'p',
                'conf.low' = '(lower',
                'conf.high' = 'upper)') %>%
  dplyr::relocate(date, analysis_type, formula, term) %>%
  tibble::remove_rownames()

# Tidy results
# Get tidy results: GLM
m2_robust_glm_tidy = m2_robust_glm_summary %>%
  dplyr::mutate(
    date = date,
    analysis_type = analysis_type,
    formula = formula,
    effect = 'fixed',
    term = rownames(.),
    odds.ratio = exp(results),
    conf.low = exp(`(lower`),
    conf.high = exp(`upper)`)) %>%
  dplyr::rename('std.error' = 'se',
                'p.value' = 'p',
                'statistic' = 't') %>%
  dplyr::select(date, analysis_type, formula, effects, term,
                odds.ratio, conf.low, conf.high, 
                std.error, statistic, p.value, missInfo) %>%
  tibble::remove_rownames()

## Save outputs -----

###### List ----
# Append latest results to existing findings on Sharepoint
# And save these results back into Sharepoint

# Working directory to save diagnostics table 
setwd(output_path) # Model outputs

##### Tidy: Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'tidy_output_list.xlsx')

append_results(
  output_file = output_file,
  table_1_to_append = m2_robust_glm_tidy,
  save_to = 'tidy_output_list.xlsx') 

##### Raw: Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'raw_output_list.xlsx')

append_results(
  output_file = output_file,
  table_1_to_append = m2_robust_glm_raw,
  save_to = 'raw_output_list.xlsx') 

##### Diagnostics 
# Append and/or save table
#output_file = str_subset( # find if file exists in directory
#  list.files(), 
#  'diagnostics_list.xlsx')

#append_results(output_file = output_file,
#               table_1_to_append = m1_glm_diagnostics_table,
#               table_2_to_append = m1_glm_vif_table,
#               is_multisheet_workbook = TRUE,
#               save_to = 'diagnostics_list.xlsx')

###### Individual files ----
# Save/export raw & tidy estimates into excel file & into folder with monthly date
setwd(paste0(output_path, 'working_folder/', dir_date))

#### Tidy and raw
writexl::write_xlsx(
  m2_robust_glm_raw, 
  paste0(
    "raw_",
    janitor::make_clean_names(analysis_type),# Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
    file_date, ".xlsx"))

writexl::write_xlsx(
m2_robust_glm_tidy, 
  paste0(
    "tidy_", 
    janitor::make_clean_names(analysis_type),# Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
    file_date, ".xlsx"))

#### Diagnostics

