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

# Load data ----------------------------------------------------------------------------

##Exp1: Time/Treatment interaction effects -----------------------------------------

### Data ----
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
rm(mi.res)

# Number of iteration used to impute datasets
iteration_number = 100

imp_datasets = complete(imputed_data_m5, action = 'long', include = TRUE)

# Relevel time bc of perfect separation 
# Between baseline where treatment = 0 
# And wedge 4 where treatment = 1
imp_datasets = imp_datasets %>%
  dplyr::group_by(`.imp`) %>%
  dplyr::mutate(
    readiness = ifelse(
      local_authority %in% c('rochdale', 'warrington'), 
      'High readiness', 'Low readiness'))

# checks 
imp_data_for_analysis = as.mids(imp_datasets)

### Formula -------------------------------------------------------------------
# Prep formula 
demographics = paste('age_at_referral_cat',
                     'gender',
                     'ethnicity_agg',
                     'disabled_status',
                     'unaccompanied_asylum_seeker',
                     'number_of_previous_child_protection_plans',
                     #'referral_no_further_action', # not in EP
                     sep = " + ")

re = " + (1 | local_authority)"

# Cluster indicator
# splines = model_data %>% 
#  select(contains('splines')) %>%
#  colnames() 

cluster_indicator = c(
  " + prop_white_british",
  " + turnover_rate_fte",
  " + population_0_to_17" #,
  #paste0(splines, sep = ' + ')
)

glmer_formula_1 = paste0( # fully-specified, per protocol
  "cla_status ~ treatment_group * readiness + wedge + ", # FE for trt + time effects
  demographics, # adjust for person level demographics
  str_flatten(cluster_indicator), # adjust for time-varying cluster level indicators
  re
) # RE intercept 4 clusters

glmer_formula_2 = paste0( # simplified spec 
  "cla_status ~ treatment_group * readiness + wedge + ", # FE for trt + time effects
  demographics, # adjust for person level demographics
  cluster_indicator[1], # adjust for time-varying cluster level indicators
  re
) # RE intercept 4 clusters

glmer_formula_list = list(
  glmer_formula_1,
  glmer_formula_2)

# Use simplified spec 
formula = glmer_formula_list[[2]]

### Fit models -----------------------------------------

# Fit model on imputed datasets with m= 5 and m=10
# Fitting models:
exp1_glmer_models = with( 
  imp_data_for_analysis, 
  lme4::glmer(
    as.formula(formula), 
    family = binomial)) 

# Set standard names to keep track of which model is which
# Names will be used to provide summaries & save outputs 
# with a tag indicating which model the estimates are from

# Analysis type = 
# 'Sample type - data type - model type'
# e.g., 'Primary sample - complete case - GLMER'

analysis_type = paste0(
  'Primary Sample: Exploratory Readiness Analysis - Imputed m5 - GLMER')

# Pooling results 
# As per Stef Van Buurren's workflow recs:
# https://stefvanbuuren.name/fimd/workflow.html
exp1_pooled_results <- mice::pool(exp1_glmer_models) # pool results

# Get summaries from pooled results
exp1_summary = summary(exp1_pooled_results) 

### Diagnostics -------------------------------------

# OPTIMIZERS
# Check how optimisers are doing
# Optimisers: variations of BOBYQA, Nelder-Mead, L-BFGS-B
# More info: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#introduction

#summary(warnings())
aa <- allFit(exp1_glmer_models$analyses[[1]])
ss <- summary(aa) 

# Convert into table 
exp1_ss_df = data.frame(
  Optimizer = names(ss$msgs),
  Message = sapply(
    ss$msgs, 
    function(msg) {
      if (is.null(msg)) {
        "[OK]"  # Replace NULL with "[OK]" or use NA if preferred
      } else {
        msg  # Keep the warning message
      }
    }),
  stringsAsFactors = FALSE)

exp1_ss_df <- exp1_ss_df %>%
  pivot_wider(names_from = Optimizer, 
              values_from = Message)

exp1_ss_df = exp1_ss_df %>%
  dplyr::mutate(analysis_type = analysis_type,
                formula = formula,
                date = date) %>%
  dplyr::relocate(analysis_type, formula)

# Performance & fit indicators: AIC, BIC, R2...
exp1_performance_df = performance::model_performance(
  exp1_glmer_models$analyses[[1]])

exp1_performance_df = exp1_performance_df %>%
  dplyr::mutate(analysis_type = analysis_type, 
                formula = formula, 
                date = date) %>%
  dplyr::relocate(analysis_type, formula)

exp1_diagnostics_table = dplyr::left_join(
  exp1_ss_df,
  exp1_performance_df,
  by = c('analysis_type', 'formula', 'date'))

# Check multicollinearity
exp1_vif_table = performance::check_collinearity(
  exp1_glmer_models$analyses[[1]]) 

exp1_vif_table = exp1_vif_table %>%
  dplyr::mutate(analysis_type = analysis_type, 
                formula = formula, 
                date = date) %>%
  dplyr::relocate(analysis_type, formula)

### Tidy up -------------------------------------------

# Tidy results into dataframes 
#1 Raw model estimates
exp1_raw_table = exp1_summary %>%
  dplyr::mutate(
    date = date,
    analysis_type = analysis_type,
    formula = formula) %>%
  dplyr::relocate(date, analysis_type, formula)

#2 Tidy model estimates 
exp1_tidy_table <- get_tidy_estimates(
  model_fit = exp1_pooled_results,
  analysis_type = analysis_type,
  formula = formula,
  date = date) 

### Save outputs ----------------------------------------

###### List ----

# Working directory to save diagnostics table 
setwd(paste0(data_path, '/model_outputs/primary_analyses/')) # Model outputs

##### Tidy: Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'tidy_output_list.xlsx')

append_results(
  output_file = output_file,
  table_1_to_append = exp1_tidy_table,
  save_to = 'tidy_output_list.xlsx') 

##### Raw: Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'raw_output_list.xlsx')

append_results(
  output_file = output_file,
  table_1_to_append = exp1_raw_table,
  save_to = 'raw_output_list.xlsx') 

##### Diagnostics 
# Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'diagnostics_list.xlsx')

append_results(output_file = output_file,
               table_1_to_append = exp1_diagnostics_table,
               table_2_to_append = exp1_vif_table,
               is_multisheet_workbook = TRUE,
               save_to = 'diagnostics_list.xlsx')

###### Individual files ----
# Save/export raw & tidy estimates into excel file & into folder with monthly date
setwd(paste0(output_path, 'working_folder/', dir_date))

#### Tidy and raw
writexl::write_xlsx(
  exp1_raw_table, 
  paste0(
    "raw_exploratory_readiness_glmer_",
    file_date, ".xlsx"))

writexl::write_xlsx(
  exp1_tidy_table, 
  paste0(
    "tidy_exploratory_readiness_glmer_",
    file_date, ".xlsx"))

#### Diagnostics
wb = openxlsx::createWorkbook()

diagnostics_list = list(
  'imputed_models_performance' = exp1_diagnostics_table,
  'imputed_models_vif' = exp1_vif_table)

# Add tables to different worksheets based on list's name
lapply(names(diagnostics_list), function(name){
  
  openxlsx::addWorksheet(wb, name)
  writeData(wb, name, diagnostics_list[[name]])
  
})

openxlsx::saveWorkbook(
  wb, paste0(
    'diagnostics_exploratory_readiness_glmer',
    file_date, '.xlsx'),
  overwrite = TRUE)