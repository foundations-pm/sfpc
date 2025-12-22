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

# Rationale for bootstrapped standard errors:
# https://academic.oup.com/ije/article/47/1/321/4091562

# https://trialsjournal.biomedcentral.com/articles/10.1186/s13063-016-1571-2 
# Conversely, a cluster-level analysis, or a mixed-effects model or GEE with 
# a small-sample correction led to much wider confidence intervals and larger P values, 
# which more appropriately reflected the uncertainty around 
# the size of the treatment effect estimate.

# Prep data for missing indicator analysis
missing_indicator_data = dplyr::mutate(
  data,
  across(
    .cols = c('gender_final','ethnicity_final', 'disability_status_clean', 'uasc_clean'),
    .fns = ~ ifelse(is.na(.x), 'Missing', .x)
  )
)

## Complete case analysis --------------------------------------------------------

### Set-up ---------------------------------------------------------------------

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
  "cla_status ~ intervention_group + wedge + ", # FE for trt + time effects
  demographics, # adjust for person level demographics
  str_flatten(cluster_indicator), # adjust for time-varying cluster level indicators
  re
) # RE intercept 4 clusters

### Fit model -------------------------------------

# Fit model on standard data: complete case analysis
# Fit model on data with missing indicator recoded: missing indicator analysis

tictoc::tic()

m1_list = lapply(
  c('data', 'missing_indicator_data'), 
  function(dataset){
    
    df = get(dataset)
    
    lme4::glmer(
      as.formula(formula), 
      data = df,
      family = binomial)
    
  })

tictoc::toc()

# Set standard names to keep track of which model is which
# Names will be used to provide summaries & save outputs 
# with a tag indicating which model the estimates are from

# Analysis type = 
# 'Sample type - data type - model type'
# e.g., 'Primary sample - complete case - GLMER'

names(m1_list) = c('Primary sample - Complete case - GLMER',
                   'Primary sample - Missing Indicator - GLMER')

names_m1 = names(m1_list)

# Check summary of models
m1_summary_list = lapply(
  setNames(names_m1, names_m1),
  function(names_index) summary(m1_list[[names_index]]))

names(m1_summary_list)

### Diagnostics -----------------------------------------

# Workplan:
# (1) Troubleshooting model (checking optimiser performance and model warnings) 
# (2) Model diagnostics / fit assessment (VIF, R2, AIC, BIC) - TBC, check model assumptions 
# (e.g., heteroskedasticity etc.)

# Resources: 
# (1 Trouble shooting)
# GLMM FAQ, Aug 2024: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#introduction  
# Intro to Multilevel Modelling: Chap. 7, Model Estimation Options, Problems, and Troubleshooting https://www.learn-mlms.com/07-module-7.html 
# Convergence warnings in lme4: https://rpubs.com/palday/lme4-singular-convergence 
# lme4 convergence warnings: troubleshooting https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html 
# Dealing with convergence failure in mixed models: https://www.youtube.com/watch?v=tSO5JmXR8hk&list=PL8F480DgtpW9_IT7xN1XeRF_dglZmK0nM&index=21 

# (2 Model diagnostics & inference)
# Performance package: https://easystats.github.io/performance/index.html 
# Building and Comparing Mixed Models in R: ICC, Bayes Factor, and Variance Explained: https://www.youtube.com/watch?v=Wtk5iZ65XHk&list=PL8F480DgtpW9_IT7xN1XeRF_dglZmK0nM&index=4
# Mixed Models: Diagnostics and Inference https://sscc.wisc.edu/sscc/pubs/MM/MM_DiagInfer.html
  
# (1) Troubleshooting GLMM:
# https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#introduction 
# and https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html  
# and https://www.learn-mlms.com/07-module-7.html 

# Approach to troubleshooting:
# 1. double-check the model specification and the data for mistakes
# 2. center and scale continuous predictor variables (e.g. with scale())
# 3. try all available optimizers (e.g. several different implementations of BOBYQA and Nelder-Mead, L-BFGS-B from optim, nlminb(), …).
# "we consider it the gold standard; if all optimizers converge to values that are practically equivalent"
# "(it’s up to the user to decide what “practically equivalent means for their case”)" 
# "then we would consider the model fit to be good enough."

# OPTIMIZERS
# Check how optimisers are doing
# Optimisers: variations of BOBYQA, Nelder-Mead, L-BFGS-B
# More info: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#introduction

#summary(warnings())
m1_ss_list = lapply(setNames(names_m1, names_m1), 
                    function(names_index){
                      
                      aa <- allFit(m1_list[[names_index]])
                      ss <- summary(aa) 
                      
                    })

# Convert into table 
m1_ss_table = purrr::map_dfr(
  setNames(names_m1, names_m1), 
  function(names_index){
    
    msgs_list = m1_ss_list[[names_index]]$msgs
    
    # How many rows per optimiser? (at least 1 if NULL)
    n_per_opt <- vapply(
      msgs_list,
      function(msg) if (is.null(msg)) 1L else length(msg),
      integer(1)
    )
    
    # Flatten all messages into a single character vector
    all_msgs <- unlist(
      lapply(msgs_list, function(msg) {
        if (is.null(msg)) "[OK]" else as.character(msg)
      }),
      use.names = FALSE
    )
    
    # One row per message
    ss_df <- data.frame(
      Optimizer = rep(names(msgs_list), times = n_per_opt),
      Message   = all_msgs,
      stringsAsFactors = FALSE
    )
    
    ss_df = ss_df %>%
      dplyr::mutate(
        analysis_type = names_index,
        formula = formula,
        date = date) %>%
      dplyr::relocate(analysis_type, formula)
    
  })

# REVIEW OF WARNINGS 

# 1. Convergence issues - not necessarily fatal; false positive rate is high with lme4

# 2. Singular fits - check https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#singular-fits for full explanations that may apply
# Singular fits commonly occur in two scenarios: ... applicable scenario here:
# (1) small numbers of random-effect levels (e.g. <5), 
# as illustrated in these simulations and discussed (in a somewhat different, Bayesian context) by Gelman (2006).

# Ben Bolker and others suggest great approaches to singularity problems -
# To discuss with Andi on 23/01
  
# (2) Model diagnostics / fit assessment 
# https://easystats.github.io/performance/index.html 
# https://www.youtube.com/watch?v=Wtk5iZ65XHk&list=PL8F480DgtpW9_IT7xN1XeRF_dglZmK0nM&index=4
# https://sscc.wisc.edu/sscc/pubs/MM/MM_DiagInfer.html

# Overall checks 
#performance::check_model(m1_list[[1]])
#performance::check_model(m1_list[[2]])

# Check ICC
#m1_icc = lapply(
#  setNames(names_m1, names_m1),
#  function(names_index) performance::icc(m1_list[[names_index]]))

#print(m1_icc)

# Check VIF
# Quick look
#performance::check_collinearity(m1_list[[1]]) 
#performance::check_collinearity(m1_list[[2]]) 
#car::vif(m1_list[[1]])
#car::vif(m1_list[[2]])

# VIF table 
m1_vif_table = purrr::map_dfr(
  setNames(names_m1, names_m1),
  function(names_index) { 
    
    vif_table = performance::check_collinearity(m1_list[[names_index]]) 
    
    vif_table = vif_table %>%
      dplyr::mutate(analysis_type = names_index, 
                    formula = formula, 
                    date = date) %>%
      dplyr::relocate(analysis_type, formula)
    
  })

# Performance & fit indicators: AIC, BIC, R2...
m1_diagnostics_table = purrr::map_dfr( 
  setNames(names_m1, names_m1),
  function(names_index){
    
    performance_df = performance::model_performance(m1_list[[names_index]])
    
    performance_df = performance_df %>%
      dplyr::mutate(analysis_type = names_index, 
                    formula = formula, 
                    date = date) %>%
      dplyr::relocate(analysis_type, formula)
    
  })

m1_diagnostics_table = dplyr::left_join(
  m1_ss_table,
  m1_diagnostics_table,
  by = c('analysis_type', 'formula', 'date'))

### Tidy up ---------------------------------------------

# Tidy results into dataframes 
#1 Raw model estimates

raw_m1_list <- lapply(
  setNames(names_m1, names_m1),
  function(names_index){
    
    summary_model = m1_summary_list[[names_index]]
    
    get_raw_estimates(
      summary_model_fit = summary_model,
      analysis_type = names_index,
      formula = formula,
      date = date) 
    
  })

names(raw_m1_list)

#2 Tidy model estimates 

# Format tidy: 
# Date, analysis type, formula, effect, term, 
# odds_ratio, conf.high, conf.low,
# std.error, statistic, p.value

# Analysis type = 
# 'Sample type - data type - model type'
# e.g., 'Primary sample - complete case - GLMER'

tidy_m1_list <- lapply(
  setNames(names_m1, names_m1),
  function(names_index) {
    
    
    model_fit = m1_list[[names_index]]
    
    get_tidy_estimates(
      model_fit = model_fit,
      analysis_type = names_index,
      formula = formula,
      date = date) 
    
  })

names(tidy_m1_list)

### Save outputs ----------------------------------------

###### List ----
# Append latest results to existing findings on Sharepoint
# And save these results back into Sharepoint

# Bind all results from analyses into one df
m1_raw_table = do.call(bind_rows, raw_m1_list)
m1_tidy_table = do.call(bind_rows, tidy_m1_list)

# Working directory to save diagnostics table 
setwd(paste0(sharepoint_path, '/Outputs/Primary analyses')) # Where I store the long list of all analyses

##### Tidy: Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'tidy_output_list.xlsx')

append_results(
  output_file = output_file,
  table_1_to_append = m1_tidy_table,
  save_to = 'tidy_output_list.xlsx') 

##### Raw: Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'raw_output_list.xlsx')

append_results(
  output_file = output_file,
  table_1_to_append = m1_raw_table,
  save_to = 'raw_output_list.xlsx') 

##### Diagnostics 
# Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'diagnostics_list.xlsx')

append_results(output_file = output_file,
               table_1_to_append = m1_diagnostics_table,
               table_2_to_append = m1_vif_table,
               is_multisheet_workbook = TRUE,
               save_to = 'diagnostics_list.xlsx')

###### Individual files ----
# Save/export raw & tidy estimates into excel file & into folder with monthly date
setwd(output_path) # Month folder 

#### Tidy and raw
print(names_m1)

lapply(
  setNames(names_m1, names_m1),
  function(names_index) {
    
    writexl::write_xlsx(
      raw_m1_list[[names_index]], 
      paste0(
        "raw_",
        janitor::make_clean_names(names_index), # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
        file_date, ".xlsx"))
    
    writexl::write_xlsx(
      tidy_m1_list[[names_index]], 
      paste0(
        "tidy_", 
        janitor::make_clean_names(names_index), # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
        file_date, ".xlsx"))
  })

#### Diagnostics
wb = openxlsx::createWorkbook()

diagnostics_list = list(
  'complete_case_glmer_performance' = m1_diagnostics_table,
  'complete_case_glmer_vif' = m1_vif_table)

# Add tables to different worksheets based on list's name
lapply(names(diagnostics_list), function(name){
  
  openxlsx::addWorksheet(wb, name)
  writeData(wb, name, diagnostics_list[[name]])
  
})

openxlsx::saveWorkbook(
  wb, paste0(
    'diagnostics_primary_sample_complete_case_glmer', 
    file_date , '.xlsx'),
  overwrite = TRUE)
