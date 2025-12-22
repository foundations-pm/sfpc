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
data_path = paste0(sharepoint_path, '/Datasets/analytical_datasets/main_sample_primary_outcome_imputed_data')
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

file.info(
  list.files(
    full.names=T))[,1, drop=F]

load(paste0("main_sample_m10_imputation/",
            "main_sample_m10_imputation.Rdata"))

imputed_data_m10 = mi.res 

load(
  paste0("main_sample_m20_imputation/",
         "main_sample_m20_imputation.Rdata"))

imputed_data_m20 = mi.res 

rm(mi.res)

# Number of iteration used to impute datasets
iteration_number = 10

# checks 
#d10_m10 = complete(imputed_data_m10, 10)
#d10_m10 %>% dplyr::group_by(ethnicity_final) %>% dplyr::summarise(n())
#d20_m20 = complete(imputed_data_m20, 20)
#d20_m20 %>% dplyr::group_by(ethnicity_final) %>% dplyr::summarise(n())

## Multiple imputation analysis --------------------------------------------------------

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

### Fit models -----------------------------------------

# Fit model on imputed datasets with m= 10 and m=20
# Fitting models:
tictoc::tic()

m2_list = lapply( # Creates a list of model objects
  c("imputed_data_m10", "imputed_data_m20"),  # models are fitted to both datasets
  function(dataset){
    
    print(paste0(
      'Fitting model for: ', dataset))
    
    df = get(dataset)
    
    # Fit model 
    with( 
      df, 
      lme4::glmer(
        as.formula(formula), 
        family = binomial)) 
    
  })

tictoc::toc()

# Save model lists
setwd(paste0(output_path, '/', dir_date))

lapply(c(1:2), \(index){
  saveRDS(
    m2_list[[index]],
    file= paste0(
      "glmer_model_", 
      janitor::make_clean_names(names(m2_list[index])),
      ".RData"))
  
})

# Set standard names to keep track of which model is which
# Names will be used to provide summaries & save outputs 
# with a tag indicating which model the estimates are from

# Analysis type = 
# 'Sample type - data type - model type'
# e.g., 'Primary sample - complete case - GLMER'

names(m2_list) = c('Primary sample - Imputed m10 - GLMER',
                   'Primary sample - Imputed m20 - GLMER')

names_m2 = names(m2_list)

#### Pooling results ----
# As per Stef Van Buurren's workflow recs:
# https://stefvanbuuren.name/fimd/workflow.html

m2_pooled_results_list <- lapply(
  setNames(names_m2, names_m2), function(names_index){
    
    mice::pool(m2_list[[names_index]]) # pool results
    
  })

names(m2_pooled_results_list)

# Save pooled results 
setwd(paste0(output_path, '/', dir_date))

lapply(c(1:2), \(index){
  saveRDS(
    m2_pooled_results_list[[index]],
    file= paste0(
      "glmer_pooled_results_", 
      janitor::make_clean_names(names(m2_pooled_results_list[index])),
      ".RData"))
  
})

# Get summaries from pooled results
# This retrieves raw model estimates for m=5 and m=10
m2_summary_list = lapply(
  setNames(names_m2, names_m2), function(names_index){ 
    
    summary(m2_pooled_results_list[[names_index]]) })

names(m2_summary_list)

#### Marginal effects ----
# Get average marginal effects 
ame <- marginaleffects::avg_predictions(
  m2_list[['Primary sample - Imputed m10 - GLMER']],
  by = 'intervention_group',
  hypothesis = "b2 - b1 = 0")

summary(ame)

### Diagnostics -------------------------------------

# OPTIMIZERS
# Check how optimisers are doing
# Optimisers: variations of BOBYQA, Nelder-Mead, L-BFGS-B
# More info: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#introduction

#summary(warnings())

m2_ss_list = lapply(
  setNames(names_m2, names_m2), 
  function(names_index){
    
    # assess only the first imputed dataset for m=10 and m=20
    aa <- allFit(m2_list[[names_index]]$analyses[[1]])
    ss <- summary(aa) 
    
  })

# Save object
setwd(paste0(output_path, '/', dir_date))

lapply(c(1:2), \(index){
  saveRDS(
    m2_ss_list[[index]],
    file= paste0(
      "AllFit_", 
      janitor::make_clean_names(names(m2_ss_list[index])),
      ".RData"))
  
})

# Convert into table 
m2_ss_table = purrr::map_dfr(
  setNames(names_m2, names_m2), 
  function(names_index){
    
    msgs_list = m2_ss_list[[names_index]]$msgs
    
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
      dplyr::mutate(analysis_type = names_index,
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
#performance::check_model(m2_list[['complete_case']])
#performance::check_model(m2_list[['missing_indicator']])
  
# Check ICC
#m2_icc = lapply(
#  setNames(names_m2, names_m2),
#  function(names_index) performance::icc(m2_list[[names_index]]))
  
#print(m2_icc)

# Check VIF
#car::vif(m2_list[['complete_case']])
#car::vif(m2_list[['missing_indicator']])
  
m2_vif_table = purrr::map_dfr(
  setNames(names_m2, names_m2),
  function(names_index) { 
      
    # assess only the first imputed dataset for m=5 and m=10
    vif_table = performance::check_collinearity(
      m2_list[[names_index]]$analyses[[1]]) 
    
    vif_table = vif_table %>%
      dplyr::mutate(analysis_type = names_index, 
                    formula = formula, 
                    date = date) %>%
      dplyr::relocate(analysis_type, formula)
    
  })

# Performance & fit indicators: AIC, BIC, R2...
m2_diagnostics_table = purrr::map_dfr( 
  setNames(names_m2, names_m2),
  function(names_index){
    
    # assess only the first imputed dataset for m=5 and m=10
    performance_df = performance::model_performance(
      m2_list[[names_index]]$analyses[[1]])
    
    performance_df = performance_df %>%
      dplyr::mutate(analysis_type = names_index, 
                    formula = formula, 
                    date = date) %>%
      dplyr::relocate(analysis_type, formula)
    
  })

m2_diagnostics_table = dplyr::left_join(
  m2_ss_table,
  m2_diagnostics_table,
  by = c('analysis_type', 'formula', 'date'))

### Tidy up -------------------------------------------

# Tidy results into dataframes 
#1 Raw model estimates

raw_m2_list <- lapply(
  setNames(names_m2, names_m2),
  function(names_index){
    
    summary_model = m2_summary_list[[names_index]]
    
    summary_model = summary_model %>%
      dplyr::mutate(
        date = date,
        analysis_type = names_index,
        formula = formula) %>%
      dplyr::relocate(date, analysis_type, formula)
    
  })

names(raw_m2_list)

#2 Tidy model estimates 

# Format tidy: 
# Date, analysis type, formula, effect, term, 
# odds_ratio, conf.high, conf.low,
# std.error, statistic, p.value
# df if applicable

# Analysis type = 
# 'Sample type - data type - model type'
# e.g., 'Primary sample - complete case - GLMER'

tidy_m2_list <- lapply(
  setNames(names_m2, names_m2),
  function(names_index) {
    
    
    model_fit = m2_pooled_results_list[[names_index]]
    
    get_tidy_estimates(
      model_fit = model_fit,
      analysis_type = names_index,
      formula = formula,
      date = date) 
    
  })

names(tidy_m2_list)

### Save outputs ----------------------------------------

###### List ----
# Append latest results to existing findings on Sharepoint
# And save these results back into Sharepoint

# Bind all results from analyses into one df
m2_raw_table = do.call(bind_rows, raw_m2_list)
m2_tidy_table = do.call(bind_rows, tidy_m2_list)

# Working directory to save diagnostics table 
setwd(paste0(sharepoint_path, '/Outputs/Primary analyses')) # Where I store the long list of all analyses

##### Tidy: Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'tidy_output_list.xlsx')

append_results(
  output_file = output_file,
  table_1_to_append = m2_tidy_table,
  save_to = 'tidy_output_list.xlsx') 

##### Raw: Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'raw_output_list.xlsx')

append_results(
  output_file = output_file,
  table_1_to_append = m2_raw_table,
  save_to = 'raw_output_list.xlsx') 

##### Diagnostics 
# Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'diagnostics_list.xlsx')

append_results(output_file = output_file,
               table_1_to_append = m2_diagnostics_table,
               table_2_to_append = m2_vif_table,
               is_multisheet_workbook = TRUE,
               save_to = 'diagnostics_list.xlsx')

###### Individual files ----
# Save/export raw & tidy estimates into excel file & into folder with monthly date
setwd(paste0(output_path, '/', dir_date))

#### Tidy and raw

lapply(
  setNames(names_m2, names_m2),
  function(names_index) {
    
    writexl::write_xlsx(
      raw_m2_list[[names_index]], 
      paste0(
        "raw_",
        janitor::make_clean_names(names_index),# Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
        file_date, ".xlsx"))
    
    writexl::write_xlsx(
      tidy_m2_list[[names_index]], 
      paste0(
        "tidy_",
        janitor::make_clean_names(names_index),# Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
        file_date, ".xlsx"))
  })

#### Diagnostics
wb = openxlsx::createWorkbook()

diagnostics_list = list(
  'imputed_models_performance' = m2_diagnostics_table,
  'imputed_models_vif' = m2_vif_table)

# Add tables to different worksheets based on list's name
lapply(names(diagnostics_list), function(name){
  
  openxlsx::addWorksheet(wb, name)
  writeData(wb, name, diagnostics_list[[name]])
  
})

openxlsx::saveWorkbook(
  wb, paste0(
    'diagnostics_primary_sample_imputed_data_glmer',
    file_date, '.xlsx'),
  overwrite = TRUE)
