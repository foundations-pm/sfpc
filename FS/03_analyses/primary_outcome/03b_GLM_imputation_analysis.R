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
data_path = paste0(sharepoint_path, '/Datasets/analytical_datasets/main_sample_primary_outcome_imputed_data')
output_path = paste0(sharepoint_path, '/Outputs/Primary analyses/3. Sensitivity analysis')

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

# Number of iteration used to impute datasets
iteration_number = 10

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

analysis_type = 'Primary sample - Imputed m10 - CR2 Satterthwaite Robust SE GLM'

### Fit models -------------------------------------------------------------------

# fit logistic GLM model on imputed data 
# Fitting models:
m_fit = with( 
  imputed_data_m10, 
  stats::glm(
    as.formula(formula),
    family = binomial(link = "logit")))

# check difference when using non-robust standard SE
# Very sinmilar estimates 
# m2_glm_summary = summary(m2_glm_list[[2]])
# m2_glm_pooled_results = mice::pool(m2_glm_list[[2]]) 
# m2_glm_tidy = broom.mixed::tidy(m2_glm_pooled_results,
#   conf.int=TRUE, 
#   exponentiate=TRUE,
#   effects=c("fixed"))

### Robust SE ----

#tictoc::tic()

#m2_robust_glm_pooled_fit = pool_glm_with_robust_se(
#  imputed_data = imputed_data_m10,
#  formula = formula,
#  family = binomial(link = "logit"),
#  cluster = 'local_authority',
#  cluster_robust_method = 'CR2')

#tictoc::toc()

# Set working directory
setwd(paste0(output_path, '/', dir_date, '/R objects'))

# Extract all imputed datasets
imputed_list <- mice::complete(imputed_data_m10, "all")
names(imputed_list) = 1:10

# Loop through imputations: fit model, get coef + robust vcov
robust_model_list <- lapply(names(imputed_list)[10], function(name) {
  
  print(paste0('Fitting model on dataset :', name))
  
  df = imputed_list[[name]]
  
  model <- glm(
    formula = formula,
    family = binomial(link = "logit"),
    data = df
  )
  
  print('Vcov matrix')
  
  vcov_cr <- clubSandwich::vcovCR(
    model,
    cluster = df[['local_authority']],
    type = 'CR2'
  )
  
  print('List model fit and vcov matrix')
  
  list = list(
    coef = coef(model),
    vcov = vcov_cr,
    formula = formula
  )
  
  print('Save outputs')
  
  saveRDS(
    list,
    paste0('glm_fit_and_cr2_vcov_list_imputed_data_m', name, '.Rdata'))
  
  return(list)
  
})

# Raw summary  
m2_robust_glm_summary = summary(
  m2_robust_glm_pooled_fit,
  exponentiate = TRUE, # exp doesn't work with base R summary - exp() in tidy below
  conf.int = TRUE)

## Tidy ----
analysis_type = 'Primary sample - Imputed M10 - CR2 Robust SE GLM'

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
    effects = 'fixed',
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
setwd(paste0(sharepoint_path, '/Outputs/Primary analyses')) # Where I store the long list of all analyses

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
setwd(paste0(output_path, '/', dir_date))

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
