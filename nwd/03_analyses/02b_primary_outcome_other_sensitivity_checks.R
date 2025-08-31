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

# 1 load data 
data <- readRDS(file = paste0(
  data_path, 'primary_analysis_analytical_dataset_V2.Rds'))

# Note on output storage ---------------------------------------------------------------

# Individual model files to be saved in Month-Year (%B %Y format) folder 
# Output files to be created to store all findings from sensitivity analyses in folder sensitivity_analyses

setwd(output_path)

##S1: Change in treatment assignment -----------------------------------------

### Data --------------------------------------------------------

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
rm(mi.res)

imp_datasets = complete(imputed_data_m5, action = 'long', include = TRUE)

# Number of iteration used to impute datasets
iteration_number = 100

# Change treatment definition
# 2 definitions proposed in EP:
# Def 1 = treated if child spent at least half of their referral in treatment condition 
# Def 2 =  treated if child spent at least 4 weeks of their referral in treatment condition 
# TO NOTE: we cannot execute these definitions exactly. This is a deviation from protocol > 
# Trial did not collect referral closing date so cannot infer consistently this date across cases
# Meaning we don't know the actual length of open referrals for children 
# Instead, we assume length of referral = a child's total time spent in the study 
# Time spent in the study = start is referral date; 
# end is: becoming looked after OR 18 months post-referral (if not LAC before)

# Add LA treatment start and EOSP 
imp_datasets = imp_datasets %>%
  dplyr::group_by(`.imp`) %>%
  dplyr::mutate( # eosp = end of study period
    eosp = as.Date( # add 18 months to DOB
      referral_date) %m+% months(18),
    la_treatment_start  = case_when(
      local_authority == 'rochdale' ~ as.Date('2020-04-01'),
      local_authority == 'warrington' ~ as.Date('2021-04-01'),
      local_authority == 'norfolk' ~ as.Date('2021-06-01'),
      local_authority == 'redcar' ~ as.Date('2021-09-01'))) 

#### 1 Alternative treatment definition 1 ----

# Derive the halfway point in the child's time spent in the trial
# To calculate the half study period date:
#1 use end of study period (eosp = the end of study period for each child)
#2 eosp = 18 months post referral OR time between referral date and date period of care commenced
#3 get halfway point between referral date and eosp date 
imp_datasets = imp_datasets %>%
  dplyr::group_by(`.imp`) %>%
  dplyr::mutate( 
    half_open_referral_date = 
      as.Date(referral_date) + (as.numeric( 
        as.Date(eosp) - as.Date(referral_date)) / 2))

# Derive treatment group called 'treatment halfway group'
# If LA treatment start == or is BEFORE halfway date, then treatment = 1, otherwise 0
imp_datasets = imp_datasets %>%
  dplyr::group_by(`.imp`) %>%
  dplyr::group_by(local_authority) %>%
  dplyr::mutate(
    treatment_half_way_group = ifelse(
      la_treatment_start <= half_open_referral_date, 1, 0))

#### 2 Alternative treatment definition 2 ----

# Derive treatment group where treatment = 1 for children who have spent at least 4 weeks 
# of their time in the trial within the treatment condition
imp_datasets = imp_datasets %>%
  dplyr::group_by(`.imp`) %>%
  dplyr::group_by(local_authority) %>%
  dplyr::mutate(
    treatment_4_weeks_group = case_when(
      # if study period is under 4 weeks (28 days), assign to control
      as.numeric(as.Date(eosp) - as.Date(referral_date)) <= 28 ~ 0,
      # if study period is above 4 weeks, and child was referred AFTER cluster
      # went into treatment condition, assign to treatment
      (as.numeric(as.Date(eosp) - as.Date(referral_date)) > 28) &
        (as.Date(la_treatment_start) < as.Date(referral_date)) ~ 1,
      # if study period is above 4 weeks, and child was referred BEFORE cluster
      # went into treatment condition, check if:
      # cluster treatment switch date is BEFORE the child's end of study time - 28 days
      # if its BEFORE, assign to treatment 
      (as.numeric(as.Date(eosp) - as.Date(referral_date)) > 28) &
        (as.Date(la_treatment_start) >= as.Date(referral_date)) &
        (as.Date(la_treatment_start) < (as.Date(eosp) - 28)) ~ 1,
      # - if it's AFTER, assign to control
      (as.numeric(as.Date(eosp) - as.Date(referral_date)) > 28) &
        (as.Date(la_treatment_start) >= as.Date(referral_date)) &
        (as.Date(la_treatment_start) >= (as.Date(eosp) - 28)) ~ 0,
      TRUE ~ NA
      ))

# Treatment 4 weeks seems very generous in terms of assigning treated cases 
# Probably due to the length of 'open referral' which we consider being 
# the length of study participation = wider window for being exposed during 4 weeks 

### Descriptives ----------------------------------------------------------------

# Alternative treatment assignment method 1 
# Treatment / control group distribution  
imp_datasets %>% 
  dplyr::filter(.imp == 0) %>%
  dplyr::group_by(treatment_half_way_group) %>%
  dplyr::summarise(n())

imp_datasets %>% 
  dplyr::filter(.imp == 0) %>%
  dplyr::group_by(local_authority, treatment_half_way_group) %>%
  dplyr::summarise(n())

# Alternative treatment assignment method 2 
# Treatment / control group distribution 
imp_datasets %>% 
  dplyr::filter(.imp == 0) %>%
  dplyr::group_by(treatment_4_weeks_group) %>%
  dplyr::summarise(n())

imp_datasets %>% 
  dplyr::filter(.imp == 0) %>%
  dplyr::group_by(local_authority, treatment_4_weeks_group) %>%
  dplyr::summarise(n())


imp_datasets %>% 
  dplyr::filter(.imp == 0) %>%
  dplyr::group_by(local_authority) %>%
  dplyr::summarise(n())

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
  "cla_status ~ treatment_half_way_group + wedge + ", # FE for trt + time effects
  demographics, # adjust for person level demographics
  str_flatten(cluster_indicator), # adjust for time-varying cluster level indicators
  re
) # RE intercept 4 clusters

glmer_formula_2 = paste0( # simplified spec 
  "cla_status ~ treatment_half_way_group + wedge + ", # FE for trt + time effects
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

# Turn datasets into a mids class before fitting models 
imp_data_for_analysis = as.mids(imp_datasets)

# Fit model on imputed datasets with m= 5 and m=10
# Fitting models:
s1_glmer_models = with( 
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
  'Alternative Treatment Definition: ', 
  'Halfway Treatment Group - Imputed m5 - GLMER')

# Pooling results 
# As per Stef Van Buurren's workflow recs:
# https://stefvanbuuren.name/fimd/workflow.html
s1_pooled_results <- mice::pool(s1_glmer_models) # pool results

# Get summaries from pooled results
s1_summary = summary(s1_pooled_results) 

### Diagnostics -------------------------------------

# OPTIMIZERS
# Check how optimisers are doing
# Optimisers: variations of BOBYQA, Nelder-Mead, L-BFGS-B
# More info: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#introduction

#summary(warnings())
aa <- allFit(s1_glmer_models$analyses[[1]])
ss <- summary(aa) 

# Convert into table 
s1_ss_df = data.frame(
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

s1_ss_df <- s1_ss_df %>%
  pivot_wider(names_from = Optimizer, 
              values_from = Message)

s1_ss_df = s1_ss_df %>%
  dplyr::mutate(analysis_type = analysis_type,
                formula = formula,
                date = date) %>%
  dplyr::relocate(analysis_type, formula)

# Performance & fit indicators: AIC, BIC, R2...
s1_performance_df = performance::model_performance(
  s1_glmer_models$analyses[[1]])
    
s1_performance_df = s1_performance_df %>%
      dplyr::mutate(analysis_type = analysis_type, 
                    formula = formula, 
                    date = date) %>%
      dplyr::relocate(analysis_type, formula)

s1_diagnostics_table = dplyr::left_join(
  s1_ss_df,
  s1_performance_df,
  by = c('analysis_type', 'formula', 'date'))

# Check multicollinearity
s1_vif_table = performance::check_collinearity(
  s1_glmer_models$analyses[[1]]) 

s1_vif_table = s1_vif_table %>%
  dplyr::mutate(analysis_type = analysis_type, 
                formula = formula, 
                date = date) %>%
  dplyr::relocate(analysis_type, formula)

### Tidy up -------------------------------------------

# Tidy results into dataframes 
#1 Raw model estimates
s1_raw_table = s1_summary %>%
  dplyr::mutate(
    date = date,
    analysis_type = analysis_type,
    formula = formula) %>%
  dplyr::relocate(date, analysis_type, formula)

#2 Tidy model estimates 
s1_tidy_table <- get_tidy_estimates(
      model_fit = s1_pooled_results,
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
  table_1_to_append = s1_tidy_table,
  save_to = 'tidy_output_list.xlsx') 

##### Raw: Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'raw_output_list.xlsx')

append_results(
  output_file = output_file,
  table_1_to_append = s1_raw_table,
  save_to = 'raw_output_list.xlsx') 

##### Diagnostics 
# Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'diagnostics_list.xlsx')

append_results(output_file = output_file,
               table_1_to_append = s1_diagnostics_table,
               table_2_to_append = s1_vif_table,
               is_multisheet_workbook = TRUE,
               save_to = 'diagnostics_list.xlsx')

###### Individual files ----
# Save/export raw & tidy estimates into excel file & into folder with monthly date
setwd(paste0(output_path, 'working_folder/', dir_date))

#### Tidy and raw
writexl::write_xlsx(
  s1_raw_table, 
  paste0(
    "raw_treatment_halfway_glmer_",
    file_date, ".xlsx"))

writexl::write_xlsx(
  s1_tidy_table, 
  paste0(
    "tidy_treatment_halfway_glmer_",
    file_date, ".xlsx"))

#### Diagnostics
wb = openxlsx::createWorkbook()

diagnostics_list = list(
  'imputed_models_performance' = s1_diagnostics_table,
  'imputed_models_vif' = s1_vif_table)

# Add tables to different worksheets based on list's name
lapply(names(diagnostics_list), function(name){
  
  openxlsx::addWorksheet(wb, name)
  writeData(wb, name, diagnostics_list[[name]])
  
})

openxlsx::saveWorkbook(
  wb, paste0(
    'diagnostics_treatment_halfway_glmer',
    file_date, '.xlsx'),
  overwrite = TRUE)

##S2: Time/Treatment interaction effects -----------------------------------------

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
    wedge  = relevel(wedge, ref = c('wedge_1')))

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
  "cla_status ~ treatment_group * wedge + ", # FE for trt + time effects
  demographics, # adjust for person level demographics
  str_flatten(cluster_indicator), # adjust for time-varying cluster level indicators
  re
) # RE intercept 4 clusters

glmer_formula_2 = paste0( # simplified spec 
  "cla_status ~ treatment_group * wedge + ", # FE for trt + time effects
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
s2_glmer_models = with( 
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
  'Primary Sample: Time Treatment Effects - Imputed m5 - GLMER')

# Pooling results 
# As per Stef Van Buurren's workflow recs:
# https://stefvanbuuren.name/fimd/workflow.html
s2_pooled_results <- mice::pool(s2_glmer_models) # pool results

# Get summaries from pooled results
s2_summary = summary(s2_pooled_results) 

### Diagnostics -------------------------------------

# OPTIMIZERS
# Check how optimisers are doing
# Optimisers: variations of BOBYQA, Nelder-Mead, L-BFGS-B
# More info: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#introduction

#summary(warnings())
aa <- allFit(s2_glmer_models$analyses[[1]])
ss <- summary(aa) 

# Convert into table 
s2_ss_df = data.frame(
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

s2_ss_df <- s2_ss_df %>%
  pivot_wider(names_from = Optimizer, 
              values_from = Message)

s2_ss_df = s2_ss_df %>%
  dplyr::mutate(analysis_type = analysis_type,
                formula = formula,
                date = date) %>%
  dplyr::relocate(analysis_type, formula)

# Performance & fit indicators: AIC, BIC, R2...
s2_performance_df = performance::model_performance(
  s2_glmer_models$analyses[[1]])

s2_performance_df = s2_performance_df %>%
  dplyr::mutate(analysis_type = analysis_type, 
                formula = formula, 
                date = date) %>%
  dplyr::relocate(analysis_type, formula)

s2_diagnostics_table = dplyr::left_join(
  s2_ss_df,
  s2_performance_df,
  by = c('analysis_type', 'formula', 'date'))

# Check multicollinearity
s2_vif_table = performance::check_collinearity(
  s2_glmer_models$analyses[[1]]) 

s2_vif_table = s2_vif_table %>%
  dplyr::mutate(analysis_type = analysis_type, 
                formula = formula, 
                date = date) %>%
  dplyr::relocate(analysis_type, formula)

### Tidy up -------------------------------------------

# Tidy results into dataframes 
#1 Raw model estimates
s2_raw_table = s2_summary %>%
  dplyr::mutate(
    date = date,
    analysis_type = analysis_type,
    formula = formula) %>%
  dplyr::relocate(date, analysis_type, formula)

#2 Tidy model estimates 
s2_tidy_table <- get_tidy_estimates(
  model_fit = s2_pooled_results,
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
  table_1_to_append = s2_tidy_table,
  save_to = 'tidy_output_list.xlsx') 

##### Raw: Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'raw_output_list.xlsx')

append_results(
  output_file = output_file,
  table_1_to_append = s2_raw_table,
  save_to = 'raw_output_list.xlsx') 

##### Diagnostics 
# Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'diagnostics_list.xlsx')

append_results(output_file = output_file,
               table_1_to_append = s2_diagnostics_table,
               table_2_to_append = s2_vif_table,
               is_multisheet_workbook = TRUE,
               save_to = 'diagnostics_list.xlsx')

###### Individual files ----
# Save/export raw & tidy estimates into excel file & into folder with monthly date
setwd(paste0(output_path, 'working_folder/', dir_date))

#### Tidy and raw
writexl::write_xlsx(
  s2_raw_table, 
  paste0(
    "raw_time_treatment_effects_glmer_",
    file_date, ".xlsx"))

writexl::write_xlsx(
  s2_tidy_table, 
  paste0(
    "tidy_time_treatment_effects_glmer_",
    file_date, ".xlsx"))

#### Diagnostics
wb = openxlsx::createWorkbook()

diagnostics_list = list(
  'imputed_models_performance' = s2_diagnostics_table,
  'imputed_models_vif' = s2_vif_table)

# Add tables to different worksheets based on list's name
lapply(names(diagnostics_list), function(name){
  
  openxlsx::addWorksheet(wb, name)
  writeData(wb, name, diagnostics_list[[name]])
  
})

openxlsx::saveWorkbook(
  wb, paste0(
    'diagnostics_time_treatment_effects_glmer',
    file_date, '.xlsx'),
  overwrite = TRUE)

##S2: Widening of age gap  -----------------------------------------

### Data --------------------------------------------------------
### Formula --------------------------------------------------------
### Fit model --------------------------------------------------------
### Save outputs --------------------------------------------------------
