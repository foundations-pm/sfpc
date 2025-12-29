#------------------------------------------------#
#
# FAMILY SAFEGUARDING: STEPPED WEDGE ANALYSES ----
#
#------------------------------------------------#

# ANALYSIS: SENSITIVITY ANALYSES 2 ----

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

# S1: Change in intervention group definition ----------------------------------

## Load data ----
# Main sample: children referred to CSC during trial period
setwd(data_path)

data = readRDS(file = paste0(
  data_path, "/primary_outcome_sample_analytical_dataset_final.Rds")) 

## Analysis ----
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
data = data %>%
  dplyr::mutate( # eosp = end of study period
    eosp = as.Date( # add 18 months to DOB
      referral_date) %m+% months(18),
    la_treatment_start  = case_when(
      local_authority == 'walsall' ~ as.Date('2020-09-01'),
      local_authority == 'lancashire' ~ as.Date('2021-02-01'),
      local_authority == 'telford' ~ as.Date('2021-06-28'),
      local_authority == 'wandsworth' ~ as.Date('2022-01-24'),
      local_authority == 'swindon' ~ as.Date('2022-05-24')
      )
    ) 

#### 1 Alternative treatment definition 1 ----

# Derive the halfway point in the child's time spent in the trial
# To calculate the half study period date:
#1 use end of study period (eosp = the end of study period for each child)
#2 eosp = 18 months post referral OR time between referral date and date period of care commenced
#3 get halfway point between referral date and eosp date 
data = data %>%
  dplyr::mutate( 
    half_open_referral_date = 
      as.Date(referral_date) + (as.numeric( 
        as.Date(eosp) - as.Date(referral_date)) / 2))

# Derive treatment group called 'treatment halfway group'
# If LA treatment start == or is BEFORE halfway date, then treatment = 1, otherwise 0
data = data %>%
  dplyr::group_by(local_authority) %>%
  dplyr::mutate(
    treatment_half_way_group = ifelse(
      la_treatment_start <= half_open_referral_date, 1, 0))

#### 2 Alternative treatment definition 2 ----

# Derive treatment group where treatment = 1 for children who have spent at least 4 weeks 
# of their time in the trial within the treatment condition
data = data %>%
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

data %>% 
  dplyr::group_by(local_authority) %>%
  dplyr::summarise(n())

# Alternative treatment assignment method 1 
# Treatment / control group distribution  
data %>% 
  dplyr::group_by(treatment_half_way_group) %>%
  dplyr::summarise(n())

data %>% 
  dplyr::group_by(local_authority, treatment_half_way_group) %>%
  dplyr::summarise(n())

# Alternative treatment assignment method 2 
# Treatment / control group distribution 
data %>% 
  dplyr::group_by(treatment_4_weeks_group) %>%
  dplyr::summarise(n())

data %>% 
  dplyr::group_by(local_authority, treatment_4_weeks_group) %>%
  dplyr::summarise(n())

# ------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------ #

#S2: Time * intervention interaction -------------------------------------------

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

# Time * intervention group interaction 
formula = paste0( # fully-specified, per protocol
  "cla_status ~ intervention_group * wedge + ", # FE for trt + time effects
  demographics, # adjust for person level demographics
  str_flatten(cluster_indicator), # adjust for time-varying cluster level indicators
  re
) # RE intercept 4 clusters

print(formula)

## 1. Missing indicator analyses -----------------------------------------------

### 1.1 Load data -----------------

# Alternative sample: children who were born during their first referral during the trial
setwd(data_path)

s2_data = readRDS(file = paste0(
  data_path, "/primary_outcome_sample_analytical_dataset_final.Rds")) 

### 1.3 Fit model ----
analysis_type = 'Time * intervention - Missing indicator - GLMER'

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
tictoc::tic()

s2_glmer = lme4::glmer(
  as.formula(formula), 
  data = s2_data, 
  family = binomial #,
  #nAGQ= 0
)

tictoc::toc()

# Save model fit 
setwd(
  paste0(secondary_output_path, '/', dir_date, '/R Objects')
)  

saveRDS(
  s2_glmer,
  file= paste0(
    "glmer_fit_", 
    str_replace(
      janitor::make_clean_names(analysis_type),
      '_glmer', ''
    ),
    ".RData")
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
analysis_type = 'Time * intervention - Missing indicator - GLMER'

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
analysis_type = 'Time * intervention - Missing indicator - GLMER'

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

#### 2.3 Fit model ----
analysis_type = 'Time * intervention - Imputation m10 - GLMER'

# Fit model on imputed datasets with m= 10 and m=20
# Fitting models:
tictoc::tic()

s2_imp_model = with( 
  imputed_data_m10, 
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
  s2_imp_model,
  file= paste0(
    "glmer_fit_", 
    str_replace(
      janitor::make_clean_names(analysis_type),
      '_glmer', ''
    ),
    ".RData")
)

# Pool results & summary
s2_pooled_results <- mice::pool(s2_imp_model) # pool results
s2_summary = summary(s2_pooled_results) 

#### 2.4 Diagnostics ----
s2_glmer_ss_table = get_optimisers_warning_messages(
  glmer_model_fit = s2_imp_model[['analyses']][[1]],
  formula = formula,
  analysis_type = analysis_type)

# VIF table 
s2_glmer_vif_table = get_vif_table(
  model_fit = s2_imp_model[['analyses']][[1]],
  formula = formula,
  analysis_type = analysis_type)

# Performance & fit indicators: AIC, BIC, R2...
s2_glmer_performance_table = get_performance_table(
  s2_imp_model[['analyses']][[1]],
  formula = formula,
  analysis_type = analysis_type)

s2_glmer_diagnostics_table = dplyr::left_join(
  s2_glmer_ss_table,
  s2_glmer_performance_table,
  by = c('analysis_type', 'formula', 'date'))

#### 2.5 Tidy -----
# Tidy results into dataframes 

#1 Raw model estimates
s2_glmer_raw <- s2_summary %>%
  dplyr::mutate(
    date = date,
    analysis_type = analysis_type,
    formula = formula) %>%
  dplyr::relocate(date, analysis_type, formula)

#2 Tidy model estimates 
s2_glmer_tidy = get_tidy_estimates(
  model_fit = s2_pooled_results,
  analysis_type = analysis_type,
  formula = formula,
  date = date) 

#### 2.6 Save outputs ----

###### List ----
# Working directory to save outputs table 
setwd(main_output_path)
analysis_type = 'Time * intervention - Imputation m10 - GLMER'

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
analysis_type = 'Time * intervention - Imputation m10 - GLMER'

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