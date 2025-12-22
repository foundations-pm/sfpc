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
output_path = paste0(sharepoint_path, '/Outputs/Primary analyses/2. Main analysis')

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

# Prep data for missing indicator analysis
missing_indicator_data = dplyr::mutate(
  data,
  across(
    .cols = c('gender_final','ethnicity_final', 'disability_status_clean', 'uasc_clean'),
    .fns = ~ ifelse(is.na(.x), 'Missing', .x)
  )
)

## S1: Factors identified at the start of the assessment ------------------------

factor_cols = data %>% dplyr::select(any_of(contains('factors_'))) %>% colnames()

data = dplyr::mutate(
  data, 
  risk_factors_identified_post_assessment = case_when(
    if_any(factor_cols, ~ .x == 'Yes') ~ 'Yes',
    TRUE ~ 'No')
  )

s1_data = dplyr::filter(
  data, 
  risk_factors_identified_post_assessment == 'Yes')

nrow(s1_data) # 10901

## S1: without unborn -----------------------------------------------------------

# Check sample characteristics
covariates = c(
  'local_authority',
  'wedge',
  'intervention_group',
  'cla_status',
  'age_at_referral_final',
  'gender_final',
  'ethnicity_final',
  'disabled_status_clean',
  'uasc_clean',
  'number_of_previous_cpp_clean')

# Check NAs
map(s1_data, ~sum(is.na(.)))

# General sample distribution
s1_data_desc = s1_data %>%
  dplyr::select(any_of(covariates)) %>%
  dplyr::mutate(
    intervention_group = as.character(intervention_group),
    cla_status = as.character(cla_status)) 

sample_desc_table = s1_data_desc %>%
  describe(class = 'categorical') 

# Describing sample characteristics by LA
sample_desc_la_table = s1_data_desc %>%
  dplyr::group_by(local_authority) %>%
  describe(class = 'categorical',
           group = 'local_authority')

# Describing characteristics of those who were referred 
# during control vs intervention periods 
sample_desc_exposure_table = s1_data_desc %>%
  dplyr::group_by(intervention_group) %>%
  describe(class = 'categorical',
           group = 'intervention_group') 

# Describing characteristics of those who became looked after within 18 months 
sample_desc_outcome_table = s1_data_desc %>%
  dplyr::select(any_of(covariates)) %>%
  dplyr::mutate(intervention_group = as.character(intervention_group),
                cla_status = as.character(cla_status)) %>%
  dplyr::group_by(cla_status) %>%
  describe(class = 'categorical',
           group = 'cla_status') 

# Outcome distribution by wedge
prop_outcome_by_wedge = s1_data_desc %>%
  dplyr::group_by(wedge, cla_status) %>% 
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(wedge) %>%
  dplyr::mutate(freq = count/sum(count)) 

# Baseline outcome distribution by wedge by LA
prop_outcome_by_la_wedge = s1_data_desc %>%
  dplyr::group_by(local_authority, wedge, cla_status) %>% 
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(local_authority, wedge) %>%
  dplyr::mutate(freq = count/sum(count)) %>%
  dplyr::arrange(cla_status, local_authority, wedge)

sample_desc_list = list(
  'Sample description' = sample_desc_table,
  'Sample description by LA' = sample_desc_la_table,
  'Treatment group' = sample_desc_exposure_table,
  'Outcome group' = sample_desc_outcome_table,
  'Outcome freq by period' = prop_outcome_by_wedge,
  'Outcome freq by period and LA' = prop_outcome_by_la_wedge)

sample_desc_list = lapply(
  sample_desc_list, function(table){
    
    table%>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        freq = round(freq,2),
        count = 5*round(`count`/5),
        count = ifelse(count <= 5, '[z]', count))
    
  })

# Save descriptives
setwd(paste0(output_path, '/descriptives'))

# Save outputs
wb = openxlsx::createWorkbook()

# Add tables to different worksheets based on list's name
lapply(names(sample_desc_list), function(name){
  
  openxlsx::addWorksheet(wb, name)
  writeData(wb,
            name,  
            sample_desc_list[[name]])
  
})

openxlsx::saveWorkbook(wb, 'previous_cp_sample_description.xlsx',
                       overwrite = TRUE)


