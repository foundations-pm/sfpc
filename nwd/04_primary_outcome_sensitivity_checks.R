# Primary outcome analysis for No Wrong Doors RCT DR1 ----

# Set up  ----

#Paths
user_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(user_directory,'Foundations/High-SFPC-Impact - ')

# Data and output paths
data_path = paste0(sharepoint_path, 'QA/outputs/')

output_path = paste0(sharepoint_path,
                     'QA/outputs/model_outputs/sensitivity_analyses/')

# Dates
date = format(Sys.Date(),"%Y/%m/%d") # date format to save within dataframes

file_date = format(Sys.Date(),"_%Y%b%d") # date format to save files

dir_date = format(Sys.Date(),"%B %Y") # date format to create directories

# Set up folders to save output findings 
# in a neat an organised manner 
# Save individual files in a new directory
# Named after the month when the analyses were conducted
main_dir = output_path

sub_dir = dir_date

if(!dir.exists(file.path(paste0(main_dir, sub_dir)))){
  
  dir.create(file.path(main_dir, sub_dir)) # create new dir
  paste0("Creating new directory: '", sub_dir,"'")# confirm new dir is created
  
} else { 
  
  cat(
    crayon::green(
      crayon::bold(
        paste0("Directory '", sub_dir, "' already exists."))))
  
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

# Generic formula -------------------------------------------------------------------------------

setwd(output_path)

# Prep formula 
demographics = paste('age_at_referral_cat',
                     'gender',
                     'ethnicity_agg',
                     'disabled_status',
                     'unaccompanied_asylum_seeker',
                     'number_of_previous_child_protection_plans',
                     sep = " + ")

re = " + (1 | local_authority)"

cluster_indicator = str_flatten(
  c(" + prop_white_british" #,
    #" + turnover_rate_fte",
    #" + population_0_to_17" #,
    #" + splines::ns(cla_rate_per_10_000_children, df = 5)" #,
    #" + splines::ns(cpp_rate_per_10_000_children, df = 5)" #,
    #" + splines::ns(cin_rate_per_10_000_children, df = 5)"
  ))

formula = paste0(
  "cla_status ~ treatment_group + wedge + ", # FE for trt + time effects
  demographics, # adjust for person level demographics
  cluster_indicator, # adjust for time-varying cluster level indicators
  re
) # RE intercept 4 clusters

# Sensitivity analyses ------------------------------------------------------------------

## S1: Children with CP > 0 ------------


#### Data --------------------------------------------
s_data = dplyr::filter(
  data, 
  number_of_previous_child_protection_plans != '0')

# Check sample characteristics
covariates = c(
  'local_authority',
  'wedge',
  'treatment_group',
  'cla_status',
  'age_at_referral_cat',
  'gender',
  'ethnicity_agg',
  'disabled_status',
  'unaccompanied_asylum_seeker',
  'referral_no_further_action')

model_desc_table = s_data %>%
  select(any_of(covariates)) %>%
  mutate(treatment_group = as.character(treatment_group),
         cla_status = as.character(cla_status)) %>%
  describe(class = 'categorical') %>%
  mutate(count = ifelse(count < 5, '[z]', count))

# No UASC in the CPP only cohort
# Removing UASC from formula

# Check corr of CPP with outcome
chisq.test(data$cla_status, 
           data$number_of_previous_child_protection_plans)

CramerV(data$cla_status, 
        data$number_of_previous_child_protection_plans)

#### Formula --------------------------------
demographics = paste('age_at_referral_cat',
                     'gender',
                     'ethnicity_agg',
                     'disabled_status',
                     #'unaccompanied_asylum_seeker',
                     'number_of_previous_child_protection_plans',
                     sep = " + ")

s1_glmer_formula = paste0(
  "cla_status ~ treatment_group + wedge + ", # FE for trt + time effects
  demographics, # adjust for person level demographics
  cluster_indicator, # adjust for time-varying cluster level indicators
  re
) # RE intercept 4 clusters

s1_glm_formula = paste0(
  "cla_status ~ treatment_group + wedge + local_authority + ", # FE for trt + time effects
  demographics, # adjust for person level demographics
  cluster_indicator#, # adjust for time-varying cluster level indicators
  #re
) # RE intercept 4 clusters

####Fit model -------------------------------------

# Fit mixed model
s1_glmer = lme4::glmer(
  as.formula(s1_glmer_formula), 
  data = s_data, 
  family = binomial)

s1_m_summary = summary(s1_glmer)

# fit logistic GLM model to compare
s1_glm = stats::glm(
  as.formula(s1_glm_formula),
  data = s_data, 
  family = binomial)

s1_tidy_glm = broom::tidy(s1_glm)

# Get raw results 
s1_raw_m = get_raw_estimates(
  summary_model_fit = s1_m_summary,
  analysis_type = 'Sensitivity: Children with CPP > 0',
  formula = formula,
  date = date)

# Get tidy results 
s1_tidy_m = get_tidy_estimates(
  model_fit = s1_glmer, 
  analysis_type = 'Sensitivity: Children with CPP > 0',
  formula = formula,
  date = date)

# Get performance checks 
performance_table = performance::model_performance(s1_glmer)

#### Save outputs -----------------------------------------
writexl::write_xlsx(
  s1_raw_m,
  paste0(
    main_dir, sub_dir, # saves file into the Month/Year folder when the analyses were conducted
    "/raw_S1_Children_CPP_glmer_model", # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
    file_date, ".xlsx"))

writexl::write_xlsx(
  s1_tidy_m, 
  paste0(main_dir, sub_dir, # saves file into the Month/Year folder when the analyses were conducted
         "/tidy__S1_Children_CPP_glmer_model", # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
         file_date, ".xlsx"))

# Save sensitivity results, and append to list of outputs
setwd(data_path)

main_dir = paste0(data_path, 'model_outputs/')
  
append_results(
  output_file = 'tidy_output_list.xlsx',
  table_to_append = s1_tidy_m,
  save_to = 'tidy_output_list.xlsx')

append_results(
  output_file = 'raw_output_list.xlsx',
  table_to_append = s1_raw_m,
  save_to = 'raw_output_list.xlsx')

performance_table = dplyr::mutate(
  performance_table,
  Score_log = as.character(Score_log))

append_results( 
  output_file = "performance_output_list.xlsx",
  table_to_append = performance_table,
  save_to = "performance_output_list.xlsx")

##S2: Children with an open CP only -------------------------------------------------------------------------------

### Data --------------------------------------------------------
### Formula --------------------------------------------------------
### Fit model --------------------------------------------------------
### Save outputs --------------------------------------------------------


##S3: Change in treatment assignment -----------------------------------------

### Data --------------------------------------------------------
### Formula --------------------------------------------------------
### Fit model --------------------------------------------------------
### Save outputs --------------------------------------------------------

##S4: Time/Treatment interaction effects -----------------------------------------

### Data --------------------------------------------------------
### Formula --------------------------------------------------------
### Fit model --------------------------------------------------------
### Save outputs --------------------------------------------------------

