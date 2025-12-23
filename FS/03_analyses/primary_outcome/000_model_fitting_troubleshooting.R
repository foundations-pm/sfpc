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

# Prep data:
# Prep data for missing indicator analysis
missing_indicator_data = dplyr::mutate(
  data,
  across(
    .cols = c('gender_final','ethnicity_final', 'disability_status_clean', 'uasc_clean'),
    .fns = ~ ifelse(is.na(.x), 'Missing', .x)
  )
)

### Troubleshooting ---------------------------------

# TROUBLESHOOTING THE TIME IT TAKES TO RUN GLMER 

#### Std fitting time ----
tictoc::tic()

m_test = lme4::glmer(
  as.formula(formula), 
  data = data,
  family = binomial)

tictoc::toc() # about 9 minutes?!

#### Without unborn ----
tictoc::tic()

m_test = lme4::glmer(
  as.formula(formula), 
  data = data[data$unborn_flag == 'born',],
  family = binomial)

tictoc::toc()  # about 11 minutes?!

#### Without NAs ----
# Without missing - using missing indicator data only
tictoc::tic()

m_test = lme4::glmer(
  as.formula(formula), 
  data = missing_indicator_data,
  family = binomial)

tictoc::toc()  # about 15 minutes?!

#### Sample down  ----
idx = sample(nrow(data), size = 10000)
subdat = data[idx, ]

tictoc::tic()

m_test = lme4::glmer(
  as.formula(formula), 
  data = subdat,
  family = binomial)

tictoc::toc()  # about 3 minutes 


#### Check optimiser performance ----

# Need to investigate this convergence issue 
# Look at: 
# 1 Multicollinearity 
# 2 Perfect separation

# For code below, need to have saved model objects from AllFits()
# In environment 
m2_ss_list$`Primary sample - Imputed m10 - GLMER`$which.OK
m2_ss_list$`Primary sample - Imputed m10 - GLMER`$llik
m2_ss_list$`Primary sample - Imputed m10 - GLMER`$fixef
m2_ss_list$`Primary sample - Imputed m10 - GLMER`$sdcor

# Check gradient
g <- m2_list$`Primary sample - Imputed m10 - GLMER`$analyses[[1]]@optinfo$derivs$gradient
max(abs(g)) # 0.006608379

#### Change optimiser ----

# Use nAGQ = 0
tictoc::tic()

m_test = lme4::glmer(
  as.formula(formula), 
  data = data,
  family = binomial,
  nAGQ= 0#,
  #control = glmerControl(optimizer = "bobyqa")
  )

tictoc::toc() # absolutely instantaneous: 2.53 sec

# Use bobyqa
tictoc::tic()

m_test = lme4::glmer(
  as.formula(formula), 
  data = data,
  family = binomial,
  #nAGQ= 0,
  control = glmerControl(optimizer = "bobyqa")
)

tictoc::toc() 
# not instantaneous 
# 6mins, better than usual
# However, warning message = 
#Warning message:
#  In commonArgs(par, fn, control, environment()) :
#  maxfun < 10 * length(par)^2 is not recommended.

# Combine both
tictoc::tic()

m_test = lme4::glmer(
  as.formula(formula), 
  data = data,
  family = binomial,
  nAGQ= 0,
  control = glmerControl(optimizer = "bobyqa")
)

tictoc::toc() # instantaneous, 3.39 sec


