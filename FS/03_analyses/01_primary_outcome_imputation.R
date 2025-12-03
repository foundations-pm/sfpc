#------------------------------------------------#
#
# FAMILY SAFEGUARDING: STEPPED WEDGE ANALYSES ----
#
#------------------------------------------------#

# ANALYSIS: IMPUTATION ----

r_directory = 'C:/Users/PerrineMachuel/'

# Libraries 
{ source(paste0(r_directory, "Documents/SFPC/FS/config.R")) }

# Functions 
{ source(paste0(r_directory, "Documents/SFPC/FS/functions.R"))}

# Set-up  ----
sharepoint_path = paste0(r_directory,'Foundations/High-SFPC-Impact - Family Safeguarding')

# Data and output paths
data_path = paste0(sharepoint_path, '/Datasets/analytical_datasets')
output_path = paste0(data_path, '/main_sample_primary_outcome_imputed_data')

# Working directory
wd = paste0(r_directory, "Documents/SFPC/FS/")

# Dates 
date = format(Sys.Date(),"%Y/%m/%d") # date format to save within dataframes

file_date = format(Sys.Date(),"_%Y%b%d") # date format to save files

dir_date = format(Sys.Date(),"%B %Y") # date format to create directories

# Load data --------------------------------------------------------------------

# Main sample: children referred to CSC during trial period
setwd(data_path)

data = readRDS(file = paste0(
  data_path, "/primary_outcome_sample_analytical_dataset_final.Rds")) 

## Workplan
#0 Assess MNAR
#1 Multiple imputation: binary Norfolk var, regular using LA
#2 Multilevel imputation 
#3 Sensitivity checks: do the results change drastically including/excluding auxiliary variables 

# Missingness assessment---------------------------------------------

# Overall missing data
# Percent cases missing out of all
sum(is.na(data)) / (dim(data)[1] * dim(data)[2]) * 100 # 9.9%

# Percent children with at least 1 missing value 
sum(!complete.cases(data)) / nrow(data) * 100 
# 98.4% - very high, probs bc of % FSM, cla start date and cla number

# Complete missingness analysis:
# Get data with columns of interest only
covariates = c(
  'local_authority',
  'wedge',
  'month_year_referral',
  'intervention_group',
  'cla_status',
  'referral_no_further_action_clean',
  'unborn_flag',
  'age_at_referral_final',
  'gender_final',
  'ethnicity_final',
  'disability_status_clean',
  'number_of_previous_cpp_clean',
  'uasc_clean',
  'age_at_referral_numeric_final',
  'prop_cyp_eligible_and_claiming_for_fsm_out_of_all_pupils',
  'prop_white_british')

missing_data = data %>% 
  dplyr::select(any_of(covariates)) %>%
  dplyr::mutate(month_year_referral = as.character(month_year_referral))

# Percent children with at least 1 missing value 
# Without prop cyp FSM, cla start date and cla number
# nb children with missing cases / all children in sample * 100
(nrow(missing_data) - nrow(na.omit(missing_data[,-c(15)]))) / nrow(missing_data) * 100 # 16.3%

#NAs per column
colSums(is.na(missing_data))

# Checks nb & % NAs 
get_nas_per_column = function(df) {
  
  dplyr::tibble(
    column = names(df),
    missing = colSums(is.na(df)),
    percent_missing = round(missing/nrow(df) * 100, 2)
  )}

nas_per_col_table = get_nas_per_column(missing_data)

# Missing:
# 3.46% gender 
# 9% ethnicity 
# 6% UASC
# <1% disability
# 48% prop FSM eligible > dropping var from analyses

# Need to impute: gender, ethnicity, disability and UASC status

# Missingness analysis: MNAR checks ----

missing_data = dplyr::mutate(
  missing_data,
  is_missing_gender = ifelse(is.na(gender_final), 1, 0),
  is_missing_ethnicity = ifelse(is.na(ethnicity_final), 1, 0),
  is_missing_disability = ifelse(is.na(disability_status_clean), 1, 0),
  is_missing_uasc = ifelse(is.na(uasc_clean), 1, 0))

# MNAR with Cramer's V and Chi squared test
mnar_covariates = covariates[-c(13,14)]

missing_vars <- c(
  "is_missing_gender",
  "is_missing_ethnicity",
  "is_missing_disability",
  "is_missing_uasc"
)

# All combination of missing vars and MNAR covariates
grid = tidyr::expand_grid(
  missing_vars = missing_vars,
  mnar_covariates = mnar_covariates
)  

grid = grid %>% 
  dplyr:: mutate(base_missing = str_remove(missing_vars, "^is_missing_")) %>%
  dplyr::filter(!str_detect(mnar_covariates, base_missing)) %>%
  dplyr::select(-base_missing)

mnar_table <- purrr::pmap_dfr(
  
  grid,
  \(missing_vars, mnar_covariates) {
    check_mnar(
      data = missing_data,
      missing_var = missing_vars,
      covariate = mnar_covariates
    ) |>
      dplyr::mutate(
        missing_var = missing_vars,
        covariate = mnar_covariates)
  })

# Save table 
setwd(
  paste0(
    sharepoint_path, 
    '/Outputs/Primary analyses/Sample descriptives/Main sample'))

write.xlsx(
  mnar_table, 
  paste0('mnar_table_primary_outcome_main_sample', file_date, '.xlsx'))

# Subgroup analysis: check association within LAs 
la_grid = dplyr::filter(
  grid, 
  !missing_vars %in% c('is_missing_disability', 'is_missing_uasc'),
  mnar_covariates != 'local_authority')

clusters = unique(missing_data$local_authority)
names(clusters) = clusters

mnar_table_by_la <-  purrr::map_dfr(
  
  clusters,
  \(j) {
    data_sub <- missing_data |> 
      dplyr::filter(.data[['local_authority']] == j)
    
    purrr::pmap_dfr(
      
      la_grid,
      \(missing_vars, mnar_covariates) {
        
        check_mnar(
          data = data_sub,
          missing_var = missing_vars,
          covariate = mnar_covariates
        ) |>
          dplyr::mutate(
            missing_var = missing_vars,
            covariate = mnar_covariates)
      }) 
    
  }, .id = 'local_authorities')

# Multiple imputation -----------------------------------------

## Set-up -------
covariates = c(
  'unique_child_id',
  'referral_date',
  'month_year_referral',
  'local_authority',
  'wedge',
  'intervention_group',
  'cla_status',
  'referral_no_further_action_clean',
  'unborn_flag',
  'age_at_referral_final',
  'gender_final',
  'ethnicity_final',
  'disability_status_clean',
  'number_of_previous_cpp_clean',
  'uasc_clean',
  #'age_at_referral_numeric_final',
  #'prop_cyp_eligible_and_claiming_for_fsm_out_of_all_pupils',
  'prop_white_british')

# Refine model data:
# select only model predictors 
imp_data = data %>% 
  dplyr::select(any_of(covariates)) %>%
  dplyr::mutate(month_year_referral = as.character(month_year_referral))

# View missing data pattern
mice::md.pattern(imp_data)

# Get predictor matrix 
predm <- make.predictorMatrix(imp_data)

# Set which predictors should be used to impute 
# All but child id and referral date
predm[,"unique_child_id"] <- 0
predm[,"referral_date" ] <- 0

# Imp method: 
# Reg for binary(uasc and disability status)
# Poly reg for factor levels (gender and ethnicity)
meth <- make.method(imp_data) # all correct!

## Imputation m=10 ----

# Further resources:
# https://stats.stackexchange.com/questions/577135/iterations-in-multiple-imputation
# https://stefvanbuuren.name/fimd/sec-howmany.html 
# https://bookdown.org/mike/data_analysis/imputation-missing-data.html 
# https://www.econstor.eu/bitstream/10419/113873/1/79212409X.pdf 

imputed_data_m10 <- mice::mice(
  imp_data,
  m = 10, 
  method = meth,
  seed = 123, 
  predictorMatrix = predm,
  maxit = 10) # start with 10 iterations and see if convergence looks good

# Save imputed data
setwd(paste0(output_path, '/m10'))

miceadds::write.mice.imputation(
  imputed_data_m10, 
  name = "main_sample_m10_imputation", 
  include.varnames=TRUE,
  long=TRUE, 
  mids2spss=FALSE,
  dattype=NULL)

# Check logged events 
View(imputed_data_m10$loggedEvents)

# Check convergence 
convergence_m10 = plot(imputed_data_m10) 
# looks pretty good to me except for disability 
# probably due to the very few missing vars 

## Checks m10 ----
setwd(paste0(output_path, '/checks'))

# Check the imputed values
# Visualize observed vs imputed values 
imp_plot = propplot(
  imputed_data_m10, 
  label_size = 10,
  show_prop = TRUE,
  prop_size = 2)

ggsave('observed_v_imputed_data_m10.jpg')

imp_plot_trt = propplot(
  imputed_data_m10,
  formula = as.formula(
    paste0("gender_final + ethnicity_final + disability_status_clean",
           " + uasc_clean ~ intervention_group")),
  title = 'Proportion observed versus imputed data by intervention group',
  label_size = 7) 

ggsave('observed_v_imputed_data_by_intervention_group_m10.jpg')

imp_plot_la = propplot(
  imputed_data_m10, 
  formula = as.formula(
    paste0("gender_final + ethnicity_final + disability_status_clean",
           " + uasc_clean ~ local_authority")),
  title = 'Proportion observed versus imputed data by local authority',
  label_size = 5) 

ggsave('observed_v_imputed_data_by_la_m10.jpg')

## Imputation m=20 ----

imputed_data_m20 <- mice::mice(
  imp_data,
  m = 20, 
  method = meth,
  seed = 123, 
  predictorMatrix = predm,
  maxit = 10) # start with 10 iterations and see if convergence looks good

# Save imputed data
setwd(paste0(output_path, '/m20'))

miceadds::write.mice.imputation(
  imputed_data_m20, 
  name = "main_sample_m20_imputation", 
  include.varnames=TRUE,
  long=TRUE, 
  mids2spss=FALSE,
  dattype=NULL)

# Check logged events 
View(imputed_data_m20$loggedEvents)

# Check convergence 
convergence_m20 = plot(imputed_data_m20) 
# looks pretty good to me except for disability 
# probably due to the very few missing vars 

## Checks m20 ----
setwd(paste0(output_path, '/checks'))

# Check the imputed values
# Visualize observed vs imputed values 
imp_plot = propplot(
  imputed_data_m20, 
  label_size = 10,
  show_prop = TRUE,
  prop_size = 2)

ggsave('observed_v_imputed_data_m20.jpg')

imp_plot_trt = propplot(
  imputed_data_m20,
  formula = as.formula(
    paste0("gender_final + ethnicity_final + disability_status_clean",
           " + uasc_clean ~ intervention_group")),
  title = 'Proportion observed versus imputed data by intervention group',
  label_size = 7) 

ggsave('observed_v_imputed_data_by_intervention_group_m20.jpg')

imp_plot_la = propplot(
  imputed_data_m20, 
  formula = as.formula(
    paste0("gender_final + ethnicity_final + disability_status_clean",
           " + uasc_clean ~ local_authority")),
  title = 'Proportion observed versus imputed data by local authority',
  label_size = 5) 

ggsave('observed_v_imputed_data_by_la_m20.jpg')