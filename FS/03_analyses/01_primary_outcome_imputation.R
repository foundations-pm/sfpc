#------------------------------------------------#
#
# FAMILY SAFEGUARDING: STEPPED WEDGE ANALYSES ----
#
#------------------------------------------------#

## ANALYSIS: IMPUTATION ----

r_directory = 'C:/Users/PerrineMachuel/'

# Libraries 
{ source(paste0(r_directory, "Documents/SFPC/FS/config.R")) }

# Functions 
{ source(paste0(r_directory, "Documents/SFPC/FS/functions.R"))}

## Set-up  ----
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

## Missingness assessment---------------------------------------------

# Get data with columns of interest only
covariates = c(
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
  'age_at_referral_numeric_final',
  'prop_cyp_eligible_and_claiming_for_fsm_out_of_all_pupils',
  'prop_white_british')

model_data = dplyr::select(data, any_of(covariates))

colSums(is.na(model_data))

# Checks nb & % NAs 
get_nas_per_column = function(df) {
  
  dplyr::tibble(
    column = names(df),
    missing = colSums(is.na(df)),
    percent_missing = round(missing/nrow(df) * 100, 2)
  )}

nas_per_col_table = get_nas_per_column(model_data)

# Missing:
# 3.46% gender 
# 9% ethnicity 
# 6% UASC
# <1% disability
# 48% prop FSM eligible > dropping var from analyses

# Need to impute: gender, ethnicity, disability and UASC status

## Missingness analysis: MNAR checks ----

missing_data = dplyr::mutate(
  model_data,
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
  missing_covariate = missing_vars,
  auxiliary = mnar_covariates
)  

grid = grid %>% 
  dplyr:: mutate(base_missing = str_remove(missing_covariate, "^is_missing_")) %>%
  dplyr::filter(!str_detect(auxiliary, base_missing)) %>%
  dplyr::select(-base_missing)

mnar_table <- purrr::pmap_dfr(
  grid,
  \(missing_covariate, auxiliary) {
    check_mnar(
      data = missing_data,
      missing_covariate = missing_covariate,
      auxiliary = auxiliary
    ) |>
      dplyr::mutate(
        missing_covariate = missing_covariate,
        auxiliary = auxiliary)
  })

View(mnar_table)

gender_mnar_table = purrr::map_dfr(
  missingness_analysis_covariates[-8], function(cov){ # removing gender
    
    check_mnar(data = missing_data,
               missing_covariate = 'is_missing_gender',
               auxiliary = cov) })

View(gender_mnar_table)

# Subgroup analysis: check association within LAs 
clusters = unique(missing_data$local_authority)

mnar_table_la = purrr::map_dfr(
  clusters, function(c){
    
    la_data = dplyr::filter(
      missing_data, 
      local_authority == c)
    
    table = purrr::map_dfr(
      covariates[-2], function(cov){
        
        check_mnar(data = la_data,
                   missing_covariate = 'is_missing_ethnicity',
                   auxiliary = cov) }) 
    
    table %>% 
      dplyr::mutate(cluster = c) %>%
      relocate(cluster) })

mnar_table_la %>%
  arrange(cluster, strength_of_association, desc(stat_significance)) %>%
  View()

#View(mnar_table_la)

# Cont-Cat corr
#test = stats::glm(is_missing_ethnicity ~ local_authority + turnover_rate_fte,
#                  family = binomial,
#                  data = missing_data)

#test_tb = broom::tidy(test, exponentiate = TRUE)

# LA missingness checks: crosstabs
missing_data %>%
  dplyr::filter(local_authority == 'norfolk') %>%
  dplyr::group_by(disabled_status, is_missing_ethnicity) %>%
  dplyr::summarise(n())

missing_data %>%
  dplyr::filter(local_authority == 'norfolk') %>%
  dplyr::group_by(unaccompanied_asylum_seeker, is_missing_ethnicity) %>%
  dplyr::summarise(n())

missing_data %>%
  dplyr::filter(local_authority == 'norfolk') %>%
  dplyr::group_by(number_of_previous_child_protection_plans, 
                  is_missing_ethnicity) %>%
  dplyr::summarise(n())

missing_data %>%
  dplyr::filter(local_authority == 'norfolk') %>%
  dplyr::group_by(wedge, 
                  is_missing_ethnicity) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(freq = round(count/sum(count),2))

# 11% missing at baseline; then 6, 4, 6 and 8%
# In general, missingness in N is
# in those without previous CPP, not disabled, not UASC 

## Multiple imputation -----------------------------------------

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
  'number_of_previous_child_protection_plans',
  'referral_no_further_action',
  'prop_white_british',
  'population_0_to_17',
  'turnover_rate_fte',
  'new_referrals_rate_per_10_000_children',
  'cin_rate_per_10_000_children',
  'cpp_rate_per_10_000_children',
  'cla_rate_per_10_000_children')

# Refine model data:
# select only model predictors 
model_data = data %>% select(
  child_id, referral_date, cla_status,
  any_of(covariates)) %>%
  mutate(
    wedge = relevel(
      factor(wedge), ref = 'baseline'),
    is_norfolk = ifelse(
      local_authority == 'norfolk', 1, 0),
    splines_cla_rates = data.frame(splines::ns(
      cla_rate_per_10_000_children, df = 5)),
    splines_cin_rates = data.frame(splines::ns(
      cin_rate_per_10_000_children, df = 5)),
    splines_cpp_rates = data.frame(splines::ns(
      cpp_rate_per_10_000_children, df = 5))) %>%
  mutate(across(.cols = contains('splines'),
                .fns = .as.numeric)) %>%
  mutate(gender = relevel(
    factor(as.character(gender)), ref = 'Male')) %>%
  relocate(is_norfolk, .after = local_authority) 

model_data = tidyr::unpack(
  model_data, cols=c(
    splines_cla_rates,
    splines_cpp_rates,
    splines_cin_rates),
  names_sep = '_')

# View missing data pattern
mice::md.pattern(model_data)

# Get predictor matrix 
predm <- make.predictorMatrix(model_data)

### Imputation m=5 ----

# Further resources:
# https://stats.stackexchange.com/questions/577135/iterations-in-multiple-imputation
# https://stefvanbuuren.name/fimd/sec-howmany.html 
# https://bookdown.org/mike/data_analysis/imputation-missing-data.html 
# https://www.econstor.eu/bitstream/10419/113873/1/79212409X.pdf 

#1 Impute data: use binary var for Norfolk

# Set which predictors should be used to impute 
# missing ethnicity values
rates = model_data %>% dplyr::select(
  contains('rate')) %>% colnames()

predm[,"child_id"] <- 0
predm[,"referral_date" ] <- 0
predm[, "local_authority"] <- 0
predm[, "population_0_to_17"] <- 0
predm[, "prop_white_british"] <- 0
predm[, rates] <- 0

imputed_data_m5 <- mice::mice(
  model_data,
  m = 5, 
  method = 'polyreg',
  seed = 123, 
  predictorMatrix = predm,
  maxit = 100)

# Check logged events 
imputed_data_m5$loggedEvents

# Save imputed data
setwd(paste0(output_path, "imputed_datasets/"))

miceadds::write.mice.imputation(
  imputed_data_m5, 
  name = "Norfolk_binary_single_level_m5_imputation", 
  include.varnames=TRUE,
  long=TRUE, 
  mids2spss=FALSE,
  dattype=NULL)

# Check the imputed values for 'ethnicity'
#imputed_data_m5$imp$ethnicity

# Check convergence 
plot(imputed_data_m5)

# Visualize observed vs imputed values for ethnicity
imp_plot = propplot(imputed_data_m5, 
                    label_size = 10,
                    show_prop = TRUE,
                    prop_size = 2)

imp_plot_trt = propplot(
  imputed_data_m5,
  ethnicity_agg ~ treatment_group,
  label_size = 7) 

imp_plot_la = propplot(
  imputed_data_m5, 
  ethnicity_agg ~ local_authority,
  label_size = 5) 

# Extract completed imputed datasets and check consistency
#complete_data_1 <- complete(imputation_model, 1)  # First imputed dataset
#complete_data_2 <- complete(imputation_model, 2)  # Second imputed dataset

# Check summaries
#summary(complete_data_1$ethnicity_agg)
#summary(complete_data_2$ethnicity_agg)

### Imputation m=10 ----

# Sensitivity check: Increase the number of imputations to 10
imputed_data_m10 <- mice(
  model_data, 
  m = 10,           # Number of multiple imputations
  method = c('ethnicity_agg' = 'polyreg'),  # Predictive mean matching (appropriate for mixed data)
  predictorMatrix = predm,
  maxit = 100,      # Maximum iterations for convergence
  seed = 123)

# Check logged events 
imputed_data_m10$loggedEvents

# Save imputed data
setwd(paste0(output_path, "imputed_datasets/"))

miceadds::write.mice.imputation(
  imputed_data_m10, 
  name = "Norfolk_binary_single_level_m10_imputation", 
  include.varnames=TRUE,
  long=TRUE, 
  mids2spss=FALSE,
  dattype=NULL)

# Compare summaries of the two imputation models
summary(imputed_data_m5)
summary(imputed_data_m10)



