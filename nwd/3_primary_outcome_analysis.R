# Primary outcome analysis for No Wrong Doors RCT DR1 ----

# Set up  ----

#Paths
user_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(user_directory,'Foundations/High-SFPC-Impact - ')

# Data and output paths
data_path = paste0(sharepoint_path, 'QA/processing/linked_data/')

output_path = paste0(sharepoint_path, 'QA/outputs/')

# Dates
date = format(Sys.Date(),"%Y/%m/%d") # date format to save within dataframes

file_date = format(Sys.Date(),"_%Y%b%d") # date format to save files

dir_date = format(Sys.Date(),"%B %Y") # date format to create directories

# Set up folders to save output findings 
# in a neat an organised manner 
# Save individual files in a new directory
# Named after the month when the analyses were conducted
main_dir = paste0(
  output_path, "model_outputs/")

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

# Load data ----

# 1 load data 
data <- readRDS(file = paste0(
  output_path, 'primary_analysis_analytical_dataset_V2.Rds'))

# Descriptives ----

## Sample demographics ----
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
  'cla_cin_cpp_rate_per_10_000_children')

model_desc_table = data %>%
  select(any_of(covariates)) %>%
  mutate(treatment_group = as.character(treatment_group),
         cla_status = as.character(cla_status)) %>%
  describe(class = 'categorical') %>%
  mutate(count = ifelse(count < 5, '[z]', count))

model_desc_la_table = data %>%
  select(any_of(covariates)) %>%
  mutate(treatment_group = as.character(treatment_group),
         cla_status = as.character(cla_status)) %>%
  group_by(local_authority) %>%
  describe(class = 'categorical',
           group = 'local_authority') %>%
  mutate(count = ifelse(count < 5, '[z]', count))

writexl::write_xlsx(
  model_desc_table, 
  paste0(
    output_path,
    "model_outputs/",
    "nwd_model_sample_descriptives.xlsx"))

writexl::write_xlsx(
  model_desc_la_table, 
  paste0(
    output_path,
    "model_outputs/",
    "nwd_model_sample_descriptives_by_la.xlsx"))

## Outcome descriptives ----

outcome_desc_for_tavistock = data %>% 
  dplyr::group_by(local_authority, wedge) %>%
  dplyr::summarise(
    children_eligible_referred = n(), # total nb of children referred during wedge
    children_referred_with_previous_cpp_plans = sum(
      number_of_previous_child_protection_plans != "0"), 
    children_looked_after = sum( # total number of children looked after during wedge
      !is.na(date_period_of_care_commenced)),
    children_referred_who_became_looked_after_within_18_months= sum(
      cla_status),
    test = sum(cla_status),
    children_referred_with_previous_cpp_and_who_became_looked_after_during_the_trial = sum(
      cla_status == 1 & number_of_previous_child_protection_plans != "0")) %>% # total number experiencing the outcome out of the children referred during wedge
  dplyr::mutate( # cumulative numbers
    cumulative_number_of_eligible_children = cumsum(
      children_eligible_referred),
    cumulative_number_of_eligible_children_with_previous_cpp_plans = cumsum(
      children_referred_with_previous_cpp_plans),
    cumulative_number_of_children_in_care = cumsum(
      children_looked_after),
    cumulative_number_of_children_experiencing_outcome = cumsum(
      children_referred_who_became_looked_after_within_18_months),
    cumulative_number_of_children_referred_with_previous_cpp_and_who_became_looked_after_during_the_trial = cumsum(
      children_referred_with_previous_cpp_and_who_became_looked_after_during_the_trial)) %>%
  ungroup() %>%
  dplyr::mutate(across(
    .cols = where(is.numeric), 
    .fns = ~ ifelse(.x < 10, '[z]', .x))) # suppression checks


# Imputation ----

## Workplan
#0 Assess MNAR
#1 Multiple imputation: binary Norfolk var, regular using LA
#2 Multilevel imputation 
#3 Sensitivity checks: do the results change drastically including/excluding auxiliary variables 

## Missingness assessment----

# MNAR with Cramer's V and Chi squared test

missing_data = dplyr::mutate(
  data,
  is_missing_ethnicity = ifelse(is.na(ethnicity_agg), 1, 0),
  is_missing_gender = ifelse(gender == 'Other', 1, 0))

covariates = c(
  'cla_status',
  'local_authority',
  'wedge',
  'treatment_group',
  'age_at_referral_cat',
  'gender',
  'disabled_status',
  'unaccompanied_asylum_seeker',
  'number_of_previous_child_protection_plans',
  'referral_no_further_action')

mnar_table = purrr::map_dfr(
  covariates, function(cov){
    
    check_mnar(data = missing_data,
               missing_covariate = 'is_missing_ethnicity',
               auxiliary = cov) })

View(mnar_table)

# Subgroup analysis: check assocation within LAs 
clusters = unique(missing_data$local_authority)

mnar_table_la = purrr::map_dfr(
  clusters, function(c){
    
    la_data = filter(
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

View(mnar_table_la)

# LA missingness checks: crosstabs
missing_data %>%
  filter(local_authority == 'norfolk') %>%
  dplyr::group_by(disabled_status, is_missing_ethnicity) %>%
  dplyr::summarise(n())

missing_data %>%
  filter(local_authority == 'norfolk') %>%
  dplyr::group_by(unaccompanied_asylum_seeker, is_missing_ethnicity) %>%
  dplyr::summarise(n())

missing_data %>%
  filter(local_authority == 'norfolk') %>%
  dplyr::group_by(number_of_previous_child_protection_plans, 
                  is_missing_ethnicity) %>%
  dplyr::summarise(n())

missing_data %>%
  filter(local_authority == 'norfolk') %>%
  dplyr::group_by(wedge, 
                  is_missing_ethnicity) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(freq = round(count/sum(count),2))

# 11% missing at baseline; then 6, 4, 6 and 8%
# In general, missingness in N is
# in those without previous CPP, not disabled, not UASC 

## Multiple imputation ---- 

covariates = c(
  'local_authority',
  'wedge',
  'treatment_group',
  'age_at_referral_cat',
  'gender',
  'ethnicity_agg',
  'disabled_status',
  'unaccompanied_asylum_seeker',
  'number_of_previous_child_protection_plans',
  'referral_no_further_action',
  'prop_white_british')

# Refine model data:
# select only model predictors 
model_data = data %>% select(
  child_id, referral_date, cla_status,
  any_of(covariates), contains('rate_per')) %>%
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
  filter(gender != 'Other') %>%
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
splines = model_data %>% dplyr::select(
  contains('splines')) %>% colnames()

predm[,"child_id"] <- 0
predm[,"referral_date" ] <- 0
predm[, "local_authority"] <- 0
predm[, splines] <- 0

imputed_data_m5 <- mice::mice(
  model_data,
  m = 5, 
  method = 'polyreg',
  seed = 123, 
  predictorMatrix = predm,
  maxit = 10)

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
  maxit = 10,      # Maximum iterations for convergence
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

## Hot deck imputation ----
#TBC

# Performance checks
#1 Convergence Diagnostics
#2 Assessing Imputed Values - not doing
#3 Comparing Distributions of Imputed and Observed Data
#4 Check for Consistency Across Imputed Datasets - not doing 
#5 Model Fit on Imputed Data
#6 Pooling Results
#7 Sensitivity Analyses

# Fit models ----

# Rationale for bootstrapped standard errors:
# https://academic.oup.com/ije/article/47/1/321/4091562

# https://trialsjournal.biomedcentral.com/articles/10.1186/s13063-016-1571-2 
# Conversely, a cluster-level analysis, or a mixed-effects model or GEE with 
# a small-sample correction led to much wider confidence intervals and larger P values, 
# which more appropriately reflected the uncertainty around 
# the size of the treatment effect estimate.

##1 Complete case & MI method ----
# MI = missing indicator 

setwd(output_path)

# Prep formula 
demographics = paste('age_at_referral_cat',
                     'gender',
                     'ethnicity_agg',
                     'disabled_status',
                     'unaccompanied_asylum_seeker',
                     'number_of_previous_child_protection_plans',
                     #'referral_no_further_action',
                     sep = " + ")

re = " + (1 | local_authority)"

cluster_indicator = c(
  " + prop_white_british"#,
  #" + splines::ns(cla_rate_per_10_000_children, df = 5)" #,
  #" + splines::ns(cpp_rate_per_10_000_children, df = 5)" #,
  #" + splines::ns(cin_rate_per_10_000_children, df = 5)"
  )

formula = paste0(
  "cla_status ~ treatment_group + wedge + ", # FE for trt + time effects
  demographics, # adjust for person level demographics
  cluster_indicator, # adjust for time-varying cluster level indicators
  re
) # RE intercept 4 clusters

# Prep data:
# Prep data for missing indicator analysis
missing_indicator_data = mutate(
  data,
  ethnicity_agg = ifelse(
    is.na(ethnicity_agg), 'Missing', ethnicity_agg))

##### Fit model ----

# Fit model on standard data: complete case analysis
# Fit model on data with missing indicator recoded: missing indicator analysis
m1_list = lapply(c('data', 'missing_indicator_data'), function(dataset){
  
  df = get(dataset)
  
  lme4::glmer(
    as.formula(formula), 
    data = df, #[data$gender != 'Other', ],
    family = binomial)
  
})

# Set standard names to keep track of which model is which
# Names will be used to provide summaries & save outputs 
# with a tag indicating which model the estimates are from
names(m1_list) = c('complete_case', 'missing_indicator')

names_m1 = names(m1_list)

# Check summary of models
summary_m1 = lapply(setNames(names_m1, names_m1),
       function(names_index) summary(m1_list[[names_index]]))

names(summary_m1)

#2 Check optimisers:
#lapply(setNames(names_m1, names_m1), 
#       function(names_index){
  
#  aa <- allFit(m1_list[[names_index]])
#  ss <- summary(aa)
#  print(ss$msgs[!sapply(ss$msgs,is.null)])
  
#})

# Tidy results into dataframes 
# Raw model estimates
raw_m1_list <- lapply(
  setNames(names_m1, names_m1),
  function(names_index){
    
    data.frame(
      analysis_type = names_index,
      formula = formula,
      Coefficients = summary_m1[[names_index]]$coefficients[, "Estimate"],       # Log Odds
      `Standard Error` = summary_m1[[names_index]]$coefficients[, "Std. Error"],
      #`z value` = summary_m1$coefficients[, "z value"],           # Optional
      `p-value` = summary_m1[[names_index]]$coefficients[, "Pr(>|z|)"],
      date = date)
    
  })

names(raw_m1_list)
  
# Tidy estimates 
tidy_m1_list <- lapply(
  setNames(names_m1, names_m1),
  function(names_index) {
    
    tidy_m1 = broom.mixed::tidy(
      m1_list[[names_index]], conf.int=TRUE, 
      exponentiate=TRUE,
      #effects=c("fixed", "ran_pars")
      effects=c("fixed"))
    
    tidy_m1 = tidy_m1 %>%
      dplyr::mutate(
        date = date,
        across(where(is.numeric), round,4),
        analysis_type = names_index,
        formula = formula) %>%
      dplyr::relocate(analysis_type, formula) 
    
  })

names(tidy_m1_list)

#### Save outputs ----

# Save/export raw & tidy estimates into excel file & into folder with monthly date
lapply(
  setNames(names_m1, names_m1),
  function(names_index) {
    
    writexl::write_xlsx(
      raw_m1_list[[names_index]], 
      paste0(
        main_dir, sub_dir, # saves file into the Month/Year folder when the analyses were conducted
        "/raw_", names_index, "_glmer_model", # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
        file_date, ".xlsx"))
    
    writexl::write_xlsx(
      tidy_m1_list[[names_index]], 
      paste0(main_dir, sub_dir, # saves file into the Month/Year folder when the analyses were conducted
             "/tidy_", names_index, "_glmer_model", # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
             file_date, ".xlsx"))
  })

# Bind all results from analyses into one df
raw_table = do.call(bind_rows, raw_m1_list)
tidy_table = do.call(bind_rows, tidy_m1_list)

# Append latest results to existing findings on Sharepoint
# And save these results back into Sharepoint

lapply(
  c("tidy_output_list.xlsx", "raw_output_list.xlsx"),
  
  function(name_of_the_output_file){
    
    print(cat(crayon::bold(crayon::green(
      paste0('Appending latest results to findings output file: ',
             name_of_the_output_file,'\n')))))
    
    # Is there an exisiting findings output file?
    file = str_subset( # find if file exists in directory
      list.files("model_outputs/"), 
      name_of_the_output_file)
    
    print(cat(crayon::bold(crayon::green(
      paste0('Is there an existing findings output file?\n')))))
    
    if(purrr::is_empty(file)){ 
      
      print(cat(crayon::bold(crayon::red(
        paste0('There is no existing findings output file.\n')))))
      
    } else{ print(cat(crayon::bold(crayon::green(
      paste0('There is an existing findings output file.\n')))))
      
    }
    
    # Fetch the table of latest findings to append:
    # It is either 'tidy_table' or 'raw_table'
    table = paste0(
      str_remove(name_of_the_output_file, '_output_list.xlsx'), '_table')
    
    if(purrr::is_empty(file)){        
      
      print(cat(crayon::bold(crayon::green(
        paste0('The table to save is: ', table, '\n'))))) 
      
    } else{print(cat(crayon::bold(crayon::green(
      paste0('The table to append is: ', table, '\n')))))
      
    }
    
    # Fetch table
    table_to_append = get(table)
    
    # Append and/or save table
    findings_table = append_results( # bespoke function to find in the functions.R script
      output_file = file,
      table_to_append = table_to_append,
      save_to = name_of_the_output_file)
  })


##2 Imputed data: multiple imputation ----

# To check in case of singular fit: 
# https://www.biorxiv.org/content/10.1101/2021.05.03.442487v3

# Example of model fitting and pooling
#fit <- with(imputed_data_m5, lm(age ~ gender + cluster))
#pooled_results <- pool(fit)
#summary(pooled_results)

# Prep data:
# Read data
setwd(paste0(output_path, "/imputed_datasets/"))

file.info(
  list.files(
    "Norfolk_binary_single_level_m5_imputation/",
    full.names=T))[,1, drop=F]

load(paste0("Norfolk_binary_single_level_m5_imputation/",
            "Norfolk_binary_single_level_m5_imputation.Rdata"))

imputed_data_m5 = mi.res 

imputed_data_m10 = load(
  paste0("Norfolk_binary_single_level_m10_imputation/",
         "Norfolk_binary_single_level_m10_imputation.Rdata"))

imputed_data_m10 = mi.res 

rm(mi.res)

# Number of iteration used to impute datasets
iteration_number = 100

# checks 
#d5_m5 = complete(imputed_data_m5, 5)
#d5_m5_test = complete(mi.res, 5)
#d5_m5 %>% dplyr::group_by(ethnicity_agg) %>% dplyr::summarise(n())
#d5_m5_test %>% dplyr::group_by(ethnicity_agg) %>% dplyr::summarise(n())

# Prep formula 
demographics = paste('age_at_referral_cat',
                     'gender',
                     'ethnicity_agg',
                     'disabled_status',
                     'unaccompanied_asylum_seeker',
                     'number_of_previous_child_protection_plans',
                     #'referral_no_further_action',
                     sep = " + ")

re = " (1 | local_authority)"

# Cluster indicator
#splines = model_data %>% 
#  select(contains('splines')) %>%
#  colnames() 

cluster_indicator = c(
  'prop_white_british +' #,
  #str_flatten(paste0(splines, sep = ' + '))
  )

formula = paste0(
  "cla_status ~ treatment_group + wedge + ", # FE for trt + time effects
  demographics, " + ", # adjust for person level demographics
  cluster_indicator, # adjust for time-varying cluster level indicators
  re) # RE intercept 4 clusters

#### Fit models ----

# Fit model on imputed datasets with m= 5 and m=10
# Fitting models:
m2_list = lapply( # Creates a list of model objects
  c("imputed_data_m5", "imputed_data_m10"),  # models are fitted to both datasets
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

# Set standard names to keep track of which model is which
# Names will be used to provide summaries & save outputs 
# with a tag indicating which model the estimates are from
names(m2_list) = c('imputation_m5', 'imputation_m10')

names_m2 = names(m2_list)

# Pooling results 
# As per Stef Van Buurren's workflow recs:
# https://stefvanbuuren.name/fimd/workflow.html

m2_pooled_results_list <- lapply(
  setNames(names_m2, names_m2), function(names_index){
    
    mice::pool(m2_list[[names_index]]) # pool results
    
    })

names(m2_pooled_results_list)

# Get summaries from pooled results
# This retrieves raw model estimates for m=5 and m=10
m2_summary_list = lapply(
  setNames(names_m2, names_m2), function(names_index){ 
    
    summary(m2_pooled_results_list[[names_index]]) })

# Tidy outputs into dataframes 
# Raw estimate table
raw_m2_list <- lapply(
  setNames(names_m2, names_m2), function(names_index){
    
    data.frame(
      analysis_type = names_index,
      formula = formula,
      Coefficients = m2_summary_list[[names_index]]$estimate,       # Log Odds
      `Standard.Error` = m2_summary_list[[names_index]]$std.error,
      #`Statistic` = m2_summary_list[[names_index]]$statistic,           # Optional
      `df` =  m2_summary_list[[names_index]]$df,
      `p.value` = m2_summary_list[[names_index]]$p.value,
      number_of_iteration = iteration_number,
      date = date)
    
  })

# Tidy estimate table 
tidy_m2_list = lapply(
  setNames(names_m2, names_m2), function(names_index){
    
    broom.mixed::tidy(
      m2_pooled_results_list[[names_index]], conf.int=TRUE, 
      exponentiate=TRUE,
      effects="fixed")
    
    tidy_m2 = tidy_m2 %>%
      dplyr::mutate(
        across(where(is.numeric), round, 4),
        analysis_type = names_index,
        formula = formula,
        number_of_iteration = iteration_number,
        date = date) %>%
      dplyr::relocate(analysis_type, formula)
    
  })

#### Save outputs ----

##3 Imputed data: hot deck ----

# Stuff to think about ----
# To think about:

# 0 End of study period 

# 1 Correct model specification: 
# fixed/random effects 4 cluster and time 
# what participant level covariates to include
# how to model: age, 'acuity' and censoring 
# what cluster level covariates to include: 
# time-dependent, readiness, different length of wedges 
# control/treatment group balance: 
# weighted analyses so cluster 1 does not influence outcome too much? 

# 2 Robustness 
# Robust standard errors: 
# To check whether study is under powered   
# How to correct for small nb of clusters:
# x clubSandwich robust SE 
# x lmerTest Kenward-Roger correction
# Fixed vs random effects & consequences on SE (see https://www.youtube.com/watch?v=aG_zxnsVtLc)

# Issues to investigate:
# Small cluster correction - OK
# Different length for wedges - OK (weighting + time trends)
# Complexity of model; not converging; too granular; random effects very small: 
# 1 High Dimensionality 
# 2 Sparse data
# 3 Unique rows 

# Model 1: standard model 1 (time unvarying, random and fixed effects)
# Random effects for clusters, fixed effects for time; time-unvarying indicators only

# Fit model: simplest
# FE for time, RE intercepts for clusters 

# Assumptions for this model: 

# Fixed effects assumption
# 1 Common secular trend: 
# The effect of time is common across clusters and sequences 
# Modeled as categorical: not assuming any parametric shape (e.g. linear)
# 2 constant intervention effect: 
# The difference between control and treatment in outcomes is constant through time 

# RE intercept assumption:
# Random differences in outcome at baseline adjusted for

# Check the convergence information
# https://rdrr.io/cran/lme4/man/convergence.html
#model_0@optinfo$conv$opt
#isSingular(model_0)

# Standard errors:
# https://economics.mit.edu/sites/default/files/2022-09/cluster-6.pdf

# Get robust standard errors using the cluster sandwich estimator
#robust_se <- clubSandwich::coef_test(
#  model_1, vcov = "CR2", cluster = data$local_authority)

# vcov specifies the bias-reduced "CR2" variance estimator,
# which is recommended for small-sample clustered designs.

# Print the robust results (including adjusted standard errors)
#print(robust_se)
