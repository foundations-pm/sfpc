#------------------------------------------------#
#
# FAMILY SAFEGUARDING: STEPPED WEDGE ANALYSES ----
#
#------------------------------------------------#

# Set up  ----

r_directory = 'C:/Users/PerrineMachuel/'

# Libraries 
{ source(paste0(r_directory, "Documents/SFPC/FS/config.R")) }

# Functions 
{ source(paste0(r_directory, "Documents/SFPC/FS/functions.R"))}

# Set-up  ----
sharepoint_path = paste0(r_directory,'Foundations/High-SFPC-Impact - Family Safeguarding')

# Data and output paths
data_path = paste0(sharepoint_path, '/Datasets/analytical_datasets/')
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

# Load data --------------------------------------------------------------------

# Main sample: children referred to CSC during trial period
setwd(data_path)

data = readRDS(file = paste0(
  data_path, "/primary_outcome_sample_analytical_dataset_final.Rds")) 

# Simulation set-up ------------------------------------------------------------

# Set seed 
# Some seed thoughts 
# https://stats.stackexchange.com/questions/80407/am-i-creating-bias-by-using-the-same-random-seed-over-and-over
set.seed(57602)

# Tidy dataset
# Columns: clusters, time period, y0 and y1 (observed) per clusters per periods

data = data %>% 
  dplyr::select(
    local_authority, wedge, cla_status, intervention_group) %>%
  dplyr::mutate(
    strata = case_when(
      local_authority %in% c('walsall', 'lancashire') ~ 'High readiness', 
      TRUE ~ 'Low readiness'),
    strata = factor(
      strata, levels = c('High readiness', 'Low readiness'))) 

# Check a few stuff
# Baseline outcome by LA 
prop_baseline = data %>% 
  dplyr::filter(wedge == "baseline") %>%
  dplyr::group_by(local_authority) %>% 
  dplyr::summarise(prop_baseline = sum(cla_status == 1) / n())

prop_control = data %>% 
  dplyr::filter(intervention_group == 0) %>%
  dplyr::group_by(local_authority) %>% 
  dplyr::summarise(prop_baseline = sum(cla_status == 1) / n())

# Sample size
sample_size = data %>% 
  dplyr::group_by(local_authority) %>% 
  dplyr::summarise(n())

sample_size_by_wedge = data %>% 
  dplyr::group_by(local_authority, wedge) %>% 
  dplyr::summarise(n())

### Random sequence assignment -----
# Random assignment of treatment and control groups between LAs:

# The assingment of treatment/control condition is at the LA level
# The assignment is to a sequence of binary events within wedges of the trial:
# Baseline = 0, wedge n (= 1:4) = {0,1}

# Permutations possible are limited due to the stratified randomisation
# For Walsall and Lanc ('high readiness LAs'), these can be:
# 0,1,1,1,1,1 (switching to intervention in wedge 1)
# 0,0,1,1,1,1 (switching to intervention in wedge 2)
# The only permutation possible is between wedge 1 and wedge 2 
# and there's only 2 different randomisation within this stratum
# Same logic for Telford, Wandsworth and Swindon ('low readiness LAs'):
# 0,0,0,1,1,1 (switching to intervention in wedge 3)
# 0,0,0,0,1,1 (switching to intervention in wedge 4)
# 0,0,0,0,0,1 (switching to intervention in wedge 5)

# There are a defined number of children recruited within LAs per trial wedges
# Will randomise LAs to a sequence 
# And assign treatment groups to individual participants based on that sequence

clusters = levels(data$local_authority)

sequence_randomiser = function(clusters){
  
  sequence_1 = list(
    c(0,1,1,1,1,1),
    c(0,0,1,1,1,1))
  
  sequence_2 = list(
    c(0,0,0,1,1,1),
    c(0,0,0,0,1,1),
    c(0,0,0,0,0,1))
  
  stratum_1_randomised = rbinom(1, 1, 0.5) + 1 
  stratum_2_randomised = rbinom(1, 2, 0.5) + 1
  stratum_3_randomised = rbinom(1, 1, 0.5) + 1
  # if stratum_randomised == 0, then assign first vector in the sequence (0 + 1 = 1); 
  # if stratum_randomised == 1, then assign second vector in the sequence (1 + 1 = 2)
  # if stratum_randomised == 2, then assign second vector in the sequence (2 + 1 = 3)
  
  randomised_sequence_vector = setNames(
    
    # Note: first vector in sequence = sequence_1[1] = 0,1,1,1,1; 
    # Second vector in sequence = sequence_1[2] = sequence_1[-1] = 0,0,1,1,1
    # Same with sequence_2: sequence_2[1] = 0,0,0,1,1; 
    # sequence_2[-1] = sequence_2[2] = 0,0,0,0,1
    c(
      sequence_1[stratum_1_randomised],
      sequence_1[-stratum_1_randomised], # inverse of what Rochdale was randomly assigned 
      sequence_2[stratum_2_randomised], 
      sequence_2[-stratum_2_randomised][stratum_3_randomised], # first or second inverse
      sequence_2[-stratum_2_randomised][-stratum_3_randomised] # first or second inverse
      
      ), 
    
    clusters)
  
}

# Test
randomised_sequence = sequence_randomiser(clusters)
rm(randomised_sequence)

### Simulated dataset -----

# Realised outcomes:
# Simulated outcome, based on clusters' baseline outcome probabilities
# i.e., proportion of children becoming looked after within 18 months at baseline
# This means we simulate outcomes where the Null hypothesis is true: 
# The proportion of children going into 18 within months over the trial period
# will not be different from baseline proportions 

simulate_data = function(
    cluster_vector,
    period_vector,
    probability_vector,
    sample_size_list){
  
  # Assign a random sequence to clusters 
  # Stratified by readiness levels:
  # Rochdale/Warrington randomised together, Norfolk/Redcar randomised together
  randomised_sequence = sequence_randomiser(cluster_vector)
  
  # Empty dataframe
  sim_data = data.frame()
  
  # Fill-in sim_data with simulated outcomes
  # Based on binomial distribution with p = baseline probability for each clusters
  for (cluster in cluster_vector) {
    
    #print(paste0('Cluster: ', cluster))
    
    # Baseline probability of becoming looked after for this cluster
    prob <- probability_vector[cluster]
    #print(paste0('Baseline probability for ', cluster, ' : ', prob))
    
    n_period = 0
    
    for (period in period_vector) {
      
      n_period = n_period + 1
      
      #print(paste0('Period: ', period))
      #print(paste0('Period number: ', n_period))
      
      # Sample size for this cluster during this period 
      n <- sample_size_list[[cluster]][[period]][['sample_size']]
      #print(paste0('Number of children recruite in ',
      #             cluster, ' in ', period, ' : ', n, ' children'))
      
      # Treatment status for this cluster, during this period 
      treatment <- randomised_sequence[[cluster]][n_period]
      #print(paste0('Treatment status for ', cluster,
      #             ' in ', period, ' : ', treatment))
      
      sim_cluster_period <- data.frame(
        local_authority = cluster,
        wedge = period,
        treatment_group = treatment,
        cla_status = rbinom(n, size = 1, prob = prob))
      
      sim_data <- rbind(sim_data, sim_cluster_period)
      
    }
  }
  
  return(sim_data)
  
}

### Tidy  -----
# Tidy-up simulated findings:
# Retrieve ATE & CIs into a df 
summarise_glmm_model = function(model_fit, formula){
  
  tidy_sim_model = broom.mixed::tidy(
    model_fit, conf.int=TRUE, 
    exponentiate=TRUE,
    effects=c("fixed"))
  
  tidy_sim_model = tidy_sim_model %>%
    dplyr::mutate(formula = formula) %>%
    dplyr::rename('odds.ratio' = 'estimate') %>%
    dplyr::select(
      formula, term, odds.ratio, conf.low, conf.high, p.value) 
}

summarise_sandwich_robust_glm_model = function(
    model_table, ci_table, formula){
  
  tidy_model <- ci_table %>%
    dplyr::left_join(
      model_table, 
      by = c('Coef' = 'Coef',
             'beta' = 'beta',
             'SE' = 'SE',
             'df' = 'df_t')) %>%
    dplyr::mutate(
      formula = formula,
      odds.ratio = exp(beta),
      conf.low = exp(CI_L),
      conf.high = exp(CI_U)) %>%
    dplyr::rename(
      'term' = 'Coef',
      'statistic' = 'tstat',
      'p.value' = 'p_t',
      'std.error' = 'SE') %>%
    dplyr::select(
      formula, term, odds.ratio, conf.low, conf.high,
      std.error, statistic, p.value, df)
  
}

### Simulation pipeline -----
# Write the pipeline to iterate over 

# Simulation parameters 

wedge_sizes <- data %>% # individuals per cluster per wedge
  dplyr::group_by(local_authority, wedge) %>% 
  dplyr::summarise(sample_size = n()) 

clusters <- levels(wedge_sizes$local_authority)

periods <- unique(wedge_sizes$wedge) 

wedge_sizes <- lapply(
  split(wedge_sizes, wedge_sizes$local_authority, drop = TRUE),
  function(x) { split(x, x[['wedge']], drop = TRUE) })

probabilities <- c( # Baseline probabilities of becoming LAC within 18months
  'walsall' = 0.0613, 'lancashire' = 0.0635,
  'telford' = 0.0833, 'wandsworth' = 0.0321,
  'swindon' = 0.0345) 

formula_glmer = 'cla_status ~ intervention_group + wedge + (1|local_authority)'
formula_glm = 'cla_status ~ intervention_group + wedge + local_authority'

swcrt_simulation_pipeline = function(cluster_vector,
                                     period_vector,
                                     probability_vector,
                                     sample_size_list,
                                     method = 'glmer',
                                     formula){
  
  print('Simulate data')
  
  sim_data = simulate_data(
    cluster_vector = cluster_vector,
    period_vector = period_vector,
    probability_vector = probability_vector,
    sample_size_list = sample_size_list)
  
  
  if(method == 'glmer'){
    
    print('Fitting a GLMER model from lme4 package')
    
    sim_model = lme4::glmer(
      as.formula(formula), 
      data = sim_data,
      family = binomial)
    
    tidy_model = summarise_glmm_model(sim_model, formula)
    
  } 
  
  else {
    
    print('Fitting a GLM model from stats package')
    
    sim_model = stats::glm(
      as.formula(formula), 
      data = sim_data,
      family =  binomial(link = "logit"))
    
    if(method == 'CR3'){
      
      print('Estimating cluster-robust SE using the CR3 method')
      
      # Cluster-robust covariance matrix
      cr3_vcov <- clubSandwich::vcovCR(
        sim_model, 
        cluster = sim_data[['local_authority']], 
        type = 'CR3')
      
      # Get t-stat, p.value and df
      cr3_model_stats <- clubSandwich::coef_test(
        sim_model, 
        vcov = cr3_vcov, 
        test = "naive-t") 
      
      # Calculate confidence intervals
      cr3_ci <- clubSandwich::conf_int(
        sim_model, 
        vcov = cr3_vcov, 
        test = "naive-t", 
        level = 0.95)
      
      tidy_model = summarise_sandwich_robust_glm_model(
        model_table = cr3_model_stats, 
        ci_table = cr3_ci,
        formula = formula)
      
    }
    
    if(method == 'CR2'){
      
      print('Estimating cluster-robust SE using the CR2 method')
      
      # Cluster-robust covariance matrix
      cr2_vcov <- clubSandwich::vcovCR(
        sim_model, 
        cluster = sim_data[['local_authority']], 
        type = 'CR2')
      
      # Get t-stat, p.value and df
      cr2_model_stats <- clubSandwich::coef_test(
        sim_model, 
        vcov = cr2_vcov, 
        test = "Satterthwaite") 
      
      # Calculate confidence intervals
      cr2_ci <- clubSandwich::conf_int(
        sim_model, 
        vcov = cr2_vcov, 
        test = "Satterthwaite", 
        level = 0.95)
      
      tidy_model = summarise_sandwich_robust_glm_model(
        model_table = cr2_model_stats, 
        ci_table = cr2_ci,
        formula = formula)
      
    }
    
  }
  
  tidy_model = tidy_model %>%
    dplyr::mutate(date = date)
  
  return(tidy_model)
  
}

## Retrieve results ----

get_results <- function(simulation_results,
                        simulation_type,
                        n_replication,
                        formula,
                        date){
  
  purrr::map_dfr(unique(simulation_results[[1]]), 
                 function(covariate){
                   
                   simulation_results %>%
                     dplyr::filter(term == covariate) %>%
                     dplyr::summarise(
                       `Proportion of p-value < 0.05` = sum(p.value < 0.05)/n()) %>%
                     dplyr::mutate(simulation_type = simulation_type,
                                   term = covariate,
                                   n_replication = n_replication,
                                   formula = formula,
                                   date = date) %>%
                     dplyr::relocate(`Proportion of p-value < 0.05`, .after = term)
                   
                 }) 
  
} 

# Test pipeline: 
#test = swcrt_simulation_pipeline(
#  cluster_vector = clusters,
#  period_vector = periods,
#  probability_vector = probabilities,
#  sample_size_list = wedge_sizes,
#  family = 'glm',
#  formula = formula_glm)

# Run simulation ----

## GLMM ----

# Set directory
setwd(paste0(output_path, '/simulation'))

### Run simulation ----

# Running simulation by batches - running 3 sessions at a time
# Splitting the batch ranges in 3
# Running each batch a 100 times (33*3*100 = 10,000 reps)
batch_range <- 1:33
n_replication = 100

tic()

for (batch in batch_range) {
  output_file <- file.path("GLMM/", paste0("results_batch_", batch, ".xlsx"))
  
  tic()
  
  if (file.exists(output_file)) {
    cat("Batch", batch, "already exists. Skipping.\n")
    next
  }
  
  sim_results_glmm <- replicate(n_replication, 
                                swcrt_simulation_pipeline(
                                  cluster_vector = clusters,
                                  period_vector = periods,
                                  probability_vector = probabilities,
                                  sample_size_list = wedge_sizes,
                                  method = 'glmer',
                                  formula = formula_glmer), 
                                simplify = FALSE) %>% 
    purrr::list_rbind()
  
  writexl::write_xlsx(sim_results_glmm, output_file)
  cat("Saved batch", batch, "\n")
  
  toc()
  
}

toc()

### Check results ----
glmm_results <- list.files(
  path = paste0(output_path, 'simulation/GLMM'),
  pattern = "*.xlsx",
  full.names = TRUE)

df <- lapply(glmm_results, read_xlsx) %>% 
  bind_rows()

overall_results <- df %>%
  dplyr::filter(term == 'treatment_group') %>%
  dplyr::summarise(
    `Proportion of p-value < 0.05 with GLMM` = sum(p.value < 0.05)/n()) 

#overall_results <- sim_results_glmm %>%
#  dplyr::summarise(
#    `Proportion of p-value < 0.05 with GLMM` = sum(p.value < 0.05)/n()) 

#print(overall_results)

#results_glmm = get_results(
#  sim_results_glmm,
#  simulation_type = simulation_type,
#  n_replication = n_replication,
#  formula = formula_glmer,
#  date = date)

# Save outputs 
# setwd(output_path) 

# Save simulation results 
#writexl::write_xlsx(
#  sim_results_glmm,
#  paste0("simulation_results_glmm", file_date, ".xlsx"))

# Save summary results
# output_file = str_subset( # find if file exists in directory
#  list.files(), 
#  'simulation_results.xlsx')

#append_results(
#  output_file = output_file,
#  table_1_to_append = results_glmm,
#  save_to = 'simulation_results.xlsx')
