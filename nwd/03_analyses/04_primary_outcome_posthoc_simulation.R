# Primary outcome analysis for No Wrong Doors RCT DR1 ----

# Set up  ----

#Paths
user_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(user_directory,'Foundations/High-SFPC-Impact - ')

# Data and output paths

# where the primary outcome dataset is
data_path = paste0(sharepoint_path, 'QA/outputs/') 

# where to save final output list
output_path = paste0(sharepoint_path, 'QA/outputs/model_outputs/primary_analyses/')

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


# Load data --------------------------------------------------------------------

data <- readRDS(file = paste0(
  data_path, 'primary_analysis_analytical_dataset_V2.Rds'))

# Simulation set-up ------------------------------------------------------------

# Set seed 
# Some seed thoughts 
# https://stats.stackexchange.com/questions/80407/am-i-creating-bias-by-using-the-same-random-seed-over-and-over
set.seed(57602)

# Tidy dataset
# Columns: clusters, time period, y0 and y1 (observed) per clusters per periods
clusters <- c('rochdale', 'warrington', 
              'norfolk', 'redcar') 

data = data %>% 
  dplyr::select(local_authority, readiness, wedge,
                cla_status, treatment_group) %>%
  dplyr::mutate(local_authority = factor(
    local_authority, levels = clusters)) %>%
  dplyr::rename(strata = readiness)

# Check a few stuff
# Baseline outcome by LA 
prop_baseline = data %>% 
  dplyr::filter(wedge == "baseline") %>%
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
# For Rochdale and Warrington ('high readiness LAs'), these can be:
# 0,1,1,1,1 (switching to intervention in wedge 1)
# 0,0,1,1,1 (switching to intervention in wedge 2)
# The only permutation possible is between wedge 1 and wedge 2 
# and there's only 2 different randomisation within this stratum
# Same logic for Norfolk and Redcar ('low readiness LAs'):
# 0,0,0,1,1 (switching to intervention in wedge 3)
# 0,0,0,0,1 (switching to intervention in wedge 4)

# There are a defined number of children recruited within LAs per trial wedges
# Will randomise LAs to a sequence 
# And assign treatment groups to individual participants based on that sequence

sequence_randomiser = function(clusters){
  
  sequence_1 = list(
    c(0,1,1,1,1),
    c(0,0,1,1,1))
  
  sequence_2 = list(
    c(0,0,0,1,1),
    c(0,0,0,0,1))
  
  stratum_1_randomised = rbinom(1, 1, 0.5) + 1 
  stratum_2_randomised = rbinom(1, 1, 0.5) + 1
  # if stratum_randomised == 0, then assign first vector in the sequence (0 + 1 = 1); 
  # if stratum_randomised == 1, then assign second vector in the sequence (1 + 1 = 2)
  
  randomised_sequence_vector = setNames(
    
    # Note: first vector in sequence = sequence_1[1] = 0,1,1,1,1; 
    # Second vector in sequence = sequence_1[2] = sequence_1[-1] = 0,0,1,1,1
    # Same with sequence_2: sequence_2[1] = 0,0,0,1,1; 
    # sequence_2[-1] = sequence_2[2] = 0,0,0,0,1
    c(
      sequence_1[stratum_1_randomised],
      sequence_1[-stratum_1_randomised], # inverse of what Rochdale was randomly assigned 
      sequence_2[stratum_2_randomised], 
      sequence_2[-stratum_2_randomised]),
    
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
summarise_glmm_model = function(model_fit){
  
  tidy_sim_model = broom.mixed::tidy(
    model_fit, conf.int=TRUE, 
    exponentiate=TRUE,
    effects=c("fixed"))
  
  tidy_sim_model = tidy_sim_model %>%
    dplyr::rename('odds.ratio' = 'estimate') %>%
    dplyr::select(term, odds.ratio, conf.low, conf.high, p.value) 
}

summarise_sandwich_robust_glm_model = function(
    model_table, ci_table){
  
  tidy_model <- ci_table %>%
    dplyr::left_join(
      model_table, 
      by = c('Coef' = 'Coef',
             'beta' = 'beta',
             'SE' = 'SE',
             'df' = 'df_t')) %>%
    dplyr::mutate(
      odds.ratio = exp(beta),
      conf.low = exp(CI_L),
      conf.high = exp(CI_U)) %>%
    dplyr::rename(
      'term' = 'Coef',
      'statistic' = 'tstat',
      'p.value' = 'p_t',
      'std.error' = 'SE') %>%
    dplyr::select(term, odds.ratio, conf.low, conf.high,
                  std.error, statistic, p.value, df)
  
}

### Simulation pipeline -----
# Write the pipeline to iterate over 

# Simulation parameters 

clusters <- levels(data$local_authority)

periods <- unique(data[['wedge']]) 

wedge_sizes <- data %>% # individuals per cluster per wedge
  dplyr::group_by(local_authority, wedge) %>% 
  dplyr::summarise(sample_size = n()) 

wedge_sizes <- lapply(
  split(wedge_sizes, wedge_sizes$local_authority, drop = TRUE),
  function(x) { split(x, x[['wedge']], drop = TRUE) })

probabilities <- c( # Baseline probabilities of becoming LAC within 18months
  'rochdale' = 0.0624, 'warrington' = 0.0584,
  'norfolk' = 0.0607, 'redcar' = 0.0386) 

formula_glmer = 'cla_status ~ treatment_group + wedge + (1|local_authority)'
formula_glm = 'cla_status ~ treatment_group + wedge + local_authority'

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
    
    tidy_model = summarise_glmm_model(sim_model)
    
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
        ci_table = cr3_ci)
      
    }
    
    if(family == 'CR2'){
      
      # Cluster-robust covariance matrix
      cr2_vcov <- clubSandwich::vcovCR(
        sim_model, 
        cluster = sim_data[['local_authority']], 
        type = 'CR2')
      
      # Get t-stat, p.value and df
      cr2_model_stats <- clubSandwich::coef_test(
        sim_model, 
        vcov = cr2_vcov, 
        test = "naive-t") 
      
      # Calculate confidence intervals
      cr2_ci <- clubSandwich::conf_int(
        sim_model, 
        vcov = cr2_vcov, 
        test = "naive-t", 
        level = 0.95)
      
      tidy_model = summarise_sandwich_robust_glm_model(
        model_table = cr2_model_stats, 
        ci_table = cr2_ci)
      
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

# Simulation with GLMER
# Simulating the ATE with 10,000 iterations
n_replication = 2
simulation_type = 'GLMM'

tic()

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

toc()

# Get results 
overvall_results <- sim_results_glmm %>%
  dplyr::summarise(
    `Proportion of p-value < 0.05 with GLMM` = sum(p.value < 0.05)/n()) 

print(overvall_results)

results_glmm = get_results(
  sim_results_glmm,
  simulation_type = simulation_type,
  n_replication = n_replication,
  formula = formula_glmer,
  date = date)

# Save outputs 
setwd(output_path) 

# Save simulation results 
writexl::write_xlsx(
  sim_results_glmm,
  paste0("simulation_results_glmm", file_date, ".xlsx"))

# Save summary results
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'simulation_results.xlsx')

append_results(
  output_file = output_file,
  table_1_to_append = results_glmm,
  save_to = 'simulation_results.xlsx')

## CR3 method ----

# Simulation with GLM
# Simulating the ATE with 10,000 iterations
n_replication = 2
simulation_type = 'CR3'

tic()

sim_results_cr3 <- replicate(n_replication, 
                         swcrt_simulation_pipeline(
                           cluster_vector = clusters,
                           period_vector = periods,
                           probability_vector = probabilities,
                           sample_size_list = wedge_sizes,
                           method = 'CR3',
                           formula = formula_glm), 
                         simplify = FALSE) %>% 
  purrr::list_rbind()

toc()

# Get results 
overall_results <- sim_results_cr3 %>%
  dplyr::summarise(
    `Proportion of p-value < 0.05 with CR3` = sum(p.value < 0.05)/n()) 

print(overall_results)

results_cr3 = get_results(
  sim_results_cr3,
  simulation_type = simulation_type,
  n_replication = n_replication,
  formula = formula_glm,
  date = date)

# Save outputs 
setwd(output_path) 

# Save simulation results 
writexl::write_xlsx(
  sim_results_cr3,
  paste0("simulation_results_cr3", file_date, ".xlsx"))

# Save summary results
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'simulation_results.xlsx')

append_results(
  output_file = output_file,
  table_1_to_append = results_cr3,
  save_to = 'simulation_results.xlsx')

## CR2 Method ----

# Simulation with GLM
# Simulating the ATE with 10,000 iterations
n_replication = 2
simulation_type = 'CR2'

tic()

sim_results_cr2 <- replicate(n_replication, 
                         swcrt_simulation_pipeline(
                           cluster_vector = clusters,
                           period_vector = periods,
                           probability_vector = probabilities,
                           sample_size_list = wedge_sizes,
                           method = 'CR2',
                           formula = formula_glm), 
                         simplify = FALSE) %>% 
  purrr::list_rbind()

toc()

# Get results 
overall_results <- sim_results_cr2 %>%
  dplyr::summarise(
    `Proportion of p-value < 0.05 with CR2` = sum(p.value < 0.05)/n()) 

print(overall_results)

results_cr2 = get_results(
  sim_results_cr2,
  simulation_type = simulation_type,
  n_replication = n_replication,
  formula = formula_glm,
  date = date)

# Save outputs 
setwd(output_path) 

# Save simulation results 
writexl::write_xlsx(
  sim_results_cr2,
  paste0("simulation_results_cr2", file_date, ".xlsx"))

# Save summary results
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'simulation_results.xlsx')

append_results(
  output_file = output_file,
  table_1_to_append = results_cr2,
  save_to = 'simulation_results.xlsx')

----------------------------------------------------------------------
----------------------------------------------------------------------

# Test & learn from Andi's work -----------------------------------------------------------------

## Sharp variance bounds ----
devtools::install_github("JonasMikhaeil/SharpVarianceBounds")
library(sharpvar)
library(tictoc)

set.seed(42)
theN <- 100

# Simulated datasets
sim_dat <- tibble(latent_magic = 50 + rnorm(theN, 0, sqrt(.6)),
                  y0 = round(latent_magic + rnorm(theN, 0, sqrt(.4)), 1),
                  y1 = round(latent_magic + 0.2 + rnorm(theN, 0, sqrt(.4)), 1),
                  x  = latent_magic + rnorm(theN, mean = 0, sd = 1.5),
                  TE = y1 - y0) |>
  select(-latent_magic)

# Correlation between potential outcomes across Y0 and Y1 conditions 
# For each individuals
sim_dat |>
  select(-TE) |>
  cor() |>
  round(2)

trueSATE <- mean(sim_dat$TE)
trueSATE

# Random assignment of control and treatment group 
# Simulating an efficacy trial 
completely_randomiser <- function(dat,
                                  treat_var_name = "treat",
                                  prop_treat = 0.5) {
  treat_n   <- ceiling(prop_treat * nrow(dat))
  control_n <- nrow(dat) - treat_n
  zero_ones <- c(rep(1, treat_n), rep(0, control_n))
  dat |>
    dplyr::mutate(!!treat_var_name := sample(zero_ones))
}

# Generate realised potential
realise_potential <- function(dat, y_name = y, y0, y1, treat) {
  dat |>
    dplyr::mutate(!!y_name := ifelse(treat == 1, y1, y0))
}

# Clean up / summarise findings for the classic 
# and sharp bounds SE
summarise_mod <- function(mod, dat, treat_var = "treat") {
  mod |>
    tidy() |>
    dplyr::filter(term == treat_var) |>
    dplyr::mutate(
      se_sharp = sharpvar(resid(mod), dat$treat, upper = TRUE) |> sqrt(),
      df = df.residual(mod)
    ) |>
    dplyr::rename(se_classic = std.error) |>
    dplyr::select(-c(statistic, p.value)) |>
    pivot_longer(
      cols = starts_with("se_"),
      names_prefix = "se_",
      names_to = "type",
      values_to = "se"
    ) |>
    dplyr::mutate(ci_lower = estimate + qt(.05/2, df) * se,
           ci_upper = estimate + qt(1 - (.05/2), df) * se)
}

# Simulation pipeline

one_sim <- function(popn_dat) {
  one_trial <- popn_dat |>
    completely_randomiser() |>
    realise_potential("y", y0, y1, treat)
  
  the_mod <- lm(y ~ treat + x, data = one_trial)
  
  summarise_mod(the_mod, one_trial)
}


# Simulating the ATE with 10,000 iterations
tic()
res <- replicate(10000, one_sim(sim_dat), simplify = FALSE) |> list_rbind()
toc()

# Coverage of true ATE within 95%CI
tallied_res <- res |>
  mutate(truth = trueSATE,
         covered = between(truth, ci_lower, ci_upper)) |>
  group_by(type) |>
  summarise(`proportion 95% CI coverage` = mean(covered))

tallied_res

## ATE simulation for an RCT ----
set.seed(3137942)
theN <- 500

sim_dat <- tibble(latent_magic = 50 + rnorm(theN, 0, sqrt(.6)),
                  y0 = round(latent_magic + rnorm(theN, 0, sqrt(.4)), 1),
                  y1 = round(latent_magic + 0.2 + rnorm(theN, 0, sqrt(.4)), 1),
                  ITS = y1 - y0) |>
  select(-latent_magic)

trueATE = mean(sim_dat$ITS)

# Estimate ATE once 
rct_dat <- sim_dat |>
  dplyr::mutate(
    treat = rbinom(n(), 1, .5),
    y = treat * y1 + (1 - treat) * y0,
    y1 = ifelse(treat == 1, y1, NA),
    y0 = ifelse(treat == 0, y0, NA)
  ) |>
  dplyr::select(-ITS)

rct_dat

lm_mod <- stats::lm(y ~ treat, data = rct_dat)

cbind(ATE = coef(lm_mod), confint(lm_mod))["treat",]

# Estimate ATE: simulation
sim_reses <- data.frame()

for (i in 1:2000) {
  rct_dat <- sim_dat |>
    dplyr::mutate(treat = rbinom(n(), 1, .5),
                  y = treat * y1 + (1 - treat) * y0)
  
  lm_mod <- lm(y ~ treat, data = rct_dat)
  this_res <- cbind(ATE = coef(lm_mod), confint(lm_mod))["treat", ]
  
  sim_reses <- dplyr::bind_rows(sim_reses, this_res)
}

# Compare TRUE ATE to mean of simulated ATES
sim_reses  |>
  mutate(across(everything(), \(x) round(x, 2)))

c(
  `mean of ATE estimates` = mean(sim_reses$ATE),
  `true ATE` = trueATE,
  `true ATE - mean of estimates` = trueATE - mean(sim_reses$ATE) 
) |> round(4)

# Variation in the estimated ATEs
sim_reses |>
  ggplot(aes(x = ATE)) +
  geom_histogram(bins = 40) +
  xlab("estimated ATE")

# 95% CI coverage: 
sim_reses <- sim_reses |>
  mutate(trueATE_in_interval = trueATE >= `2.5 %` &
           trueATE <= `97.5 %`)

cover <- mean(sim_reses$trueATE_in_interval)
cover
