################################################################################
#
#                Running model for final analysis (CLA) 2024 
#                               SFPC 
#                      Family Safeguarding Model 
#                       Tavistock Analysis
#                        Emily Walker 2024
#
################################################################################

setwd('C:/Users/EmilyWalker/Foundations/High-SFPC-Impact - Working folder/sfpc_familysafeguarding_cleaning')

# Read in cleaned and merged data frame for CLA outcome 

load("Output/cla_merge_dr2_dr3.RData")

# Fit model ----

# To think about:

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

# Before fitting model, all relationships (between predictors and between each predictor and the outcome)
# should be explored numerically. 

# Model 1: standard model 1 (time unvarying, random and fixed effects)
# Random effects for clusters, fixed effects for time; time-unvarying indicators only
colnames(cla_merge)

# Filter 
model_data = filter(cla_merge,
                    `primary_outcome` == 1)

# Must add to indivindual covariates: UASC and age at ref 
# Prep formula 
individual_covariates <- c('gender1',
                           'ethnicity1',
                           'disability1',
                           'previous cpp1')

individual_covariates <- paste0("`", individual_covariates, "`")
individual_covariates <- paste(individual_covariates, collapse = " + ")

### Have to aggregate ethnicity and clean gender

# Fit model: simplest
#model_data = model_data %>% filter(
#  !is.na(ethnicity_agg),
#  gender != 'Other')

model_fe = lme4::glmer(
  as.formula(
    paste0(
      "primary_outcome ~ treatment + time_period + ",
      individual_covariates,  " + (1 | la)")), 
  data = cla_merge,
  family = binomial)

summary(model_fe)