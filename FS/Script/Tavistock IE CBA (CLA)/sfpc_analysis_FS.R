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

library(mice)
library(reshape2)
library(RColorBrewer)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(readxl)
library(tibble)
library(data.table)
library(arsenal)
library(visdat)
library(VIM)
library(cobalt)
library(broom)


# Create function for propplot ----
# This function uses the following packages:
# - mice
# - reshape2
# - RColorBrewer
# - ggplot2
propplot <- function(
    x, formula, facet = "wrap",
    label_size = 10, show_prop = FALSE, prop_size = 2, ...) {
  library(ggplot2)
  
  cd <- data.frame(mice::complete(x, "long", include = TRUE))
  cd$.imp <- factor(cd$.imp)
  
  r <- as.data.frame(is.na(x$data))
  
  impcat <- x$meth != "" & sapply(x$data, is.factor)
  vnames <- names(impcat)[impcat]
  
  if (missing(formula)) {
    formula <- as.formula(paste(paste(vnames, collapse = "+",
                                      sep = ""), "~1", sep = ""))
  }
  
  tmsx <- terms(formula[-3], data = x$data)
  xnames <- attr(tmsx, "term.labels")
  xnames <- xnames[xnames %in% vnames]
  
  if (paste(formula[3]) != "1") {
    wvars <- gsub("[[:space:]]*\\|[[:print:]]*", "", paste(formula)[3])
    wvars <- all.vars(as.formula(paste("~", wvars)))
    wvars <- attr(terms(as.formula(paste("~", wvars))), "term.labels")
    if (grepl("\\|", formula[3])) {
      svars <- gsub("[[:print:]]*\\|[[:space:]]*", "", paste(formula)[3])
      svars <- all.vars(as.formula(paste("~", svars)))
    } else {
      svars <- ".imp"
    }
  } else {
    wvars <- NULL
    svars <- ".imp"
  }
  
  for (i in seq_along(xnames)) {
    xvar <- xnames[i]
    select <- cd$.imp != 0 & !r[, xvar]
    cd[select, xvar] <- NA
  }
  
  
  for (i in which(!wvars %in% names(cd))) {
    cd[, wvars[i]] <- with(cd, eval(parse(text = wvars[i])))
  }
  
  meltDF <- reshape2::melt(cd[, c(wvars, svars, xnames)], id.vars = c(wvars, svars))
  meltDF <- meltDF[!is.na(meltDF$value), ]
  
  
  wvars <- if (!is.null(wvars)) paste0("`", wvars, "`")
  
  a <- plyr::ddply(meltDF, c(wvars, svars, "variable", "value"), plyr::summarize,
                   count = length(value))
  b <- plyr::ddply(meltDF, c(wvars, svars, "variable"), plyr::summarize,
                   tot = length(value))
  mdf <- merge(a,b)
  mdf$prop <- mdf$count / mdf$tot
  
  plotDF <- merge(unique(meltDF), mdf)
  plotDF$value <- factor(plotDF$value,
                         levels = unique(unlist(lapply(x$data[, xnames], levels))),
                         ordered = T)
  
  p <- ggplot(plotDF, aes(x = value, fill = get(svars), y = prop)) +
    geom_bar(position = "dodge", stat = "identity") +
    theme(
      axis.text.x = element_text(angle = 10, hjust = 1, size = label_size),
      legend.position = "bottom", ...) +
    ylab("proportion") +
    scale_fill_manual(name = "",
                      values = c("red",
                                 colorRampPalette(
                                   RColorBrewer::brewer.pal(9, "Blues"))(x$m + 3)[1:x$m + 3])) +
    guides(fill = guide_legend(nrow = 1)) +
    ggtitle('Ethnic proportions: observed versus imputed data')
  
  if(isTRUE(show_prop)){
    
    p <- p + geom_text(
      aes(label = round(prop, 2)), 
      position = position_dodge(width = 0.9),   # Align text with bars
      vjust = -0.5,
      size = prop_size)
    
  }
  
  if (facet == "wrap")
    if (length(xnames) > 1) {
      return(p + facet_wrap(c("variable", wvars), scales = "free"))
    } else {
      if (is.null(wvars)) {
        return(p)
      } else {
        return(p + facet_wrap(wvars, scales = "free"))
      }
    }
  
  if (facet == "grid")
    if (!is.null(wvars)) {
      return(p + facet_grid(paste(paste(wvars, collapse = "+"), "~ variable"),
                            scales = "free"))
    }
  
  
}

# Read in cleaned and merged data frame for CLA outcome ----

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

cla_merge <- cla_merge %>%
  rename(
    child_la_id = `child la id`,
    prev_cpp = `previous cpp1`
  )


# Factor 
cla_merge <- cla_merge %>%
  mutate(
    
    gender1 = relevel(
      factor(gender1), ref = 'Male'),
    
    ethnicity1 = relevel(
      factor(ethnicity1), ref = 'White British or Irish'),
    
    disability1 = relevel(
      factor(disability1), ref = '0'),
    
    uasc1 = relevel(
      factor(uasc1), ref = '0'),
    
    `prev_cpp` = relevel(
      factor(`prev_cpp`), ref = '0'),
    
    age_group = relevel(
      factor(age_group), ref = 'unborn'),
    
    la = relevel(
      factor(la), ref = 'Lancashire'
    )
  )
  

# Multiple Imputation: 

# Must add to individual covariates: UASC and age at ref 

# Creating an example dataframe with only the variables of interest. 
model_data <- cla_merge %>% select(la, `child_la_id`, `prev_cpp`, `gender1`, 
                                   ethnicity1, disability1, uasc1, age_group, 
                                   primary_outcome, treatment, time_period)


# Prep formula 
individual_covariates <- c('gender1',
                           'ethnicity1',
                           'disability1',
                           'prev_cpp',
                           'uasc1',
                           'age_group'
)

individual_covariates <- paste0("`", individual_covariates, "`")
individual_covariates <- paste(individual_covariates, collapse = " + ")


# View missing data pattern 
mice::md.pattern(model_data)

bin_predm <- make.predictorMatrix(model_data)

bin_predm["ethnicity1", ] <- c(
  1, 0, 1, 1, #la: in, child la id: out, previous cpp1: in, gender1: in
  0,1,1,1,    #ethnicity1: out, disability1, uasc1, age_group
  1,1,1)    # primary_outcome, treatment, time_period
 

imputed_data <- mice::mice(
  model_data, # model data only includes what you need in your analysis, no other vars
  m = 5, 
  method = 'polyreg',
  seed = 123, 
  predictorMatrix = bin_predm,
  maxit = 100) # 1,000 iterations will take a while, suggest you start with 10 just to see if it works


# Check logged events 
imputed_data$loggedEvents

# Check the imputed values for 'ethnicity'
imputed_data$imp$ethnicity1

## Imputation checks ----

# Check convergence 
plot(imputed_data)

# Visualize observed vs imputed values for ethnicity
imp_plot = propplot(imputed_data, 
                    label_size = 10,
                    show_prop = TRUE,
                    prop_size = 2)

imp_plot_trt = propplot(
  imputed_data,
  ethnicity1 ~ treatment,
  label_size = 7) 

imp_plot_la = propplot(
  imputed_data, 
  ethnicity1 ~ la,
  label_size = 5) 

# Extract completed imputed datasets and check consistency
#complete_data_1 <- complete(imputation_model, 1)  # First imputed dataset
#complete_data_2 <- complete(imputation_model, 2)  # Second imputed dataset

# Check summaries
#summary(complete_data_1$ethnicity_agg)
#summary(complete_data_2$ethnicity_agg)



# Model 1: standard model 1 (time unvarying, random and fixed effects)
# Random effects for clusters, fixed effects for time; time-unvarying indicators only


# Fit model: simplest
#model_data = model_data %>% filter(
#  !is.na(ethnicity_agg),
#  gender != 'Other')

formula = paste0(
  "primary_outcome ~ treatment + time_period + ",
  individual_covariates,  " + (1 | la)")

model_complete_case = lme4::glmer(
  as.formula(formula), 
  data = model_data,
  family = binomial)

#Exporting the log odds/coefficients 
model_cc <- summary(model_complete_case)

# Create a data frame with coefficients, standard errors, and p-values
coefficients_df <- data.frame(
  Coefficients = model_cc$coefficients[, "Estimate"],       # Log Odds
  `Standard Error` = model_cc$coefficients[, "Std. Error"],
  `z value` = model_cc$coefficients[, "z value"],           # Optional
  `p-value` = model_cc$coefficients[, "Pr(>|z|)"]
)

# Export the data frame to a CSV file
write.csv(coefficients_df, file = "Output/coefficients_complete_case.csv", row.names = TRUE)

# Exponentiating odds rations 
tidy_complete_case = broom.mixed::tidy(
  model_complete_case, conf.int=TRUE, # use pooled results if using imputed data
  exponentiate=TRUE,
  effects="fixed") # keep just fixed effects terms

tidy_complete_case = tidy_complete_case %>%
  dplyr::mutate(
    across(where(is.numeric), round,4),
    model = 'complete case analysis', # just indicating which data is used
    formula = formula) %>% # making sure you keep a trace of the formula / model spec that was used to get estimates
  dplyr::relocate(model, formula)

View(tidy_complete_case)

write.csv(tidy_complete_case, "Output/logit_complete_case.csv", row.names = FALSE)

# Imputed model ----

model_imputed = with(
  imputed_data,  # df is a list with imputed data, the output of the mice function above
  lme4::glmer(
    as.formula(formula), 
    family = binomial))


# Check summary 
pooled_results <- mice::pool(model_imputed) # pooling all the estimates across imputed datasets

#Exporting the log odds/coefficients 
summary_imp <- summary(pooled_results)

# Create a data frame with coefficients, standard errors, and p-values
coefficients_imp <- data.frame(
  Variable = rownames(summary_imp),       
  `Log Odds (Estimate)` = summary_imp$estimate,         # Log Odds (Pooled Estimates)
  `Standard Error` = summary_imp$std.error,             # Standard Errors (Pooled)
  `z value` = summary_imp$statistic,                    # z-values
  `p-value` = summary_imp$p.value                       # p-values
)

# Export the data frame to a CSV file
write.csv(coefficients_imp, file = "Output/coefficients_imputed.csv", row.names = TRUE)

# tidy results 

tidy_imp = broom.mixed::tidy(
  pooled_results, conf.int=TRUE, # use pooled results if using imputed data
  exponentiate=TRUE,
  effects="fixed") # keep just fixed effects terms

tidy_imp = tidy_imp %>%
  dplyr::mutate(
    across(where(is.numeric), round,4),
    model = 'imputed data, iteration = 100, datasets = 5', # just indicating which data is used
    formula = formula) %>% # making sure you keep a trace of the formula / model spec that was used to get estimates
  dplyr::relocate(model, formula)


write.csv(tidy_imp, "Output/logit_imputed.csv", row.names = FALSE)


# Sensitivity analysis ----
# Complete case. Create new dataframe without Swindon. ----
# Look if the estimates look different. 

model_s = lme4::glmer(
  as.formula(formula), 
  data = model_data[model_data$la != 'Swindon',],
  family = binomial)

summary_swind <- summary(model_s)

# Create a data frame with coefficients, standard errors, and p-values
coefficients_swind <- data.frame(
  Coefficients = summary_swind$coefficients[, "Estimate"],       # Log Odds
  `Standard Error` = summary_swind$coefficients[, "Std. Error"],
  `z value` = summary_swind$coefficients[, "z value"],           # Optional
  `p-value` = summary_swind$coefficients[, "Pr(>|z|)"]
)

write.csv(coefficients_swind, "Output/coefficient_swind.csv", row.names = FALSE)

# Exponentiating odds rations 
tidy_s = broom.mixed::tidy(
  model_s, conf.int=TRUE, # use pooled results if using imputed data
  exponentiate=TRUE,
  effects="fixed") # keep just fixed effects terms

tidy_s = tidy_s %>%
  dplyr::mutate(
    across(where(is.numeric), round,4),
    model = 'complete case analysis', # just indicating which data is used
    formula = formula) %>% # making sure you keep a trace of the formula / model spec that was used to get estimates
  dplyr::relocate(model, formula)


write.csv(tidy_s, "Output/logit_wo_swindon.csv", row.names = FALSE)

View(tidy_s)

# Created an imputed dataset without Swindon ----
# Creating an example dataframe with only the variables of interest. 
model_data_swind <- cla_merge %>% select(la, `child_la_id`, `prev_cpp`, `gender1`, 
                                        ethnicity1, disability1, uasc1, age_group, 
                                        primary_outcome, treatment, time_period)

model_data_swind <- model_data_swind[model_data_swind$la != "Swindon", ]

# Prep formula 
individual_covariates <- c('gender1',
                           'ethnicity1',
                           'disability1',
                           'prev_cpp',
                           'uasc1',
                           'age_group')

individual_covariates <- paste0("`", individual_covariates, "`")
individual_covariates <- paste(individual_covariates, collapse = " + ")

# View missing data pattern 
mice::md.pattern(model_data_swind)

bin_predm <- make.predictorMatrix(model_data_swind)

bin_predm["ethnicity1", ] <- c(
  1, 0, 1, 1, #la: in, child la id: out, previous cpp1: in, gender1: in
  0,1,1,1,    #ethnicity1: out, disability1, uasc1, age_group
  1,1,1)    # primary_outcome, treatment, time_period


imputed_data_swind <- mice::mice(
  model_data_swind, # model data only includes what you need in your analysis, no other vars
  m = 5, 
  method = 'polyreg',
  seed = 123, 
  predictorMatrix = bin_predm,
  maxit = 100) # 1,000 iterations will take a while, suggest you start with 10 just to see if it works


# Without Swindon with imputed data
model_s_imp = with(
  imputed_data_swind,
  lme4::glmer(
    as.formula(formula), 
    family = binomial))

summary_swind_imp <- summary(model_s_imp)


# Check summary 
pooled_results <- mice::pool(model_s_imp) # pooling all the estimates across imputed datasets

#Exporting the log odds/coefficients 
summary_s_imp <- summary(pooled_results)

# Create a data frame with coefficients, standard errors, and p-values
coefficients_s_imp <- data.frame(
  Variable = rownames(summary_s_imp),       
  `Log Odds (Estimate)` = summary_s_imp$estimate,         # Log Odds (Pooled Estimates)
  `Standard Error` = summary_s_imp$std.error,             # Standard Errors (Pooled)
  `z value` = summary_s_imp$statistic,                    # z-values
  `p-value` = summary_s_imp$p.value                       # p-values
)

# Export the data frame to a CSV file
write.csv(coefficients_s_imp, file = "Output/coefficients_swind_imputed.csv", row.names = TRUE)

# tidy results 

tidy_s_imp = broom.mixed::tidy(
  pooled_results, conf.int=TRUE, # use pooled results if using imputed data
  exponentiate=TRUE,
  effects="fixed") # keep just fixed effects terms

tidy_s_imp = tidy_s_imp %>%
  dplyr::mutate(
    across(where(is.numeric), round,4),
    model = 'imputed data, iteration = 100, datasets = 5', # just indicating which data is used
    formula = formula) %>% # making sure you keep a trace of the formula / model spec that was used to get estimates
  dplyr::relocate(model, formula)


write.csv(tidy_s_imp, "Output/logit_swind_imputed.csv", row.names = FALSE)


# Sensitivity analysis
# Complete case. Create new dataframe without Telford. ----
# Look if the estimates look different.

model_t = lme4::glmer(
  as.formula(formula), 
  data = model_data[model_data$la != 'Telford',],
  family = binomial)

summary_telf <- summary(model_t)

#Exporting the log odds 
coefficients_telf <- data.frame(
  Coefficients = summary_telf$coefficients[, "Estimate"],       # Log Odds
  `Standard Error` = summary_telf$coefficients[, "Std. Error"],
  `z value` = summary_telf$coefficients[, "z value"],           # Optional
  `p-value` = summary_telf$coefficients[, "Pr(>|z|)"]
)

write.csv(coefficients_telf, "Output/coefficient_telford.csv", row.names = FALSE)


# Exponentiating the results in odds ratios
tidy_t = broom.mixed::tidy(
  model_t, conf.int=TRUE, # use pooled results if using imputed data
  exponentiate=TRUE,
  effects="fixed") # keep just fixed effects terms

tidy_t = tidy_t %>%
  dplyr::mutate(
    across(where(is.numeric), round,4),
    model = 'complete case analysis without telford', # just indicating which data is used
    formula = formula) %>% # making sure you keep a trace of the formula / model spec that was used to get estimates
  dplyr::relocate(model, formula)


write.csv(tidy_t, "Output/logit_wo_telford.csv", row.names = FALSE)

# Using imputed data without Telford ----
# Created an imputed dataset without Telford
# Creating an example dataframe with only the variables of interest. 
model_data_telf <- cla_merge %>% select(la, `child_la_id`, `prev_cpp`, `gender1`, 
                                   ethnicity1, disability1, uasc1, age_group, 
                                   primary_outcome, treatment, time_period)

model_data_telf <- model_data_telf[model_data_telf$la != "Telford", ]

# Prep formula 
individual_covariates <- c('gender1',
                           'ethnicity1',
                           'disability1',
                           'prev_cpp',
                           'uasc1',
                           'age_group')

individual_covariates <- paste0("`", individual_covariates, "`")
individual_covariates <- paste(individual_covariates, collapse = " + ")

# View missing data pattern 
mice::md.pattern(model_data_telf)

bin_predm <- make.predictorMatrix(model_data_telf)

bin_predm["ethnicity1", ] <- c(
  1, 0, 1, 1, #la: in, child la id: out, previous cpp1: in, gender1: in
  0,1,1,1,    #ethnicity1: out, disability1, uasc1, age_group
  1,1,1)    # primary_outcome, treatment, time_period


imputed_data_telf <- mice::mice(
  model_data_telf, # model data only includes what you need in your analysis, no other vars
  m = 5, 
  method = 'polyreg',
  seed = 123, 
  predictorMatrix = bin_predm,
  maxit = 100) # 1,000 iterations will take a while, suggest you start with 10 just to see if it works


# Without Telford with imputed data
model_t_imp = with(
  imputed_data_telf,
    lme4::glmer(
  as.formula(formula), 
  family = binomial))

summary_telf_imp <- summary(model_t_imp)


# Check summary 
pooled_results <- mice::pool(model_t_imp) # pooling all the estimates across imputed datasets

#Exporting the log odds/coefficients 
summary_t_imp <- summary(pooled_results)

# Create a data frame with coefficients, standard errors, and p-values
coefficients_t_imp <- data.frame(
  Variable = rownames(summary_t_imp),       
  `Log Odds (Estimate)` = summary_t_imp$estimate,         # Log Odds (Pooled Estimates)
  `Standard Error` = summary_t_imp$std.error,             # Standard Errors (Pooled)
  `z value` = summary_t_imp$statistic,                    # z-values
  `p-value` = summary_t_imp$p.value                       # p-values
)

# Export the data frame to a CSV file
write.csv(coefficients_t_imp, file = "Output/coefficients_telf_imputed.csv", row.names = TRUE)

# tidy results 

tidy_t_imp = broom.mixed::tidy(
  pooled_results, conf.int=TRUE, # use pooled results if using imputed data
  exponentiate=TRUE,
  effects="fixed") # keep just fixed effects terms

tidy_t_imp = tidy_t_imp %>%
  dplyr::mutate(
    across(where(is.numeric), round,4),
    model = 'imputed data, iteration = 100, datasets = 5', # just indicating which data is used
    formula = formula) %>% # making sure you keep a trace of the formula / model spec that was used to get estimates
  dplyr::relocate(model, formula)


write.csv(tidy_t_imp, "Output/logit_telf_imputed.csv", row.names = FALSE)

# Sensitivity analysis
# Complete case. Create new dataframe without Walsall. ----
# Look if the estimates look different.

model_wl = lme4::glmer(
  as.formula(formula), 
  data = model_data[model_data$la != 'Walsall',],
  family = binomial)

summary_walsall <- summary(model_wl)

#Exporting the log odds 
coefficients_walsall <- data.frame(
  Coefficients = summary_walsall$coefficients[, "Estimate"],       # Log Odds
  `Standard Error` = summary_walsall$coefficients[, "Std. Error"],
  `z value` = summary_walsall$coefficients[, "z value"],           # Optional
  `p-value` = summary_walsall$coefficients[, "Pr(>|z|)"]
)

write.csv(coefficients_walsall, "Output/coefficient_walsall.csv", row.names = FALSE)


# Exponentiating the results in odds ratios
tidy_wl = broom.mixed::tidy(
  model_wl, conf.int=TRUE, # use pooled results if using imputed data
  exponentiate=TRUE,
  effects="fixed") # keep just fixed effects terms

tidy_wl = tidy_wl %>%
  dplyr::mutate(
    across(where(is.numeric), round,4),
    model = 'complete case analysis without telford', # just indicating which data is used
    formula = formula) %>% # making sure you keep a trace of the formula / model spec that was used to get estimates
  dplyr::relocate(model, formula)


write.csv(tidy_wl, "Output/logit_wo_walsall.csv", row.names = FALSE)


# Sensitivity analysis
# Complete case. Create new dataframe without Wandsworth. ----
# Look if the estimates look different.

model_wn = lme4::glmer(
  as.formula(formula), 
  data = model_data[model_data$la != 'Wandsworth',],
  family = binomial)

summary_wands <- summary(model_wn)

#Exporting the log odds 
coefficients_wands <- data.frame(
  Coefficients = summary_wands$coefficients[, "Estimate"],       # Log Odds
  `Standard Error` = summary_wands$coefficients[, "Std. Error"],
  `z value` = summary_wands$coefficients[, "z value"],           # Optional
  `p-value` = summary_wands$coefficients[, "Pr(>|z|)"]
)

write.csv(coefficients_wands, "Output/coefficient_wands.csv", row.names = FALSE)


# Exponentiating the results in odds ratios
tidy_wn = broom.mixed::tidy(
  model_wn, conf.int=TRUE, # use pooled results if using imputed data
  exponentiate=TRUE,
  effects="fixed") # keep just fixed effects terms

tidy_wn = tidy_wn %>%
  dplyr::mutate(
    across(where(is.numeric), round,4),
    model = 'complete case analysis without telford', # just indicating which data is used
    formula = formula) %>% # making sure you keep a trace of the formula / model spec that was used to get estimates
  dplyr::relocate(model, formula)


write.csv(tidy_wn, "Output/logit_wo_wandsworth.csv", row.names = FALSE)


# Sensitivity analysis
# Complete case. Create new dataframe without Lancashire. ----
# Look if the estimates look different.

model_l = lme4::glmer(
  as.formula(formula), 
  data = model_data[model_data$la != 'Lancashire',],
  family = binomial)

summary_wands <- summary(model_wn)

#Exporting the log odds 
coefficients_wands <- data.frame(
  Coefficients = summary_wands$coefficients[, "Estimate"],       # Log Odds
  `Standard Error` = summary_wands$coefficients[, "Std. Error"],
  `z value` = summary_wands$coefficients[, "z value"],           # Optional
  `p-value` = summary_wands$coefficients[, "Pr(>|z|)"]
)

write.csv(coefficients_wands, "Output/coefficient_wands.csv", row.names = FALSE)


# Exponentiating the results in odds ratios
tidy_wn = broom.mixed::tidy(
  model_wn, conf.int=TRUE, # use pooled results if using imputed data
  exponentiate=TRUE,
  effects="fixed") # keep just fixed effects terms

tidy_wn = tidy_wn %>%
  dplyr::mutate(
    across(where(is.numeric), round,4),
    model = 'complete case analysis without telford', # just indicating which data is used
    formula = formula) %>% # making sure you keep a trace of the formula / model spec that was used to get estimates
  dplyr::relocate(model, formula)


write.csv(tidy_wn, "Output/logit_wo_wandsworth.csv", row.names = FALSE)

################################################################################

# Loading in the data with the cla and cpp variable merged
load("Output/merged_cla_rate.RData")
load("Output/merged_cpp_rate.RData")

# Running the model 

# CLA 
merge_cla_rate <- merge_cla_rate %>%
  rename(
    child_la_id = `child la id`,
    prev_cpp = `previous cpp1`
  )


# Factor 
merge_cla_rate <- merge_cla_rate %>%
  mutate(
    
    gender1 = relevel(
      factor(gender1), ref = 'Male'),
    
    ethnicity1 = relevel(
      factor(ethnicity1), ref = 'White British or Irish'),
    
    disability1 = relevel(
      factor(disability1), ref = '0'),
    
    uasc1 = relevel(
      factor(uasc1), ref = '0'),
    
    `prev_cpp` = relevel(
      factor(`prev_cpp`), ref = '0'),
    
    age_group = relevel(
      factor(age_group), ref = 'unborn'),
    
    la = relevel(
      factor(la), ref = 'Lancashire'
    )
  )


# Multiple Imputation: 

# Must add to individual covariates: UASC and age at ref 

# Creating an example dataframe with only the variables of interest. 
model_data <- merge_cla_rate %>% select(la, `child_la_id`, `prev_cpp`, `gender1`, 
                                   ethnicity1, disability1, uasc1, age_group, 
                                   primary_outcome, treatment, time_period, `cla rate`)

# Rename cla rate 
model_data <- model_data %>%
  rename(`cla_rate` = `cla rate`)

# Prep formula 
individual_covariates <- c('gender1',
                           'ethnicity1',
                           'disability1',
                           'prev_cpp',
                           'uasc1',
                           'age_group'
)

individual_covariates <- paste0("`", individual_covariates, "`")
individual_covariates <- paste(individual_covariates, collapse = " + ")

# Time varying covariate 
la_covariate <- "`cla_rate`"  

# Adding time varying and individual 
all_covariates <- paste(individual_covariates, la_covariate, sep = " + ")


# View missing data pattern 
mice::md.pattern(model_data)

bin_predm <- make.predictorMatrix(model_data)

bin_predm["ethnicity1", ] <- c(
  1, 0, 1, 1, #la: in, child la id: out, previous cpp1: in, gender1: in
  0,1,1,1,    #ethnicity1: out, disability1, uasc1, age_group
  1,1,1,1)    # primary_outcome, treatment, time_period, cla rate


imputed_data <- mice::mice(
  model_data, # model data only includes what you need in your analysis, no other vars
  m = 5, 
  method = 'polyreg',
  seed = 123, 
  predictorMatrix = bin_predm,
  maxit = 100) # 1,000 iterations will take a while, suggest you start with 10 just to see if it works


# Check logged events 
imputed_data$loggedEvents

# Check the imputed values for 'ethnicity'
imputed_data$imp$ethnicity1

## Imputation checks ----

# Check convergence 
plot(imputed_data)

# Visualize observed vs imputed values for ethnicity
imp_plot = propplot(imputed_data, 
                    label_size = 10,
                    show_prop = TRUE,
                    prop_size = 2)

imp_plot_trt = propplot(
  imputed_data,
  ethnicity1 ~ treatment,
  label_size = 7) 

imp_plot_la = propplot(
  imputed_data, 
  ethnicity1 ~ la,
  label_size = 5) 

# Formular for model 
formula = paste0(
  "primary_outcome ~ treatment + time_period + ",
  all_covariates,  " + (1 | la)")

# Imputed model ----

model_imputed_cla = with(
  imputed_data,  # df is a list with imputed data, the output of the mice function above
  lme4::glmer(
    as.formula(formula), 
    family = binomial))


# Check summary 
pooled_results <- mice::pool(model_imputed_cla) # pooling all the estimates across imputed datasets

#Exporting the log odds/coefficients 
summary_imp <- summary(pooled_results)

# Create a data frame with coefficients, standard errors, and p-values
coefficients_imp_cla <- data.frame(
  Variable = rownames(summary_imp),       
  `Log Odds (Estimate)` = summary_imp$estimate,         # Log Odds (Pooled Estimates)
  `Standard Error` = summary_imp$std.error,             # Standard Errors (Pooled)
  `z value` = summary_imp$statistic,                    # z-values
  `p-value` = summary_imp$p.value                       # p-values
)

# Export the data frame to a CSV file
write.csv(coefficients_imp_cla, file = "Output/coeff_imputed_cla.csv", row.names = TRUE)

# tidy results 

tidy_imp_cla = broom.mixed::tidy(
  pooled_results, conf.int=TRUE, # use pooled results if using imputed data
  exponentiate=TRUE,
  effects="fixed") # keep just fixed effects terms

tidy_imp_cla = tidy_imp_cla %>%
  dplyr::mutate(
    across(where(is.numeric), round,4),
    model = 'imputed data, iteration = 100, datasets = 5', # just indicating which data is used
    formula = formula) %>% # making sure you keep a trace of the formula / model spec that was used to get estimates
  dplyr::relocate(model, formula)


write.csv(tidy_imp_cla, "Output/logit_imputed_cla.csv", row.names = FALSE)


# CPP
merge_cpp_rate <- merge_cpp_rate %>%
  rename(
    child_la_id = `child la id`,
    prev_cpp = `previous cpp1`
  )


# Factor 
merge_cpp_rate <- merge_cpp_rate %>%
  mutate(
    
    gender1 = relevel(
      factor(gender1), ref = 'Male'),
    
    ethnicity1 = relevel(
      factor(ethnicity1), ref = 'White British or Irish'),
    
    disability1 = relevel(
      factor(disability1), ref = '0'),
    
    uasc1 = relevel(
      factor(uasc1), ref = '0'),
    
    `prev_cpp` = relevel(
      factor(`prev_cpp`), ref = '0'),
    
    age_group = relevel(
      factor(age_group), ref = 'unborn'),
    
    la = relevel(
      factor(la), ref = 'Lancashire'),
      
      readiness = relevel(
        factor(readiness), ref = 'Low readiness')
    )
  


# Multiple Imputation: 

# Must add to individual covariates: UASC and age at ref 

# Creating an example dataframe with only the variables of interest. 
model_data <- merge_cpp_rate %>% select(la, `child_la_id`, `prev_cpp`, `gender1`, 
                                        ethnicity1, disability1, uasc1, age_group, 
                                        primary_outcome, treatment, time_period, readiness, `cpp_rate`)


# Prep formula 
individual_covariates <- c('gender1',
                           'ethnicity1',
                           'disability1',
                           'prev_cpp',
                           'uasc1',
                           'age_group'
)

individual_covariates <- paste0("`", individual_covariates, "`")
individual_covariates <- paste(individual_covariates, collapse = " + ")

la_covariate <- "`cpp_rate`"

all_covariates <- paste(individual_covariates, la_covariate, sep = " + ")


# View missing data pattern 
mice::md.pattern(model_data)

bin_predm <- make.predictorMatrix(model_data)

bin_predm["ethnicity1", ] <- c(
  1, 0, 1, 1, #la: in, child la id: out, previous cpp1: in, gender1: in
  0,1,1,1,    #ethnicity1: out, disability1, uasc1, age_group
  1,1,1,1,1)    # primary_outcome, treatment, time_period, readiness, cpp_rate


imputed_data <- mice::mice(
  model_data, # model data only includes what you need in your analysis, no other vars
  m = 5, 
  method = 'polyreg',
  seed = 123, 
  predictorMatrix = bin_predm,
  maxit = 100) # 1,000 iterations will take a while, suggest you start with 10 just to see if it works


# Check logged events 
imputed_data$loggedEvents

# Check the imputed values for 'ethnicity'
imputed_data$imp$ethnicity1

## Imputation checks ----

# Check convergence 
plot(imputed_data)

# Visualize observed vs imputed values for ethnicity
imp_plot = propplot(imputed_data, 
                    label_size = 10,
                    show_prop = TRUE,
                    prop_size = 2)

imp_plot_trt = propplot(
  imputed_data,
  ethnicity1 ~ treatment,
  label_size = 7) 

imp_plot_la = propplot(
  imputed_data, 
  ethnicity1 ~ la,
  label_size = 5) 

# Formular for model 
formula = paste0(
  "primary_outcome ~ treatment + time_period + ",
  all_covariates,  " + (1 | la)")

# Imputed model ----

model_imputed_cpp = with(
  imputed_data,  # df is a list with imputed data, the output of the mice function above
  lme4::glmer(
    as.formula(formula), 
    family = binomial))


# Check summary 
pooled_results <- mice::pool(model_imputed_cla) # pooling all the estimates across imputed datasets

#Exporting the log odds/coefficients 
summary_imp <- summary(pooled_results)

# Create a data frame with coefficients, standard errors, and p-values
coefficients_imp_cpp <- data.frame(
  Variable = rownames(summary_imp),       
  `Log Odds (Estimate)` = summary_imp$estimate,         # Log Odds (Pooled Estimates)
  `Standard Error` = summary_imp$std.error,             # Standard Errors (Pooled)
  `z value` = summary_imp$statistic,                    # z-values
  `p-value` = summary_imp$p.value                       # p-values
)

# Export the data frame to a CSV file
write.csv(coefficients_imp_cpp, file = "Output/coeff_imputed_cla.csv", row.names = TRUE)

# tidy results 

tidy_imp_cpp = broom.mixed::tidy(
  pooled_results, conf.int=TRUE, # use pooled results if using imputed data
  exponentiate=TRUE,
  effects="fixed") # keep just fixed effects terms

tidy_imp_cpp = tidy_imp_cpp %>%
  dplyr::mutate(
    across(where(is.numeric), round,4),
    model = 'imputed data, iteration = 100, datasets = 5', # just indicating which data is used
    formula = formula) %>% # making sure you keep a trace of the formula / model spec that was used to get estimates
  dplyr::relocate(model, formula)


write.csv(tidy_imp_cpp, "Output/logit_imputed_cpp.csv", row.names = FALSE)

# TABLE FOR TAVISTOCK----
#What the table is: total children referred in LA by trial wedge, total children in care in LA by trial wedge, and out of those referred during the trial wedge and LA, how many of these will experience the outcome, as well as the cumulative sum for these numbers.
outcome_desc_for_tavistock = cla_merge %>% 
  dplyr::group_by(la, time_period) %>%
  dplyr::summarise(
    children_eligible_referred = n(), # total nb of children referred during wedge
    children_referred_with_previous_cpp_plans = sum(
      prev_cpp != "0"), 
    children_looked_after = sum( # total number of children looked after during wedge
      !is.na(`cla date`)),
    children_referred_who_became_looked_after_within_18_months= sum(
      primary_outcome),
    children_referred_with_previous_cpp_and_who_became_looked_after_during_the_trial = sum(
      primary_outcome == 1 & prev_cpp != "0")) %>% # total number experiencing the outcome out of the children referred during wedge
  dplyr::mutate( # cumulative numbers
    cumulative_number_of_eligible_children = cumsum(
      children_eligible_referred),
    cumulative_number_of_eligible_children_with_previous_cpp_plans = cumsum(
      children_referred_with_previous_cpp_plans),
    cumulative_number_of_children_in_care = cumsum(
      children_looked_after),
    cumulative_number_of_children_experiencing_outcome = cumsum(
      primary_outcome),
    cumulative_number_of_children_referred_with_previous_cpp_and_who_became_looked_after_during_the_trial = cumsum(
      children_referred_with_previous_cpp_and_who_became_looked_after_during_the_trial)) %>%
  ungroup() %>%
  dplyr::mutate(across(
    .cols = where(is.numeric), 
    .fns = ~ ifelse(.x < 10, '[z]', .x))) # suppression checks
