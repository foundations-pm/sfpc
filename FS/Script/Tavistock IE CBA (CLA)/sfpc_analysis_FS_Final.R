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

# Loading packages ----
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
library(performance)
library(naniar)


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

load("Output/cla_merge_pri_outcome.RData")


# Model as per the protocol---- 

# A normally distributed random intercept at the level of the cluster. 
# This random effect estimates the stochastic variation of individual clusters 
# around the conditional mean of the clusters.
# Multiple Imputation: 


#Creating an example dataframe with only the variables of interest. 
model_data <- cla_merge %>% select(la, `child la id`, `previous cpp1`, `gender1`, 
                                   ethnicity1, disability1, uasc1, age_group, 
                                   primary_outcome, treatment, time_period, 
                                   white_british_percentage_in_month, turnover_rate_fte,
                                   `no_children`)


model_data <- model_data %>%
  rename(
    prev_cpp = `previous cpp1`,
    child_la_id = `child la id`)

# Factor 
model_data <- model_data %>%
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
      factor(age_group), ref = '3 and under'),
    
    la = relevel(
      factor(la), ref = 'Lancashire'),
    
    primary_outcome = relevel(
      factor(primary_outcome), ref = '0'),
    
    treatment = relevel(
      factor(treatment), ref = '0'),
    
    time_period = relevel(
      factor(time_period), ref = 'Baseline')
  )

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


la_covariate <- c("`white_british_percentage_in_month`",
                  "`turnover_rate_fte`",
                  "`no_children`")

all_covariates <- paste(individual_covariates, la_covariate, sep = " + ")


# View missing data pattern 
mice::md.pattern(model_data)

#Create imputed dataset----

bin_predm <- mice::make.predictorMatrix(model_data)

bin_predm["ethnicity1", ] <- c(
  1, 0, 1, 1, #la: in, child la id: out, previous cpp1: in, gender1: in
  0,1,1,1,    #ethnicity1: out, disability1, uasc1, age_group
  1,1,1,    # primary_outcome, treatment, time_period, 
  0,0,0)   # white btitish, turnover, children


# Drop predictors from matrix that should not be used to predict anything
bin_predm[, c("child_la_id", "white_british_percentage_in_month", 
              "turnover_rate_fte", "no_children")] <- 0

# Define methods vector
methods <- rep("polyreg", ncol(model_data))
names(methods) <- colnames(model_data)

# Skip imputation for selected variables
methods[c("child_la_id", "white_british_percentage_in_month", 
          "turnover_rate_fte", "no_children", 
          "age_group", "disability1")] <- ""

imputed_data <- mice::mice(
  model_data, # model data only includes what you need in your analysis, no other vars
  m = 5, 
  method = methods,
  seed = 123, 
  predictorMatrix = bin_predm,
  maxit = 100) # 1,000 iterations will take a while, suggest you start with 10 just to see if it works


print(bin_predm)
print(methods)
model_data:str(model_data)
colSums(is.na(model_data))
sum(is.na(model_data$ethnicity1))
sum(is.na(cla_merge$ethnicity1))

saveRDS(imputed_data, file = "Output/imputed_data.rds")

# Check logged events 
imputed_data$loggedEvents

# Check the imputed values for 'ethnicity'
imputed_data$imp$ethnicity1

#Imputation checks ----

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


#Prep formula ----
individual_covariates <- c('gender1',
                           'ethnicity1',
                           'age_group',
                           'disability1',
                           'uasc1',
                           'prev_cpp')

individual_covariates <- paste0("`", individual_covariates, "`")
individual_covariates <- paste(individual_covariates, collapse = " + ")

la_covariate <- c("`white_british_percentage_in_month`",
                  "`turnover_rate_fte`",
                  "`no_children`" )

all_covariates <- paste(individual_covariates, str_flatten(la_covariate, ' + '), sep = " + ")


formula = paste0(
  "primary_outcome ~ treatment + time_period + ",
  all_covariates,  " + (1 | la)")

imputed_data_born = lapply(
  imputed_data[1], function(df){
  
   dplyr::filter(df, age_group != 'unborn')
  
})

#Run model (specified, born+unborn, imputed data)----
model_timevar = with(
  imputed_data, 
  lme4::glmer(
    as.formula(formula), 
    family = binomial))

model_timevar_born = lme4::glmer(
  data = model_data[model_data$age_group != 'unborn',],
  as.formula(formula), 
  family = binomial)

model_timevar_unborn = lme4::glmer(
  data = model_data[model_data$age_group == 'unborn',],
  as.formula('primary_outcome ~ treatment + time_period + (1 | la)'), 
  family = binomial)

# Check summary 
pooled_results <- mice::pool(model_timevar) # pooling all the estimates across imputed datasets

#Exporting the log odds/coefficients 
summary_timevar <- summary(pooled_results)

#Create a data frame with coefficients, standard errors, and p-values ----
coefficients_timevar <- data.frame(
  Variable = rownames(summary_timevar),       
  `Log Odds (Estimate)` = summary_timevar$estimate,         # Log Odds (Pooled Estimates)
  `Standard Error` = summary_timevar$std.error,             # Standard Errors (Pooled)
  `z value` = summary_timevar$statistic,                    # z-values
  `p-value` = summary_timevar$p.value                       # p-values
)

# Export the data frame to a CSV file
write.csv(coefficients_timevar, file = "Output/coeff_timevar.csv", row.names = TRUE)

# tidy results 

tidy_timevar = broom.mixed::tidy(
  pooled_results, conf.int=TRUE, # use pooled results if using imputed data
  exponentiate=TRUE,
  effects="fixed") # keep just fixed effects terms

tidy_timevar = tidy_timevar %>%
  dplyr::mutate(
    across(where(is.numeric), round,4),
    model = 'protocol specificed, with time varying indicators, using imputed data, iteration = 100, datasets = 5', # just indicating which data is used
    formula = paste(formula, collapse = " + ")) %>% # making sure you keep a trace of the formula / model spec that was used to get estimates
  dplyr::relocate(model, formula)


write.csv(tidy_timevar, "Output/logit_timevar.csv", row.names = FALSE)


#Diagnostics----
m1_list <- model_timevar$analyses

names_m1 <- paste0("imp", seq_along(m1_list))
names(m1_list) <- names_m1 

#1 diag #1 = warning messages 
warning_messages_model_timevar = get_optimisers_warning_messages(
  glmer_model_fit = model_timevar$analyses[[1]],
  formula = formula,
  analysis_type = 'GLMER - Imputed Dataset m=5 - Born and Unborn sample'
)

#2 diag #2 is multicollinearity

vif_model_timevar = performance::check_collinearity(
  model_timevar$analyses[[1]])  

#3 diag #3 is goodness of fit metrics

s1_glmer_performance_table <- performance::model_performance(
  model_timevar$analyses[[1]]) %>%
      as.data.frame() %>%
      dplyr::mutate(
        #analysis_type = name_index,
        formula = formula,
        date = Sys.Date())

#Adjusted model (adjusted, born+unborn, imputed data)----
# For child i referred to CSC in la during trial period. 
# Where the vector of LA is simplifed with proportion White British children aged XX referred to CSC at the time of referral of child in LA at time period.  

# Creating model data
model_data <- cla_merge %>% select(la, `child la id`,
                                   primary_outcome, treatment, time_period, 
                                   white_british_percentage_in_month)

model_data <- model_data %>%
  rename(
    child_la_id = `child la id`)

# Factor 
model_data <- model_data %>%
  mutate(
    
    la = relevel(
      factor(la), ref = 'Lancashire'),
    
    primary_outcome = relevel(
      factor(primary_outcome), ref = '0'),
    
    treatment = relevel(
      factor(treatment), ref = '0'),
    
    time_period = relevel(
      factor(time_period), ref = 'Baseline')
  )

la_covariate <- c("`white_british_percentage_in_month`")

mice::md.pattern(model_data) #No need to impute

#Running model----
formula = paste0(
  "primary_outcome ~ treatment + time_period + ",
  la_covariate,  " + (1 | la)")

model_adjusted = with(
  model_data, 
  lme4::glmer(
    as.formula(formula), 
    family = binomial))

#Exporting the log odds/coefficients----
model_adj <- summary(model_adjusted)

# Create a data frame with coefficients, standard errors, and p-values
coefficients_adj <- data.frame(
  Coefficients = model_adj$coefficients[, "Estimate"],       # Log Odds
  `Standard Error` = model_adj$coefficients[, "Std. Error"],
  `z value` = model_adj$coefficients[, "z value"],           # Optional
  `p-value` = model_adj$coefficients[, "Pr(>|z|)"]
)

# Export the data frame to a CSV file
write.csv(coefficients_adj, file = "Output/coefficients_adjusted.csv", row.names = TRUE)

# Exponentiating odds rations 
tidy_adjusted = broom.mixed::tidy(
  model_adjusted, conf.int=TRUE, # use pooled results if using imputed data
  exponentiate=TRUE,
  effects="fixed") # keep just fixed effects terms

tidy_adjusted = tidy_adjusted %>%
  dplyr::mutate(
    across(where(is.numeric), round,4),
    model = 'model adjusted', # just indicating which data is used
    formula = formula) %>% # making sure you keep a trace of the formula / model spec that was used to get estimates
  dplyr::relocate(model, formula)

View(tidy_adjusted)

write.csv(tidy_adjusted, "Output/logit_adjusted.csv", row.names = FALSE)



#Model sensitivity (specified, born only, imputed)----

# Creating imputed dataset without unborn babies 
model_data_born <- model_data[model_data$age_group != "unborn", ]

model_data_born <- model_data_born %>%
  filter(!if_all(everything(), is.na))

model_data_born <- model_data_born %>%
  filter(!is.na(disability1))

mice::md.pattern(model_data_born)

miss_var_summary(model_data_born)

bin_predm <- make.predictorMatrix(model_data_born)

bin_predm["ethnicity1", ] <- c(
  1, 0, 1, 1, #la: in, child la id: out, previous cpp1: in, gender1: in
  0,1,1,1,    #ethnicity1: out, disability1, uasc1, age_group
  1,1,1,    # primary_outcome, treatment, time_period, 
  0,0,0)   # white btitish, turnover, children


# Drop predictors from matrix that should not be used to predict anything
bin_predm[, c("child_la_id", "white_british_percentage_in_month", 
              "turnover_rate_fte", "no_children")] <- 0

# Define methods vector
methods <- rep("polyreg", ncol(model_data_born))
names(methods) <- colnames(model_data_born)

# Skip imputation for selected variables
methods[c("child_la_id", "disability1", "white_british_percentage_in_month", 
          "turnover_rate_fte", "no_children", 
          "age_group")] <- ""

imputed_data_born <- mice::mice(
  model_data_born, # model data only includes what you need in your analysis, no other vars
  m = 5, 
  method = 'polyreg',
  seed = 123, 
  predictorMatrix = bin_predm,
  maxit = 100) # 1,000 iterations will take a while, suggest you start with 10 just to see if it works


# Writing the formular 
individual_covariates <- c('gender1',
                           'ethnicity1',
                           'age_group',
                           'disability1',
                           'uasc1',
                           'prev_cpp')

individual_covariates <- paste0("`", individual_covariates, "`")
individual_covariates <- paste(individual_covariates, collapse = " + ")

la_covariate <- c("`white_british_percentage_in_month`",
                  "`turnover_rate_fte`",
                  "`no_children`" )

all_covariates <- paste(individual_covariates, str_flatten(la_covariate, ' + '), sep = " + ")


formula = paste0(
  "primary_outcome ~ treatment + time_period + ",
  all_covariates,  " + (1 | la)")



# Running the model 
model_timevar_born = lme4::glmer(
  data = model_data[model_data$age_group != 'unborn',],
  as.formula(formula), 
  family = binomial)

model_timevar_born = with(
  imputed_data_born, 
  lme4::glmer(
    as.formula(formula), 
    family = binomial))


#Model sensitivity (specified, unborn only, non imputed, do demographics)
model_timevar_unborn = lme4::glmer(
  data = model_data[model_data$age_group == 'unborn',],
  as.formula('primary_outcome ~ treatment + time_period + (1 | la)'), 
  family = binomial)

# Writing optimiser formular----
get_optimisers_warning_messages = function(
    glmer_model_fit,
    formula,
    analysis_type){
  
  aa <- lme4::allFit(glmer_model_fit)
  ss_list <- summary(aa) 
  
  # Convert into table 
  ss_df = data.frame(
    Optimizer = names(ss_list$msgs),
    Message = sapply(
      ss_list$msgs, 
      function(msg) {
        if (is.null(msg)) {
          "[OK]"  # Replace NULL with "[OK]" or use NA if preferred
        } else {
          msg  # Keep the warning message
        }
      }),
    stringsAsFactors = FALSE)
  
  ss_df <- ss_df %>%
    pivot_wider(names_from = Optimizer, 
                values_from = Message)
  
  ss_df = ss_df %>%
    dplyr::mutate(analysis_type = analysis_type,
                  formula = formula,
                  date = date) %>%
    dplyr::relocate(analysis_type, formula)
  
}

