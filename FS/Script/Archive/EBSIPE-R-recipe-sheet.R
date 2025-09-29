#############################
# EBSIPE R recipe sheet
# last updated 10 December 2020
#############################


## R Basics ------------

## Operations in the console 
50+5 # addition
50-5 # subtraction
55*3 # multiplication
55/5 # division
5^2  # 5 to the power of 2

## Functions and help
sqrt(25)
?sqrt

## Saving information in objects  
## Pressing Alt and - is a keyboard shortcut for writing the assignment operator (<-) in RStudio.  
saved1 <- 5+5
saved1

## Text (or 'character strings')
saved2 <- "Hello EBSIPE"
saved2

## Remove objects
rm(saved2) 

## Vectors
saved3 <- c(5,55,555,6,77,7,1)
saved3

## Information about an object's characteristics 
saved3 <- c(5,55,555,6,77,7,1)
length(saved3) # how many pieces of information
class(saved3) # the object class

## Comparison operations - result is TRUE or FALSE
6 > 3 # is 6 greater than 3?
6 < 3 # is 6 less than 3?
6 == 3 # is 6 equal to 3?
6 != 3 # is 6 not equal to 3?
6 %in% c(2,3,4,5,6) # is the number 6 included in the list of numbers?
6 >= 3 # is 6 greater than or equal to 3?
6 <= 3 # is 6 less than or equal to 3


## Installing and loading packages ---------------

install.packages("tidyverse") # only needs doing once per computer
library(tidyverse) # needs doing every R session you use it

# check whether a package is installed
"servr" %in% rownames(installed.packages())


## Reading in data --------------

dolab1 <- read_csv("../data/dolab1.csv")

# examining a data frame
dim(dolab1) # number of rows and columns (short for 'dimensions')
View(dolab1) # similar view to Excel, but you can not change anything.
names(dolab1) # column names
head(dolab1) # view the first few rows
str(dolab1) # structure of the data
glimpse(dolab1) # alternative to str()
nrow(dolab1) # number of rows
ncol(dolab1) # number of columns 
class(dolab1) # object class(es)
summary(dolab1) # summary of variables 


## Subsetting data ---------

library(tidyverse) # load the package

# ctrl + shift + M is a shortcut for typing the pipe operator %>%

## Selecting variables by name
dolab1 %>% select(gender) # No comma needed
dolab1 %>% select(gender, TreatmentGroup) # two variables

# Selecting rows with certain attributes 
dolab1 %>% filter(id==188)
dolab1 %>% filter(gender=="female")

# We can also expand this easily to combine several criteria: e.g. female and aged 10
dolab1 %>% filter(gender=="female" & B_Age==10)


## Creating new variables--------------

# using the $ selector 
dolab1$newvar <- 1
dolab1$newvar <- "Mark"

# using mutate 
dolab1 <- dolab1 %>% mutate(newvar = 1)
dolab1 <- dolab1 %>% mutate(newvar = "Mark")

## Removing variables
dolab1 <- dolab1 %>% select(-newvar)

## Calculating new variables  
dolab1 <- dolab1 %>% mutate(BMI = Weight/Height^2)

## New variables conditional on existing ones  
dolab1 <- dolab1 %>% 
  mutate(old_child = ifelse(B_Age > 9, yes = 1, no = 0) )


## Creating a categorical or 'factor' variable====

# levels are the values in the dataset
# labels are the descriptive text that tell you what the levels refer to
dolab1 <- dolab1 %>% 
  mutate(treat_group_f = factor(treat_group, 
                                levels = c(0,1), 
                                labels = c("control", "treatment" ) ) )

# Find out what levels a variable has								
levels(dolab1$treat_group_f)


## Descriptive statistics --------------

# whole dataset
summary(dolab1)
# single variable
summary(dolab1$Height) 

# or use describe() from the psych package
library(psych)
describe(dolab1)
describe(dolab1$Height) 
describe(dolab1$Height, fast = T) # the more common statistics

# use describe for certain variables in a dataset, and certain descriptive statistics
dolab1 %>% 
  select(weight, height, b_Age) %>% # select these variables
  describe() %>% 
  select(n, mean, sd) # select these descriptive statistics (describe gives a lot as default)

# counting the number of observations
dolab1 %>% nrow() # this works for data frames
dolab1$b_age %>% length() # this works for single vectors
dolab1$height %>% na.omit() %>% length() # remove the NAs before counting

# finding the number and proportion of missing observations
is.na(dolab1$height) %>% sum() # number
is.na(dolab1$height) %>% mean() # proportion

# interval / numerical variables
mean(dolab1$height, na.rm = TRUE)
sd(dolab1$height, na.rm = TRUE)
range(dolab1$height, na.rm = TRUE)
quantile(dolab1$height, probs = c(0.25, 0.5, 0.75), na.rm = T ) # quantiles
IQR(dolab1$height, na.rm = T) # interquartile range


## Describing categorical variables ---------------------

# a simple table
table(dolab1$genderm, useNA = "ifany")

# Cross-tabulations and percentages  
dolab1 %>% 
  select(gender,b_age) %>% 
  table(useNA = "ifany")

# Table totals ('margins')  
dolab1 %>% 
  select(gender,b_age) %>% 
  table(useNA = "ifany") %>% 
  addmargins()

# table of proportions and percentages
dolab1 %>% 
  select(gender,b_age) %>% 
  table(useNA = "ifany") %>% 
  prop.table()  %>% 
  round(3)      %>%   # round to three decimal places
  `*`(100)      %>%   # 30% instead of 0.3
  addmargins()        # For totals (also known as margins)

# table of proportions and percentages - changing the totals
dolab1 %>% 
  select(gender,b_age) %>% 
  table(useNA = "ifany") %>% 
  # margin = 2 calculates proportion within columns
  prop.table(margin = 2)  %>% 
  round(3)      %>%  
  `*`(100)      %>%  
  # margin = 1 calculates column totals only
  addmargins(margin = 1)  


## Summarising variables using summarise() and group_by() ---------

dolab1 %>% 
  summarise(avg_age = mean(b_age) )

# What if we wanted to compare the average age of boys and girls?  
dolab1 %>% 
  group_by(gender) %>% 
  summarise(avg_age = mean(b_age) ) %>% 
  ungroup()


## Reshaping a dataset

dolab1_long <- dolab1 %>% 
  gather(b_read_perc, pi_read_perc, 
         key = "time_read_perc", 
         value = "read_perc")


## Charts / data visualisation --------------

# quick functions for simple charts
hist(dolab1$weight)
boxplot(dolab1$weight)
plot(dolab1$b_read_perc, dolab1$pi_read_perc)

# make sure tidyverse is loaded to use ggplot()
library(tidyverse)

# histogram
ggplot(dolab1) + # first the dataset
  aes(x = weight) + # then the variables to be visualised
  geom_histogram() # then the geometric object to be used

# histogram side by side
ggplot(dolab1) +
  aes(x = weight, y = stat(density) ) + # stat(density) gives density on the y axis
  geom_histogram(bins = 20) + # bins sets the number of bars on the histogram
  facet_wrap(~gender) # this line produces separate charts for each value of gender

# boxplot
ggplot(dolab1) +
  aes(y = weight) +
  geom_boxplot()

# boxplots side by side
ggplot(dolab1) + 
  aes(y = weight) +
  geom_boxplot() + 
  facet_wrap(~gender)

# bar plot for categorical variable
# for code to create the categorical variable, see above
ggplot(dolab1) + 
  aes(x = treat_group_f) + 
  geom_bar() +
  coord_flip()

# scatterplot for two numerical variables
ggplot(dolab1) + 
  aes(x = b_read_perc, y = pi_read_perc) +
  geom_point()
  
# adding a title, axis labels and data source
# create the chart
ggplot(dolab1) +
  aes(x = weight, y = stat(density) )  +
  geom_histogram() +
  # add title, axis labels and data source
  labs(title = "Distribution of Weight in DOLAB1 study", 
       x = "Weight in kg", 
       y = "Probability density of weight", 
       caption = "Source: DOLAB1 dataset") 

# quantile-quantile plot 
qqplot(dolab1 %>% filter(treat_group==0) %>% pull(b_read_perc), 
       dolab1 %>% filter(treat_group==1) %>% pull(b_read_perc) )
abline(0,1)


## Linear regression ---------

# load package and read data
library(tidyverse) 
dolab1 <- read_csv("../data/dolab1.csv")

# correlation coefficient
cor(dolab1$b_read_perc, dolab1$pi_read_perc, use = "complete.obs") 

# simple linear regression
dolab_model <- lm(pi_read_perc ~ b_read_perc,  
                data = dolab1)  

# including a squared term for age
dolab_model2 <- lm(pi_read_perc ~ b_read_perc + b_age + I(b_age^2),  
                  data = dolab1)  

# including an interaction term
dolab_model3 <- lm(pi_read_perc ~ b_read_perc + b_age + b_read_perc:b_age,  
                   data = dolab1)  

# getting information from the model object
dolab_model # basic information
summary(dolab_model) # more information
summary(dolab_model)$r.squared #R2 value
summary(dolab_model)$adj.r.squared # adjusted (important for multiple regression)
coef(dolab_model)
dolab_model$coefficients
dolab_model$residuals
dolab_model$fitted.values

# list of what else is available in the model object
dolab_model %>% names()
# and the model summary
dolab_model %>% summary() %>% names()
# table of coefficients and hypothesis tests
dolab_model %>% summary() %>% coef()
# extract info from coefficients table
summary(dolab_model)$coefficients["b_read_perc", "t value"]

# plot of simple linear regression line
ggplot(dolab1) +
  aes(x = b_read_perc, y = pi_read_perc) +
  geom_point() +
  geom_smooth(method = "lm")

# standardised regression coefficients 
library(reghelper) 
dolab_model_std <- beta(dolab_model)
dolab_model_std

# using a linear model to predict new values
# input values
predict_input <- data.frame(b_read_perc = c(10,20,30,40) )
# predicted values
predict(dolab_model, predict_input)
# or to create it as a single data frame
predict_output <- 
  bind_cols(pi_read_perc_predict = predict(dolab_model, predict_input), 
            predict_input)

# producing regression tables
library(texreg)
knitreg(list(dolab_model) )

# confidence intervals
confint(dolab_model)
b_read_perc_ci <- paste(round(confint(dolab_model)["b_read_perc", "2.5 %"], 3), 
                   round(confint(dolab_model)["b_read_perc", "97.5 %"], 3), 
                   sep = "-")

# coefficient plot
plotreg(dolab_model, ci.inner = F) 

# diagnostic plots
plot(dolab_model)

# check for multicollinearity (if more than one explanatory variable)
library(car)
vif(dolab_model)


## Logistic regression ----------------------

# read data and package
library(tidyverse)
dolab1 <- read_csv("../data/dolab1.csv")

# create a binary outcome variable for illustration
dolab1 <- 
  dolab1 %>% 
  mutate(success = ifelse( (pi_read_perc - b_read_perc) >0, 
                          yes = 1, 
                          no = 0) )

# a logistic regression model of 'success' with one predictor variable
model1 <- glm(success ~ b_read_perc, 
             family = binomial(link = logit),   
             data = dolab1)

# a logistic regression model of 'success' with two predictor variables
model2 <- glm(success ~ b_read_perc + b_age, 
             family = binomial(link = logit),   
             data = dolab1)

# model summary
summary(model2)

# model coefficients
coef(model2)

# odds ratios
exp(coef(model2) )

# extract one of the odds ratios
exp(coef(model2) )[3]

# a logistic regression table of two models with odds ratios
library(texreg)
knitreg(list(model1,
             model2), 
        override.coef = list(exp(coef(model1) ), 
                             exp(coef(model2) ) ), 
        override.ci.low = list(exp(confint(model1)[ , "2.5 %"]),
                               exp(confint(model2)[ , "2.5 %"])), 
        override.ci.up = list(exp(confint(model1)[ , "97.5 %"]),
                              exp(confint(model2)[ , "97.5 %"])), 
        ci.test = 1)

# predicted probabilities
predict_input <- expand.grid(b_read_perc = seq(0,40,1), 
                             b_age = c(6,9) )
pred_prob <- predict(model2, predict_input, type = "response")
predict_output <- bind_cols(success_pred = pred_prob, 
                            predict_input)

# chart of predicted probabilities
ggplot(predict_output) +
  aes(x = b_read_perc, y = success_pred, colour = as.factor(b_age) ) +
  geom_line()

# table of coefficients and p values
model2 %>% summary() %>% coef()

# LRT for model coefficients
anova(model, test ="LRT")

# confidence interval for odds ratio
exp(confint(model))

# check for multicollinearity
library(car)
vif(model2)

# check for data points with high influence according to Cook's distance
IMs <- influence.measures(model2)
dolab1$id[IMs$is.inf[,"cook.d"]]

# a plot of odds ratios
plotreg(list(model1,
             model2), 
        override.coef = list(exp(coef(model1) ), 
                             exp(coef(model2) ) ), 
        override.ci.low = list(exp(confint(model1)[ , "2.5 %"]),
                               exp(confint(model2)[ , "2.5 %"])), 
        override.ci.up = list(exp(confint(model1)[ , "97.5 %"]),
                              exp(confint(model2)[ , "97.5 %"])), 
        ci.test = 1)  +
  geom_hline(yintercept = 1) +
  labs(y = "")


## Effect sizes ---------------------------

# read data and package
library(tidyverse)
dolab1 <- read_csv("../data/dolab1.csv")

# create factor variable for treatment assignment
dolab1 <- dolab1 %>% 
  mutate(treat_group_f = factor(treat_group, 
                                levels = c(1,0), 
                                labels = c("treatment", "control") ) )

library(effsize) # you may need to install this

# Cohen's d
cohen.d(read_chg ~ treat_group_f, data = dolab1)

# Hedges g
cohen.d(read_chg ~ treat_group_f, data = dolab1, 
        hedges.correction = T)

# Glass' delta
cohen.d(read_chg ~ treat_group_f, data = dolab1, 
        pooled = F)

# Cohen's d, accounting for baseline differences
library(esc) # you may need to install this
esc_mean_gain(gain1mean = trt_mean, 
              gain1sd = trt_sd, 
              grp1n = trt_n, 
              grp1r = trt_cor, 
              gain2mean = ctl_mean, 
              gain2sd = ctl_sd, 
              grp2n = ctl_n, 
              grp2r = ctl_cor, 
              es.type = "d") 


## Confidence interval for the mean ----------------------------

# numeric
t.test(dolab1$b_read_perc)$conf.int

# categorical proportion
library(MultinomialCI) 
multinomialCI(dolab1$gender %>% as.factor() %>% table(), 
              alpha = 0.05)


## Hypothesis tests --------------------------------

# one sample t test
t.test(dolab1$b_read_perc, mu = 40)

# two sample t test
t.test(b_read_perc ~ treat_group_f, data = dolab1)

# paired t test
t.test(dolab1$b_read_perc,
       dolab1$pi_read_perc, 
       paired = TRUE)

# extracting information from a hypothesis test
t.test(dolab1$b_read_perc, mu = 40)$statistic # the test statistic t
t.test(dolab1$b_read_perc, mu = 40)$parameter # degrees of freedom
t.test(dolab1$b_read_perc, mu = 40)$p.value # p value


# non-parametric tests
# Two samples: Wilcoxon-Mann-Whitney test
wilcox.test(b_read_perc ~ treat_group_f, data = dolab1)
# Paired samples:
wilcox.test(dolab1$pi_read_perc, 
            dolab1$b_read_perc,
            paired = TRUE)

# chi-squared test
chisq.test(dolab1$gender, dolab1$treat_group_f)


## Tables in R Markdown --------------------

library(pander)

# descriptive statistics for a sample
library(psych)
describe(dolab1 %>% select(height, weight, b_read_perc) ) %>% 
  select(n, mean, sd, median) %>% 
  # as.data.frame() %>% 
  pander(row.names = c("Height", "Weight", "Reading score"), 
         col.names = c("Sample size", "Mean", "Std dev", "Median"), 
         round = 1)

# comparing two groups
dolab1 %>% 
  group_by(treat_group_f) %>% 
  summarise(height_mean = mean(height, na.rm = T), 
            height_sd = sd(height, na.rm = T), 
            weight_mean = mean(weight, na.rm = T), 
            weight_sd = sd(weight, na.rm = T), 
            b_read_perc_mean = mean(b_read_perc), 
            b_read_perc_sd = sd(b_read_perc) ) %>% 
  select(-1) %>% t() %>% as.data.frame() %>% 
  pander(col.names = c(levels(dolab1$treat_group_f) ), 
         row.names = c("Mean height, m", 
                       "Std dev height, m", 
                       "Mean weight, kg", 
                       "Std dev weight, kg", 
                       "Mean baseline reading score", 
                       "Std dev baseline reading score"), 
         round = 1, 
         caption = "Neat table, flipped so that rows become columns")

# hypothesis tests
library(broom)

height_test <- 
  t.test(height ~ treat_group_f, data = dolab1) %>% 
  tidy() %>% 
  mutate(variable = "height") %>% 
  select(variable, method, statistic, parameter, p.value)

gender_test <- 
  chisq.test(dolab1$gender, 
             dolab1$treat_group) %>% 
  tidy() %>% 
  mutate(variable = "gender") %>% 
  select(variable, method, statistic, parameter, p.value)

bind_rows(height_test, gender_test) %>% 
  pander(caption = "hypothesis tests", 
         col.names = c("Variable", "Test type", 
                       "test statistic", "df", "p value"), 
         round = 2) 

