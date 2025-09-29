######## running checks on merged data #######
library(tidyverse)
library(skimr)
library(psych)
library(pander)
library(finalfit)

skim(all_dr1_bind)

# DR1
#descriptives pre-merged data sets ------==
all_lancs_dr1 %>% 
  select(`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`) %>% 
  table() #just gives frequencies 
  #should also rename vars
summary(all_lancs_dr1)
  # gives min, quartiles, mean, median, max for each variable

#data types for each var ====
#dates 

#missing data ====
is.na(all_lancs_dr1$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`) %>% 
  sum() # number
is.na(all_lancs_dr1$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`) %>% 
  mean() # proportion - 76% of values are missing


#descriptive stats ====

#create a var for each DR1, each LA

# #overall check 
# ggplot(all_dr1_bind) + # first the dataset
#   aes(x = 'New referrals this month') + # then the variables to be visualised
#   geom_histogram()
# 
# #run by DR1
# all_dr1_bind %>% 
#   group_by(LA) %>% #replace this w DR1 categories, var not yet made
#   summarise(newCLA_mean = mean('New CLA this month', na.rm=T), 
#             newCLA_sd = sd('New CLA this month', na.rm = T), 
#             newCLA_median = median('New CLA this month', na.rm = T), 
#             newCLA_IQR = IQR('New CLA this month', na.rm = T), 
#             newCLA_missing = is.na('New CLA this month') %>% mean()) %>% 
#   ungroup()
# 
# #run by LA
# all_lancs_dr1 %>% 
#   group_by(LA) %>% #replace this w DR1 categories
#   summarise(weight_mean = mean(weight, na.rm=T), 
#             weight_sd = sd(weight, na.rm = T), 
#             weight_median = median(weight, na.rm = T), 
#             weight_IQR = IQR(weight, na.rm = T) ) %>% 
#   ungroup()


#descriptives post-merged data sets ------


# investigate CiN categories ---------------

# the variables are as follows - renaming 

#CIN started 
  #=== of CIN plans that started this month in the LA 
  #Number of CIN plans that started this month in the LA (exc CPP)


#open ===CIN this month
  #Number of open CIN cases this month in the LA 
  #Number of open CIN cases this month in the LA (all) 
  #Number of open CIN cases this month in the LA (narrow definition provided June 2021)
  #Number of open CIN cases this month in the LA (wider definition)
  #Number of open CIN cases this month in the LA (narrow definition)

#rename 
DR1_FS <- all_dr1_bind %>% 
  rename("CiN_started" = 'cin starts',
         "CiN_started_excl_CPP" = 'Number of CIN plans that started this month in the LA (exc CPP)', 
         "CiN_open"= 'cin open',
         "CiN_open_longterm" = 'Number of open CIN cases this month in the LA (long term)',
         "CiN_open_all" = 'Number of open CIN cases this month in the LA (all)',
         "CiN_open_narrowJun21" = 'Number of open CIN cases this month in the LA (narrow definition provided June 2021)',
         "CiN_open_narrow" = 'Number of open CIN cases this month in the LA (narrow definition)',
         "CiN_open_wide" = 'Number of open CIN cases this month in the LA (wider definition)'
         )

# CiN started
#check to see cross tabs
describe(DR1_FS)

DR1_FS %>% 
  select(LA, CiN_started) %>% 
  table(useNA = "ifany")
write.csv(DR1_FS, "Data/FS_DR1/DR1_FS.csv")

dependent <- c("CiN_started", "CiN_started_excl_CPP", "CiN_open", "CiN_open_longterm", "CiN_open_all",
               "CiN_open_narrowJun21", "CiN_open_narrow","CiN_open_wide"  )
explanatory <- c("month", "la")

DR1_FS %>% 
  missing_plot(dependent, explanatory)
  #CiN_started
    # looks like one LA has CiN_started but NOT CiN_started_excl_CPP
    # one LA has BOTH CiN_started AND CiN_started_excl_CPP
  #CiN_open
    # 2 LAs missing CiN_open_wide
    # 1 LA missing CiN_open_all
    # 1 LA missing CiN_open_narrowJun21
    # .5 LA missing CiN_open_narrow

# figure out which LAs have the missingness

# first var to look at: CiN_open 
#trying to call back where data is missing, particularly which months in which LA
DR1_FS %>% 
  group_by(Month, LA) %>% 
  select(CiN_open_all) %>% 
  filter(CiN_open_all != "NA") %>% 
  table() 
  

# only looking at cases where CiN_open is missing 
# CiN_open_missing <- DR1_FS %>%
#   group_by(LA, Month) %>% # looking to see which months within LAs is missing 
#   select(LA, Month, CiN_open) %>% #the variables we do need to look at
#   #filter(CiN_open == "NA") %>%  #only looking at missing values
#  # is.na() %>% 
#   summarise(count = sum(is.na(CiN_open))) %>% 
#  # print(n = 175) %>% 
#   
# #returns all LA/Months where CiN_open is missing   
# CiN_open_NA <- CiN_open_missing %>%   
#   filter(count ==1) %>% 
#   rename("CiN_open_NA" = "count") %>% 
#   print(n = 37)
 
# try combining the 2 into one chunk of code 

#CiN_open_NA
CiN_open_NA <- DR1_FS %>%
  group_by(la, month) %>%
  select(la, month, CiN_open) %>%
  summarise(count = sum(is.na(CiN_open))) %>% 
  filter(count ==1) %>% 
  rename("CiN_open_NA" = "count") %>% 
  print(n = 41)

CiN_open_NA %>% 
  print(n = 41)
#CiN_open_longterm
CiN_open_longterm_NA <- DR1_FS %>%
  group_by(la, month) %>%
  select(la, month, CiN_open_longterm) %>%
  summarise(count = sum(is.na(CiN_open_longterm))) %>% 
  filter(count == 0) %>% 
  rename("CiN_open_longterm_NA" = "count") 

CiN_open_longterm_NA

#CiN_open_all
CiN_open_all_NA <- DR1_FS %>%
  group_by(la, month) %>%
  select(la, month, CiN_open_all) %>%
  summarise(count = sum(is.na(CiN_open_all))) %>% 
  filter(count == 0) %>% 
  rename("CiN_open_all_NA" = "count")
CiN_open_all_NA

#CiN_open_wide
CiN_open_wide_NA <- DR1_FS %>%
  group_by(la, month) %>%
  select(la, month, CiN_open_wide) %>%
  summarise(count = sum(is.na(CiN_open_wide))) %>% 
  filter(count == 0) %>% 
  rename("CiN_open_wide_NA" = "count")

CiN_open_wide_NA %>% 
  print(n=24)
  
#CiN_open_narrow
CiN_open_narrow_NA <- DR1_FS %>%
  group_by(la, month) %>%
  select(la, month, CiN_open_narrow) %>%
  summarise(count = sum(is.na(CiN_open_narrow))) %>% 
  filter(count == 0) %>% 
  rename("CiN_open_narrow_NA" = "count")
CiN_open_narrow_NA

#CiN_open_narrow
CiN_open_narrowJun21_NA <- DR1_FS %>%
  group_by(la, month) %>%
  select(la, month, CiN_open_narrowJun21) %>%
  summarise(count = sum(is.na(CiN_open_narrowJun21))) %>% 
  filter(count == 0) %>% 
  rename("CiN_open_narrowJun21_NA" = "count")
CiN_open_narrowJun21_NA

  
# CiN_started 
CiN_started_NA <- DR1_FS %>%
  group_by(la, month) %>%
  select(la, month, CiN_started) %>%
  summarise(count = sum(is.na(CiN_started))) %>% 
  filter(count ==1) %>% 
  rename("CiN_started_NA" = "count")  

CiN_started_NA 

#CiN_started_excl_CPP
CiN_started_excl_CPP_NA <- DR1_FS %>%
  group_by(la, month) %>%
  select(la, month, CiN_started_excl_CPP) %>%
  summarise(count = sum(is.na(CiN_started_excl_CPP))) %>% 
  filter(count ==0) %>% 
  rename("CiN_started_excl_CPP_NA" = "count")  

CiN_started_excl_CPP_NA 



# now start looking at differences ----

#https://foundationsww.sharepoint.com/:f:/r/Projects/CSC_Projects/2.%20Projects/2.8.%20SFPC%20(Strengthening%20Families,%20Protecting%20Children)/Trials/2_Impact%20Evaluation/Data%20Cleaning?csf=1&web=1&e=C0XwjL

# CiN_started_NA ======
CiN_started_NA
  #Swindon 2021-04 to 2021-09
  #Wandsworth 2019-10 to 2020-09
CiN_started_excl_CPP_NA
  #Wandsworth 2019-10 to 2020-09
  # why is swindon missing?

#impute CiN_started_excl_CPP into CiN_started 


#notes https://foundationsww.sharepoint.com/:x:/r/Projects/CSC_Projects/2.%20Projects/2.8.%20SFPC%20(Strengthening%20Families,%20Protecting%20Children)/Trials/2_Impact%20Evaluation/Data%20Request%20Impact%20Evaluation/Data%20requests_issues_log.xlsx?d=w2ebebe7662ca483896a6c176ae038a16&csf=1&web=1&e=VP4Neo
# Wandsworth noted that they are not able to provide the wider definition prior to Apr 2020


#CiN_open ======
CiN_open_NA %>% 
  print(n = 37)
  #Swindon 2019-10 to 2020-10
  #Walsall 2019-10 to 2020-09
  #Wandsworth 2019-10 to 2020-09

CiN_open_longterm_NA
  #Swindon 2019-10 to 2020-09

CiN_open_all_NA
  #Swindon 2019-10 to 2020-09

CiN_open_narrow_NA
  #Wandsworth 2020-04 to 2020-09

CiN_open_narrowJun21_NA
  #Walsall 2019-10 to 2020-09

CiN_open_wide_NA %>% 
  print(n = 24)
  #Walsall 2019-10 to 2020-09
  #Wandsworth 2019-10 to 2020-09

#comparing diff variables via histograms 
ggplot(DR1_FS) + # first the dataset
  aes(x = CiN_started) + # then the variables to be visualised
  geom_histogram() # then the geometric object to be used


#look at missingness for each variable 
pacman::p_load(
  rio,           # import/export
  tidyverse,     # data mgmt and viz
  naniar,        # assess and visualize missingness
  mice           # missing data imputation
)
pct_miss(DR1_FS$CiN_started)


# email communications about CiN started peculiarities



# email communications about CiN open peculiartiies

# explanation on final decision

#final data set 












