
#                Tavistock Edge of care analysis 
#                07/11/24



# NWD Definition 
# 1. Families where there are significant child protection concerns, where child protection plans are in place and during the early stages of court proceedings, and where social workers are having to make decisions on whether sufficient change is possible to allow the child to remain at home.

# https://www.researchinpractice.org.uk/children/publications/2018/december/edge-of-care-cost-calculator-change-project-report/

#Is currently the subject of a child protection plan
#Meets the criteria for statutory services as a child in need
#Has recently left care
#Has been recorded as ‘missing’ recently
#Is currently in a foster placement
#Is absent from school more than 10 per cent of the time.
#Domestic violence incident(s) have been reported in the family/carer home.
#There are known to be substance misuse issues in the family or for the young person themselves.

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
library(openxlsx)

load("Output/edge_cla.RData")
load("Output/edge_cpp.RData")
load("Output/edge_scl.RData")
load("Output/cla_merge_dr2_dr3.RData")



# Creating tables 
# TABLE FOR TAVISTOCK----
#What the table is: total children referred in LA by trial wedge, total children in care in LA by trial wedge, and out of those referred during the trial wedge and LA, how many of these will experience the outcome, as well as the cumulative sum for these numbers.
outcome_desc_for_tavistock = cla_merge %>% 
  dplyr::group_by(la, time_period) %>%
  dplyr::summarise(
    children_eligible_referred = n(), # total nb of children referred during wedge
    children_referred_with_previous_cpp_plans = sum(`previous cpp1` != "0"), 
    children_looked_after = sum( # total number of children looked after during wedge
      !is.na(`cla date`)),
    children_referred_who_became_looked_after_within_18_months= sum(`primary_outcome`),
    children_referred_with_previous_cpp_and_who_became_looked_after_during_the_trial = sum(
      `primary_outcome` == 1 & `previous cpp1` != "0")) %>% # total number experiencing the outcome out of the children referred during wedge
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


# Creating dataframe to allow more of the edge of care definitions to be included


edge_cla <- left_join(leftmerge_cla, leftmerge_cpp, by = c("child la id", "ref date", "previous cpp", "age at ref",
                                                            "ref trio", "ref_duo", "la")) %>%
  select(`child la id`, `ref date`, `previous cpp`, `age at ref`, `ref trio`, `ref_duo`, `la`,
         `Start date of CLA (Period of care start date)`, `CPP Start Date`, `CPP End Date`)



edge_cla <- left_join(edge_cla, leftmerge_scl, by = c("child la id", "ref date", "previous cpp", "age at ref",
                                                           "ref trio", "ref_duo", "la")) %>%
  select(`child la id`, `ref date`, `previous cpp`, `age at ref`, `ref trio`, ref_duo, `la`,
         `Start date of CLA (Period of care start date)`, `CPP Start Date`, `CPP End Date`, 
         `Summer term 2020`, `Autumn term 2020`, `Spring term 2021`,`Summer term 2021`,                                           
         `Autumn term 2021`, `Spring term 2022`, `Summer term 2022`)

# School abs is above 10% for any 1 term 
edge_cla <- edge_cla %>%
  mutate(scl_abs = as.integer(if_any(c(`Summer term 2020`, `Autumn term 2020`, `Spring term 2021`,`Summer term 2021`,                                           
                                       `Autumn term 2021`, `Spring term 2022`, `Summer term 2022`), ~ . >= 10)))

# School abs over 10% across terms 
edge_cla <- edge_cla %>%
  mutate(overall_abs = as.integer(rowMeans(select(., `Summer term 2020`, `Autumn term 2020`, `Spring term 2021`, 
                                                  `Summer term 2021`, `Autumn term 2021`, `Spring term 2022`, 
                                                  `Summer term 2022`) %>% 
                                             mutate(across(everything(), as.numeric)), 
                                           na.rm = TRUE) >= 10))


# Total number of observations (rows * columns)
total_observations <- nrow(edge_cla) * ncol(edge_cla)

# Missingness per column as a percentage
missing_percentage_per_column <- (colSums(is.na(edge_cla)) / nrow(edge_cla)) * 100
print(missing_percentage_per_column)

#Summer term 2020                              Autumn term 2020 
#91.92141884                                   78.94064286 
#Spring term 2021                              Summer term 2021                              Autumn term 2021 
#69.07867945                                   72.90101526                                   69.86364849 
#Spring term 2022                              Summer term 2022                             scl_abs 
#69.65722551                                   71.05584654                                   94.36479295 
#overall_abs 
#64.84209344 


# Prepping to merge the outcomes to the filtered data set
colnames(cla_merge)[4]  <- "ref date"
colnames(cla_merge)[6]  <- "previous cpp"
colnames(cla_merge)[8]  <- "age at ref"
colnames(cla_merge)[13]  <- "ref trio"
colnames(cla_merge)[14]  <- "ref_duo"

class(cla_merge$`age at ref`)
class(edge_cla$`age at ref`)

str(cla_merge$`age at ref`)

edge_cla$`age at ref` <- as.factor(edge_cla$`age at ref`)

# Check duplicates in edge_cla
cla_merge %>%
  count(`child la id`, `ref date`, `previous cpp`, `age at ref`, `ref trio`, `ref_duo`, `la`) %>%
  filter(n > 1)

edge_cla %>%
  count(`child la id`, `ref date`, `previous cpp`, `age at ref`, `ref trio`, `ref_duo`, `la`) %>%
  filter(n > 1)

# drop duplicates 
cla_merge <- cla_merge %>%
  distinct(`child la id`, `ref date`, `previous cpp`, `age at ref`, `ref trio`, `ref_duo`, `la`, .keep_all = TRUE)

edge_cla <- edge_cla %>%
  distinct(`child la id`, `ref date`, `previous cpp`, `age at ref`, `ref trio`, `ref_duo`, `la`, .keep_all = TRUE)

#Merging 
edge_cla_filter <- left_join(cla_merge, edge_cla, by = c("child la id", "ref date", "previous cpp", "age at ref",
                                                         "ref trio", "ref_duo", "la")) %>%
  select(`child la id`, `ref date`, `previous cpp`, `age at ref`, `ref trio`, ref_duo, `la`,
         `Start date of CLA (Period of care start date)`, `CPP Start Date`, `CPP End Date`, `scl_abs`, `overall_abs`)



# Creating a variable for whether the CYP started a CPP plan within the trial period 
edge_cla_filter <- edge_cla_filter %>%
  mutate(
    cpp_start = ifelse(is.na(`CPP Start Date`), 0, 1)
  )


# Creating a new table with ref duo, school abs, cpp plan 
summary_table_edge_of_care_Tavistock_additional_vars <- edge_cla_filter %>%
  group_by(la) %>%
  summarise(
    total_children_referred = n(), # Total number of children referred
    children_with_cpp_start = sum(!is.na(`CPP Start Date`)), # CPP start is not NA
    children_with_cla_start = sum(!is.na(`Start date of CLA (Period of care start date)`)), # CLA start is not NA
    children_with_both_cpp_and_cla = sum(!is.na(`CPP Start Date`) & !is.na(`Start date of CLA (Period of care start date)`)), # Both CPP and CLA are not NA
    children_with_ref_duo = sum(`ref_duo` == 1, na.rm = TRUE), # REF duo equals 1
    children_with_scl_abs = sum(`scl_abs` == 1, na.rm = TRUE), # SCL abs equals 1
    children_with_ref_duo_and_cla_start = sum(`ref_duo` == 1 & !is.na(`Start date of CLA (Period of care start date)`), na.rm = TRUE), # REF duo == 1 and CLA start is not NA
    children_with_scl_abs_and_cla_start = sum(`scl_abs` == 1 & !is.na(`Start date of CLA (Period of care start date)`), na.rm = TRUE), # SCL abs == 1 and CLA start is not NA
  ) %>%
  mutate(
      cumulative_children_referred = cumsum(total_children_referred),
      cumulative_children_with_cpp_start = cumsum(children_with_cpp_start),
      cumulative_children_with_cla_start = cumsum(children_with_cla_start),
      cumulative_children_with_both_cpp_and_cla = cumsum(children_with_both_cpp_and_cla),
      cumulative_children_with_ref_duo_and_cla_start = cumsum(children_with_ref_duo_and_cla_start),
      cumulative_children_with_scl_abs_and_cla_start = cumsum(children_with_scl_abs_and_cla_start)
    ) %>%
      ungroup() %>%
      mutate(across(
        .cols = where(is.numeric),
        .fns = ~ ifelse(.x < 10, '[z]', .x) # Suppress values less than 10
      ))


# Saving table
write.xlsx(outcome_desc_for_tavistock, "Output/summary_table_edge_of_care_Tavistick.xlsx")

write.xlsx(summary_table_edge_of_care_Tavistock_additional_vars, "Output/summary_table_edge_of_care_Tavistick_add_vars.xlsx")

