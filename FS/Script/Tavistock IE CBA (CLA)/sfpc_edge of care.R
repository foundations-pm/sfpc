
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

load("Output/edge_cla.RData")
load("Output/edge_cpp.RData")
load("Output/edge_scl.RData")
load("Output/cla_merge_dr2_dr3.RData")



edge_cla <- left_join(leftmerge_cla, leftmerge_cpp, by = c("child la id", "ref date", "previous cpp", "age at ref",
                                                            "ref trio", "la")) %>%
  select(`child la id`, `ref date`, `previous cpp`, `age at ref`, `ref trio`, `la`,
         `Start date of CLA (Period of care start date)`, `CPP Start Date`, `CPP End Date`)



edge_cla <- left_join(edge_cla, leftmerge_scl, by = c("child la id", "ref date", "previous cpp", "age at ref",
                                                           "ref trio", "la")) %>%
  select(`child la id`, `ref date`, `previous cpp`, `age at ref`, `ref trio`, `la`,
         `Start date of CLA (Period of care start date)`, `CPP Start Date`, `CPP End Date`, 
         `Summer term 2020`, `Autumn term 2020`, `Spring term 2021`,`Summer term 2021`,                                           
         `Autumn term 2021`, `Spring term 2022`, `Summer term 2022`)


edge_cla <- edge_cla %>%
  mutate(scl_abs = as.integer(if_any(c(`Summer term 2020`, `Autumn term 2020`, `Spring term 2021`,`Summer term 2021`,                                           
                                       `Autumn term 2021`, `Spring term 2022`, `Summer term 2022`), ~ . >= 10)))



# Substance use/domestic abuse 
# Finding the code for 'trio of vulnerabilities' (MH/DA/SU)
# ",Domestic violence - Parent/Carer" = 3B 
# ",Domestic violence - Child" = 3A
# ",Domestic violence - Other" = 3C
# ",Mental health concerns - Parent/Carer" = 4B
# ",Drug misuse - Parent/Carer" = 2B
# ",Alcohol misuse - Parent/Carer" = 1B

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
      `primary_outcome`),
    cumulative_number_of_children_referred_with_previous_cpp_and_who_became_looked_after_during_the_trial = cumsum(
      children_referred_with_previous_cpp_and_who_became_looked_after_during_the_trial)) %>%
  ungroup() %>%
  dplyr::mutate(across(
    .cols = where(is.numeric), 
    .fns = ~ ifelse(.x < 10, '[z]', .x))) # suppression checks
