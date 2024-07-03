
# SFPC impact evaluation (NWD)
    # Data Merging DR3 outcomes onto DR2
    # Author: AA and DR
    # Date: 05/09/2023


##########
# NOTES #
########

# details the step by step process of merging DR3 onto DR2
# only complete when DR2 is cleaned and binded together
# only complete when each DR3 tab is cleaned and binded to each other
  # DR3 should NOT be binded into a master dataset
  # I recommend binding DR3 together by tabs (aka outcomes)
  # THEN individually combine each DR3 tab(outcome) to DR2, one at a time,
  # using the instructions below 

rm(list = ls())
# libraries -----------

library(tidyverse)
library(dplyr)
library(readxl)
library(tibble)
library(lubridate)
library(data.table)
library(ggplot2)
#install.packages("ggplot2")
library(arsenal)
#install.packages("arsenal")
library(pacman)
#install.packages("pacman")
library(naniar)
#install.packages("naniar")

# import datasets ---------

# DR2 fully binded dataset===========

load ("Output/alldr2.RData")


#merging instructions ----------


# pre-merging work ====

#create unique identifier variables #######
#to be applied to EACH imported data set
#for EACH FILE TO BE MERGED W INDIVIDUAL CHILD DATA
 #sort Child ID, case ID, date for each var 

#check to see that there are no duplicates of childcaseID
  # run command to see uniqueness
  
  # Show duplicate rows
  
  sum(duplicated(all_dr2))
  duplicated(all_dr2)
  
  # 175 duplicated rows 
  175/56110*100   # .31% are duplicated 
  
  duplicates <- duplicated(all_dr2) | duplicated(all_dr2, fromLast = TRUE)
  
  duplicate_rows <- all_dr2[duplicates, ]
  print(duplicate_rows)
  
  # Drop duplicate rows
  all_dr2 <- all_dr2[!duplicates, ]
  
  # Add a variable to DR2 referral dates which allows merging with DR1 ----
  all_dr2$`Month` <- as.Date(format(all_dr2$`Referral Date`, "%Y-%m-01"))

  # DR3 individually binded datasets ======
  
  #outcome 1
  
  load ("Output/alldr3careep.RData")
  
  #outcome 2 
  
  load ("Output/alldr3cla.RData")
  
  #outcome 3
  
  load ("Output/alldr3cin.RData")
  
  #outcome 4
  
  load ("Output/alldr3neet.RData")
  
  
  #check to see that there are no duplicates of childcaseID
  # run command to see uniqueness
  
  # Show duplicate rows
  
  sum(duplicated(all_careep_dr3)) # 0 Duplicates
  sum(duplicated(all_cin_dr3)) # 0 Duplicates
  sum(duplicated(all_cla_dr3)) # 0 Duplicates
  sum(duplicated(all_neet_dr3)) # 0 Duplicates
  duplicated(all_careep_dr3)
  

#run descriptive statistics on DR3 dataset pre-merge ######

# run descriptives on missingness
  is.na(all_careep_dr3$`Days spent in care`) %>% sum() # number = 2086
  is.na(all_careep_dr3$`Days spent in care`) %>% mean() # proportion = 39%
  

#run descriptive statistics on DR2 dataset pre-merge ######

# run descriptives on missingness
  is.na(all_dr2$`Age at Referral`) %>% sum() # number = 11729
  is.na(all_dr2$`Age at Referral`) %>% mean() # proportion = 21%

###############################################################################
  
# merging ========
# great resource here: https://epirhandbook.com/en/joining-data.html

# merge ONE data set (aka individual DR3 tab) to a fully binded/cleaned DR2 ####
  #merge by unique identifier
  
###############################################################################  
  
  # FULL JOIN ----
  # DECIDED TO USE LEF TJOIN RATHER THAN FULL JOIN !!
  # Merging DR2 to DR3 file for CLA ----
#  merged_cla <- full_join(all_dr2, all_cla_dr3, by = c("Child ID"))
  
  # Detected an unexpected many-to-many relationship between `x` and `y`.
  #ℹ Row 967 of `x` matches multiple rows in `y`.
  #ℹ Row 32 of `y` matches multiple rows in `x`.
  #ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.
  
  # Merging DR2 to DR3 file for NEET ----
#  merged_neet <- full_join(all_dr2, all_neet_dr3, by = c("Child ID"))
  
  #Detected an unexpected many-to-many relationship between `x` and `y`.
  #ℹ Row 443 of `x` matches multiple rows in `y`.
  #ℹ Row 103 of `y` matches multiple rows in `x`.
  #ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.
  
  # Merging DR2 to DR3 file for Care Experience ----
#  merged_careep <- full_join(all_dr2, all_careep_dr3, by = c("Child ID"))
  
  #Detected an unexpected many-to-many relationship between `x` and `y`.
  #ℹ Row 425 of `x` matches multiple rows in `y`.
  #ℹ Row 1685 of `y` matches multiple rows in `x`.
  #ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.
  
  # Merging DR2 to DR3 file for CIN ----
#  merged_cin <- full_join(all_dr2, all_cin_dr3, by = c("Child ID"))
  
  #Detected an unexpected many-to-many relationship between `x` and `y`.
  #ℹ Row 439 of `x` matches multiple rows in `y`.
  #ℹ Row 129 of `y` matches multiple rows in `x`.
  #ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.
  
  #full join - most inclusive, returns all rows from both dataframes - we prefer this

  ################################################################################
  # LEFT JOIN ----
  # Trying with LEFT JOIN
  #joined_data <- left_join(df1, df2, by = "ID")
  
  #A LEFT OUTER JOIN will return all records from the LEFT table joined with the RIGHT table where possible.
  #If there are matches, though, it will still return all rows that match. Therefore, one row in the LEFT table that matches two rows in the RIGHT table will return as two rows, just like an INNER JOIN.
  
  #  If the key column in both the left and right array contains duplicates, then the result is a many-to-many merge.
  
  # Merging DR2 to DR3 file for CLA ----
  leftmerge_cla <- left_join(all_dr2, all_cla_dr3, by = c("Child ID"))
  
  #Warning message:
  #In left_join(all_dr2, all_cla_dr3, by = c("Child ID")) :
  #Detected an unexpected many-to-many relationship between `x` and `y`.
  #ℹ Row 967 of `x` matches multiple rows in `y`.
  #ℹ Row 32 of `y` matches multiple rows in `x`.
  #ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.
  
  # Merging DR2 to DR3 file for NEET ----
  leftmerge_neet <- left_join(all_dr2, all_neet_dr3, by = c("Child ID"))
  
  #Warning message:
  #In left_join(all_dr2, all_neet_dr3, by = c("Child ID")) :
  #Detected an unexpected many-to-many relationship between `x` and `y`.
  #ℹ Row 443 of `x` matches multiple rows in `y`.
  #ℹ Row 103 of `y` matches multiple rows in `x`.
  #ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.
  
  # Merging DR2 to DR3 file for Care Experience ----
  leftmerge_careep <- left_join(all_dr2, all_careep_dr3, by = c("Child ID"))
  
  #Warning message:
  #In left_join(all_dr2, all_careep_dr3, by = c("Child ID")) :
  #Detected an unexpected many-to-many relationship between `x` and `y`.
  #ℹ Row 425 of `x` matches multiple rows in `y`.
  #ℹ Row 1685 of `y` matches multiple rows in `x`.
  #ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.
  
  # Merging DR2 to DR3 file for CIN ----
  leftmerge_cin <- left_join(all_dr2, all_cin_dr3, by = c("Child ID"))
  
  #Warning message:
  #In left_join(all_dr2, all_cin_dr3, by = c("Child ID")) :
  #Detected an unexpected many-to-many relationship between `x` and `y`.
  #ℹ Row 439 of `x` matches multiple rows in `y`.
  #ℹ Row 129 of `y` matches multiple rows in `x`.
  #ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.

  ################################################################################
  # MERGE CHECKS -----
  # DR2 as benchmark 
  dim(all_dr2) # rows: 55832    cols: 17
  
  # Check the Dimensions----
  # CLA
  dim(leftmerge_cla) # rows: 55870    cols: 20
  55870-55832 #38 (this means there are 38 cases where a child with one referral entry has more than 1 outcome entry)
  
  # NEET
  dim(leftmerge_neet) # rows: 55921    cols: 24
  55921-55832 #89 (this means there are 89 cases where a child with one referral entry has more than 1 outcome entry)
  
  # CAREEP
  dim(leftmerge_careep) # row: 64489    cols: 28
  64489-55832 # 8657 (this means there are 8657 cases where a child with one referral entry has more than one outcome entry)
  
  # CIN
  dim(leftmerge_cin) # row: 72551    cols: 21
  72551-55832 # 16719 (this means there are 16719 cases where a child with one referral entry has more than one outcome entry)
  
 
  # Check for Missing Values -----
  # Good resource: https://epirhandbook.com/en/missing-data.html#nan
  
  # CLA - child in care start date outcome ----
  #Percent of rows with any value missing
  pct_miss_case(leftmerge_cla)  #100
  
  # Visual to see missingness
  gg_miss_var(leftmerge_cla, show_pct = TRUE)
  
  # Visual to see missingness by LA
  leftmerge_cla %>% 
    gg_miss_var(show_pct = TRUE, facet = `LA.x`)
  ggsave("Output/CLA_missing.png", width = 8, height = 6, units = "in")

  # How much outcome data is there for the outcomes:
  # CLA 
  # Leicester: 
  missing_summarycla <- leftmerge_cla %>%
    group_by(LA.x) %>%
    summarize(percentage_available = mean(!is.na(`Date period of care commenced.x`)) * 100)
  
  # % of children in DR2 who have outcome data for CLA start date 
  #	Leicester
  # 4.8
  
  # Norfolk
  # 4.7
  
  # Redcar
  # 3.3
  
  # Rochdale
  # 2.9
  
  # Warrington
  # 5.0
  
  
  # Missing by outcome Careep - days spent in care
  missing_summarycareep <- leftmerge_careep %>%
    group_by(LA.x) %>%
    summarize(percentage_missing_Care_Start = mean(!is.na(`Date period of care commenced.x`)) * 100,
              percentage_missing_Care_End = mean(!is.na(`Date period of care ended`)) * 100)
  
  colnames(all_careep_dr3)
  
  print(missing_summarycareep)
  
  # % of children in DR2 who have outcome data for Care start and end date
  # LA.x                          % Care_Start                 % Care_End
  #1 Leicester                        4.8                       0.05 
  #2 Norfolk                          11.9                      22.9
  #3 Redcar                           6.5                      14.3 
  #4 Rochdale                         7.5                       6.6 
  #5 Warrington                       13.3                      14.8
  
  # Missing by outcome NEET
  
  missing_summaryneet <- leftmerge_neet %>%
    group_by(LA.x) %>%
    summarize(percentage_missing_2019 = mean(!is.na("Main activity -\r\nProcessing year 2019")) * 100,
              percentage_missing_2020 = mean(!is.na("Main activity -\r\nProcessing year 2020")) * 100,
              percentage_missing_2021 = mean(!is.na("Main activity - \r\nProcessing year 2021")) * 100,
              percentage_missing_2022 = mean(!is.na("Main activity -\r\nProcessing year 2022")) * 100,
              percentage_missing_2023 = mean(!is.na("Main activity -\r\nProcessing year 2023")) * 100)
              
  colnames(leftmerge_neet)
  
  # % of children in DR2 who have outcome data for Care start and end date # This doesn't look accurate
  # LA.x                          % Care_Start                 % Care_End
  #1 Leicester                        100                      100 
  #2 Norfolk                          100                      100
  #3 Redcar                           100                      100 
  #4 Rochdale                         100                      100
  #5 Warrington                       100                      100
  
  # Missing by outcome Cin
  
  missing_summarycin <- leftmerge_cin %>%
    group_by(LA.x) %>%
    summarize(percentage_missing = mean(!is.na(`CIN Closure Date`)) * 100)
  
  
  # % of children in DR2 who have outcome data for CLA start date 
  #	Leicester
  # 0.4
  
  # Norfolk
  # 49.9
  
  # Redcar
  # 64.2
  
  # Rochdale
  # 0
  
  # Warrington
  # 85.6
  
  # Check for Duplicates ----
  # CLA
  any(duplicated(leftmerge_cla)) #FALSE
  
  # Careep
  any(duplicated(leftmerge_careep)) # FALSE
  
  # NEET
  any(duplicated(leftmerge_neet)) # FALSE
  
  # NEET
  any(duplicated(leftmerge_cin)) # FALSE
  
  
  ################################################################################
  # Prep for merge with DR1 ----
  # CLA 
  # Drop LA y, referral ID y and date period of care commenced y
  leftmerge_cla <- leftmerge_cla %>%
    select(-LA.y)
  
  leftmerge_cla <- leftmerge_cla %>%
    select(-`Date period of care commenced.y`)
  
  leftmerge_cla <- leftmerge_cla %>%
    select(-`Referral ID (or Case ID).y`)
  
  # Rename LA, DPCC.x and referral ID
  
  names(leftmerge_cla)[names(leftmerge_cla) == "LA.x"] <- "LA"
  
  names(leftmerge_cla)[names(leftmerge_cla) == "Date period of care commenced.x"] <- "Date period of care commenced"
  
  names(leftmerge_cla)[names(leftmerge_cla) == "Referral ID (or Case ID).x"] <- "Referral ID (or Case ID)"

  # Careep
  # Drop LA y 
  leftmerge_careep <- leftmerge_careep %>%
    select(-LA.y)
  
  leftmerge_careep <- leftmerge_careep %>%
    select(-`Referral ID (or Case ID).y`)
  
  # Rename LA (lowercase in DR1 - match)
  leftmerge_careep <- leftmerge_careep %>%
    rename(LA = LA.x)
  
  leftmerge_careep <- leftmerge_careep %>%
    rename(`Date period of care commenced` = `Date period of care commenced.x`)
  
  leftmerge_careep <- leftmerge_careep %>%
    rename(`Referral ID (or Case ID)` = `Referral ID (or Case ID).x`)
  
  # Drop period of care commenced y
  
  leftmerge_careep <- leftmerge_careep %>%
    select(-`Date period of care commenced.y`)
  
  # Neet
  # Drop LA y 
  leftmerge_neet <- leftmerge_neet %>%
    select(-LA.y)

  # Rename LA (lowercase in DR1 - match)
  leftmerge_neet <- leftmerge_neet %>%
    rename(LA = LA.x)
  
  # CIN
  # Drop LA y 
  leftmerge_cin <- leftmerge_cin %>%
    select(-LA.y)
  
  # Rename LA (lowercase in DR1 - match)
  leftmerge_cin <- leftmerge_cin %>%
    rename(LA = LA.x)
  
    # DR1 fully binded dataset===========
  
  load ("Output/alldr1.RData")
  
  
  # MERGE DR1 onto the combined DR2 & DR3 ----
  # CLA
  test_merge_cla <- merge(leftmerge_cla, all_dr1, by = c("LA", "Month"), all.x = TRUE)

  # Careep
  test_merge_careep <- merge(leftmerge_careep, all_dr1, by = c("LA", "Month"), all.x = TRUE)
  
  # NEET
  test_merge_neet <- merge(leftmerge_neet, all_dr1, by = c("LA", "Month"), all.x = TRUE)

  # CIN
  test_merge_cin <- merge(leftmerge_cin, all_dr1, by = c("LA", "Month"), all.x = TRUE)

###############################################################################
  
  # Save the datasets
  
  save(test_merge_cla, file = "Output/merged_cla.RData")
  
  save(test_merge_neet, file = "Output/merged_neet.RData")
  
  save(test_merge_careep, file = "Output/merged_careep.RData")
  
  save(test_merge_cin, file = "Output/merged_cin.RData")

 # Missing data checks

  #CLA
  
  #Percent of rows with any value missing
  pct_miss_case(test_merge_cla)  #100
  
  # Visual to see missingness
  gg_miss_var(test_merge_cla, show_pct = TRUE)
  
  # Visual to see missingness by LA
  test_merge_cla %>% 
    gg_miss_var(show_pct = TRUE, facet = `LA`)
  ggsave("Output/FinalCLA_missing.png", width = 8, height = 6, units = "in")
  
  #NEET
  
  #Percent of rows with any value missing
  pct_miss_case(test_merge_neet)  #100
  
  # Visual to see missingness
  gg_miss_var(test_merge_neet, show_pct = TRUE)
  
  # Visual to see missingness by LA
  test_merge_neet %>% 
    gg_miss_var(show_pct = TRUE, facet = `LA`)
  ggsave("Output/FinalNEET_missing.png", width = 8, height = 6, units = "in")
  
  #CIN
  
  #Percent of rows with any value missing
  pct_miss_case(test_merge_cin)  #100
  
  # Visual to see missingness
  gg_miss_var(test_merge_cin, show_pct = TRUE)
  
  # Visual to see missingness by LA
  test_merge_cin %>% 
    gg_miss_var(show_pct = TRUE, facet = `LA`)
  ggsave("Output/FinalCIN_missing.png", width = 8, height = 6, units = "in")
  
  #CARE EXP 
  
  #Percent of rows with any value missing
  pct_miss_case(test_merge_careep)  #100
  
  # Visual to see missingness
  gg_miss_var(test_merge_careep, show_pct = TRUE)
  
  # Visual to see missingness by LA
  test_merge_careep %>% 
    gg_miss_var(show_pct = TRUE, facet = `LA`)
  ggsave("Output/FinalCAREEXP_missing.png", width = 8, height = 6, units = "in")
  
  