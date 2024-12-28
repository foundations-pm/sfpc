# Data Pre-Processing Script for No Wrong Doors RCT DR1 ----

# Set-up  ----
user_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(user_directory,'Foundations/High-SFPC-Impact - ')

# Data and output paths
data_path = paste0(sharepoint_path, 'Data/')
output_path = paste0(sharepoint_path, 'QA/')

# Working directory
wd = paste0(user_directory, "Documents/sfpc/nwd/")

# Libraries 
{ source(paste0(wd, "config.R")) }

# Functions 
{ source(paste0(wd, "functions.R"))}

# Load data ----

# Set working dir to data path
setwd(data_path)

# Load data:

# Create list of file names
DR1_file_paths <- list.files("NWD_DR1", # Name of folder with DR1 data
                        pattern="*.xlsx", full.names=TRUE)

# Pre-processing ####

### ISSUE 1
# ISSUE 1: Rochdale Nov 2020 format is too different to treat with others
# ISSUE 1 mitigation:
# Remove incomplete files
# DR1_file_paths = DR1_file_paths[-16] # Rochdale Nov 2020
# And treat files separately in processing

# Read files into local env
DR1_list <- lapply(DR1_file_paths, function(file) {  
  print(file) 
  readxl::read_excel(file, sheet = "DR1 - Data - aggregate level")})

# Clean list names
string_to_remove = c("NWD_DR1/", ".xlsx")
DR1_file_paths <- stringr::str_remove_all(DR1_file_paths, 
                                          paste(string_to_remove,
                                                collapse = "|"))
names(DR1_list) = DR1_file_paths

# Create LA string
#la_string = str_extract(DR1_names, ".+?(?=_)")

## Quality checks 1 ----
# Key steps:
# 1- Datasets dimensions
# 2- Column names 
# 3- Missing data 
# 4- Unique values / unexpected values (e.g., int vs cat)

lapply(1:length(DR1_list), function(i){ 
  print(names(DR1_list[i]))
  print(dim(DR1_list[[i]])) 
  print(sum(is.na(DR1_list[[i]])))})

# Remove last row in Rochdale Nov 20 return: mistake in completion
nrow_rochdale_nov20 = nrow(DR1_list[["rochdale_dr1_nov20"]])
DR1_list[["rochdale_dr1_nov20"]] <- DR1_list[[
  "rochdale_dr1_nov20"]][1:(nrow_rochdale_nov20-1),]

## Data prep ----

## Workplan 

# STEP 1: merge records

# 1.1 - Keep only mandatory return & assign new names

# 1 Month
# 2 Nb completed assessment by CSC = assessment_completed_agg (int)
# 3 CLA rate per 10,000 children = cla_agg_rate_per_10000 (int)
# 4 Nb of children looked after at the end of the month in LA = cla_agg (int)
# 5 Nb of newly looked after children at the end of month = cla_start_agg (int)
# 6 Number of CIN plans started = cin_start_agg (int)
# 7 Number of open CIN cases = cin_agg (int)
# 8 Number of CPP plans started = cpp_start_agg (int)
# 9 Number of CPP plans = cpp_agg (int)
# 10 Number of new referrals = new_referrals_agg (int)
# 11 Proportion of CYP eligible and claiming FSM, out of all pupils 
# = fsm_agg_prop (int)

# 1.2 - Create a column to identify each dataset by LA and month of return

# 1.3 - Merge all records 

# STEP 2: checks

# 2.1 - Data cleaning checks
# Dates of returns: from Oct 2019 to ...
# Value inconsistencies (class & range/categories)
# Total number missing 
# Location missing data 

### Merge records ----
# Create merge df:
# 1 - Keep mandatory returns: columns of interest only
# 2 - Create a column to identify each dataset by LA and month of return
# 3 - Merge all records 

# create clean names for mandatory returns
mandatory_fields <- janitor::make_clean_names(colnames(
  DR1_list[['leicester_dr1_apr21']])[-c(2,3)]) 
# remove proportion of FSM eligible, as not part of mandatory returns
# remove number of assessment completed, also not part of mandatory return

# Create merged table
DR1_data <- purrr::map_dfr(1:length(DR1_list), function(i) {
  
  # Identify LA return by its name 
  # format = <LA name> _ < month/year of return>
  name = names(DR1_list)[i] 
  
  # Merge all records into one table:
  
  DR1_list[[i]] <- DR1_list[[i]] %>% 
    janitor::clean_names() %>% # Clean column names 
    dplyr::mutate(local_authority = name) %>% # New column with LA identifier
    dplyr::select(local_authority, # select only mandatory returns
                  any_of(mandatory_fields)) %>%
    dplyr::mutate(across(everything(), as.character))
  # temporarily convert all columns as character to bind them together

  # check dimension of data
  print(names(DR1_list[i])) 
  print(dim(DR1_list[[i]])) 
  
  # Quality checks 
  # Checks: there should be 12 columns
  # 11 mandatory returns + 1 column for LA return name 
  if(dim(DR1_list[[i]])[[2]] != 10){ 
    print("ISSUE WITH DATA TO CHECK") }
  
  return(DR1_list[[i]])
  
})

### Quality checks 2 ----

# Data cleaning checks 

# Check completedness of returns: 

# 6 months records from Oct 2019:
# Supposedly, 42 months until end of month starting 01 March 2023

# 2019-2020: 1 return (Nov20) > 
# should be 12 months, from 01/10/2019 to 01/09/2020

# 2020-2021: 2 returns: Apr21, Nov21 >
# Apr21 should be 01/10/2020 to 01/03/2021, 
# Nov21 should be 01/04/2021 to 01/09/2021

# 2021-2022: 2 returns: Apr22, Nov22 >
# Apr22 should be 01/10/2021 to 01/03/2022,
# Nov22 should be 01/04/2022 to 01/09/2022

# 2022-2023: 1 return: Apr23 >
# Apr23 should be 01/10/2022 to 01/03/2023

# Assign date class & arrange column conveniently
DR1_data <- DR1_data %>% 
  dplyr::mutate(month = as.Date(month, "%Y-%m-%d"),
                month_return = gsub(".*\\_", "", local_authority),
                month_return = factor(
                  month_return,
                  levels = c("nov20", "apr21", "nov21", "apr22",
                             "nov22", "apr23", "nov23", "apr24")),
                local_authority = str_extract(local_authority, ".+?(?=_)")) %>%
  dplyr::relocate(local_authority, month_return)

# Return date checks 
length(unique(DR1_data$month))
DR1_data %>% group_by(local_authority) %>% distinct(month) %>% summarise(n())

returns_date_checks <- DR1_data %>% 
  group_by(local_authority, month_return) %>% 
  summarise(total_nb_months = n(), # total number of months returned
            min_month = min(month), # min month
            max_month = max(month)) %>% # max month
  as.data.frame()  %>%
  dplyr::arrange(local_authority) 

# Check total number of months returned
returns_date_checks %>%
  group_by(local_authority) %>%
  summarise(sum(total_nb_months))

# Set working dir to data path
setwd(paste0(output_path,"/pre_processing/"))

# Save table
writexl::write_xlsx(returns_date_checks, 
                    "DR1_raw_monthly_returns_quality_checks.xlsx")

# Clean return dates 
month_range = list('nov20' = seq(as.Date('2019-10-01'),
                                 by = "month", length.out = 12),
                   'apr21' = seq(as.Date('2020-10-01'),
                                 by = "month", length.out = 6),
                   'nov21' = seq(as.Date('2021-04-01'),
                                 by = "month", length.out = 6),
                   'apr22' = seq(as.Date('2021-10-01'),
                                 by = "month", length.out = 6),
                   'nov22' = seq(as.Date('2022-04-01'),
                                 by = "month", length.out = 6),
                   'apr23' = seq(as.Date('2022-10-01'),
                                 by = "month", length.out = 6),
                   'nov23' = seq(as.Date('2023-04-01'),
                                 by = "month", length.out = 6),
                   'apr24' = seq(as.Date('2023-10-01'),
                                 by = "month", length.out = 7))

# Rochdale does not have an Apr 21 record 
# Apr 21 (2020-10-01 to 2021-03-01) records are in Nov 21 returns

DR1_data_cleaned = DR1_data %>% 
  filter(case_when(
    month_return == 'nov20' ~ month %in% month_range[['nov20']],
    month_return == 'apr21' ~ month %in% month_range[['apr21']],
    month_return == 'nov21'  & local_authority != 'rochdale' ~ 
      month %in% month_range[['nov21']],
    month_return == 'nov21'  & local_authority == 'rochdale' ~ 
      month %in% c(month_range[['apr21']], month_range[['nov21']]),
    month_return == 'apr22' ~ month %in% month_range[['apr22']],
    month_return == 'nov22' ~ month %in% month_range[['nov22']],
    month_return == 'apr23' ~ month %in% month_range[['apr23']],
    month_return == 'nov23' ~ month %in% month_range[['nov23']],
    month_return == 'apr24' ~ month %in% month_range[['apr24']]
  )) #%>%
  #mutate(
  #  month_return = case_when(
  #    local_authority == 'rochdale' & month %in% month_range[['apr21']] ~ "apr21", 
  #    .default = month_return))

returns_date_checks <- DR1_data_cleaned %>% 
  group_by(local_authority, month_return) %>% 
  summarise(total_nb_months = n(), # total number of months returned
            min_month = min(month), # min month
            max_month = max(month)) %>% # max month
  as.data.frame()  %>%
  dplyr::arrange(local_authority) 

# Save table
writexl::write_xlsx(returns_date_checks, 
                    "DR1_pre_processed_monthly_returns_checks.xlsx")

### Save pre-processed data ----
writexl::write_xlsx(
  DR1_data_cleaned, 
  path = paste0(output_path,
                "pre_processing/pre_processed_data/DR1/",
                "DR1_pre_processed.xlsx"))

### EDA pre-processed records ----
# Check data report
#setwd(paste0(output_path, "pre_processing/Data reports/DR1/"))
#makeDataReport(DR1_data_cleaned, replace = TRUE)

# Processing ####

## Variable class ----
DR1_data_cleaned <- mutate(
  DR1_data_cleaned,
  month = as.Date(month),
  across(.cols = any_of(c(colnames(DR1_data_cleaned)[-c(1:3)])),
         .fns = as.numeric))

## Derive DR1 indicators ----
DR1_streamlined = DR1_data_cleaned %>%
  mutate(
    total_cla = number_of_children_looked_after_at_the_end_of_the_month_in_the_la + 
      number_of_children_who_newly_became_looked_after_this_month_in_the_la,
    total_cin = number_of_cin_plans_that_started_this_month_in_the_la +
      number_of_open_cin_cases_this_month_in_the_la,
    total_cpp = number_of_cp_ps_that_started_this_month_in_the_la +
      number_of_open_cp_ps_this_month_in_the_la) %>%
  relocate(starts_with('total_'),
           .after = cla_rate_per_10_000_children)

## Save processed DR1 data ----
output_path = paste0(sharepoint_path, 'QA/processing/')

writexl::write_xlsx(
  DR1_streamlined, 
  path = paste0(output_path,
                "processed_data/DR1/",
                "DR1_processed.xlsx"))

