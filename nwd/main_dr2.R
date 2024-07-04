# Data Cleaning Script for No Wrong Doors RCT dr2 ----

# Paths  ----
user_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(user_directory,'Foundations/High-SFPC-Impact - ')

# Data and output paths
data_path = paste0(sharepoint_path, 'Data/')
output_path = paste0(sharepoint_path, 'QA/')

# Working directory
wd = paste0(user_directory, "Documents/sfpc/nwd/")

# Libraries ----
{ source(paste0(wd, "config.R")) }

# Functions ----
{ source(paste0(wd, "functions.R"))}

# Load data ----

# Set working dir to data path
setwd(data_path)

# Load data:

# Create list of file names
dr2_names <- list.files("NWD_DR2", # Name of folder with dr2 data
                        pattern="*.xlsx", full.names=TRUE)

# Remove incomplete files
#dr2_names = dr2_names[-16] # Rochdale Nov 2020

# Read files into local env
dr2_list <- lapply(dr2_names, function(file) {  
  print(file) 
  readxl::read_excel(file, sheet = "dr2 - Data - aggregate level")})

# Assign names to each file
string_to_remove = c("NWD_dr2/", ".xlsx")
dr2_names <- stringr::str_remove_all(dr2_names, 
                                     paste(string_to_remove,
                                           collapse = "|"))
names(dr2_list) = dr2_names

# Create LA string
#la_string = str_extract(dr2_names, ".+?(?=_)")

# Quality checks ----
# Key steps:
# 1- Datasets dimensions
# 2- Column names 
# 3- Missing data 
# 4- Unique values / unexpected values (e.g., int vs cat)

lapply(1:length(dr2_list), function(i){ 
  print(names(dr2_list[i]))
  print(dim(dr2_list[[i]])) 
  print(sum(is.na(dr2_list[[i]])))})

#colnames(dr2_list[['NWD_dr2/warrington_dr2_nov20.xlsx']])
#colnames(dr2_list[['NWD_dr2/warrington_dr2_nov22.xlsx']])
#colnames(dr2_list[['NWD_dr2/leicester_dr2_nov20.xlsx']])

# Data cleaning ----

## Workplan ----

# STEP 1: merge 

# 1.1 - Keep only mandatory return & assing new names

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

## Step 1: merge ----
# Create merge df:
# 1 - Keep mandatory returns: columns of interest only
# 2 - Create a column to identify each dataset by LA and month of return
# 3 - Merge all records 

# create clean names for mandatory returns
mandatory_returns <- janitor::make_clean_names(colnames(
  dr2_list[['leicester_dr2_apr21']])) 

# Create merged table
dr2_data <- purrr::map_dfr(1:length(dr2_list), function(i) {
  
  # Identify LA return by its name 
  # format = <LA name> _ < month/year of return>
  name = names(dr2_list)[i] 
  
  # Merge all records into one table:
  
  dr2_list[[i]] <- dr2_list[[i]] %>% 
    janitor::clean_names() %>% # Clean column names 
    dplyr::mutate(local_authority = name) %>% # New column with LA identifier
    dplyr::select(local_authority, # select only mandatory returns
                  any_of(mandatory_returns)) %>%
    dplyr::mutate(across(everything(), as.character))
  # temporarily convert all columns as character to bind them together
  
  # check dimension of data
  print(names(dr2_list[i])) 
  print(dim(dr2_list[[i]])) 
  
  # Quality checks 
  # Checks: there should be 12 columns
  # 11 mandatory returns + 1 column for LA return name 
  if(dim(dr2_list[[i]])[[2]] != 12){ 
    print("ISSUE WITH DATA TO CHECK") }
  
  return(dr2_list[[i]])
  
})

## Step 2: checks ----

# Data cleaning checks 

# Check completedness of returns: 

# 6 months records from Oct 2019:
# Supposedly, 36months
# 2019-2020: (1) Oct 19 - March 20; (2) Apr 20 - Sept 20 = this is likely one record (nov20)
# 2020-2021:  (1) Oct 20 - March 21 (Apr 21?); (2) Apr 21 - Sept 21 (Nov21?)
# 2021-2022:  (1) Oct 21 - March 22 (Apr 22?); (2) Apr 22 - Sept 22 (Nov22?)

# Assign date class & arrange column conveniently
dr2_data <- dr2_data %>% 
  dplyr::mutate(month = as.Date(month, "%Y-%m-%d"),
                month_return = gsub(".*\\_", "", local_authority),
                month_return = factor(
                  month_return,
                  levels = c("nov20", "apr21", "nov21", "apr22", "nov22")),
                local_authority = str_extract(local_authority, ".+?(?=_)")) %>%
  dplyr::relocate(local_authority, month_return)

# Return date checks 
returns_date_checks <- dr2_data %>% 
  group_by(local_authority, month_return) %>% 
  summarise(total_nb_months = n(), # total number of months returned
            min_month = min(month), # min month
            max_month = max(month)) %>% # max month
  as.data.frame()  %>%
  dplyr::arrange(local_authority) 

returns_date_checks %>%
  group_by(local_authority) %>%
  summarise(sum(total_nb_months))

# Save table
writexl::write_xlsx(returns_date_checks, 
                    paste0(output_path, 
                           "dr2_monthly_returns_quality_checks.csv"))
