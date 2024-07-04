# Data Cleaning Script for No Wrong Doors RCT DR2 ----

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

# Create list of file paths
DR2_file_paths <- list.files("NWD_DR2", 
                             pattern="*.xlsx",
                             full.names=TRUE)

## ISSUE 0 ####
# Rochdale Nov20 data format too different to be treated with other files
# ISSUE 0 mitigation: create separate list 

DR2_rochdale_nov20_file_path = DR2_file_paths[c(8,9,10)]
names(DR2_rochdale_nov20_file_path) = DR2_rochdale_nov20_file_path

DR2_file_paths = DR2_file_paths[-c(8,9,10)]
names(DR2_file_paths) = DR2_file_paths

# Create DR2 file lists: read files into local env
DR2_list = lapply(DR2_file_paths, function(.file_paths){
  
  read_xlsx_worksheets(.file_paths) })

DR2_rochdale_nov20_list = lapply(DR2_rochdale_nov20_file_path,
                                 read_excel)

# Clean list names
string_to_remove = c("NWD_DR2/", ".xlsx")

names(DR2_list) <- stringr::str_remove_all(
  names(DR2_list),  
  paste(string_to_remove, collapse = "|"))

names(DR2_rochdale_nov20_list) <- stringr::str_remove_all(
  names(DR2_rochdale_nov20_list), 
  paste(string_to_remove, collapse = "|"))

# Quality checks ----
# Key steps:
# 1- Datasets dimensions
# 2- Column names 
# 3- Missing data 
# 4- Unique values / unexpected values (e.g., int vs cat)

for(name in names(DR2_file_paths)){
  
  print(name)
  
  for(i in 1:3){ 
    
    print(names(DR2_list[[name]][i]))
    
    # Dimension of data
    print(dim(DR2_list[[name]][[i]])) 
    
    # Number of missing
    print(sum(is.na(DR2_list[[name]][[i]]))) }
}

### ISSUE 1 2 ####

# ISSUE 1: Warrington Nov 2020 missing education fields for CLA and CP 
# ISSUE 2: Warrington Nov 2020 one extra FSM col

lapply(1:length(DR2_rochdale_nov20_list), function(i){ 
  print(names(DR2_rochdale_nov20_list[i]))
  print(dim(DR2_rochdale_nov20_list[[i]])) 
  print(sum(is.na(DR2_rochdale_nov20_list[[i]])))})

### ISSUE 3 4 5 ####
# ISSUE 3: Rochdale Nov 20 ref missing PPE
# ISSUE 4: Rochdale Nov 20 cla missing FSM and PPE (all educ, same as warrington)
# ISSUE 5: Rochdale Nov 20 cpp missing FSM and PPE (all educ, same as warrington)
# AND includes CPP end dates when not needed

# Data cleaning ----

# Create population-specific lists 
DR2_referrals = lapply(DR2_list, '[[', 1)
names(DR2_referrals) = paste0(names(DR2_referrals), "_referrals")

DR2_CPP = lapply(DR2_list, '[[', 2)
names(DR2_CPP) = paste0(names(DR2_CPP), "_CPP")

DR2_CLA = lapply(DR2_list, '[[', 3)
names(DR2_CLA) = paste0(names(DR2_CLA), "_CLA")

## Work plan ----

# STEP 1: merge 

# 1.1 - Keep only mandatory return & assign clean names
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
  DR2_list[['leicester_DR2_apr21']][[1]])) 

# Create merged table
DR2_data <- purrr::map_dfr(1:length(DR2_list), function(i) {
  
  # Identify LA return by its name 
  # format = <LA name> _ < month/year of return>
  name = names(DR2_list)[i] 
  
  # Merge all records into one table:
  
  DR2_list[[i]] <- DR2_list[[i]] %>% 
    janitor::clean_names() %>% # Clean column names 
    dplyr::mutate(local_authority = name) %>% # New column with LA identifier
    dplyr::select(local_authority, # select only mandatory returns
                  any_of(mandatory_returns)) %>%
    dplyr::mutate(across(everything(), as.character))
  # temporarily convert all columns as character to bind them together
  
  # check dimension of data
  print(names(DR2_list[i])) 
  print(dim(DR2_list[[i]])) 
  
  # Quality checks 
  # Checks: there should be 12 columns
  # 11 mandatory returns + 1 column for LA return name 
  if(dim(DR2_list[[i]])[[2]] != 12){ 
    print("ISSUE WITH DATA TO CHECK") }
  
  return(DR2_list[[i]])
  
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
DR2_data <- DR2_data %>% 
  dplyr::mutate(month = as.Date(month, "%Y-%m-%d"),
                month_return = gsub(".*\\_", "", local_authority),
                month_return = factor(
                  month_return,
                  levels = c("nov20", "apr21", "nov21", "apr22", "nov22")),
                local_authority = str_extract(local_authority, ".+?(?=_)")) %>%
  dplyr::relocate(local_authority, month_return)

# Return date checks 
returns_date_checks <- DR2_data %>% 
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
                           "DR2_monthly_returns_quality_checks.csv"))
