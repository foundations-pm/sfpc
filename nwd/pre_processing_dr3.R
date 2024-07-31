# Data Cleaning Script for No Wrong Doors RCT DR3 ----

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
DR3_file_paths <- list.files("NWD_DR3", 
                             pattern="*.xlsx",
                             full.names=TRUE)

names(DR3_file_paths) = DR3_file_paths

# Create DR3 file lists: read files into local env
DR3_list = lapply(DR3_file_paths, function(.file_paths){
  
  read_xlsx_worksheets(.file_paths) }) # this function can be found in the functions.R script

# Clean list names
string_to_remove = c("NWD_DR3/", ".xlsx")

names(DR3_list) <- stringr::str_remove_all(
  names(DR3_list),  
  paste(string_to_remove, collapse = "|"))

# Quality checks ----
# Key steps:
# 1- Data set dimensions
# 2- Column names 
# 3- Missing data 
# 4- Unique values / unexpected values (e.g., int vs cat)

for(name in names(DR3_list)){
  
  print(name)
  
  for(i in 1:length(DR3_list[[name]])){ 
    
    # Print name of dataset: which return from which LA
    print(names(DR3_list[[name]][i]))
    
    # Dimension of data
    print(dim(DR3_list[[name]][[i]])) 
    
    # Number of missing
    print(sum(is.na(DR3_list[[name]][[i]]))) }
}

### ISSUE 0 ----
# Where are Rochdale CLA obs?

# TO CHECK: uneven number of columns:
# suggest there might be mandatory fields missing

### ISSUE 0.1 ----
# Warrington needs a pivot wider

# Data cleaning ----

## Work plan ----

# STEP 1: Identify table in the spreadsheets & assess consistency of column names
# STEP 2: Create 3 population-specific lists (referrals, CLA, CPP) 
# and resize dataframes so the correct columns display 
# - needs to be bespoke due to variability in data format
# STEP 3: Assign standard, clean column names 
# STEP 4: Merge files

## STEP 1: identify table  ----

# Check tables are situated at different row locations depending on population:
# Check consistency of column names 

row_checks_cla = purrr::map_dfr(
  DR3_list, ~ check_table_location(data = .x[[1]],
                                   row_start = 3))

row_checks_care_episode = purrr::map_dfr(
  DR3_list, ~ check_table_location(data = .x[[2]],
                                   row_start = 1))

row_checks_neet = purrr::map_dfr(
  DR3_list, ~ check_table_location(data = .x[[3]], 
                                   row_start = 1))

row_checks_cin = purrr::map_dfr(
  DR3_list, ~ check_table_location(data = .x[[4]], 
                                   row_start = 5))

View(row_checks_cla)
View(row_checks_care_episode)
View(row_checks_neet)
View(row_checks_cin)

### ISSUE 1 ----
# Formatting inconsistencies 

# CLA: All LA tables located at row 3
# CIN: All LA tables located at row 5

# OLD DATA:
# Care episode:
# Norfolk: table starting at row 1 and delete row 2
# Redcar: columns is already good, must delete row 1 
# Rochdale: columns already good
# Warrington: all good

# TO FLAG: NEW DATA PRE PROCESSING CHANGE ----
# NEW DATA:
# table starting at row 1 and must delete row 2 in redcar and rochdale

# NEET:
# Norfolk, Redcar, Rochdale, starting at row 1 
# Warrington NEET: need pivot 

### ISSUE 2 ----
# Warrington CIN: Referral ID is Child ID + Referral ID 

## STEP 2/3/4: population specific resizing, columns and merge ----

### 1 CLA resizing, columns and merge ----

# Resizing
DR3_cla = lapply(DR3_list, '[[', 1) # select first item within list

DR3_cla <- purrr::map(DR3_cla, ~ janitor::clean_names( # clean col names to standard names
  janitor::row_to_names( # make row 1 the columns of each data set in list
    .x[3:nrow(.x),], 1))) # resize data frame to start from row 3

lapply(DR3_cla, colnames)

# Add standard colnames & LA identifier: LA and month of return
cla_colnames = colnames(DR3_cla[[2]])

# Merge data frames 
DR3_cla = purrr::map_dfr(names(DR3_cla), function(name) {
    
    local_authority = str_extract(name, ".+?(?=_)")
    month_return = gsub(".*\\_", "", name)
    
    DR3_cla[[name]] = DR3_cla[[name]] %>% 
      dplyr::mutate(
        month_return = month_return,
        local_authority = local_authority) %>%
      relocate(local_authority, month_return)
    
    DR3_cla[[name]] %>% 
      dplyr::select(local_authority,
                    month_return,
                    any_of(cla_colnames)) })

# CHECKS TO PERFORM ----
# Check row nb in dataset correspond to raw data

### 2 CIN resizing, columns and merge ----
# And assign standard col names

DR3_cin = lapply(DR3_list, '[[', 4) # select first item within list

DR3_cin <- purrr::map(DR3_cin, ~ janitor::clean_names( # clean col names to standard names
  janitor::row_to_names( # make row 1 the columns of each data set in list
    .x[5:nrow(.x),], 1))) # resize data frame to start from row 3

lapply(DR3_cin, colnames)

# Add standard colnames & LA identifier: LA and month of return
cin_colnames = colnames(DR3_cin[[1]])

# Merge data frames 
DR3_cin = purrr::map_dfr(names(DR3_cin), function(name) {
  
  local_authority = str_extract(name, ".+?(?=_)")
  month_return = gsub(".*\\_", "", name)
  
  DR3_cin[[name]] = DR3_cin[[name]] %>% 
    dplyr::mutate(
      month_return = month_return,
      local_authority = local_authority) %>%
    relocate(local_authority, month_return)
  
  DR3_cin[[name]] %>% 
    dplyr::select(local_authority,
                  month_return,
                  any_of(cin_colnames)) })

### 3 Care Episode resizing, columns and merge ----
DR3_care_episode = lapply(DR3_list, '[[', 2) # select first item within list

# Resizing
# Locate table for Norfolk: starts at row 1
DR3_care_episode <- purrr::map(DR3_care_episode, ~ janitor::clean_names( # clean col names to standard names
  janitor::row_to_names( # make row 1 the columns of each data set in list
    .x[1:nrow(.x),], 1))) # resize data frame to start from row 3

# Delete row 1 in Rochdale and Redcar due to formatting issue
DR3_care_episode[['rochdale_dr3_final_apr24']] <- DR3_care_episode[['rochdale_dr3_final_apr24']][-1,]
DR3_care_episode[['redcar_dr3_final_apr24']] <- DR3_care_episode[['redcar_dr3_final_apr24']][-1,]

# Set standard colnames
lapply(DR3_care_episode, colnames) # check which colnames have standard format
lapply(DR3_care_episode, ncol) # check nb of cols

care_episode_colnames = colnames(
  DR3_care_episode[['redcar_dr3_final_apr24']])

# Merge
DR3_care_episode = purrr::map_dfr(names(DR3_care_episode), function(name) {
  
  local_authority = str_extract(name, ".+?(?=_)")
  month_return = gsub(".*\\_", "", name)
  
  DR3_care_episode[[name]] = DR3_care_episode[[name]] %>% 
    janitor::clean_names() %>%
    dplyr::mutate(
      month_return = month_return,
      local_authority = local_authority) %>%
    relocate(local_authority, month_return) %>%
    dplyr::mutate(across(.cols = c(
      'child_id',
      'referral_id_or_case_id',
      'date_period_of_care_commenced',
      'date_episode_commenced',
      'date_episode_ceased',
      'date_period_of_care_ended'),
      .fns = as.character)) # temporarily
  
  DR3_care_episode[[name]] %>% 
    dplyr::select(local_authority,
                  month_return,
                  any_of(care_episode_colnames)) })

### 4 NEET resizing, columns and merge ----
DR3_neet = lapply(DR3_list, '[[', 3) # select first item within list

# Resize
DR3_neet <- purrr::map(DR3_neet, ~ janitor::clean_names( # clean col names to standard names
  janitor::row_to_names( # make row 1 the columns of each data set in list
    .x[1:nrow(.x),], 1))) # resize data frame to start from row 3

lapply(DR3_neet, colnames)

# Pivot Warrington
DR3_neet[['warrington_dr3_final_apr24']] <- tidyr::pivot_wider(
  DR3_neet[['warrington_dr3_final_apr24']],
  names_from = 'processing_year_903_submission',
  values_from = 'activ_status',
  names_prefix = 'main_activity_processing_year_')

# Select standard columns
neet_colnames = colnames(DR3_neet[[1]])

# Merge
DR3_neet = purrr::map_dfr(names(DR3_neet), function(name) {
  
  local_authority = str_extract(name, ".+?(?=_)")
  month_return = gsub(".*\\_", "", name)
  
  DR3_neet[[name]] = DR3_neet[[name]] %>% 
    janitor::clean_names() %>%
    dplyr::mutate(
      month_return = month_return,
      local_authority = local_authority) %>%
    relocate(local_authority, month_return) #%>%
    #dplyr::mutate(across(.cols = c(
    #  'child_id',
    #  'referral_id_or_case_id',
    #  'date_period_of_care_commenced',
    #  'date_episode_commenced',
    #  'date_episode_ceased',
    #  'date_period_of_care_ended'),
    #  .fns = as.character)) # temporarily
  
  DR3_neet[[name]] %>% 
    dplyr::select(local_authority,
                  month_return,
                  any_of(neet_colnames)) })

# Exploratory Missing Analysis ----

### Workplan ----

# (1) Total number missing & percent missing (out of total cases), per LA and month of return
# (2) Location of missingness

### Missingness report 
DR3_cleaned_list = list("cla" = DR3_cla,
                        "care_episode" = DR3_care_episode,
                        "neet" = DR3_neet,
                        "cin" = DR3_cin)

for(name in names(DR3_cleaned_list)){
  
  print(name)
  
  missing = DR3_cleaned_list[[name]] %>%
    dplyr::group_by(local_authority, month_return) %>%
    dplyr::summarise(
      across(
        everything(),
        list(count_missing = ~ sum(is.na(.x)),
             prop_missing = ~ round(sum(is.na(.x))/length(.x), 3))),
      .groups = "drop") 

  summary = DR3_cleaned_list[[name]] %>% 
    dplyr::group_by(local_authority, month_return) %>% 
    dplyr::summarise(
      "number_{name}" := length(child_id),
      unique_children = length(unique(child_id)))
  
  DR3_summary = merge(summary, missing,
                      by = c("local_authority", 
                             "month_return"))
  
  # Save table
  writexl::write_xlsx(DR3_summary, 
                      paste0(output_path, 
                             "pre_processing/DR3_missingness_summary_",
                             name, ".xlsx"))
}

lapply(names(DR3_cleaned_list), function(name){
  
  writexl::write_xlsx(
    DR3_cleaned_list[[name]],
    path = paste0(output_path,
                  "pre_processing/pre_processed_data/DR3/",
                  "DR3_pre_processed_", name, ".xlsx")) })


### EDA report ----
setwd(paste0(output_path,"pre_processing/Data reports/DR3/"))

makeDataReport(DR3_cla)
makeDataReport(DR3_care_episode)
makeDataReport(DR3_neet)
makeDataReport(DR3_cin)