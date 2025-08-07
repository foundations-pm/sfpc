# Data Cleaning Script for No Wrong Doors RCT DR2 ----

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

# Pre-processing ----

## Load data ----

# Set working dir to data path
setwd(data_path)

# Load data:
# Create list of file paths
DR2_file_paths <- list.files("NWD_DR2", 
                             pattern="*.xlsx",
                             full.names=TRUE)

### ISSUE 0 ####
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

## Quality checks ----
# Key steps:
# 1- Data set dimensions
# 2- Column names 
# 3- Missing data 
# 4- Unique values / unexpected values (e.g., int vs cat)

for(name in names(DR2_list)){
  
  print(name)
  
  for(i in 1:3){ 
    
    # Print name of dataset: which return from which LA
    print(names(DR2_list[[name]][i]))
    
    # Dimension of data
    print(dim(DR2_list[[name]][[i]])) 
    
    # Number of missing
    print(sum(is.na(DR2_list[[name]][[i]]))) }
}

### ISSUE 1 2 ####

# ISSUE 1: Warrington Nov 2020 missing education fields for CLA and CP 
# ISSUE 2: Warrington Nov 2020 one extra FSM col for ref, NOT IMPORTANT

lapply(1:length(DR2_rochdale_nov20_list), function(i){ 
  print(names(DR2_rochdale_nov20_list[i])) # Same as above
  print(dim(DR2_rochdale_nov20_list[[i]])) 
  print(sum(is.na(DR2_rochdale_nov20_list[[i]])))})

### ISSUE 3 4 5 ####
# ISSUE 3: Rochdale Nov 20 ref missing PPE
# ISSUE 4: Rochdale Nov 20 cla missing FSM and PPE (all educ, same as warrington)
# ISSUE 5: Rochdale Nov 20 cpp missing FSM and PPE (all educ, same as warrington)
# AND includes CPP end dates when not needed, NOT IMPORTANT

## Data cleaning ----

## Work plan

# STEP 1: Identify table in the spreadsheets & assess consistency of column names
# STEP 2: Resize dataframe so the correct columns display
# STEP 3: Assign standard, clean column names 
# STEP 4: Create 3 population-specific lists (referrals, CLA, CPP) & merge files

### STEP 1: identify table  ----

# Check tables are situated after row 3 in each spreadsheet
# Check consistentcy of column names at the same tiem
row_checks = lapply(1:3, function(i) { 
  
  purrr::map_dfr( # Note: this could be written more efficiently (see line 128 below)
    1:length(DR2_list), function(j){ DR2_list[[j]][[i]][3,] }) })

#lapply(row_checks, View)
# Table columns located on row 3 for each return 
# Some column names have spelling mistakes, to correct before merging

### STEP 2: resize ----

DR2_list <- purrr::map(DR2_list, ~ map(
  .x, ~ janitor::clean_names( # clean col names to standard names
    janitor::row_to_names( # make row 1 the columns of each dataset in list
      .x[3:nrow(.x),], 1)))) # resize dataframe to start from row 3

# Add a return-identifier column (LA and date of return)
for(name_x in names(DR2_list)){
  
  local_authority = str_extract(names(DR2_list[name_x]), ".+?(?=_)")
  month_return = gsub(".*\\_", "", names(DR2_list[name_x]))
  
  print(local_authority)
  print(month_return)
  
  for(name_y in 1:length(DR2_list[[name_x]])){
    
    DR2_list[[name_x]][[name_y]] = DR2_list[[name_x]][[name_y]] %>% 
      dplyr::mutate(
        month_return = month_return,
        local_authority = local_authority) %>%
      relocate(local_authority, month_return)
    
  }}

### STEP 3: assign standard col names ----

# Correct non-standard names & keep only necessary columns:

# Derive standard col names from valid return
# we know Leicester return in April 22 uses the expected col names:
referrals_cols = colnames(DR2_list[['leicester_dr2_apr22']][[1]]) 
cla_cols = colnames(DR2_list[['leicester_dr2_apr22']][[2]]) 
cpp_cols = colnames(DR2_list[['leicester_dr2_apr22']][[3]]) 

# Ref dataset for Warrington in Apr 22 does not have standard col names
# Fixing this 
colnames(DR2_list[['warrington_dr2_apr22']][[1]]) = referrals_cols

# Assigning standard col names to Rochdale Nov20 

# 1) add LA and month return info
for(name in 1:length(DR2_rochdale_nov20_list)){
  
  local_authority = str_extract(names(DR2_rochdale_nov20_list[name]), ".+?(?=_)")
  month_return = 'nov20'
  
  print(local_authority)
  print(month_return)
  
  DR2_rochdale_nov20_list[[name]] = DR2_rochdale_nov20_list[[name]] %>% 
    dplyr::mutate(
      month_return = month_return,
      local_authority = local_authority) %>%
    relocate(local_authority, month_return) }

# 2) Assign appropriate col names 

colnames(DR2_rochdale_nov20_list[[
  "rochdale_dr2_nov20ref"]]) = referrals_cols

DR2_rochdale_nov20_list[["rochdale_dr2_nov20ref"]] = select(
  DR2_rochdale_nov20_list[[ "rochdale_dr2_nov20ref"]],
  all_of(referrals_cols)) # DROP NA column

colnames(DR2_rochdale_nov20_list[[
  "rochdale_dr2_nov20cla"]]) = cla_cols[1:(length(cla_cols)-2)]

# Drop CP end date in Rochdale Nov20: info not needed
DR2_rochdale_nov20_list[["rochdale_dr2_nov20cp"]] = select(
  DR2_rochdale_nov20_list[["rochdale_dr2_nov20cp"]], -`CP Plan End Date`)

# Assign standard cols
colnames(DR2_rochdale_nov20_list[["rochdale_dr2_nov20cp"]]) = cpp_cols[1:(length(cpp_cols)-2)]

# Temporarily change all cols as character to merge records
DR2_rochdale_nov20_list = lapply(
  DR2_rochdale_nov20_list, function(data){
    
    dplyr::mutate(data, across(.cols = all_of(colnames(data)),
                               .fns = as.character)) })

### STEP 4: population-specific merges ----

# Create population-specific lists 
DR2_referrals = lapply(DR2_list, '[[', 1) # select first item within list

DR2_referrals = purrr::map_dfr(
  DR2_referrals, function(.x) {
    
    .x %>% dplyr::select(local_authority,
                         month_return,
                         any_of(referrals_cols)) })

DR2_referrals = bind_rows(
  DR2_referrals, 
  DR2_rochdale_nov20_list[['rochdale_dr2_nov20ref']])

DR2_CLA = lapply(DR2_list, '[[', 2) # select 2nd item within list 

DR2_CLA = purrr::map_dfr(
  DR2_CLA, function(.x) {
    
    .x %>% dplyr::select(local_authority,
                         month_return,
                         any_of(cla_cols)) })

DR2_CLA = bind_rows(
  DR2_CLA, DR2_rochdale_nov20_list[['rochdale_dr2_nov20cla']])

DR2_CPP = lapply(DR2_list, '[[', 3) # select 3rd item within list 

DR2_CPP = purrr::map_dfr(
  DR2_CPP, function(.x) {
    
    .x %>% dplyr::select(local_authority,
                         month_return,
                         any_of(cpp_cols)) })

DR2_CPP = bind_rows(
  DR2_CPP, DR2_rochdale_nov20_list[['rochdale_dr2_nov20cp']])

## Exploratory Missing Analysis ----
setwd(output_path)

### Workplan 

# (1) Total number missing & percent missing (out of total cases), per LA and month of return
# (2) Location of missingness

### Field completedness: sum NAs per column, per LA and month of return 
# To find returns where columns are poorly populated
DR2_cleaned_list = list("referrals" = DR2_referrals,
                        "CPP" = DR2_CPP,
                        "CLA" = DR2_CLA)

for(name in names(DR2_cleaned_list)){
  
  print(name)
  
  missing = DR2_cleaned_list[[name]] %>%
    dplyr::group_by(local_authority, month_return) %>%
    dplyr::summarise(
      across(
        everything(),
        list(count_missing = ~ sum(is.na(.x)),
             prop_missing = ~ round(sum(is.na(.x))/length(.x), 3))),
      .groups = "drop") 
  
  summary = DR2_cleaned_list[[name]] %>% 
    dplyr::group_by(local_authority, month_return) %>% 
    dplyr::summarise(
      "number_{name}" := length(child_id),
      unique_children = length(unique(child_id)))
  
  DR2_summary = merge(summary, missing,
                      by = c("local_authority", "month_return"))
  
  View(DR2_summary)
  
  # Save table
  writexl::write_xlsx(DR2_summary, 
                      paste0(output_path, 
                             "pre_processing/DR2_missingness_summary_",
                             name, ".xlsx"))
}

## Save pre-processed data ----

lapply(names(DR2_cleaned_list), function(name){
  
  writexl::write_xlsx(
    DR2_cleaned_list[[name]],
    path = paste0(output_path,
                  "pre_processing/pre_processed_data/DR2/",
                  "DR2_pre_processed_", name, ".xlsx")) })
       
## EDA report  ----
#setwd(paste0(output_path, "pre_processing/Data reports/DR2/"))

#makeDataReport(DR2_referrals)
#makeDataReport(DR2_CLA)
#makeDataReport(DR2_CPP)

# ____________________________________________

# Processing ----

## Variable class checks ----

# Check variable class
lapply(DR2_cleaned_list, summary)

# Date vars:
# DOB (%Y-%m) 
# Ref: Referral date 
# CPP: CP plan start date
# CLA: Date period of care commenced

# Cat vars: 
# Ref: referral NFA
# gender, ethnicity, 
# disabled status, UASC, FSM, PPE

# Cont vars: 
# nb of previous CP plans

# To discard before analysis:
# child_id, referral_id_or_case_id
# = specific analysis for these 

# Check all date vars are in same format across all pre_proccessed records
lapply(
  DR2_cleaned_list, function(data){ 
    
    data %>% 
      dplyr::select(local_authority, 
             month_return,
             year_and_month_of_birth_of_the_child,
             any_of(contains('date'))) %>%
      dplyr::group_by(local_authority, month_return) %>%
      dplyr::filter(row_number()==1)
    
  })

## Fix dates and age ----

#1. Standardised dates & age indicators 
#2. Assign correct variable class

DR2_cleaned_list = lapply(
  
  names(DR2_cleaned_list), function(name){
    
    print(paste0('Dataset cleaned: ', name))
    
    print(paste0('Cleaning DATES'))
    
    date_cols = colnames(DR2_cleaned_list[[name]])[grepl(
      'date', colnames(DR2_cleaned_list[[name]]), fixed=T)]
    
    print(paste0("Date column(s): ",
                 str_flatten(date_cols, collapse = " ")))
    
    # Treat Rochdale separately because of different date format in raw file
    rochdale_nov20 = DR2_cleaned_list[[name]] %>%
      dplyr::filter(
        local_authority == 'rochdale' &
          month_return == 'nov20') %>%
      dplyr::mutate(
        number_of_previous_child_protection_plans = as.numeric(
          number_of_previous_child_protection_plans),
        year_and_month_of_birth_of_the_child = as.Date(
          paste0('01/', year_and_month_of_birth_of_the_child),
          format = "%d/%m/%Y"),
        across(.cols = any_of(date_cols),
               .fns = as.Date))
    
    other_dr2 = DR2_cleaned_list[[name]] %>% 
      dplyr::filter(
        local_authority != 'rochdale' |
          month_return != 'nov20') %>%
      dplyr::mutate(
        across(
          .cols = any_of(
            c(date_cols, "number_of_previous_child_protection_plans")),
          .fns = as.numeric)) %>%
      dplyr::mutate(
        across(
          .cols = any_of(date_cols),
          .fns = ~ as.Date(.x, origin = "1899-12-30")))
    
    print(paste0('Cleaning AGE'))
    
    # Bespoke age handling for norfolk and redcar
    # select N and R and change age as date according to excel format
    norfolk_redcar_age_dr2 = other_dr2 %>% 
      dplyr::filter(local_authority %in% c("norfolk", "redcar")) %>%
      dplyr::mutate(
        year_and_month_of_birth_of_the_child = as.numeric(
          year_and_month_of_birth_of_the_child),
        year_and_month_of_birth_of_the_child = as.Date(
          year_and_month_of_birth_of_the_child,
          origin = "1899-12-30"))
    
    # Keep all other LAs together
    age_dr2 = other_dr2 %>% 
      dplyr::filter(!local_authority %in% c("norfolk", "redcar")) %>%
      dplyr::mutate(year_and_month_of_birth_of_the_child = as.Date(
        paste0('01/', year_and_month_of_birth_of_the_child),
        format = "%d/%m/%Y"))
    
    DR2_cleaned_list[[name]] = dplyr::bind_rows(
      age_dr2, norfolk_redcar_age_dr2, # these 2 constitute 'other_dr2'
      rochdale_nov20)
    
    return(DR2_cleaned_list[[name]])
    
  })

names(DR2_cleaned_list) = c("referrals", "cpp", "cla")

## Save DR2 processed ----
output_path = paste0(output_path, 'processing/')

lapply(names(DR2_cleaned_list), function(name){
  
  writexl::write_xlsx(
    DR2_cleaned_list[[name]],
    path = paste0(output_path,
                  "processed_data/DR2/",
                  "DR2_processed_", name, ".xlsx")) })
