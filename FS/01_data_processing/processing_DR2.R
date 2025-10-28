#------------------------------------------------#
#
# FAMILY SAFEGUARDING: STEPPED WEDGE ANALYSES ----
#
#------------------------------------------------#


## DATA CLEANING: DATA RETURN 2 ----

## 1. Set-up  ----
r_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(r_directory,'Foundations/High-SFPC-Impact - Family Safeguarding')

# Data and output paths
data_path = paste0(sharepoint_path, '/Data')
output_path = paste0(sharepoint_path, '/Datasets/pre_processing')

# Working directory
wd = paste0(r_directory, "Documents/SFPC/FS/")

# Libraries 
{ source(paste0(r_directory, "Documents/SFPC/FS/config.R")) }

# Functions 
{ source(paste0(r_directory, "Documents/SFPC/FS/functions.R"))}

# Dates 
date = format(Sys.Date(),"%Y/%m/%d") # date format to save within dataframes

file_date = format(Sys.Date(),"_%Y%b%d") # date format to save files

dir_date = format(Sys.Date(),"%B %Y") # date format to create directories

## 2. Pre-processing ----

### 1. Load data ----

# Set working dir to data path
setwd(data_path)

# Load data:
# Create list of file paths
DR2_file_paths <- list.files("FS_DR2", 
                             pattern="*.xlsx",
                             full.names=TRUE)

# List of all DR2 files
DR2_list = lapply(DR2_file_paths, function(.file_paths){
  
  read_xlsx_worksheets(.file_paths,
                       sheet_name = 'Data|Sheet') })

# Add in Swindon Nov 2022 - sheet called 'DR2' 
# Sheet name too generic to be called with others
DR2_list[[8]][[1]] = readxl::read_excel(DR2_file_paths[[8]], sheet = 'DR2')

# Assign names to list
# Clean list names
names(DR2_list) = DR2_file_paths

string_to_remove = c("FS_DR2/", ".xlsx")

names(DR2_list) <- stringr::str_remove_all(
  names(DR2_list),  
  paste(string_to_remove, collapse = "|"))

### 2. Inspect data ----
# Key steps:
# 1- Data set dimensions
# 2- Column names 
# 3- Missing data 
# 4- Unique values / unexpected values (e.g., int vs cat)

for(name in names(DR2_list)){
  
  print(name)
  
  # Print name of dataset: which return from which LA
  print(names(DR2_list[[name]][1]))
  
  # Dimension of data
  print(dim(DR2_list[[name]][[1]])) 
  
  # Print row 3 to assess where table starts
  print(DR2_list[[name]][[1]][3,]) }

### 3. Resize data ----

# Swindon Nov 22: table starts at row 1 - colnames already correct
# Resize all tables starting from row 3
DR2_list[-8] <- purrr::map(DR2_list[-8], ~ map( # remove Swindon (8th in the list ) from operation
  .x, ~ janitor::clean_names( # clean col names to standard names
    janitor::row_to_names( # make row 1 the columns of each dataset in list
      .x[3:nrow(.x),], 1)))) # resize dataframe to start from row 3

DR2_list[['swindon_dr2_nov22']][[1]] <-  janitor::clean_names(
  DR2_list[['swindon_dr2_nov22']][[1]])

lapply(DR2_list, function(data) colnames(data[[1]]))
lapply(DR2_list, function(data) dim(data[[1]]))

### 4. Add LA ID ---- 

# Locate month return and LA
for(return in names(DR2_list)){ 
  
  local_authority = str_extract(names(DR2_list[return]), ".+?(?=_)")
  month_return = gsub(".*\\_", "", names(DR2_list[return]))
  
  print(local_authority)
  print(month_return)
  
  # Assign LA name and month return
  DR2_list[[return]][[1]] = DR2_list[[return]][[1]] %>% 
      dplyr::mutate(
        month_return = month_return,
        local_authority = local_authority) %>%
      relocate(local_authority, month_return) }

### 5. Standardise columns ----

# Cols to standardise: 
# (1) Telford Nov 22 needs ethnicity and DOB columns standardised
# (2) All LAs except Lancashire need cols factors assessed at the end... 1B,2B,3A,3B,3C,4B

# Telford Nov 22 -
# 1 need to collapse (1) year_and_month_of_birth_of_the_child with (stores month of birth)
# (2) ethnicity (stores year of birth)
# 2 ethnicity_2 needs to be ethnicity 

DR2_list[['telford_dr2_nov22']][[1]] = DR2_list[['telford_dr2_nov22']][[1]] %>%
  dplyr::mutate(
    year_and_month_of_birth_of_the_child = paste(
      year_and_month_of_birth_of_the_child, ethnicity, sep ='/')) %>%
  dplyr::select(-ethnicity) %>%
  dplyr::rename(ethnicity = ethnicity_2)

# All LAs except Lancashire -
# factors_identified_at_the_end_of_assessment columns needed for:
# 1B, 2B, 3A, 3B, 3C, 4B
DR2_list[5:20] = lapply(DR2_list[5:20], function(data) { # 1:4 is lancashire
  
  data[[1]] %>%
    dplyr::mutate(
      #1B
      factors_identified_at_the_end_of_assessment_1b = case_when(
        grepl('\\b1b\\b|\\b1B\\b', factors_identified_at_the_end_of_assessment) ~ 'Yes',
        TRUE ~ 'No'),
      #2B
      factors_identified_at_the_end_of_assessment_2b = case_when(
        grepl('\\b2b\\b|\\b2B\\b', factors_identified_at_the_end_of_assessment) ~ 'Yes',
        TRUE ~ 'No'),
      #3A
      factors_identified_at_the_end_of_assessment_3a = case_when(
        grepl('\\b3a\\b|\\b3A\\b', factors_identified_at_the_end_of_assessment) ~ 'Yes',
        TRUE ~ 'No'),
      #3B
      factors_identified_at_the_end_of_assessment_3b = case_when(
        grepl('\\b3b\\b|\\b3B\\b', factors_identified_at_the_end_of_assessment) ~ 'Yes',
        TRUE ~ 'No'),
      #3C
      factors_identified_at_the_end_of_assessment_3c = case_when(
        grepl('\\b3c\\b|\\b3B\\b', factors_identified_at_the_end_of_assessment) ~ 'Yes',
        TRUE ~ 'No'),
      #4B
      factors_identified_at_the_end_of_assessment_4b = case_when(
        grepl('\\b4b\\b|\\b4B\\b', factors_identified_at_the_end_of_assessment) ~ 'Yes',
        TRUE ~ 'No')) %>%
    dplyr::relocate(contains('factors'), .after = last_col())
}) 

# All data is 
# Bring Lancaster to same list level as other LAs
DR2_list[1:4] <- lapply(DR2_list[1:4], `[[`, 1)

### 6. Unify data classes ----

#### 1. As character ----
# Turn all columns into character class
DR2_list_transformed = lapply(
  DR2_list, function(data) { 
    data %>% dplyr::mutate(across(everything(), as.character)) })

#### 2. As date ----
# Turn date columns into dates
DR2_list_transformed = lapply(DR2_list_transformed, function(data) {
  
  data %>% 
    dplyr::mutate(
      across(.cols = c('referral_date', 'assessment_actual_start_date'),
             .fns = ~ case_when(
               grepl('/', .x) ~ as.Date(.x, '%d/%m/%Y'),
               grepl('-', .x) ~ as.Date(.x, '%Y-%m-%d'),
               TRUE ~ janitor::excel_numeric_to_date(as.numeric(.x))
             ))) %>%
    dplyr::mutate(
      across(
        .cols = 'year_and_month_of_birth_of_the_child',
        .fns = ~ case_when(
          grepl('/', .x) ~ as.Date(paste0('01/', .x), '%d/%m/%Y'),
          grepl('-', .x) ~ as.Date(paste0('01-', .x), '%d-%m-%d'),
          TRUE ~ janitor::excel_numeric_to_date(as.numeric(.x))
        )))
}) 


#### 3. QA ----
# Compare missingness pre and post cleaning
# Count number of NAs per column 
# Make table 
dr2_pre_cleaning_missingness = purrr::map_dfr(
  DR2_list, function(data_return) {
    dplyr::tibble(
      column = names(data_return),
      n_missing = colSums(is.na(data_return))
    )
  }, .id = 'data_return')

dr2_post_cleaning_missingness = purrr::map_dfr(
  DR2_list_transformed, function(data_return) {
    dplyr::tibble(
      column = names(data_return),
      n_missing = colSums(is.na(data_return))
    )
  }, .id = 'data_return')

# save tables
tables = c('dr2_pre_cleaning_missingness', 'dr2_post_cleaning_missingness')

mget(tables) %>%                     # fetch objects safely by name
  iwalk(~ write_xlsx(
    .x,
    file.path(output_path, "DR2", "QA", paste0(.y, file_date, ".xlsx"))
  ))

# discrepancies between DOBs because of unborn children 
# the rest of NAs are rightfully coded - either NULL, / or N/A values
# is OK to code unborn as NULL because other demographic characteristics can identify 'unborn'
# IMPORTANT NOTE ---
# some unborn children have no DOB > this will show as NA in year_and_month_of_birth_of_the_child
# some unborn children have a DOB > but other of their demographic characteristics are coded as 'unborn'

### 7. Bind rows ----
DR2_data = do.call(bind_rows, DR2_list_transformed)

DR2_data = DR2_data %>% 
  dplyr::select(
  -na, -contains('na_'), -child_birth_month_and_year) 

### 8. Save data ---- 
saveRDS(DR2_data, file = paste0(
  output_path,"/DR2/DR2_pre_processed_data.Rds")) 

