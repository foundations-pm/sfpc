setwd('C:/Users/PerrineMachuel/Foundations/High-SFPC-Impact - sfpc_familysafeguarding_cleaning/')

load("Output/cla_merge_dr2_dr3.RData")

cla_merge <- cla_merge %>%
  dplyr::rename(
    child_la_id = `child la id`,
    prev_cpp = `previous cpp1`
  )

# Factor 
cla_merge <- cla_merge %>%
  mutate(
    
    gender1 = relevel(
      factor(gender1), ref = 'Male'),
    
    ethnicity1 = relevel(
      factor(ethnicity1), ref = 'White British or Irish'),
    
    disability1 = relevel(
      factor(disability1), ref = '0'),
    
    uasc1 = relevel(
      factor(uasc1), ref = '0'),
    
    `prev_cpp` = relevel(
      factor(`prev_cpp`), ref = '0'),
    
    age_group = relevel(
      factor(age_group), ref = 'unborn'),
    
    la = relevel(
      factor(la), ref = 'Lancashire'
    )
  )


# Creating an example dataframe with only the variables of interest. 
model_data <- cla_merge %>% select(
  la, time_period, treatment, primary_outcome,
  gender1, age_group, ethnicity1, disability1,
  uasc1, `prev_cpp`) 

model_desc_table = model_data %>%
  dplyr::mutate(across(.cols = is.numeric,
                .fns = as.factor)) %>%
  dplyr::relocate(la) %>%
  describe(class = 'categorical') %>%
  mutate(count = ifelse(count < 5, '[z]', count))

model_desc_la_table = model_data %>%
  dplyr::mutate(across(.cols = is.numeric,
                .fns = as.factor)) %>%
  dplyr::relocate(la) %>%
  group_by(la) %>%
  describe(class = 'categorical',
           group = 'la') %>%
  mutate(count = ifelse(count < 5, '[z]', count))

# Paths  ----
user_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(user_directory,'Foundations/High-SFPC-Impact - ')

# Data and output paths
data_path = paste0(sharepoint_path, 'QA/processing/linked_data/')

output_path = paste0(sharepoint_path, 'QA/outputs/')

writexl::write_xlsx(
  model_desc_table, 
  paste0(
    output_path,
    "model_outputs/",
    "v1_tavistock_drop/",
    "fs_model_sample_descriptives.xlsx"))

writexl::write_xlsx(
  model_desc_la_table, 
  paste0(
    output_path,
    "model_outputs/",
    "v1_tavistock_drop/",
    "fs_model_sample_descriptives_by_la.xlsx"))