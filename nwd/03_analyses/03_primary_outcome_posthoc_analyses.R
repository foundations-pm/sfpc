# Primary outcome analysis for No Wrong Doors RCT DR1 ----

# Set up  ----

#Paths
user_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(user_directory,'Foundations/High-SFPC-Impact - ')

# Data and output paths
data_path = paste0(sharepoint_path, 'QA/outputs/')

output_path = paste0(sharepoint_path,
                     'QA/outputs/model_outputs/sensitivity_analyses/')

# Dates
date = format(Sys.Date(),"%Y/%m/%d") # date format to save within dataframes

file_date = format(Sys.Date(),"_%Y%b%d") # date format to save files

dir_date = format(Sys.Date(),"%B %Y") # date format to create directories

# Set up folders to save output findings 
# in a neat an organised manner 
# Save individual files in a new directory
# Named after the month when the analyses were conducted
main_dir = output_path

sub_dir = dir_date

if(!dir.exists(file.path(paste0(main_dir, sub_dir)))){
  
  dir.create(file.path(main_dir, sub_dir)) # create new dir
  paste0("Creating new directory: '", sub_dir,"'")# confirm new dir is created
  
} else { 
  
  cat(
    crayon::green(
      crayon::bold(
        paste0("Directory '", sub_dir, "' already exists."))))
  
} # confirms dir already exists

# Working directory
wd = paste0(user_directory, "Documents/sfpc/nwd/")

# Libraries 
{ source(paste0(wd, "config.R")) }

# Functions
{ source(paste0(wd, "functions.R"))}

# Load data --------------------------------------------------------------------

# 1 load data 
data <- readRDS(file = paste0(
  data_path, 'primary_analysis_analytical_dataset_V2.Rds'))

# Posthoc analyses (PH) ---------------------------------------------------------

## PH1: RTM effects ------------------------------------------------------------

### Data --------------------------------------------------------

#### Un-imputed data -------------------
# Already loaded

#### Derive RTM indicator --------------
# Add yearly LAC rates to investigate RTM effects
# National rates (2020-2024): https://explore-education-statistics.service.gov.uk/data-tables/children-looked-after-in-england-including-adoptions/2024?subjectId=dc0d45df-d9e5-4d83-73ce-08dcfa65edf3 
# National rates (2019): https://assets.publishing.service.gov.uk/media/5de7a998e5274a06d662b19f/Children_looked_after_in_England_2019_Text.pdf

# Join data 
national_lac_rates = data.frame(
  year = c(2019,2020, 2021, 2022),
  national_lac_rates_per_10_000_children = c(65, 68, 69, 70))

yearly_lac_rates_by_LA = data %>%
  dplyr::mutate(year = year(referral_date)) %>%
  dplyr::select(local_authority, year, month, cla_rate_per_10_000_children) %>%
  dplyr::group_by(local_authority) %>%
  dplyr::distinct(month, .keep_all = TRUE) %>% 
  dplyr::group_by(local_authority, year) %>%
  dplyr::summarise(yearly_lac_rates_by_LA = mean(cla_rate_per_10_000_children)) %>%
  dplyr::ungroup() 

# Sense checks with published data:
# All LAs v close to rates per 10,000 children published in DfE SSDA903
# Except Rochdale: published = 99, reported in trial = 105

data = data %>%
  dplyr::mutate(year = year(referral_date))

data = left_join(data, yearly_lac_rates_by_LA, 
                 by = c('local_authority', 'year'))

data = left_join(data, national_lac_rates, 
                 by = c('year'))

# Divergence from national averages:
# RTM effects 
data = dplyr::mutate(
  data,
  rtm_year_indicator = as.numeric(
    yearly_lac_rates_by_LA) -  as.numeric(national_lac_rates_per_10_000_children),
  rtm_month_indicator = as.numeric(
    cla_rate_per_10_000_children) - as.numeric(national_lac_rates_per_10_000_children))

#### Imputed data --------------------------------------
setwd(paste0(output_path, "/imputed_datasets/"))

file.info(
  list.files(
    "Norfolk_binary_single_level_m5_imputation/",
    full.names=T))[,1, drop=F]

load(paste0("Norfolk_binary_single_level_m5_imputation/",
            "Norfolk_binary_single_level_m5_imputation.Rdata"))

imputed_data_m5 = mi.res 

imputed_data_m10 = load(
  paste0("Norfolk_binary_single_level_m10_imputation/",
         "Norfolk_binary_single_level_m10_imputation.Rdata"))

imputed_data_m10 = mi.res 

rm(mi.res)

# Add indicator to each imputed dataset
# IN PROGRESS

### Formula --------------------------------------------------------

# Prep formula 
demographics = paste('age_at_referral_cat',
                     'gender',
                     'ethnicity_agg',
                     'disabled_status',
                     'unaccompanied_asylum_seeker',
                     'number_of_previous_child_protection_plans',
                     #'referral_no_further_action',
                     sep = " + ")

re = " + (1 | local_authority)"

cluster_indicator = str_flatten(
  c(" + prop_white_british",
    #" + rtm_year_indicator"#,
    " + rtm_month_indicator"
    #" + turnover_rate_fte",
    #" + population_0_to_17" #,
    #" + splines::ns(cla_rate_per_10_000_children, df = 5)" #,
    #" + splines::ns(cpp_rate_per_10_000_children, df = 5)" #,
    #" + splines::ns(cin_rate_per_10_000_children, df = 5)"
  ))

formula = paste0(
  "cla_status ~ treatment_group + wedge + ", # FE for trt + time effects
  demographics, # adjust for person level demographics
  cluster_indicator, # adjust for time-varying cluster level indicators
  re
) # RE intercept 4 clusters

# Prep data:
# Prep data for missing indicator analysis
missing_indicator_data = dplyr::mutate(
  data,
  ethnicity_agg = ifelse(
    is.na(ethnicity_agg), 'Missing', ethnicity_agg))

### Fit model -----------------------------------------------------------

ph1_m_list = lapply(
  c('data', 'missing_indicator_data'), 
  function(dataset){
    
    df = get(dataset)
    
    lme4::glmer(
      as.formula(formula), 
      data = df,
      family = binomial)
    
  })

# Set standard names to keep track of which model is which
# Names will be used to provide summaries & save outputs 
# with a tag indicating which model the estimates are from
names(ph1_m_list) = c('complete_case', 'missing_indicator')
names_ph1_m = names(ph1_m_list)

### Post-fit pipeline ----

# Tidy output
#2 Tidy model estimates 
tidy_ph1_list <- lapply(
  setNames(names_ph1_m, names_ph1_m),
  function(names_index) {
    
    tidy_m1 = broom.mixed::tidy(
      ph1_m_list[[names_index]], conf.int=TRUE, 
      exponentiate=TRUE,
      #effects=c("fixed", "ran_pars")
      effects=c("fixed"))
    
    tidy_m1 = tidy_m1 %>%
      dplyr::mutate(
        date = date,
        across(where(is.numeric), round,4),
        analysis_type = names_index,
        formula = formula,
        #icc = m1_icc[[names_index]]
      ) %>%
      dplyr::relocate(analysis_type, formula) 
    
  })

names(tidy_ph1_list)
ph1_tidy_table = do.call(bind_rows, tidy_ph1_list)
View(ph1_tidy_table)

### Save outputs --------------------------------------------------------
setwd(data_path)

output_file = 'tidy_output_list.xlsx'
table_to_append = ph1_tidy_table

output_tb = readxl::read_excel(
  paste0(#"model_outputs/", 
    output_file))

output_tb = output_tb %>%
  dplyr::relocate(date) %>%
  dplyr::mutate(across(.cols = c(6:16),
                       .fns = ~ as.numeric(.x)))

output_tb = dplyr::bind_rows(
  output_tb, table_to_append)

writexl::write_xlsx(
  output_tb,
  paste0(data_path,
         'model_outputs/',
         output_file))

## PH2: Seasonal effects -------------------------------------------------------

### PH2.1: Demand on CSC -------------------------------------------------------

#### Data --------------------------------------------------------
#### Formula --------------------------------------------------------
#### Fit model --------------------------------------------------------
#### Save outputs --------------------------------------------------------


### PH2.2: COVID-19 effects ----------------------------------------------------

#### Data --------------------------------------------------------
#### Formula --------------------------------------------------------
#### Fit model --------------------------------------------------------
#### Save outputs --------------------------------------------------------

