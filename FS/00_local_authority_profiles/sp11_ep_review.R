#------------------------------------------
#
# Family Safeguarding - LA profiles
#
#------------------------------------------

# Set-up  ----
r_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(r_directory,'Foundations/High-SFPC-Impact - Family Safeguarding')

# Data and output paths
data_path = paste0(sharepoint_path, '/Outputs/Local authority profiles/SP11 Review November 2025')
output_path = data_path

# Working directory
wd = paste0(r_directory, "Documents/SFPC/FS/")

# Libraries 
{ source(paste0(r_directory, "Documents/SFPC/FS/config.R")) }

# Functions 
{ source(paste0(r_directory, "Documents/SFPC/FS/functions.R"))}

# Read data ---

# DfE SSDA904 build your own tables 
# https://explore-education-statistics.service.gov.uk/data-tables/children-looked-after-in-england-including-adoptions/2024?subjectId=dc0d45df-d9e5-4d83-73ce-08dcfa65edf3

referral_data = utils::read.csv(paste0(data_path, '/data-children-in-need.csv'))

# Plot data --- 

# Add cohort identifier
referral_data = dplyr::mutate(
  referral_data,
  cohort = case_when(
    la_name == 'Hertfordshire' ~ 'cohort_1',
    la_name %in% c('Peterborough', 'Bracknell Forest', 'West Berkshire') ~ 'cohort_2',
    TRUE ~ 'cohort_3')
)

# Cohort time frames
cohort_1_time_period = c(
  "2015", "2016", "2017", "2018", "2019",
  "2020", "2021", "2022", "2023", "2024", "2025")

cohort_2_time_period = cohort_1_time_period[4:length(cohort_1_time_period)]

cohort_3_time_period = cohort_2_time_period[3:length(cohort_2_time_period)]

# Average nb of children referred annually in each cohort since implementation 
# Avg for cohort 1, just Herts 
referral_data %>%
  #dplyr::filter(time_period %in% cohort_1_time_period) %>%
  dplyr::filter(la_name == 'Hertfordshire') %>%
  dplyr::summarise(avg_children_referred = mean(referral_child_count))

# Avg for Cohort 2, since 2018 (West BErskhire, Peterborough and Bracknell Forest)
referral_data %>%
  dplyr::filter(time_period %in% cohort_2_time_period) %>%
  dplyr::filter(cohort == 'cohort_2') %>%
  dplyr::summarise(avg_children_referred = mean(referral_child_count))

# Avg for Cohort 3, since 2018 (West BErskhire, Peterborough and Bracknell Forest)
referral_data %>%
  dplyr::filter(time_period %in% cohort_3_time_period) %>%
  dplyr::filter(cohort == 'cohort_3') %>%
  dplyr::summarise(avg_children_referred = mean(referral_child_count))

# Compute avg per cohort per year
avg_referred_cohort_data <- referral_data %>% 
  dplyr::group_by(time_period, cohort) %>% 
  dplyr::summarise(
    avg_referred = mean(referral_child_count))

# Nb of children referred to CSC since 2013
children_referred_to_csc_plot = avg_referred_cohort_data %>%
  ggplot2::ggplot(.) +
  ggplot2::geom_point(
    aes(x = time_period, y = avg_referred, colour = cohort)) +
  ggplot2::geom_line(
    aes(x = time_period, y = avg_referred, colour = cohort)) +
  labs(
    title = paste0(
      "Number of children with a referral in the year"),
    subtitle = paste0(
      'Family Safeguarding: ETF-Funded Evaluation'),
    x = "Year",
    y = "Number",
    colour = "Local authority") +
  scale_x_date(date_labels = "%Y")   +
  ggplot2::scale_color_manual(
    values = c(
      '#007069', #'#464C8B', 
      '#708CCC', '#E89633'
      #, '#E86E42'
      )) +
  ggplot2::theme_minimal()

# Children referred to CSC since 2015: rate per 10,000


