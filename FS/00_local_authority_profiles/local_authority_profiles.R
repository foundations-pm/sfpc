#------------------------------------------
#
# Family Safeguarding - LA profiles
#
#------------------------------------------

# Set-up  ----
r_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(r_directory,'Foundations/High-SFPC-Impact - Family Safeguarding')

# Data and output paths
data_path = paste0(sharepoint_path, '/Data')
output_path = paste0(sharepoint_path, '/Outputs/Local authority profiles')

# Working directory
wd = paste0(r_directory, "Documents/SFPC/FS/")

# Libraries 
{ source(paste0(r_directory, "Documents/SFPC/config.R")) }

# Functions 
{ source(paste0(r_directory, "Documents/SFPC/functions.R"))}


# Read data ---

# DfE SSDA904 build your own tables 
# https://explore-education-statistics.service.gov.uk/data-tables/children-looked-after-in-england-including-adoptions/2024?subjectId=dc0d45df-d9e5-4d83-73ce-08dcfa65edf3

cla_data = utils::read.csv(paste0(data_path, '/dfe_cla_data_2024.csv'))

# Plot data --- 

# CLA at 31st March between 2020 and 2024
cla_at_31st_march_plot = cla_data %>%
  dplyr::filter(
    population_count == 'Children looked after at 31 March each year') %>%
  ggplot2::ggplot(.) +
  ggplot2::geom_point(
    aes(x = time_period, y = rate_per_10000, colour = la_name)) +
  ggplot2::geom_line(
    aes(x = time_period, y = rate_per_10000, colour = la_name)) +
  labs(
    title = paste0(
      "Children Looked After (CLA) at 31st March"),
    subtitle = paste0(
      'Strengthening Families, Protecting Children: Family Safeguarding Evaluation'),
    x = "Year",
    y = "Rate per 10,000",
    colour = "Local authority") +
  ggplot2::scale_color_manual(
    values = c(
      '#007069', '#464C8B', '#708CCC', '#E89633', '#E86E42')) +
  ggplot2::theme_minimal()

# Save plot
setwd(output_path)
ggplot2::ggsave('cla_at_31st_march_plot.jpg')

# Children ceasing to be looked after each year
cla_ceasing_plot = cla_data %>%
  dplyr::filter(
    population_count == 'Children ceasing to be looked after each year') %>%
  ggplot2::ggplot(.) +
  ggplot2::geom_point(
    aes(x = time_period, y = rate_per_10000, colour = la_name)) +
  ggplot2::geom_line(
    aes(x = time_period, y = rate_per_10000, colour = la_name)) +
  labs(
    title = paste0(
      "Children ceasing to be looked after each year"),
    subtitle = paste0(
      'Strengthening Families, Protecting Children: Family Safeguarding Evaluation'),
    x = "Year",
    y = "Rate per 10,000",
    colour = "Local authority") +
  ggplot2::scale_color_manual(
    values = c(
      '#007069', '#464C8B', '#708CCC', '#E89633', '#E86E42')) +
  ggplot2::theme_minimal()

# Save plot
setwd(output_path)
ggplot2::ggsave('cla_ceasing_plot.jpg')

# Children starting to be looked after each year
cla_starting_plot = cla_data %>%
  dplyr::filter(
    population_count == 'Children starting to be looked after each year') %>%
  ggplot2::ggplot(.) +
  ggplot2::geom_point(
    aes(x = time_period, y = rate_per_10000, colour = la_name)) +
  ggplot2::geom_line(
    aes(x = time_period, y = rate_per_10000, colour = la_name)) +
  labs(
    title = paste0(
      "Children starting to be looked after each year"),
    subtitle = paste0(
      'Strengthening Families, Protecting Children: Family Safeguarding Evaluation'),
    x = "Year",
    y = "Rate per 10,000",
    colour = "Local authority") +
  ggplot2::scale_color_manual(
    values = c(
      '#007069', '#464C8B', '#708CCC', '#E89633', '#E86E42')) +
  ggplot2::theme_minimal()

# Save plot
setwd(output_path)
ggplot2::ggsave('cla_starting_plot.jpg')


