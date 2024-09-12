
# Script to organise Child IDs from DR2 to compile data request for DR3 #

setwd('C:/Users/EmilyWalker/Foundations/High-SFPC-Impact - Working folder/sfpc_familysafeguarding_cleaning')

#########################################################################


# Saving DR2 dataframes as Excels to copy the Child IDs into the latest DR3 return

# Lancashire
write_xlsx(bind_lancs_dr2,"Output/DR2 Excel/dr2_lancs.xlsx")
# 24,017 observations

# Checking whether this matches up with the individual files 
# Create a list of DataFrames
lanc_list <- list(lancs_dr2_nov20, lancs_dr2_nov21, 
                  lancs_dr2_nov22, lancs_dr2_apr23)
# Get the number of rows in each DataFrame
row_counts <- sapply(lanc_list, nrow)
# Print the row counts
print(row_counts)
# 9592 6881 6996  548
9592 + 6881 + 6996 + 548 # = 24017

# Filtering the dates so that they are from 01 March 2020 - 30 November 2022
start_date <- as.Date("2020-03-01")
end_date <- as.Date("2022-11-30")

# Filter the DataFrame by the date range
filtered_lancs <- bind_lancs_dr2[bind_lancs_dr2$`Referral Date` >= start_date & bind_lancs_dr2$`Referral Date` <= end_date, ]

# Saving Excel with the correct dates. 
write_xlsx(filtered_lancs,"Output/DR2 Excel/dr2_lancs_dates.xlsx")

##########################################################

# Swindon
write_xlsx(bind_swind_dr2,"Output/DR2 Excel/dr2_swind.xlsx")
# 10,892 observations
# Checking whether this matches up with the individual files 
# Create a list of DataFrames
swind_list <- list(swind_dr2_nov20, swind_dr2_nov21, 
                   swind_dr2_nov22, swind_dr2_apr23)
# Get the number of rows in each DataFrame
row_counts <- sapply(swind_list, nrow)
# Print the row counts
print(row_counts)
# 2559 4086 3216 1031
2559 + 4086 + 3216 + 1031 # 10892

# Filtering the dates so that they are from 01 March 2020 - 30 November 2022
start_date <- as.Date("2020-03-01")
end_date <- as.Date("2022-11-30")

# Filter the DataFrame by the date range
filtered_swind <- bind_swind_dr2[bind_swind_dr2$`Referral Date` >= start_date & bind_swind_dr2$`Referral Date` <= end_date, ]

# Saving Excel with the correct dates. 
write_xlsx(filtered_swind,"Output/DR2 Excel/dr2_swind_dates.xlsx")
###########################################################

# Telford 
write_xlsx(bind_telford_dr2,"Output/DR2 Excel/dr2_telford.xlsx")
# 5,605 observations
# Checking whether this matches up with the individual files 
# Create a list of DataFrames
telf_list <- list(telford_dr2_nov20, telford_dr2_nov21, 
                  telford_dr2_nov22, telford_dr2_apr23)
# Get the number of rows in each DataFrame
row_counts <- sapply(telf_list, nrow)
# Print the row counts
print(row_counts)
# 1522 1841 2109  133
1522 + 1841 + 2109 + 133 #5605

# Filtering the dates so that they are from 01 March 2020 - 30 November 2022
start_date <- as.Date("2020-03-01")
end_date <- as.Date("2022-11-30")

# Filter the DataFrame by the date range
filtered_telford <- bind_telford_dr2[bind_telford_dr2$`Referral Date` >= start_date & bind_telford_dr2$`Referral Date` <= end_date, ]

# Saving Excel with the correct dates. 
write_xlsx(filtered_telford,"Output/DR2 Excel/dr2_telford_dates.xlsx")
################################################################################

# Walsall 
write_xlsx(bind_walsall_dr2,"Output/DR2 Excel/dr2_walsall.xlsx")
# 11,303 observations
# Checking whether this matches up with the individual files 
# Create a list of DataFrames
walsall_list <- list(walsall_dr2_nov20, walsall_dr2_nov21, 
                     walsall_dr2_nov22, walsall_dr2_apr23)
# Get the number of rows in each DataFrame
row_counts <- sapply(walsall_list, nrow)
# Print the row counts
print(row_counts)
# 3098 3830 4026  349
3098 + 3830 + 4026 + 349 #11303

# Filtering the dates so that they are from 01 March 2020 - 30 November 2022
start_date <- as.Date("2020-03-01")
end_date <- as.Date("2022-11-30")

# Filter the DataFrame by the date range
filtered_walsall <- bind_walsall_dr2[bind_walsall_dr2$`Referral Date` >= start_date & bind_walsall_dr2$`Referral Date` <= end_date, ]

# Saving Excel with the correct dates. 
write_xlsx(filtered_walsall,"Output/DR2 Excel/dr2_walsall_dates.xlsx")

###############################################################################

# Wandsworth
write_xlsx(bind_wands_dr2,"Output/DR2 Excel/dr2_wands.xlsx")
# 12,192  observations
# Checking whether this matches up with the individual files 
# Create a list of DataFrames
wands_list <- list(wands_dr2_nov20, wands_dr2_nov21, 
                   wands_dr2_nov22, wands_dr2_apr23)
# Get the number of rows in each DataFrame
row_counts <- sapply(wands_list, nrow)
# Print the row counts
print(row_counts)
#3224 3104 5660  204
3224 + 3104 + 5660 + 204 #12192

# Filtering the dates so that they are from 01 March 2020 - 30 November 2022
start_date <- as.Date("2020-03-01")
end_date <- as.Date("2022-11-30")

# Filter the DataFrame by the date range
filtered_wands <- bind_wands_dr2[bind_wands_dr2$`Referral Date` >= start_date & bind_wands_dr2$`Referral Date` <= end_date, ]

# Saving Excel with the correct dates. 
write_xlsx(filtered_wands,"Output/DR2 Excel/dr2_wands_dates.xlsx")

------------------------------------------------------------------------------
  
# Checking totals in the filtered files 

# Checking whether this matches up with the individual files 
# Create a list of DataFrames
filtered_list <- list(filtered_lancs, filtered_swind, filtered_telford,
                      filtered_walsall, filtered_wands)
# Get the number of rows in each DataFrame
row_counts <- sapply(filtered_list, nrow)
# Print the row counts
print(row_counts)
#Lancs: 19702  
#Swind: 8942  
#Telford: 4761  
#Walsall: 9774 
#Wandsworth: 10333

------------------------------------------------------------------------------
  
# Deleting duplicate rows
# LANC
unique_IDs_lanc <- filtered_lancs %>%
  distinct(`Child ID`, .keep_all = TRUE)

# How many rows
nrow(unique_IDs_lanc) #15127

# SWIND
unique_IDs_swind <- filtered_swind %>%
  distinct(`Child ID`, .keep_all = TRUE)
# How many rows
nrow(unique_IDs_swind) #6019

# TELF
unique_IDs_telf <- filtered_telford %>%
  distinct(`Child ID`, .keep_all = TRUE)
# How many rows
nrow(unique_IDs_telf) #3814

# WALSALL
unique_IDs_walsall <- filtered_walsall %>%
  distinct(`Child ID`, .keep_all = TRUE)
# How many rows
nrow(unique_IDs_walsall) #7251

# WANDSWORTH
unique_IDs_wands <- filtered_wands %>%
  distinct(`Child ID`, .keep_all = TRUE)
# How many rows
nrow(unique_IDs_wands) #5602
