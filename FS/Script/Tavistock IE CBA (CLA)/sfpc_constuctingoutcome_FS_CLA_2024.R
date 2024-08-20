################################################################################
#
#                Constructing primary outcome variable (CLA) 2024 
#                     From merged dataset DR2 and DR3 
#                               SFPC 
#                      Family Safeguarding Model 
#                       Tavistock Analysis
#                        Emily Walker 2024
#
################################################################################

setwd('C:/Users/EmilyWalker/Foundations/High-SFPC-Impact - Working folder/sfpc_familysafeguarding_cleaning')

load("Output/cla_merge_dr2_dr3.RData")

# Constructing the primary outcome variable 
# Whether or not the child has become looked after
# measure: Coded 1 if the child has become looked after at any
# point within 18 months of the referral. Coded 0 if the
# child has not become looked after within this period.

cla_merge$primary_outcome <- ifelse(
  is.na(cla_merge$`cla date`), 
  0,
  ifelse(
    cla_merge$`cla date` <= cla_merge$`ref date 18months`,
    1,
    0
  )
)

table(cla_merge$primary_outcome)
#  0          1 
# 8606       790 


# Treatment condition:
################################################################################
# COPY AND PASTED FROM PREVIOUS SCRIPT - NOT READY YET 
################################################################################

# Create treatment/control variable ----

# Dates----
# Trial period began: March 2020
# Trial period ended: November 2022
# (subset to only include referal dates within this time period)

# Lanc go live: 01/02/2021 (1 Feb 2021)
# Swind go live: 24/05/2022 (24 May 2022)
# Telf go live: 28/06/2021 (28 June 2021)
# Wands go live: 24/01/22 (24 jan 2022)
# Walsall go live: 01/09/2020 (1 sept 2020)

# ASSIGNMENT INFO ----
# CONTROL: Children whose first referral in the trial period took place when
# the local authority was running their business as usual model.

# TREATMENT: Children whose first referral in the trial period took place when the
# local authority was running the Family Safeguarding model.

# randomise the order in which local authorities implement the programme, 
#in six month intervals, rather than which local authority implements the model.

# Create a dataframe of go-live dates----

# Define variables----
la <- c("Lancashire", "Swindon", "Telford", "Walsall", "Wandsworth")
go.live <- as.Date(c("2021-02-01", "2022-05-24", "2021-06-28", "2020-09-01", "2022-01-24"))

go_live <- data.frame(la, go.live)

print(go_live)

# Merging go live dates on to the main dataframe ----
cla_merge <- merge(cla_merge, go_live, by = c("la"), all.x = TRUE)


# Trial period began: March 2020
# Trial period ended: November 2022

# Creating the treatment/control variable----
# CLA
cla_merge$treatment <- ifelse(cla_merge$`ref date1` > cla_merge$`go.live`, 1, 0)

# Check the proportion of treatment and control 
cla_merge %>%
  select(`treatment`)%>%
  table(useNA = "ifany") %>%
  addmargins

#   0    1    <NA>   Sum 
# 4755  4551   90    9396 


# How many children in the treatment entered care and how many in the control 

proportions <- tapply(cla_merge$primary_outcome, cla_merge$treatment, mean)
print(proportions)
#0          1 
#0.06435331 0.10635025 

CLA_table <- table(Treatment = cla_merge$treatment, `Entered care` = cla_merge$primary_outcome)
# Calculate percentage of entering care by treatment group
percent_table <- prop.table(CLA_table, 1) * 100
print(percent_table)
# Extract the percentage for those who entered care (entering care = 1)
care_percentage <- percent_table[, 2]


colnames(cla_merge)
