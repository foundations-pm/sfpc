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
#0          1 
#21,117    895 

# TO DO: ENSURE THAT FILTERING IS COMPLETE IN THE OTHER SCRIPTS i.e.: -----
# SUBSET: ----
#0-12 at the time of referral 
#who have been referred within the trial period
#and whose initial assessment identified parental substance misuse, domestic violence, or parental mental health as factors identified at the end of assessment. 
#Since these factors are only identified at assessment, our sample is restricted to children whose referral has progressed to an assessment 
#and where one of the factors identified at assessment includes one of the three factors defined above.

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
cla <- merge(test_merge_cla, go_live, by = c("la"), all.x = TRUE)

cpp <- merge(test_merge_cpp, go_live, by = c("la"), all.x = TRUE)

proc <- merge(test_merge_proc, go_live, by = c("la"), all.x = TRUE)

scl <- merge(test_merge_scl, go_live, by = c("la"), all.x = TRUE)

# Trial period began: March 2020
# Trial period ended?: November 2022

# Creating the treatment/control variable----
# CLA
cla$treatment <- ifelse(cla$`ref date` > cla$`go.live`, 1, 0)

# CPP 
cpp$treatment <- ifelse(cpp$`ref date` > cpp$`go.live`, 1, 0)

# PROC
proc$treatment <- ifelse(proc$`ref date` > proc$`go.live`, 1, 0)

# SCL
scl$treatment <- ifelse(scl$`ref date` > scl$`go.live`, 1, 0)


# Check the proportion of treatment and control 
cla %>%
  select(`treatment`)%>%
  table(useNA = "ifany") %>%
  addmargins