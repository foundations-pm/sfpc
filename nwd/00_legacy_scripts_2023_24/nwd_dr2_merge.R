
# SFPC impact evaluation (NWD)
# Data cleaning DR2 Ref

# Author: AA
# Date: 26/07/2023
# Edited DR 30/08/2023 to update file paths

rm(list = ls())

#install.packages("tidyverse")
library(tidyverse)
# install.packages("readxlsb")
library(readxl)
# install.packages("tibble")
library(tibble)
# install.packages("dplyr")
library(dplyr)
#install.packages("psych")
library(psych)
#install.packages("plyr")
library(plyr)
library(writexl)
#install.packages("writexl")
library(openxlsx)
#install.packages('openxlsx')
library(naniar)
#install.packages('naniar')

# Read in DR2 files ####

load ("Output/alldr2cla.RData")

load ("Output/alldr2cp.RData")

load ("Output/alldr2ref.RData")


# merge all dr2 data

all_dr2 <- rbind.fill(all_dr2cla,all_dr2cp,all_dr2ref)

# save binded dr2 dataset

save(all_dr2, file = "Output/alldr2.RData")

#DR2 Missing data

# Missing DR2 data 

#Percent of rows with any value missing
pct_miss_case(all_dr2)  #100%

# Visual to see missingness
gg_miss_var(all_dr2, show_pct = TRUE)

# Visual to see missingness by LA
all_dr2 %>% 
  gg_miss_var(show_pct = TRUE, facet = `LA`)
ggsave("Output/alldr2missing.png", width = 8, height = 6, units = "in")


