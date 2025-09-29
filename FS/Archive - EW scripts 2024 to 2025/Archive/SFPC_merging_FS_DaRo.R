########INFO########################
# SFPC link Family Safeguarding data ##
# David Rodriguez
# last updated 1.12.2022

#load packages
library("tidyverse")
library("readxl")
library("readxlsb")
#swindon uses xlsb 
#install.packages("readxlsb") #needs to be installed

# DR1 ----------

#list file paths for DR1
filenamesDR1 <- sort(list.files(path="Data/FS_DR1")) 
filenamesDR1 <- filenamesDR1[-1] #dropping 'archive' from the list
filenamesDR1
# read-in one
library(readxl)
lancashire_dr1_nov20 <- read_excel("Data/FS_DR1/lancashire_dr1_nov20.xlsx", 
                                     sheet = "DR1 - Data - aggregate level", 
                                     col_names = TRUE,
                                     trim_ws = TRUE,
                                     skip = 1)

#make it a loop
# for (i in 1:5) { 
#   list[[i]] <- 
#     read_excel(paste("Data/FS_DR1/", filenamesDR1[i]), 
#                     sheet = "DR1 - Data - aggregate level", 
#                     col_names = TRUE,
#                     trim_ws = TRUE,
#                     skip = 1)              
# }

# [] is the number of element in the list 
filenamesDR1[4] 

#loop take 2 
for (i in filenamesDR1){
  l[i] <- 
  read_excel(paste("Data/FS_DR1/", filenamesDR1[i]),
                    sheet = "DR1 - Data - aggregate level",
                    col_names = TRUE,
                    trim_ws = TRUE,
                    skip = 1)
}

#loop take 3 
for (file_name in filenamesDR1) {
  extension <- tools::file_ext(file_name)
  if (extension == "xls"| extension == "xlsx"){
    file_name <- read_excel(paste("Data/FS_DR1/", file_name, sep = ""),
                            sheet = "DR1 - Data - aggregate level",
                            col_names = TRUE,
                            trim_ws = TRUE,
                            skip = 1)
  }
  if (extension == "xlsb") next 
}  

#loop take 4
for (i in 1:length(filenamesDR1)) {
    data[i] <- read_excel(paste("Data/FS_DR1/", filenamesDR1[i], sep = ""),
                            sheet = "DR1 - Data - aggregate level",
                            col_names = TRUE,
                            trim_ws = TRUE,
                            skip = 1)
  }




# one LA at a time 
filenamesDR1_lancashire <- sort(list.files(path="Data/FS_DR1", pattern = "lancashire")) 
   