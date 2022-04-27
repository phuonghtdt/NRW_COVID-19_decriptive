#################
#(8) vaccinations in NRW
#################
library(reshape2, ggplot2)
library('ggplot2')
library('scales')
library(tidyverse)



path_data <- paste0("C:/Users/huynh/sciebo/NRW_descriptive/data")

######################
#Import data
#####################
# Bundesland level
vacc_BUND<- read_csv(paste0(path_data,"/Aktuell_Deutschland_Bundeslaender_COVID-19-Impfungen.csv"))
vacc_BUND <- vacc_BUND %>% filter(BundeslandId_Impfort=="05")
colnames(vacc_BUND) <- c("rep_date",'state_code',"vac_type",'dose',"vac_n")
# County (lankreis) level
vacc_LK<- read_csv(paste0(path_data,"/Aktuell_Deutschland_Landkreise_COVID-19-Impfungen.csv"))
vacc_LK<- vacc_LK%>% filter(str_sub(LandkreisId_Impfort,1,2)=='05') # or is code of NRW
colnames(vacc_LK) <- c("rep_date",'county_code',"age group",'dose',"vac_n")



###################
#clean data
###################
###Bundeslank level
vacc_BUND$vac_type <- as.factor(vacc_BUND$vac_type)
vacc_BUND$dose <- as.factor(vacc_BUND$dose)
levels(vacc_BUND$dose) <- c("1", "2","booster") # label dose 3 as booster
vacc_BUND$rep_date <- as.Date(vacc_BUND$rep_date)
vacc_BUND$maxvacdate <- max(vacc_BUND$rep_date)
vacc_BUND$vac_week <- as.integer((as.Date(vacc_BUND$rep_date)-as.Date("2020-01-01"))/7+1)

#revert week to date: . ##Wed (1/1/2020), Wed is the start of a week
vacc_BUND$vac_week_date <- lubridate::ymd( "2020-01-01" ) + lubridate::weeks(vacc_BUND$vac_week-1)# start at week 1 of the year

# County (lankreis) level
vacc_LK$dose <- as.factor(vacc_LK$dose)
levels(vacc_LK$dose) <- c("1", "2","booster") # label dose 3 as booster
vacc_LK$rep_date <- as.Date(vacc_LK$rep_date)
vacc_LK$maxvacdate <- max(vacc_LK$rep_date)
vacc_LK$vac_week <- as.integer((as.Date(vacc_LK$rep_date)-as.Date("2020-01-01"))/7+1)
#revert week to date: . ##Wed (1/1/2020), Wed is the start of a week
vacc_LK$vac_week_date <- lubridate::ymd( "2020-01-01" ) + lubridate::weeks(vacc_LK$vac_week-1)# start at week 1 of the year



