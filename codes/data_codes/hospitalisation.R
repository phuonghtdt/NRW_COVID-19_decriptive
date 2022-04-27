#################
#line list of COVID-19 in NRW
#################
library(reshape2, ggplot2)
library('ggplot2')
library('scales')
library(tidyverse)



path_data <- paste0("C:/Users/huynh/sciebo/NRW_descriptive/data")

######################
#Import data
#####################

hosp <- read_excel(paste0(path_data,"/211229 NRW dateninput modellierungen wwu.xlsx"), sheet = 2)

###################
#clean data
###################


colnames(hosp)
colnames(hosp) <- c("rep_date","total","normal","intensive","ventilation")

hosp$rep_date<- as.Date(hosp$rep_date)
hosp$rep_week <- as.integer((as.Date(hosp$rep_date)-as.Date("2020-01-01"))/7+1)
hosp$maxrepweek <- max(hosp$rep_week)

#revert week (number) to start hospe of a week: 1/1/2020 is on Wednesday
## So Wednesday is start of a week
hosp$rep_week_date <- lubridate::ymd( "2020-01-01" ) + lubridate::weeks(hosp$rep_week-1)# indicated date starts week 2 of the year
hosp$rep_week_date <- as.Date(hosp$rep_week_date)

