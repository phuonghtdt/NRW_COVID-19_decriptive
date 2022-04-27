#################
#(1) COVID-19_RKI
#################
library(reshape2, ggplot2)
library('ggplot2')
library('scales')
library(tidyverse)



path_data <- paste0("C:/Users/huynh/sciebo/NRW_descriptive/data")
path_data_code <- paste0("C:/Users/huynh/sciebo/NRW_descriptive/gitHub/NRW_COVID-19_decriptive/codes/data_codes")
######################
#Import data
#####################

# download.file("https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/main/data-truth/JHU/truth_JHU-Incident%20Cases.csv", destfile = paste0(path_data,"/truth_JHU-Incident_Cases.csv")) # download data from Github## check update version
germany <- read.csv(paste0(path_data,"/truth_JHU-Incident_Cases.csv"))

###################
#clean data
###################
germany <- germany%>%filter(location=="DE") #Germany only
germany$value[germany$value == -554] <-(17248+11315)/2 # replace negative value=ave of 2 nearby days
germany <- germany[,-(1:2)]                            # remove location and location_name
names(germany) <- c("rep_date", "case_n")                     #rename
germany$rep_date <- as.Date(germany$rep_date)          #date format
germany<- germany %>%
  mutate(avg_inf = rollapply(case_n,7,function(x) sum(x)/7,fill=NA, align="right") )#Ave. 7days ago



