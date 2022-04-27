#################
#(6) Deprivation upto 2015
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

source(paste0(path_data_code,"/NUTS3.R"))

# download.file("https://raw.githubusercontent.com/lekroll/GISD/master/Revisions/2019/Bund/Kreis/Kreis.csv", destfile = paste0(path_data,"/GISD/GISD_upto2015.csv")) # download data from Github## check update version
gisd <- read.csv(paste0(path_data,"/GISD/GISD_upto2015.csv"), sep = ",",row.names = 1)
colnames(gisd) <- c("county_id","county","year","pop","GISD_Score","GISD_5","GISD_10","GISD_k") # the pop from stat will be use -> rm this
gisd <- gisd %>%dplyr::select(-c(pop)) # rm pop
gisd <- gisd %>%dplyr::filter(year == 2015) # the most recent data
gisd$county <- gsub(",.*", "", gisd$county) #rm string after comma
gisd <- merge(nuts3,gisd, by = "county")    # add NUTS info
gisd$pop.dens <- gisd$pop_n/gisd$area_sq_km
# gisd$GISD_5 <- as.factor(gisd$GISD_5)


###################
#clean data
###################




