#################
#(7) Regulations
#################
library(reshape2, ggplot2)
library('ggplot2')
library('scales')
library(tidyverse)



path_data <- paste0("C:/Users/huynh/sciebo/NRW_descriptive/data")

######################
#Import data
#####################

## Bundesland level
reg_bund <- read.csv(paste0(path_data,"/bl_massnahmen_oberkategorien.csv"),row.names = 1, encoding="UTF-8")# encoding="UTF-8" : umlaut character)
reg_bund <- reg_bund %>% filter(bundesland == "Nordrhein-Westfalen")
reg_bund <- reg_bund %>% dplyr::select(-c(ags2, bundesland))

reg_bund <- pivot_longer(reg_bund, cols=2:dim(reg_bund)[2], names_to = "date", values_to = "measure_imp") # transpose from cols to rows
reg_bund$date <- gsub("^.{0,1}", "", reg_bund$date)# Replace first character () with empty string ""
reg_bund$date <- as.Date(reg_bund$date , format="%Y%m%d")
reg_bund$measure_imp <- as.factor(reg_bund$measure_imp)


## Landkreis level
reg_lk <- read.csv(paste0(path_data,"/kr_massnahmen_oberkategorien.csv"),row.names = 1, encoding="UTF-8")# encoding="UTF-8" : umlaut character)
reg_lk <- reg_lk %>% filter(bundesland == "Nordrhein-Westfalen")
reg_lk <- reg_lk %>% filter(m_code != "M22"&m_code != "M23"&m_code != "M24")# these measures were not applied

###################
#clean data
###################




