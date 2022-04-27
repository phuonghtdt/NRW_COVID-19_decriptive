
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
dat = read.csv(paste0(path_data,"/211229_Fall-Liste.csv"), sep=";", encoding="UTF-8")# encoding="UTF-8" : umlaut character
colnames(dat)
dat <- dat %>% dplyr::select(-contains("InterneRef")) #rm
names(dat) <- c("rep_date","county", "disease_start", "age",'hosp')   #rename
dat$hosp <- as.factor(dat$hosp)
###################
#clean data
###################
dat$county[dat$county == "StadtRegion Aachen"] <- "SK StadtRegion Aachen" #change to the same format with other counties
dat$county <- gsub("^.{0,3}", "", dat$county)# Replace first 3 characters (SK ) with empty string ""
dat$rep_date<- as.Date(dat$rep_date, "%Y-%m-%d")
dat$disease_start<- as.Date(dat$disease_start, "%Y-%m-%d")
dat$county <- as.factor(dat$county)
dat$rep_week <- as.integer((as.Date(dat$rep_date)-as.Date("2020-01-01"))/7+1) #wednessday starts a week
dat$maxrepweek <- max(dat$rep_week) 

#revert week to date: 
#check which day is starting of a week : Wednesday
dat$rep_week_date <- lubridate::ymd( "2020-01-01" ) + lubridate::weeks(dat$rep_week-1)
dat$rep_week_date <- as.Date(dat$rep_week_date)
# age group:19
dat$agegroup<- dat$age
dat$agegroup <- cut(dat$agegroup, breaks=c(-Inf, 4, 9,14, 19,24,29,34,39,44,49,54,59,64,69,74,79,84,89, Inf))
dat$agegroup <- factor(dat$agegroup, levels=c("(-Inf,4]", "(4,9]", "(9,14]", "(14,19]", "(19,24]", "(24,29]", "(29,34]", "(34,39]", "(39,44]", "(44,49]", "(49,54]", "(54,59]", "(59,64]", "(64,69]", "(69,74]", "(74,79]", "(79,84]", "(84,89]",  "(89, Inf]"),
                       labels=c("0-4","5-9","10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
                                "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
                                "75-79", "80-84", "85-89", ">90"), ordered=T)
# agegroup2: 6
dat$agegroup2<- dat$age
dat$agegroup2 <- cut(dat$agegroup2, breaks=c(-Inf,4,14,34,59,79, Inf))
dat$agegroup2 <- factor(dat$agegroup2, levels=c("(-Inf,4]", "(4,14]", "(14,34]", "(34,59]", "(59,79]",  "(79, Inf]"),
                        labels=c("0-4","5-14","15-34", "35-59", "60-79",">80"), ordered=T)


# These two counties:  "M??lheim an der Ruhr", "St??dteregion Aachen" (nuts3) is different from dat ("M??lheim a.d.Ruhr", "StadtRegion Aachen" )
##rename it to be inline with other dataset

dat$county <- gsub("M??lheim a.d.Ruhr", "M??lheim an der Ruhr", dat$county)
dat$county <- gsub("StadtRegion Aachen", "St??dteregion Aachen", dat$county)

# remove rep week 8 because of insufficient data 
dat <- dat %>% filter(rep_week >=10)#




