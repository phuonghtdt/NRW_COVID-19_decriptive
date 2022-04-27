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

#(1.1) rki data:https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/a99afefd4258435f8af660b6cbed9bf7_0/about
dat_rki  <- read.csv(paste0(path_data,"/RKI_COVID19_Nordrhein-Westfalen.csv"), encoding="UTF-8")# encoding="UTF-8" : umlaut character

dat_rki <- dat_rki%>%
  dplyr::select(Landkreis,Geschlecht,Altersgruppe,Meldedatum,Refdatum,IstErkrankungsbeginn,NeuerFall,NeuerTodesfall,NeuGenesen,AnzahlFall,AnzahlTodesfall,AnzahlGenesen)
colnames(dat_rki) <- c("kreis","gender","agegroup","rep_date","ref_date","disease_onset","newcase","newdeath","newrecover","case","death","recover")
dat_rki$rep_date <- as.Date(dat_rki$rep_date)
# dat_rki <- dat_rki %>% filter(rep_date <= as.Date("2021-12-29")) # the same as dat
dat_rki$rep_week <- as.integer((as.Date(dat_rki$rep_date)-as.Date("2020-01-01"))/7+1) #wednessday starts a week
dat_rki$maxrepweek <- max(dat_rki$rep_week) 
# remove rep week 8 because of insufficient data 
dat_rki <- dat_rki %>% filter(rep_week >=10)# data from rki is avaible from week 1 and become more consistent since week 9
#revert week to date: 
#check which day is starting of a week : Wednesday
dat_rki$rep_week_date <- lubridate::ymd( "2020-01-01" ) + lubridate::weeks(dat_rki$rep_week
                                                                           -1)
dat_rki$rep_week_date <- as.Date(dat_rki$rep_week_date)

#0: Case is included in the publication for the current day and in the one for the previous day.
#1: case is only included in the current publication
#-1: case is only contained in the publication of the previous day
# why the number in 2022 is all negative? shall it be corrected later.

###################
#clean data
###################




