#################
#(2) age distribution in NRW
#################
library(reshape2, ggplot2)
library('ggplot2')
library('scales')
library(tidyverse)



path_data <- paste0("C:/Users/huynh/sciebo/NRW_descriptive/data")

######################
#Import data
#####################
pop <- read.csv(paste0(path_data,"/NRW_cencus/NRW_pop_bycounty_NUTS_ID.csv"),row.names = 1, encoding="UTF-8")
pop <- pop%>% select(c("pop_n", "NUTS_ID")) #county name has error ->rm it
## age: 90 mean 90 and older
pop_age = read.csv(paste0(path_data,"/NRW_population_by age.csv"))

pop_county <- read.csv(paste0(path_data,"/NRW_cencus/NRW_pop_bycounty_NUTS_ID.csv"),row.names = 1, encoding="UTF-8")
pop_county <- pop_county%>% dplyr::select(c("pop_n", "NUTS_ID")) #county name has error ->rm it
area <- read.csv(paste0(path_data,"/Size_territory_de.csv"), encoding="UTF-8")
area <- area %>% select(area_sq_km,NUTS_ID)

###################
#clean data
###################

pop_age$agegroup<- pop_age$age
pop_age$agegroup <- cut(pop_age$agegroup, breaks=c(-Inf, 4, 9,14, 19,24,29,34,39,44,49,54,59,64,69,74,79,84,89, Inf))
pop_age$agegroup <- factor(pop_age$agegroup, levels=c("(-Inf,4]", "(4,9]", "(9,14]", "(14,19]", "(19,24]", "(24,29]", "(29,34]", "(34,39]", "(39,44]", "(44,49]", "(49,54]", "(54,59]", "(59,64]", "(64,69]", "(69,74]", "(74,79]", "(79,84]", "(84,89]",  "(89, Inf]"),
                           labels=c("0-4","5-9","10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
                                    "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
                                    "75-79", "80-84", "85-89", ">90"), ordered=T)
# agegroup2: 6
pop_age$agegroup2<- pop_age$age
pop_age$agegroup2 <- cut(pop_age$agegroup2, breaks=c(-Inf,4,14,34,59,79, Inf))
pop_age$agegroup2 <- factor(pop_age$agegroup2, levels=c("(-Inf,4]", "(4,14]", "(14,34]", "(34,59]", "(59,79]",  "(79, Inf]"),
                            labels=c("0-4","5-14","15-34", "35-59", "60-79",">80"), ordered=T)




