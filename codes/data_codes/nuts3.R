#################
#(5) NUTS_3
#################
library(reshape2, ggplot2)
library('ggplot2')
library('scales')
library(tidyverse)

library(sf) # for reading shape files into R
library(tmap)#for creating the map
library(dplyr)
library(ggpubr) #ggarrange
library(pheatmap)

path_data <- paste0("C:/Users/huynh/sciebo/NRW_descriptive/data")

######################
#Import data
#####################

nuts3 <- read_sf(paste0(path_data,"/NUTS/NUTS_RG_20M_2021_3035.shp")) #Eurostat
# nuts3 <- read_sf(paste0(path_data,"/NUTS/nuts1000_12-31.gk3.shape/nuts1000_1231_3")) # RKI
nuts3 <- nuts3 %>% dplyr::filter(CNTR_CODE == "DE") # chose only Germany
nuts3$NAME_LATN <- gsub(",.*", "",nuts3$NAME_LATN) #rm string after comma
colnames(nuts3)[which(names(nuts3) == "NAME_LATN")] <- "county"  # rename a col
nuts3 <- nuts3 %>% dplyr::filter(nuts3$NUTS_ID %in% pop_county$NUTS_ID) # choose only NRW
nuts3 <- merge(nuts3,pop_county, by = "NUTS_ID") # add population

area <- read.csv(paste0(path_data,"/Size_territory_de.csv"), encoding="UTF-8")
area <- area %>% select(area_sq_km,NUTS_ID)

nuts3 <- merge(nuts3,area, by = "NUTS_ID") # add area


###################
#clean data
###################




