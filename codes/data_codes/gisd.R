#################
#(6) Deprivation upto 2015
#################
library(reshape2, ggplot2)
library('ggplot2')
library('scales')
library(tidyverse)
library(dplyr)



path_data <- paste0("C:/Users/huynh/sciebo/NRW_descriptive/data")
path_data_code <- paste0("C:/Users/huynh/sciebo/NRW_descriptive/gitHub/NRW_COVID-19_decriptive/codes/data_codes")
######################
#Import data
#####################

source(paste0(path_data_code,"/nuts3.R"),encoding="UTF-8")
county_code <- read.csv(paste0(path_data,"/county_code.csv"),row.names = 1)

# download.file("https://raw.githubusercontent.com/lekroll/GISD/master/Revisions/2019/Bund/Kreis/Kreis.csv", destfile = paste0(path_data,"/GISD/GISD_upto2015.csv")) # download data from Github## check update version
gisd <- read.csv(paste0(path_data,"/GISD/GISD_upto2015.csv"), sep = ",",row.names = 1)
colnames(gisd) <- c("county_id","county","year","pop","GISD_Score","GISD_5","GISD_10","GISD_k") # the pop from stat will be use -> rm this
gisd <- gisd %>%dplyr::select(-c(pop)) # rm pop
gisd <- gisd %>%dplyr::filter(year == 2015) # the most recent data
gisd$county <- gsub(",.*", "", gisd$county) #rm string after comma

gisd <- merge(nuts3_sf,gisd, by = "county")    # add NUTS info

gisd$pop.dens <- gisd$pop_n/gisd$area_sq_km # add density

# Extracting data.frame from simple features object(sf)
gisd_sf <- gisd
gisd <- dplyr::select(as.data.frame(gisd),-geometry)

# unemploy
unemploy <- read.csv(paste0(path_data,"/unemployment.csv"), sep = ",",encoding="UTF-8")
unemploy <- unemploy%>% dplyr::select(-c(county))# county of county_code data more accuracy
unemploy <- merge(county_code,unemploy,by = "county_code")
unemploy <- unemploy%>% dplyr::select(-c(county_code))# rm county
unemploy$unemployed_rate <- gsub("," ,".", unemploy$unemployed_rate) # replace comma
unemploy$unemployed_rate <- as.numeric(unemploy$unemployed_rate)
# living space
living_space <- read.csv(paste0(path_data,"/living_space.csv"), sep = ",",encoding="UTF-8")
living_space<- living_space%>% dplyr::select(-c(county))# county of county_code data more accuracy
living_space<- merge(county_code,living_space,by = "county_code")
living_space<-living_space%>% dplyr::select(-c(county_code))
living_space$living_space <- gsub(",",".",living_space$living_space)# replace comma
living_space$living_space <- as.numeric(living_space$living_space)
#age_older75
age_older75 <- read.csv(paste0(path_data,"/age_older75.csv"), sep = ",",encoding="UTF-8")
age_older75<- age_older75%>% dplyr::select(-c(county))# county of county_code data more accuracy
age_older75<- merge(county_code,age_older75,by = "county_code")
age_older75<-age_older75%>% dplyr::select(-c(county_code))
age_older75$age_older75 <- gsub(",",".",age_older75$age_older75)# replace comma
age_older75$age_older75 <- as.numeric(age_older75$age_older75)

# average age
ave.age <- read.csv(paste0(path_data,"/average_age.csv"), sep = ",",encoding="UTF-8")
ave.age <- ave.age%>% dplyr::select(-c(county))# county of county_code data more accuracy
ave.age <- merge(county_code,ave.age,by = "county_code")
ave.age <- ave.age%>% dplyr::select(-c(county_code))# rm county
ave.age$ave.age <- gsub(",",".",ave.age$ave.age)# replace comma
ave.age$ave.age <- as.numeric(ave.age$ave.age)

#density

density <- read.csv(paste0(path_data,"/density.csv"), sep = ",",encoding="UTF-8")
density <- density%>% dplyr::select(-c(county))# county of county_code data more accuracy
density <- merge(county_code,density,by = "county_code")
density <- density%>% dplyr::select(-c(county_code))# rm county
density$density <- gsub("\\.","",density$density)# replace dot 
density$density <- gsub(",",".",density$density)# replace comma
density$density <- as.numeric(density$density)

gisd <- merge(gisd,unemploy, by ="county") # add unemployment
gisd <- merge(living_space,gisd, by ="county") # add living_space
gisd <- merge(age_older75,gisd, by ="county") # add age_older75
gisd <- merge(ave.age,gisd, by ="county") # add ave.age
gisd <- merge(density,gisd, by ="county") # add density

rm(unemploy,living_space,age_older75,ave.age,density)


dummy <- gisd %>% dplyr::select(density,pop.dens)


# rm(pop_county,area_county,area,county)
