# Loading packages

rm(list = ls())
#desolve Programm
library(epiDisplay)
library("deSolve")
library(plyr)
library(ggplot2)
library("RColorBrewer")
library(wesanderson)
library("ggsci")
library(scales)
#library(xlsx)
#Susanne
library(openxlsx)
library("readr")
library(dplyr)
library(ggrepel)
library(directlabels)
library(writexl)
library(cowplot)
library(Cairo)
library("readxl")
library(zoo)
library(grid)
library(epiDisplay)
library(ggpubr)
library(glue)
library(stringr)

set.seed(123456789)


#corona

#################### Funtion Corona #############################################################################

# File_paths

#################to be adapted#############################

Date_today <- as.Date('28.12.2021', format='%d.%m.%Y') 


# FILE_path<-paste0("C:/Users/jaeger/Desktop/vom server/COVID-19 LZG/Fdlle/")                               #Veronika
FILE_path<-paste0("C:/Users/huynh/sciebo/NRW_descriptive/data/")                               #Veronika
#FILE_path<-paste0("")                                              #Tom
#FILE_path<-paste0("~/Desktop/EPICode/")                            #Susanne
#FILE_path<-paste0("C:/Users/zeglin/Documents/EPICode/")             #Susanne Testlauf
#FILE_path<-paste0("C:/Users/zeglin/Documents/EpiCode/LZG/LZG/")     #Susanne2



Read_path_VaccData_BUND<-paste0(FILE_path,"Aktuell_Deutschland_Bundeslaender_COVID-19-Impfungen.csv")
Read_path_VaccData_LK<-paste0(FILE_path,"Aktuell_Deutschland_Landkreise_COVID-19-Impfungen.csv")


# Loading data

#######################################################################################################
#### Data ####
#######################################################################################################

#read vaccination data
vacc_rki_BUND<- read_csv(Read_path_VaccData_BUND)
vacc_rki_LK<- read_csv(Read_path_VaccData_LK)
vacc_rki_LK <- vacc_rki_LK %>% filter(str_sub(LandkreisId_Impfort,1,2)=='05') 

###################################################################################################
# CAVE: It is necessary to distinguish between the vaccines, since only one dose is need when vaccinated with J&J.
# dim_first & prax_first vaccinations will count first vaccinations WITHOUT the vaccine by J&J. 
# For modeling purposes J&J will be treated as second vaccination, therefore it will be added to dim_sec & prax_sec vaccinations,
# even though the datatable given by the LZG treats it as first vaccination.

# # NRW population
# NRW_pop <- 179255700
# NRW_pop_vacc <- NRW_pop-869901

#pop
NRW_pop <- 17925570
NRW_under12 <- 2014762
NRW_pop_vacc <- NRW_pop - NRW_under12

# vacciantions in NRW

vacc_rki_NRW <- vacc_rki_BUND %>% filter(BundeslandId_Impfort=="05")
colnames(vacc_rki_NRW) <- c("vac_date",'state_code',"vac_type",'dose',"vac_n")

### Vaccination progresss
vacc_rki_NRW$vac_type <- as.factor(vacc_rki_NRW$vac_type)
vacc_rki_NRW$dose <- as.factor(vacc_rki_NRW$dose)
vacc_rki_NRW$vac_date <- as.Date(vacc_rki_NRW$vac_date)
vacc_rki_NRW$maxvacdate <- max(vacc_rki_NRW$vac_date)
vacc_rki_NRW$vac_week <- as.integer((as.Date(vacc_rki_NRW$vac_date)-as.Date("2020-01-01"))/7+1)
#revert week to date: . ##Sat (1/1/2020), Sunday is the start of a week
vacc_rki_NRW$vac_week_date <- lubridate::ymd( "2020-01-01" ) + lubridate::weeks(vacc_rki_NRW$vac_week-1)# start at week 1 of the year

#vaccination_by date#############################
ggplot(vacc_rki_NRW,
       aes(x = vac_date, y=vac_n/1000 , group = vac_type))+
  # geom_bar(position="fill", stat="identity")+
  geom_line(aes(color=vac_type))+
  facet_grid(row = vars(dose))+
  labs(x = "Vaccination time", y = "Vacinated individual/1k pop daily") +
  ggtitle("NWR_vaccination")+
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(face = "bold",hjust = 0.5))+
  scale_x_date(date_labels="%d-%m-%Y", date_breaks = "7 days", expand = c(0, 0))

# vaccination by week#############################
vac_byweek <- vacc_rki_NRW %>% group_by(vac_week_date, dose, vac_type) %>%
  summarise_if(is.numeric, sum)
vac_byweek <- vac_byweek%>%select(-c("vac_week"))# delete vac_week as it's wrong due to previous step

ggplot(vac_byweek,
       aes(x = vac_week_date, y=vac_n/1000 , group = vac_type))+
  # geom_bar(position="fill", stat="identity")+
  geom_line(aes(color=vac_type))+
  facet_grid(row = vars(dose))+
  labs(x = "Vaccination time", y = "Vacinated individual/1k pop weekly") +
  ggtitle("NWR_vaccination")+
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(face = "bold",hjust = 0.5))+
  scale_x_date(date_labels="%d-%m-%Y", date_breaks = "7 days", expand = c(0, 0))

# vaccination progress regardless of vac_type####################
vac_alltype1 <- vacc_rki_NRW %>%filter(dose==1)%>% group_by(vac_date) %>%
  summarise_if(is.numeric, sum)
vac_alltype1 <- vac_alltype1%>%select(-c("vac_week"))# delete vac_week as it's wrong due to previous step
vac_alltype1$vac_n_cum <- cumsum(vac_alltype1$vac_n)
vac_alltype1$dose <- 1

vac_alltype2 <- vacc_rki_NRW %>%filter(dose==2)%>% group_by(vac_date) %>%
  summarise_if(is.numeric, sum)
vac_alltype2 <- vac_alltype2%>%select(-c("vac_week"))# delete vac_week as it's wrong due to previous step
vac_alltype2$vac_n_cum <- cumsum(vac_alltype2$vac_n)
vac_alltype2$dose <- 2

vac_alltype3 <- vacc_rki_NRW %>%filter(dose==3)%>% group_by(vac_date) %>%
  summarise_if(is.numeric, sum)
vac_alltype3 <- vac_alltype3%>%select(-c("vac_week"))# delete vac_week as it's wrong due to previous step
vac_alltype3$vac_n_cum <- cumsum(vac_alltype3$vac_n)
vac_alltype3$dose <- 3

vac_alltype <- rbind(vac_alltype1,vac_alltype2,vac_alltype3)
vac_alltype$dose <- as.factor(vac_alltype$dose )

ggplot(vac_alltype,
       aes(x = vac_date,y=vac_n/1000, color = dose))+
  # geom_bar(position="fill", stat="identity")+
  geom_line()+
  # facet_grid(row = vars(dose))+
  labs(x = "Vaccination time", y = "Vacinated individual/1k pop") +
  ggtitle("NWR_vaccination")+
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(face = "bold",hjust = 0.5))+
  scale_x_date(date_labels="%d-%m-%Y", date_breaks = "7 days", expand = c(0, 0))

#cum vac over time ###################
NRW_pop_vacc <- NRW_pop-869901
NRW_pop_vacc <- NRW_pop - NRW_under12

ggplot(vac_alltype,
       aes(x = vac_date,y=vac_n_cum/NRW_pop_vacc*100, color = dose))+
  # geom_bar(position="fill", stat="identity")+
  geom_line()+
  # facet_grid(row = vars(dose))+
  labs(x = "Vaccination time", y = "Vacinated percentage (%)/ target pop") +
  ggtitle("NWR_vaccination")+
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(face = "bold",hjust = 0.5))+
  scale_x_date(date_labels="%d-%m-%Y", date_breaks = "7 days", expand = c(0, 0))

# vaccination by week
# vacc_rki_NRW includes all vaccines
vacc_rki_NRW <- vacc_rki_NRW %>% 
  select(c(Impfdatum, Impfstoff, Impfserie, Anzahl))

# vacc_rki_NRW_excl_jj excludes vaccine by Johnson & Johnson. 
vacc_rki_NRW_excl_jj <- vacc_rki_NRW %>% 
  filter(Impfstoff !="Janssen")

vacc_rki_NRW_1_excl_jj <- vacc_rki_NRW_excl_jj %>%
  filter (Impfserie ==1)

vacc_rki_NRW_2_excl_jj <- vacc_rki_NRW %>% 
  filter (Impfserie == 2)

vacc_rki_NRW_3_excl_jj <- vacc_rki_NRW %>% 
  filter (Impfserie == 3)

# number of total Johnson & Johnson (Janssen) DIM vaccinations
johnson_vacc <- vacc_rki_NRW %>%
  filter(Impfstoff == "Janssen")

vacc_first_combined <- (vacc_rki_NRW_1_excl_jj)
vacc_second_combined <- rbind(vacc_rki_NRW_2_excl_jj, johnson_vacc)
vacc_third_combined <- vacc_rki_NRW_3_excl_jj
vacc_first_inclusive_jj <- rbind(vacc_rki_NRW_1_excl_jj, johnson_vacc)

# Ersitimpfungen
vacc_first_combined_per_day <- vacc_first_combined %>%
  dplyr::select(c(Impfdatum, Anzahl)) %>%
  dplyr::group_by(Impfdatum) %>%
  dplyr::summarise(Erstimpfung_day = sum(Anzahl)) %>%
  ungroup

vacc_first_combined_per_day$maxImpfdatum <- max(vacc_first_combined_per_day$Impfdatum)
vacc_first_combined_per_day$Impf_week <- as.integer((as.Date(vacc_first_combined_per_day$Impfdatum)-as.Date("2021-01-04"))/7+1)
vacc_first_combined_per_day$maxImpfweek <- max(vacc_first_combined_per_day$Impf_week)


vacc_first_combined_per_week <- vacc_first_combined_per_day %>%
  dplyr::select(c(Impf_week, Erstimpfung_day, Impfdatum)) %>%
  dplyr::group_by(Impf_week) %>%
  dplyr::summarise(Erstimpfung_week = sum(Erstimpfung_day), 
                   start_week = min(Impfdatum),
                   end_week = max(Impfdatum)) %>%
  ungroup
vacc_first_combined_per_week$Erstimpfung_week_average_day <- round(vacc_first_combined_per_week$Erstimpfung_week/7)

# Ersitimpfungen inklusive JJ
vacc_first_inclusive_jj_per_day <- vacc_first_inclusive_jj %>%
  dplyr::select(c(Impfdatum, Anzahl)) %>%
  dplyr::group_by(Impfdatum) %>%
  dplyr::summarise(Erstimpfung_day = sum(Anzahl)) %>%
  ungroup

vacc_first_inclusive_jj_per_day$maxImpfdatum <- max(vacc_first_inclusive_jj_per_day$Impfdatum)
vacc_first_inclusive_jj_per_day$Impf_week <- as.integer((as.Date(vacc_first_inclusive_jj_per_day$Impfdatum)-as.Date("2021-01-04"))/7+1)
vacc_first_inclusive_jj_per_day$maxImpfweek <- max(vacc_first_inclusive_jj_per_day$Impf_week)


vacc_first_inclusive_jj_per_week <- vacc_first_inclusive_jj_per_day %>%
  dplyr::select(c(Impf_week, Erstimpfung_day, Impfdatum)) %>%
  dplyr::group_by(Impf_week) %>%
  dplyr::summarise(Erstimpfung_week = sum(Erstimpfung_day), 
                   start_week = min(Impfdatum),
                   end_week = max(Impfdatum)) %>%
  ungroup
vacc_first_inclusive_jj_per_week$Erstimpfung_week_average_day <- round(vacc_first_inclusive_jj_per_week$Erstimpfung_week/7)

# Zweitimpfungen
vacc_second_combined_per_day <- vacc_second_combined %>%
  dplyr::select(c(Impfdatum, Anzahl)) %>%
  dplyr::group_by(Impfdatum) %>%
  dplyr::summarise(Zweitimpfung_day = sum(Anzahl)) %>%
  ungroup

vacc_second_combined_per_day$maxImpfdatum <- max(vacc_second_combined_per_day$Impfdatum)
vacc_second_combined_per_day$Impf_week <- as.integer((as.Date(vacc_second_combined_per_day$Impfdatum)-as.Date("2021-01-04"))/7+1)
vacc_second_combined_per_day$maxImpfweek <- max(vacc_second_combined_per_day$Impf_week)



vacc_second_combined_per_week <- vacc_second_combined_per_day %>%
  dplyr::select(c(Impf_week, Zweitimpfung_day, Impfdatum)) %>%
  dplyr::group_by(Impf_week) %>%
  dplyr::summarise(Zweitimpfung_week = sum(Zweitimpfung_day), 
                   start_week = min(Impfdatum),
                   end_week = max(Impfdatum)) %>%
  ungroup
vacc_second_combined_per_week$Zweitimpfung_week_average_day <- round(vacc_second_combined_per_week$Zweitimpfung_week/7)



# Drittimpfungen
vacc_third_combined_per_day <- vacc_third_combined %>%
  dplyr::select(c(Impfdatum, Anzahl)) %>%
  dplyr::group_by(Impfdatum) %>%
  dplyr::summarise(Drittimpfung_day = sum(Anzahl)) %>%
  ungroup

vacc_third_combined_per_day$maxImpfdatum <- max(vacc_third_combined_per_day$Impfdatum)
vacc_third_combined_per_day$Impf_week <- as.integer((as.Date(vacc_third_combined_per_day$Impfdatum)-as.Date("2021-01-04"))/7+1)
vacc_third_combined_per_day$maxImpfweek <- max(vacc_third_combined_per_day$Impf_week)

vacc_third_combined_per_week <- vacc_third_combined_per_day %>%
  dplyr::select(c(Impf_week, Drittimpfung_day, Impfdatum)) %>%
  dplyr::group_by(Impf_week) %>%
  dplyr::summarise(Drittimpfung_week = sum(Drittimpfung_day), 
                   start_week = min(Impfdatum),
                   end_week = max(Impfdatum)) %>%
  ungroup
vacc_third_combined_per_week$Drittimpfung_week_average_day <- round(vacc_third_combined_per_week$Drittimpfung_week/7)

# Johnson
johnson_vacc_per_day <- johnson_vacc %>%
  dplyr::select(c(Impfdatum, Anzahl)) %>%
  dplyr::group_by(Impfdatum) %>%
  dplyr::summarise(Johnson_day = sum(Anzahl)) %>%
  ungroup

johnson_vacc_per_day$maxImpfdatum <- max(johnson_vacc_per_day$Impfdatum)
johnson_vacc_per_day$Impf_week <- as.integer((as.Date(johnson_vacc_per_day$Impfdatum)-as.Date("2021-01-04"))/7+1)
johnson_vacc_per_day$maxImpfweek <- max(johnson_vacc_per_day$Impf_week)

johnson_vacc_per_week <- johnson_vacc_per_day %>%
  dplyr::select(c(Impf_week, Johnson_day, Impfdatum)) %>%
  dplyr::group_by(Impf_week) %>%
  dplyr::summarise(Johnson_week = sum(Johnson_day), 
                   start_week = min(Impfdatum),
                   end_week = max(Impfdatum)) %>%
  ungroup
johnson_vacc_per_week$Johnson_week_average_day <- round(johnson_vacc_per_week$Johnson_week/7)


#what percentage of the population will be vaccinated after 90 days modelling
#only including 5+ years in N_vacc
N_NRW <- 17925570
N_vacc <- N_NRW-869901

#how many first vaccinations assumed after 90 days of modelling (assuming 18.000 first vaccinations per day) - CAVE only use 76 days as 14 days needed to develop full protection
sum(vacc_first_combined_per_day$Erstimpfung_day) + 76*18000
#percentages of eligible population
(sum(vacc_first_combined_per_day$Erstimpfung_day) + 76*18000)*100/N_vacc 

#how many fully vaccinated assumed after 90 days of modelling (assuming 25.000 second or J&J vaccinations per day)- CAVE only use 76 days as 14 days needed to develop full protection
sum(vacc_second_combined_per_day$Zweitimpfung_day) + 76*25000
#percentage of eligible population
(sum(vacc_second_combined_per_day$Zweitimpfung_day) + 76*25000)*100/N_vacc 


## vaccinations based on LK data, i.e. second vaccinations implies full coverage, i.e. also includes J&J and people who received only one shot after infection

vacc_lk_first_per_day <- vacc_rki_LK %>%
  filter(Impfschutz==1) %>%
  group_by(Impfdatum) %>%
  summarise(vacc_per_day=sum(Anzahl))

vacc_lk_second_per_day <- vacc_rki_LK %>%
  filter(Impfschutz==2) %>%
  group_by(Impfdatum) %>%
  summarise(vacc_per_day=sum(Anzahl))


######f|r Bericht get rid of last 14 days
vacc_first_combined_per_day_without_14_days <- vacc_first_combined_per_day %>% filter(Impfdatum<Date_today-13)
all_erst_14days <- sum(vacc_first_combined_per_day_without_14_days$Erstimpfung_day)
all_erst_14days_impfbar<-all_erst_14days/N_vacc*100
all_erst_14days__NRW<-all_erst_14days/N_NRW*100
johnson_vacc_per_day_without_14_days <- johnson_vacc_per_day %>% filter(Impfdatum<Date_today-13)
all_erst_14days <- sum(vacc_first_combined_per_day_without_14_days$Erstimpfung_day)+ sum(johnson_vacc_per_day_without_14_days$Johnson_day)
all_erst_14days_plus_jj_impfbar<-all_erst_14days/N_vacc*100
all_erst_14days__plus_jj_NRW<-all_erst_14days/N_NRW*100

vacc_second_combined_per_day_without_14_days <- vacc_second_combined_per_day %>% filter(Impfdatum<Date_today-13)
all_second_14days <- sum(vacc_second_combined_per_day_without_14_days$Zweitimpfung_day)
all_second_14days_plus_jj_impfbar<-all_second_14days/N_vacc*100
all_second_14days_plus_jj_NRW<-all_second_14days/N_NRW*100


##### Erstimpfung inclusive J&J + Prozent Erstgeimpft bis heute
all_erst_plus_jj <- sum(vacc_first_combined_per_day$Erstimpfung_day) + sum(johnson_vacc_per_day$Johnson_day)
all_erst_plus_jj_impfbar<-all_erst_plus_jj/N_vacc*100
all_erst_plus_jj_NRW<-all_erst_plus_jj/N_NRW*100
##### Zweitimpfungen inclusive J&H + Prozent Zweitgeimpft bis heute
all_zweir_plus_jj<-sum(vacc_second_combined_per_day$Zweitimpfung_day)
all_zweir_plus_jj_impfbar<-all_zweir_plus_jj/N_vacc*100
all_zweir_plus_jj_NRW<-all_zweir_plus_jj/N_NRW*100

