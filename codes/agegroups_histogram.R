
rm(list=ls())
library(haven)
library(ggplot2)
library(lubridate)
library(frequency)
library(pastecs)
library(epiDisplay)
library(naniar)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(survival)
library(survminer)
library(zoo)
library(data.table)
library(Hmisc)
library(summarytools)
library(patchwork)
library(ggthemr)
# ggthemr("fresh")
library(gtsummary)
library(table1)
library("readxl")
library(reshape2)
library("kableExtra")
library(DescTools)
library(aweek)

export_date<-"2021-12-29"
# export_path<-"C:/Users/jaeger/Desktop/vom server/COVID-19 LZG/Analyse/now casting/data_public_"
export_path<-"C:/Users/huynh/sciebo/NRW_descriptive/data"

colfunc <- colorRampPalette(c("black", "white"))
colfunc(19)
palette <- c("red","#003366", "olivedrab3","#336699", "#CCFFFF", "#028482", "#663399", "#990033", "#FF3333", "royalblue1","#FF9900", 
             "#266A2E", "#55D43F", "#404040", "#C87533", "#DCDCDC", "#FFDE00", "#F1B2E1", "#FF717E", "maroon3")
print(palette)
scales::show_col(palette)

palette1 <- c("#8e0152","#c51b7d", "#de77ae","#f1b6da", "#fde0ef",
              "#276419", "#4d9221", "#7fbc41","#b8e186","#e6f5d0",
              "#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7",
              "#e0e0e0", "#bababa", "#878787", "#4d4d4d", "#1a1a1a")











print(palette1)
scales::show_col(palette1)


#pop
NRW_pop <- 17925570
NRW_under12 <- 2014762
NRW_vacc <- NRW_pop -NRW_under12
#age proportion
p_0_4 <-  0.177
p_between_18_59 <-  0.543
p_over_60 <-  0.280

# data <- read_excel("C:/Users/jaeger/Desktop/vom server/COVID-19 LZG/Fdlle/201217 NRW dateninput modellierungen wwu.xlsx")
data <- read.csv("C:/Users/huynh/sciebo/NRW_descriptive/data/data_public.csv", sep = ";")
# data <- data %>% select(-InterneRef)
# names(data) <- c("rep_date", "disease_start", "age")
data$rep_date <- as.Date(data$rep_date)
data$disease_start <- as.Date(data$disease_start)

data$maxrepdate <- max(data$rep_date)
data$rep_week <- as.integer((as.Date(data$rep_date)-as.Date("2020-01-01"))/7+1)
#revert week to date: week1, 2020 inlude 3 date 1,2,3, week2 start at 4,... ##Monday is start
# data$rep_week_date <- lubridate::ymd( "2020-01-04" ) + lubridate::weeks(data$rep_week - 1 )#
data$rep_week_date <- lubridate::ymd( "2020-01-04" ) + lubridate::weeks(data$rep_week-2)# start at week 2 of the year

data$maxrepweek <- max(data$rep_week)
#set the rep date for all obs to missing with reporting dates before 20th of feb
data$rep_date[data$rep_date<"2020-02-20"]<-NA

#set all disease start dates to missing if disease start before 1st of feb or after date of data export 
data$disease_start[data$disease_start<"2020-02-01"]<-NA
data$disease_start[data$disease_start>data$maxrepdate]<-NA


#set all disease start dates to missing for which the disease start date was more than 7 days after the rep date
data$disease_start[as.numeric(data$rep_date-data$disease_start) < -7] <- NA
data$disease_start[as.numeric(data$rep_date-data$disease_start) > 30] <- NA

#remove observations with all missings (rep_date or disease_start)
data <- data %>% filter(!is.na(rep_date) | !is.na(disease_start))


#set implausible age to NA
data$age2<-data$age
data$age2[data$age<0 | data$age>110] <- NA


# create age groups for plot
data$agegroup<- data$age2
data$agegroup <- cut(data$agegroup, breaks=c(-Inf, 4, 9,14, 19,24,29,34,39,44,49,54,59,64,69,74,79,84,89, Inf))
data$agegroup <- factor(data$agegroup, levels=c("(-Inf,4]", "(4,9]", "(9,14]", "(14,19]", "(19,24]", "(24,29]",
                                              "(29,34]", "(34,39]", "(39,44]", "(44,49]", "(49,54]", 
                                              "(54,59]", "(59,64]", "(64,69]", "(69,74]", "(74,79]", "(79,84]", "(84,89]", 
                                              "(89, Inf]"),
                       labels=c("0-4","5-9","10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
                                "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
                                "75-79", "80-84", "85-89", ">90"), ordered=T)



tab1(data$agegroup[data$rep_week==(data$maxrepweek)], decimal=2, graph=F)
tab1(data$agegroup[data$rep_week==(data$maxrepweek-1)], decimal=2, graph=F)

#create data structure for histogram & remove rep week 8 and 9 and rep_week == maxrepweek
histo_setup <- data %>% filter(!is.na(age) & rep_week>9 & (rep_week<maxrepweek)) %>%
  group_by(rep_week, agegroup) %>% dplyr::summarise(cases_n=n()) 

histo_setup$maxrepweek <- max(histo_setup$rep_week)

histo_setup %>%
  filter(!is.na(agegroup) & rep_week>9 & (rep_week<maxrepweek)) %>%
           ggplot(aes(x=rep_week, y=cases_n, fill=agegroup)) + geom_bar(position="fill", stat="identity")+
  scale_y_continuous(labels = scales::percent_format()) + scale_fill_manual(values=palette, name="Age group") + 
  theme(axis.text.x = element_text(angle = 45))

#create data structure for histogram & remove rep week 8 and 9 and rep_week_date
#case prop_all age group################################
histo_setup <- data %>% filter(!is.na(age) & rep_week>9 & (rep_week<maxrepweek)) %>%
  group_by(rep_week,rep_week_date, agegroup) %>% dplyr::summarise(cases_n=n()) 
histo_setup$maxrepweek <- max(histo_setup$rep_week)

histo_setup %>%
  filter(!is.na(agegroup) & rep_week>9 & (rep_week<maxrepweek)) %>%
  ggplot(aes(x=rep_week_date, y=cases_n, fill=agegroup)) + 
  geom_bar(position="fill", stat="identity")+
  scale_y_continuous(labels = scales::percent_format()) + 
  scale_fill_manual(values=palette, name="Age group") +
  labs(x = "Reported date", y = "Proportion of reported cases") +
  ggtitle("NWR_COVID-19 cases")+
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(face = "bold",hjust = 0.5))+
  scale_x_date(date_labels="%d-%m-%Y", date_breaks = "15 days", expand = c(0, 0))

# cases  per 100k pop overtime################
histo_setup %>%
  filter(!is.na(agegroup) & rep_week>9 & (rep_week<maxrepweek)) %>%
  ggplot(aes(x=rep_week_date, y=cases_n/NRW_pop*100000, color=agegroup)) + 
  geom_line()+
  # facet_grid(row = vars(agegroup))+
  # scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values=palette1, name="Age group") +
  labs(x = "Reported date", y = "Cases per 100k inhabitants") +
  ggtitle("NWR_COVID-19 cases")+
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(face = "bold",hjust = 0.5))+
  scale_x_date(date_labels="%d-%m-%Y", date_breaks = "15 days", expand = c(0, 0))


# scale by age distribution##################
histo_setup<- histo_setup %>% filter(!is.na(agegroup) & rep_week>9 & (rep_week<maxrepweek))

# 19 age group

agegroup <- c("0-4","5-9","10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
              "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
              "75-79", "80-84", "85-89", ">90")
agepro <- c(0.048528499,	0.04581885,	0.045676651,	0.048371684,	0.057699811,	0.062332021,	0.06519497,
           0.061183494,	0.058848952,	0.060657709,	0.079779722,	0.08206707,	0.070059195,	0.057226074,
           0.046680803,	0.039416041,	0.040065337,	0.020080533,	0.010312587)

agegroup_pro <- c()

for ( i in (1:dim(histo_setup)[1])){#dim(histo_setup)[1]
  for (j in (1:length(agegroup))){
    if(histo_setup$agegroup[i]== agegroup[j]){
      agegroup_pro[i] = agepro[j]
    }
  }
}

histo_setup$agegroup_pro <- agegroup_pro

histo_setup$cases_n_agescale <- histo_setup$cases_n/(histo_setup$agegroup_pro*NRW_pop)*1000

  ggplot(histo_setup,aes(x=rep_week_date, y=cases_n_agescale, color=agegroup)) + 
  geom_line()+
  # facet_grid(row = vars(agegroup))+
  # scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values=palette1, name="Age group") +
  labs(x = "Reported date", y = "Cases per 1k inhabitants in each age group") +
  ggtitle("NWR_COVID-19 cases")+
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(face = "bold",hjust = 0.5))+
  scale_x_date(date_labels="%d-%m-%Y", date_breaks = "15 days", expand = c(0, 0))

#percentage of infected individuals in each age group#######################
 
  ggplot(histo_setup,aes(x=rep_week_date, y=cases_n_agescale/agegroup_pop*100, color=agegroup)) + 
    geom_line()+
    # facet_grid(row = vars(agegroup))+
    # scale_y_continuous(labels = scales::percent_format()) +
    scale_color_manual(values=palette1, name="Age group") +
    labs(x = "Reported date", y = "percentage of infected individuals in each age group") +
    ggtitle("NWR_COVID-19 cases")+
    theme(axis.text.x = element_text(angle = 90),
          plot.title = element_text(face = "bold",hjust = 0.5))+
    scale_x_date(date_labels="%d-%m-%Y", date_breaks = "15 days", expand = c(0, 0)) 
  
# pop distribution       
histo_setup$agegroup_pop <- histo_setup$agegroup_pro*NRW_pop
  
histo_setup %>%
  filter(rep_week==11) %>%
  ggplot(aes(x = agegroup, y= agegroup_pop, fill=agegroup)) + 
  geom_bar(stat="identity")+
  # facet_grid(row = vars(agegroup))+
  # scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values=palette, name="Age group") +
  labs(y = "pop") +
  ggtitle("NRW_ pop distribution")+
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(face = "bold",hjust = 0.5))
