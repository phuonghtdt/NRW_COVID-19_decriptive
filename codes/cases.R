
rm(list=ls())

library(reshape2, ggplot2)
library('ggplot2')
library('scales')
library(tidyverse)
library(surveillance)
library(lubridate)
library(broom)
library(runjags)
library(rjags)
library(EpiEstim)
library(incidence)
library(epitools)

#pop
NRW_pop <- 17925570
NRW_under12 <- 2014762
NRW_vacc <- NRW_pop -NRW_under12

# Read data
# is "data_public" generated fromFall_Liste? Why don't retain "hosp_status" variable
dat = read.csv("C:/Users/huynh/sciebo/NRW_descriptive/data/data_public.csv", sep=";")
# dat$rep_date<-strptime(dat$rep_date, format = "%d%b%y")
# dat$rep_date<- as.Date(dat$rep_date, "%Y-%m-%d")
# dat$disease_start<-strptime(dat$disease_start, format = "%d%b%y")
# dat$disease_start<- as.Date(dat$disease_start, "%Y-%m-%d")
# dat$age <- ifelse(dat$age=="-1", NA, dat$age)

#revert week to date: week1,2020 include 3 dates 1,2,3, week2 start at 4,... ##Mon is start of a week
#check which day is starting of a week 
# data$rep_week_date <- lubridate::ymd( "2020-01-04" ) + lubridate::weeks(data$rep_week - 1 )#
dat$rep_week_date <- lubridate::ymd( "2020-01-04" ) + lubridate::weeks(dat$rep_week-2)# indicated date starts week 2 of the year
dat$rep_week_date <- as.Date(dat$rep_week_date)


dat$agegroup<- dat$age
dat$agegroup <- cut(dat$agegroup, breaks=c(-Inf, 4, 9,14, 19,24,29,34,39,44,49,54,59,64,69,74,79,84,89, Inf))
dat$agegroup <- factor(dat$agegroup, levels=c("(-Inf,4]", "(4,9]", "(9,14]", "(14,19]", "(19,24]", "(24,29]",
                                              "(29,34]", "(34,39]", "(39,44]", "(44,49]", "(49,54]", 
                                              "(54,59]", "(59,64]", "(64,69]", "(69,74]", "(74,79]", "(79,84]", "(84,89]", 
                                              "(89, Inf]"),
                       labels=c("0-4","5-9","10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
                                "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
                                "75-79", "80-84", "85-89", ">90"), ordered=T)


dat_allAge <- dat %>%
  group_by(rep_week_date,rep_week) %>%
  dplyr::summarise(cases=n()) %>% # sum cases per week in all age groups
  ungroup()%>%
  mutate(agegroup = "allAge") # add col "agegroup"
dat_allAge <- dat_allAge[,c(1,2,4,3)] #arrange col order as in 'dat_week' for merging data later

dat_week <- dat %>%
  group_by(rep_week_date,rep_week, agegroup) %>%
  dplyr::summarise(cases=n()) %>% #sum cases per week by age groups
  ungroup() 

dat_week <- rbind(dat_week, dat_allAge)# add cases of allAge

# %>%complete(rep_week_date,rep_week, agegroup)

# datdummy$cases <- ifelse(is.na(datdummy$cases), 0, datdummy$cases) # replace NA by = 0 ## be cautious!

dat_week <- dat_week %>% group_by(rep_week) %>% mutate(percent_repweek=cases/sum(cases))

dat_date <- dat %>% group_by(rep_date, agegroup) %>% dplyr::summarise(cases=n())
dat_date <- dat_date %>% filter(rep_date>"2020-02-26")
dat_date <- dat_date %>% group_by(rep_date) %>% mutate(percent_repdate=cases/sum(cases))

# age_nrw = read_csv("C:/Users/Tom/Documents/Praktikum M|nster/Covid-19 LZG/Analyse/now_casting/Alter_NRW.csv")
age_nrw = read.csv("C:/Users/huynh/sciebo/NRW_descriptive/data/age_distribution_nrw.csv") 
age_nrw$agegroup <- factor(age_nrw$agegroup, levels=c("(-Inf,4]", "(4,9]", "(9,14]", "(14,19]", "(19,24]", "(24,29]",
                                                          "(29,34]", "(34,39]", "(39,44]", "(44,49]", "(49,54]", 
                                                          "(54,59]", "(59,64]", "(64,69]", "(69,74]", "(74,79]", "(79,84]", "(84,89]", 
                                                          "(89, Inf]"),
                             labels=c("0-4","5-9","10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
                                      "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
                                      "75-79", "80-84", "85-89", ">90"), ordered=T)

all_age <- data.frame(agegroup = c("allAge"),
                 inhabitants= c(sum(age_nrw$inhabitants)))
age_nrw <- rbind(age_nrw,all_age)
                 
dat_all<- merge(dat_week, age_nrw, by="agegroup", all=T)# add nrw age distribution
# pop pulation

dat_all$incper100000<- dat_all$cases/dat_all$inhabitants*100000
# dat_all$incper100000<- dat_all$cases/NRW_pop*100000

#division 0
dat_all$incper100000_grouped <- cut(dat_all$incper100000, breaks=c(-Inf, 5, 10, 15, 20, 35,50,100,150,200,300,Inf))
dat_all$incper100000_grouped <- factor(dat_all$incper100000_grouped, levels=c("(-Inf,5]", "(5,10]", "(10,15]", "(15,20]", "(20,35]", "(35,50]",
                                                                                  "(50,100]", "(100,150]", "(150,200]", "(200,300]", "(300, Inf]"),
                                         labels=c("0-5",">5-10",">10-15", ">15-20", ">20-35", ">35-50", ">50-100", ">100-150",
                                                  ">150-200", ">200-300", ">300"), ordered=T)

##division 1
# dat_all$incper100000_grouped <- cut(dat_all$incper100000, breaks=c(-Inf, 10, 20, 30, 40,50, 100,200,400,Inf))
# dat_all$incper100000_grouped <- factor(dat_all$incper100000_grouped, levels=c("(-Inf,10]", "(10,20]", "(20,30]", "(30,40]", "(40,50]", "(50,100]",
#                                                                               "(100,200]", "(200,400]", "(400, Inf]"),
#                                        labels=c("0-10",">20-30",">30-40", ">40-50", ">50-100", ">100-200", ">200-400", ">200-400",
#                                                  ">400"), ordered=T)

# #division 2
# dat_all$incper100000_grouped <- cut(dat_all$incper100000, breaks=c(-Inf,50,100,150, 200,250,300,350,400,Inf))
# dat_all$incper100000_grouped <- factor(dat_all$incper100000_grouped, levels=c("(-Inf,50]", "(50,100]", "(100,150]", "(150,200]", "(200,250]", "(250,300]",
#                                                                               "(300,350]", "(350,400]", "(400, Inf]"),
#                                        labels=c("0-50",">50-100",">100-150", ">150-200", ">200-250", ">250-300", ">300-350", ">350-400",
#                                                  ">400"), ordered=T)


# #division 3
# dat_all$incper100000_grouped <- cut(dat_all$incper100000, breaks=c(-Inf,25,50,75,100,200,300,400,500,Inf))
# dat_all$incper100000_grouped <- factor(dat_all$incper100000_grouped, levels=c("(-Inf,25]", "(25,50]", "(50,75]", "(75,100]", "(100,200]", "(200,300]",
#                                                                               "(300,400]", "(400,500]", "(500, Inf]"),
#                                        labels=c("0-25",">25-50",">50-75", ">75-100", ">100-200", ">200-300", ">300-400", ">400-500",
#                                                 ">500"), ordered=T)

# dat_all <- dat_all %>% filter(rep_week >=10) # more consistent report time? week 10 onward
dat_all <- dat_all %>% filter(rep_week >=11) # compared to RKI figure

dat_all %>% 
  filter(!is.na(agegroup)) %>%
  ggplot(aes(rep_week_date, agegroup)) +
  geom_tile(aes(fill=incper100000_grouped), colour = "black")+
  geom_text(aes(label = round(incper100000)), size = 2, colour = "#585858") + #size of number in each cell
  #division 0
  scale_fill_manual(values=c("#FFF7FC","#EDE7F3", "#D0D1E6", "#A6BDDC", "#74AAD0",
                             "#3691C0", "#0470B0", "#044E7B", "#333F50", "#404040", "#262626")) +
  ## division 1,2,3
  # scale_fill_brewer(palette = "Blues")+ #BuPu, BuGn, Blues, Oranges,Reds,....
  labs(fill = "incidence (per 100,000)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="right",
        legend.title=element_text(size=18, colour = "black", face = "bold") ,
        legend.text=element_text(size=17, colour = "black"),
        axis.title = element_text(size=18, face="bold", colour = "black"),
        plot.title = element_text(face="bold", size=15, hjust=0),
        axis.text.x = element_text(size = 10,  hjust = 1, colour = "black", angle = 30),
        axis.text.y = element_text(size = 10)) + 
  scale_x_date(date_labels="%d-%m-%Y", date_breaks = "30 days", expand = c(0, 0)) +
  # scale_x_continuous(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0))

# ggsave("Heatmap.png", plot = last_plot(), width=18, height=8, path = "C:/Users/Tom/Documents/Praktikum M|nster/Covimod/plots")

