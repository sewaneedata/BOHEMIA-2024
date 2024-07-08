# PURPOSE:
# Creating a bar chart,  showing OSU over seven visits for efficacy dataset and over 4 visits for safety data set. Additionally, creating a tabulated format of five number summary of sufficiency
#Load Libraries
library(gsheet)
library(tidyverse)
library(dplyr)
library(gsheet)
#Load data from data.r

source('data.r')

#The goal here is to get ownership, sufficiency, and usage data from safety
#Starting with average ownership data
hh_ownership<-kenya_healthecon_total %>% 
  mutate(bednet_yn=ifelse(num_bed_nets==0,'no','yes')) %>%
  distinct(hhid,.keep_all = TRUE) %>% 
  drop_na(bednet_yn)
#look into ownership data by visit 
# hh_ownership_by_visit<-hh_ownership %>% 
#   group_by(visit) %>% 
#   distinct(hhid,.keep_all = TRUE) %>% 
#   drop_na(bednet_yn)
# need visit data from monthly if want this

hh_ownership %>% 
  group_by(bednet_yn) %>% 
  tally() %>% 
  mutate(total_percentage_ownership=(n/sum(n))*100)
#Next is sufficiency, with has a standard 2:1 ratio people to bed nets
hh_sufficiency <- hh_ownership %>% 
  filter(bednet_yn=='yes') %>% 
  distinct(hhid,.keep_all = TRUE) %>% 
  mutate(sufficiency=(num_hh_members/num_bed_nets)) 
# hh_sufficiency %>% 
#   group_by(sufficiency) %>%
#   tally() %>%
#   mutate(sufficiency=(n/sum(n))*100)
#Usage on a household level needs to be found
hh_usage<-kenya_safety_total %>%
  group_by(hhid,visit) %>% 
  mutate(sleep_net_last_night=(sleep_net_last_night=='yes'))

hh_last_usage<-hh_usage %>% 
  group_by(visit,sleep_net_last_night) %>% 
  filter(!is.na(sleep_net_last_night)) %>%
  tally() %>% 
  mutate(percent_last_usage=(n/sum(n))*100)

hh_average_usage<-hh_usage %>%
  mutate(nights_sleep_net=ifelse(nights_sleep_net=='dk',NA,nights_sleep_net)) %>%
  mutate(nights_sleep_net=as.numeric(nights_sleep_net)) %>% 
  group_by(hhid,visit) %>% 
  summarise(average_nights_last_week=median(nights_sleep_net,na.rm = TRUE))
#graph of how many households used bed nets over the past week, faceted by visit
ggplot(data=hh_average_usage,aes(x=average_nights_last_week))+
  geom_histogram()+
  facet_wrap(~visit)
#graph showing % of hh that slept under a bed last night 
ggplot(data=hh_last_usage,aes(x=sleep_net_last_night,y=percent_last_usage))+
  geom_col()+
  facet_wrap(~visit)





#Usage on an individual level needs to be found
effi_usage<-kenya_efficacy_total %>%
  group_by(hhid,visit) %>% 
  mutate(sleep_under_net_last_night=(sleep_under_net_last_night=='yes'))

effi_last_usage<-effi_usage %>% 
  group_by(visit,sleep_under_net_last_night) %>% 
  filter(!is.na(sleep_under_net_last_night)) %>%
  tally() %>% 
  mutate(percent_last_usage=(n/sum(n))*100)

effi_average_usage<-effi_usage %>%
  mutate(num_nights_sleep_under_net=ifelse(num_nights_sleep_under_net=='dk',NA,num_nights_sleep_under_net)) %>%
  mutate(num_nights_sleep_under_net=as.numeric(num_nights_sleep_under_net)) %>% 
  group_by(hhid,visit) %>% 
  summarise(average_nights_last_week=median(num_nights_sleep_under_net,na.rm = TRUE))
#graph of how many households used bed nets over the past week, faceted by visit
ggplot(data=effi_average_usage,aes(x=average_nights_last_week))+
  geom_histogram()+
  facet_wrap(~visit)
#graph showing % of individual that slept under a bed last night 
ggplot(data=effi_last_usage,aes(x=sleep_under_net_last_night,y=percent_last_usage))+
  geom_col()+
  facet_wrap(~visit)

