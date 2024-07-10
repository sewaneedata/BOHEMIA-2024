#PURPOSE:Scatterplot of efficacy data where the x-axis is malaria incidents and y-axis is percent usage and coloured by clusters 

#Load Libraries
library(gsheet)
library(tidyverse)
library(dplyr)
library(gsheet)
#Load data

source('data.r')

# make a new data set called Malaria incidence 
mal<-mal_incidence %>% 
  group_by(cluster, visit) %>% 
  mutate(incident_case=as.numeric(incident_case)) %>% 
  drop_na(incident_case) %>% 
  summarize('incidents'=sum(incident_case))

# take out cluster, visit and sleep_net_last_night from kenya_safety_total
safety_c<-kenya_safety_total %>%
  group_by(cluster, visit, sleep_net_last_night) %>% 
  tally

# remove nas
safety_c<-na.omit(safety_c)  

# pivoted wider from values column
safety_c<-safety_c %>% 
  pivot_wider(names_from = sleep_net_last_night, values_from = n) 

# if na, put zero
safety_c[is.na(safety_c)] <- 0

#make safety_c_m from safety_c
safety_c_m<-safety_c %>% 
  mutate(sum=yes+no) %>% 
  mutate(percent_usage=(yes/sum)*100) %>% 
  left_join(mal, by=c('cluster', 'visit'))

#make a plot showing the correlation between bednet usage and malaria incidence
ggplot(data=safety_c_m, aes(x=incidents, y=percent_usage))+
  geom_point()+
  labs(title='scatter plot of malaria incidents vs percent usage for each visit')+
  theme_minimal()+
  facet_wrap(~visit)

