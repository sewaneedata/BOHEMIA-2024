#PURPOSE:Scatterplot of efficacy data where the x-axis is malaria incidents and y-axis is percent usage and coloured by clusters 

#Load Libraries
library(gsheet)
library(tidyverse)
library(dplyr)
library(gsheet)
#Load data

source('data.r')

mal<-mal_incidence %>% 
  group_by(cluster, visit) %>% 
  mutate(incident_case=as.numeric(incident_case)) %>% 
  drop_na(incident_case) %>% 
  summarize('incidents'=sum(incident_case))

safety_c<-kenya_safety_total %>%
  group_by(cluster, visit, sleep_net_last_night) %>% 
  tally

safety_c<-na.omit(safety_c)  

safety_c<-safety_c %>% 
  pivot_wider(names_from = sleep_net_last_night, values_from = n) 

safety_c[is.na(safety_c)] <- 0

safety_c_m<-safety_c %>% 
  mutate(sum=yes+no) %>% 
  mutate(percent_usage=(yes/sum)*100) %>% 
  left_join(mal, by=c('cluster', 'visit'))

ggplot(data=safety_c_m, aes(x=incidents, y=percent_usage, color=cluster))+
  geom_point()+
  facet_wrap(~visit)+
  labs(title='scatter plot of malaria incidents vs percent usage for each visit')+
  theme_minimal()

