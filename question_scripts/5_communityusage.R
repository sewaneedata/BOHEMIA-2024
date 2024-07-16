#PURPOSE: Make scatterplot of efficacy data where the x-axis is malaria incidents and y-axis is percent usage and this is coloured by clusters 

# OUTPUTS: mal_inc_vs_usage and scatter_usage

#Load Libraries
library(gsheet)
library(tidyverse)
library(dplyr)
library(gsheet)

#Load data
source('data.R')

# make a new data set called Malaria incidence 
mal<-mal_incidence %>% 
  group_by(cluster, visit) %>% 
  mutate(incident_case=as.numeric(incident_case)) %>% 
  drop_na(incident_case) %>% 
  summarize('incidents'=sum(incident_case))

# look at cluster, visit and sleep_net_last_night from kenya_safety_total
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

#make safety_c_m from safety_c which tells us the percentage of people who did use a bed net last night
safety_c_m<-safety_c %>% 
  mutate(sum=yes+no) %>% 
  mutate(percent_usage=(yes/sum)*100) %>% 
  left_join(mal, by=c('cluster', 'visit')) %>% 
  # edit the x and y axis names
  rename(Incidents = incidents,
         'Percent Usage (%)' = percent_usage)

#make a plot showing the correlation between bednet usage and malaria incidence
#print('Bed Net Usage and Malaria Incidence by each visit in safety data')
mal_inc_vs_usage <- ggplot(data=safety_c_m, aes(x=Incidents, y=`Percent Usage (%)`))+
  geom_point(color="#4B0082") +
  labs(title='Scatter plot of malaria incidents against percent usage for each visit')+
  theme_minimal()+
  facet_wrap(~visit) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none" 
  )


# create a dataset that says the average amount of people who did and did not use bednet last night
safety_cv<-safety_c %>% 
  group_by(cluster) %>% 
  mutate(avg_yes=(sum(yes)/4)) %>% 
  mutate(avg_no=(sum(no)/4)) %>% 
  select(-yes, -no, -visit) %>% 
  distinct


# create a dataset that says how many incidents there were
mal_v<-mal %>% 
  filter(visit %in% c('V1', 'V2', 'V3', 'V4')) %>% 
  group_by(cluster) %>% 
  mutate(total_incidents=sum(incidents)) %>% 
  select(-visit, -incidents) %>% 
  distinct

# make a dataset that says what cluster is a part of each village
village<-kenya_demography %>% 
  select(cluster, village)

# join the above to find the incidents and bed net usage in each village
safety_c_m_v<-mal_v %>% 
  left_join(safety_cv, by='cluster') %>% 
  left_join(village, by='cluster') %>% 
  mutate(sum=avg_yes+avg_no) %>% 
  mutate(percent_usage=(avg_yes/sum)*100) %>% 
  distinct


#print('Scatter plot of safety data, averaging visits into total % usage')
scatter_usage <- ggplot(safety_c_m_v, aes(x = total_incidents, y = percent_usage, color = village)) +
  geom_point(size = 1.5, alpha = 0.8) + 
  scale_color_manual(values = colorRampPalette(c("#4B0082", "#9666B2"))(length(unique(safety_c_m_v$village)))) +
  labs(title = "Scatter Plot of Malaria Incidents vs. Percent Bed Net Usage",
       x = "Total Malaria Incidents",
       y = "Percent Bed Net Usage",
       caption='Colored by Villages') +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none" 
  )



