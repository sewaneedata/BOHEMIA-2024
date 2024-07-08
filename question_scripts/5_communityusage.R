#PURPOSE:Scatterplot of efficacy data where the x-axis is malaria incidents and y-axis is percent usage and coloured by villages. 

#Load Libraries
library(gsheet)
library(tidyverse)
library(dplyr)
library(gsheet)
#Load data

source('data.r')

kenya_safety_summary<-kenya_safety_eff %>% 
  arrange((visit)) %>% 
  group_by(visit, sleep_ln_safety) %>% 
  tally

kenya_safety_summary<-na.omit(kenya_safety_summary) %>% 
  pivot_wider(names_from = sleep_ln_safety, values_from=n) %>% 
  rename('yes_safety'=yes) %>% 
  rename('no_safety'=no)

kenya_effi_summary<-kenya_safety_eff %>% 
  arrange((visit)) %>% 
  group_by(visit, sleep_ln_eff) %>% 
  tally
kenya_effi_summary<-na.omit(kenya_effi_summary) %>% 
  pivot_wider(names_from=sleep_ln_eff, values_from=n) %>% 
  rename('yes_eff'=yes) %>% 
  rename('no_eff'=no)

kenya_safety_eff_summ<-kenya_safety_summary %>% 
  left_join(kenya_effi_summary, by='visit')

kenya_safety_eff_long <- kenya_safety_eff_summ %>%
  pivot_longer(cols = c('no_safety', 'yes_safety', 'no_eff', 'yes_eff'), 
               names_to = 'y_n', 
               values_to = 'number')

ggplot(kenya_safety_eff_long, aes(x = visit, y = number, fill =y_n)) +
  geom_col(position = "dodge") +
  labs(x = "Visit", y = "Count", fill = "Category", title = "Safety and Efficiency by Visit") +
  theme_minimal()

