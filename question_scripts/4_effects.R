# PURPOSE:


#Load Libraries
library(gsheet)
library(tidyverse)
library(dplyr)
library(gsheet)
#Load data

source('data.r')

# Scatterplot of efficacy data where the x-axis is temperature and y-axis is percent usage, Coloured by villages and faceted by visits. 

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
  select(yes_safety, yes_eff) %>% 
  pivot_longer(cols = c('yes_safety', 'yes_eff'), 
               names_to = 'y_n', 
               values_to = 'number') %>% 
  mutate('month'= case_when( visit == 'V1' ~ "October",
                             visit == 'V2' ~ "November",
                             visit == 'V3' ~ "December",
                             visit=="V4" ~ "January"))


kenya_safety_eff_long$month <- factor(kenya_safety_eff_long$month, 
                                      levels = c("October", "November", "December", "January"))

ggplot(kenya_safety_eff_long, aes(x = month, y = number, fill =y_n)) +
  geom_col(position = "dodge") +
  labs(x = "Visit", y = "Count", fill = "Category", title = "Safety and Efficacy by Visit") +
  theme_minimal()


weather_k <- weather %>%
  mutate(month = case_when(
    month(DATE) == 1  ~ "January",
    month(DATE) == 2  ~ "February",
    month(DATE) == 3  ~ "March",
    month(DATE) == 4  ~ "April",
    month(DATE) == 5  ~ "May",
    month(DATE) == 6  ~ "June",
    month(DATE) == 7  ~ "July",
    month(DATE) == 8  ~ "August",
    month(DATE) == 9  ~ "September",
    month(DATE) == 10 ~ "October",
    month(DATE) == 11 ~ "November",
    month(DATE) == 12 ~ "December"
  ))

weather_k<-weather_k %>% 
  filter(month %in% c('October', 'November', 'December', 'January'))


weather_k_t<-weather_k %>% 
  group_by(month) %>% 
  summarize(avg_temp=mean(TAVG))

weather_k_p<-weather_k %>% 
  group_by(month) %>% 
  summarize(avg_prcp=mean(PRCP, na.rm=TRUE))

weather_t_p<-weather_k_p %>% 
  left_join(weather_k_t, by='month')

str(weather_t_p)

weather_t_p$month <- factor(weather_t_p$month, 
                                      levels = c("October", "November", "December", "January"))


ggplot(weather_t_p, aes(x=month, y = avg_temp, group=1)) +
  geom_line()

ggplot(weather_t_p, aes(x=month, y = avg_prcp, group=1)) +
  geom_line()

library(scales) 
weather_t_p_long <- weather_t_p %>%
  pivot_longer(cols = c(avg_prcp, avg_temp), 
               names_to = "variable",       
               values_to = "value") 

weather_t_p_long$value <- rescale(weather_t_p_long$value)
kenya_safety_eff_re<-kenya_safety_eff_long
kenya_safety_eff_long$number <- rescale(kenya_safety_eff_long$number)


# Plot
ggplot(kenya_safety_eff_long, aes(x = month)) +
  geom_col(aes(y = number, fill = y_n, alpha=.5), position = "dodge") +
  geom_line(data = weather_t_p_long, aes(y = value, color = variable, group = variable)) +
  labs(x = "Month", 
       y = "Values and Bars", 
       fill = "Efficacy v Safety", 
       color = "Weather Variable",
       title = "Combined Safety/Efficacy and Weather Data")  +
  theme_minimal() 



