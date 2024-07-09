# PURPOSE:


#Load Libraries
library(gsheet)
library(tidyverse)
library(dplyr)
library(gsheet)
#Load data

source('data.r')

# Scatterplot of efficacy data where the x-axis is temperature and y-axis is percent usage, Coloured by villages and faceted by visits. 

#getting data on the number of people slept under bed nets from safety
kenya_safety_summary<-kenya_safety_eff %>% 
  arrange((visit)) %>% 
  group_by(visit, sleep_ln_safety) %>% 
  tally

#renaming and making percentage of people sleeping under bed nets
kenya_safety_summary<-na.omit(kenya_safety_summary) %>% 
  pivot_wider(names_from = sleep_ln_safety, values_from=n) %>% 
  rename('yes_safety'=yes) %>% 
  rename('no_safety'=no)%>% 
  mutate('percent_yes_safety'=(yes_safety/(no_safety+yes_safety))*100)

#Doing it for efficacy(number of people)
kenya_effi_summary<-kenya_safety_eff %>% 
  arrange((visit)) %>% 
  group_by(visit, sleep_ln_eff) %>% 
  tally
#getting percentage for efficacy
kenya_effi_summary<-na.omit(kenya_effi_summary) %>% 
  pivot_wider(names_from=sleep_ln_eff, values_from=n) %>% 
  rename('yes_eff'=yes) %>% 
  rename('no_eff'=no) %>% 
  mutate('percent_yes_eff'=(yes_eff/(no_eff+yes_eff))*100)

#adding the two tables
kenya_safety_eff_summ<-kenya_safety_summary %>% 
  left_join(kenya_effi_summary, by='visit')

#Adding months to the data 
kenya_safety_eff_long <- kenya_safety_eff_summ %>%
  select(percent_yes_safety, percent_yes_eff) %>% 
  pivot_longer(cols = c('percent_yes_safety', 'percent_yes_eff'), 
               names_to = 'percent_y_n', 
               values_to = 'number') %>% 
  mutate('month'= case_when( visit == 'V1' ~ "October",
                             visit == 'V2' ~ "November",
                             visit == 'V3' ~ "December",
                             visit=="V4" ~ "January"))


#arranging months in order
kenya_safety_eff_long$month <- factor(kenya_safety_eff_long$month, 
                                      levels = c("October", "November", "December", "January"))

#creating a plot for the percentage of people
ggplot(kenya_safety_eff_long, aes(x = month, y = number, fill =percent_y_n)) +
  geom_col(position = "dodge") +
  labs(x = "Visit", y = "Count", fill = "Category", title = "Safety and Efficacy by Visit") +
  theme_minimal()+
  ylim(0,100)


#getting weather data and adding month
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

#selecting only the months from the survey
weather_k<-weather_k %>% 
  filter(month %in% c('October', 'November', 'December', 'January'))


#getting sum of temp for each month
weather_k_t<-weather_k %>% 
  group_by(month) %>% 
  summarize(sum_temp=sum(TAVG))
#getting sum of prcp for each month
weather_k_p<-weather_k %>% 
  group_by(month) %>% 
  summarize(sum_prcp=sum(PRCP, na.rm=TRUE))

#adding the two tables
weather_t_p<-weather_k_p %>% 
  left_join(weather_k_t, by='month')

str(weather_t_p)

#arranging months in order
weather_t_p$month <- factor(weather_t_p$month, 
                                      levels = c("October", "November", "December", "January"))


#creating graph for temp
ggplot(weather_t_p, aes(x=month, y = sum_temp, group=1)) +
  geom_line()

#creating graph for prcp
ggplot(weather_t_p, aes(x=month, y = sum_prcp, group=1)) +
  geom_line()

#used for rescaling
library(scales) 



#DR RUDD PLOT
# p1 <- ggplot( ) +
#   geom_col( data = ), aes( x= month, y=total ), fill="grey70", alpha = 0.6 ) +
#   labs( y = "Rainfall (mm)" ) +
#   theme_bw( base_size = 12 ) +
#   theme( axis.line = element_line() )
# 
# p2 <- ggplot() +
#   geom_line( data = monthly_mosquito_totals %>% filter( species == "all" ), aes(x=month, y=sum, color = species, linetype=assignment ), linewidth=0.75, alpha = 0.6 ) +
#   geom_point( data = monthly_mosquito_totals %>% filter( species == "all" ), aes(x=month, y=sum, color = species ), size=1, alpha = 0.6 ) +
#   labs( y = "Mosquito population" ) +
#   theme_bw( base_size = 12 ) +
#   theme( axis.line = element_line() )
# 
# p3 <- ggplot( ) +
#   geom_col( data = monthly_rainfall %>% filter( month >= min( monthly_mosquito_totals$month ) ), aes( x= month, y=total/1000 ), fill="grey70", alpha = 0.6 ) +
#   geom_line( data = monthly_mosquito_totals %>% filter( species == "all" ), aes(x=month, y=sum/6000, color = species, linetype=assignment ), linewidth=0.75, alpha = 0.6 ) +
#   geom_point( data = monthly_mosquito_totals %>% filter( species == "all" ), aes(x=month, y=sum/6000, color = species ), size=1, alpha = 0.6 ) +
#   geom_line( data = prevalence, aes(x=mean_visit_date, y=prevalence, linetype = factor(assignment)), linewidth=1) +
#   geom_point( data = prevalence, aes(x=mean_visit_date, y=prevalence), size=2 ) +
#   expand_limits(y=0) + 
#   # scale_x_continuous( breaks = unique( rates_by_assignment_and_visit$Visit ) ) +
#   labs( y= "Prevalence", linetype = "Assignment", color = "species") + 
#   theme_bw( base_size = 12 ) +
#   theme( legend.position = "bottom",
#          legend.box = "horizontal",
#          legend.margin = margin() )


#rescaling prcp to temp
weather_t_p$sum_prcp_rescaled <- rescale(weather_t_p$sum_prcp, to = range(weather_t_p$sum_temp))


#creating the plot
ggplot(weather_t_p, aes(x = month)) +
  geom_line(aes(y = sum_temp, color='Temperature'), group=1) +
  geom_line(aes(y = sum_prcp_rescaled, color='Precipitation'), group=1) +
  geom_col(
    data = kenya_safety_eff_long, 
    aes(y = number / max(number) * max(weather_t_p$sum_temp),  # Rescale and normalize safety data
        fill = percent_y_n), 
    position = "dodge", 
    alpha=.5
  ) +
  scale_y_continuous(
    name = "Temperature", 
    sec.axis = sec_axis(
      trans = function(x) { 
        x * max(weather_t_p$sum_prcp) / max(weather_t_p$sum_temp)
      },
      name = "Precipitation" 
    )
  ) +
  scale_color_manual(values = c("Temperature" = "red", "Precipitation" = "black"))+
  labs(x = "Month", fill = "Category", title = "Weather, Safety, and Efficacy Over Time", subtitle = 'People Using Bed Nets is in percentage.') +
  theme_minimal()


#The same thing for efficacy over 6 months
kenya_effi_summary_n<-kenya_efficacy_total %>% 
  arrange((visit)) %>% 
  group_by(visit, sleep_under_net_last_night ) %>% 
  tally
#getting percentage for efficacy
kenya_effi_summary_n<-na.omit(kenya_effi_summary_n) %>% 
  pivot_wider(names_from=sleep_under_net_last_night, values_from=n) %>% 
  rename('yes_eff'=yes) %>% 
  rename('no_eff'=no) %>% 
  mutate('percent_yes_eff'=(yes_eff/(no_eff+yes_eff))*100)

#Adding months to the data to make percentage graph
kenya_effsum_long <- kenya_effi_summary_n %>%
  select(percent_yes_eff)  %>% 
  pivot_longer(cols = c('percent_yes_eff'), 
               names_to = 'y_n', 
               values_to = 'number') %>% 
  mutate('month'= case_when( visit == 'V1' ~ "October",
                             visit == 'V2' ~ "November",
                             visit == 'V3' ~ "December",
                             visit=="V4" ~ "January",
                             visit=='V5' ~ "February",
                             visit=='V6' ~ "March", 
                             visit=='V7' ~ "April"))



#USING CODE FROM 3 HERE for SUM GRAPH
diff_efficacy_long<-kenya_efficacy_total %>% 
  mutate(sleep_under_net_last_night=ifelse(sleep_under_net_last_night=='dk', NA, sleep_under_net_last_night)) %>% 
  drop_na(sleep_under_net_last_night) %>% 
  group_by(visit, sleep_under_net_last_night) %>% 
  tally 

diff_efficacy_long<-diff_efficacy_long %>% 
  mutate('month'= case_when( visit == 'V1' ~ "October",
                             visit == 'V2' ~ "November",
                             visit == 'V3' ~ "December",
                             visit=="V4" ~ "January",
                             visit=='V5' ~ "February",
                             visit=='V6' ~ "March", 
                             visit=='V7' ~ "April"))


weather_e <- weather %>%
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

weather_e<-weather_e %>% 
  filter(month %in% c('October', 'November', 'December', 'January', 'February', 'March', 'April'))


#getting sum of temp for each month
weather_e_t<-weather_e %>% 
  group_by(month) %>% 
  summarize(sum_temp=sum(TAVG, na.rm=TRUE))
#getting sum of prcp for each month
weather_e_p<-weather_e %>% 
  group_by(month) %>% 
  summarize(sum_prcp=sum(PRCP, na.rm=TRUE))

#adding the two tables
weather_e_t_p<-weather_e_p %>% 
  left_join(weather_e_t, by='month')

#arranging months in order
weather_e_t_p$month <- factor(weather_e_t_p$month, 
                            levels = c("October", "November", "December", "January", "February", "March", "April"))

weather_e_t_p$sum_prcp_rescaled <- rescale(weather_e_t_p$sum_prcp, to = range(weather_e_t_p$sum_temp))


ggplot(weather_e_t_p, aes(x = month)) +
  geom_line(aes(y = sum_temp, color='Temperature'), group=1) +
  geom_line(aes(y = sum_prcp_rescaled, color='Precipitation'), group=1) +
  geom_col(
    data=diff_efficacy_long,
    aes(y = n / max(n) * max(weather_e_t_p$sum_temp),  # Rescale and normalize safety data
        fill = sleep_under_net_last_night), 
    position = "stack", 
    alpha=.5
  ) +
  scale_y_continuous(
    name = "Temperature", 
    sec.axis = sec_axis(
      trans = function(x) { 
        x * max(weather_e_t_p$sum_prcp) / max(weather_e_t_p$sum_temp)
      },
      name = "Precipitation" 
    )
  ) +
  scale_color_manual(values = c("Temperature" = "red", "Precipitation" = "black"))+
  labs(x = "Month", fill = "Category", title = "Weather, Safety, and Efficacy Over Time", subtitle = 'People Using Bed Nets is in percentage.') +
  theme_minimal()

#SUM GRAPH
ggplot(weather_e_t_p, aes(x = month)) +
  geom_line(aes(y = sum_temp, color='Temperature'), group=1) +
  geom_line(aes(y = sum_prcp_rescaled, color='Precipitation'), group=1) +
  geom_col(
    data=diff_efficacy_long,
    aes(y = n / max(n) * max(weather_e_t_p$sum_temp),  # Rescale and normalize safety data
        fill = sleep_under_net_last_night), 
    position = "stack", 
    alpha=.5
  ) +
  scale_y_continuous(
    name = "Temperature", 
    sec.axis = sec_axis(
      trans = function(x) { 
        x * max(weather_e_t_p$sum_prcp) / max(weather_e_t_p$sum_temp)
      },
      name = "Precipitation" 
    )
  ) +
  scale_color_manual(values = c("Temperature" = "red", "Precipitation" = "black"))+
  labs(x = "Month", fill = "Category", title = "Weather, Safety, and Efficacy Over Time", subtitle = 'People Using Bed Nets is in percentage.') +
  theme_minimal()


#PERCENTAGE GRAPH
ggplot(weather_e_t_p, aes(x = month)) +
  geom_line(aes(y = sum_temp, color='Temperature'), group=1) +
  geom_line(aes(y = sum_prcp_rescaled, color='Precipitation'), group=1) +
  geom_col(
    data=kenya_effsum_long,
    aes(y = n / max(n) * max(weather_e_t_p$sum_temp)),   
# Rescale and normalize safety data 
    position = "dodge", 
    alpha=.5
  ) +
  scale_y_continuous(
    name = "Temperature", 
    sec.axis = sec_axis(
      trans = function(x) { 
        x * max(weather_e_t_p$sum_prcp) / max(weather_e_t_p$sum_temp)
      },
      name = "Precipitation" 
    )
  ) +
  scale_color_manual(values = c("Temperature" = "red", "Precipitation" = "black"))+
  labs(x = "Month", fill = "Category", title = "Weather, Safety, and Efficacy Over Time", subtitle = 'People Using Bed Nets is in percentage.') +
  theme_minimal()
