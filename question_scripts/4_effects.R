# PURPOSE: show the environmental factors on bednet usage


#Load Libraries
library(gsheet)
library(tidyverse)
library(dplyr)
library(gsheet)
library(lubridate)
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
  labs(x = "Visit", y = "Percentage", fill = "Category", title = "Safety and Efficacy by Bi-Weekly Visit") +
  theme_minimal()+
  ylim(0,100)


#getting weather data and adding month
weather_k <- weather %>%
  mutate(month1 = ymd(paste0(month, "-01"))) %>% 
  mutate(month = case_when(
    month(month1) == 1  ~ "January",
    month(month1) == 2  ~ "February",
    month(month1) == 3  ~ "March",
    month(month1) == 4  ~ "April",
    month(month1) == 5  ~ "May",
    month(month1) == 6  ~ "June",
    month(month1) == 7  ~ "July",
    month(month1) == 8  ~ "August",
    month(month1) == 9  ~ "September",
    month(month1) == 10 ~ "October",
    month(month1) == 11 ~ "November",
    month(month1) == 12 ~ "December"
  ))

#selecting only the months from the survey
weather_k<-weather_k %>% 
  filter(month %in% c('October', 'November', 'December', 'January'))


#getting average temp for each month
weather_k_t<-weather_k %>% 
  group_by(month) %>% 
  summarize(avg_temp=mean(((LST_Day+LST_Night)/2)-273.15))
#getting sum of prcp for each month
weather_k_p<-weather_k %>% 
  group_by(month) %>% 
  summarize(sum_prcp=mean(rain.mm.accum, na.rm=TRUE))

#adding the two tables
weather_t_p<-weather_k_p %>% 
  left_join(weather_k_t, by='month')


#arranging months in order
weather_t_p$month <- factor(weather_t_p$month, 
                                      levels = c("October", "November", "December", "January"))


#creating graph for temp
ggplot(weather_t_p, aes(x=month, y = avg_temp, group=1)) +
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
weather_t_p$avg_temp_rescaled <- rescale(weather_t_p$avg_temp, to = range(weather_t_p$sum_prcp))


#creating the plot
print(ggplot(weather_t_p, aes(x = month)) +
  geom_line(aes(y = avg_temp_rescaled, color='Temperature'), group=1) +
  geom_line(aes(y = sum_prcp, color='Precipitation'), group=1) +
  geom_col(
    data = kenya_safety_eff_long, 
    aes(y = number / max(number) * max(weather_t_p$sum_prcp),  # Rescale and normalize safety data
        fill = percent_y_n), 
    position = "dodge", 
    alpha=.5
  ) +
  scale_y_continuous(
    name = "Precipitation (mm)", 
    sec.axis = sec_axis(
      trans = ~ (((. /100*.2))+26.4),
      name = "Temperature (Celsius)"
    ),
    limits = c(0, max(weather_t_p$avg_temp_rescaled) * 1)
  ) +
  scale_color_manual(values = c("Temperature" = "#9666B2", 
                                "Precipitation" = "#000000"))+
  scale_fill_manual(values = c("percent_yes_eff" = "#4B0082", "percent_yes_safety" = "#9666B2"),
                      labels = c("Percent Usage in Efficacy", "Percent Usage in Safety"),  # Match legend labels to axis
                      name = "") +   # Update legend title
  labs(color='', x = "Month", fill = "Category", title = "Environmental Factors vs Usage") +
  theme_minimal())


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

# changing month name
weather_e <- weather %>%
  mutate(month1 = ymd(paste0(month, "-01"))) %>% 
  mutate(month = case_when(
    month(month1) == 1  ~ "January",
    month(month1) == 2  ~ "February",
    month(month1) == 3  ~ "March",
    month(month1) == 4  ~ "April",
    month(month1) == 5  ~ "May",
    month(month1) == 6  ~ "June",
    month(month1) == 7  ~ "July",
    month(month1) == 8  ~ "August",
    month(month1) == 9  ~ "September",
    month(month1) == 10 ~ "October",
    month(month1) == 11 ~ "November",
    month(month1) == 12 ~ "December"
  ))

# taking specific months out from the dataset
weather_e<-weather_e %>% 
  filter(month %in% c('October', 'November', 'December', 'January', 'February', 'March', 'April'))


#getting sum of temp for each month
weather_e_t<-weather_e %>% 
  group_by(month) %>% 
  summarize(avg_temp=mean(((LST_Day+LST_Night)/2), na.rm=TRUE))

weather_e %>% 
  filter(month=='April') %>% 
  group_by(month, rain.mm.accum) %>% 
  tally
#getting sum of prcp for each month
weather_e_p<-weather_e %>% 
  group_by(month) %>% 
  summarize(sum_prcp=mean(rain.mm.accum, na.rm=TRUE))

#adding the two tables
weather_e_t_p<-weather_e_p %>% 
  left_join(weather_e_t, by='month')

#arranging months in order
weather_e_t_p$month <- factor(weather_e_t_p$month, 
                            levels = c("October", "November", "December", "January", "February", "March", "April"))

weather_e_t_p$avg_temp_rescaled <- rescale(weather_e_t_p$avg_temp, to = range(weather_e_t_p$sum_prcp))

# same thing but different dataset
ggplot(weather_e_t_p, aes(x = month)) +
  geom_line(aes(y = avg_temp_rescaled, color = "Temperature"), group = 1, size = 1.2) +
  geom_line(aes(y = sum_prcp, color = "Precipitation"), group = 1, linetype = "dashed") + 
  geom_col(
    data = diff_efficacy_long,
    aes(y = n / max(n) * max(weather_e_t_p$avg_temp_rescaled),  # Rescale to temperature
        fill = sleep_under_net_last_night), 
    position = "stack",  
    alpha = 0.6
  ) +
  scale_y_continuous(
    name = "Temperature (Celsius)",   # Primary axis is temperature
    sec.axis = sec_axis(
      trans = ~ . / (max(weather_e_t_p$avg_temp_rescaled) / max(weather_e_t_p$sum_prcp)),  # Transformation to precipitation
      name = "Precipitation (mm)"      # Secondary axis is precipitation
    )
  ) +
  scale_fill_manual(values = c("yes" = "#5C2D91", "no" = "#D6B2CC"),  # Sewanee purple shades
                    name = "Slept Under Net Last Night",
                    labels = c("Yes", "No")) +          # Explicit labels for the legend
  scale_color_manual(values = c("Temperature" = "#9666B2", "Precipitation" = "#4B0082")) + # Adjusted line colors
  labs(x = "Month", title = "Weather, Safety, and Efficacy Over Time (monthly)",
       subtitle = 'People Using Bed Nets in Numbers (Rescaled)') + # Updated subtitle
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5), 
    axis.title = element_text(size = 12),  
    axis.text = element_text(size = 10),  
    legend.title = element_text(size = 11), 
    legend.position = "bottom",           
    panel.grid.minor = element_blank()    
  )

# 
kenya_effsum_long$month <- factor(kenya_effsum_long$month, 
                              levels = c("October", "November", "December", "January", "February", "March", "April"))


#PERCENTAGE GRAPH

# customizing ggplot
p<-ggplot(weather_e_t_p, aes(x = month)) +
  geom_line(aes(y = avg_temp_rescaled, color = 'Temperature'), group = 1, size = 1.2) +
  geom_line(aes(y = sum_prcp, color = 'Precipitation'), group = 1, linetype = "dashed") + # Dashed line for precipitation
  geom_col(
    data = kenya_effsum_long,
    aes(y = number / max(number) * max(weather_e_t_p$sum_prcp)), 
    position = position_dodge(width = 0.7),  
    alpha = 0.6,                           
    fill = "#5C2D91"                       
  ) +
  scale_y_continuous(
    name = "Precipitation (mm)",
    sec.axis = sec_axis(
      trans = ~ (((. /10)*.04)+26.8),
      name = "Temperature (Celsius)"
    ),
    limits = c(0, max(weather_e_t_p$avg_temp_rescaled) * 1)
  ) +
  scale_color_manual(values = c("Temperature" = "#9666B2", 
                                "Precipitation" = "#000000")) + 
  labs(x = "Month", fill = "Category", title = "Environmental Factors vs. Bed Net Usage", color='') +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5), 
    axis.title = element_text(size = 12),  
    axis.text = element_text(size = 10),  
    legend.title = element_text(size = 11), 
    legend.position = "bottom",           
    panel.grid.minor = element_blank()  
  )

# add labels to month
labels<-c("October\n71.65%", "November\n68.32%", "December\n67.88%", "January\n67.22%", "February\n67.12%", "March\n67.20%", "April\n82.14%")

# join the label in p(ggplot)
print("Environmental Effects on Bed Nets:Temperature and Precipitation ")
print(p+scale_x_discrete(label=labels))


