#Load Libraries
library(gsheet)
library(tidyverse)
library(dplyr)
library(gsheet)
#Load data

source('data.r')

# PURPOSE:to know if people said yes to sleeping under net last night, they likely sleep more nights under net within last week
# Box plot of efficacy usage data by visit and discussion of statistical findings. 

#table of number of nights slept under net last week for efficacy
table(kenya_efficacy_total$num_nights_sleep_under_net)



#changing dk to NA, dropping NA, and turning number of nights to numeric
diff_efficacy_num<-kenya_efficacy_total %>% 
  mutate(num_nights_sleep_under_net=ifelse(num_nights_sleep_under_net=='dk', NA, num_nights_sleep_under_net)) %>% 
  drop_na(num_nights_sleep_under_net) %>%
  mutate( num_nights_sleep_under_net = as.numeric( num_nights_sleep_under_net ) )

#Creating histogram of number of nights faceted by visit
ggplot( diff_efficacy_num ) + 
  geom_histogram(aes(x=num_nights_sleep_under_net)) + 
  facet_wrap(~visit)+
  labs(
    title='Histogram of Number of Nights Slept Under Net for each Visit'
  )

#creating boxplot of number of nights
ggplot(data=diff_efficacy_num, aes(y=num_nights_sleep_under_net, x=visit))+
  geom_boxplot()+
  labs(
    title='Boxplot of Number of Nights Slept Under Net for each Visit'
  )

#creating table of slept under bed net last night
table(kenya_efficacy_total$sleep_under_net_last_night)

#changing dk to NA, dropping NA, and grouping by visit and sleep under net , then tally to see number of people in each visit who said y/n
diff_efficacy_long<-kenya_efficacy_total %>% 
  mutate(sleep_under_net_last_night=ifelse(sleep_under_net_last_night=='dk', NA, sleep_under_net_last_night)) %>% 
  drop_na(sleep_under_net_last_night) %>% 
  group_by(visit, sleep_under_net_last_night) %>% 
   tally 



# Creating the plot of sleep under bed net colored by y/n
differences_usage <- (ggplot(diff_efficacy_long, aes(x = visit, y = n, fill = sleep_under_net_last_night)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("yes" = "#4B0082", "no" = "#9666B2"),
                    labels = c("Yes", "No"),  # Match legend labels to axis
                    name = "") +
  labs(y = "Count", x = "Visits", fill = "Type") +
  theme_minimal()+
  labs(
    title=' Slept under bed net last night for each visit'
  )+
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5), 
      axis.title = element_text(size = 12),  
      axis.text = element_text(size = 10),  
      legend.title = element_text(size = 11), 
      legend.position = "bottom",           
      panel.grid.minor = element_blank()  
    ))

# creating a new dataset which is grouped by visit
diff_total <- diff_efficacy_long %>%
  group_by(visit)
# Combining this with the previous dataset and then reordering by y/n
diff_efficacy_total <- bind_rows(diff_efficacy_long, diff_total) %>%
  mutate(sleep_under_net_last_night = factor(sleep_under_net_last_night, levels = c('yes', 'no'))) %>% 
  distinct() 

# Create a summary of grouped by visit and number of nights slept under bed net
diff_efficacy_numt <- diff_efficacy_num %>%
  group_by(visit, num_nights_sleep_under_net) %>%
  tally

#average for number of nights sleept under net within last week
diff_eff_avg_n<-diff_efficacy_numt %>% 
  group_by(num_nights_sleep_under_net) %>% 
  summarize(avg_num=mean(n))

# average for count of sleep under net last night
diff_eff_avg<-diff_efficacy_total %>% 
  group_by(sleep_under_net_last_night) %>% 
  summarize(avg_visit=mean(n))

# Combined plot of slept under bed net last night v number of nights slept under bed net


library(RColorBrewer)  # For color palettes

# Graph for showing average count of sleep under a net last night and average number of nights sleep under net within last week
ggplot() +
  geom_col(data = diff_eff_avg,
           aes(x = ifelse(sleep_under_net_last_night == "yes", 7, 0),  # Swap x values
               y = avg_visit, fill = sleep_under_net_last_night),
           alpha = 0.8,
           width = 0.8) +
  geom_line(data = diff_eff_avg_n,
            aes(x = num_nights_sleep_under_net, y = avg_num),
            color = "#5C2D91",
            size = 1.2) +
  scale_fill_manual(values = c("yes" = "#4B0082", "no" = "#9666B2"),
                    labels = c("Yes", "No"),  # Match legend labels to axis
                    name = "") +   # Update legend title
  scale_x_continuous(breaks = c(0, 7),
                     labels = c("No", "Yes"),  # Swap labels to match bars
                     sec.axis = sec_axis(~ .,
                                         breaks = 0:7,
                                         labels = 0:7)) +
  ##9666B2
  labs(y = "Count",
       x = "Sleep Under Net Last Night vs Last Week",
       title = 'Nightly vs Weekly Bed Net Usage', 
       fill='') +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        legend.position = "bottom") 


#creating a new dataset with distinct rows and pivoting wide for sleep under net last night
diff_efficacy_total_table <- diff_efficacy_total %>%
  distinct() %>% 
  pivot_wider(names_from = sleep_under_net_last_night, values_from = n) %>% 
  distinct()

# creating a new datset with num_nights sleep under net and pivoting wide
diff_efficacy_numt_table <- diff_efficacy_numt %>%
  pivot_wider(names_from = num_nights_sleep_under_net, values_from = n)


#combining both tables

combined_table <- left_join(diff_efficacy_total_table, diff_efficacy_numt_table, by = "visit")

# Print the final table to see the summary of both the variables in the efficacy dataset


