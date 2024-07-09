#Load Libraries
library(gsheet)
library(tidyverse)
library(dplyr)
library(gsheet)
#Load data

source('data.r')

# PURPOSE:
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
ggplot(diff_efficacy_long, aes(x = visit, y = n, fill = sleep_under_net_last_night)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("yes" = "skyblue", "no" = "pink")) +
  labs(y = "Value", x = "Visit", fill = "Type") +
  theme_minimal()+
  labs(
    title='Bar Chart of Slept Under Net Last Night for each visit'
  )

# creating a new dataset which is grouped by visit
diff_total <- diff_efficacy_long %>%
  group_by(visit) 
# Combining this with the previous dataset and then reordering by y/n
diff_efficacy_total <- bind_rows(diff_efficacy_long, diff_total) %>%
  mutate(sleep_under_net_last_night = factor(sleep_under_net_last_night, levels = c('yes', 'no')))

# Create a summary of grouped by visit and number of nights slept under bed net
diff_efficacy_numt <- diff_efficacy_num %>%
  group_by(visit, num_nights_sleep_under_net) %>%
  tally

# Combined plot of slept under bed net last night v number of nights slept under bed net
ggplot() +
  geom_col(data = diff_efficacy_total, aes(x = sleep_under_net_last_night, y = n, fill = sleep_under_net_last_night), alpha = 0.6) +
  geom_line(data = diff_efficacy_numt, aes(x = num_nights_sleep_under_net, y = n, group = visit), color = "blue") +
  facet_wrap(~visit) +
  scale_fill_manual(values = c("total" = "grey", "yes" = "blue", "no" = "red")) +
  labs(y = "Count", x = "Sleep Under Net Last Night", fill = "Type", title='slept under net last night vs last week') +
  theme_minimal()

#creating a new dataset with distinct rows and pivoting wide for sleep under net last night
diff_efficacy_total_table <- diff_efficacy_total %>%
  distinct() %>% 
  pivot_wider(names_from = sleep_under_net_last_night, values_from = n) %>% 
  distinct()

# creating a new datset with num_nights sleep under net and pivoting wide
diff_efficacy_numt_table <- diff_efficacy_numt %>%
  pivot_wider(names_from = num_nights_sleep_under_net, values_from = n)

print(diff_efficacy_total_table)
print(diff_efficacy_numt_table)

#combining both tables

combined_table <- left_join(diff_efficacy_total_table, diff_efficacy_numt_table, by = "visit")

# Print the final table to see the summary of both the variables in the efficacy dataset
print(combined_table)

