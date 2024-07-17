#PURPOSE:Tabulated data showing net usage and changes over time, from household data spanning three months. 
#Load Libraries
library(gsheet)
library(tidyverse)
library(dplyr)
library(gsheet)
#Load data

source('data.r')
#selecting necessary columns and then creating previous answer column using lag and dropping NAs
test <- kenya_safety_total %>%
  select( extid, visit, todays_date, sleep_net_last_night ) %>% 
  group_by(extid) %>% 
  arrange(todays_date) %>% 
  mutate(prev_ans=lag(sleep_net_last_night)) %>%
  drop_na( prev_ans ) 
#creating a new dataset for percentage of people who changed their answer from the previous one
test_a <- test %>% 
  mutate(same_ans=(sleep_net_last_night==prev_ans)) %>% 
  group_by( visit, prev_ans ) %>%
  summarize( pct_unchanged = 100*mean( same_ans, na.rm=TRUE ) ) %>% 
  mutate(prev_ans=str_to_title(prev_ans))

test_a$prev_ans <- factor(test_a$prev_ans, 
                                      levels = c("Yes", "No"))
  
#creating a ggplot

percent_usage_visits <- ggplot(test_a, aes(x = visit, y = pct_unchanged, fill = prev_ans)) +
  geom_col(position = position_dodge()) + 
  ylim(0, 100) +
  scale_fill_manual(values = c("#4B0082", "#9666B2")) +
  labs(
    title = "Percent usage unchanged from previous visit",
    x = "Visits",
    y = "Percentage unchanged (%)",
    fill='Previous answer'
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5), 
    axis.title = element_text(size = 12),  
    axis.text = element_text(size = 10),  
    legend.title = element_text(size = 11), 
    legend.position = "bottom",           
    panel.grid.minor = element_blank()  
  )

