# load libraries

library (tidyverse)
library(gsheet)
library(tidyverse)
library(dplyr)
library(gsheet)
#Load data from data.r
source('data.r')

#PURPOSE:Information describing bed nets, ex: Price, # of bednets per household, Defining LLIN’, and Age/Source
## Avg price
healtheconmonthly_totaln<- healtheconmonthly_total %>% 
  mutate(bed_nets_past_month_kes=as.numeric(bed_nets_past_month_kes)) %>% 
  drop_na(bed_nets_past_month_kes)

# distinct(hhid, .keep_all = TRUE)
summary(healtheconmonthly_totaln$bed_nets_past_month_kes)

## Avg Number per hh
kenya_healtheconn<-kenya_healthecon_total %>% 
  distinct(hhid, .keep_all = TRUE) %>% 
  group_by(hhid, num_bed_nets) %>% 
  tally() %>% 
  drop_na(num_bed_nets)
summary(kenya_healtheconn$num_bed_nets)

## Avg Age of people based on usage of bednets
table(kenya_safety_total$nights_sleep_net)

# adding bednets yes or no column in a new safety bednets dataset and selecting required columns
kenya_safety_bn<-kenya_safety_total %>% 
  mutate(bednetsyn=ifelse(nights_sleep_net>0, 1, 0)) %>% 
  drop_na(bednetsyn, corrected_age) %>% 
  select(bednetsyn, corrected_age, sex)


# Doing a summary of the mean of age for people who use bednets

kenya_safety_age_summary <- kenya_safety_bn %>%
  group_by(bednetsyn) %>%
  summarize(average_age = mean(corrected_age, na.rm = TRUE)) %>%
  mutate(bed_nets_yn = if_else(bednetsyn == 1, 'yes', 'no')) %>%
  select(bed_nets_yn, average_age)


# Calculate the average age for 'yes' and 'no' categories
kenya_safety_sex_summary <- kenya_safety_bn %>%
  group_by(sex, bednetsyn) %>%
  summarize(total = n()) %>%
  mutate(bed_nets_yn = if_else(bednetsyn == 1, 'yes', 'no')) %>%
  select(sex, bed_nets_yn, total) %>% 
  pivot_wider(names_from = sex, values_from=total)

#joining age summary with the sex summary
kenya_safety_summary<-kenya_safety_sex_summary %>% 
  left_join(kenya_safety_age_summary, by='bed_nets_yn')

# finding the summary for the price of bednets
summary(healtheconmonthly_totaln$bed_nets_past_month_kes)
#finding the summary for the number of bednets per household
summary(kenya_healtheconn$num_bed_nets)
kenya_safety_summary
