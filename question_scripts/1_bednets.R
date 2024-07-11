# load libraries

library (tidyverse)
library(gsheet)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggbreak)
library(RColorBrewer)
#Load data from data.r
source('data.r')

#PURPOSE:Information describing bed nets, ex: Price, # of bednets per household, Defining LLIN’, and Age/Source
## Avg price
healtheconmonthly_totaln<- healtheconmonthly_total %>% 
  mutate(bed_nets_past_month_kes=(as.numeric(bed_nets_past_month_kes)*0.0078)) %>% 
  drop_na(bed_nets_past_month_kes)

# distinct(hhid, .keep_all = TRUE)

print('SUMMARY OF PRICE OF BEDNETS')
print(summary(healtheconmonthly_totaln$bed_nets_past_month_kes))

bednets_price<-healtheconmonthly_totaln %>% 
  group_by(bed_nets_past_month_kes) %>% 
  tally
  

ggplot(data=bednets_price, aes(x=bed_nets_past_month_kes, y=n)) +
  geom_point() + 
  geom_segment(mapping=aes(x=bed_nets_past_month_kes,
                           y=n,
                           xend = bed_nets_past_month_kes,
                           yend = 0))+
  scale_x_break(c(25, 55))
  #scale_x_continuous(trans='log')
#geom_col()

bednets_free<-healtheconmonthly_totaln %>% 
  mutate(free_yn=ifelse(bed_nets_past_month_kes>0, 'not free', 'free')) %>% 
  group_by(free_yn) %>% 
  tally 

ggplot(data = bednets_free, aes(x = free_yn, y = n, fill = free_yn)) + 
  geom_col(alpha = 0.8) +                                        
  scale_fill_manual(values = c("free" = "#5C2D91", "not free" = "#9666B2")) +   
  labs(x = "Received Free Bed Net", y = "Count", fill = "") +             
  ggtitle("Count of Households Receiving Free Bed Nets") +            
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    axis.title = element_text(size = 12),                     
    axis.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),                    
    legend.position = "none"                                 
  )

## Avg Number per hh
kenya_healtheconn<-kenya_healthecon_total %>% 
  distinct(hhid, .keep_all = TRUE) %>% 
  group_by(hhid, num_bed_nets) %>% 
  tally() %>% 
  drop_na(num_bed_nets)

print('SUMMARY OF NUMBER OF BEDNETS PER HOUSEHOLD')
print(summary(kenya_healtheconn$num_bed_nets))

## Avg Age of people based on usage of bednets
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

print('Summary of people using bednets considering age and sex')
print(kenya_safety_summary)


# FIELD WORKER NUMBER CODE
# wid_d<-kenya_demography %>% 
#   group_by(wid_manual) %>% 
#   tally 
# 
# wid_h<-kenya_healthecon_baseline %>% 
#   group_by(wid_manual) %>%
#   tally
# 
# wid_hn<-kenya_healthecon_baseline_new %>% 
#   group_by(wid_manual) %>% 
#   tally
# 
# wid_e<-kenya_efficacy %>% 
#   group_by(wid_manual) %>% 
#   tally
# 
# wid_hm<-healthecon_monthly %>%
#   group_by(wid_manual) %>% 
#   tally
# 
# 
# wid<-rbind(wid_d, wid_e, wid_h, wid_hm, wid_hn)
# 
# wid<-wid %>% 
#   distinct(wid_manual)
# 
# nrow(wid)
