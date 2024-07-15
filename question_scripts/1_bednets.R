
# load libraries

library (tidyverse)
library(gsheet)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggbreak)
library(RColorBrewer)
library(knitr)
library(markdown) #install.packages('markdown')
library(reactable)

#Load data from data.r
source('data.r')

#PURPOSE:Information describing bed nets, ex: Price, # of bednets per household, Defining LLIN’, and Age/Source

# Outputs: bed_net_cost_df, receive_free_bed_nets, bed_net_number_df, kenya_safety_summary

## making a table of Avg price- KES converted to $
# create a table that shows the cost of bed nets obtained by each household over the entire trial which requires the use of the following question:
# 2a. from healthecon_monthly questionnaire [If yes to 2. Has the household obtained any bed nets in the past 4 weeks? □ Yes □ No] What was the cost for each net obtained (per bed net obtained)?
healtheconmonthly_totaln<- healtheconmonthly_total %>% 
  # create a column that has the price of bed nets in usd
  mutate(bed_nets_past_month_kes=(as.numeric(bed_nets_past_month_kes)*0.0078)) %>% 
  drop_na(bed_nets_past_month_kes)

#bed_net_cost_2<-gsub('1st Qu.', '1st Quartile', bed_net_cost)
# ??? use gsub to change the names of the columns to better words
# make bed_net_cost into a dataframe --  bed_net_cost<-as.data.frame(bed_net_cost)

bed_net_cost<-summary(healtheconmonthly_totaln$bed_nets_past_month_kes)
bed_net_cost_df <- as.data.frame(t(as.matrix(bed_net_cost)))
bed_net_cost_df <- bed_net_cost_df %>%
  mutate(across(everything(), ~ format(.x, digits = 3, scientific = FALSE))) %>% 
  rename('Minimum'='Min.') %>% 
  rename('1st Quartile'='1st Qu.') %>% 
  rename('3rd Quartile'='3rd Qu.') %>% 
  rename('Maximum'='Max.')
reactable(bed_net_cost_df, columns=list(), pagination=FALSE)


#make new dataset called bednets_price
bednets_price<-healtheconmonthly_totaln %>% 
  group_by(bed_nets_past_month_kes) %>% 
  tally
  
# make a graph about bednet price in $ (bought in past months)
ggplot(data=bednets_price, aes(x=bed_nets_past_month_kes, y=n)) +
  geom_point() + 
  geom_segment(mapping=aes(x=bed_nets_past_month_kes,
                           y=n,
                           xend = bed_nets_past_month_kes,
                           yend = 0))+
  scale_x_break(c(25, 55))
  #scale_x_continuous(trans='log')
#geom_col()

#new dataset for free bednets
bednets_free<-healtheconmonthly_totaln %>% 
  mutate(free_yn=ifelse(bed_nets_past_month_kes>0, 'not free', 'free')) %>% 
  group_by(free_yn) %>% 
  tally 

# make graph for the free bednets 


receive_free_bed_nets <- ggplot(data = bednets_free, aes(x = free_yn, y = n, fill = free_yn)) + 
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

## Avg Number of bed nets per hh
kenya_healtheconn<-kenya_healthecon_total %>% 
  distinct(hhid, .keep_all = TRUE) %>% 
  group_by(hhid, num_bed_nets) %>% 
  tally() %>% 
  drop_na(num_bed_nets)

# change names of column and assign a name to the summary below

bed_net_number<-summary(kenya_healtheconn$num_bed_nets)
bed_net_number_df <- as.data.frame(t(as.matrix(bed_net_number)))
bed_net_number_df <- bed_net_number_df %>%
  mutate(across(everything(), ~ format(.x, digits = 3, scientific = FALSE))) %>% 
  rename('Minimum'='Min.') %>% 
  rename('1st Quartile'='1st Qu.') %>% 
  rename('3rd Quartile'='3rd Qu.') %>% 
  rename('Maximum'='Max.')
reactable(bed_net_number_df, columns=list(), pagination=FALSE)


## Avg Age of people based on usage of bednets
# adding bednets yes or no column in a new safety bednets dataset and selecting required columns
kenya_safety_bn<-kenya_safety_total %>% 
  mutate(bednetsyn=ifelse(nights_sleep_net>0, 1, 0)) %>% 
  drop_na(bednetsyn, corrected_age) %>% 
  select(bednetsyn, corrected_age, sex)


# Doing a summary of the mean of age for people who use bednets/who don't

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


kenya_safety_summary <-kenya_safety_summary %>% 
  rename('Bed Net Yes/No'=bed_nets_yn) %>% 
  rename('Average Age'=average_age)

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
