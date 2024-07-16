# PURPOSE:The goal here is to get ownership, sufficiency, and usage data from safety/ healthecon/ efficacy
# Output
# Creating a bar chart,  showing OSU over seven visits for efficacy dataset and over 4 visits for safety data set. Additionally, creating a tabulated format of five number summary of sufficiency
#Load Libraries
library(gsheet)
library(tidyverse)
library(dplyr)
library(gsheet)
library(RColorBrewer)

#Load data from data.r
source('data.R')

### CODE CHUNK FROM 3_differences.R. for justifying the use of sleep_under_net_last_night.
diff_efficacy_num<-kenya_efficacy_total %>% 
  mutate(num_nights_sleep_under_net=ifelse(num_nights_sleep_under_net=='dk', NA, num_nights_sleep_under_net)) %>% 
  drop_na(num_nights_sleep_under_net) %>%
  mutate( num_nights_sleep_under_net = as.numeric( num_nights_sleep_under_net ) )

diff_efficacy_long<-kenya_efficacy_total %>% 
  mutate(sleep_under_net_last_night=ifelse(sleep_under_net_last_night=='dk', NA, sleep_under_net_last_night)) %>% 
  drop_na(sleep_under_net_last_night) %>% 
  group_by(visit, sleep_under_net_last_night) %>% 
  tally 

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
perc_osu <- ggplot() +
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
### END OF CODE CHUNK FROM ANOTHER QUESTION


#Starting with average ownership data
hh_ownership<-kenya_healthecon_total %>% 
  mutate(bednet_yn=ifelse(num_bed_nets==0,'no','yes')) %>%
  distinct(hhid,.keep_all = TRUE) %>% 
  drop_na(bednet_yn)
#look into ownership data by visit 
# hh_ownership_by_visit<-hh_ownership %>% 
#   group_by(visit) %>% 
#   distinct(hhid,.keep_all = TRUE) %>% 
#   drop_na(bednet_yn)
# need visit data from monthly if want this



# get percentage for bednet ownership
hh_ownership %>% 
  group_by(bednet_yn) %>% 
  tally() %>% 
  mutate(total_percentage_ownership=(n/sum(n))*100)
#Next is sufficiency, with has a standard 2:1 ratio people to bed nets
hh_sufficiency <- hh_ownership %>% 
  filter(bednet_yn=='yes') %>% 
  distinct(hhid,.keep_all = TRUE) %>% 
  mutate(sufficiency=(num_hh_members/num_bed_nets)) %>% 
  select(sufficiency, hhid, bednet_yn) %>% 
  mutate(sufficient=ifelse(sufficiency>=2, 'yes', 'no'))

# hh_sufficiency %>% 
#   group_by(sufficiency) %>%
#   tally() %>%
#   mutate(sufficiency=(n/sum(n))*100)

#Usage on a household level needs to be found
hh_usage<-kenya_safety_total %>%
  group_by(hhid,visit) %>% 
  mutate(sleep_net_last_night=(sleep_net_last_night=='yes'))

#get percentage of last night usage
hh_last_usage<-hh_usage %>% 
  group_by(visit,sleep_net_last_night) %>% 
  filter(!is.na(sleep_net_last_night)) %>%
  tally() %>% 
  mutate(percent_last_usage=(n/sum(n))*100)

#  Get Avarage night slept under bednet within last week
hh_average_usage<-hh_usage %>%
  mutate(nights_sleep_net=ifelse(nights_sleep_net=='dk',NA,nights_sleep_net)) %>%
  mutate(nights_sleep_net=as.numeric(nights_sleep_net)) %>% 
  group_by(hhid,visit) %>% 
  summarise(average_nights_last_week=median(nights_sleep_net,na.rm = TRUE))

#graph of how many households used bed nets over the past week, faceted by visit
ggplot(data=hh_average_usage,aes(x=average_nights_last_week))+
  geom_histogram()+
  facet_wrap(~visit)+
  labs(
    title='histogram of average night slept under net last week from safety'
  )

#graph showing % of hh that slept under a bed last night 
ggplot(data=hh_last_usage,aes(x=sleep_net_last_night,y=percent_last_usage))+
  geom_col()+
  facet_wrap(~visit)+
  labs(
    title='bar chart of slept under net last night for each visit from safety'
  )

# new dataset for bednet sufficiency
hh_suffsum<-hh_sufficiency %>% 
  group_by(sufficient) %>% 
  tally %>% 
  mutate(n=(n/sum(n)*100)) %>% 
  mutate(index=ifelse(sufficient=='yes', 'yes', 'no')) %>% 
  mutate(data='Sufficiency') %>% 
  select(-sufficient)
  
# new dataset for bednet ownership
hh_ownsum<-hh_ownership %>% 
  group_by(bednet_yn) %>% 
  tally %>% 
  mutate(n=(n/sum(n))*100) %>% 
  mutate(index=ifelse(bednet_yn=='yes', 'yes', 'no')) %>% 
  mutate(data='Ownership') %>% 
  select(-bednet_yn)

# new dataset for bednet usage
hh_lastsum<-hh_last_usage %>% 
  group_by(sleep_net_last_night) %>% 
  summarize(n=sum(n)) %>% 
  mutate(n=n/4) %>% 
  mutate(n=(n/sum(n))*100) %>% 
  mutate(sleep_net_last_night=ifelse(sleep_net_last_night==TRUE, 'yes', 'no')) %>% 
  mutate(index=ifelse(sleep_net_last_night=='yes', 'yes', 'no')) %>% 
  mutate(data='Usage') %>% 
  select(-sleep_net_last_night)

#combining the three new datasets
osu_sum<-rbind(hh_ownsum, hh_lastsum, hh_suffsum)

# filtering out yes options
osu_sum_t<-osu_sum %>% 
  filter(index=='yes')

# take values for yes/ no columns from n column
osu_sum<-osu_sum %>% 
  pivot_wider(names_from = 'index', values_from=n) %>% 
  select(data, yes, no) %>% 
  mutate(yes=round(yes, digits=2)) %>% 
  mutate(no=round(no, digits=2))


#cat('\n Table showing Ownership, Sufficiency and Usage Data \n')


# make graph on bednet ownership, sufficiency and usage 
print(ggplot(osu_sum_t, aes(x = data, y = n, fill = data)) + 
  geom_col(alpha = 0.8) +                               
  scale_fill_manual(values = c("Ownership" = "#5C2D91",  
                               "Usage" = "#9666B2",
                               "Sufficiency" = "#4B0082")) +
  labs(y = "Percentage", x = "", fill = "") +              
  ggtitle("Percentage of Households with Bed Nets:\nOwned, Used Last Night, and Sufficiency") + # More descriptive title
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), 
        axis.title.y = element_text(size = 12),  
        axis.text = element_text(size = 10),  
        panel.grid.major.x = element_blank(),  
        legend.position = "none")  +
  ylim(0, 100))




#Usage on an individual level needs to be found
# effi_usage<-kenya_efficacy_total %>%
#   group_by(hhid,visit) %>% 
#   mutate(sleep_under_net_last_night=(sleep_under_net_last_night=='yes'))


# new dataset for usage percentage
# effi_last_usage<-effi_usage %>% 
#   group_by(visit,sleep_under_net_last_night) %>% 
#   filter(!is.na(sleep_under_net_last_night)) %>%
#   tally() %>% 
#   mutate(percent_last_usage=(n/sum(n))*100)

# average 
# effi_average_usage<-effi_usage %>%
#   mutate(num_nights_sleep_under_net=ifelse(num_nights_sleep_under_net=='dk',NA,num_nights_sleep_under_net)) %>%
#   mutate(num_nights_sleep_under_net=as.numeric(num_nights_sleep_under_net)) %>% 
#   group_by(hhid,visit) %>% 
#   summarise(average_nights_last_week=median(num_nights_sleep_under_net,na.rm = TRUE))

#graph of how many households used bed nets over the past week, faceted by visit

# ggplot(data=effi_average_usage,aes(x=average_nights_last_week))+
#   geom_histogram()+
#   facet_wrap(~visit)+
#   labs(
#     title='histogram of average night slept under net last week from efficacy'
#   )

#graph showing % of individual that slept under a bed last night 

# ggplot(data=effi_last_usage,aes(x=sleep_under_net_last_night,y=percent_last_usage))+
#   geom_col()+
#   facet_wrap(~visit)+
#   labs(
#     title='bar chart of slept under net last night for each visit from efficacy'
#   )


