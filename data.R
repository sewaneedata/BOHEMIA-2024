#Load Libraries
library(gsheet)
library(tidyverse)
library(dplyr)
#Load data

kenya_safetynew<- read_csv('dataset/kenya_safetynew.csv')
kenya_efficacy<- read_csv('dataset/kenya_efficacy.csv')
kenya_healthecon_baseline_new<- read_csv('dataset/kenya_healthecon_baseline_new.csv')
kenya_demography<- read_csv('dataset/kenya_demography.csv')
healthecon_monthly<- read_csv('dataset/healthecon_monthly.csv')
kenya_healthecon_baseline<- read_csv('dataset/kenya_healthecon_baseline.csv')
kenya_safety<- read_csv('dataset/kenya_safety.csv')

kenya_nsafety<-kenya_safety %>% 
  mutate('type'='og') %>% 
  select(type, KEY, visit, todays_date, hhid, dob, age, sex, extid, sleep_net_last_night, nights_sleep_net)

kenya_nsafetynew<-kenya_safetynew %>% 
  mutate('type'='new') %>% 
  select(type, KEY, visit, todays_date, hhid, dob, age, sex, extid, sleep_net_last_night, nights_sleep_net)


kenya_safety_total<- rbind(kenya_nsafety, kenya_nsafetynew)

which(kenya_safety$extid %in% kenya_safetynew$extid)

kenya_nhealthecon<-kenya_healthecon_baseline %>% 
  select(todays_date,hhid, extid, KEY, Latitude, Longitude, Altitude, num_members, num_bed_nets, instanceID, malaria_care_yn, malaria_care_where, malaria_test_yn, malaria_test_result, malaria_treatment_yn, malaria_consult_tests_kes, malaria_hospitalization_kes, malaria_medication_kes, malaria_travel_to_clinic_kes, malaria_food_kes, malaria_other_kes, malaria_miss_school, malaria_miss_work, malaria_away_pay, malaria_away_pay_kes )

duplicated(kenya_safety_total)
