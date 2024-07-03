# PURPOSE:
# Creating a bar chart,  showing OSU over seven visits for efficacy dataset and over 4 visits for safety data set. Additionally, creating a tabulated format of five number summary of sufficiency
#Load Libraries
library(gsheet)
library(tidyverse)
library(dplyr)
library(gsheet)
#Load data

kenya_safetynew<- read_csv('dataset/kenya_safetynew.csv')
kenya_efficacy<- read_csv('dataset/kenya_efficacy.csv')
kenya_healthecon_baseline_new<- read_csv('dataset/kenya_healthecon_baseline_new.csv')
kenya_demography<- read_csv('dataset/kenya_demography.csv')
healthecon_monthly<- read_csv('dataset/healthecon_monthly.csv')
kenya_healthecon_baseline<- read_csv('dataset/kenya_healthecon_baseline.csv')
kenya_safety<- read_csv('dataset/kenya_safety.csv')
kenya_ae<- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1dsc5m5abfSbttIgwPvukzi7ca-zA9iU2RpPifZsWA1Y/edit?usp=sharing')

#creating new dataset with columns required from safety
kenya_nsafety<-kenya_safety %>% 
  mutate('type'='og') %>% #type of dataset (safety or new)
  select(extid, visit, type, todays_date,
         sleep_net_last_night, nights_sleep_net, ae_symptoms, 
         pregnant_yn,
         num_months_pregnant,
         months_pregnant_dk,
         anc, iptp)

#creating new dataset with columns required from safetynew 
kenya_nsafetynew<-kenya_safetynew %>%
  mutate('type'='new') %>% #type of dataset (safety or new) 
  mutate(ae_symptoms=NA) %>%  #ae_symptoms not present in the new one
  mutate(months_pregnant_dk=NA) %>% #months_pregnant_dk not present in the new one
  select(extid, visit, type, todays_date, 
         sleep_net_last_night, nights_sleep_net, 
         ae_symptoms, 
         pregnant_yn,
         num_months_pregnant,
         months_pregnant_dk,
         anc, iptp)

#creating new dataset with columns required from demography
kenya_ndemography<-kenya_demography %>% 
  select(extid, ward, village, KEY, instanceID, Latitude, Longitude, Altitude ,hhid, 
         corrected_age, geo_cluster_num, cluster, dob, sex, num_hh_members )

#creating new dataset merging safety and safetynew and then adding required columns from demography
kenya_safety_total<- rbind(kenya_nsafety, kenya_nsafetynew)  %>% 
  left_join(kenya_ndemography, by='extid')

# creating a dataset for adverse events in safety
safety_nae <- left_join(kenya_safety_total, kenya_ae, by = c('KEY' = 'PARENT_KEY'))

#selecting columns from healthecon baseline
kenya_nhealthecon<-kenya_healthecon_baseline %>% 
  mutate(type='og') %>% #type of dataset (baseline v new)
  select(extid, #
         type, 
         todays_date,  
         num_bed_nets, extid, #
         todays_date,  
         num_bed_nets, malaria_care_yn,
         malaria_care_where,
         malaria_care_where_specify,
         num_visit_hf,
         num_visit_hospital,
         num_visit_pharmacy,
         num_visit_informal_drug_vendor,
         num_visit_other,
         overnight_hf_hospital_healer,
         num_overnight_hf_hospital_healer,
         malaria_test_yn,
         malaria_test_result,
         malaria_treatment_yn,
         malaria_consult_tests_kes,
         malaria_hospitalization_kes,
         malaria_medication_kes,
         malaria_travel_to_clinic_kes,
         malaria_food_kes,
         malaria_other_kes,
         malaria_miss_school,
         num_malaria_miss_school_days,
         malaria_miss_work,
         num_malaria_miss_work_days,
         malaria_away_pay,
         malaria_away_pay_kes,
         other_member_care,
         other_member_care_select,
         other_member_care_unlisted,
         seek_care_other_ill,
         seek_care_other_ill_where,
         seek_care_other_ill_where_specify,
         num_visit_hf_other_ill,
         num_visit_hospital_other_ill,
         num_visit_pharmacy_other_ill,
         num_visit_informal_drug_vendor_other_ill,
         num_visit_traditional_healer_other_ill,
         num_visit_other_other_ill,
         other_ill_overnight_hopsital,
         num_nights_other_ill_hopsital,
         seek_care_other_ill_malaria_test,
         seek_care_other_ill_malaria_test_result,
         seek_care_other_ill_malaria_treatment,
         seek_care_other_ill_malaria_test_kes,
         seek_care_other_ill_malaria_treatment_kes,
         seek_care_other_ill_other_kes)

#selecting columns from healthecon_new
kenya_nhealtheconnew <- kenya_healthecon_baseline_new %>%
  rowwise() %>%
  mutate(num_bed_nets = ifelse(hhid %in% kenya_healthecon_baseline$hhid, 
                               kenya_healthecon_baseline$num_bed_nets[which(kenya_healthecon_baseline$hhid == hhid)], 
                               num_bed_nets)) %>% #adding num_bed_nets as it is not present in new dataset
  ungroup() %>% 
  mutate(type='new') %>% 
  mutate(malaria_care_where_specify=NA) %>% #putting NA to columns that are not present in the new dataset
  mutate(num_visit_hospital=NA) %>% 
  mutate(num_visit_pharmacy=NA) %>% 
  mutate(num_visit_informal_drug_vendor=NA) %>% 
  mutate(num_visit_other=NA) %>%
  mutate(num_overnight_hf_hospital_healer=NA) %>% 
  mutate(other_member_care_select=NA) %>% 
  mutate(other_member_care_unlisted=NA) %>% 
  mutate(seek_care_other_ill_where_specify=NA) %>% 
  mutate(num_visit_pharmacy_other_ill=NA) %>% 
  mutate(num_visit_informal_drug_vendor_other_ill=NA) %>% 
  mutate(num_visit_traditional_healer_other_ill=NA) %>% 
  mutate(num_visit_other_other_ill=NA) %>% 
  mutate(num_nights_other_ill_hopsital=NA) %>% 
  mutate(seek_care_other_ill_malaria_treatment=NA) %>% 
  mutate(seek_care_other_ill_malaria_test_kes=NA) %>% 
  mutate(seek_care_other_ill_malaria_treatment_kes=NA) %>% 
  mutate(seek_care_other_ill_other_kes=NA) %>% 
  select(extid, #
         type, 
         todays_date,  
         num_bed_nets, extid, #
         todays_date,  
         num_bed_nets, 
         malaria_care_yn,
         malaria_care_where,
         malaria_care_where_specify,
         num_visit_hf,
         num_visit_hospital,
         num_visit_pharmacy,
         num_visit_informal_drug_vendor,
         num_visit_other,
         overnight_hf_hospital_healer,
         num_overnight_hf_hospital_healer,
         malaria_test_yn,
         malaria_test_result,
         malaria_treatment_yn,
         malaria_consult_tests_kes,
         malaria_hospitalization_kes,
         malaria_medication_kes,
         malaria_travel_to_clinic_kes,
         malaria_food_kes,
         malaria_other_kes,
         malaria_miss_school,
         num_malaria_miss_school_days,
         malaria_miss_work,
         num_malaria_miss_work_days,
         malaria_away_pay,
         malaria_away_pay_kes,
         other_member_care,
         other_member_care_select,
         other_member_care_unlisted,
         seek_care_other_ill,
         seek_care_other_ill_where,
         seek_care_other_ill_where_specify,
         num_visit_hf_other_ill,
         num_visit_hospital_other_ill,
         num_visit_pharmacy_other_ill,
         num_visit_informal_drug_vendor_other_ill,
         num_visit_traditional_healer_other_ill,
         num_visit_other_other_ill,
         other_ill_overnight_hopsital,
         num_nights_other_ill_hopsital,
         seek_care_other_ill_malaria_test,
         seek_care_other_ill_malaria_test_result,
         seek_care_other_ill_malaria_treatment,
         seek_care_other_ill_malaria_test_kes,
         seek_care_other_ill_malaria_treatment_kes,
         seek_care_other_ill_other_kes)

#binding healthecon with healtheconnew and then joining demography
kenya_healthecon_total<-rbind(kenya_nhealthecon, kenya_nhealtheconnew) %>% 
  left_join(kenya_ndemography, by='extid')

#selecting columns from efficacy and then joining demography
kenya_efficacy_total<-kenya_efficacy %>% 
  select(extid,
         start_time,
         end_time,
         visits_done, 
         malaria_fever_last_month,
         treated_for_malaria_yn,
         malaria_treatment,
         sleep_under_net_last_night,
         num_nights_sleep_under_net
  ) %>% 
  left_join(kenya_ndemography, by='extid')

#selecting columns from healtheconmonthly and adding demography
healtheconmonthly_total <- healthecon_monthly %>%
  rowwise() %>%
  mutate(num_bed_nets = ifelse(hhid %in% kenya_healthecon_baseline$hhid, 
                               kenya_healthecon_baseline$num_bed_nets[which(kenya_healthecon_baseline$hhid == hhid)], 
                               num_bed_nets)) %>% #adding num_bed_nets from baseline
  select(extid, #
         todays_date,  
         num_bed_nets, extid, #
         todays_date,  
         num_bed_nets, 
         new_bednets_past_month,
         bed_nets_past_month_kes,
         malaria_care_yn,
         malaria_care_where,
         malaria_care_where_specify,
         num_visit_hf,
         num_visit_hospital,
         num_visit_pharmacy,
         num_visit_informal_drug_vendor,
         num_visit_other,
         overnight_hospital,
         num_overnight_hospital,
         malaria_test_yn,
         malaria_test_result,
         malaria_treatment_yn,
         malaria_consult_tests_kes,
         malaria_hospitalization_kes,
         malaria_medication_kes,
         malaria_travel_to_clinic_kes,
         malaria_food_kes,
         malaria_other_kes,
         malaria_miss_school,
         num_malaria_miss_school_days,
         malaria_miss_work,
         num_malaria_miss_work_days,
         malaria_away_pay,
         malaria_away_pay_kes,
         other_member_care,
         other_member_care_select,
         seek_care_other_ill,
         seek_care_other_ill_where,
         seek_care_other_ill_where_specify,
         num_visit_hf_other_ill,
         num_visit_hospital_other_ill,
         num_visit_pharmacy_other_ill,
         num_visit_informal_drug_vendor_other_ill,
         num_visit_traditional_healer_other_ill,
         num_visit_other_other_ill,
         other_ill_overnight_hopsital,
         num_nights_other_ill_hopsital,
         seek_care_other_ill_malaria_test,
         seek_care_other_ill_malaria_test_result,
         seek_care_other_ill_malaria_treatment,
         seek_care_other_ill_malaria_test_kes,
         seek_care_other_ill_malaria_treatment_kes,
         seek_care_other_ill_other_kes) %>% 
  left_join(kenya_ndemography, by='extid')

#The goal here is to get ownership, sufficiency, and usage data from safety
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

hh_ownership %>% 
  group_by(bednet_yn) %>% 
  tally() %>% 
  mutate(total_percentage_ownership=(n/sum(n))*100)
#Next is sufficiency, with has a standard 2:1 ratio people to bed nets
hh_sufficiency <- hh_ownership %>% 
  filter(bednet_yn=='yes') %>% 
  distinct(hhid,.keep_all = TRUE) %>% 
  mutate(sufficiency=(num_hh_members/num_bed_nets)) 
# hh_sufficiency %>% 
#   group_by(sufficiency) %>%
#   tally() %>%
#   mutate(sufficiency=(n/sum(n))*100)
#Usage on a household level needs to be found
hh_usage<-kenya_safety_total %>%
  group_by(hhid,visit) %>% 
  mutate(sleep_net_last_night=(sleep_net_last_night=='yes'))

hh_last_usage<-hh_usage %>% 
  group_by(visit,sleep_net_last_night) %>% 
  filter(!is.na(sleep_net_last_night)) %>%
  tally() %>% 
  mutate(percent_last_usage=(n/sum(n))*100)

hh_average_usage<-hh_usage %>%
  mutate(nights_sleep_net=ifelse(nights_sleep_net=='dk',NA,nights_sleep_net)) %>%
  mutate(nights_sleep_net=as.numeric(nights_sleep_net)) %>% 
  group_by(hhid,visit) %>% 
  summarise(average_nights_last_week=median(nights_sleep_net,na.rm = TRUE))
#graph of how many households used bed nets over the past week, faceted by visit
ggplot(data=hh_average_usage,aes(x=average_nights_last_week))+
  geom_histogram()+
  facet_wrap(~visit)
#graph showing % of hh that slept under a bed last night 
ggplot(data=hh_last_usage,aes(x=sleep_net_last_night,y=percent_last_usage))+
  geom_col()+
  facet_wrap(~visit)








