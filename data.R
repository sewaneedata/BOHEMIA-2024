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
