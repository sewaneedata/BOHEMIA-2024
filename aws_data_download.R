# DO NOT TOUCH THIS SCRIPT AT ALL
# PURPOSE: this RScript serves to access the original data from AWS and create datasets necessary for the BOHEMIA & DataLab collaboration. This dataset will be anonymized and have removed all necessary hhhids and extids that should not be used for analysis

#load libraries
library(tidyverse)
library(cloudbrewr)

# log into AWS where the data is stored
ENV_PIPELINE_STAGE <- Sys.getenv("PIPELINE_STAGE")
DATA_STAGING_BUCKET_NAME <- 'databrew.org'
DATA_LAKE_BUCKET_NAME <- 'bohemia-lake-db'
PROJECT_SOURCE <- 'kwale'
SE_FOLDER_TARGET <- glue::glue('{PROJECT_SOURCE}/sanitized-form')

pad_hhid <- function(data){
  if('hhid' %in% names(data)){
    data %>%
      dplyr::mutate(hhid = stringr::str_pad(hhid, 5, pad = "0"))
  }else{
    data
  }
}

tryCatch({
  logger::log_info('Attempt AWS login')
  # login to AWS - this will be bypassed if executed in CI/CD environment
  cloudbrewr::aws_login(
    role_name = 'cloudbrewr-aws-role',
    profile_name =  'cloudbrewr-aws-role',
    pipeline_stage = 'production')
  
}, error = function(e){
  logger::log_error('AWS Login Failed')
  stop(e$message)
})


# For each of the necessary datasets this script will:
# 1. load in the data,
# 2. make any necessary data cleaning edits and merges, and
# 3. download the dataset as the appropriate csv file



# SAFETY

  safety <- cloudbrewr::aws_s3_get_table(
    bucket = DATA_STAGING_BUCKET_NAME,
    key = glue::glue('{PROJECT_SOURCE}/sanitized-form/safety/safety.csv')) %>%
    pad_hhid() %>% 
    select(KEY, visit, todays_date, hhid, cluster) # select the columns we need
  
  safety_repeat_individual <- cloudbrewr::aws_s3_get_table(
    bucket = DATA_STAGING_BUCKET_NAME,
    key = glue::glue('{PROJECT_SOURCE}/sanitized-form/safety/safety-repeat_individual.csv')) %>%
    pad_hhid()
  
  safetynew <- cloudbrewr::aws_s3_get_table(
    bucket =DATA_STAGING_BUCKET_NAME,
    key = glue::glue('{PROJECT_SOURCE}/sanitized-form/safetynew/safetynew.csv')) %>%
    pad_hhid() %>% 
    select(KEY, visit, todays_date, hhid, cluster, intervention) # select the columns we need
  
  safetynew_repeat_individual <- cloudbrewr::aws_s3_get_table(
    bucket =DATA_STAGING_BUCKET_NAME,
    key = glue::glue('{PROJECT_SOURCE}/sanitized-form/safetynew/safetynew-repeat_individual.csv')) %>%
    pad_hhid()
  
    # at the discretion of the study, some households were dropped and need to be removed
    dropped_hhid <- cloudbrewr::aws_s3_get_table(
      bucket ='bohemia-lake-db',
      key = glue::glue('bohemia_ext_data/v0_dropped_hhid/v0_dropped_hhid.csv')) %>%
      pad_hhid()
    
    # make the hhid column have the preceding 0 if it needs it
    dropped_hhid$dropped_hhid <- sprintf("%05d", dropped_hhid$dropped_hhid)
  
  # create the safety dataset
  safety_dataset <- left_join(safety, safety_repeat_individual, by=c('KEY' = 'PARENT_KEY')) %>% 
    filter(!(hhid %in% dropped_hhid$dropped_hhid), # remove dropped households
           extid != '2437-61', # erroneous entry
           safety_status != 'UNDEFINED') # erroneous entry
  
  # create the safetynew dataset
  safetynew_dataset <- left_join(safetynew, safetynew_repeat_individual, 
                                 by=c('KEY' = 'PARENT_KEY')) %>% 
    filter(!(hhid %in% dropped_hhid$dropped_hhid), # remove dropped households
           extid != '2437-61', # erroneous entry
           safety_status != 'UNDEFINED') # erroneous entry
  
  # save the csv files
  write_csv(safety_dataset, file = 'kenya_safety.csv')
  write_csv(safetynew_dataset, file = 'kenya_safetynew.csv')



# DEMOGRAPHY
  
  v0_dem_repeat <- cloudbrewr::aws_s3_get_table(
    bucket = DATA_STAGING_BUCKET_NAME,
    key = glue::glue('{PROJECT_SOURCE}/sanitized-form/v0demography/v0demography-repeat_individual.csv')) %>%
    pad_hhid()
  
  v0_demography <- cloudbrewr::aws_s3_get_table(
    bucket = DATA_STAGING_BUCKET_NAME,
    key = glue::glue('{PROJECT_SOURCE}/sanitized-form/v0demography/v0demography.csv')) %>%
    pad_hhid()
  
  # create the demography dataset
  demography_dataset <- left_join(v0_demography, v0_dem_repeat, by=c('KEY' = 'PARENT_KEY')) %>% 
    filter(!(hhid %in% dropped_hhid$dropped_hhid)) 
  # IMPORTANT NOTE: dob and sex should be used from demography not from safety or efficacy
  
  # download the data
  write_csv(demography_dataset, file = 'kenya_demography.csv')


  
# EFFICACY
  
  efficacy <- cloudbrewr::aws_s3_get_table(
    bucket = DATA_STAGING_BUCKET_NAME,
    key = glue::glue('{PROJECT_SOURCE}/sanitized-form/efficacy/efficacy.csv')) %>%
    pad_hhid()
    # no changes need to be made to this dataset

  efficacy_dataset <- efficacy
  
  # save the csv file
  write_csv(efficacy_dataset, file = 'kenya_efficacy.csv')


  
# HEALTH ECON BASELINE NEW

  healthecon_baseline_new <- cloudbrewr::aws_s3_get_table(
    bucket = DATA_STAGING_BUCKET_NAME,
    key = glue::glue('{PROJECT_SOURCE}/sanitized-form/healtheconnew/healtheconnew.csv')) %>%
    pad_hhid()
  
  healthecon_baseline_new_repeat <- cloudbrewr::aws_s3_get_table(
    bucket = DATA_STAGING_BUCKET_NAME,
    key = glue::glue('{PROJECT_SOURCE}/sanitized-form/healtheconnew/healtheconnew-repeat_individual.csv')) %>%
    pad_hhid()
  
  healthecon_baseline_new_dataset <- left_join(healthecon_baseline_new, healthecon_baseline_new_repeat, by=c('KEY' = 'PARENT_KEY'))
  
  # save the csv file
  write_csv(healthecon_baseline_new_dataset, file = 'kenya_healthecon_baseline_new.csv')


  
# HEALTH ECON MONTHLY
  
  healthecon_monthly <- cloudbrewr::aws_s3_get_table(
    bucket = DATA_STAGING_BUCKET_NAME,
    key = glue::glue('{PROJECT_SOURCE}/sanitized-form/healtheconmonthly/healtheconmonthly.csv')) %>%
    pad_hhid()
  
  healthecon_monthly_repeat <- cloudbrewr::aws_s3_get_table(
    bucket = DATA_STAGING_BUCKET_NAME,
    key = glue::glue('{PROJECT_SOURCE}/sanitized-form/healtheconmonthly/healtheconmonthly-repeat_individual.csv')) %>%
    pad_hhid()
  
  # create the healthecon baseline dataset
  healthecon_monthly_dataset <- left_join(healthecon_monthly, healthecon_monthly_repeat, 
                                          by=c('KEY' = 'PARENT_KEY'))
  
  # save the csv file
  write_csv(healthecon_monthly_dataset, file = 'healthecon_monthly.csv')


  
# HEALTH ECON BASELINE

  healthecon_baseline <- cloudbrewr::aws_s3_get_table(
    bucket = DATA_STAGING_BUCKET_NAME,
    key = glue::glue('{PROJECT_SOURCE}/sanitized-form/healtheconbaseline/healtheconbaseline.csv')) %>%
    pad_hhid()
  
  healthecon_baseline_repeat <- cloudbrewr::aws_s3_get_table(
    bucket = DATA_STAGING_BUCKET_NAME,
    key = glue::glue('{PROJECT_SOURCE}/sanitized-form/healtheconbaseline/healtheconbaseline-repeat_individual.csv')) %>%
    pad_hhid()
  
  # create the healthecon baseline dataset
  healthecon_baseline_dataset <- left_join(healthecon_baseline, healthecon_baseline_repeat, 
                                           by=c('KEY' = 'PARENT_KEY'))
  
  # health econ baseline
  write_csv(healthecon_baseline_dataset, file = 'kenya_healthecon_baseline.csv')
  
  
=======
# PURPOSE: this RScript serves to access the original data from AWS and create datasets necessary for the BOHEMIA & DataLab collaboration. This dataset will be anonymized and have removed all necessary hhhids and extids that should not be used for analysis

#load libraries
library(tidyverse)
library(cloudbrewr)

# log into AWS where the data is stored
ENV_PIPELINE_STAGE <- Sys.getenv("PIPELINE_STAGE")
DATA_STAGING_BUCKET_NAME <- 'databrew.org'
DATA_LAKE_BUCKET_NAME <- 'bohemia-lake-db'
PROJECT_SOURCE <- 'kwale'
SE_FOLDER_TARGET <- glue::glue('{PROJECT_SOURCE}/sanitized-form')

pad_hhid <- function(data){
  if('hhid' %in% names(data)){
    data %>%
      dplyr::mutate(hhid = stringr::str_pad(hhid, 5, pad = "0"))
  }else{
    data
  }
}

tryCatch({
  logger::log_info('Attempt AWS login')
  # login to AWS - this will be bypassed if executed in CI/CD environment
  cloudbrewr::aws_login(
    role_name = 'cloudbrewr-aws-role',
    profile_name =  'cloudbrewr-aws-role',
    pipeline_stage = 'production')
  
}, error = function(e){
  logger::log_error('AWS Login Failed')
  stop(e$message)
})


# For each of the necessary datasets this script will:
# 1. load in the data,
# 2. make any necessary data cleaning edits and merges, and
# 3. download the dataset as the appropriate csv file



# SAFETY

  safety <- cloudbrewr::aws_s3_get_table(
    bucket = DATA_STAGING_BUCKET_NAME,
    key = glue::glue('{PROJECT_SOURCE}/sanitized-form/safety/safety.csv')) %>%
    pad_hhid() %>% 
    select(KEY, visit, todays_date, hhid, cluster) # select the columns we need
  
  safety_repeat_individual <- cloudbrewr::aws_s3_get_table(
    bucket = DATA_STAGING_BUCKET_NAME,
    key = glue::glue('{PROJECT_SOURCE}/sanitized-form/safety/safety-repeat_individual.csv')) %>%
    pad_hhid()
  
  safety_ae <- cloudbrewr::aws_s3_get_table(
    bucket = DATA_STAGING_BUCKET_NAME,
    key = glue::glue('{PROJECT_SOURCE}/sanitized-form/safety/safety-repeat_ae_symptom.csv')) %>%
    pad_hhid()
  
  safetynew <- cloudbrewr::aws_s3_get_table(
    bucket =DATA_STAGING_BUCKET_NAME,
    key = glue::glue('{PROJECT_SOURCE}/sanitized-form/safetynew/safetynew.csv')) %>%
    pad_hhid() %>% 
    select(KEY, visit, todays_date, hhid, cluster, intervention) # select the columns we need
  
  safetynew_repeat_individual <- cloudbrewr::aws_s3_get_table(
    bucket =DATA_STAGING_BUCKET_NAME,
    key = glue::glue('{PROJECT_SOURCE}/sanitized-form/safetynew/safetynew-repeat_individual.csv')) %>%
    pad_hhid()
  
    # at the discretion of the study, some households were dropped and need to be removed
    dropped_hhid <- cloudbrewr::aws_s3_get_table(
      bucket ='bohemia-lake-db',
      key = glue::glue('bohemia_ext_data/v0_dropped_hhid/v0_dropped_hhid.csv')) %>%
      pad_hhid()
    
    # make the hhid column have the preceding 0 if it needs it
    dropped_hhid$dropped_hhid <- sprintf("%05d", dropped_hhid$dropped_hhid)
  
  # create the safety dataset
    
  safety_dataset <- left_join(safety, safety_repeat_individual, by=c('KEY' = 'PARENT_KEY')) %>% 
    filter(!(hhid %in% dropped_hhid$dropped_hhid), # remove dropped households
           extid != '2437-61', # erroneous entry
           safety_status != 'UNDEFINED') # erroneous entry
  
  # create the safetynew dataset
  safetynew_dataset <- left_join(safetynew, safetynew_repeat_individual, 
                                 by=c('KEY' = 'PARENT_KEY')) %>% 
    filter(!(hhid %in% dropped_hhid$dropped_hhid), # remove dropped households
           extid != '2437-61', # erroneous entry
           safety_status != 'UNDEFINED') # erroneous entry
  
  # save the csv files
  write_csv(safety_dataset, file = 'kenya_safety.csv')
  write_csv(safetynew_dataset, file = 'kenya_safetynew.csv')
  write_csv(safety_ae, file = 'kenya_safety_adverse_events.csv')



# DEMOGRAPHY
  
  v0_dem_repeat <- cloudbrewr::aws_s3_get_table(
    bucket = DATA_STAGING_BUCKET_NAME,
    key = glue::glue('{PROJECT_SOURCE}/sanitized-form/v0demography/v0demography-repeat_individual.csv')) %>%
    pad_hhid()
  
  v0_demography <- cloudbrewr::aws_s3_get_table(
    bucket = DATA_STAGING_BUCKET_NAME,
    key = glue::glue('{PROJECT_SOURCE}/sanitized-form/v0demography/v0demography.csv')) %>%
    pad_hhid()
  
  # create the demography dataset
  demography_dataset <- left_join(v0_demography, v0_dem_repeat, by=c('KEY' = 'PARENT_KEY')) %>% 
    filter(!(hhid %in% dropped_hhid$dropped_hhid)) 
  # IMPORTANT NOTE: dob and sex should be used from demography not from safety or efficacy
  
  # download the data
  write_csv(demography_dataset, file = 'kenya_demography.csv')


  
# EFFICACY
  
  efficacy <- cloudbrewr::aws_s3_get_table(
    bucket = DATA_STAGING_BUCKET_NAME,
    key = glue::glue('{PROJECT_SOURCE}/sanitized-form/efficacy/efficacy.csv')) %>%
    pad_hhid()
    # no changes need to be made to this dataset

  efficacy_dataset <- efficacy
  
  # save the csv file
  write_csv(efficacy_dataset, file = 'kenya_efficacy.csv')


  
# HEALTH ECON BASELINE NEW

  healthecon_baseline_new <- cloudbrewr::aws_s3_get_table(
    bucket = DATA_STAGING_BUCKET_NAME,
    key = glue::glue('{PROJECT_SOURCE}/sanitized-form/healtheconnew/healtheconnew.csv')) %>%
    pad_hhid()
  
  healthecon_baseline_new_repeat <- cloudbrewr::aws_s3_get_table(
    bucket = DATA_STAGING_BUCKET_NAME,
    key = glue::glue('{PROJECT_SOURCE}/sanitized-form/healtheconnew/healtheconnew-repeat_individual.csv')) %>%
    pad_hhid()
  
  healthecon_baseline_new_dataset <- left_join(healthecon_baseline_new, healthecon_baseline_new_repeat, by=c('KEY' = 'PARENT_KEY'))
  
  # save the csv file
  write_csv(healthecon_baseline_new_dataset, file = 'kenya_healthecon_baseline_new.csv')


  
# HEALTH ECON MONTHLY
  
  healthecon_monthly <- cloudbrewr::aws_s3_get_table(
    bucket = DATA_STAGING_BUCKET_NAME,
    key = glue::glue('{PROJECT_SOURCE}/sanitized-form/healtheconmonthly/healtheconmonthly.csv')) %>%
    pad_hhid()
  
  healthecon_monthly_repeat <- cloudbrewr::aws_s3_get_table(
    bucket = DATA_STAGING_BUCKET_NAME,
    key = glue::glue('{PROJECT_SOURCE}/sanitized-form/healtheconmonthly/healtheconmonthly-repeat_individual.csv')) %>%
    pad_hhid()
  
  # create the healthecon baseline dataset
  healthecon_monthly_dataset <- left_join(healthecon_monthly, healthecon_monthly_repeat, 
                                          by=c('KEY' = 'PARENT_KEY'))
  
  # save the csv file
  write_csv(healthecon_monthly_dataset, file = 'healthecon_monthly.csv')


  
# HEALTH ECON BASELINE

  healthecon_baseline <- cloudbrewr::aws_s3_get_table(
    bucket = DATA_STAGING_BUCKET_NAME,
    key = glue::glue('{PROJECT_SOURCE}/sanitized-form/healtheconbaseline/healtheconbaseline.csv')) %>%
    pad_hhid()
  
  healthecon_baseline_repeat <- cloudbrewr::aws_s3_get_table(
    bucket = DATA_STAGING_BUCKET_NAME,
    key = glue::glue('{PROJECT_SOURCE}/sanitized-form/healtheconbaseline/healtheconbaseline-repeat_individual.csv')) %>%
    pad_hhid()
  
  # create the healthecon baseline dataset
  healthecon_baseline_dataset <- left_join(healthecon_baseline, healthecon_baseline_repeat, 
                                           by=c('KEY' = 'PARENT_KEY'))
  
  # health econ baseline
  write_csv(healthecon_baseline_dataset, file = 'kenya_healthecon_baseline.csv')
  
>>>>>>> 1dc0baa12e45e0299a5bc61535b067627469ca7a
