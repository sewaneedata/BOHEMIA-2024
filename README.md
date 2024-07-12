# BOHEMIA-2024

## Introduction

The [BOHEMIA project](https://bohemiaconsortium.org) is studying the efficacy of ivermectin in reducing the transmission of malaria. We worked with BOHEMIA, who provided us with the data, to explore the possible relationships between the usage of bed nets and other factors. The codes in this repository will accomplish the following:

- Summarize bed net and demographic data
- Analyze relationship between Ownership, Sufficiency, and Usage of bed nets
- Analyze differences in bednet usage over time within a specific dataset (efficacy)
- Analyze the effects of environmental factors on bednet usage
- Analyze the effects of community usage on malaria data

## Datasets

We worked with 10 datasets in total. Some of the datasets can be found [here](https://drive.google.com/drive/u/0/folders/1jq1dtoTScgSA9h5vtEtLAtdjp_tF-Nxs), and the remaining datasets are retrieved by the `data.r` script.

1. `kenya_demography.csv` : has demographic information on the sample population.
2. `kenya_safety.csv` : has information on usage of bed nets over 4 visits.
3. `kenya_safety_new.csv`: has information on usage of bed nets for people added in the survey after the initial visit (for kenya_demography). 
4. `kenya_efficacy.csv` : has information on the usage of bed nets for children aged 5-15 over 7 visits. And the visits are 2 weeks before kenya_safety visits.
5. `kenya_healthecon_baseline.csv` : has information on the ownership of bed nets.
6. `kenya_healthecon_baseline_new.csv` : has information on the ownership of bed nets for people added in  the survey after the initial visit (for kenya_demography)
7. `healthecon_monthly.csv` : has monthly information on the price of bed nets.
8. `malaria_incidence` : has information on malaria incidents over 7 visits for each cluster.
9. `kenya_ae` : has information on the adverse event symptoms.
10. `weather` : has information on the weather data for Kwale in 2022.

## Question Scripts

 * Repository Folder :
    - `data.r` : R script used to load all datasets. We then clean the datasets by selecting the necessary columns, and merging the original datasets with the new datasets and adding demographic information.
    - `aws_data_download.r` : R script used to download datasets from AWS (Amazon Web Services).
  
 * question_scripts Folder :
   1. `1_bednets.r` : R script for answering questions related to Bed Nets. Specifically, price of bed nets, number of bed nets per household, demographics of people who use bed nets.
   2. `2_correlations.r` : R script for analyzing data on ownership, sufficiency, and usage of bed nets for both safety and efficacy dataset.
   3. `3_differences.r` : R script creating histograms and bar charts showing the usage of bed nets for the efficacy dataset over 7 visits (6 months). Also, a layered bar chart showing the relation in columns: slept_under_net_last_night and num_nights_sleep_under_net (over last week).
   4. `4_effects.r` : R script showing the relationship between bed net usage and environmental factors (temperature and precipitation) for both safety and efficacy datasets (over 4 months) along with only efficacy dataset (over 6 months).
   5. `5_communmityusage.r` : R script trying to analyze if there is a relationship between malaria incidents and bed net usage for each cluster using a scatterplot.
   6. `6_autocorsafe.r` : R script analysing the likelihood of someone using or not using a bed net based on their previous answers for safety dataset.
   7. `7_autocoreff.r`: R script analysing the likelihood of someone using or not using a bed net based on their previous answers for the efficacy dataset.
  
## Instructions

- Retrieve the datasets and place them in a seperate dataset folder inside the repository.
- Run the R scripts in the following order :
    1. `data.r`
    2. `1_bednets.r`
    3. `2_correlations.r`
    4. `3_differences.r`
    5. `4_effects.r`
    6. `5_communityusage.r`
    7. `6_autocorsafe.R`
    8. `7_autocoreff.R`

