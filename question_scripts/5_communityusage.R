#PURPOSE:Scatterplot of efficacy data where the x-axis is malaria incidents and y-axis is percent usage and coloured by villages. 

#load data
library(tidyverse)
library(ggplot2)
library(dplyr)

kenya_efficacy_table<-kenya_efficacy_total

kenya_efficacy_table %>% 
  
  
