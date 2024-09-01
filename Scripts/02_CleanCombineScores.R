library(tidyverse)
library(readxl)
source("functions/clean_scores.R")

#Here we create a full data frame of school-level standardized test score data
#This takes a while due to the magnitude of the data being processed
years <- c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)
FullScores <- tibble(School_Name = c(), School_ID = c(), Num_Tests = c(), Part_Rate = c(), SAT_ERW = c(), SAT_Math = c(), SAT_Total = c(), Target_Grp = c(), Year = c(), State = c(), FIPS = c(), Test = c(), TestType = c())
for(year in years) {
  FullScores <- rbind(FullScores, cleanScores(year))
}
#Save full data frame
save(FullScores, file = "Data/Cleaned_Scores/cleaned_scores_all_years.Rdata")
