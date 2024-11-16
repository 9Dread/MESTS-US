library(tidyverse)
library(catboost)
#Full variable set:
load("Data/tmp.Rdata")

pumaWealth2020 <- read_csv("Data/puma_wealth_inequality.csv") |> 
  filter(year == 2020) |> 
  select(!contains("sd")) |> 
  select(!obs) |> 
  rename(FIPS = statefip)
df <- df |> select(!geometry) |> as_tibble() |> 
  mutate(puma = as.numeric(puma)) |> 
  select(Year, Test, NCES_ID, puma, FIPS, tract2010, tract2020, zip, SAT_Total, SAT_Math, SAT_ERW) |> 
  left_join(pumaWealth2020, by=c("FIPS", "puma"))
rm(pumaWealth2020)

#Here we join with Social Capital data. 
scZIP <- read_csv("Data/Social_Capital/social_capital_zip.csv") |> 
  select(zip, civic_organizations_zip, volunteering_rate_zip, clustering_zip, support_ratio_zip)
df <- semi_join(df, scZIP, by="zip") |> 
  left_join(scZIP, by="zip")
rm(scZIP)

scHS <- read_csv("Data/Social_Capital/social_capital_high_school.csv") |> 
  rename(NCES_ID = high_school) |> 
  select(!c("high_school_name","zip","county","students_9_to_12")) |> 
  select(!contains("_se_"))
df <- semi_join(df, scHS, by="NCES_ID") |> 
  left_join(scHS, by="NCES_ID")
rm(scHS)

#Now add Opportunity Atlas data
tractUM <- read_csv("Data/Opportunity_Atlas/tract_inc_age35_allSubgroups.csv") |> 
  select(tract, contains("rP")) |> 
  select(tract, contains("gP")) |> 
  rename(tract2010 = tract)
df <- df |> 
  mutate(tract2010 = as.numeric(tract2010)) |> 
  semi_join(tractUM, by="tract2010") |> 
  left_join(tractUM, by="tract2010")
rm(tractUM)

tractNC <- read_csv("Data/Opportunity_Atlas/tract_allNeighborhoodCharacteristics.csv") |> 
  select(!c(id2, name, aland_sqmi))  |> 
  rename(tract2010 = tract)
df <- df |> 
  mutate(tract2010 = as.numeric(tract2010)) |> 
  semi_join(tractNC, by="tract2010") |> 
  left_join(tractNC, by="tract2010")
rm(tractNC)
df <- select(df, !c("cntygp97", "cntygp98"))
set.seed(1667)
frame <- drop_na(df) |> 
  distinct()
ind <- createDataPartition(frame$SAT_Total, p = 0.9, list = FALSE)
train <- frame[ind,]
test <- frame[-ind,]
write_csv(train, "Data/train_test/train_full.csv")
write_csv(test, "Data/train_test/test_full.csv")
