library(tidyverse)
library(readxl)
library(tidycensus)
library(tigris)
library(mapview)

#We need SC, 
scHS <- read_csv("Data/Social_Capital/social_capital_high_school.csv") |> 
  mutate(ec_own_ses_se_rate = ec_own_ses_se_hs/ec_own_ses_hs, 
         ec_parent_ses_se_rate = ec_parent_ses_se_hs/ec_parent_ses_hs, 
         ec_high_own_ses_se_rate = ec_high_own_ses_se_hs/ec_high_own_ses_hs,
         ec_high_parent_ses_se_rate = ec_high_parent_ses_se_hs/ec_high_parent_ses_hs) |> 
  rename(NCES_ID = high_school)

#SE is generally low enough
ggplot(scHS) +
  geom_histogram(aes(x = ec_own_ses_se_rate))
ggplot(scHS) +
  geom_histogram(aes(x = ec_parent_ses_se_rate))
ggplot(scHS) +
  geom_histogram(aes(x = ec_high_own_ses_se_rate))
ggplot(scHS) +
  geom_histogram(aes(x = ec_high_parent_ses_se_rate))

scZIP <- read_csv("Data/Social_Capital/social_capital_zip.csv") |> 
  select(c(zip, ec_zip, ec_se_zip, civic_organizations_zip, support_ratio_zip)) |> 
  mutate(ec_zip_se_rate = ec_se_zip/ec_zip)

ggplot(scZIP) +
  geom_histogram(aes(x = ec_zip_se_rate))


load('Data/NJ_ACTSAT_Comparison_Train_test/full_frame.Rdata')
schools <- fullSchools |> 
  select(School_ID, SAT_Math, SAT_ERW, SAT_Total, SAT_Participation)
njSchools <- read_excel("Data/nces_and_state_ids/nces_and_state_id_nj.xlsx", skip = 14) |> 
  select(ZIP, `NCES School ID`, `State School ID`, `Low Grade*`, `High Grade*`, `Locale Code*`, `Locale*`, Type) |> 
  rename(zip = ZIP, NCES_ID = `NCES School ID`, School_ID = `State School ID`, Low_Grade = `Low Grade*`, High_Grade = `High Grade*`, Locale_Code = `Locale Code*`, Locale = `Locale*`, School_Type = Type)  |> 
  mutate(School_ID = str_c(str_sub(School_ID, 4, 9), str_sub(School_ID, 11, -1)), fips = 34, Locale = as.factor(Locale), zip = as.numeric(zip))
njSchools <- left_join(schools, njSchools, by = "School_ID")

#merge with scZIP, scHS
scHS |> 
  mutate(len = str_length(NCES_ID)) |> 
  distinct(len)
#Some 8 long, some 12 long?

scSchools <- semi_join(njSchools, scHS, by = 'NCES_ID')
#about 500 missing; maybe impute values using other vars?

maHSEnroll <- get_acs(geography = "tract", 
                      variables = c(male_num_enroll_pub_grade9to12 = "B14002_017", female_num_enroll_pub_grade9to12 = "B14002_041"), 
                      state = "MA", 
                      year = 2020,
                      geometry = TRUE)
ggplot(maHSEnroll) +
  geom_sf() +
  theme_void()
#Tracts change every 10 yr
#Interactive ArcGIS map:
mapview(maHSEnroll)
