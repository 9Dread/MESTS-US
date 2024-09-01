library(tidyverse)
library(readxl)
library(arcgisbinding)
library(tidycensus)
arc.check_product()

#Load and standardize SAT data
#Only NC and SC have test-taker and percent-taken variables. TX has a primitive mask. Maybe we can use school size as a proxy.
#For now, ignore those and focus on scores only.
njSAT2020 <- read_excel("2020_SATS/2020_SAT_NJ.xlsx", sheet = 2) |>
  filter(Test == "SAT") |>
  mutate(School_Avg = as.double(School_Avg)) |> 
  pivot_wider(id_cols = c(CountyCode, CountyName, DistrictCode, DistrictName, SchoolCode, SchoolName), values_from = School_Avg, names_from = Subject) |>
  mutate(School_ID = str_c(CountyCode, DistrictCode, SchoolCode), Total = Math + `Reading and Writing`, ERW = `Reading and Writing`, School_Name = SchoolName) |> 
  select(School_ID, School_Name, ERW, Math, Total) |> 
  rename(SAT_ERW = ERW, SAT_Math = Math, SAT_Total = Total)

ncSAT2020 <- read_excel("2020_SATS/2020_SAT_NC.xlsx") |> 
  select(!c('...4', '...5')) |> 
  rename(School_Name = '...3', District_Code = '...1', School_Code = 'School System &School', Test_Takers = '# Tested', Percent_Tested = '% Tested', SAT_Total = Total, SAT_ERW = ERW, SAT_Math = Math) |> 
  drop_na(District_Code, School_Name) |> 
  select(!c(Test_Takers, Percent_Tested)) |> 
  mutate(District_Code = ifelse(nchar(District_Code) == 2, str_c('0', District_Code), District_Code)) |> 
  mutate(School_ID = str_c(District_Code, School_Code)) |> 
  select(!c(District_Code, School_Code))

scSAT2020 <- read_excel("2020_SATS/2020_SAT_SC.xlsx", skip = 2)  |> 
  select(!Num_Grade_12) |> 
  drop_na(School_ID) |> 
  select(!c(District_Name, Test_Takers, Percent_Tested))

txSAT2020 <- read_excel("2020_SATS/2020_SAT_TX.xlsx", sheet = 4) |> 
  filter(Group == "All Students") |> 
  select(Campus, CampName, ERW, Math, Total) |> 
  mutate(Total = as.double(Total)) |> 
  rename(School_ID = Campus, School_Name = CampName, SAT_ERW = ERW, SAT_Math = Math, SAT_Total = Total)

maSAT2020 <- read_excel("2020_SATS/2020_SAT_MA.xlsx", skip = 1) |> 
  select(!c(`Tests Taken`, Writing)) |> 
  rename(School_Name = `School Name`, School_ID = `School Code`, SAT_ERW = `Reading / Writing`, SAT_Math = Math) |> 
  mutate(SAT_Total = SAT_ERW + SAT_Math)

gaSAT2020 <- read_csv("2020_SATS/2020_SAT_GA.csv") |>
  select(!c(LONG_SCHOOL_YEAR, SCHOOL_DSTRCT_NM, SUBGRP_DESC, NATIONAL_NUM_TESTED_CNT, STATE_NUM_TESTED_CNT, DSTRCT_NUM_TESTED_CNT, INSTN_NUM_TESTED_CNT, STATE_AVG_SCORE_VAL, DSTRCT_AVG_SCORE_VAL)) |> 
  mutate(School_ID = str_c(SCHOOL_DISTRCT_CD, INSTN_NUMBER)) |> 
  select(!c(SCHOOL_DISTRCT_CD, INSTN_NUMBER)) |> 
  filter(TEST_CMPNT_TYP_CD != "Essay Analysis Score - New", TEST_CMPNT_TYP_CD != "Essay Reading Score - New", TEST_CMPNT_TYP_CD != "Essay Total", TEST_CMPNT_TYP_CD != "Essay Writing Score - New") |> 
  pivot_wider(names_from = TEST_CMPNT_TYP_CD, values_from = INSTN_AVG_SCORE_VAL) |> 
  rename(School_Name = INSTN_NAME, SAT_Total = `Combined Test Score`, SAT_Math = `Math Section Score - New`, SAT_RD = `Reading Test  Score - New`, SAT_WR = `WritLang Test  Score - New`) |> 
  mutate(SAT_Total = as.numeric(SAT_Total), SAT_RD = as.numeric(SAT_RD), SAT_WR = as.numeric(SAT_WR), SAT_Math = as.numeric(SAT_Math)) |> 
  mutate(SAT_ERW = SAT_RD + SAT_WR) |> 
  select(!c(SAT_RD, SAT_WR))

coSAT2019 <- read_excel("2020_SATS/2019_SAT_CO.xlsx", skip = 10) |>
  filter(`School Name` != "All Schools", Test == "SAT") |> 
  mutate(School_ID = str_c(`District Code`, `School Code`)) |> 
  rename(School_Name = `School Name`, SAT_Total = `Total Score Mean Score...12`, SAT_ERW = `Evidence-Based Reading & Writing Mean Score...14`, SAT_Math = `Mathematics Mean Score...16`) |> 
  select(School_ID, School_Name, SAT_Total, SAT_ERW, SAT_Math) |> 
  mutate(SAT_Total = as.numeric(SAT_Total), SAT_ERW = as.numeric(SAT_ERW), SAT_Math = as.numeric(SAT_Math))
  

#Now, match school IDs to NCES IDs.
#These data frames are from NCES' public school search tool.
njSchools <- read_excel("nces_and_state_ids/nces_and_state_id_nj.xlsx", skip = 14) |> 
  select(`NCES School ID`, `State School ID`, `Low Grade*`, `High Grade*`, `Locale Code*`, `Locale*`, `Students*`, `Teachers*`, `Student Teacher Ratio*`, `Free Lunch*`, `Reduced Lunch*`, Type) |> 
  rename(NCES_ID = `NCES School ID`, School_ID = `State School ID`, Low_Grade = `Low Grade*`, High_Grade = `High Grade*`, Locale_Code = `Locale Code*`, Locale = `Locale*`, Num_Students = `Students*`, Num_Teachers = `Teachers*`, Student_Teacher_Ratio = `Student Teacher Ratio*`, Num_Free_Lunch = `Free Lunch*`, Num_Reduced_Lunch = `Reduced Lunch*`, School_Type = Type)  |> 
  mutate(School_ID = str_c(str_sub(School_ID, 4, 9), str_sub(School_ID, 11, -1)), fips = 34)
njSchools <- left_join(njSAT2020, njSchools, by = "School_ID")

ncSchools <- read_excel("nces_and_state_ids/nces_and_state_id_nc.xlsx", skip = 14) |> 
  select(`NCES School ID`, `State School ID`, `Low Grade*`, `High Grade*`, `Locale Code*`, `Locale*`, `Students*`, `Teachers*`, `Student Teacher Ratio*`, `Free Lunch*`, `Reduced Lunch*`, Type) |> 
  rename(NCES_ID = `NCES School ID`, School_ID = `State School ID`, Low_Grade = `Low Grade*`, High_Grade = `High Grade*`, Locale_Code = `Locale Code*`, Locale = `Locale*`, Num_Students = `Students*`, Num_Teachers = `Teachers*`, Student_Teacher_Ratio = `Student Teacher Ratio*`, Num_Free_Lunch = `Free Lunch*`, Num_Reduced_Lunch = `Reduced Lunch*`, School_Type = Type) |> 
  mutate(School_ID = str_c(str_sub(School_ID, 4, 6), str_sub(School_ID, 8, 10)), fips = 37)
ncSchools <- left_join(ncSAT2020, ncSchools, by = "School_ID")

scSchools <- read_excel("nces_and_state_ids/nces_and_state_id_sc.xlsx", skip = 14) |> 
  select(`NCES School ID`, `State School ID`, `Low Grade*`, `High Grade*`, `Locale Code*`, `Locale*`, `Students*`, `Teachers*`, `Student Teacher Ratio*`, `Free Lunch*`, `Reduced Lunch*`, Type) |> 
  rename(NCES_ID = `NCES School ID`, School_ID = `State School ID`, Low_Grade = `Low Grade*`, High_Grade = `High Grade*`, Locale_Code = `Locale Code*`, Locale = `Locale*`, Num_Students = `Students*`, Num_Teachers = `Teachers*`, Student_Teacher_Ratio = `Student Teacher Ratio*`, Num_Free_Lunch = `Free Lunch*`, Num_Reduced_Lunch = `Reduced Lunch*`, School_Type = Type) |>
  mutate(School_ID = str_c(str_sub(School_ID, 4, 7), str_sub(School_ID, 9, 11)), fips = 45)
scSchools <- left_join(scSAT2020, scSchools, by = "School_ID")

txSchools <- read_excel("nces_and_state_ids/nces_and_state_id_tx.xlsx", skip = 14) |> 
  select(`NCES School ID`, `State School ID`, `Low Grade*`, `High Grade*`, `Locale Code*`, `Locale*`, `Students*`, `Teachers*`, `Student Teacher Ratio*`, `Free Lunch*`, `Reduced Lunch*`, Type) |> 
  rename(NCES_ID = `NCES School ID`, School_ID = `State School ID`, Low_Grade = `Low Grade*`, High_Grade = `High Grade*`, Locale_Code = `Locale Code*`, Locale = `Locale*`, Num_Students = `Students*`, Num_Teachers = `Teachers*`, Student_Teacher_Ratio = `Student Teacher Ratio*`, Num_Free_Lunch = `Free Lunch*`, Num_Reduced_Lunch = `Reduced Lunch*`, School_Type = Type) |> 
  mutate(School_ID = str_sub(School_ID, 11, -1), fips = 48)
txSchools <- left_join(txSAT2020, txSchools, by = "School_ID")

maSchools <- read_excel("nces_and_state_ids/nces_and_state_id_ma.xlsx", skip = 14) |> 
  select(`NCES School ID`, `State School ID`, `Low Grade*`, `High Grade*`, `Locale Code*`, `Locale*`, `Students*`, `Teachers*`, `Student Teacher Ratio*`, `Free Lunch*`, `Reduced Lunch*`, Type) |> 
  rename(NCES_ID = `NCES School ID`, School_ID = `State School ID`, Low_Grade = `Low Grade*`, High_Grade = `High Grade*`, Locale_Code = `Locale Code*`, Locale = `Locale*`, Num_Students = `Students*`, Num_Teachers = `Teachers*`, Student_Teacher_Ratio = `Student Teacher Ratio*`, Num_Free_Lunch = `Free Lunch*`, Num_Reduced_Lunch = `Reduced Lunch*`, School_Type = Type) |> 
  mutate(School_ID = str_sub(School_ID, 9, -1), fips = 25)
maSchools <- left_join(maSAT2020, maSchools, by = "School_ID")

gaSchools <- read_excel("nces_and_state_ids/nces_and_state_id_ga.xlsx", skip = 14) |> 
  select(`NCES School ID`, `State School ID`, `Low Grade*`, `High Grade*`, `Locale Code*`, `Locale*`, `Students*`, `Teachers*`, `Student Teacher Ratio*`, `Free Lunch*`, `Reduced Lunch*`, Type) |> 
  rename(NCES_ID = `NCES School ID`, School_ID = `State School ID`, Low_Grade = `Low Grade*`, High_Grade = `High Grade*`, Locale_Code = `Locale Code*`, Locale = `Locale*`, Num_Students = `Students*`, Num_Teachers = `Teachers*`, Student_Teacher_Ratio = `Student Teacher Ratio*`, Num_Free_Lunch = `Free Lunch*`, Num_Reduced_Lunch = `Reduced Lunch*`, School_Type = Type) |> 
  mutate(School_ID = ifelse(str_sub(School_ID, 7, 7) == "-", str_c(str_sub(School_ID, 4, 6), str_sub(School_ID, 8, 11)), str_sub(School_ID, 4, 10)), fips = 13)
gaSchools <- left_join(gaSAT2020, gaSchools, by = "School_ID")

coSchools <- read_excel("nces_and_state_ids/nces_and_state_id_co.xlsx", skip = 14) |> 
  select(`NCES School ID`, `State School ID`, `Low Grade*`, `High Grade*`, `Locale Code*`, `Locale*`, `Students*`, `Teachers*`, `Student Teacher Ratio*`, `Free Lunch*`, `Reduced Lunch*`, Type) |> 
  rename(NCES_ID = `NCES School ID`, School_ID = `State School ID`, Low_Grade = `Low Grade*`, High_Grade = `High Grade*`, Locale_Code = `Locale Code*`, Locale = `Locale*`, Num_Students = `Students*`, Num_Teachers = `Teachers*`, Student_Teacher_Ratio = `Student Teacher Ratio*`, Num_Free_Lunch = `Free Lunch*`, Num_Reduced_Lunch = `Reduced Lunch*`, School_Type = Type) |> 
  mutate(School_ID = str_c(str_sub(School_ID, 4, 7), str_sub(School_ID, 9, 12)), fips = 8)
coSchools <- left_join(coSAT2019, coSchools, by = "School_ID")

#Create a combined schools frame
allSchools <- rbind(njSchools, ncSchools, scSchools, txSchools, coSchools, maSchools, gaSchools) |> 
  select(!c(Num_Free_Lunch, Num_Reduced_Lunch, School_ID)) |> 
  filter(High_Grade == "12" | High_Grade == "13") |> 
  drop_na(NCES_ID)

#Geographic poverty dataset
schoolpov <- arc.open("C:/Users/dread/OneDrive/Documents/ArcGIS/Packages/school_pov_metrics_05f34c/p20/school_pov_metrics.lyrx")
schoolpov_df <- arc.select(object = schoolpov)
schoolpov_df <- schoolpov_df |> 
  filter(fips == 34 | fips == 37 | fips == 45 | fips == 48 | fips == 25 | fips == 13 | fips == 8) |> 
  select(!c(NAME, fips, ncessch_num)) |> 
  rename(NCES_ID = NCESSCH, NCES_ID_1 = NCESSCH1)

#Now, join everything together, then write back into ArcGIS
allSchools <- left_join(allSchools, schoolpov_df, by = "NCES_ID") |> 
  mutate(SAT_Total = as.numeric(SAT_Total), SAT_ERW = as.numeric(SAT_ERW), SAT_Math = as.numeric(SAT_Math), Num_Students = as.numeric(Num_Students), Num_Teachers = as.numeric(Num_Teachers), Student_Teacher_Ratio = as.numeric(Student_Teacher_Ratio))

#Calling census data for a few more variables
#If you need an API key, get one here:
#https://api.census.gov/data/key_signup.html
census_api_key('YOUR API KEY HERE')

#male_num_enroll_pub_grade5to8 = "B14002_014"
#female_num_enroll_pub_grade5to8 = "B14002_038"

maHSEnroll <- get_acs(geography = "block", 
            variables = c(male_num_enroll_pub_grade9to12 = "B14002_017", female_num_enroll_pub_grade9to12 = "B14002_041"), 
            state = "MA", 
            year = 2020) |> 
  group_by(GEOID) |> 
  reframe(estimate = sum(estimate),
          moe = moe_sum(moe, estimate),
          name = NAME) |> 
  distinct(name, .keep_all = TRUE)

gdb_path <- "C:/Users/dread/OneDrive/Documents/ArcGIS/Projects/EducationOpportunity/EducationOpportunity.gdb"
arc.write(file.path(gdb_path, "all_schools_data_pts"), data=allSchools, coords=c("LON", "LAT"), shape_info=list(type='Point',hasZ=FALSE), overwrite = TRUE)
