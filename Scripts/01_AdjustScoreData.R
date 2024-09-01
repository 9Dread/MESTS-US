library(tidyverse)
library(readxl)
library(writexl)
#To make the process of cleaning un-standardized SAT/ACT data easier, we adjust some of the data files here.

#Combine ERW/Math SAT files for recent Colorado data:
CO_Combined_2021 <- cbind(
  read_excel(path = "Data/CO_Tests/2021_SAT_ERW_CO.xlsx", skip=27, sheet = "Gender") |> 
    rename_with(.fn = ~ str_remove_all(.x, pattern = "\n")) |>
    rename_with(.fn = ~ str_remove_all(.x, pattern = "\r")) |>
    rename_with(.fn = ~ str_remove_all(.x, pattern = " ")) |> 
    select(Level, DistrictCode, SchoolCode, SchoolName, Grade, Gender, NumberofValidScores, ParticipationRate, MeanScaleScore) |> 
    rename(EBRW = MeanScaleScore),
  read_excel(path = "Data/CO_Tests/2021_SAT_Math_CO.xlsx", skip=27, sheet = "Gender") |> 
    rename_with(.fn = ~ str_remove_all(.x, pattern = "\n")) |>
    rename_with(.fn = ~ str_remove_all(.x, pattern = "\r")) |>
    rename_with(.fn = ~ str_remove_all(.x, pattern = " ")) |> 
    select(MeanScaleScore) |> 
    rename(Mathematics = MeanScaleScore)
) |> 
  mutate(Total = as.numeric(EBRW) + as.numeric(Mathematics))
write_xlsx(CO_Combined_2021, "Data/CO_Tests/2021_SAT_CO.xlsx")

CO_Combined_2022 <- cbind(
  read_excel(path = "Data/CO_Tests/2022_SAT_ERW_CO.xlsx", skip=12, sheet = "Gender") |> 
    rename_with(.fn = ~ str_remove_all(.x, pattern = "\n")) |>
    rename_with(.fn = ~ str_remove_all(.x, pattern = "\r")) |>
    rename_with(.fn = ~ str_remove_all(.x, pattern = " ")) |> 
    select(Level, DistrictCode, SchoolCode, SchoolName, Grade, Gender, NumberofValidScores, ParticipationRate, MeanScaleScore) |> 
    rename(EBRW = MeanScaleScore),
  read_excel(path = "Data/CO_Tests/2022_SAT_Math_CO.xlsx", skip=12, sheet = "Gender") |> 
    rename_with(.fn = ~ str_remove_all(.x, pattern = "\n")) |>
    rename_with(.fn = ~ str_remove_all(.x, pattern = "\r")) |>
    rename_with(.fn = ~ str_remove_all(.x, pattern = " ")) |> 
    select(MeanScaleScore) |> 
    rename(Mathematics = MeanScaleScore)
) |> 
  mutate(Total = as.numeric(EBRW) + as.numeric(Mathematics))
write_xlsx(CO_Combined_2022, "Data/CO_Tests/2022_SAT_CO.xlsx")

CO_Combined_2023 <- cbind(
  read_excel(path = "Data/CO_Tests/2023_SAT_ERW_CO.xlsx", skip=12, sheet = "Gender") |> 
    rename_with(.fn = ~ str_remove_all(.x, pattern = "\n")) |>
    rename_with(.fn = ~ str_remove_all(.x, pattern = "\r")) |>
    rename_with(.fn = ~ str_remove_all(.x, pattern = " ")) |> 
    select(Level, DistrictCode, SchoolCode, SchoolName, Grade, Gender, NumberofValidScores, ParticipationRate, MeanScaleScore) |> 
    rename(EBRW = MeanScaleScore),
  read_excel(path = "Data/CO_Tests/2023_SAT_Math_CO.xlsx", skip=12, sheet = "Gender") |> 
    rename_with(.fn = ~ str_remove_all(.x, pattern = "\n")) |>
    rename_with(.fn = ~ str_remove_all(.x, pattern = "\r")) |>
    rename_with(.fn = ~ str_remove_all(.x, pattern = " ")) |> 
    select(MeanScaleScore) |> 
    rename(Mathematics = MeanScaleScore)
) |> 
  mutate(Total = as.numeric(EBRW) + as.numeric(Mathematics))
write_xlsx(CO_Combined_2023, "Data/CO_Tests/2023_SAT_CO.xlsx")

rm(CO_Combined_2021, CO_Combined_2022, CO_Combined_2023)


#Adjust TX Campus codes as needed, save them as excel if they are CSVs
#NCES grad counts and the provided participation rates to calculate test counts?
#12th grade counts arent the same as number of grads
#For now, just use masked counts as actual counts
TX_ACT_2011 <- read_csv("Data/TX_Tests/2011_ACT_TX.csv") |> 
  mutate(CAMPUS = as.character(CAMPUS)) |> 
  mutate(CAMPUS = case_when(
    str_length(CAMPUS) == 7 ~ str_c("00",CAMPUS),
    str_length(CAMPUS) == 8 ~ str_c("0",CAMPUS),
    str_length(CAMPUS) == 9 ~ CAMPUS),
  ) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '"'))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '='))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '<')))
write_xlsx(TX_ACT_2011, "Data/TX_Tests/2011_ACT_TX.xlsx")
TX_ACT_2012 <- read_csv("Data/TX_Tests/2012_ACT_TX.csv") |> 
  mutate(across(everything(), \(x) str_remove_all(x, '"'))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '='))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '<')))
write_xlsx(TX_ACT_2012, "Data/TX_Tests/2012_ACT_TX.xlsx")
TX_ACT_2013 <- read_csv("Data/TX_Tests/2013_ACT_TX.csv") |> 
  mutate(across(everything(), \(x) str_remove_all(x, '"'))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '='))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '<')))
write_xlsx(TX_ACT_2013, "Data/TX_Tests/2013_ACT_TX.xlsx")
TX_ACT_2014 <- read_csv("Data/TX_Tests/2014_ACT_TX.csv") |> 
  mutate(across(everything(), \(x) str_remove_all(x, '"'))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '='))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '<')))
write_xlsx(TX_ACT_2014, "Data/TX_Tests/2014_ACT_TX.xlsx")
TX_ACT_2015 <- read_csv("Data/TX_Tests/2015_ACT_TX.csv") |> 
  mutate(across(everything(), \(x) str_remove_all(x, '"'))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '='))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '<')))
write_xlsx(TX_ACT_2015, "Data/TX_Tests/2015_ACT_TX.xlsx")
TX_ACT_2016 <- read_csv("Data/TX_Tests/2016_ACT_TX.csv") |> 
  mutate(across(everything(), \(x) str_remove_all(x, '"'))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '='))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '<')))
write_xlsx(TX_ACT_2016, "Data/TX_Tests/2016_ACT_TX.xlsx")
TX_ACT_2017 <- read_csv("Data/TX_Tests/2017_ACT_TX.csv") |> 
  mutate(across(everything(), \(x) str_remove_all(x, '"'))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '='))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '<')))
write_xlsx(TX_ACT_2017, "Data/TX_Tests/2017_ACT_TX.xlsx")
TX_SAT_2017 <- read_csv("Data/TX_Tests/2017_SAT_TX.csv") |> 
  mutate(across(everything(), \(x) str_remove_all(x, '"'))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '='))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '<')))
write_xlsx(TX_SAT_2017, "Data/TX_Tests/2017_SAT_TX.xlsx")
TX_ACT_2018 <- read_excel("Data/TX_Tests/2018_ACT_TX_Base.xlsx", sheet="ACT_Campus_Data_Class_2018") |> 
  mutate(across(everything(), \(x) str_remove_all(x, '"'))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '='))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '<')))
write_xlsx(TX_ACT_2018, "Data/TX_Tests/2018_ACT_TX.xlsx")
TX_SAT_2018 <- read_excel("Data/TX_Tests/2018_SAT_TX_Base.xlsx", sheet="SAT_Campus_Data_Class_2018") |> 
  mutate(across(everything(), \(x) str_remove_all(x, '"'))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '='))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '<')))
write_xlsx(TX_SAT_2018, "Data/TX_Tests/2018_SAT_TX.xlsx")
TX_ACT_2019 <- read_excel("Data/TX_Tests/2019_ACT_TX_Base.xlsx", sheet="ACT_Campus_Data_Class_2019") |> 
  mutate(across(everything(), \(x) str_remove_all(x, '"'))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '='))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '<')))
write_xlsx(TX_ACT_2019, "Data/TX_Tests/2019_ACT_TX.xlsx")
TX_SAT_2019 <- read_excel("Data/TX_Tests/2019_SAT_TX_Base.xlsx", sheet="SAT_Campus_Data_Class_2019") |> 
  mutate(across(everything(), \(x) str_remove_all(x, '"'))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '='))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '<')))
write_xlsx(TX_SAT_2019, "Data/TX_Tests/2019_SAT_TX.xlsx")
TX_ACT_2020 <- read_excel("Data/TX_Tests/2020_ACT_TX_Base.xlsx", sheet="act-campus-data-class-2020") |> 
  mutate(across(everything(), \(x) str_remove_all(x, '"'))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '='))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '<')))
write_xlsx(TX_ACT_2020, "Data/TX_Tests/2020_ACT_TX.xlsx")
TX_SAT_2020 <- read_excel("Data/TX_Tests/2020_SAT_TX_Base.xlsx", sheet="sat-campus-data-class-2020") |> 
  mutate(across(everything(), \(x) str_remove_all(x, '"'))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '='))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '<')))
write_xlsx(TX_SAT_2020, "Data/TX_Tests/2020_SAT_TX.xlsx")
TX_ACT_2021 <- read_excel("Data/TX_Tests/2021_ACT_TX_Base.xlsx", sheet="act-campus-data-class-2021") |> 
  mutate(across(everything(), \(x) str_remove_all(x, '"'))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '='))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '<')))
write_xlsx(TX_ACT_2021, "Data/TX_Tests/2021_ACT_TX.xlsx")
TX_SAT_2021 <- read_excel("Data/TX_Tests/2021_SAT_TX_Base.xlsx", sheet="sat-campus-data-class-2021") |> 
  mutate(across(everything(), \(x) str_remove_all(x, '"'))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '='))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '<')))
write_xlsx(TX_SAT_2021, "Data/TX_Tests/2021_SAT_TX.xlsx")
TX_ACT_2022 <- read_excel("Data/TX_Tests/2022_ACT_TX_Base.xlsx", sheet="act-campus-data-class-2022") |> 
  mutate(across(everything(), \(x) str_remove_all(x, '"'))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '='))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '<')))
write_xlsx(TX_ACT_2022, "Data/TX_Tests/2022_ACT_TX.xlsx")
TX_SAT_2022 <- read_excel("Data/TX_Tests/2022_SAT_TX_Base.xlsx", sheet="sat-campus-data-class-2022") |> 
  mutate(across(everything(), \(x) str_remove_all(x, '"'))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '='))) |> 
  mutate(across(everything(), \(x) str_remove_all(x, '<')))
write_xlsx(TX_SAT_2022, "Data/TX_Tests/2022_SAT_TX.xlsx")
rm(TX_ACT_2011, TX_ACT_2012, TX_ACT_2013, TX_ACT_2014, TX_ACT_2015, TX_ACT_2016, TX_ACT_2017, TX_SAT_2017, TX_ACT_2018, TX_SAT_2018, TX_ACT_2019, TX_SAT_2019, TX_ACT_2020, TX_SAT_2020, TX_ACT_2021, TX_SAT_2021, TX_ACT_2022, TX_SAT_2022)

#Adjust TN code lengths:
years <- c(2016, 2017, 2018, 2019, 2021, 2022, 2023)
for(year in years) {
  TN_ACT <- read_excel(paste0("Data/TN_Tests/",year,"_ACT_TN_Base.xlsx")) |> 
    mutate(School = as.character(School), District = as.character(District)) |> 
    mutate(District = case_when(
      str_length(District) == 1 ~ str_c("0000",District),
      str_length(District) == 2 ~ str_c("000",District),
      str_length(District) == 3 ~ str_c("00",District),
      str_length(District) == 4 ~ str_c("0",District),
      str_length(District) == 5 ~ District),
      School = case_when(
        str_length(School) == 1 ~ str_c("000",School),
        str_length(School) == 2 ~ str_c("00",School),
        str_length(School) == 3 ~ str_c("0",School),
        str_length(School) == 4 ~ School
      ))
  write_xlsx(TN_ACT, paste0("Data/TN_Tests/",year,"_ACT_TN.xlsx"))
}
TN_ACT <- read_csv("Data/TN_Tests/2020_ACT_TN_Base.csv") |> 
  mutate(School = as.character(School), District = as.character(District)) |> 
  mutate(District = case_when(
    str_length(District) == 1 ~ str_c("0000",District),
    str_length(District) == 2 ~ str_c("000",District),
    str_length(District) == 3 ~ str_c("00",District),
    str_length(District) == 4 ~ str_c("0",District),
    str_length(District) == 5 ~ District),
    School = case_when(
      str_length(School) == 1 ~ str_c("000",School),
      str_length(School) == 2 ~ str_c("00",School),
      str_length(School) == 3 ~ str_c("0",School),
      str_length(School) == 4 ~ School
    ))
write_xlsx(TN_ACT, "Data/TN_Tests/2020_ACT_TN.xlsx")
rm(TN_ACT, years, year)

#Create ACT and SAT data files from NJ school database files
nj_counts_2017 <- read_excel("Data/nces_and_state_ids/nces_and_state_id_nj.xlsx", skip = 14) |> 
  rename_with(.fn = ~ str_remove_all(.x, pattern = "\n")) |>
  rename_with(.fn = ~ str_remove_all(.x, pattern = "\r")) |>
  rename_with(.fn = ~ str_remove_all(.x, pattern = " ")) |> 
  select(NCESSchoolID,StateSchoolID) |> 
  rename(NCES_ID = NCESSchoolID, Code = StateSchoolID) |> 
  mutate(Code = str_c(str_sub(Code, 4, 9), str_sub(Code, 11, -1))) |> 
  left_join(
    read_csv("Data/11th_12th_grade_membership/membershipcount.csv", skip = 6) |> 
      rename_with(.fn = ~ str_remove_all(.x, pattern = " ")) |> 
      select(`SchoolID-NCESAssigned[PublicSchool]Latestavailableyear`, ends_with(str_sub(as.character(2017), 3, 4))) |> 
      rename_with(.fn = ~ str_sub(.x, 1, 7)) |> 
      rename(NCES_ID = SchoolI) |> 
      mutate(NCES_ID = as.character(NCES_ID), Grade12 = as.numeric(Grade12), Grade11 = as.numeric(Grade11)),
    by = "NCES_ID"
  )
NJ_ACT_2017 <- read_excel("Data/NJ_Tests/2017_NJ_School_Database.xlsx", sheet = "PSAT-SAT-ACTPerformance") |> 
  filter(Test == "ACT") |> 
  mutate(Code = str_c(CountyCode,DistrictCode,SchoolCode)) |> 
  select(Code, Subject, School_Avg) |> 
  pivot_wider(names_from = Subject, values_from = School_Avg) |> 
  mutate(English = as.numeric(English), Reading = as.numeric(Reading), Math = as.numeric(Math), Science = as.numeric(Science)) |> 
  mutate(Composite = round((English+Reading+Math+Science)/4))
ACTParticipation <- read_excel("Data/NJ_Tests/2017_NJ_School_Database.xlsx", sheet = "PSAT-SAT-ACTParticipation") |> 
  mutate(Code = str_c(CountyCode,DistrictCode,SchoolCode)) |> 
  select(Code, ACT) |> 
  rename(Participation = ACT)
NJ_SAT_2017 <- read_excel("Data/NJ_Tests/2017_NJ_School_Database.xlsx", sheet = "PSAT-SAT-ACTPerformance") |> 
  filter(Test == "SAT") |> 
  mutate(Code = str_c(CountyCode,DistrictCode,SchoolCode)) |> 
  select(Code, Subject, School_Avg) |> 
  pivot_wider(names_from = Subject, values_from = School_Avg) |> 
  rename_with(.fn = ~ str_remove_all(.x, " ")) |> 
  mutate(ReadingandWriting = as.numeric(ReadingandWriting), Math = as.numeric(Math)) |> 
  mutate(Total = ReadingandWriting + Math)
SATParticipation <- read_excel("Data/NJ_Tests/2017_NJ_School_Database.xlsx", sheet = "PSAT-SAT-ACTParticipation") |> 
  mutate(Code = str_c(CountyCode,DistrictCode,SchoolCode)) |> 
  select(Code, SAT) |> 
  rename(Participation = SAT)
Names <- read_excel("Data/NJ_Tests/2017_NJ_School_Database.xlsx", sheet = "SchoolHeader") |> 
  rename_with(.fn = ~str_remove_all(.x, " ")) |> 
  mutate(Code = str_c(CountyCode,DistrictCode,SchoolCode)) |>
  select(Code, SchoolName)
NJ_ACT_2017 <- left_join(NJ_ACT_2017, ACTParticipation, by = "Code") |> 
  left_join(Names, by="Code") |> 
  left_join(
    nj_counts_2017,
    by = "Code"
  ) |> 
  mutate(Participation = as.numeric(Participation)) |> 
  mutate(Tests = round(Grade12 * Participation / 100)) |> 
  select(!c(Grade11, NCES_ID, Grade12))
NJ_SAT_2017 <- left_join(NJ_SAT_2017, SATParticipation, by = "Code") |> 
  left_join(Names, by="Code") |> 
  left_join(
    nj_counts_2017,
    by = "Code"
  ) |> 
  mutate(Participation = as.numeric(Participation)) |> 
  mutate(Tests = round(Grade12 * Participation / 100)) |> 
  select(!c(Grade11, NCES_ID, Grade12))
write_xlsx(NJ_ACT_2017, "Data/NJ_Tests/2017_ACT_NJ.xlsx")
write_xlsx(NJ_SAT_2017, "Data/NJ_Tests/2017_SAT_NJ.xlsx")
rm(Names, nj_counts_2017, NJ_ACT_2017, NJ_SAT_2017)

years <- c(2018, 2019, 2020, 2021, 2022, 2023)
for(year in years) {
  nj_counts <- read_excel("Data/nces_and_state_ids/nces_and_state_id_nj.xlsx", skip = 14) |> 
    rename_with(.fn = ~ str_remove_all(.x, pattern = "\n")) |>
    rename_with(.fn = ~ str_remove_all(.x, pattern = "\r")) |>
    rename_with(.fn = ~ str_remove_all(.x, pattern = " ")) |> 
    select(NCESSchoolID,StateSchoolID) |> 
    rename(NCES_ID = NCESSchoolID, Code = StateSchoolID) |> 
    mutate(Code = str_c(str_sub(Code, 4, 9), str_sub(Code, 11, -1))) |> 
    left_join(
      read_csv("Data/11th_12th_grade_membership/membershipcount.csv", skip = 6) |> 
        rename_with(.fn = ~ str_remove_all(.x, pattern = " ")) |> 
        select(`SchoolID-NCESAssigned[PublicSchool]Latestavailableyear`, ends_with(str_sub(as.character(year), 3, 4))) |> 
        rename_with(.fn = ~ str_sub(.x, 1, 7)) |> 
        rename(NCES_ID = SchoolI) |> 
        mutate(NCES_ID = as.character(NCES_ID), Grade12 = as.numeric(Grade12), Grade11 = as.numeric(Grade11)),
      by = "NCES_ID"
    )
  ACT <- read_excel(paste0("Data/NJ_Tests/",year,"_NJ_School_Database.xlsx"), sheet = "PSAT-SAT-ACTPerformance") |> 
    filter(Test == "ACT") |> 
    mutate(Code = str_c(CountyCode,DistrictCode,SchoolCode)) |> 
    select(Code, Subject, School_Avg, SchoolName) |> 
    pivot_wider(names_from = Subject, values_from = School_Avg) |> 
    mutate(English = as.numeric(English), Reading = as.numeric(Reading), Math = as.numeric(Math), Science = as.numeric(Science)) |> 
    mutate(Composite = round((English+Reading+Math+Science)/4))
  ACTParticipation <- read_excel(paste0("Data/NJ_Tests/",year,"_NJ_School_Database.xlsx"), sheet = "PSAT-SAT-ACTParticipation") |> 
    mutate(Code = str_c(CountyCode,DistrictCode,SchoolCode)) |> 
    select(Code, ACT) |> 
    rename(Participation = ACT)
  ACT <- left_join(ACT, ACTParticipation, by = "Code") |> 
    left_join(nj_counts, by = "Code") |> 
    mutate(Participation = as.numeric(Participation)) |> 
    mutate(Tests = round(Grade12 * Participation / 100)) |> 
    select(!c(Grade11, NCES_ID, Grade12))
  SAT <- read_excel(paste0("Data/NJ_Tests/",year,"_NJ_School_Database.xlsx"), sheet = "PSAT-SAT-ACTPerformance") |> 
    filter(Test == "SAT") |> 
    mutate(Code = str_c(CountyCode,DistrictCode,SchoolCode)) |> 
    select(SchoolName, Code, Subject, School_Avg) |> 
    pivot_wider(names_from = Subject, values_from = School_Avg) |> 
    rename_with(.fn = ~ str_remove_all(.x, " ")) |> 
    mutate(ReadingandWriting = as.numeric(ReadingandWriting), Math = as.numeric(Math)) |> 
    mutate(Total = ReadingandWriting + Math)
  SATParticipation <- read_excel(paste0("Data/NJ_Tests/",year,"_NJ_School_Database.xlsx"), sheet = "PSAT-SAT-ACTParticipation") |> 
    mutate(Code = str_c(CountyCode,DistrictCode,SchoolCode)) |> 
    select(Code, SAT) |> 
    rename(Participation = SAT)
  SAT <- left_join(SAT, SATParticipation, by = "Code") |> 
    left_join(nj_counts, by = "Code") |> 
    mutate(Participation = as.numeric(Participation)) |> 
    mutate(Tests = round(Grade12 * Participation / 100)) |> 
    select(!c(Grade11, NCES_ID, Grade12))
  write_xlsx(SAT, paste0("Data/NJ_Tests/",year,"_SAT_NJ.xlsx"))
  write_xlsx(ACT, paste0("Data/NJ_Tests/",year,"_ACT_NJ.xlsx"))
}
rm(ACT, ACTParticipation, SAT, SATParticipation, nj_counts, year, years)

#Adjust NC files
years <- c(2017, 2018, 2019, 2020, 2021, 2022, 2023)
for(year in years) {
  if(year %in% c(2017, 2018, 2019)) {
    SAT <- read_excel(paste0("Data/NC_Tests/",year,"_SAT_NC_Base.xlsx"), skip = 1) |> 
      rename(Dist = `...1`, SchoolName = `...3`) |> 
      select(!c(`...4`, `...5`)) |> 
      mutate(`School System & School` = as.numeric(`School System & School`)) |> 
      drop_na(`School System & School`) |> 
      mutate(`School System & School` = as.character(`School System & School`)) |> 
      mutate(`School System & School` = case_when(
        str_length(`School System & School`) == 2 ~ str_c("0",`School System & School`),
        str_length(`School System & School`) == 3 ~ `School System & School`))
    write_xlsx(SAT, paste0("Data/NC_Tests/",year,"_SAT_NC.xlsx"))
  } else {
    if(year == 2020) {
      SAT <- read_excel(paste0("Data/NC_Tests/",year,"_SAT_NC_Base.xlsx"), skip = 0) |> 
        rename(Dist = `...1`, SchoolName = `...3`) |> 
        select(!c(`...4`, `...5`)) |> 
        mutate(`School System &School` = as.numeric(`School System &School`)) |> 
        drop_na(`School System &School`) |> 
        mutate(`School System &School` = as.character(`School System &School`)) |> 
        mutate(`School System &School` = case_when(
          str_length(`School System &School`) == 2 ~ str_c("0",`School System &School`),
          str_length(`School System &School`) == 3 ~ `School System &School`))
      write_xlsx(SAT, paste0("Data/NC_Tests/",year,"_SAT_NC.xlsx"))
    } else if(year == 2021) {
      SAT <- read_excel(paste0("Data/NC_Tests/",year,"_SAT_NC_Base.xlsx"), skip = 1) |> 
        rename(Dist = `...1`, SchoolName = `...3`) |> 
        mutate(`School System &School` = as.numeric(`School System &School`)) |> 
        drop_na(`School System &School`) |> 
        mutate(`School System &School` = as.character(`School System &School`)) |> 
        mutate(`School System &School` = case_when(
          str_length(`School System &School`) == 2 ~ str_c("0",`School System &School`),
          str_length(`School System &School`) == 3 ~ `School System &School`))
      write_xlsx(SAT, paste0("Data/NC_Tests/",year,"_SAT_NC.xlsx"))
    } else {
      SAT <- read_excel(paste0("Data/NC_Tests/",year,"_SAT_NC_Base.xlsx"), skip = 1) |> 
        rename(Dist = `...1`, SchoolName = `...3`) |> 
        mutate(`School System & School` = as.numeric(`School System & School`)) |> 
        drop_na(`School System & School`) |> 
        mutate(`School System & School` = as.character(`School System & School`)) |> 
        mutate(`School System & School` = case_when(
          str_length(`School System & School`) == 2 ~ str_c("0",`School System & School`),
          str_length(`School System & School`) == 3 ~ `School System & School`))
      write_xlsx(SAT, paste0("Data/NC_Tests/",year,"_SAT_NC.xlsx"))
    }
  }
}
years <- c(2018, 2019, 2021, 2022, 2023)
for(year in years) {
  if(year %in% c(2018, 2019)) {
    ACT <- read_excel(paste0("Data/NC_Tests/",year,"_ACT_NC_Base.xlsx"), skip = 15, col_names = c("System", "School", "SystemorSchoolName", "NumberTested", "CompositeMean", "EnglishMean", "MetEnglishBenchmarkPercent", "MathMean", "MetMathBenchmarkPercent", "ReadingMean", "MetReadBenchmarkPercent", "ScienceMean", "MetScienceBenchmarkPercent", "MetAllFourBenchmarks", "WritingMean", "MetWritingBenchmarkPercent", "MetAllFourPlusWritingBenchmarks")) |> 
      select(School, SystemorSchoolName, NumberTested, CompositeMean, EnglishMean, MathMean, ReadingMean, ScienceMean) |> 
      mutate(School = as.numeric(School), NumberTested = as.numeric(NumberTested)) |> 
      drop_na(School) |> 
      mutate(School = as.character(School)) |> 
      mutate(School = case_when(
        str_length(School) == 5 ~ str_c("0",School),
        str_length(School) == 6 ~ School)) 
    write_xlsx(ACT, paste0("Data/NC_Tests/",year,"_ACT_NC.xlsx"))
  } else {
    ACT <- read_excel(paste0("Data/NC_Tests/",year,"_ACT_NC_Base.xlsx"), skip = 15, col_names = c("System", "School", "SystemorSchoolName", "NumberTested", "CompositeMean", "EnglishMean", "MetEnglishBenchmarkPercent", "MathMean", "MetMathBenchmarkPercent", "ReadingMean", "MetReadBenchmarkPercent", "ScienceMean", "MetScienceBenchmarkPercent", "MetAllFourBenchmarks"))  |> 
      select(School, SystemorSchoolName, NumberTested, CompositeMean, EnglishMean, MathMean, ReadingMean, ScienceMean) |> 
      mutate(School = as.numeric(School), NumberTested = as.numeric(NumberTested)) |> 
      drop_na(School) |> 
      mutate(School = as.character(School)) |> 
      mutate(School = case_when(
        str_length(School) == 5 ~ str_c("0",School),
        str_length(School) == 6 ~ School))
    write_xlsx(ACT, paste0("Data/NC_Tests/",year,"_ACT_NC.xlsx"))
  }
}
rm(SAT, ACT, year, years)

#Adjust SC ACT files
years <- c(2018, 2019, 2020, 2021, 2022, 2023)
for(year in years) {
  sc_counts <- read_excel("Data/nces_and_state_ids/nces_and_state_id_sc.xlsx", skip = 14) |> 
    rename_with(.fn = ~ str_remove_all(.x, pattern = "\n")) |>
    rename_with(.fn = ~ str_remove_all(.x, pattern = "\r")) |>
    rename_with(.fn = ~ str_remove_all(.x, pattern = " ")) |> 
    select(NCESSchoolID,StateSchoolID) |> 
    rename(NCES_ID = NCESSchoolID, School = StateSchoolID) |> 
    mutate(School = str_c(str_sub(School, 4, 7), str_sub(School, 9, 11))) |> 
    left_join(
      read_csv("Data/11th_12th_grade_membership/membershipcount.csv", skip = 6) |> 
        rename_with(.fn = ~ str_remove_all(.x, pattern = " ")) |> 
        select(`SchoolID-NCESAssigned[PublicSchool]Latestavailableyear`, ends_with(str_sub(as.character(year), 3, 4))) |> 
        rename_with(.fn = ~ str_sub(.x, 1, 7)) |> 
        rename(NCES_ID = SchoolI) |> 
        mutate(NCES_ID = as.character(NCES_ID), Grade12 = as.numeric(Grade12), Grade11 = as.numeric(Grade11)),
      by = "NCES_ID"
    )
  #Create participation rates
  ACT <- read_excel(paste0("Data/SC_Tests/",year,"_ACT_SC_Base.xlsx"), skip = 5, col_names = c("School", "DistName", "SchoolName", "x", "Tests", "English", "Math", "Reading", "Science", "xx", "Composite")) |> 
    select(School, SchoolName, Tests, English, Math, Reading, Science, Composite) |> 
    mutate(Tests = as.numeric(Tests), English = as.numeric(English), Math = as.numeric(Math), Science = as.numeric(Science), Reading = as.numeric(Reading), Composite = as.numeric(Composite)) |> 
    mutate(across(where(is.numeric), \(x) round(x * 10)/10)) |> 
    left_join(sc_counts, by="School") |> 
    mutate(Participation = Tests/Grade12*100) |> 
    mutate(Participation = case_when(
      Participation >= 100 ~ 100,
      Participation < 100 ~ Participation
    )) |> 
    select(!c(NCES_ID, Grade11, Grade12))
  SAT <- read_excel(paste0("Data/SC_Tests/",year,"_SAT_SC_Base.xlsx"), skip = 2) |> 
    rename_with(.fn = ~ str_remove_all(.x, pattern = "\n")) |>
    rename_with(.fn = ~ str_remove_all(.x, pattern = "\r")) |>
    rename_with(.fn = ~ str_remove_all(.x, pattern = " ")) |>
    drop_na() |> 
    mutate(PercentTested = as.numeric(str_remove_all(PercentTested, "%"))) |> 
    mutate(PercentTested = round(PercentTested * 1000)/10)
  write_xlsx(SAT, paste0("Data/SC_Tests/",year,"_SAT_SC.xlsx"))
  write_xlsx(ACT, paste0("Data/SC_Tests/",year,"_ACT_SC.xlsx"))
}
rm(SAT, ACT, sc_counts)

#Add variables to MA data
years <- c(2017, 2018, 2019, 2020, 2021, 2022, 2023)
for(year in years) {
  ma_counts <- read_excel("Data/nces_and_state_ids/nces_and_state_id_ma.xlsx", skip = 14) |> 
    rename_with(.fn = ~ str_remove_all(.x, pattern = "\n")) |>
    rename_with(.fn = ~ str_remove_all(.x, pattern = "\r")) |>
    rename_with(.fn = ~ str_remove_all(.x, pattern = " ")) |> 
    select(NCESSchoolID,StateSchoolID) |> 
    rename(NCES_ID = NCESSchoolID, SchoolCode = StateSchoolID) |> 
    mutate(SchoolCode = str_sub(SchoolCode, 9, -1)) |> 
    left_join(
      read_csv("Data/11th_12th_grade_membership/membershipcount.csv", skip = 6) |> 
        rename_with(.fn = ~ str_remove_all(.x, pattern = " ")) |> 
        select(`SchoolID-NCESAssigned[PublicSchool]Latestavailableyear`, ends_with(str_sub(as.character(year), 3, 4))) |> 
        rename_with(.fn = ~ str_sub(.x, 1, 7)) |> 
        rename(NCES_ID = SchoolI) |> 
        mutate(NCES_ID = as.character(NCES_ID), Grade12 = as.numeric(Grade12), Grade11 = as.numeric(Grade11)),
      by = "NCES_ID"
    ) |> 
    mutate(TotalCount = Grade11+Grade12)
  SAT <- read_excel(paste0("Data/MA_Tests/",year,"_SAT_MA_Base.xlsx"), skip = 1) |> 
    rename_with(.fn = ~ str_remove_all(.x, pattern = "\n")) |>
    rename_with(.fn = ~ str_remove_all(.x, pattern = "\r")) |>
    rename_with(.fn = ~ str_remove_all(.x, pattern = " ")) |> 
    left_join(ma_counts, by="SchoolCode") |> 
    mutate(TestsTaken = as.numeric(TestsTaken), `Reading/Writing` = as.numeric(`Reading/Writing`), Math = as.numeric(Math)) |> 
    mutate(Total = Math + `Reading/Writing`, Participation = TestsTaken/TotalCount) |> 
    mutate(Participation = round(Participation * 1000)/10) |> 
    mutate(Participation = case_when(
      Participation >= 100 ~ 100,
      Participation < 100 ~ Participation
    )) |> 
    select(SchoolName, SchoolCode, Participation, TestsTaken, `Reading/Writing`, Math, Total)
  write_xlsx(SAT, paste0("Data/MA_Tests/",year,"_SAT_MA.xlsx"))
}
rm(ma_counts, SAT)

#Diffuse IA data file by year
years <- c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)
for(year in years) {
  ACT <- read_excel("Data/IA_Tests/ACT_IA_Base.xlsx") |> 
    distinct(District, School, Grad_Year, .keep_all = TRUE) |> 
    filter(Grad_Year == {{year}})
  write_xlsx(ACT, paste0("Data/IA_Tests/",year,"_ACT_IA.xlsx"))
}
rm(ACT)

#Adjust GA files separately from the rest since they have both highest and recent score avgs
source("functions/clean_scores.R")
years <- c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)
for(year in years) {
  Counts <- read_csv(paste0("Data/GA_Tests/Enroll_Count_",year,".csv")) |> 
    rename_with(.fn = ~ toupper(.x)) |> 
    mutate(School_ID = str_c(SCHOOL_DSTRCT_CD, INSTN_NUMBER)) |> 
    filter(ENROLLMENT_PERIOD == "Spring", GRADE_LEVEL == "12th" | GRADE_LEVEL == "11th") |> 
    select(School_ID, GRADE_LEVEL, ENROLLMENT_COUNT) |> 
    pivot_wider(names_from = GRADE_LEVEL, values_from = ENROLLMENT_COUNT) |> 
    mutate(across(c("11th", "12th"), as.numeric)) |>
    mutate(TotalCount = `11th` + `12th`) |> 
    select(School_ID, TotalCount)
  GA_Tests <- read_csv(paste0("Data/GA_Tests/",year,"_ACT_GA.csv")) |> 
    filter(SUBGRP_DESC == "All Students") |> 
    mutate(Year = str_c("20",str_sub(LONG_SCHOOL_YEAR, 6, 7)), SCHOOL_DISTRCT_CD = str_sub(SCHOOL_DISTRCT_CD, 1, 4), INSTN_NUM_TESTED_CNT = as.numeric(INSTN_NUM_TESTED_CNT)) |>
    mutate(INSTN_NUMBER = ifelse(str_length(INSTN_NUMBER) == 3, str_c("0", INSTN_NUMBER), INSTN_NUMBER)) |> 
    mutate(School_ID = str_c(SCHOOL_DISTRCT_CD, INSTN_NUMBER)) |> 
    rename(School_Name = INSTN_NAME, Num_Tests = INSTN_NUM_TESTED_CNT) |>
    select(Year, School_ID, School_Name, Num_Tests, INSTN_AVG_SCORE_VAL, TEST_CMPNT_TYP_CD) |> 
    pivot_wider(names_from = TEST_CMPNT_TYP_CD, values_from = c(INSTN_AVG_SCORE_VAL, Num_Tests)) |> 
    rename(Num_Tests = Num_Tests_Composite, Composite = INSTN_AVG_SCORE_VAL_Composite, English = INSTN_AVG_SCORE_VAL_English, Reading = INSTN_AVG_SCORE_VAL_Reading, Mathematics = INSTN_AVG_SCORE_VAL_Mathematics, Science = INSTN_AVG_SCORE_VAL_Science) |> 
    select(Year, School_ID, School_Name, Num_Tests, English, Mathematics, Reading, Science, Composite) |> 
    drop_na() |> 
    mutate(across(all_of(c("English", "Mathematics", "Reading", "Science", "Composite")), round)) |> 
    rename(Writing = English, Total = Composite, Math = Mathematics) |> 
    mutate(RandW = Reading + Writing)
  SATMath <- ACTtoSATMath(tibble(Math = GA_Tests$Math)) |> 
    rename(SAT_Math = SAT)
  SATTotal <- ACTtoSATTotal(tibble(Total = GA_Tests$Total))$Total |> 
    rename(SAT_Total = SAT)
  SATERW <- ACTtoSATERW(tibble(RandW = GA_Tests$RandW))$ERW |> 
    rename(SAT_ERW = SAT)
  GA_Tests <- select(GA_Tests, !c(RandW, Reading, Writing, Math, Science, Total))
  GA_Tests <- cbind(GA_Tests, SATMath, SATTotal, SATERW) |> 
    left_join(Counts, by="School_ID") |> 
    drop_na() |> 
    mutate(Part_Rate = Num_Tests/TotalCount) |> 
    mutate(Part_Rate = round(Part_Rate * 1000)/10) |> 
    mutate(Part_Rate = case_when(
      Part_Rate >= 100 ~ 100,
      Part_Rate < 100 ~ Part_Rate
    )) |> 
    select(!TotalCount) |> 
    mutate(Target_Grp = "All", State = "GA", FIPS = 13, Test = "ACT", TestType = "Recent") |> 
    relocate(School_Name, School_ID, Num_Tests, Part_Rate, SAT_ERW, SAT_Math, SAT_Total, Target_Grp, Year, State, FIPS, Test, TestType)
  if(year %in% c(2016:2019)) {
    SAT_High <- read_csv(paste0("Data/GA_Tests/",year,"_SAT_HIGHEST_GA.csv")) |> 
      filter(SUBGRP_DESC == "All Students") |> 
      mutate(Year = str_c("20",str_sub(LONG_SCHOOL_YEAR, 6, 7)), SCHOOL_DISTRCT_CD = str_sub(SCHOOL_DISTRCT_CD, 1, 4), INSTN_NUM_TESTED_CNT = as.numeric(INSTN_NUM_TESTED_CNT)) |>
      mutate(School_ID = str_c(SCHOOL_DISTRCT_CD, INSTN_NUMBER)) |> 
      rename(School_Name = INSTN_NAME, Num_Tests = INSTN_NUM_TESTED_CNT) |>
      select(Year, School_ID, School_Name, Num_Tests, INSTN_AVG_SCORE_VAL, TEST_CMPNT_TYP_CD) |> 
      drop_na() |>
      pivot_wider(names_from = TEST_CMPNT_TYP_CD, values_from = INSTN_AVG_SCORE_VAL) |> 
      rename_with(.fn = ~ str_remove_all(.x, " ")) |> 
      rename(SAT_ERW = `EvidenceBasedReadingandWriting-New`, SAT_Math = `MathSectionScore-New`, SAT_Total = `CombinedTestScore`) |> 
      mutate(across(c(SAT_ERW, SAT_Total, SAT_Math), as.numeric)) |> 
      select(Year, School_ID, School_Name, Num_Tests, SAT_Math, SAT_ERW, SAT_Total) |> 
      mutate(Num_Tests = round(Num_Tests)) |> 
      left_join(Counts, by="School_ID") |> 
      drop_na() |> 
      mutate(Part_Rate = Num_Tests/TotalCount) |> 
      mutate(Part_Rate = round(Part_Rate * 1000)/10) |> 
      mutate(Part_Rate = case_when(
        Part_Rate >= 100 ~ 100,
        Part_Rate < 100 ~ Part_Rate
      )) |> 
      select(!TotalCount) |> 
      mutate(Target_Grp = "All", State = "GA", FIPS = 13, Test = "SAT", TestType = "Highest") |> 
      relocate(School_Name, School_ID, Num_Tests, Part_Rate, SAT_ERW, SAT_Math, SAT_Total, Target_Grp, Year, State, FIPS, Test, TestType)
      
    SAT_Recent <- read_csv(paste0("Data/GA_Tests/",year,"_SAT_RECENT_GA.csv")) |> 
      filter(SUBGRP_DESC == "All Students") |> 
      mutate(Year = str_c("20",str_sub(LONG_SCHOOL_YEAR, 6, 7)), SCHOOL_DISTRCT_CD = str_sub(SCHOOL_DISTRCT_CD, 1, 4), INSTN_NUM_TESTED_CNT = as.numeric(INSTN_NUM_TESTED_CNT)) |>
      mutate(School_ID = str_c(SCHOOL_DISTRCT_CD, INSTN_NUMBER)) |> 
      rename(School_Name = INSTN_NAME, Num_Tests = INSTN_NUM_TESTED_CNT) |>
      select(Year, School_ID, School_Name, Num_Tests, INSTN_AVG_SCORE_VAL, TEST_CMPNT_TYP_CD) |> 
      drop_na() |>
      pivot_wider(names_from = TEST_CMPNT_TYP_CD, values_from = INSTN_AVG_SCORE_VAL) |> 
      rename_with(.fn = ~ str_remove_all(.x, " ")) |> 
      rename(SAT_ERW = `EvidenceBasedReadingandWriting-New`, SAT_Math = `MathSectionScore-New`, SAT_Total = `CombinedTestScore`) |> 
      mutate(across(c(SAT_ERW, SAT_Total, SAT_Math), as.numeric)) |> 
      select(Year, School_ID, School_Name, Num_Tests, SAT_Math, SAT_ERW, SAT_Total) |> 
      mutate(Num_Tests = round(Num_Tests)) |> 
      left_join(Counts, by="School_ID") |> 
      drop_na() |> 
      mutate(Part_Rate = Num_Tests/TotalCount) |> 
      mutate(Part_Rate = round(Part_Rate * 1000)/10) |> 
      mutate(Part_Rate = case_when(
        Part_Rate >= 100 ~ 100,
        Part_Rate < 100 ~ Part_Rate
      )) |> 
      select(!TotalCount) |> 
      mutate(Target_Grp = "All", State = "GA", FIPS = 13, Test = "SAT", TestType = "Recent") |> 
      relocate(School_Name, School_ID, Num_Tests, Part_Rate, SAT_ERW, SAT_Math, SAT_Total, Target_Grp, Year, State, FIPS, Test, TestType)
    GA_Tests <- rbind(GA_Tests, SAT_High, SAT_Recent)
  } else if(year %in% c(2020, 2021, 2022)) {
    SAT_High <- read_csv(paste0("Data/GA_Tests/",year,"_SAT_HIGHEST_GA.csv")) |> 
      filter(SUBGRP_DESC == "All Students") |> 
      mutate(Year = str_c("20",str_sub(LONG_SCHOOL_YEAR, 6, 7)), SCHOOL_DISTRCT_CD = str_sub(SCHOOL_DISTRCT_CD, 1, 4), INSTN_NUM_TESTED_CNT = as.numeric(INSTN_NUM_TESTED_CNT)) |>
      mutate(School_ID = str_c(SCHOOL_DISTRCT_CD, INSTN_NUMBER)) |> 
      rename(School_Name = INSTN_NAME, Num_Tests = INSTN_NUM_TESTED_CNT) |>
      select(Year, School_ID, School_Name, Num_Tests, INSTN_AVG_SCORE_VAL, TEST_CMPNT_TYP_CD) |> 
      drop_na() |>
      pivot_wider(names_from = TEST_CMPNT_TYP_CD, values_from = INSTN_AVG_SCORE_VAL) |> 
      rename_with(.fn = ~ str_remove_all(.x, " ")) |> 
      rename(Reading = `ReadingTestScore-New`, Writing = `WritLangTestScore-New`, SAT_Math = `MathSectionScore-New`, SAT_Total = `CombinedTestScore`) |> 
      mutate(across(c(Reading, Writing, SAT_Total, SAT_Math), as.numeric)) |> 
      mutate(SAT_ERW = Reading + Writing) |> 
      select(Year, School_ID, School_Name, Num_Tests, SAT_Math, SAT_ERW, SAT_Total) |> 
      mutate(Num_Tests = round(Num_Tests)) |> 
      left_join(Counts, by="School_ID") |> 
      drop_na() |> 
      mutate(Part_Rate = Num_Tests/TotalCount) |> 
      mutate(Part_Rate = round(Part_Rate * 1000)/10) |> 
      mutate(Part_Rate = case_when(
        Part_Rate >= 100 ~ 100,
        Part_Rate < 100 ~ Part_Rate
      )) |> 
      select(!TotalCount) |> 
      mutate(Target_Grp = "All", State = "GA", FIPS = 13, Test = "SAT", TestType = "Highest") |> 
      relocate(School_Name, School_ID, Num_Tests, Part_Rate, SAT_ERW, SAT_Math, SAT_Total, Target_Grp, Year, State, FIPS, Test, TestType)
    
    SAT_Recent <- read_csv(paste0("Data/GA_Tests/",year,"_SAT_RECENT_GA.csv")) |> 
      filter(SUBGRP_DESC == "All Students") |> 
      mutate(Year = str_c("20",str_sub(LONG_SCHOOL_YEAR, 6, 7)), SCHOOL_DISTRCT_CD = str_sub(SCHOOL_DISTRCT_CD, 1, 4), INSTN_NUM_TESTED_CNT = as.numeric(INSTN_NUM_TESTED_CNT)) |>
      mutate(School_ID = str_c(SCHOOL_DISTRCT_CD, INSTN_NUMBER)) |> 
      rename(School_Name = INSTN_NAME, Num_Tests = INSTN_NUM_TESTED_CNT) |>
      select(Year, School_ID, School_Name, Num_Tests, INSTN_AVG_SCORE_VAL, TEST_CMPNT_TYP_CD) |> 
      drop_na() |>
      pivot_wider(names_from = TEST_CMPNT_TYP_CD, values_from = INSTN_AVG_SCORE_VAL) |> 
      rename_with(.fn = ~ str_remove_all(.x, " ")) |> 
      rename(Reading = `ReadingTestScore-New`, Writing = `WritLangTestScore-New`, SAT_Math = `MathSectionScore-New`, SAT_Total = `CombinedTestScore`) |> 
      mutate(across(c(Reading, Writing, SAT_Total, SAT_Math), as.numeric)) |> 
      mutate(SAT_ERW = Reading + Writing) |> 
      select(Year, School_ID, School_Name, Num_Tests, SAT_Math, SAT_ERW, SAT_Total) |> 
      mutate(Num_Tests = round(Num_Tests)) |> 
      left_join(Counts, by="School_ID") |> 
      drop_na() |> 
      mutate(Part_Rate = Num_Tests/TotalCount) |> 
      mutate(Part_Rate = round(Part_Rate * 1000)/10) |> 
      mutate(Part_Rate = case_when(
        Part_Rate >= 100 ~ 100,
        Part_Rate < 100 ~ Part_Rate
      )) |> 
      select(!TotalCount) |> 
      mutate(Target_Grp = "All", State = "GA", FIPS = 13, Test = "SAT", TestType = "Recent") |> 
      relocate(School_Name, School_ID, Num_Tests, Part_Rate, SAT_ERW, SAT_Math, SAT_Total, Target_Grp, Year, State, FIPS, Test, TestType)
    GA_Tests <- rbind(GA_Tests, SAT_High, SAT_Recent)
  }
  save(GA_Tests, file = paste0("Data/GA_Tests/",year,"_TESTS_GA.Rdata"))
}
rm(Counts, GA_Tests, SAT_High, SAT_Recent, SATERW, SATMath, SATTotal, year, years, ACTtoSATERW, ACTtoSATMath, ACTtoSATTotal, cleanScores, SATonly, ACTonly)

