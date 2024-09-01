library(tidyverse)
library(readxl)

#Data files source: https://rc.doe.state.nj.us/download
#ACT composite and SAT total conversion table, source: https://www.act.org/content/dam/act/unsecured/documents/ACT-SAT-Concordance-Tables.pdf
ACTSATConcordance <- tribble(
  ~ACT_Total, ~SAT_Low, ~SAT_High, ~SAT_Point,
  36, 1570, 1600, 1590,
  35, 1530, 1560, 1540,
  34, 1490, 1520, 1500,
  33, 1450, 1480, 1460,
  32, 1420, 1440, 1430,
  31, 1390, 1410, 1400,
  30, 1360, 1380, 1370,
  29, 1330, 1350, 1340,
  28, 1300, 1320, 1310,
  27, 1260, 1290, 1280,
  26, 1230, 1250, 1240,
  25, 1200, 1220, 1210,
  24, 1160, 1190, 1180,
  23, 1130, 1150, 1140,
  22, 1100, 1120, 1110,
  21, 1060, 1090, 1080,
  20, 1030, 1050, 1040,
  19, 990, 1020, 1010,
  18, 960, 980, 970,
  17, 920, 950, 930,
  16, 880, 910, 890,
  15, 830, 870, 850,
  14, 780, 820, 800,
  13, 730, 770, 760,
  12, 690, 720, 710,
  11, 650, 680, 670,
  10, 620, 640, 630,
  9, 590, 610, 590
)

years <- c(2017, 2018, 2019, 2020, 2021, 2022, 2023)

fullSchools <- tribble(
  ~School_ID, ~SAT_Math, ~SAT_ERW, ~SAT_Total, ~ACT_English, ~ACT_Math, ~ACT_Reading, ~ACT_Science, ~ACT_Total, ~SAT_Conversion_Error, ~SAT_Participation, ~ACT_Participation, ~Num_Teachers, ~Num_Students, ~Num_Grade12, ~Student_Teacher_Ratio, ~Year, ~Participation_Differential
)

for(y in seq_along(years)) {
  path <- paste0("Data/NJ_Tests/",years[y],"_NJ_School_Database.xlsx")
  njTests <- read_excel(path, sheet = "PSAT-SAT-ACTPerformance") |>
    filter(Test == "SAT" | Test == "ACT") |>
    mutate(School_Avg = as.double(School_Avg)) |> 
    select(CountyCode, DistrictCode, SchoolCode, Test, Subject, School_Avg) |> 
    pivot_wider(values_from = School_Avg, names_from = c(Test, Subject)) |>
    rename(SAT_ERW = `SAT_Reading and Writing`) |> 
    mutate(School_ID = str_c(CountyCode, DistrictCode, SchoolCode), SAT_Total = SAT_Math + SAT_ERW, ACT_Total = round((ACT_Math + ACT_English + ACT_Science + ACT_Reading)/4)) |> 
    select(School_ID, starts_with("SAT"), starts_with("ACT")) |> 
    mutate(SAT_Conversion_Error = SAT_Total - ACTSATConcordance[37-ACT_Total,]$SAT_Point)

  #Participation rates are calculated as percentages of 12th grade students that have taken the test at any time during high school.
  njTestParticipation <- read_excel(path, sheet = "PSAT-SAT-ACTParticipation") |> 
    mutate(School_ID = str_c(CountyCode, DistrictCode, SchoolCode), SAT = as.numeric(SAT), ACT = as.numeric(ACT)) |> 
    rename(SAT_Participation = SAT, ACT_Participation = ACT) |> 
    select(School_ID, SAT_Participation, ACT_Participation)
  njTests <- left_join(njTests, njTestParticipation, by = "School_ID")
  
  #2017 has an untidy EnrollmentTrendsbyGrade sheet and needs to be treated differently
  if(years[y] != 2017) {
    njSchools <- left_join(
      read_excel(path, sheet = "TeachersExperience") |> 
        mutate(School_ID = str_c(CountyCode, DistrictCode, SchoolCode), TeacherCount_School = as.numeric(TeacherCount_School)) |>
        rename(Num_Teachers = TeacherCount_School) |> 
        select(School_ID, Num_Teachers),
      read_excel(path, sheet = "EnrollmentTrendsbyGrade") |> 
        mutate(School_ID = str_c(CountyCode, DistrictCode, SchoolCode), Grade12 = as.numeric(Grade12), Total = as.numeric(Total)) |>
        rename(Num_Students = Total, Num_Grade12 = Grade12) |> 
        select(School_ID, Num_Students, Num_Grade12),
      by = "School_ID"
    ) |> 
      mutate(Student_Teacher_Ratio = Num_Students/Num_Teachers)
    njSchools <- left_join(njTests, njSchools, by = "School_ID") |> 
      mutate(Year = years[y], Participation_Differential = SAT_Participation - ACT_Participation)
  } else {
    njSchools <- left_join(
      read_excel(path, sheet = "TeachersExperience") |> 
        mutate(School_ID = str_c(CountyCode, DistrictCode, SchoolCode), TeacherCount_School = as.numeric(TeacherCount_School)) |>
        rename(Num_Teachers = TeacherCount_School) |> 
        select(School_ID, Num_Teachers),
      read_excel(path, sheet = "EnrollmentTrendsbyGrade") |> 
        pivot_wider(id_cols = c("CountyCode", "DistrictCode", "SchoolCode"), names_from = "Grade", values_from = "Count") |> 
        mutate(School_ID = str_c(CountyCode, DistrictCode, SchoolCode), Num_Grade12 = as.numeric(`Grade 12`), Num_Students = as.numeric(`Total Enrollment`)) |>
        select(School_ID, Num_Students, Num_Grade12),
      by = "School_ID"
    ) |> 
      mutate(Student_Teacher_Ratio = Num_Students/Num_Teachers)
    njSchools <- left_join(njTests, njSchools, by = "School_ID") |> 
      mutate(Year = years[y], Participation_Differential = SAT_Participation - ACT_Participation)
  }
  fullSchools <- rbind(fullSchools, njSchools)
  tmp <- njSchools |> 
    drop_na()
  save(tmp, file = paste0("Data/NJ_ACTSAT_Comparison_Train_Test/modeldata_",years[y],".Rdata"))
}

fullSchools <- fullSchools |> 
  mutate(Year = as.factor(Year))
save(fullSchools, ACTSATConcordance, file = "Data/NJ_ACTSAT_Comparison_Train_Test/full_frame.Rdata")
