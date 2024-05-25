library(tidyverse)
library(readxl)

stateTest <- read_csv('seda_school_pool_gcs_5.0.csv')
njSAT <- read_excel('NJ_SATACT_scores_2022.xlsx')
txSAT <- read_excel('sat-campus-data-class-2022.xlsx', sheet = 4) |> 
  filter(Group == "All Students") |> 
  

#NJ = 34, TX = 48
stateTest <- stateTest |>
    filter(str_sub(as.character(sedasch), 1, 2) == '48') |>
    mutate(sedasch = as.character(sedasch))

njSAT <- njSAT |>
    mutate(sedasch = str_c('34', CountyCode, DistrictCode, SchoolCode), sedaschname = SchoolName,
           School_Avg = as.double(School_Avg))

joined <- inner_join(txSAT, stateTest, by = "sedaschname") |>
    filter(Test == "SAT", Subject == "Math") |> 
    mutate(avgscore = gcs_mn_avg_eb - gradecenter)

