library(tidyverse)
library(readxl)

stateTest <- read_csv('Data/seda_school_pool_gcs_5.0.csv') |> 
  mutate(state = str_sub(as.character(sedasch), 1, 2), avgscore = gcs_mn_avg_eb - gradecenter)
stateTest <- read_csv('Data/seda_school_pool_cs_5.0.csv') |> 
  mutate(state = str_sub(as.character(sedasch), 1, 2), avgscore = cs_mn_avg_eb - gradecenter)
njSAT <- read_excel('Data/NJ_SATACT_scores_2022.xlsx')|>
  filter(Test == "SAT") |>
  mutate(School_Avg = as.double(School_Avg)) |> 
  pivot_wider(id_cols = c(CountyCode, CountyName, DistrictCode, DistrictName, SchoolCode, SchoolName), values_from = School_Avg, names_from = Subject) |>
  mutate(sedasch = str_c('34', CountyCode, DistrictCode, SchoolCode), sedaschname = SchoolName, Total = Math + `Reading and Writing`, ERW = `Reading and Writing`, state = '34') |> 
  select(state, sedaschname, ERW, Math, Total)
txSAT <- read_excel('Data/tx-sat-campus-data-class-2022.xlsx', sheet = 4) |> 
  filter(Group == "All Students") |> 
  mutate(sedaschname = CampName, Total = as.double(Total), state = '48') |> 
  select(state, sedaschname, ERW, Math, Total)

#NJ = 34, TX = 48
stateTest <- stateTest |>
    filter(state == '48' | state == '34') |>
    mutate(sedasch = as.character(sedasch))
bothSAT <- rbind(njSAT, txSAT)

joined <- inner_join(bothSAT, stateTest, by = c('sedaschname', 'state'))

ggplot(joined, mapping = aes(x = Total, y = avgscore, color = stateabb)) +
  geom_point()
