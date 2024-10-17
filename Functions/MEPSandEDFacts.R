mepsAndEDFacts <- function() {
  data <- get_education_data(level = "schools",
                             source = "meps",
                             filters = list(year = 2015)) |> 
    drop_na() |> 
    select(year, ncessch, meps_poverty_pct, meps_poverty_ptl) |> 
    rename(Year = year, NCES_ID = ncessch)
  data2 <- get_education_data(level = "schools",
                              source = "edfacts",
                              topic = "assessments",
                              filters = list(year = 2015, grade_edfacts = 9, race = 99, sex = 99, lep = 99, homeless = 99, migrant = 99, disability = 99, econ_disadvantaged = 99, foster_care = 99, military_connected = 99)) |> 
    drop_na() |> 
    select(ncessch, year, read_test_pct_prof_midpt, math_test_pct_prof_midpt) |> 
    rename(Year = year, NCES_ID = ncessch)
  years <- c(2016, 2017, 2018, 2020)
  for(year in years) {
    data <- rbind(data, get_education_data(level = "schools",
                                           source = "meps",
                                           filters = list(year = year)) |> 
                    drop_na() |> 
                    select(year, ncessch, meps_poverty_pct, meps_poverty_ptl) |> 
                    rename(Year = year, NCES_ID = ncessch))
    data2 <- rbind(data2, get_education_data(level = "schools",
                                                      source = "edfacts",
                                                      topic = "assessments",
                                                      filters = list(year = year, grade_edfacts = 9, race = 99, sex = 99, lep = 99, homeless = 99, migrant = 99, disability = 99, econ_disadvantaged = 99, foster_care = 99, military_connected = 99)) |> 
                     drop_na() |> 
                     select(ncessch, year, read_test_pct_prof_midpt, math_test_pct_prof_midpt) |> 
                     rename(Year = year, NCES_ID = ncessch))
  }
  data <- inner_join(data, data2, by=c("NCES_ID", "Year"))
  data <- mutate(data, Year = Year + 1)
  return(data)
}