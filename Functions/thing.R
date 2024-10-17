data <- get_education_data(level = "school-districts",
                           source = "ccd",
                           topic = "finance",
                           filters = list(year = 2010)) |> 
  select(!c(censusid, fips, exp_current_resa, exp_current_state_local_funds, exp_current_federal_funds, exp_utilities_energy, exp_tech_supplies_services, exp_tech_equipment, rev_local_parent_govt, rev_fed_state_math_sci_teach, rev_fed_state_drug_free)) |>
  select(!contains("_arra")) |> 
  mutate(across(is.numeric, \(x) ifelse(x %in% c(-1, -2, -3), NA, x))) |> 
  drop_na()
years <- c(2011:2020)
for(year in years) {
  data <- rbind(data, get_education_data(level = "school-districts",
                                         source = "ccd",
                                         topic = "finance",
                                         filters = list(year = year)) |> 
                  select(!c(censusid, fips, exp_current_resa, exp_current_state_local_funds, exp_current_federal_funds, exp_utilities_energy, exp_tech_supplies_services, exp_tech_equipment, rev_local_parent_govt, rev_fed_state_math_sci_teach, rev_fed_state_drug_free)) |>
                  select(!contains("_arra")) |> 
                  mutate(across(is.numeric, \(x) ifelse(x %in% c(-1, -2, -3), NA, x))) |> 
                  drop_na())
}
data <- rename(data, Year = year) |> 
  mutate(Year = Year + 1) |> 
  mutate(Year = as.character(Year))
