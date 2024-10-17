getCRDC <- function() {
  years <- c(2015, 2020)
  chrncabsnt <- get_education_data(level = "schools",
                           source = "crdc",
                           topic = "directory",
                           filters = list(year = 2013, g11 = 1, g12 = 1)) |> 
    select(year, ncessch) |> 
    distinct() |> 
    left_join(get_education_data(level = "schools",
                                 source = "crdc",
                                 topic = "chronic-absenteeism",
                                 filters = list(year = 2013, race = 99, sex = 99),
                                 subtopic = c("race", "sex")) |> 
                select(ncessch, year, students_chronically_absent),
              by=c("year", "ncessch")) |>
    mutate(students_chronically_absent = ifelse(students_chronically_absent<0, NA, students_chronically_absent)) |> 
    drop_na() |> 
    left_join(get_education_data(level = "schools",
                                 source = "ccd",
                                 topic = "enrollment",
                                 filters = list(year = 2013, grade = 99, race = 99, sex = 99)) |> 
                select(year, ncessch, enrollment),
              by = c("year", "ncessch")) |> 
    mutate(enrollment = ifelse(enrollment<0, NA, enrollment)) |> 
    mutate(absntee_pct = students_chronically_absent/enrollment) |> 
    drop_na()
    
  for(year in years) {
    new <- get_education_data(level = "schools",
                              source = "crdc",
                              topic = "directory",
                              filters = list(year = year, g11 = 1, g12 = 1)) |> 
      select(year, ncessch) |> 
      distinct() |> 
      left_join(get_education_data(level = "schools",
                                   source = "crdc",
                                   topic = "chronic-absenteeism",
                                   filters = list(year = year, race = 99, sex = 99),
                                   subtopic = c("race", "sex")) |> 
                  select(ncessch, year, students_chronically_absent),
                by=c("year", "ncessch")) |>
      mutate(students_chronically_absent = ifelse(students_chronically_absent<0, NA, students_chronically_absent)) |> 
      drop_na() |> 
      left_join(get_education_data(level = "schools",
                                   source = "ccd",
                                   topic = "enrollment",
                                   filters = list(year = year, grade = 99, race = 99, sex = 99)) |> 
                  select(year, ncessch, enrollment),
                by = c("year", "ncessch")) |> 
      mutate(enrollment = ifelse(enrollment<0, NA, enrollment)) |> 
      mutate(absntee_pct = students_chronically_absent/enrollment) |> 
      drop_na()
    chrncabsnt <- rbind(chrncabsnt, new)
  }
  chrncabsnt <- chrncabsnt |> 
    mutate(year = year + 1) |> 
    mutate(year = as.character(year)) |> 
    rename(Year = year, NCES_ID = ncessch) |> 
    distinct()
}
