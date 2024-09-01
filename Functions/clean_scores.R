cleanScores <- function(year) {
  #Leaving out UT because no participation
  states <- c("TX", "TN", "SC", "NJ", "NC", "MA", "IA", "GA", "CO")
  fips_codes <- c(48, 47, 45, 34, 37, 25, 19, 13, 8)
  #Create data frame to hold everything
  df <- tibble(School_Name = c(), School_ID = c(), Num_Tests = c(), Part_Rate = c(), SAT_ERW = c(), SAT_Math = c(), SAT_Total = c(), Target_Grp = c(), Year = c(), State = c(), FIPS = c(), Test = c(), TestType = c())
  for(i in 1:length(states)) {
    #Georgia was process differently because it contains both highest and recent scores for schools; treat it differently here
    if(states[i] == "GA" & {{year}} != 2023) {
      load(paste0("Data/GA_Tests/",year,"_TESTS_GA.Rdata"))
      df <- rbind(df, GA_Tests)
    } else if(states[i] != "GA") {
      #Read meta file for cleaning instructions
      Meta <- read_excel(paste0("Data/", states[i], "_Tests/", states[i], "_Meta.xlsx"), na = "NA", sheet = "Sheet1") |> 
        filter(Year == {{year}})
      
      #Check for ACT, SAT, or both for the specified year; run specified algorithm
      if(Meta$SAT == "Y" && Meta$ACT == "Y") {
        tmp_1 <- SATonly(states[i], {{year}}, fips_codes[i])
        tmp_2 <- ACTonly(states[i], {{year}}, fips_codes[i])
        tmp <- rbind(tmp_1, tmp_2)
        rm(tmp_1, tmp_2)
        df <- rbind(df, tmp)
      } else if(Meta$SAT == "Y") {
        tmp <- SATonly(states[i], {{year}}, fips_codes[i])
        df <- rbind(df, tmp)
      } else if(Meta$ACT == "Y") {
        tmp <- ACTonly(states[i], {{year}}, fips_codes[i])
        df <- rbind(df, tmp)
      }
    }
  }
  
  #Save data
  save(df, file = paste0("Data/Cleaned_Scores/cleaned_scores_",year,".Rdata"))
  message("Saved for ", year)
  
  #Return data to make an overall dataset
  return(df)
}

ACTonly <- function(state, year, fips) {
  Meta <- read_excel(paste0("Data/", {{state}}, "_Tests/", {{state}}, "_Meta.xlsx"), na = c("","NA"), sheet = "ACT") |> 
    filter(Year == {{year}})
  Scores <- read_excel(paste0("Data/", {{state}}, "_Tests/", {{year}},"_ACT_", {{state}}, ".xlsx"), skip = as.numeric(Meta$Skip), sheet = Meta$Sheet)  |> 
    rename_with(.fn = ~ str_remove_all(.x, pattern = "\n")) |>
    rename_with(.fn = ~ str_remove_all(.x, pattern = "\r")) |>
    rename_with(.fn = ~ str_remove_all(.x, pattern = " "))
  if(!is.na(Meta$Filter_Var)) {
    filters <- str_split_1(Meta$Filter_Var, ",") |> 
      str_remove_all(pattern = "'")
    filter_exps <- str_split_1(Meta$Filter_Exp, ",") |> 
      str_remove_all(pattern = "'")
    filter_vals <- str_split_1(Meta$Filter_Val, ",") |> 
      str_remove_all(pattern = "'")
    for(j in 1:length(filters)) {
      if(filter_exps[j] == "==") {
        Scores <- Scores |> 
          filter(.data[[filters[j]]] == filter_vals[j])
      } else if(filter_exps[j] == "%in%") {
        Scores <- Scores |> 
          filter(.data[[filters[j]]] %in% str_split_1(filter_vals[j], ";"))
      } else {
        Scores <- Scores |> 
          filter(.data[[filters[j]]] != filter_vals[j])
      }
    }
  }
  codes <- str_split_1(Meta$Code_Var, ",")
  #Create school codes column, add to Scores frame
  School_ID <- ""
  for(k in 1:length(codes)) {
    School_ID <- str_c(School_ID, Scores[[codes[k]]])
  }
  Scores <- cbind(Scores, School_ID)
  #Remove useless columns
  if(!is.na(Meta$Remove_Cols)) {Scores <- select(Scores, -any_of(c(str_split_1(Meta$Remove_Cols, ","), codes)))}
  #Convert numeric columns to numeric data type, filter out NAs since some schools are reported with no data
  #Also convert ACT scores to SAT scores
  numerics <- str_split_1(Meta$Numerics, ",")
  Scores <- mutate(Scores, across(all_of(numerics), as.numeric)) |> 
    drop_na() |> 
    mutate(across(all_of(numerics), round)) |> 
    rename(Reading = .data[[Meta$ACT_Reading]],
           Writing = .data[[Meta$ACT_Writing]],
           Math = .data[[Meta$ACT_Math]],
           Science = .data[[Meta$ACT_Science]],
           Total = .data[[Meta$ACT_Total]]) |> 
    mutate(RandW = Reading + Writing)
  #Conversion using functions:
  SATMath <- ACTtoSATMath(tibble(Math = Scores$Math)) |> 
    rename(SAT_Math = SAT)
  SATTotal <- ACTtoSATTotal(tibble(Total = Scores$Total))$Total |> 
    rename(SAT_Total = SAT)
  SATERW <- ACTtoSATERW(tibble(RandW = Scores$RandW))$ERW |> 
    rename(SAT_ERW = SAT)
  Scores <- select(Scores, !c(RandW, Reading, Writing, Math, Science, Total))
  Scores <- cbind(Scores, SATMath, SATTotal, SATERW)
  #Check whether data has participation rates; if not, and the data has test counts, calculate participation using NCES student counts
  #If the target group is 11th graders, participation = tests/11th graders;
  #If graduating seniors, participation = tests/12th graders;
  #If no specific grade, participation = tests/(11th + 12th).
  if(is.na(Meta$Part_Rate)) {
    nces_ids <- read_excel(paste0("Data/nces_and_state_ids/nces_and_state_id_",tolower({{state}}),".xlsx"), skip = 14) |> 
      rename_with(.fn = ~ str_remove_all(.x, pattern = "\n")) |>
      rename_with(.fn = ~ str_remove_all(.x, pattern = "\r")) |>
      rename_with(.fn = ~ str_remove_all(.x, pattern = " ")) |> 
      select(NCESSchoolID,StateSchoolID) |> 
      rename(NCES_ID = NCESSchoolID, School_ID = StateSchoolID)
    
    #transform the state school id depending on the format the state uses
    if({{state}} == "TX") {nces_ids <- mutate(nces_ids, School_ID = str_sub(School_ID, 11, -1))}
    else if({{state}} == "TN") {nces_ids <- mutate(nces_ids, School_ID = str_c(str_sub(School_ID, 4, 8), str_sub(School_ID, 10, 13)))}
    else if({{state}} == "SC") {nces_ids <- mutate(nces_ids, School_ID = str_c(str_sub(School_ID, 4, 7), str_sub(School_ID, 9, 11)))}
    else if({{state}} == "NJ") {nces_ids <- mutate(nces_ids, School_ID = str_c(str_sub(School_ID, 4, 9), str_sub(School_ID, 11, -1)))}
    else if({{state}} == "NC") {nces_ids <- mutate(nces_ids, School_ID = str_c(str_sub(School_ID, 4, 6), str_sub(School_ID, 8, 10)))}
    else if({{state}} == "MA") {nces_ids <- mutate(nces_ids, School_ID = str_sub(School_ID, 9, -1))}
    else if({{state}} == "IA") {nces_ids <- mutate(nces_ids, School_ID = str_c(str_sub(School_ID, 17, 20), "0", str_sub(School_ID, 22, 24)))}
    else if({{state}} == "GA") {nces_ids <- mutate(nces_ids, School_ID = ifelse(str_sub(School_ID, 7, 7) == '-', str_c(str_sub(School_ID, 4, 6), str_sub(School_ID, 8, 11)), str_sub(School_ID, 4, 10)))}
    else if({{state}} == "CO") {nces_ids <- mutate(nces_ids, School_ID = str_c(str_sub(School_ID, 4, 7), str_sub(School_ID, 9, 12)))}
    
    #Add membership counts to create participation rates:
    Scores <- left_join(Scores, nces_ids, by = "School_ID") |> 
      drop_na()
    Scores <- left_join(
      Scores,
      read_csv("Data/11th_12th_grade_membership/membershipcount.csv", skip = 6) |> 
        rename_with(.fn = ~ str_remove_all(.x, pattern = " ")) |> 
        select(`SchoolID-NCESAssigned[PublicSchool]Latestavailableyear`, ends_with(str_sub(as.character({{year}}), 3, 4))) |> 
        rename_with(.fn = ~ str_sub(.x, 1, 7)) |> 
        rename(NCES_ID = SchoolI) |> 
        mutate(NCES_ID = as.character(NCES_ID), Grade12 = as.numeric(Grade12), Grade11 = as.numeric(Grade11)),
      by = "NCES_ID"
    )
    
    #Create participation rates:
    if(Meta$Target_Grp == "11th") {
      Scores <- mutate(Scores, p_rate = round(1000*.data[[Meta$Num_Tests]]/Grade11)/10)
    } else if(Meta$Target_Grp == "12th") {
      Scores <- mutate(Scores, p_rate = round(1000*.data[[Meta$Num_Tests]]/Grade12)/10)
    } else {
      Scores <- mutate(Scores, p_rate = round(1000*.data[[Meta$Num_Tests]]/(Grade11 + Grade12))/10)
    }
    Scores <- Scores |> 
      drop_na(p_rate) |> 
      select(!c(NCES_ID, Grade11, Grade12)) |> 
      mutate(p_rate = case_when(
        p_rate >= 100 ~ 100,
        p_rate < 100 ~ p_rate
      ))
    Meta <- mutate(Meta, Part_Rate = "p_rate", Numerics = paste0(Numerics,",p_rate"))
  }
  Meta <- mutate(Meta, Numerics = paste0("SAT_ERW,SAT_Math,SAT_Total,",Part_Rate,",",Num_Tests), SAT_ERW = "SAT_ERW", SAT_Math = "SAT_Math", SAT_Total = "SAT_Total")
  #If need to pivot wider, do so
  if(Meta$Pivot_Wide == "Y") {
    names <- distinct(as_tibble(Scores[[Meta$Pivot_Names_From]]))
    #Drop NAs again after pivoting since some pivoted columns are unavailable due to faulty data
    #e.g., if the pivot group is gender, and scores are only reported for Female students, then the data is incomplete
    Scores <- Scores |> 
      pivot_wider(names_from = Meta$Pivot_Names_From, values_from = numerics, values_fill = NA)  |> 
      drop_na()
    
    #Calculate relevant variables for combined pivot groups
    #Use test counts as the weighting variable
    #First, create total test count variable:
    num_tests_index <- min(which(str_starts(names(Scores), Meta$Num_Tests)))
    num_groups <- length(names$value)
    Scores$Num_Tests <- rowSums(Scores[num_tests_index:(num_tests_index+num_groups-1)])
    #Create participation rate variable:
    participation_index <- min(which(str_starts(names(Scores), Meta$Part_Rate)))
    p_rates <- cbind(Scores[num_tests_index:(num_tests_index+num_groups-1)], Scores[participation_index:(participation_index+num_groups-1)])
    for(m in 1:num_groups) {
      p_rates[,m] <- p_rates[,m] * 100 / p_rates[,m+num_groups]
    }
    p_rates$Total_Tests <- rowSums(p_rates[1:num_groups])
    p_rates <- select(p_rates, Total_Tests)
    Scores <- cbind(Scores, p_rates) |> 
      mutate(Part_Rate = Num_Tests/Total_Tests)
    #For all other groups, create the total numeric variable as the weighted sum of the group values
    #Apply weight to test scores
    for(l in names$value) {
      group_count_name <- paste0(Meta$Num_Tests, "_", l)
      Scores$Weight <- Scores[[group_count_name]]/Scores$Num_Tests
      Scores <- mutate(Scores, across(contains(l, ignore.case = FALSE), \(x) x * Weight))
    }
    #ERW:
    ERW_index <- min(which(str_starts(names(Scores), Meta$SAT_ERW)))
    SAT_ERW <- tibble(SAT_ERW = rowSums(Scores[ERW_index:(ERW_index+num_groups-1)]))
    #Math:
    Math_index <- min(which(str_starts(names(Scores), Meta$SAT_Math)))
    SAT_Math <- tibble(SAT_Math = rowSums(Scores[Math_index:(Math_index+num_groups-1)]))
    #Total:
    Total_index <- min(which(str_starts(names(Scores), Meta$SAT_Total)))
    SAT_Total <- tibble(SAT_Total = rowSums(Scores[Total_index:(Total_index+num_groups-1)]))
    #Bind:
    Scores <- cbind(Scores, SAT_ERW, SAT_Math, SAT_Total)
    #Final Selection:
    Scores <- rename(Scores, School_Name = .data[[Meta$School_Name]]) |> 
      select(School_Name, School_ID, Num_Tests, Part_Rate, SAT_ERW, SAT_Math, SAT_Total) |> 
      mutate(Target_Grp = Meta$Target_Grp, Year = {{year}}, State = {{state}}, FIPS = {{fips}}, Test = "ACT", TestType = Meta$TestType)
  } else {
    #Final Selection:
    Scores <- rename(Scores, School_Name = .data[[Meta$School_Name]], Num_Tests = .data[[Meta$Num_Tests]], Part_Rate = .data[[Meta$Part_Rate]]) |> 
      select(School_Name, School_ID, Num_Tests, Part_Rate, SAT_ERW, SAT_Math, SAT_Total) |> 
      mutate(Part_Rate = case_when(
        Part_Rate >= 100 ~ 100,
        Part_Rate < 100 ~ Part_Rate
      )) |> 
      mutate(Target_Grp = Meta$Target_Grp, Year = {{year}}, State = {{state}}, FIPS = {{fips}}, Test = "ACT", TestType = Meta$TestType)
  }
  return(Scores)
}

SATonly <- function(state, year, fips) {
  Meta <- read_excel(paste0("Data/", {{state}}, "_Tests/", {{state}}, "_Meta.xlsx"), na = c("","NA"), sheet = "SAT") |> 
    filter(Year == {{year}})
  Scores <- read_excel(paste0("Data/", {{state}}, "_Tests/", {{year}},"_SAT_", {{state}}, ".xlsx"), skip = as.numeric(Meta$Skip), sheet = Meta$Sheet)  |> 
    rename_with(.fn = ~ str_remove_all(.x, pattern = "\n")) |>
    rename_with(.fn = ~ str_remove_all(.x, pattern = "\r")) |>
    rename_with(.fn = ~ str_remove_all(.x, pattern = " "))
  if(!is.na(Meta$Filter_Var)) {
    filters <- str_split_1(Meta$Filter_Var, ",") |> 
      str_remove_all(pattern = "'")
    filter_exps <- str_split_1(Meta$Filter_Exp, ",") |> 
      str_remove_all(pattern = "'")
    filter_vals <- str_split_1(Meta$Filter_Val, ",") |> 
      str_remove_all(pattern = "'")
    for(j in 1:length(filters)) {
      if(filter_exps[j] == "==") {
        Scores <- Scores |> 
          filter(.data[[filters[j]]] == filter_vals[j])
      } else if(filter_exps[j] == "%in%") {
        Scores <- Scores |> 
          filter(.data[[filters[j]]] %in% str_split_1(filter_vals[j], ";"))
      } else {
        Scores <- Scores |> 
          filter(.data[[filters[j]]] != filter_vals[j])
      }
    }
  }
  codes <- str_split_1(Meta$Code_Var, ",")
  #Create school codes column, add to Scores frame
  School_ID <- ""
  for(k in 1:length(codes)) {
    School_ID <- str_c(School_ID, Scores[[codes[k]]])
  }
  Scores <- cbind(Scores, School_ID)
  #Remove useless columns
  if(!is.na(Meta$Remove_Cols)) {Scores <- select(Scores, -any_of(c(str_split_1(Meta$Remove_Cols, ","), codes)))}
  #Convert numeric columns to numeric data type, filter out NAs since some schools are reported with no data
  numerics <- str_split_1(Meta$Numerics, ",")
  Scores <- mutate(Scores, across(all_of(numerics), as.numeric)) |> 
    drop_na()
  
  #Check whether data has participation rates; if not, and the data has test counts, calculate participation using NCES student counts
  #If the target group is 11th graders, participation = tests/11th graders;
  #If graduating seniors, participation = tests/12th graders;
  #If no specific grade, participation = tests/(11th + 12th).
  if(is.na(Meta$Part_Rate)) {
    nces_ids <- read_excel(paste0("Data/nces_and_state_ids/nces_and_state_id_",tolower({{state}}),".xlsx"), skip = 14) |> 
      rename_with(.fn = ~ str_remove_all(.x, pattern = "\n")) |>
      rename_with(.fn = ~ str_remove_all(.x, pattern = "\r")) |>
      rename_with(.fn = ~ str_remove_all(.x, pattern = " ")) |> 
      select(NCESSchoolID,StateSchoolID) |> 
      rename(NCES_ID = NCESSchoolID, School_ID = StateSchoolID)
    
    #transform the state school id depending on the format the state uses
    if({{state}} == "TX") {nces_ids <- mutate(nces_ids, School_ID = str_sub(School_ID, 11, -1))}
    else if({{state}} == "TN") {nces_ids <- mutate(nces_ids, School_ID = str_c(str_sub(School_ID, 4, 8), str_sub(School_ID, 10, 13)))}
    else if({{state}} == "SC") {nces_ids <- mutate(nces_ids, School_ID = str_c(str_sub(School_ID, 4, 7), str_sub(School_ID, 9, 11)))}
    else if({{state}} == "NJ") {nces_ids <- mutate(nces_ids, School_ID = str_c(str_sub(School_ID, 4, 9), str_sub(School_ID, 11, -1)))}
    else if({{state}} == "NC") {nces_ids <- mutate(nces_ids, School_ID = str_c(str_sub(School_ID, 4, 6), str_sub(School_ID, 8, 10)))}
    else if({{state}} == "MA") {nces_ids <- mutate(nces_ids, School_ID = str_sub(School_ID, 9, -1))}
    else if({{state}} == "IA") {nces_ids <- mutate(nces_ids, School_ID = str_c(str_sub(School_ID, 17, 20), "0", str_sub(School_ID, 22, 24)))}
    else if({{state}} == "GA") {nces_ids <- mutate(nces_ids, School_ID = ifelse(str_sub(School_ID, 7, 7) == '-', str_c(str_sub(School_ID, 4, 6), str_sub(School_ID, 8, 11)), str_sub(School_ID, 4, 10)))}
    else if({{state}} == "CO") {nces_ids <- mutate(nces_ids, School_ID = str_c(str_sub(School_ID, 4, 7), str_sub(School_ID, 9, 12)))}
    
    #Add membership counts to create participation rates:
    Scores <- left_join(Scores, nces_ids, by = "School_ID") |> 
      drop_na()
    Scores <- left_join(
      Scores,
      read_csv("Data/11th_12th_grade_membership/membershipcount.csv", skip = 6) |> 
        rename_with(.fn = ~ str_remove_all(.x, pattern = " ")) |> 
        select(`SchoolID-NCESAssigned[PublicSchool]Latestavailableyear`, ends_with(str_sub(as.character({{year}}), 3, 4))) |> 
        rename_with(.fn = ~ str_sub(.x, 1, 7)) |> 
        rename(NCES_ID = SchoolI) |> 
        mutate(NCES_ID = as.character(NCES_ID), Grade12 = as.numeric(Grade12), Grade11 = as.numeric(Grade11)),
      by = "NCES_ID"
    )
    
    #Create participation rates:
    if(Meta$Target_Grp == "11th") {
      Scores <- mutate(Scores, p_rate = round(1000*.data[[Meta$Num_Tests]]/Grade11)/10)
    } else if(Meta$Target_Grp == "12th") {
      Scores <- mutate(Scores, p_rate = round(1000*.data[[Meta$Num_Tests]]/Grade12)/10)
    } else {
      Scores <- mutate(Scores, p_rate = round(1000*.data[[Meta$Num_Tests]]/(Grade11 + Grade12))/10)
    }
    Scores <- Scores |> 
      drop_na(p_rate) |> 
      select(!c(NCES_ID, Grade11, Grade12)) |> 
      mutate(p_rate = case_when(
        p_rate >= 100 ~ 100,
        p_rate < 100 ~ p_rate
      ))
    Meta <- mutate(Meta, Part_Rate = "p_rate", Numerics = paste0(Numerics,",p_rate"))
    numerics <- str_split_1(Meta$Numerics, ",")
  }
  #If need to pivot wider, do so
  if(Meta$Pivot_Wide == "Y") {
    names <- distinct(as_tibble(Scores[[Meta$Pivot_Names_From]]))
    #Drop NAs again after pivoting since some pivoted columns are unavailable due to faulty data
    #e.g., if the pivot group is gender, and scores are only reported for Female students, then the data is incomplete
    Scores <- Scores |> 
      pivot_wider(names_from = Meta$Pivot_Names_From, values_from = numerics, values_fill = NA)  |> 
      drop_na()
    
    #Calculate relevant variables for combined pivot groups
    #Use test counts as the weighting variable
    #First, create total test count variable:
    num_tests_index <- min(which(str_starts(names(Scores), Meta$Num_Tests)))
    num_groups <- length(names$value)
    Scores$Num_Tests <- rowSums(Scores[num_tests_index:(num_tests_index+num_groups-1)])
    #Create participation rate variable:
    participation_index <- min(which(str_starts(names(Scores), Meta$Part_Rate)))
    p_rates <- cbind(Scores[num_tests_index:(num_tests_index+num_groups-1)], Scores[participation_index:(participation_index+num_groups-1)])
    for(m in 1:num_groups) {
      p_rates[,m] <- p_rates[,m] * 100 / p_rates[,m+num_groups]
    }
    p_rates$Total_Tests <- rowSums(p_rates[1:num_groups])
    p_rates <- select(p_rates, Total_Tests)
    Scores <- cbind(Scores, p_rates) |> 
      mutate(Part_Rate = 100 * Num_Tests/Total_Tests)
    #For all other groups, create the total numeric variable as the weighted sum of the group values
    #Apply weight to test scores
    for(l in names$value) {
      group_count_name <- paste0(Meta$Num_Tests, "_", l)
      Scores$Weight <- Scores[[group_count_name]]/Scores$Num_Tests
      Scores <- mutate(Scores, across(contains(l, ignore.case = FALSE), \(x) x * Weight))
    }
    #ERW:
    ERW_index <- min(which(str_starts(names(Scores), Meta$SAT_ERW)))
    SAT_ERW <- tibble(SAT_ERW = rowSums(Scores[ERW_index:(ERW_index+num_groups-1)]))
    #Math:
    Math_index <- min(which(str_starts(names(Scores), Meta$SAT_Math)))
    SAT_Math <- tibble(SAT_Math = rowSums(Scores[Math_index:(Math_index+num_groups-1)]))
    #Total:
    Total_index <- min(which(str_starts(names(Scores), Meta$SAT_Total)))
    SAT_Total <- tibble(SAT_Total = rowSums(Scores[Total_index:(Total_index+num_groups-1)]))
    #Bind:
    Scores <- cbind(Scores, SAT_ERW, SAT_Math, SAT_Total)
    #Final selection:
    Scores <- rename(Scores, School_Name = .data[[Meta$School_Name]]) |> 
      select(School_Name, School_ID, Num_Tests, Part_Rate, SAT_ERW, SAT_Math, SAT_Total) |> 
      mutate(Target_Grp = Meta$Target_Grp, Year = {{year}}, State = {{state}}, FIPS = {{fips}}, Test = "SAT", Part_Rate = Part_Rate, TestType = Meta$TestType)
  } else {
    #Final selection:
    Scores <- rename(Scores, School_Name = .data[[Meta$School_Name]], Num_Tests = .data[[Meta$Num_Tests]], Part_Rate = .data[[Meta$Part_Rate]], SAT_ERW = .data[[Meta$SAT_ERW]], SAT_Math = .data[[Meta$SAT_Math]], SAT_Total = .data[[Meta$SAT_Total]]) |> 
      select(School_Name, School_ID, Num_Tests, Part_Rate, SAT_ERW, SAT_Math, SAT_Total) |>
      mutate(Part_Rate = case_when(
        Part_Rate >= 100 ~ 100,
        Part_Rate < 100 ~ Part_Rate
      )) |> 
      mutate(Target_Grp = Meta$Target_Grp, Year = {{year}}, State = {{state}}, FIPS = {{fips}}, Test = "SAT", TestType = Meta$TestType)
  }
  return(Scores)
}

ACTtoSATTotal <- function(act) {
  conversionTotal <- read_excel("Data/Excel-ACT-SAT-Concordance-Tables.xlsx", range = "Tables A1 & Table A2!G2:I30") |> 
    select(ACT, SAT)
  return(tibble(Total=conversionTotal[match(act$Total, conversionTotal$ACT), 2]))
}

ACTtoSATMath <- function(actMath) {
  conversionMath <- read_excel("Data/Excel-ACT-SAT-Concordance-Tables.xlsx", range = "Tables B1 & B2!D2:E29")
  return(tibble(conversionMath[match(actMath$Math, conversionMath$ACT), 2]))
}

ACTtoSATERW <- function(actReadingAndWriting) {
  conversionERW <- read_excel("Data/Excel-ACT-SAT-Concordance-Tables.xlsx", range = "Tables C1 & C2!D2:E61") |> 
    mutate(ACT = ifelse(str_length(ACT) == 3, as.numeric(str_sub(ACT, 2, 3)), as.numeric(ACT)))
  return(tibble(ERW=conversionERW[match(actReadingAndWriting$RandW, conversionERW$ACT), 2]))
}
