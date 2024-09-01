toSF <- function(df) {
  states <- c("TX", "TN", "SC", "NJ", "NC", "MA", "IA", "GA", "CO")
  NCES_IDS <- tibble(NCES_ID = c(), School_ID = c())
  for(state in states) {
    nces_df <- read_excel(paste0("Data/nces_and_state_ids/nces_and_state_id_",tolower(state),".xlsx"), skip = 14) |> 
      rename_with(.fn = ~ str_remove_all(.x, pattern = "\n")) |>
      rename_with(.fn = ~ str_remove_all(.x, pattern = "\r")) |>
      rename_with(.fn = ~ str_remove_all(.x, pattern = " ")) |> 
      select(NCESSchoolID,StateSchoolID) |> 
      rename(NCES_ID = NCESSchoolID, School_ID = StateSchoolID)
    if(state == "TX") {nces_df <- mutate(nces_df, School_ID = str_sub(School_ID, 11, -1), State = "TX")}
    else if(state == "TN") {nces_df <- mutate(nces_df, School_ID = str_c(str_sub(School_ID, 4, 8), str_sub(School_ID, 10, 13)), State = "TN")}
    else if(state == "SC") {nces_df <- mutate(nces_df, School_ID = str_c(str_sub(School_ID, 4, 7), str_sub(School_ID, 9, 11)), State = "SC")}
    else if(state == "NJ") {nces_df <- mutate(nces_df, School_ID = str_c(str_sub(School_ID, 4, 9), str_sub(School_ID, 11, -1)), State = "NJ")}
    else if(state == "NC") {nces_df <- mutate(nces_df, School_ID = str_c(str_sub(School_ID, 4, 6), str_sub(School_ID, 8, 10)), State = "NC")}
    else if(state == "MA") {nces_df <- mutate(nces_df, School_ID = str_sub(School_ID, 9, -1), State = "MA")}
    else if(state == "IA") {nces_df <- mutate(nces_df, School_ID = str_c(str_sub(School_ID, 17, 20), "0", str_sub(School_ID, 22, 24)), State = "IA")}
    else if(state == "GA") {nces_df <- mutate(nces_df, School_ID = ifelse(str_sub(School_ID, 7, 7) == '-', str_c(str_sub(School_ID, 4, 6), str_sub(School_ID, 8, 11)), str_sub(School_ID, 4, 10)), State = "GA")}
    else if(state == "CO") {nces_df <- mutate(nces_df, School_ID = str_c(str_sub(School_ID, 4, 7), str_sub(School_ID, 9, 12)), State = "CO")}
    NCES_IDS <- rbind(NCES_IDS, nces_df)
  }
  geo_loc <- read_csv("Data/ccd_point_geom.csv") |> 
    mutate(across(c("longitude", "latitude"), as.numeric))
  df <- left_join(df, NCES_IDS, by=c("School_ID", "State")) |> 
    drop_na(NCES_ID) |> 
    left_join(geo_loc, by="NCES_ID") |> 
    drop_na(longitude, latitude) |> 
    st_as_sf(coords = c("longitude", "latitude"),
             crs = 4269)
  return(df)
}
