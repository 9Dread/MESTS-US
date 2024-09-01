tractJoin <- function(pts, year) {
  states <- c("TN", "SC", "NJ", "NC", "MA", "IA", "GA", "CO")
  tract <- get_acs(
    geography = "tract",
    variables = c(IPR = "B05010_001", MEDINC = "B06011_001"),
    year = (year-1),
    state = "TX",
    geometry = TRUE,
    cache_table = TRUE
  ) |> 
    as_tibble() |> 
    pivot_wider(names_from = "variable", values_from = c("estimate", "moe")) |> 
    rename(Tract_GEOID = GEOID) |> 
    select(!NAME)
  for(state in states) {
    tract <- rbind(
      tract,
       get_acs(
        geography = "tract",
        variables = c(IPR = "B05010_001", MEDINC = "B06011_001"),
        year = (year-1),
        state = state,
        geometry = TRUE,
        cache_table = TRUE
      ) |> 
        as_tibble() |> 
        pivot_wider(names_from = "variable", values_from = c("estimate", "moe")) |> 
        rename(Tract_GEOID = GEOID) |> 
        select(!NAME)
    )
  }
  tract <- st_as_sf(tract, sf_column_name = "geometry")
  return(st_join(pts, tract))
}
