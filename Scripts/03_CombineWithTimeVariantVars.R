library(tidyverse)
library(readxl)
library(sf)
library(tidycensus)
library(tigris)
library(educationdata)

# options(tigris_use_cache = TRUE) <- allows you to download queried shapefiles for faster re-use if desired
load("Data/Cleaned_Scores/cleaned_scores_all_years.Rdata")
source("Functions/create_point_geography.R")
source("Functions/spatial_joins.R")
source("Functions/MEPSandEDFacts.R")
source("Functions/IPR.R")

#Convert SAT scores and schools to geometric points
#Point locations from urban institute, sourced from the CCD 2022.
FullScores <- toSF(FullScores)

#Spatial join to geographies, add geographic-level vars
years <- c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)
#This process takes a while since queries are made to the ACS database for both data of interest and geography files
#Speed thus highly depends on quality of internet
df <- filter(FullScores, Year == 2011) |> 
  tractJoin(year = 2011)
for(year in years) {
  df <- rbind(
    df,
    filter(FullScores, Year == year) |> 
      tractJoin(year = year)
  )
}
save(df, file = "Data/tmp.Rdata")


#Add zip codes
#This operation is very computationally intensive
zip_geo <- zctas(year = 2010, cb = TRUE) |> 
  as_tibble() |> 
  select(ZCTA5, geometry) |> 
  mutate(ZCTA5 = as.numeric(ZCTA5)) |> 
  rename(zip = ZCTA5) |> 
  st_as_sf()
df <- st_join(df, zip_geo)
rm(zip_geo)

#Add pumas
states <- state.abb
puma_geo <- pumas(year = 2013, state = "TX", cb=TRUE) |> as_tibble()
for(state in states) {
  puma_geo <- rbind(
    puma_geo,
    pumas(year = 2013, state = state, cb=TRUE) |> as_tibble()
  )
}
puma_geo <- select(puma_geo, PUMACE10, geometry) |> 
  st_as_sf() |> 
  rename(puma = PUMACE10) |> 
  distinct()
df <- st_join(df, puma_geo)

#We need to separate Tract_GEOID into 2010 and 2020 tracts.
df1 <- filter(df, Year <= 2020) |> 
  rename(tract2010 = Tract_GEOID)
df2 <- filter(df, Year > 2020) |> 
  rename(tract2020 = Tract_GEOID)
df1 <- left_join(df1, as_tibble(df2) |> select(NCES_ID, tract2020) |> distinct(NCES_ID, .keep_all = TRUE), by="NCES_ID") |> 
  relocate(tract2010, tract2020)
df2 <- left_join(df2, as_tibble(df1) |> select(NCES_ID, tract2010) |> distinct(NCES_ID, .keep_all = TRUE), by="NCES_ID") |> 
  relocate(tract2010, tract2020)
df <- rbind(df1, df2)
rm(df1, df2)
save(df, file="Data/tmp.Rdata")

#df1 <- mepsAndEDFacts()
#save(df1, file="Data/MEPSandEDFacts.Rdata")

#IPR <- IPR()
#save(IPR, file="Data/IPR.Rdata")
