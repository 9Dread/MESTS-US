library(tidyverse)
library(catboost)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

#Load all social capital high schools and select predictors of interest.
scHS <- read_csv("Data/Social_Capital/social_capital_high_school.csv") |> 
  rename(NCES_ID = high_school) |> 
  select(NCES_ID, ec_high_own_ses_hs, ec_high_parent_ses_hs, volunteering_rate_hs, exposure_parent_ses_hs) |> 
  drop_na()

#Get all available point locations for the high schools.
geo_loc <- read_csv("Data/ccd_point_geom.csv") |> 
  mutate(across(c("longitude", "latitude"), as.numeric))
scHS <- inner_join(scHS, geo_loc, by="NCES_ID") |> 
  drop_na(longitude, latitude) |> 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4269)
rm(geo_loc)

#Zip code join
zip_geo <- zctas(year = 2010, cb = TRUE) |> 
  as_tibble() |> 
  select(ZCTA5, geometry) |> 
  mutate(ZCTA5 = as.numeric(ZCTA5)) |> 
  rename(zip = ZCTA5) |> 
  st_as_sf()
scHS <- st_join(scHS, zip_geo)
rm(zip_geo)

#PUMA join
states <- state.abb
puma_geo <- pumas(year = 2013, state = "TX", cb=TRUE) |> as_tibble()
for(state in states) {
  puma_geo <- rbind(
    puma_geo,
    pumas(year = 2013, state = state, cb=TRUE) |> as_tibble()
  )
}
puma_geo <- select(puma_geo, PUMACE10, geometry) |> 
  distinct() |> 
  st_as_sf() |> 
  rename(puma = PUMACE10)
scHS <- st_join(scHS, puma_geo)
rm(puma_geo, state, states)

#Join with puma and scZIP data
pumaWealth2020 <- read_csv("Data/puma_wealth_inequality.csv") |> 
  filter(year == 2020) |> 
  select(statefip, puma, wealth_median, wealth_bottom50, own_outright) |> 
  rename(FIPS = statefip)
scHS <- scHS |> 
  mutate(puma = as.numeric(puma), FIPS = as.numeric(str_sub(NCES_ID, 1, 2))) |> 
  left_join(pumaWealth2020, by=c("FIPS", "puma"))
rm(pumaWealth2020)

scZIP <- read_csv("Data/Social_Capital/social_capital_zip.csv") |> 
  select(zip, civic_organizations_zip)
scHS <- semi_join(scHS, scZIP, by="zip") |> 
  left_join(scZIP, by="zip")
rm(scZIP)

scHS <- relocate(scHS, geometry, zip, puma, FIPS) |> 
  as_tibble()

#Load models and predict
load("Models/TimeInvarR2Catboost.Rdata")
scHS <- cbind(scHS, tibble(SAT_Total = predict(model1, scHS[6:13])))

load("Models/TimeInvarMathR2Catboost.Rdata")
scHS <- cbind(scHS, tibble(SAT_Math = predict(model1, scHS[6:13])))

load("Models/TimeInvarERWR2Catboost.Rdata")
scHS <- cbind(scHS, tibble(SAT_ERW = predict(model1, scHS[6:13])))

scHS <- st_as_sf(scHS)
save(scHS, file = "Data/TimeInvarPredictedFrame.Rdata")
