library(tidyverse)
library(tidycensus)
options(tigris_use_cache = TRUE)
library(arcgisbinding)
arc.check_product()

schoolpov <- arc.open("C:/Users/dread/OneDrive/Documents/ArcGIS/Packages/school_pov_metrics_05f34c/p20/school_pov_metrics.lyrx")
schoolpov_df <- arc.select(object = schoolpov)
schoolpov_df <- schoolpov_df |> 
  filter(fips == 34 | fips == 37 | fips == 45 | fips == 48)

