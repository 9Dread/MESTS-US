library(sf)
library(tidyverse)
library(viridis)
library(mapview)

load("Data/TimeInvarPredictedFrame.Rdata")
states <- states(cb=TRUE) |> 
  st_transform(crs = 4269) |> 
  filter(STUSPS != "AS") |> 
  filter(STUSPS != "PR") |> 
  filter(STUSPS != "MP") |> 
  filter(STUSPS != "VI") |> 
  filter(STUSPS != "GU")
ggplot() +
  geom_sf(data=shift_geometry(states), fill="white") +
  geom_sf(data=shift_geometry(scHS), mapping=aes(color = SAT_Total)) +
  scale_color_viridis(option = "A") +
  theme_void()

mapview(scHS, zcol = "SAT_Total")
