library(sf)
library(tidyverse)
library(viridis)
library(mapview)
library(tigris)
library(patchwork)

load("Data/TimeInvarPredictedFrame.Rdata")
states <- states(cb=TRUE) |> 
  st_transform(crs = 4269) |> 
  filter(STUSPS != "AS") |> 
  filter(STUSPS != "PR") |> 
  filter(STUSPS != "MP") |> 
  filter(STUSPS != "VI") |> 
  filter(STUSPS != "GU")
p1 <- ggplot() +
  geom_sf(data=shift_geometry(states), fill="white") +
  geom_sf(data=shift_geometry(scHS), mapping=aes(color = SAT_Total)) +
  scale_color_viridis(option = "A") +
  theme_void() +
  labs(color = "Total") +
  theme(legend.direction = "horizontal", legend.title.position = "top")

p2 <- ggplot() +
  geom_sf(data=shift_geometry(states), fill="white") +
  geom_sf(data=shift_geometry(scHS), mapping=aes(color = SAT_Math)) +
  scale_color_viridis(option = "D") +
  theme_void() +
  labs(color = "Math") +
  theme(legend.direction = "horizontal", legend.title.position = "top")

p3 <- ggplot() +
  geom_sf(data=shift_geometry(states), fill="white") +
  geom_sf(data=shift_geometry(scHS), mapping=aes(color = SAT_ERW)) +
  scale_color_viridis(option = "E") +
  theme_void() +
  labs(color = "ERW") +
  theme(legend.direction = "horizontal", legend.title.position = "top")

layout <- "
  AB
  CD
"
plot <- p1 + p2 + p3 + guide_area() +
  plot_layout(design = layout, guides = "collect") +
  plot_annotation(title = "Spatial Distribution of Predicted SAT Scores",
                  subtitle = "Each point is a high school")
ggsave(filename = "Points.PNG", path = "Figures")

mapview(scHS, zcol = "SAT_Total")
