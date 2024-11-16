library(sf)
library(tidyverse)
library(viridis)
library(tigris)
library(patchwork)
library(stars)
library(gstat)
library(gt)

scHS <- read_csv("Data/final_postprediction_frames/total_postpred.csv") |> 
  select(!c(`...1`, "geometry"))
geo_loc <- read_csv("Data/ccd_point_geom.csv") |> 
  mutate(across(everything(), as.numeric))
scHS <- inner_join(scHS, geo_loc, by="NCES_ID") |> 
  drop_na(longitude, latitude) |> 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4269)
rm(geo_loc)

states <- states(cb = TRUE) |> 
  st_transform(crs = 4269) |> 
  filter(STUSPS != "AS") |> 
  filter(STUSPS != "PR") |> 
  filter(STUSPS != "MP") |> 
  filter(STUSPS != "VI") |> 
  filter(STUSPS != "GU") |> 
  st_transform(crs = 4269)

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

#Point interpolation:
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 53fefb2483d27462542d59e4f968e06b7fd80d50
sf_use_s2(FALSE)
grid <- st_bbox(states) |> 
  st_as_stars(dx = 15000) |> 
  st_crop(states)
i <- idw(SAT_Total~1, scHS, grid)

ggplot() + geom_stars(data = i, 
                      aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  geom_sf(data = st_cast(states, "MULTILINESTRING"))
<<<<<<< HEAD
=======
#sf_use_s2(FALSE)
#grid <- st_bbox(states) |> 
#  st_as_stars(dx = 15000) |> 
#  st_crop(states)
#i <- idw(SAT_Total~1, scHS, grid)

#ggplot() + geom_stars(data = i, 
#                      aes(fill = var1.pred, x = x, y = y)) + 
#  xlab(NULL) + ylab(NULL) +
#  geom_sf(data = st_cast(states, "MULTILINESTRING"))
>>>>>>> 65c1373 (Optuna, new figures, metric and SHAP plots)
=======
>>>>>>> 53fefb2483d27462542d59e4f968e06b7fd80d50

varimp_total <- read_csv("ModelOutputs/total_feat_imp.csv")
varimp_math <- read_csv("ModelOutputs/math_feat_imp.csv")
varimp_erw <- read_csv("ModelOutputs/erw_feat_imp.csv")

gt1 <- gt(varimp_total) |> 
  tab_header(title = "SAT Total Score Model Variable Importances") |> 
  tab_style(
    style = cell_fill(color = "#fbfde2"),
    locations = cells_body(
      columns = `Feature Id`,
      rows = c(1,6,7,8,9,10,11)
      )
  ) |> 
  tab_style(
    style = cell_fill(color = "#F9E3D6"),
    locations = cells_body(
      columns = `Feature Id`,
      rows = c(2,4)
    )
  ) |> 
  tab_style(
    style = cell_fill(color = "lightcyan"),
    locations = cells_body(
      columns = `Feature Id`,
      rows = c(3,5)
    )
  )

gt2 <- gt(varimp_math) |> 
  tab_header(title = "SAT Math Score Model Variable Importances") |> 
  tab_style(
    style = cell_fill(color = "#fbfde2"),
    locations = cells_body(
      columns = `Feature Id`,
      rows = c(2,5,7,8,9,10,11)
    )
  ) |> 
  tab_style(
    style = cell_fill(color = "#F9E3D6"),
    locations = cells_body(
      columns = `Feature Id`,
      rows = c(3,4)
    )
  ) |> 
  tab_style(
    style = cell_fill(color = "lightcyan"),
    locations = cells_body(
      columns = `Feature Id`,
      rows = c(1,6)
    )
  )

gt3 <- gt(varimp_erw) |> 
  tab_header(title = "SAT ERW Score Model Variable Importances") |> 
  tab_style(
    style = cell_fill(color = "#fbfde2"),
    locations = cells_body(
      columns = `Feature Id`,
      rows = c(1,2,3,5,7,10,11)
    )
  ) |> 
  tab_style(
    style = cell_fill(color = "#F9E3D6"),
    locations = cells_body(
      columns = `Feature Id`,
      rows = c(4,8)
    )
  ) |> 
  tab_style(
    style = cell_fill(color = "lightcyan"),
    locations = cells_body(
      columns = `Feature Id`,
      rows = c(6, 9, 12)
    )
  )
gtsave(gt1, "Figures/varimp_total.png")
gtsave(gt2, "Figures/varimp_math.png")
gtsave(gt3, "Figures/varimp_erw.png")

total_metrics <- tibble(Metric = c("RMSE", "R^2"), Value = c(46.813, 76.425))
math_metrics <- tibble(Metric = c("RMSE", "R^2"), Value = c(23.967, 73.064))
erw_metrics <- tibble(Metric = c("RMSE", "R^2"), Value = c(27.574, 67.222))

gt_metrics1 <- gt(total_metrics) |> 
  tab_header(title = "Performance Metrics (Total)")
gt_metrics2 <- gt(math_metrics) |> 
  tab_header(title = "Performance Metrics (Math)")
gt_metrics3 <- gt(erw_metrics) |> 
  tab_header(title = "Performance Metrics (ERW)")

gtsave(gt_metrics1, "Figures/metrics_total.png")
gtsave(gt_metrics2, "Figures/metrics_math.png")
gtsave(gt_metrics3, "Figures/metrics_erw.png")

statesi <- c(state.abb, "DC")
counties <- counties(statesi[1], cb=TRUE)
for(state in statesi[2:51]) {
  tmp <- counties(state, cb=TRUE)
  counties <- rbind(counties, tmp)
}
counties_data <- st_join(counties, scHS, join = st_contains, left = FALSE) |> 
  group_by(STATEFP, COUNTYFP) |> 
  reframe(avg_total = mean(SAT_Total), avg_math = mean(SAT_Math), avg_erw = mean(SAT_ERW), n = n(), geometry = geometry) |> 
  distinct() |> 
  st_as_sf()

p1 <- ggplot() +
  geom_sf(data=shift_geometry(counties_data), mapping=aes(fill = avg_total), color = NA) +
  geom_sf(data=shift_geometry(states), fill = NA, color = "black") +
  scale_fill_viridis(option = "A") +
  theme_void() +
  labs(fill = "Total") +
  theme(legend.direction = "horizontal", legend.title.position = "top")

p2 <- ggplot() +
  geom_sf(data=shift_geometry(counties_data), mapping=aes(fill = avg_math), color = NA) +
  geom_sf(data=shift_geometry(states), fill= NA, color = "black") +
  scale_fill_viridis(option = "D") +
  theme_void() +
  labs(fill = "Math") +
  theme(legend.direction = "horizontal", legend.title.position = "top")

p3 <- ggplot() +
  geom_sf(data=shift_geometry(counties_data), mapping=aes(fill = avg_erw), color = NA) +
  geom_sf(data=shift_geometry(states), fill= NA, color = "black") +
  scale_fill_viridis(option = "E") +
  theme_void() +
  labs(fill = "ERW") +
  theme(legend.direction = "horizontal", legend.title.position = "top")

layout <- "
  AB
  CD
"
plot <- p1 + p2 + p3 + guide_area() +
  plot_layout(design = layout, guides = "collect") +
  plot_annotation(title = "Spatial Distribution of Predicted SAT Scores",
                  subtitle = "Average Scores by County")
ggsave(filename = "Counties.PNG", path = "Figures")

ggplot() +
  geom_sf(data=shift_geometry(counties_data), mapping=aes(fill = avg_total), color = NA) +
  geom_sf(data=shift_geometry(states |> filter(STUSPS == "AK" | STUSPS == "HI")), fill= NA, color = "black") +
  scale_fill_viridis(option = "A") +
  labs(title = "Predicted Average School-Level SAT Scores by County", fill = "SAT_Total") +
  theme(legend.direction = "vertical", legend.title.position = "top")
ggsave(filename = "Counties_Total.PNG", path = "Figures")
