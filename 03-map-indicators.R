#####################################
# 03-map-indicators.R
#
# Map HLSS30 images
#
# Fiona Liang
# Last update: Sep 07, 2026
#####################################

library(tidyverse)
library(sf)
library(terra)
library(RColorBrewer)

options(scipen = 10000)

stl <- st_read("data/tl_2020_us_uac20/tl_2020_us_uac20.shp") |>
  filter(NAME20 == "St. Louis, MO--IL")

ndvi_stl <- rast("outputs/NDVI_stl_mean.tif") |>
  crop(stl) |>
  mask(stl) |>
  as.data.frame(xy = T)

evi_stl <- rast("outputs/EVI_stl_mean.tif") |>
  crop(stl) |>
  mask(stl) |>
  as.data.frame(xy = T)

savi_stl <- rast("outputs/SAVI_stl_mean.tif") |>
  crop(stl) |>
  mask(stl) |>
  as.data.frame(xy = T)

ggplot() +
  geom_raster(data = ndvi_stl, aes(x = x, y = y, fill = mean)) +
  scale_fill_viridis_c(name = "Values") +
  labs(
    title = "Mean Normalized Difference Vegetatioon Index (NDVI)",
    subtitle = "St. Louis, Missouri - June to August of 2021",
    x = "Longitude",
    y = "Latitude",
    caption = "Data: NASA HLSS30 Daily 30m v2.0"
  ) +
  theme_minimal() +
  coord_quickmap()

ggsave("figures/NDVI_stl.png")

ggplot() +
  geom_raster(data = evi_stl, aes(x = x, y = y, fill = mean)) +
  labs(
    title = "Mean Enhanced Vegation Index (EVI)",
    subtitle = "St. Louis, Missouri - June to August of 2021",
    x = "Longitude",
    y = "Latitude",
    caption = "Data: NASA HLSS30 Daily 30m v2.0"
  ) +
  scale_fill_viridis_c(name = "Values") +
  theme_minimal() +
  coord_quickmap()

ggsave("figures/EVI_stl.png")

ggplot() +
  geom_raster(data = savi_stl, aes(x = x, y = y, fill = mean)) +
  labs(
    title = "Mean Soil-Adjusted Vegetation Index (SAVI)",
    subtitle = "St. Louis, Missouri - June to August of 2021",
    x = "Longitude",
    y = "Latitude",
    caption = "Data: NASA HLSS30 Daily 30m v2.0"
  ) +
  scale_fill_viridis_c(name = "Values") +
  theme_minimal() +
  coord_quickmap()

ggsave("figures/SAVI_stl.png")
