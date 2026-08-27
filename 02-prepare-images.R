#####################################
# 02-prepare-images.py
#
# Stack, crop, and summarize HLSS30 images
#
# Alessandra Vidal Meza
# Last update: Aug 26, 2026
#####################################

library(here)
library(tidyverse)
library(terra)
library(sf)

condition <- str_c(c("NDVI", "EVI", "SAVI", "MSAVI"), collapse = "|")
tile_id_pattern <- "T[0-9]{2}[A-Z]{3}"

tif_filepaths <- list.files(
  path = here("data", "HLSS30"),
  pattern = paste0("\\.", condition, ".tif$"),
  full.names = TRUE
)

raster_stack <- tibble(
  filepath = tif_filepaths,
  vi = str_extract(basename(filepath), condition),
  tile_id = str_extract(basename(filepath), tile_id_pattern)
) |>
  summarise(stack = list(rast(filepath)), .by = c(vi, tile_id)) # Read raster stack

scale_factor <- 0.0001
missing_value <- -9999

raster_stack <- raster_stack |>
  mutate(
    mean_raster = map(stack, function(r) {
      r <- subst(r, missing_value, NA) # Drop missing values (global raster algebra)
      r <- r * scale_factor # Apply scale factor (global raster algebra)
      mean(r, na.rm = TRUE) # Find mean (local raster algebra)
    })
  )

mosaics_vi <- raster_stack |>
  mutate(vi = as.factor(vi)) |>
  # Apply mosaic() to mean_raster iteratively
  summarize(
    mosaic = list(do.call(mosaic, mean_raster)),
    .by = vi
  ) |>
  mutate(outpath = here("outputs", paste0(vi, "_stl_mean.tif")))


stl <- st_read(here("data", "tl_2020_us_uac20", "tl_2020_us_uac20.shp")) |>
  filter(NAME20 == "St. Louis, MO--IL") |>
  vect() # Convert from sf to SpatVector object

mosaics_vi <- mosaics_vi |>
  mutate(
    mosaics_vi_stl = map(mosaic, function(r) {
      r <- project(r, crs(stl))
      crop(r, stl) # Crop to extent of St. Louis
    })
  )

pwalk(
  select(mosaics_vi, mosaics_vi_stl, outpath),
  function(mosaics_vi_stl, outpath) {
    writeRaster(mosaics_vi_stl, outpath, overwrite = TRUE) # Save each raster to outputs/
  }
)
