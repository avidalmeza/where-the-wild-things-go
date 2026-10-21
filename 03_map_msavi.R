#####################################
# 03-map-indicators.R
#
# Map HLSS30 images
#
# Fiona Liang
# Last update: Oct 20, 2026
#####################################

library(tidyverse)
library(sf)
library(terra)
library(RColorBrewer)
library(showtext)

options(scipen = 10000)

showtext_auto()
# font_add(family = "Calibri", regular = "calibri.ttf")

theme_custom <- function(base_size = 27, base_family = "Calibri") {
    theme_bw(base_size = base_size, base_family = base_family) +
    theme(plot.title = element_text(margin = margin(b = 2)),
          panel.grid = element_blank(),
          axis.ticks = element_line(linewidth = 0.45),
          axis.title = element_text(size = 25),
          legend.position = c(0.85, 0.25),
          legend.background = element_blank(),
          legend.title = element_text(size = 20, margin = margin(b = 2)), 
          legend.text = element_text(size = 16),
          legend.key.size = unit(0.2, "in"))
          #legend.key.spacing.x = unit(1.25, "pt"),
          #legend.key.spacing.y = unit(1.25, "pt")) 
}

stl <- st_read("data/tl_2020_us_uac20/tl_2020_us_uac20.shp") |>
  filter(NAME20 == "St. Louis, MO--IL")

msavi_stl <- rast("outputs/MSAVI_stl_med.tif") |>
  project(crs(vect(stl))) |>  # Match CRS 
  crop(stl) |>
  mask(stl) |>
  as.data.frame(xy = T)

# Figure 1 ----
ggplot() +
  geom_raster(data = msavi_stl, aes(x = x, y = y, fill = median)) +
  scale_fill_viridis_c(name = "MSAVI") +
  labs(
    title = "Modified soil adjusted vegetation index (MSAVI)",
    subtitle = "Median MSAVI value for June to August of 2021",
    x = element_blank(),
    y = element_blank(),
    caption = "Data: NASA HLSS30 Daily 30m v2.0") +
  theme_custom() +
  coord_quickmap()

ggsave("figures/MSAVI_stl.png")

# Figure 2 ----
events <- write_csv("racoon_events.csv")

lines_sf <- events |> 
  mutate(individual_id = as.factor(individual_id)) |> 
  arrange(individual_id, timestamp) |> 
  st_as_sf(coords = c("location_long", "location_lat"), crs = 4326) |> 
  group_by(individual_id) |> 
  summarize(do_union = FALSE) |> 
  st_cast("LINESTRING") |> 
  st_transform(crs = st_crs(stl)) |> 
  filter(individual_id == unique(events$individual_id)[6])

ggplot() +
  geom_raster(data = msavi_stl, aes(x = x, y = y, fill = median)) +
  geom_sf(data = lines_sf, linewidth = 0.76, show.legend = "line") +
  scale_fill_viridis_c(name = "MSAVI") +
  labs(
    title = "Modified soil adjusted vegetation index (MSAVI)",
    subtitle = "Median MSAVI value for June to August of 2021",
    caption = "Data: NASA HLSS30 Daily 30m v2.0",
    x = element_blank(),
    y = element_blank()
  ) +
  theme_custom() +
  coord_sf(xlim = c(-90.4, -90.2), ylim = c(38.58, 38.68), expand = FALSE)