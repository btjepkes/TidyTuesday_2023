### Tidy Tuesday - Week 21 ###
### Central Park - Squirrel Census ###
### Created on 05/25/2023 by Benjamin Tjepkes ###

# Some help from https://github.com/gdatascience/tidytuesday/blob/master/2023_05_23_tidy_tuesday_squirrels.Rmd

# Setting Up Env and Data ----

# Load packages
library(tidytuesdayR)
library(tidyverse)
library(sf)
library(tmap)
library(ggmap)
library(magick)
library(ggimage)
library(ggthemes)
library(ggspatial)
library(ggtext)
library(patchwork)
library(showtext)

# Load in squirrel obs data
tuesdata <- tidytuesdayR::tt_load(2023, week = 21)

squirrel_data <- tuesdata$squirrel_data


# Explore squirrel obs data
# squirrel_data %>% View()
# 
# squirrel_data %>% glimpse()
# 
# table(squirrel_data$`Primary Fur Color`)
# table(squirrel_data$Age)

# Load in Central Park grid

cp_grid <- st_read("2018 Central Park Squirrel Census - Hectare Grid/geo_export_12776b84-74b1-4824-ab50-bad9b278a6dd.shp")

# cp_grid_crs <- st_crs(cp_grid)
# 
# tiles="http://a.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png"
# 
# tmap_mode("plot")
# 
# tm_shape(cp_grid) +
#   tm_polygons(col = "id") +
# 
# 
# my_grid <- ggplot(cp_grid) + 
#   geom_sf() +
#   theme_map()
# 
# my_grid


# Wrangle Data ============================================================

## Clean column names
squirrels <- squirrel_data %>% janitor::clean_names()


squirrels <- squirrels %>% 
  mutate(primary_fur_color = ifelse(is.na(primary_fur_color), "Unknown", primary_fur_color))


# Create Table ============================================================

library(gt)
library(gtExtras)
library(webshot2)
library(png)

# Count the number of observations in each fur color
squirrels_summary <- squirrels %>% 
  group_by(primary_fur_color) %>% 
  summarise(Count = n()) %>% 
  ungroup() %>% 
  arrange(-Count) %>% 
  rename('Primary Fur Color' = primary_fur_color) %>% 
  mutate(Proportion = Count) %>% 
  mutate(img = c('icons/img-squirrel-gray-jpeg.jpg',
                 'icons/img-squirrel-cinnamon.png',
                 "icons/img-squirrel-black.png",
                 "icons/img-squirrel-unknown.png"))


# Create a base gt table from the summary table
squirrels_summary_gt <- squirrels_summary %>% 
  select(img, `Primary Fur Color`, Count) %>% 
  gt()

my_table <- squirrels_summary_gt %>% gt_theme_nytimes() %>% 
  tab_header(
    title = "Squirrel Sightings by Color of Pelage") %>% 
  cols_width(
    img ~ pct(18),
    'Primary Fur Color' ~ pct(28),
    Count ~ pct(15)) %>% 
  
  gt_color_rows(columns = Count, palette = "ggsci::indigo_material") %>% # "ggsci::indigo_material"
  
  # gt_plt_bar_pct(column = Proportion, scaled = FALSE, fill = "#2c7fb8", background = "gray") %>% 
  
  # gt_img_circle(column = img, border_color = "black", height = 50) %>% 
  
  gt_img_rows(columns = img, img_source = "local", height = 60) %>% 
  
  tab_options(table.width = px(200),
              container.width = px(200),
              table.margin.left = px(0),
              table.margin.right = px(0),
              container.padding.x = px(0),
              container.padding.y = px(0)) %>% 
  
  tab_style(
    list(cell_text(weight = "bold")),
    locations = cells_body(
         columns = c('Primary Fur Color', Count))) %>% 
  
  cols_label(
    Count = "Count",
    img = "Color",
    'Primary Fur Color' = "") %>% 
  
  cols_align(columns = Count, align = "center")

my_table


f <- chromote::default_chromote_object()
f$close()

gtsave_extra(data = my_table, filename = "table_bycolor.png", zoom = 2)

tbl_image <- readPNG("table_bycolor_cropped.png", native = TRUE)


# Prep ====================================================================

# Bounding box
bbox_new <- st_bbox(cp_grid)
bbox_new[1] <- bbox_new[1] + 0.01 # xmin - left
bbox_new <- bbox_new %>% st_as_sfc()
plot(bbox_new)

# Create sf object for squirrels data
data_sf <- st_as_sf(squirrels, coords = c("x", "y"))

# Find bounding box
bbox_temp <- st_bbox(data_sf)

# Convert bb to sf object
bbox_sfc <- st_as_sfc(bbox_temp)

# Buffer around sf object
buffered_sf <- st_buffer(bbox_sfc, dist = 0.005)

crs <- st_crs("+proj=longlat +datum=WGS84 +no_defs")
st_crs(buffered_sf) <- crs

bbox <- st_bbox(buffered_sf)

bbox_converted <- st_as_sfc(bbox)
st_crs(bbox_converted) <- crs

# Test Bounding Box ======================================

# tmap_mode("plot")
# 
# tm_shape(buffered_sf) +
#   tm_polygons() +
# 
# tm_shape(data_sf) +
#   tm_dots()

# Build out map ==========================================

map_cont <- ggplot() +
  
  geom_sf(data = nycity_shapefile_clipped,
          fill = "darkgray", color = "gray") + #lightgray
  coord_sf(expand = FALSE) +
  
  geom_sf(data = water_features$osm_polygons,
          color = "darkgray",
          fill = "lightblue",
          alpha = 0.8) +
  coord_sf(expand = FALSE) +
  
  geom_sf(data = road_features_main,
          color = alpha("gray80", 0.4),
          linewidth = 0.9) +
  coord_sf(expand = FALSE) +
  
  # annotation_map_tile(zoom = 14, cachedir = system.file("rosm.cache", package = "ggspatial"), type = "thunderforestoutdoors") +
  
  # geom_sf(data = cp_grid,
  #         fill = "transparent",
  #         color = "#35596c",
  #         linewidth = 0.2) +
  # coord_sf(expand = FALSE) +
  
  stat_density_2d_filled(data = squirrels,
                         aes(x = x, y = y),
                         alpha = 0.7, #0.2
                         show.legend = TRUE,
                         bins = 4) +
  
  # c("#dae6f0", "#fffb9c", "#61b69f", "#007c8b")
  # c("#ebebeb", "#edf8b1", "#7fcdbb", "#2c7fb8") latest
  scale_fill_manual(values = c("#dae6f0", "#fffb9c", "#61b69f", "#007c8b"),
                    labels = c("", "", "", "")) +
  
  guides(fill = guide_legend(title = "Point Density",
                             ncol = 4)) +
  
  xlim(bbox$xmin, bbox$xmax) +
  ylim(c(bbox$ymin, bbox$ymax)) + 
  
  theme_void() +
  
  theme(
    panel.border = element_rect(color = "gray", linewidth = 0.5, fill = "transparent"),
    legend.direction = "vertical",
    legend.box = "vertical",            
    legend.key.height = unit(0.75, "cm"),    
    legend.key.width = unit(0.75, "cm"), 
    legend.title.align = 0.5,
    legend.position = c(0.05, 0.65),
    legend.justification = c(0, 1),
    plot.background = element_rect(fill = "lightblue"),
    legend.spacing.x = unit(0.05, 'cm'))

map_cont
  
map_new <- map_cont +

 geom_point(
    data = squirrels,
    aes(x, y),  # use color = primary_fur_color
    size = 0.005) +
  
  #Annotation

  geom_textbox(aes(x = -73.979,
                   y = 40.782,
                   label = "Highest squirrel densities here",
                   halign = 0.5),
               fill = NA,
               color = "red",
               size = 3.5,
               box.color = NA,
               width = unit(1.2, "inch")) +
  geom_segment(aes(x = -73.979,
                   y = 40.7808,
                   xend = -73.97,
                   yend = 40.775),
               arrow = arrow(length = unit(0.5, 'cm')),
               color = "red", linewidth = 1) +

  # Main title
  geom_richtext(aes(x = -73.975,
                    y = 40.7975,
                    label = "<span style='font-size: 25px'>The 2018</span><br><span style='color:#1a237e'>Squirrel</span><br>Census",
                    lineheight = 1.0, hjust = 0.5),
                color = "gray30",
                size = 11,
                fontface = "bold",
                label.color = NA,
                show.legend = FALSE,
                label.r = unit(0, "lines")) +
  
  
  
  annotation_scale(aes(style = "ticks",
                       line_col = "gray30",
                       text_col = "gray30"),
                   pad_y = unit(0.2, "in"),
                   pad_x = unit(0.2, "in")) +
  
  geom_textbox(aes(x = -73.956,
                   y = 40.762,
                   label = "**Data:** Squirrel Census | **Design:** Benjamin Tjepkes **Prompt:** Tidy Tuesday 2023"), 
                fill = NA,
                color = "gray30",
                size = 2.5,
                box.color = NA,
               width = unit(2.5, "inch"))

map_new

map_w_table <- map_new + inset_element(p = tbl_image,
                                       left = 0.55,
                                       bottom = 0.1,
                                       right = 0.99,
                                       top = 0.45,
                                       clip = TRUE, on_top = TRUE) +
  theme(
    plot.background = element_rect(fill = "transparent",
                                   color = "transparent"))

map_w_table


# Saving Plots =======================================================

# Creare a subfolder in the main directory to store plots
dir.create("plots")

# Save the contour map
ggsave(filename = "plots/map_contour_20230610.png",
       plot = map_cont)

# Save the new map
ggsave(filename = "plots/map_new_20230611.png",
       plot = map_new)

# Save the new map with table (width = 10, units = "cm")
ggsave(filename = "plots/map_withtable_20230612-4.png",
       plot = map_w_table,
       dpi = 500)

image_read(path = "map_contour.png") %>% image_rotate(-29)


# State Data ==========================================================

# library(rnaturalearth)
# 
# us_states <- ne_states(country = "united states of america",
#                        returnclass = "sf")
# 
# ny_state <- subset(us_states, name == "New York")
# 
# st_transform(ny_state, 4326)
# 
# ny_state_clipped <- st_intersection(ny_state, bbox_converted)
# 
# 
# 
# library(maps)
# 
# ny_map <- maps::map(database = "state", regions = "new york")
# 
# ny_sf <- st_as_sf(ny_map, crs = 4326)
# 
# st_transform(ny_sf, 4326)
# 
# ny_sf_clipped <- st_intersection(ny_sf, bbox_converted)


us_shapefile <- sf::st_read("C:/Users/Benjamin Tjepkes/OneDrive/G_GIS/USA_Counties.shp")

ny_shapefile <- us_shapefile %>% filter(STATE_NAME == "New York")

nycity_shapefile <- ny_shapefile %>% filter(NAME == "New York")


nycity_shapefile_clipped <- st_intersection(nycity_shapefile,
                                            bbox_converted)


# Lake Data ===========================================================

library(osmdata)

water_features <- bbox_new %>% 
  opq() %>% 
  add_osm_feature(key = "water") %>% 
  osmdata_sf()

# Table Testing =======================================================

# This section creates a test table from the iris dataset
# Using the gt package, then exports that as a PNG file

# library(gt)
# library(webshot2)
# library(png)
# 
# # Create a sample gt table
# my_table <- gt(data = iris[1:3, 1:3])
# 
# # Save the table as an image
# gtsave(my_table, file = "my_table.png")
# 
# tbl_image <- readPNG("my_table.png", native = TRUE)

# Road data ===========================================================

library(osmdata)

# Load in highway features
road_features <- bbox %>% 
  opq() %>% 
  add_osm_feature(key = "highway") %>% 
  osmdata_sf()

road_features_main <- road_features$osm_lines %>% 
  filter(highway %in% c("motorway", "trunk", "primary", "secondary"))

# Have a look at the hwy features
ggplot() + geom_sf(data = road_features_main)

# Stamen tiles ========================================================

library(tidyterra)
library(maptiles)

bbox_sf_w_crs <- sf::st_set_crs(bbox_sf, 4326)

bbox_sf_w_crs <- st_transform(bbox_sf_w_crs, "EPSG:3857")

basemap <- maptiles::get_tiles(x = bbox_sf,
                               provider = "Stamen.Terrain",
                               project = FALSE)
  
stamen_data <- get_stamenmap(cp_borders, zoom = 14, maptype = "terrain-background") 
p <- ggmap(stamen_data, darken = c(0.1, "black")) + theme_void()

