library(here)
library(tidyverse)
library(sf)
library(measurements)

#updated Beach Strategies layer post 2020 dockton restoration
armor.2 <- here("data","spatial_data","BeachStrategiesShorelineArmor_update2020.shp") %>% 
  read_sf() %>% 
  st_transform(crs = 2927) %>% 
  st_zm()

#Shorezone shoreline shapefile
shoreline <- here("data", "spatial_data", "shorezone_shoreline_only", "shorezone_shoreline_only.shp") %>% 
  read_sf(crs = 2927) #Washington State Plane South (ft) / NAD83

#polygon covering the east side
east_side_crop <- here("data","spatial_data","east_side_crop.shp") %>% 
  read_sf() %>% 
  st_transform(crs = 2927) #Washington State Plane South (ft) / NAD83

#crop armor and shoreline objects to east side of the sound
shoreline_east <- st_intersection(shoreline, east_side_crop)
armor_east <- st_intersection(armor.2, east_side_crop)

#create a point for the mouth of the Puyallup River
Puy <- data.frame(lat = 47.268641, long = -122.427106) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(crs = 2927)

b_5km <- st_buffer(Puy, conv_unit(5, "km", "ft"))

#check how it looks
ggplot() +
  geom_sf(data = shoreline_east, color = "black") +
  geom_sf(data = armor_east, color = "red", size = 1) +
  geom_sf(data = b_5km) +
  geom_sf(data = Puy, size = 3, color = "blue") +
  theme_bw()

armor_5km <- st_intersection(armor_east, b_5km)
shoreline_5km <- st_intersection(shoreline_east, b_5km)

# % armored for the next 5km of shoreline north of the Puyallup River on the east side
sum(st_length(armor_5km)) / sum(st_length(shoreline_5km))

# % armored along the entire east side up to Deception Pass
sum(st_length(armor_east)) / sum(st_length(shoreline_east))
