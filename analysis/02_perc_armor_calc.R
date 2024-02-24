## Percent Armor Calculations
library(here)
library(tidyverse)
library(ggnewscale)
library(ggspatial)
library(sf)
library(lwgeom)
library(units)
library(measurements)
library(leaflet)
library(htmlwidgets)

## load data and inspect

#Shorezone shoreline shapefile
shoreline <- here("data","spatial_data", "shorezone_shoreline_only", "shorezone_shoreline_only.shp") %>% 
  read_sf(crs = 2927) #Washington State Plane South (ft) / NAD83

#Beach Strategies armoring shapefile updated with armor removed at our restoration sites
armor <- here("data","spatial_data","BeachStrategiesShorelineArmor_update.shp") %>% 
  read_sf() %>% 
  st_transform(crs = 2927) %>% 
  st_zm()

#GPS locations for our survey stations with each shoreline type
SOS_sites <- here("data","spatial_data","reupdatingthearmoringgeodatabase", "Shoreline_armoring_shore_origin_sites_UTM.shp") %>% 
  read_sf() %>% #UTM zone 10 32610
  st_transform(crs = 2927) %>% #transform site coordinates into the same crs as the shoreline layer
  mutate(site = case_when(Site_name == "Dockton Park" ~ "DOK",
                          Site_name == "Cornet Bay" ~ "COR",
                          Site_name == "Edgewater Beach" ~ "EDG",
                          Site_name == "Family Tides" ~ "FAM",
                          Site_name == "Seahurst Park" ~ "SHR", 
                          Site_name == "Turn Island" ~ "TUR",
                          Site_name == "Lost Lake" ~ "LL",
                          Site_name == "Titlow Park" ~ "TL",
                          Site_name == "Penrose Point" ~ "PR",
                          Site_name == "Waterman Shoreline Preserve" ~ "WA" ,
                          Site_name == "Howarth Park" ~ "HO" ,
                          Site_name == "Maylor Point" ~ "MA"), .after = "Site_name")
# map it!
# map <- ggplot() +
#   geom_sf(data = shoreline) +
#   geom_sf(data = armor, color = "red") +
#   geom_sf(data = SOS_sites, color = "blue", cex = 2) +
#   coord_sf(xlim = c(-123.5, -122), ylim = c(47, 48.75), crs = 4326) +
#   theme(plot.background = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())
# map

################################################################################
#create site buffers and crop armor extent to buffer

#set a center point for each site 
SOS_site_cents <- SOS_sites %>%
  group_by(site) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_centroid()

#snap it to the closest shoreline
closest_points <- SOS_site_cents %>%
  rowwise() %>%
  mutate(
    nearest_segment = shoreline[st_nearest_feature(geometry,
                                               shoreline),],
    line_to_point = st_nearest_points(geometry, nearest_segment),
    closest_point = st_cast(line_to_point, 'POINT')[2],
    snapped_point_cond = st_sfc(st_geometry(closest_point),
                                crs = st_crs(shoreline)))

# ## map snapped centroids with unsnapped centroids
# SOS_sites <- st_transform(SOS_sites, crs = 4326)
# SOS_sites$lon <- st_coordinates(SOS_sites)[,1]
# SOS_sites$lat <- st_coordinates(SOS_sites)[,2]
# 
# SOS_site_cents <- st_transform(SOS_site_cents, crs = 4326)
# SOS_site_cents$lon <- st_coordinates(SOS_site_cents)[,1]
# SOS_site_cents$lat <- st_coordinates(SOS_site_cents)[,2]
# 
# SOS_site_cents_sn <- closest_points %>%
#   select(site, snapped_point_cond) %>%
#   st_drop_geometry() %>%
#   st_as_sf() %>%
#   st_transform(crs = 4326)
# 
# SOS_site_cents_sn$lon <- st_coordinates(SOS_site_cents_sn)[,1]
# SOS_site_cents_sn$lat <- st_coordinates(SOS_site_cents_sn)[,2]
# 
# snapped_site_map <- leaflet(SOS_site_cents_sn) %>%
#   addProviderTiles(providers$Esri.WorldGrayCanvas, group =  "Esri") %>%
#   setView(lng =-122.420429, lat = 47.886010, zoom = 8) %>%
#   addCircleMarkers(lng = ~lon, lat = ~lat) %>%
#   addCircleMarkers(data = SOS_site_cents, lng = ~lon, lat = ~lat, color = "purple") %>%
#   addCircleMarkers(data = SOS_sites, lng = ~lon, lat = ~lat, color = "red") %>%
#   addPolylines(data = st_transform(shoreline, crs = 4326), color = "green") %>%
  #addPolylines(data = st_transform(SOS_armored_info, crs = 4326), color = "pink")
#   

SOS_site_cents_sn <- closest_points %>% 
  select(site, snapped_point_cond) %>% 
  st_drop_geometry() %>% 
  st_as_sf()

#calculate buffers
b_100m <- st_buffer(SOS_site_cents_sn, conv_unit(100, "m", "ft")) 
b_300m <- st_buffer(SOS_site_cents_sn, conv_unit(300, "m", "ft")) 
b_500m <- st_buffer(SOS_site_cents_sn, conv_unit(500, "m", "ft"))
b_1km <- st_buffer(SOS_site_cents_sn, conv_unit(1.2, "km", "ft")) #so this is actually 1.2km to capture the armoring at COR
b_3km <- st_buffer(SOS_site_cents_sn, conv_unit(3, "km", "ft")) 
b_5km <- st_buffer(SOS_site_cents_sn, conv_unit(5, "km", "ft")) 
b_7km <- st_buffer(SOS_site_cents_sn, conv_unit(7, "km", "ft")) 
b_10km <- st_buffer(SOS_site_cents_sn, conv_unit(10, "km", "ft"))
b_15km <- st_buffer(SOS_site_cents_sn, conv_unit(15, "km", "ft"))
b_20km <- st_buffer(SOS_site_cents_sn, conv_unit(20, "km", "ft"))

#crop shoreline to buffers
s_100m <- st_intersection(shoreline, b_100m) %>% mutate(buffer = "100m")
s_300m <- st_intersection(shoreline, b_300m) %>% mutate(buffer = "300m")
s_500m <- st_intersection(shoreline, b_500m) %>% mutate(buffer = "500m")
s_1km <- st_intersection(shoreline, b_1km) %>% mutate(buffer = "1.2km")
s_3km <- st_intersection(shoreline, b_3km) %>% mutate(buffer = "3km")
s_5km <- st_intersection(shoreline, b_5km) %>% mutate(buffer = "5km")
s_7km <- st_intersection(shoreline, b_7km) %>% mutate(buffer = "7km")
s_10km <- st_intersection(shoreline, b_10km) %>% mutate(buffer = "10km")
s_15km <- st_intersection(shoreline, b_15km) %>% mutate(buffer = "15km")
s_20km <- st_intersection(shoreline, b_20km) %>% mutate(buffer = "20km")

s_buffered <- rbind(s_100m, s_300m, s_500m, s_1km, s_3km, s_5km, s_7km, s_10km, s_15km, s_20km) %>% 
  mutate(shore_length = st_length(geometry)) %>% #calculate length of shoreline within the buffer extent
  st_drop_geometry() %>%  #remove geometry so we can just work with the numbers
  select(!OBJECTID)

#crop armor data to buffers
a_100m <- st_intersection(armor, b_100m) %>% mutate(buffer = "100m")
a_300m <- st_intersection(armor, b_300m) %>% mutate(buffer = "300m")
a_500m <- st_intersection(armor, b_500m) %>% mutate(buffer = "500m")
a_1km <- st_intersection(armor, b_1km) %>% mutate(buffer = "1.2km")
a_3km <- st_intersection(armor, b_3km) %>% mutate(buffer = "3km")
a_5km <- st_intersection(armor, b_5km) %>% mutate(buffer = "5km")
a_7km <- st_intersection(armor, b_7km) %>% mutate(buffer = "7km")
a_10km <- st_intersection(armor, b_10km) %>% mutate(buffer = "10km")
a_15km <- st_intersection(armor, b_15km) %>% mutate(buffer = "15km")
a_20km <- st_intersection(armor, b_20km) %>% mutate(buffer = "20km")

a_buffered <- rbind(a_100m, a_300m, a_500m, a_1km, a_3km, a_5km, a_7km, a_10km, a_15km, a_20km) %>% 
  mutate(armor_length = st_length(geometry)) %>% #can't use the SHAPE_length attribute, because we cropped it!
  st_drop_geometry() %>% #remove geometry so we can just work with the numbers
  group_by(site, buffer) %>% 
  summarize(armor_length = sum(armor_length)) #sum feet of armoring per site

#calculate percent armor
site_armor <- inner_join(s_buffered, a_buffered) %>% 
  mutate(perc.armor = (armor_length/shore_length)*100) %>% 
  dplyr::select(-c(shore_length, armor_length)) %>% 
  mutate(perc.armor = drop_units(perc.armor)) %>% 
  mutate(perc.armor = round(perc.armor, 2)) 

################################################################################
#calculate % armor by basin

#PSNERP PS basins outline
PSNERPbasins <- here("data","spatial_data","PSNERP_PS_basins", "psnerp_oceanographic_subbasins_geo.shp") %>% 
  read_sf() %>% 
  st_transform(crs = 2927) %>% #Washington State Plane South (ft) / NAD83
  mutate(SUBBASIN = case_when(SUBBASIN == "WH|SJ" ~ "WH", #simplify designations
                              SUBBASIN == "SJ|NC" ~ "SJ",
                              TRUE ~ SUBBASIN))

#assign armor extent to basins
basin_armor <- st_intersection(armor, PSNERPbasins) %>% #this takes a long time
  group_by(SUBBASIN) %>% 
  summarize(armor_length = sum(st_length(geometry))) %>% 
  st_drop_geometry()
  
#sum shoreline by basin
basin_shoreline <- st_intersection(shoreline, PSNERPbasins) %>% 
  mutate(shore_length = st_length(geometry)) %>% 
  group_by(SUBBASIN) %>% 
  summarize(shore_length = sum(shore_length)) %>% 
  st_drop_geometry()

#convert armor extent to percent armor by basin, assign to sites
a_basin <- inner_join(basin_armor, basin_shoreline) %>% 
  mutate(perc.armor = (armor_length/shore_length)*100) %>% 
  mutate(perc.armor = drop_units(perc.armor)) %>% 
  mutate(perc.armor = round(perc.armor, 2))

site_basins <- st_intersection(SOS_site_cents_sn, PSNERPbasins) %>% 
  st_drop_geometry()

site_basin_armor <- inner_join(a_basin, site_basins) %>% 
  dplyr::select(-c(shore_length, armor_length)) %>% 
  relocate(site) %>% 
  rename("buffer"= "SUBBASIN")

###############################################################################
#format to integrate with catch data
perc_armor <- rbind(site_armor, site_basin_armor) %>%
  mutate(buffer = replace(buffer, !endsWith(buffer, "m"), "basin")) %>% 
  pivot_wider(names_from = buffer, values_from = perc.armor) %>% 
  replace(is.na(.), 0) %>% 
  arrange(site)

#write to csv
# write_csv(perc_armor, here("data","perc_armor.csv"))

###############################################################################
#same analysis as above but updating the armor extent at dockton for 2021 & 2022
#same file as above, but with the 2020 Dockton restoration segment also removed
armor.2 <- here("data","BeachStrategiesShorelineArmor_update2020.shp") %>% 
  read_sf() %>% 
  st_transform(crs = 2927) %>% 
  st_zm()

#crop armor data to buffers
a_100m <- st_intersection(armor.2, b_100m) %>% mutate(buffer = "100m")
a_300m <- st_intersection(armor.2, b_300m) %>% mutate(buffer = "300m")
a_500m <- st_intersection(armor.2, b_500m) %>% mutate(buffer = "500m")
a_1km <- st_intersection(armor.2, b_1km) %>% mutate(buffer = "1.2km")
a_3km <- st_intersection(armor.2, b_3km) %>% mutate(buffer = "3km")
a_5km <- st_intersection(armor.2, b_5km) %>% mutate(buffer = "5km")
a_7km <- st_intersection(armor.2, b_7km) %>% mutate(buffer = "7km")
a_10km <- st_intersection(armor.2, b_10km) %>% mutate(buffer = "10km")
a_15km <- st_intersection(armor.2, b_15km) %>% mutate(buffer = "15km")
a_20km <- st_intersection(armor.2, b_20km) %>% mutate(buffer = "20km")

a_buffered2 <- rbind(a_100m, a_300m, a_500m, a_1km, a_3km, a_5km, a_7km, a_10km, a_15km, a_20km) %>% 
  mutate(armor_length = st_length(geometry)) %>% #can't use the SHAPE_length attribute, because we cropped it!
  st_drop_geometry() %>% #remove geometry so we can just work with the numbers
  group_by(site, buffer) %>% 
  summarize(armor_length = sum(armor_length)) #sum feet of armoring per site

#calculate percent armor
site_armor2 <- inner_join(s_buffered, a_buffered2) %>% 
  mutate(perc.armor = (armor_length/shore_length)*100) %>% 
  dplyr::select(-c(shore_length, armor_length)) %>% 
  mutate(perc.armor = drop_units(perc.armor)) %>% 
  mutate(perc.armor = round(perc.armor, 2)) 

######calculate % armor by basin

#assign armor extent to basins
basin_armor2 <- st_intersection(armor.2, PSNERPbasins) %>% #this takes a long time
  group_by(SUBBASIN) %>% 
  summarize(armor_length = sum(st_length(geometry))) %>% 
  st_drop_geometry()

#convert armor extent to percent armor by basin, assign to sites
a_basin2 <- inner_join(basin_armor2, basin_shoreline) %>% 
  mutate(perc.armor = (armor_length/shore_length)*100) %>% 
  mutate(perc.armor = drop_units(perc.armor)) %>% 
  mutate(perc.armor = round(perc.armor, 2))


site_basin_armor2 <- inner_join(a_basin2, site_basins) %>% 
  dplyr::select(-c(shore_length, armor_length)) %>% 
  relocate(site) %>% 
  rename("buffer"= "SUBBASIN")

###############################################################################
#format to integrate with catch data
perc_armor2 <- rbind(site_armor2, site_basin_armor2) %>%
  mutate(buffer = replace(buffer, !endsWith(buffer, "m"), "basin")) %>% 
  pivot_wider(names_from = buffer, values_from = perc.armor) %>% 
  replace(is.na(.), 0) %>% 
  arrange(site)

#write to csv
# write_csv(perc_armor2, here("data","perc_armor2.csv"))
