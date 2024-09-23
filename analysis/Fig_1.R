library(tidyverse)
library(here)
library(sf)
library(spData)
library(ggspatial)
library(cowplot)

# devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)


#### load data ####
shoreline <- here("data", "spatial_data", "WA_state_outline", "WA_state_outline.shp") %>% 
  read_sf()

sf_use_s2(FALSE) #to create centroids for the polygons

PSNERPbasins <- here("data","spatial_data","psnerp_basins_full.shp") %>% 
  read_sf() 

PSNERPcentroids <- PSNERPbasins %>%
  st_centroid()

PSNERPcentroids1 <- PSNERPcentroids %>%
  filter(!SUBBASIN %in% c("NC","SJ")) %>%
  mutate(Basin = case_when(SUBBASIN == "SC" ~ "South Central\nPuget Sound",
                           SUBBASIN == "SP" ~ "South Puget Sound",
                           SUBBASIN == "WH" ~ "Whidbey\nBasin",
                           SUBBASIN == "HC" ~ "Hood\nCanal",
                           SUBBASIN == "SF" ~ "Strait of\nJuan de Fuca"))

PSNERPcentroids2 <- PSNERPcentroids %>%
  filter(SUBBASIN %in% c("NC","SJ")) %>%
  mutate(Basin = case_when(SUBBASIN == "NC" ~ "North Central\nPuget Sound",
                           SUBBASIN == "SJ" ~ "San Juan\nIslands")) %>% 
  st_drop_geometry() %>% 
  mutate(lat = c(48.12631, 48.47924),
         lon = c(-122.72644, -123.11819)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

#GPS locations for our survey stations with each ipa
SOS_sites <- here("data","spatial_data","reupdatingthearmoringgeodatabase","Shoreline_armoring_shore_origin_sites_UTM.shp") %>% 
  read_sf() %>% #UTM zone 10 32610
  st_transform(crs = 4326) %>% #transform site coordinates into the same crs as the shoreline layer
  mutate(Site_name = ifelse(Site_name == "Waterman Shoreline Preserve", "Waterman Preserve",Site_name)) %>% 
  mutate(site = case_when(Site_name == "Dockton Park" ~ "DOK",
                          Site_name == "Cornet Bay" ~ "COR",
                          Site_name == "Edgewater Beach" ~ "EDG",
                          Site_name == "Family Tides" ~ "FAM",
                          Site_name == "Seahurst Park" ~ "SHR", 
                          Site_name == "Turn Island" ~ "TUR",
                          Site_name == "Lost Lake" ~ "LL",
                          Site_name == "Titlow Park" ~ "TL",
                          Site_name == "Penrose Point" ~ "PR",
                          Site_name == "Waterman Preserve" ~ "WA" ,
                          Site_name == "Howarth Park" ~ "HO" ,
                          Site_name == "Maylor Point" ~ "MA"), .after = "Site_name") %>% 
  mutate(veg = recode(site, 
                      'COR' = "Present", 
                      'TUR'="Present", 
                      'FAM'="Absent", 
                      'DOK'="Absent", 
                      'EDG'="Absent", 
                      'SHR'="Present", 
                      'HO' = "Present", 
                      'LL' = "Absent", 
                      'MA' = "Absent", 
                      'PR' = "Present",
                      'TL' = "Present", 
                      'WA' = "Present")) %>% 
  st_zm() %>% 
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>% 
  filter(IPA == "Natural")

#### create the inset ####
bbox <- st_bbox(SOS_sites)

inset <- ggplot() +
  geom_sf(data = us_states, fill = "grey70") +
  geom_sf(data = bbox %>% st_as_sfc(), fill = "red", 
          color = "red", linewidth = 1, alpha = 0.4) +
  coord_sf(xlim = c(-124.5, -114), ylim = c(32.5, 49)) + 
  theme_void() +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", 
                                    fill=NA, linewidth = 1.5)) 

#### create the main plot ####
#commented out values work well on some computers
p1 <- ggplot() + 
  geom_sf(data = shoreline, fill = "grey70", color = "black") + 
  geom_sf(data = PSNERPbasins, fill = "transparent", color = "grey40") +
  geom_sf(data = SOS_sites,
          aes(color = veg, fill = veg), pch = 18, size = 6) + #size = 7
  scale_color_manual(values = c("black", "chartreuse4")) +
  geom_sf_text(data = PSNERPcentroids1, aes(label = Basin), size = 2.5, fontface = "italic", color = "grey30",
               hjust = c(0.8,0.5,3.25,1.5,-.75), #left to right #c(0.7,0.5,3.5,1.5,-1)
               vjust = c(0.3,0.5,2.2,1,1.8)) + #c(0.3,1,2.2,1,1.8)) 
  geom_sf_text_repel(data = PSNERPcentroids2, aes(label = Basin), size = 2.5, fontface = "italic", color = "grey30", #size = 3
                     nudge_x = ifelse(PSNERPcentroids2$SUBBASIN == "SJ", -0.3, -.25),
                     nudge_y = ifelse(PSNERPcentroids2$SUBBASIN == "SJ", -0.05, 0.06)) +
  geom_sf_label_repel(data = SOS_sites,
                      aes(x = lon, y = lat, label = site),
                      force = 5, force_pull = 5, size = 5) + #size = 6
  theme_void() +
  coord_sf(xlim = c(-123.5, -121.8), ylim = c(46.98, 48.7),
           crs = 4326, expand = FALSE) +
  theme(panel.background = element_rect(fill = "white"), 
        legend.position = "none",
        panel.border = element_rect(color = "black", 
                                    fill=NA, linewidth = 1.5),
        text = element_text(size = 9)) +
  annotation_scale(location = "br", style = "ticks", height = unit(0.20, "cm"),) + #0.25
  annotation_north_arrow(location = "br", which_north = "true",
                         height = unit(1.5, "cm"),
                         pad_y = unit(.6, "cm"),
                         pad_x = unit(0.3, "cm"), #0.5
                         style = north_arrow_fancy_orienteering)

ggdraw() + 
  draw_plot(p1) + 
  draw_plot(inset, x = 0.67, y = 0.69, # x = 0.62, y = 0.69, 
            width = 0.3, height = 0.3)

#### save results ####

ggsave("figures/Fig_1.tiff", 
       width = 98, 
       height = 130, 
       units = "mm", 
       device = "tiff", dpi = 600)  



