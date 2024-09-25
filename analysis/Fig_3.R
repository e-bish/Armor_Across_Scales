library(tidyverse)
library(here)

perc_armor <- here("data", "perc_armor.csv") %>% 
  read_csv() %>% 
  pivot_longer(cols = !site, 
               names_to = "scale",
               values_to = "perc.armor") %>% 
  mutate(scale = factor(scale,
                        levels = c("100m", "300m", "500m", "1.2km", 
                                   "3km", "5km", "7km", "10km", "15km", 
                                   "20km", "basin"))) %>% 
  mutate(shape = ifelse(scale %in% c("500m", "1.2km", "10km"), "triangle", "circle"))


perc_armor2 <- here("data", "perc_armor2.csv") %>% 
  read_csv() %>% 
  pivot_longer(cols = !site, 
               names_to = "scale",
               values_to = "perc.armor") %>% 
  mutate(scale = factor(scale,
                        levels = c("100m", "300m", "500m", "1.2km", 
                                   "3km", "5km", "7km", "10km", "15km", 
                                   "20km", "basin"))) %>% 
  mutate(shape = ifelse(scale %in% c("500m", "1.2km", "10km"), "triangle", "circle")) 

## some stats
perc_armor2 %>% 
  group_by(site) %>% 
  summarize(mean.armor = mean(perc.armor), sd.armor = sd(perc.armor)) %>% 
  arrange(mean.armor)

perc_armor %>% 
  filter(scale == "500m") %>% 
  filter(perc.armor == max(perc.armor) | perc.armor == min(perc.armor))

perc_armor %>% 
  filter(scale == "1.2km") %>% 
  filter(perc.armor == max(perc.armor) | perc.armor == min(perc.armor))

perc_armor %>% 
  filter(scale == "10km") %>% 
  filter(perc.armor == max(perc.armor) | perc.armor == min(perc.armor))


## the plot
ggplot(data = perc_armor, 
       aes(x = scale, y =perc.armor, shape = shape)) + 
  geom_point(size = 2.5, color = 
              "blue") +
  geom_point(data = perc_armor2,
             aes(x = scale, y =perc.armor,shape = shape), size = 2.6) +
  scale_shape_manual(values = c(19, 2)) +
  facet_wrap(~ factor(site,
                      levels = c("FAM", "TUR", "COR", "MA", "WA", "HO", "SHR", "DOK", "LL", "TL", "PR", "EDG"),
                      labels = c("Family Tides", "Turn Island", "Cornet Bay", "Maylor Point", "Waterman Preserve",
                      "Howarth Park", "Seahurst Park", "Dockton Park", "Lost Lake", "Titlow Park", "Penrose Ponit", "Edgewater Beach"))) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.5), 
        legend.position = "none",
        text = element_text(size = 9)) + 
  xlab("Spatial Scale") +
  ylab("% of Shoreline with Armor")

# ggsave("figures/Fig_3.tiff", width = 6.5, height = 5.19, units = "in")

