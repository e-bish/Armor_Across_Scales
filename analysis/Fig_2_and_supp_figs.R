library(tidyverse)
library(here)
library(PNWColors)
library(patchwork)

#load the tidy fish data
here("analysis", "01_tidy_data.R") %>% source()

#### set plot colors ####
station_colors <- pnw_palette("Bay", 8)[c(8,5,1)]
spp_colors <- c(pnw_palette("Starfish",7)[2], 
                pnw_palette("Lake",8)[6], 
                pnw_palette("Starfish",7)[5],
                pnw_palette("Starfish",7)[7])

#### prep data ####
catch_summary <- net_all %>% 
  filter(org_type == "Fish") %>% 
  mutate(analysis_group = ifelse(species %in% c("Chinook", "Chum", "Herring", "Surf Smelt"), 
                                 "target", "nontarget")) %>% 
  group_by(analysis_group) %>% 
  summarize(total = sum(species_count)) %>% 
  mutate(prop = total/sum(total))

#total fish caught
catch_summary %>% summarize(sum(total))

#composition of target species
net_target <- net_all %>% 
  filter(species %in% c("Chinook", "Chum", "Herring", "Surf Smelt")) 

prep_df2 <- function(df) {
  unique_df<- unique(net_all[c("year", "month", "yday", "site", "ipa", "station")]) %>% 
    mutate(month = as.numeric(month)) %>%
    expand_grid() %>% 
    slice(rep(1:n(), times = 4)) %>% 
    mutate(species = rep(c("Chinook", "Chum", "Herring", "Surf Smelt"), each = 1222))
  
  output <- df %>% 
    group_by(year, month, yday, site, ipa, station, species) %>%
    summarise(total= sum(species_count)) %>% 
    ungroup() %>%
    mutate(month = as.numeric(month)) %>%
    mutate(total = as.numeric(total)) %>%
    mutate_if(is.character, as.factor) %>%
    full_join(unique_df) %>%
    mutate(ipa = replace(ipa, site == "TUR" & ipa == "Restored", "Natural")) %>% #no restoration at Turn Island
    mutate(total=replace_na(total, 0)) %>% 
    mutate(site = factor(site, levels = c("FAM", "TUR", "COR", "MA", "WA", "HO", "SHR", "DOK", "LL", "TL", "PR", "EDG"))) %>% 
    mutate(month_l = factor(month, labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sept")))
  
  return(output)
}

target_counts <- prep_df2(net_target)

#### Figure 2 ####
#CPUE by month
CPUE_month <- target_counts %>% 
  mutate(tax_group = ifelse(species %in% c("Chinook", "Chum"), "Salmon", "Forage Fish")) %>% 
  group_by(year, month_l, tax_group, species) %>% 
  summarize(effort = n(), catch = sum(total)) %>% 
  ungroup() %>% 
  mutate(CPUE = catch / effort)

sal_month_cpue <- CPUE_month %>% 
  filter(tax_group == "Salmon") %>% 
  ggplot() + 
  geom_boxplot(aes(x = factor(month_l), y = CPUE, fill = species)) +
  scale_fill_manual(values = spp_colors[1:2], ) + 
  theme_classic() + 
  labs(x = "Month", y = "Abundance") +
  guides(fill="none") + 
  theme(text = element_text(size=9))

ff_month_cpue <- CPUE_month %>% 
  filter(tax_group == "Forage Fish") %>% 
  ggplot() + 
  geom_boxplot(aes(x = factor(month_l), y = CPUE, fill = species)) +
  scale_fill_manual(values = spp_colors[3:4], ) + 
  theme_classic() + 
  labs(x = "Month", y = "") +
  guides(fill="none") + 
  theme(text = element_text(size=9))

CPUE_site <- target_counts %>% 
  mutate(tax_group = ifelse(species %in% c("Chinook", "Chum"), "Salmon", "Forage Fish")) %>% 
  group_by(year, site, tax_group, species) %>% 
  summarize(effort = n(), catch = sum(total)) %>% 
  ungroup() %>% 
  mutate(CPUE = catch / effort)

sal_site_cpue <- CPUE_site %>% 
  filter(tax_group == "Salmon") %>% 
  ggplot() +
  geom_boxplot(aes(x = site, y = CPUE, fill = species)) +
  scale_fill_manual(values = spp_colors[1:2], ) +
  theme_classic() + 
  labs(x = "", y = "Abundance", fill = "Species") + 
  theme(text = element_text(size=9))

ff_site_cpue <- CPUE_site %>% 
  filter(tax_group == "Forage Fish") %>% 
  ggplot() +
  geom_boxplot(aes(x = site, y = CPUE, fill = species)) +
  scale_fill_manual(values = spp_colors[3:4], ) +
  theme_classic() + 
  labs(x = "Site", y = "Abundance", fill = "") + 
  theme(text = element_text(size=9))

(sal_month_cpue + ff_month_cpue) / sal_site_cpue / ff_site_cpue +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A')

# ggsave("figures/Fig_2.png", width = 169, height = 135, units = "mm")

#### Supplement Figure S1 ####
CPUE_station_mo <- target_counts %>% 
  mutate(station = factor(station)) %>% 
  group_by(month, station, species) %>% 
  summarize(effort = n(), catch = sum(total)) %>% 
  ungroup() %>% 
  mutate(CPUE = catch / effort)

CPUE_station_mo %>% 
  filter(month < 8) %>% 
  ggplot() +
  geom_point(aes(x = factor(month, labels = c("April", "May", "June", "July")), 
                 y = CPUE, 
                 color = factor(station, labels = c("Shallow", "Mid", "Deep"))),
             size = 3) +
  theme_bw() +
  scale_color_manual(values = station_colors) +
  labs(x = "Month", y = "Catch per Set", color = "Station") +
  theme(text = element_text(size=16)) +
  facet_wrap(~species, scales = "free_y")

 # ggsave("figures/Fig_S1.png", width = 169, height = 150, units = "mm")

#### Supplement Figure S2 ####
#difference in length between depth stations
net_target %>% 
  mutate(station = factor(station, labels = c("Shallow", "Mid", "Deep"))) %>% 
  filter(mean_length_mm > 0) %>% 
  ggplot(aes(x = station, y = mean_length_mm, fill = station)) +
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(~species) +
  scale_fill_manual(values = station_colors) +
  theme_bw() +
  theme(text = element_text(size=16)) +
  labs(x = "Depth Station", y = "Mean Length per Set (mm)")

# ggsave("figures/Fig_S2.png")

#### Figure S3 ####
#length over the season years lumped
net_target %>% 
  mutate(site = factor(site, levels = c("FAM", "TUR", "COR", "MA", "WA", "HO", "SHR", "DOK", "LL", "TL", "PR", "EDG"))) %>% 
  filter(mean_length_mm > 0) %>% 
  ggplot(aes(x = site, y = mean_length_mm)) +
  geom_boxplot() +
  facet_wrap(~species, scales = "free_y") +
  theme_bw() +
  theme(text = element_text(size=10)) +
  labs(x = "", y = "Mean Length per Set (mm)")

# ggsave("figures/Fig_S3.png")