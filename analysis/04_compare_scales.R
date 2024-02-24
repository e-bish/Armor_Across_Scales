library(tidyverse)
library(here)
library(glmmTMB)
library(scales)
library(PNWColors)

#load the tidy fish data
load(here("data", "net_list.Rdata")) # or here("analysis", "01_tidy_data.R") %>% source()

spp_colors <- c(pnw_palette("Starfish",7)[2], 
                pnw_palette("Lake",8)[6], 
                pnw_palette("Starfish",7)[5],
                pnw_palette("Starfish",7)[7])

compare_armor_extent <- function(df) {
  
  #create model list
  model.list <- list(
    glmmTMB(total ~ year + slogyday + slogyday2 + veg + scale(X100m) + ipa +  (1|site), data = df, family = nbinom2),
    glmmTMB(total ~ year + slogyday + slogyday2 + veg + scale(X300m) + ipa +  (1|site), data = df, family = nbinom2),
    glmmTMB(total ~ year + slogyday + slogyday2 + veg + scale(X500m) + ipa +  (1|site), data = df, family = nbinom2),
    glmmTMB(total ~ year + slogyday + slogyday2 + veg + scale(X1.2km) + ipa +  (1|site), data = df, family = nbinom2),
    glmmTMB(total ~ year + slogyday + slogyday2 + veg + scale(X3km) + ipa +  (1|site), data = df, family = nbinom2),
    glmmTMB(total ~ year + slogyday + slogyday2 + veg + scale(X5km) + ipa +  (1|site), data = df, family = nbinom2),
    glmmTMB(total ~ year + slogyday + slogyday2 + veg + scale(X7km) + ipa +  (1|site), data = df, family = nbinom2),
    glmmTMB(total ~ year + slogyday + slogyday2 + veg + scale(X10km) + ipa +  (1|site), data = df, family = nbinom2),
    glmmTMB(total ~ year + slogyday + slogyday2 + veg + scale(X15km) + ipa + (1|site), data = df, family = nbinom2),
    glmmTMB(total ~ year + slogyday + slogyday2 + veg + scale(X20km) + ipa + (1|site), data = df, family = nbinom2),
    glmmTMB(total ~ year + slogyday + slogyday2 + veg + scale(basin) + ipa + (1|site), data = df, family = nbinom2)
  )
  
  #create a table to store model information and provide column names
  armor_extent <- c("X100m", "X300m", "X500m", "X1.2km", "X3km", "X5km", "X7km", "X10km", "X15km", "X20km", "basin")
  mod.tab <- data.frame(armor_extent)
  
  model.results <- lapply(model.list, broom.mixed::tidy)
  
  for (i in 1:length(model.results)) {
    mod.tab$est[i] <- model.results[[i]][8,5] %>% unlist()
    mod.tab$se[i] <- model.results[[i]][8,6] %>% unlist()
    mod.tab$CI_low[i] <- confint(model.list[[i]])[8,1] %>% unname()
    mod.tab$CI_high[i] <- confint(model.list[[i]])[8,2] %>% unname()
    mod.tab$error_low <- mod.tab$est - mod.tab$se
    mod.tab$error_high <-  mod.tab$est + mod.tab$se
  }
  
  return(mod.tab)
}

#create a separate data frame with effect sizes for each scale:species
chinook_eff <- compare_armor_extent(net_list$chinook) %>% 
  mutate(species = "Chinook")

chum_eff <- compare_armor_extent(net_list$chum) %>% 
  mutate(species = "Chum")

herring_eff <- compare_armor_extent(net_list$herring) %>% 
  mutate(species = "Herring")

smelt_eff <- compare_armor_extent(net_list$smelt) %>% 
  mutate(species = "Surf Smelt")

all_eff <- rbind(chinook_eff, chum_eff, herring_eff, smelt_eff) %>% 
  transform(species = factor(species, levels = c("Chinook", "Chum", 
                                                 "Herring", "Surf Smelt")))

#### Figure 4 ####
ggplot(all_eff, aes(x=factor(armor_extent, 
                             level = c("X100m", "X300m", "X500m", "X1.2km", "X3km", "X5km", "X7km", "X10km", "X15km", "X20km", "basin"),
                             label = c("100m", "300m", "500m", "1.2km", "3km", "5km", "7km", "10km", "15km", "20km", "basin")), 
                    y=est, color = species)) + 
  geom_point() +
  geom_errorbar(aes(ymin=est-se, ymax=est+se), width=.3, linewidth = 1) +
  scale_color_manual(values = spp_colors) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "grey27") +
  geom_vline(xintercept = which(all_eff$armor_extent == "X1.2km"), color = "grey", linetype = "dashed") +
  labs(x = "Spatial Scale", y = "Effect size") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.5),
        legend.position = "none",
        text =  element_text(size = 9)) + 
  facet_wrap(. ~ species, ncol = 2, scales = "free_y") 


 # ggsave("figures/Fig_4.png", width = 169, height = 120, units = "mm")  
