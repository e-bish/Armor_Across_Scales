library(tidyverse)
library(here)
library(glmmTMB)
library(DHARMa)
library(dotwhisker)
library(PNWColors)
library(sjPlot)

#load the tidy fish data
load(here("data", "net_list.Rdata")) # or here("analysis", "01_tidy_data.R") %>% source()

spp_colors <- c(pnw_palette("Starfish",7)[2], 
                pnw_palette("Lake",8)[6], 
                pnw_palette("Starfish",7)[5],
                pnw_palette("Starfish",7)[7])

chinook_mod <- glmmTMB(total ~ year + slogyday + slogyday2 + veg + scale(X500m) + (1|site), data = net_list$chinook, family = nbinom2)
chum_mod <- glmmTMB(total ~ year + slogyday + slogyday2 + veg + ipa + (1|site), data = net_list$chum, family = nbinom2)
herring_mod <- glmmTMB(total ~ year + slogyday + slogyday2 + veg + scale(X10km) + (1|site), data = net_list$herring, family = nbinom2)
smelt_mod <- glmmTMB(total ~ year + slogyday + slogyday2 + veg + (1|site), data = net_list$smelt, family = nbinom2)


check_residuals <- function(fittedModel) {
  simulationOutput <- simulateResiduals(fittedModel = fittedModel, plot = F)
  plot(simulationOutput)
}

check_residuals(chinook_mod)
check_residuals(chum_mod)
check_residuals(herring_mod)
check_residuals(smelt_mod)


chin <- broom.mixed::tidy(chinook_mod) %>% mutate(model = "Chinook")
chu <- broom.mixed::tidy(chum_mod) %>%  mutate(model = "Chum")
her <- broom.mixed::tidy(herring_mod) %>%  mutate(model = "Herring")
sme <- broom.mixed::tidy(smelt_mod) %>% mutate(model = "Surf Smelt")

tab_model(chinook_mod, chum_mod, herring_mod, smelt_mod, 
          dv.labels = c("Chinook", "Chum", "Herring", "Surf Smelt"), 
          pred.labels = c(#`(Intercept)` = "Intercept",
            `slogyday` = "Day of year (log)", 
            `slogyday2` = "Day of year (log) squared",
            `year2019` = "2019",
            `year2021` = "2021",
            `year2022` = "2022",
            `ipaRestored` = "Restored shoreline", 
            `ipaArmored` = "Armored shoreline",
            `vegPresent` = "Eelgrass present",
            `scale(X500m)` = "% armor in 500m radius",
            `scale(X10km)` = "% armor in 10km radius"),
          show.intercept = FALSE,
          transform = NULL,
          show.se = TRUE,
          show.ci = FALSE,
          show.p = FALSE,
          # to better distinguish the model columns
          CSS = list(
            modelcolumn1 = "background-color: #f0f0f0;", 
            modelcolumn3 = "background-color: #f0f0f0;"
          ))


all.models <- bind_rows(chin, chu, her, sme) %>% 
  mutate(term = ifelse(grepl("X", term) | term == "scale(basin)", "% Armor", term)) %>% 
  relabel_predictors(c(`scale(slogyday)` = "Day of year (log)", 
                       `I(slogyday2)` = "Day of year (log) squared",
                       `year2019` = "2019",
                       `year2021` = "2021",
                       `year2022` = "2022",
                       `sd__(Intercept)` = "Random Effects", 
                       `ipaRestored` = "Restored shoreline", 
                       `ipaArmored` = "Armored shoreline",
                       `vegPresent` = "Eelgrass present")) %>% 
  mutate(conf.low = estimate - std.error) %>% 
  mutate(conf.high = estimate + std.error) %>% 
  filter(!effect == "ran_pars")

all.models %>% 
  dwplot(dot_args = list(aes(colour = model)), size = 8,
         vars_order = c("Day of year (log)", "Day of year (log) squared", "2019", 
                        "2021", "2022", "Restored shoreline", "Armored shoreline", 
                        "Eelgrass present", "% Armor")) +
  geom_vline(xintercept = 0, linetype="dashed", 
             color = "grey", linewidth =1) +
  labs(x = "Coefficient Estimate", 
       color = "Species") +
  scale_color_manual(values = spp_colors) +
  theme_classic() +
  theme(text = element_text(size = 9))

# ggsave(here("figures", "Fig_S4.png"), width = 6, height = 4)


