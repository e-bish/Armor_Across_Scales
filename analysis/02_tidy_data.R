library(tidyverse)
library(here)
library(lubridate)
library(GGally)


#### Load data ####
# all of the raw data were downloaded from the survey team's shared google drive and saved as csv files
net_import <- here::here("data","raw_data", "raw.18.19.csv") %>% read_csv()
net_import2 <- here::here("data","raw_data", "raw.21.csv") %>% read_csv()
net_import3 <- here::here("data","raw_data", "raw.22.csv") %>% read_csv()

perc_armor <- here("data", "perc_armor.csv") %>% read_csv()
perc_armor2 <- here("data", "perc_armor2.csv") %>% #this file reflects the restoration completed at Dockton in 2020
  read_csv()

names(perc_armor) <- c("site", "X100m", "X300m", "X500m", "X1.2km", "X3km", "X5km", "X7km", "X10km", "X15km", "X20km", "basin")
names(perc_armor2) <- c("site", "X100m", "X300m", "X500m", "X1.2km", "X3km", "X5km", "X7km", "X10km", "X15km", "X20km", "basin")

#### Prepare data ####
net_2018.19  <- net_import %>%
  mutate(year = as.numeric(year)) %>%
  mutate(month = str_pad(month, 2, side = c("left"), pad = "0")) %>%
  mutate(day = str_pad(day, 2, side = c("left"), pad = "0")) %>%
  mutate(tax_group = replace(tax_group, species == "Tube Snout", "Aulorhynchus")) %>% 
  mutate(species = replace(species, species == "Gunnel Sp", "UnID Gunnel")) %>% 
  mutate(species = replace(species, species == "Jellyfish Sp", "UnID Jellyfish")) %>% 
  mutate(ipa = as.factor(ipa))   %>%
  mutate(species = replace_na(species, "none")) %>%
  group_by(year, month, day, site, ipa, station, org_type, tax_group,species) %>% 
  mutate(count = 1) %>%
  summarize(species_count=sum(count), mean_length_mm = mean(length_mm, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(mean_length_mm = replace_na(mean_length_mm, 0))

net_2021 <- net_import2 %>%
  mutate(month = str_pad(month, width = 2, pad = "0")) %>% 
  mutate(day = str_pad(day, width = 2, pad = "0")) %>% 
  mutate(species = ifelse(is.na(species), "none", species)) %>% 
  group_by(year, month, day, site, ipa, station, org_type, tax_group, species) %>%
  summarize(species_count = n(), mean_length_mm = mean(length_mm, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(mean_length_mm = replace_na(mean_length_mm, 0)) %>% 
  mutate(ipa = ifelse(site == "TL" & ipa == "Restored", "Natural", ipa)) %>% #fix Titlow misdesignations
  mutate(ipa = ifelse(site == "TL" & ipa == "Armored", "Restored", ipa)) %>% 
  mutate(ipa = ifelse(site == "TL" & ipa == "Armored_2", "Armored", ipa))

net_2022 <- net_import3 %>%
  mutate(month = str_pad(month, width = 2, pad = "0")) %>% 
  mutate(day = str_pad(day, width = 2, pad = "0")) %>% 
  mutate(species = ifelse(is.na(species), "none", species)) %>% 
  group_by(year, month, day, site, ipa, station, org_type, tax_group, species) %>%
  summarize(species_count = n(), mean_length_mm = mean(length_mm, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(mean_length_mm = replace_na(mean_length_mm, 0))

#create a data frame with all four years of data combined and a term for calendar day of year
net_all <- bind_rows(net_2018.19, net_2021, net_2022) %>% 
  mutate(date = paste(day, month, year, sep="-")) %>% 
  mutate(date = dmy(date)) %>% 
  mutate(yday = yday(date)) 

#### prepare data for analysis ####
prep_df <- function(df) {
  unique_df<- unique(net_all[c("year", "month", "yday","site","ipa")])
  all.group <- expand_grid(unique_df) %>% mutate(month = as.numeric(month))
  
  output <- df %>% 
    group_by(year, month, yday, site, ipa) %>%
    summarise(total= sum(species_count)) %>% #sum across depth stations
    ungroup( ) %>%
    mutate(month = as.numeric(month)) %>%
    mutate(total = as.numeric(total)) %>%
    mutate_if(is.character, as.factor) %>%
    merge(all.group, all=TRUE) %>%
    mutate(ipa = replace(ipa, site == "TUR" & ipa == "Restored", "Natural")) %>% #no restoration at Turn Island
    mutate(total=replace_na(total, 0)) %>% 
    mutate(veg = recode(site, 
                        'COR' = "Present", 
                        'TUR'="Present", 
                        'FAM'="Absent", 
                        'DOK'="Absent", 
                        'EDG'="Absent", 
                        'SHR'="Present", 
                        'HO' = "Present", 
                        'LL' = "Absent", # overall absent though armored site may have had some eelgrass, based on field notes
                        'MA' = "Absent", 
                        'PR' = "Present",
                        'TL' = "Present", 
                        'WA' = "Present")) %>% 
    mutate(slogyday = scale(log(yday)), .after = yday) %>% 
    mutate(slogyday2 = slogyday^2, .after = slogyday) %>% 
    mutate(year = as.factor(year)) %>% 
    mutate(ipa = fct_relevel(ipa, c("Natural", "Armored", "Restored"))) %>% 
    mutate(veg = fct_relevel(veg, c("Absent", "Present"))) 
  
  return(output)
}


add_perc_armor <- function(df){
  phase1 <- df %>% 
    filter(year == 2018 | year == 2019) %>% 
    inner_join(perc_armor)
  
  phase2 <- df %>% 
    filter(year == 2021 | year == 2022) %>% 
    inner_join(perc_armor2)
  
  output <- rbind(phase1, phase2)
  
  return(output)
}

#### Data list ####
#create a list of data for each of the focal species to run through the model

net_list <- list(chinook = net_all %>% 
                   filter(species == "Chinook") %>% 
                   prep_df() %>% 
                   add_perc_armor(), 
                 chum = net_all %>% 
                   filter(species == "Chum") %>% 
                   prep_df() %>% 
                   add_perc_armor(), 
                 herring = net_all %>% 
                   filter(species == "Herring") %>% 
                   prep_df() %>% 
                   add_perc_armor(), 
                 smelt = net_all %>% 
                   filter(species == "Surf Smelt") %>% 
                   prep_df() %>% 
                   add_perc_armor())

#test multicollinearity
select_vars <- function (x) {
  df <- x %>% 
    select(year, slogyday, slogyday2, veg, ipa, X500m, X1.2km, X10km) %>% 
    mutate(across(everything(), as.vector))
  return(df)
}

chinook_vars <- select_vars(net_list$chinook)
chum_vars <- select_vars(net_list$chum)
herring_vars <- select_vars(net_list$herring)
smelt_vars <- select_vars(net_list$smelt)

ggpairs(chinook_vars)
ggpairs(chum_vars)
ggpairs(herring_vars)
ggpairs(smelt_vars)

save(net_list, file = here("data", "net_list.Rdata"))

