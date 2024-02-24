library(tidyverse)
library(here)
library(glmmTMB)
library(AICcmodavg)
library(broom.mixed)
library(gt)
library(gtExtras)
library(lmtest)

#load the tidy fish data
load(here("data", "net_list.Rdata")) # or here("analysis", "01_tidy_data.R") %>% source()


compare_models <- function(df) {
  
  #create model list
  model.list <- list(
    glmmTMB(total ~ year + slogyday + slogyday2 + veg + (1|site), data = df, family = nbinom2),
    glmmTMB(total ~ year + slogyday + slogyday2 + veg + scale(X500m) + (1|site), data = df, family = nbinom2),
    glmmTMB(total ~ year + slogyday + slogyday2 + veg + scale(X1.2km) + (1|site), data = df, family = nbinom2),
    glmmTMB(total ~ year + slogyday + slogyday2 + veg + scale(X10km) + (1|site), data = df, family = nbinom2),
    glmmTMB(total ~ year + slogyday + slogyday2 + veg + ipa + (1|site), data = df, family = nbinom2),
    glmmTMB(total ~ year + slogyday + slogyday2 + veg + scale(X500m) + ipa + (1|site), data = df, family = nbinom2),
    glmmTMB(total ~ year + slogyday + slogyday2 + veg + scale(X1.2km) + ipa + (1|site), data = df, family = nbinom2),
    glmmTMB(total ~ year + slogyday + slogyday2 + veg + scale(X10km) + ipa + (1|site), data = df, family = nbinom2)
  )
  
  #create a table to store model information and provide column names
  model_form <- c("base", 
                  "+ X500m", "+ X1.2km", "+ X10km",
                  "+ shore type",  
                  "+ X500m + shore type", "+ X1.2km + shore type", "+ X10km + shore type")
  
  mod.tab <- aictab(cand.set = model.list, modnames = model_form)
  
  mod.tab2 <- data.frame(model_form)
  
  # Define columns for storing values
  mod.tab2$rest.eff <- mod.tab2$armored.eff <- mod.tab2$eff.size.se <- mod.tab2$eff.size  <- mod.tab$dAICc <- mod.tab$AICc <- NA
  
  for(i in 1:nrow(mod.tab)){
    mod.tab2$AICc[i] <- round(AICc(model.list[[i]]),2)
  }
  
  # Calculate delta AIC
  mod.tab2$dAICc <- mod.tab2$AICc - min(mod.tab2$AICc)
  
  # add effect size estimate
  for (i in c(2:4, 6:8)) {
    tmp <- broom.mixed::tidy(model.list[[i]])
    mod.tab2$eff.size[i] <- round(tmp$estimate[8], 2)
    mod.tab2$eff.size.se[i] <- round(tmp$std.error[8], 2)
  }

  # add armored ipa effect size estimates
  for (i in 1:8) {
    tmp <- broom.mixed::tidy(model.list[[i]])
    mod.tab2$armored.eff[i] <- round(as.numeric(tmp[which(tmp$term == "ipaArmored"), "estimate"]), 2)
  }

  for (i in 1:8) {
    tmp <- broom.mixed::tidy(model.list[[i]])
    mod.tab2$rest.eff[i] <- round(as.numeric(tmp[which(tmp$term == "ipaRestored"), "estimate"]), 2)
  }
  
  mod.tab2 <- mod.tab2 %>% arrange(dAICc)
  
  mod.tab <- cbind(mod.tab, mod.tab2[2:5])
  
  return(mod.tab)
}

## summary stats - these are used in tables 1 & 2
compare_models(net_list$chinook)
compare_models(net_list$chum)
compare_models(net_list$herring)
compare_models(net_list$smelt)

## inspect effect sizes for shoreline type parameter in "best" chum model
summary(glmmTMB(total ~ year + slogyday + slogyday2 + veg + ipa + (1|site), data = net_list$chum, family = nbinom2))


