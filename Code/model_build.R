## Purpose: Build Pr. HS models with fuels and weather as predictors
## Project: 2020_Fires_GEB
## Upstream: data_prep.R
## Downstream: par_ests.R; fuel_v_weather.R

library(tidyverse)
library(brms)
library(sf)
library(tictoc)
library(cmdstanr)

## Read in subset of clean data
#### eventually this will be the full dataset or thinned for spatial autocorr
shp = read_sf("InProcessData/clean_sub.shp")

d = st_drop_geometry(shp) %>% 
  rename_all(tolower) %>% 
  dplyr::select(grid_id, fire_na, ecrgn_s, tslf:covrtyp, evt_phy, cwhr_gp,
                dnbr:rdnbr_h, bi:fm1000, ads_mort = m__12_1)

## Scale variables (some we may want to transfor first (e.g., ads_mort))
scale2 <- function(x, na.rm = TRUE) (x - mean(x, na.rm = na.rm))
ds = mutate_at(d, .vars = c('tslf', 'ads_mort', 'bi'), 
               function(x) log(x + 0.01)) %>% 
  mutate_at(.vars = c('tslf', 'mcc_fri', 'bi', 'windspd', 'vpd', 
                      'rhmax', 'rhmin', 'erc', 'fm100', 'fm1000', 'ads_mort'), 
            scale2)

## subsetting by veg type to keep simple for now
rf = filter(ds, cwhr_gp %in% c("Red fir"))
ypmc = filter(ds, cwhr_gp %in% c("Yellow pine", "Mixed conifer"))
veg5 = filter(d, cwhr_gp %in% c("Yellow pine", "Mixed conifer", "Red fir",
                                 "Lowland chaparral", "Redwood")) %>% 
  mutate_at(.vars = c('tslf', 'ads_mort', 'bi'), 
            function(x) log(x + 0.01)) %>% 
  mutate_at(.vars = c('tslf', 'mcc_fri', 'bi', 'windspd', 'vpd', 
                      'rhmax', 'rhmin', 'erc', 'fm100', 'fm1000', 'ads_mort'), 
            scale2)

## playing with bayesian model structures

## basic fixed effects with varying intercepts by fire
# bm_rf = brm(rdnbr_h ~ (tslf + vpd) + (1|fire_na),
#             family = bernoulli("logit"), 
#             data = rf)
# 
# ## tslf and vpd vary by fire
# bm_rf2 = brm(rdnbr_h ~ (tslf + vpd) | fire_na,
#             family = bernoulli("logit"), 
#             data = rf)
# 
# ## global effects with varying offsets for each fire
# bm_ypmc = brm(rdnbr_h ~ tslf + ads_mort + vpd + windspd + fm1000 + 
#                 (0 + tslf + ads_mort + vpd + windspd + fm1000 | fire_na),
#              family = bernoulli("logit"), 
#              data = ypmc,
#              chains = 2, cores = 2)

## allow everything to vary by fire ID and veg type
#### ~35 min with 13K points; all fires and 5 veg types
#### 1% divergences w/ adapt_delta @ 0.8
tic()
bm = brm(rdnbr_h ~ tslf + ads_mort + vpd + windspd + fm1000 + 
           (0 + tslf + ads_mort + vpd + windspd + fm1000 | fire_na) +
           (0 + tslf + ads_mort + vpd + windspd + fm1000 | cwhr_gp),
              family = bernoulli("logit"), 
              data = veg5,
              chains = 2, cores = 2,
         backend = "cmdstanr")
toc()

## allow tslf and vpd/wind to interact. 
## Does this tell us something about fuels vs. weather-driven fires?
# bm_ypmc2 = brm(rdnbr_h ~ tslf*vpd + tslf*windspd + ads_mort + 
#                 (tslf*vpd + tslf*windspd + ads_mort | fire_na),
#               family = bernoulli("logit"), 
#               data = ypmc,
#               chains = 2, cores = 2)

## Save model for later
write_rds(bm, "Models/model0.rds")

