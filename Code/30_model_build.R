## Purpose: Build Pr. HS models with fuels and weather as predictors
## Author: Zack Steel
## Upstream: data_prep.R
## Downstream: par_ests.R; fuel_v_weather.R

library(tidyverse)
library(brms)
library(sf)
library(tictoc)
library(cmdstanr)

## Read in data (will need to download from OSF)
# shp = read_sf("local/grid_thinned_interpWeather_10.gpkg")
shp = read_sf("CleanData/grid_90_clean_interpWeather.shp")

d = st_drop_geometry(shp) %>% 
  rename_all(tolower) %>% 
  dplyr::select(grid_id, fire_na, ecrgn_s, tslf:covrtyp, evt_phy, cwhr_gp,
                dnbr:rdnbr_h, bi:fm1000, ads_mort = m__12_1)

# what veg types to keep? Keep those that have at least 1% of the data points
cwhr_count = table(d$cwhr_gp) %>% sort(decreasing=TRUE)
thresh = .01 * nrow(d)
cwhr_keep = names(cwhr_count)[cwhr_count > thresh]

## Scale variables (some we may want to transfor first (e.g., ads_mort))
scale2 <- function(x, na.rm = TRUE) (x - mean(x, na.rm = na.rm)) / 
  sd(x, na.rm = na.rm)

ds = mutate_at(d, 
               .vars = c('tslf', 'ads_mort', 'bi', 'fm1000', 'windspd'), 
                           function(x) log(x + 0.01)) %>%
  mutate_at(.vars = c('tslf', 'mcc_fri', 'bi', 'windspd', 'vpd',
                      'fm1000', 'ads_mort'),
            scale2) %>% 
  filter(cwhr_gp %in% cwhr_keep)


## allow everything to vary by fire ID; veg type varying intercepts
## ~38 min with 18.5K points; all fires and major veg types
tic()
bm = brm(rdnbr_h ~ tslf + ads_mort + vpd + windspd + fm1000 + 
           (tslf + ads_mort + vpd + windspd + fm1000 | fire_na) +
           (1 | cwhr_gp),
              family = bernoulli("logit"), 
              data = ds,
              chains = 4, cores = 4,
         backend = "cmdstanr")
toc()

## Add original data to the model object for later use
d_orig = filter(d, cwhr_gp %in% cwhr_keep)
bm$data2 = d_orig

saveRDS(bm, "Models/bm_fire.rds")

## Everything varying by veg type
## ~28 min
tic()
bm_veg = brm(rdnbr_h ~ tslf + ads_mort + vpd + windspd + fm1000 + 
           (1 | fire_na) +
           (tslf + ads_mort + vpd + windspd + fm1000 | cwhr_gp),
         family = bernoulli("logit"), 
         data = ds,
         chains = 4, cores = 4,
         backend = "cmdstanr")
toc()


## Add original data to the model object for later use
bm_veg$data2 = d_orig

write_rds(bm_veg, "Models/bm_veg.rds")

## Read back in to get Bayes r2
bm = read_rds('Models/bm_fire.rds')
bayes_R2(bm) #0.22

bm_veg = read_rds('Models/bm_veg.rds')
bayes_R2(bm_veg) #0.20
