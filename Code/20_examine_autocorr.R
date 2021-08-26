## Purpose: Quantify autocorrelation in the residuals of the PHS models.
## Project: 2020_Fires_GEB
## Upstream: data_prep.R
## Downstream: (none?)

library(tidyverse)
library(brms)
library(sf)
library(tictoc)
library(cmdstanr)
library(here)
library(ncf)
library(furrr)


data_dir = readLines(here("data_dir.txt"), n=1) # The location of root of the data directory on your computer should be specified in "data_dir.txt" (ignored by git)
source(here("Code/00_shared_functions_and_globals.R")) # This defines the function `datadir()` which makes it easy to refer to data that is stored outside the repo
# Here's an example: d = st_read(datadir("CleanData/grid_90_clean"))



## Read in subset of clean data
#### eventually this will be the full dataset or thinned for spatial autocorr
shp = read_sf(datadir("CleanData/grid_thinned_interpWeather_10.gpkg"))

coords = st_coordinates(shp)
shp$x = coords[,1]
shp$y = coords[,2]

d = st_drop_geometry(shp) %>% 
  rename_all(tolower) %>% 
  dplyr::select(grid_id, fire_na, ecrgn_s, tslf:covrtyp, evt_phy, cwhr_gp,
                dnbr:rdnbr_h, bi:fm1000, ads_mort = m__12_1, x,y)

## Scale variables (some we may want to transfor first (e.g., ads_mort))
scale2 <- function(x, na.rm = TRUE) (x - mean(x, na.rm = na.rm))
scale3 <- function(x, na.rm = TRUE) ((x - mean(x, na.rm = na.rm))/sd(x,na.rm=na.rm))
# ds = mutate_at(d, .vars = c('tslf', 'ads_mort', 'bi', 'fm1000'), 
#                function(x) log(x + 0.01)) %>% 
#   mutate_at(.vars = c('tslf', 'mcc_fri', 'bi', 'windspd', 'vpd', 
#                       'rhmax', 'rhmin', 'erc', 'fm100', 'fm1000', 'ads_mort'), 
#             scale2)

# what veg types to keep? Keep those that have at least 1% of the data points
cwhr_count = table(d$cwhr_gp) %>% sort(decreasing=TRUE)
thresh = .01 * nrow(d)
cwhr_keep = names(cwhr_count)[cwhr_count > thresh]



## subsetting by veg type to keep simple for now
# rf = filter(ds, cwhr_gp %in% c("Red fir"))
# ypmc = filter(ds, cwhr_gp %in% c("Yellow pine", "Mixed conifer"))
veg5 = filter(d, cwhr_gp %in% cwhr_keep) %>% 
  mutate_at(.vars = c('tslf', 'ads_mort', 'bi', 'fm1000'), 
            function(x) log(x + 0.01)) %>% 
  mutate_at(.vars = c('tslf', 'mcc_fri', 'bi', 'windspd', 'vpd', 
                      'fm1000', 'ads_mort'), 
            scale2) %>%
  mutate_at(.vars = c('x','y'), 
            scale3)

### Prep adjacency matrix for CAR model
# grid = veg5[,c("x","y")]
# dists = dist(grid) %>% as.matrix %>% round
# K <- nrow(grid)
# W <- array(0, c(K, K))
# W[dists == 900] <- 1 
# 
# ## remove data points that have no neighbors
# nneighbs = rowSums(W)
# hasneighbs = nneighbs != 0
# W2 = W[hasneighbs,hasneighbs]
# veg5_hasneighbs = veg5[hasneighbs,]


## Fit baseline fire model

tic()
bm_fire = brm(rdnbr_h ~ tslf + ads_mort + vpd + windspd + fm1000 +
                (tslf + ads_mort + vpd + windspd + fm1000 | fire_na) +
                (1 | cwhr_gp),
              family = bernoulli("logit"), 
              data = veg5,
              chains = 2, cores = 2,
              backend = "cmdstanr")
toc()
saveRDS(bm_fire, datadir("StatModels/bm_fire.rds"))

# # Try to add conditional autoregressive (took too much memory and time to even prep the model for compiling)
# bm_fire_car = brm(rdnbr_h ~ tslf + ads_mort + vpd + windspd + fm1000 + 
#                 (tslf + ads_mort + vpd + windspd + fm1000 | fire_na) +
#                 car(W2),
#               family = bernoulli("logit"), 
#               data = veg5_hasneighbs,
#               data2 = list(W2=W2),
#               chains = 2, cores = 2,
#               backend = "cmdstanr")






resid = resid(bm_fire)


veg5$resid = resid[,1]


## pull in orig x and y
veg5$x_noscale = filter(d, cwhr_gp %in% cwhr_keep) %>% pull(x)
veg5$y_noscale = filter(d, cwhr_gp %in% cwhr_keep) %>% pull(y)



## get correllogram, by fire

make_correllogram = function(fire_na_foc) {
  
  # d_foc = veg5
  d_foc = veg5 %>%
    filter(fire_na == fire_na_foc)
  
  d_foc = d_foc %>%
    sample_n(min(500,nrow(d_foc)))
  
  corr = spline.correlog(x = d_foc$x_noscale, y = d_foc$y_noscale, z = d_foc$resid, resamp = 1000, xmax = 20000)
  #corr2 = correlog(x = d_foc$x, y = d_foc$y, z = d_foc$resid, increment=100, resamp = 1000)
  
  plot(corr)
  
  boot_x = corr$boot$boot.summary$predicted$x %>% t %>% as.data.frame
  boot_y = corr$boot$boot.summary$predicted$y %>% t %>% as.data.frame
  
  names(boot_x) = "dist"
  names(boot_y) = paste0("bound_",names(boot_y))
  
  corr_df_foc = cbind((boot_x), (boot_y)) %>%
    mutate(dist = dist/1000) %>%
    mutate(fire = fire_na_foc)
}

fire_nas = table(veg5$fire_na) %>% sort(decreasing = TRUE)
fire_nas_foc = fire_nas[1:10]
fire_nas_foc = names(fire_nas_foc)

plan(multisession, workers = 3)
corr_df = future_map_dfr(fire_nas_foc ,make_correllogram, .options=future_options(scheduling=Inf))
# corr_df = make_correllogram("blah")

corr_df_plot = corr_df %>%
  filter(dist >= 0.9)


p = ggplot(corr_df_plot,aes(x = dist,y = bound_0.5)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_ribbon(aes(ymin=bound_0.025, ymax = bound_0.975), alpha = 0.2) +
  geom_line() +
  facet_wrap(~fire) +
  #scale_y_continuous(limits = c(-1,1)) +
  theme_bw() +
  labs(x = "Distance (km)", y = "Correlation (Moran's I)")
p

