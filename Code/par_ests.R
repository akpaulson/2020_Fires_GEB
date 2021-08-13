## Purpose: Plot and tabulate parameter estiamtes
## Project: 2020_Fires_GEB
## Upstream: model_build.R
## Downstream: 

library(tidyverse)
library(brms)
library(tidybayes)
library(ggdist)
library(cowplot)

## Read in model
m = read_rds("Models/model0.rds")

## summarise fixed effects
## Will eventually want to do this for all veg types as well
fe_ests = m %>% 
  gather_draws(pars = `^b_.*`, regex = T) %>% 
  # gather_draws(b_Intercept, b_tslf, b_ads_mort, b_vpd, b_windspd) %>% 
  median_qi() %>% 
  arrange(.value) %>% 
  mutate(Predictor = substring(.variable, 3),
         ## factor levels by current arrangement
         Predictor = factor(Predictor, unique(Predictor)))

fe_p = filter(fe_ests, Predictor != "Intercept") %>% 
  ggplot(aes(y = Predictor, x =.value, xmin = .lower, xmax = .upper)) +
  geom_pointinterval() +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  geom_vline(xintercept = 0, lty = 2) +
  xlab("Parameter Estimate") + ylab("Predictor")

## Save 
dplyr::select(fe_ests, Predictor, Medan = .value, 
              Lower95 = .lower, Upper95 = .upper) %>% 
  write.csv("Results/FixedEsts.csv", row.names = F)
save_plot("Figures/FixedDots.png", fe_p)

## combine fixed and random to mean effects of each veg type
fe = m %>% 
  gather_draws(b_tslf, b_vpd, b_ads_mort, b_windspd) %>% 
  mutate(term = substring(.variable, 3))

re = m %>% 
  spread_draws(r_cwhr_gp[veg, term])

re2 = merge(fe, re) %>% 
  group_by(term, veg) %>% 
  median_qi(gp_value = .value + r_cwhr_gp)

#### This model only includes 5 veg types. 
#### I think there are >20 in the dataset so we'll have to figure which to show and how

re_p = ggplot(re2, aes(y = term, x = gp_value, xmin = .lower, xmax = .upper, color = veg)) +
  geom_pointinterval(position = position_dodge(width = .4)) +
  scale_color_viridis_d(option = "B", end = 0.9) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  geom_vline(xintercept = 0, lty = 2) +
  xlab("Parameter Estimate") + ylab("Predictor")

## Save 
dplyr::select(re2, Predictor = term, Vegetation = veg,
              Medan = gp_value, Lower95 = .lower, Upper95 = .upper) %>% 
  write.csv("Results/Veg_FixedEsts.csv", row.names = F)
save_plot("Figures/Veg_FixedDots.png", re_p)



#### Exploring

## Extract model draws
## https://mjskay.github.io/tidybayes/articles/tidy-brms.html
get_variables(m)

## draws of all random effects
m %>% 
  spread_draws(r_fire_na[fire,term]) %>% 
  median_qi()




