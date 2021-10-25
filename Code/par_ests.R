## Purpose: Plot and tabulate parameter estimates for the veg model
## Project: 2020_Fires_GEB
## Upstream: model_build.R
## Downstream: 

library(tidyverse)
library(brms)
library(tidybayes)
library(ggdist)
library(cowplot)

## Read in model
m = read_rds("Models/bm_veg.rds")

## summarise fixed effects
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
  median_qi(gp_value = .value + r_cwhr_gp) %>% 
  mutate(lab = case_when(term == 'vpd' ~ 'Vapor\nPressure\nDeficit',
                         term == 'ads_mort' ~ 'Drought\nMortality',
                         term == 'windspd' ~ 'Wind\nSpeed',
                         term == 'fm1000' ~ '1000 hr\nFuels',
                         term == 'tslf' ~ 'Time Since\nLast Fire'),
         Vegetation = str_replace_all(veg, pattern = "[.]", replacement = " "))

re_p = ggplot(re2, aes(y = lab, x = gp_value, xmin = .lower, xmax = .upper, 
                       color = Vegetation)) +
  geom_pointinterval(position = position_dodge(width = .8)) +
  scale_color_viridis_d(option = "B", end = 0.9) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  guides(color = guide_legend(reverse=TRUE)) +
  geom_vline(xintercept = 0, lty = 2) +
  xlab("Parameter Estimate") + ylab(NULL)

## Save 
dplyr::select(re2, Predictor = term, Vegetation,
              Medan = gp_value, Lower95 = .lower, Upper95 = .upper) %>% 
  write.csv("Results/Veg_FixedEsts.csv", row.names = F)
save_plot("Figures/Veg_FixedDots.png", re_p,
          base_height = 6, base_width = 6)




