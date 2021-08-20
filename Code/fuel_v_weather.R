## Purpose: Parse out the relative effect of fuels and weather for individual fires
## Project: 2020_Fires_GEB
## Upstream: model_build.R
## Downstream: fuel_v_weather.R

library(tidyverse)
library(tidybayes)
library(brms)
library(ggdist)
library(ggrepel)
library(DT)

#### Currently the way I have this set up is relative to the average point in the dataset
#### e.g. a positive wind speed "importance" value means the fire's windspd * effect is greater  than average
#### Another way to approach this could be to make it relative to some historic standard
#### doable for TSLF, VPD and windspeed we'd have to get historic averages
#### also need to make sure I'm using the right scale (ie. odds vs. log-odds, vs. probability)


## read in model
m = read_rds("Models/model0_fires.rds")

## get median conditions of each fire
d = m$data %>% 
  dplyr::select(#-cwhr_gp, 
                -rdnbr_h) %>% 
  pivot_longer(cols = tslf:windspd, names_to = "term") %>% 
  group_by(fire_na, term) %>% 
  summarise(mean = mean(value),
            median = median(value),
            .groups = "drop")
  

## combine fixed and random to mean effects of each fire
fe = m %>% 
  gather_draws(b_tslf, b_vpd, b_ads_mort, b_windspd, b_fm1000) %>% 
  mutate(term = substring(.variable, 3))

r_fire = m %>% 
  spread_draws(r_fire_na[fire_na, term])

re_fire = merge(fe, r_fire) %>% 
  mutate(f_value = .value + r_fire_na,
         fire_na = str_replace_all(fire_na, pattern = "[.]", replacement = " ")) %>% 
  dplyr::select(term, fire_na, f_value)

## put together and get distribution of median marginal effect
d2 = merge(re_fire, d) %>% 
  group_by(term, fire_na) %>% 
  median_qi(ep_median = f_value * median) %>% 
  pivot_wider(id_cols = c(fire_na), names_from = term, 
              values_from = c(ep_median, .lower, .upper)) %>% 
  mutate(tslf_windspd = exp(ep_median_tslf) / exp(ep_median_windspd))

## tslf vs. windspd
p = ggplot(d2, aes(y = ep_median_tslf, x = ep_median_windspd, 
                   ymin = .lower_tslf, ymax = .upper_tslf,
                   xmin = .lower_windspd, xmax = .upper_windspd,
                   label = fire_na)) +
  geom_text_repel(size = 2) +
  geom_pointinterval(interval_alpha = 0.3,
                     interval_size = 2) +
  geom_pointinterval(orientation = "horizontal",
                     point_size = 2,
                     interval_alpha = 0.3,
                     interval_size = 2) +
  geom_abline(lty = 2) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  ylab("TSLF Importance") + xlab("Wind Speed Importance") 

save_plot("Figures/fuel_wind.png", p,
          base_width = 8, base_height = 6)

## tslf vs. vpd
p2 = ggplot(d2, aes(y = ep_median_tslf, x = ep_median_vpd, 
                   ymin = .lower_tslf, ymax = .upper_tslf,
                   xmin = .lower_vpd, xmax = .upper_vpd,
                   label = fire_na)) +
  geom_text_repel(size = 2) +
  geom_pointinterval(interval_alpha = 0.3,
                     interval_size = 2) +
  geom_pointinterval(orientation = "horizontal",
                     point_size = 2,
                     interval_alpha = 0.3,
                     interval_size = 2) +
  geom_abline(lty = 2) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  ylab("TSLF Importance") + xlab("VPD Importance") 

## ads vs. windspd
p3 = ggplot(d2, aes(y = ep_median_ads_mort, x = ep_median_windspd, 
                   ymin = .lower_ads_mort, ymax = .upper_ads_mort,
                   xmin = .lower_windspd, xmax = .upper_windspd,
                   label = fire_na)) +
  geom_text_repel(size = 2) +
  geom_pointinterval(interval_alpha = 0.3,
                     interval_size = 2) +
  geom_pointinterval(orientation = "horizontal",
                     point_size = 2,
                     interval_alpha = 0.3,
                     interval_size = 2) +
  geom_abline(lty = 2) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  ylab("ADS Importance") + xlab("Wind Speed Importance") 

## ads vs. vpd
p4 = ggplot(d2, aes(y = ep_median_ads_mort, x = ep_median_vpd, 
                    ymin = .lower_ads_mort, ymax = .upper_ads_mort,
                    xmin = .lower_vpd, xmax = .upper_vpd,
                    label = fire_na)) +
  geom_text_repel(size = 2) +
  geom_pointinterval(interval_alpha = 0.3,
                     interval_size = 2) +
  geom_pointinterval(orientation = "horizontal",
                     point_size = 2,
                     interval_alpha = 0.3,
                     interval_size = 2) +
  geom_abline(lty = 2) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  ylim(-3, 2.5) + xlim(-3.5, 2.5) +
  ylab("ADS Importance") + xlab("VPD Importance") 

dplyr::select(d2, fire_na, tslf_windspd) %>% 
  arrange(tslf_windspd) %>% 
  print(n = Inf)

## Table of median values
arrange(d, median) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(term = as.factor(term)) %>% 
  datatable(filter = "top")

group_by(re_fire, term, fire_na) %>% 
  summarise(ep_median = median(f_value)) %>% 
  filter(term == "windspd") %>% 
  merge(d) %>% 
  ggplot(aes(y = median, x = ep_median, label = fire_na)) +
  geom_text_repel(size = 2) +
  stat_pointinterval()

group_by(re_fire, term, fire_na) %>% 
  summarise(ep_median = median(f_value)) %>% 
  filter(term == "tslf") %>% 
  merge(d) %>% 
  ggplot(aes(y = median, x = ep_median, label = fire_na)) +
  geom_text_repel(size = 2) +
  stat_pointinterval()

group_by(re_fire, term, fire_na) %>% 
  summarise(ep_median = median(f_value)) %>% 
  filter(term == "vpd") %>% 
  merge(d) %>% 
  ggplot(aes(y = median, x = ep_median, label = fire_na)) +
  geom_text_repel(size = 2) +
  stat_pointinterval()
