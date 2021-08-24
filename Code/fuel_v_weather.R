## Purpose: Parse out the relative effect of fuels and weather for individual fires
## Project: 2020_Fires_GEB
## Upstream: model_build.R
## Downstream: 

library(tidyverse)
library(tidybayes)
library(brms)
library(ggdist)
library(ggrepel)
library(DT)
library(sf)
library(colorspace)
library(patchwork)
library(cowplot)

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
  pivot_longer(cols = tslf:fm1000, names_to = "term") %>% 
  group_by(fire_na, term) %>% 
  summarise(mean = mean(value),
            median = median(value),
            .groups = "drop")

## bring in fire area and severity data
da = read.csv('RawData/fires_2020_summary.csv') %>% 
  mutate(ha = GIS_ACRES * 0.404686) %>% 
  dplyr::select(fire_na = FIRE_NAME, ha)

sev = read_sf("local/grid_90_clean/grid_90_clean.shp") 
sev2 = group_by(sev, FIRE_NA) %>% 
  count(rdnbr_h) %>% 
  pivot_wider(names_from = rdnbr_h, values_from = n) %>% 
  mutate(phs = `1` / (`0` + `1`)) %>% 
  dplyr::select(fire_na = FIRE_NA, phs)
rm(sev)

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
              values_from = c(ep_median, .lower, .upper))
  # mutate(tslf_windspd = exp(ep_median_tslf) / exp(ep_median_windspd))

## Keep it long for univariate comparisons
d3 = merge(re_fire, d) %>% 
  group_by(term, fire_na) %>% 
  median_qi(ep_median = f_value * median) %>% 
  merge(da) %>% 
  merge(sev2) %>% 
  arrange(ha) %>% 
  mutate(lab = case_when(term == 'vpd' ~ 'Vapor\nPressure\nDeficit',
                         term == 'ads_mort' ~ 'Drought\nMortality',
                         term == 'windspd' ~ 'Wind\nSpeed',
                         term == 'fm1000' ~ '1000 hr\nFuels',
                         term == 'tslf' ~ 'Time Since\nLast Fire'))
d3$fire_na = factor(d3$fire_na, unique(d3$fire_na))

## find fires to label
## ggrepel doesn't seem to work with dodge
filter(d3, term == 'windspd') %>% 
  arrange(ep_median) %>% 
  tail
filter(d3, term == "vpd") %>% 
  arrange(ep_median) %>% 
  tail

pw = filter(d3, term %in% c('windspd', 'vpd', 'fm1000')) %>% 
  ggplot(aes(y = lab, x = ep_median, 
                    xmin = .lower, xmax = .upper,
                    group = fire_na, fill = phs,
                    point_size = ha)) +
  ## Having trouble modifying legend for pointinterval; omitting
  geom_pointinterval(position = position_dodge(width = .6),
                     interval_size = 0.5, interval_alpha = 0.5,
                     pch = 21,
                     show.legend = c(point_size = F, size = F)) +
  scale_fill_continuous_sequential("BurgYl",
                                   name = "Percent\nHigh-severity",
                                   breaks = c(.25, .5, .75),
                                   labels = c(25, 50, 75),
                                   alpha = 0.8) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  geom_vline(xintercept = 0, lty = 2, alpha = 0.75) +
  coord_cartesian(xlim = c(-2.5, 5.5)) +
  scale_x_continuous(breaks = c(log (0.25), 0, log(5), log(50)),
                     labels = c(0.25, 1, 5, 50)) +
  xlab("Relative effect (odds ratio)") + ylab(NULL) +
  annotate("text",
           x = c(0.85, 1.2, 5.2,
                 -0.95, 1.5), 
           y = c(3.35, 3.1, 3.2,
                 2.35, 2.3),
           label = c("North", "River", 'CZU',
                     'Creek', 'Hennessey'),
           size = 3)

## find fires to label
## ggrepel doesn't seem to work with dodge
filter(d3, term %in% c('ads_mort')) %>% 
  arrange(desc(ha)) %>% 
  head
filter(d3, term == "tslf") %>% 
  arrange(ep_median) %>% 
  slice(c(1, nrow(.)))
filter(d3, term == "tslf") %>% 
  arrange(desc(ha)) %>% 
  head

pf = filter(d3, term %in% c('tslf', 'ads_mort')) %>% 
  ggplot(aes(y = lab, x = ep_median, 
             xmin = .lower, xmax = .upper,
             group = fire_na, label = fire_na,
             fill = phs,
             point_size = ha)) +
  geom_pointinterval(position = position_dodge(width = .6),
                     interval_size = 0.5, interval_alpha = 0.5,
                     pch = 21,
                     show.legend = c(point_size = F, size = F)) +
  scale_fill_continuous_sequential("BurgYl",
                                   name = "Percent\nHigh-severity",
                                   breaks = c(.25, .5, .75),
                                   labels = c(25, 50, 75),
                                   alpha = 0.8) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  geom_vline(xintercept = 0, lty = 2, alpha = 0.75) +
  coord_cartesian(xlim = c(-1.5, 1.5)) +
  scale_x_continuous(breaks = c(log (0.25), log(0.5), 0, log(2), log(5)),
                     labels = c(0.25, 0.5, 1, 2, 5)) +
  xlab("Relative effect (odds ratio)") + ylab(NULL) + 
  annotate("text", 
           x = c(-1.1, -0.4, 0.25, 1, 
                 .3, .35), 
           y = c(2.25, 2.4, 2.42, 1.9, 
                 1.125, 1.35), 
           label = c('Dolan', 'SCU', 'August', 'Hog', 
                     'Castle', 'Creek'),
           size = 3)

## patch and save
layout = '
AABB
AABB
AABB
AACC'
out = pw + pf + guide_area() +
  plot_layout(design = layout, guides = 'collect') &
  theme(legend.direction = "horizontal")

save_plot("Figures/FireEffects.png", out,
          base_width = 8, base_height = 6)


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
  # ylim(-3, 2.5) + xlim(-3.5, 2.5) +
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

group_by(re_fire, term, fire_na) %>% 
  summarise(ep_median = median(f_value)) %>% 
  filter(term == "ads_mort") %>% 
  merge(d) %>% 
  ggplot(aes(y = median, x = ep_median, label = fire_na)) +
  geom_text_repel(size = 2) +
  stat_pointinterval()
