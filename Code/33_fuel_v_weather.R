## Purpose: Parse out the relative effect of fuels and weather for individual fires
## Author: Zack Steel
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

#### The way I have this set up is plotted values are relative to the average point in the dataset
#### e.g. a positive wind speed "importance" value means the fire's windspd * effect is greater  than average

## read in model
m = read_rds("Models/bm_fire.rds")

## get median conditions of each fire
d = m$data %>% 
  dplyr::select(-rdnbr_h) %>% 
  pivot_longer(cols = tslf:fm1000, names_to = "term") %>% 
  group_by(fire_na, term) %>% 
  summarise(mean = mean(value),
            median = median(value),
            .groups = "drop")

## bring in fire area and severity data
da = read.csv('RawData/fires_2020_summary.csv') %>% 
  mutate(ha = GIS_ACRES * 0.404686) %>% 
  dplyr::select(fire_na = FIRE_NAME, ha)

sev2 = read_csv("Results/sample_phs.csv")

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

## calculate raw parameter estimates for each fire
re_fire2 = group_by(re_fire, term, fire_na) %>% 
  median_qi(f_value, .width = .9)

write_csv(re_fire2, 'FixedEsts_fires.csv')

## put together and get distribution of median marginal effect
d2 = merge(re_fire, d) %>% 
  group_by(term, fire_na) %>% 
  median_qi(ep_median = f_value * median, .width = .9) %>% 
  pivot_wider(id_cols = c(fire_na), names_from = term, 
              values_from = c(ep_median, .lower, .upper))

## just pull median values 
d2.med = dplyr::select(d2, fire_na:ep_median_windspd)

## Keep it long for univariate comparisons
d3 = merge(re_fire, d) %>% 
  group_by(term, fire_na) %>% 
  median_qi(ep_median = f_value * median, .width = .9) %>% 
  merge(da) %>% 
  merge(sev2) %>%
  arrange(ha) %>% 
  mutate(lab = case_when(term == 'vpd' ~ 'Vapor\nPressure\nDeficit',
                         term == 'ads_mort' ~ 'Drought\nMortality',
                         term == 'windspd' ~ 'Wind\nSpeed',
                         term == 'fm1000' ~ '1000 hr\nFuels',
                         term == 'tslf' ~ 'Time Since\nLast Fire')) %>% 
  ## Get fire rank for each term
  group_by(term) %>% 
  arrange(term, ep_median) %>% 
  mutate(rank = row_number(),
         lab = factor(lab, 
                           c('Vapor\nPressure\nDeficit',
                             'Time Since\nLast Fire',
                             'Wind\nSpeed',
                             'Drought\nMortality',
                             '1000 hr\nFuels')),
         sig = ifelse((.lower < 0 & .upper < 0) |
                        (.lower > 0 & .upper > 0), 1, 0),
         flab = ifelse(sig == 1, fire_na, ""))

## Simplify 
d.out = dplyr::select(d3, fire = fire_na, ha, phs, term, lab, rank, median = ep_median, 
                      lower95 = .lower, upper95 = .upper) %>% 
  mutate_at(vars(median, lower95, upper95), exp) %>% 
  mutate_if(is.numeric, round, 3)

write_csv(d.out, 'Results/fires_oddsratio.csv')

## Build facets individually
f1 = function(pdat) {
  ggplot(pdat, aes(x = rank, y = ep_median, ymin = .lower, ymax = .upper,
             group = fire_na, fill = phs, point_size = ha, label = flab)) +
    ## Having trouble modifying legend for pointinterval; omitting
    geom_pointinterval(interval_size = 0.5, interval_alpha = 0.25,
                       pch = 21, show.legend = c(point_size = F, size = F)) +
    geom_text_repel(size = 2, box.padding = 0.5, max.overlaps = Inf) +
    scale_fill_continuous_sequential("BurgYl",
                                     name = "Percent\nHigh-severity",
                                     breaks = c(.25, .5, .75),
                                     labels = c(25, 50, 75),
                                     alpha = 0.8) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    geom_hline(yintercept = 0, lty = 2, alpha = 0.75) +
    xlim(0, 36) + ylab(NULL) + xlab(NULL)
}
  
## Time since last fire
p.a = filter(d3, term == "tslf") %>% 
  f1() +
  coord_cartesian(ylim = c(-0.75, 0.75)) +
  scale_y_continuous(breaks = c(log(0.5), 0, log(2)),
                     labels = c(0.5 , 1, 2)) +
  xlab('Time Since Last Fire - Effect Rank')

## Drought mortality
p.b = filter(d3, term == "ads_mort") %>% 
  f1() +
  xlab("Drought Mortality - Effect Rank") +
  coord_cartesian(ylim = c(-0.15, 0.4)) +
  scale_y_continuous(breaks = c(log(0.8), 0, log(1.25)),
                     labels = c(0.8 , 1, 1.25))

## VPD
p.c = filter(d3, term == "vpd") %>% 
  f1() +
  coord_cartesian(ylim = c(-2.5, 2)) +
  scale_y_continuous(breaks = c(log(0.25), 0, log(5)),
                     labels = c(0.25 , 1, 5)) +
    xlab("Vapor Pressure Deficit - Effect Rank") 

## 1000 hr fuels
p.d = filter(d3, term == "fm1000") %>% 
  f1() + 
  coord_cartesian(ylim = c(-2.5, 2)) +
  scale_y_continuous(breaks = c(log(0.25), 0, log(5)),
                     labels = c(0.25 , 1, 5)) +
  xlab("1000-hr Fuel Moisture - Effect Rank")

## Wind speed
p.e = filter(d3, term == "windspd") %>% 
  f1() +
  coord_cartesian(ylim = c(-1, 6)) +
  scale_y_continuous(breaks = c(0, log(5), log(50), log(500)),
                     labels = c(1, 5, 50, 500)) +
  xlab("Wind Speed - Effect Rank")

## Patch it up  
p = p.a + p.b + 
  p.c + ylab("Relative Effect Size\n(odds ratio)") + p.d + 
  p.e + guide_area() +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') +
  plot_layout(nrow = 3, guides = "collect") &
  theme(legend.direction = "horizontal")

save_plot("Figures/FireEffects.png", p,
          base_width = 9, base_height = 7)

library(DT)
mutate_if(d3, is.numeric, round, 4) %>% 
  datatable(filter = "top")

