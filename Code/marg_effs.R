## Purpose: Build marginal effects plots 
## Project: 2020_Fires_GEB
## Upstream: model_build.R
## Downstream:

library(tidyverse)
library(tidybayes)
library(brms)
library(ggdist)
library(modelr)
library(cowplot)
library(patchwork)

## read in model
m = read_rds("Models/bm_veg.rds")

scale2 <- function(x, na.rm = TRUE) (x - mean(x, na.rm = na.rm)) / 
  sd(x, na.rm = na.rm)

m$data2 = mutate(m$data2, 
            tslf_l = log(tslf + 0.01),
            ads_l = log(ads_mort + 0.01),
            fm1000_l = log(fm1000 + 0.01))

## What is the median fire in terms of phs?
# mfire = read_csv("Results/sample_phs.csv") %>% 
#   filter(phs == median(phs)) %>% 
#   pull(fire_na)

## set up plotting function
f1 <- function(x_var, #model variable
               x_var_orig, #unstandardized, but transformed
               x_seq, #unstandardized scale
               exp_x, #do we need to exponentiate x for plotting?,
               veg_show #which veg types to highlight?
               ) {
  ## set up new data, gotta be an easier way to fill "typical" values
  newdata0 <- data.frame(tslf = 0, ads_mort = 0, vpd = 0, windspd = 0, fm1000 = 0,
                         # fire_na = mfire, #Using median fire in terms of phs
                         cwhr_gp = c({{veg_show}}, "new")) %>% 
    ## drop variable to be expanded below
    dplyr::select(-{{x_var}})
  
          ## do the same for all veg types
          # newdata0_all = data.frame(tslf = 0, ads_mort = 0, vpd = 0, windspd = 0, fm1000 = 0,
          #                          fire_na = mfire,
          #                          cwhr_gp = unique(m$data2$cwhr_gp)) %>% 
          #   ## drop variable to be expanded below
          #   dplyr::select(-{{x_var}})
  
  ## get range of values for user-defined variable
  ## Note the use of "glue syntax" and "embracing" https://dplyr.tidyverse.org/articles/programming.html
  
  d_sum <- summarize(m$data2, 
                      sd = sd({{x_var_orig}}, na.rm = T),
                      mn = mean({{x_var_orig}}, na.rm = T))
  
  ## all combos of variable sequence and veg groups
  newdata <- data_grid(m$data,
                       .x_us = x_seq,
                       # "{{x_var}}" := seq_range({{x_var}}, n = 100),
                       cwhr_gp = c({{veg_show}})) %>%
    mutate({{x_var}} := (.x_us - d_sum$mn) / d_sum$sd) %>%
  left_join(newdata0, by = "cwhr_gp") %>%
  ## Get fitted draws (expected predictions)
  add_epred_draws(m, ndraws = 500,
                  re_formula = ~ (tslf + ads_mort + vpd + windspd + fm1000 | cwhr_gp),
                  allow_new_levels = T) 
          # 
          #   # for all
          #   newdata_all <- data_grid(m$data,
          #                        .x_us = x_seq,
          #                        cwhr_gp = unique(m$data2$cwhr_gp)) %>%
          #     mutate({{x_var}} := (.x_us - d_sum$mn) / d_sum$sd) %>%
          #     left_join(newdata0_all, by = "cwhr_gp") %>%
          #     ## Get fitted draws (expected predictions)
          #     add_epred_draws(m, ndraws = 100) %>%
          #     # group_by(cwhr_gp) %>%
          #     median_qi()
  
  if(exp_x == T) {
    newdata$.x_us = exp(newdata$.x_us)
    # newdata_all$.x_us = exp(newdata_all$.x_us)
  }

  ggplot(newdata, aes(x = .x_us, y = .epred, color = cwhr_gp,
                          fill = cwhr_gp, group = cwhr_gp)) +
    # geom_line(data = newdata_all, inherit.aes = F,
    #           aes(x = .x_us, y = .epred, group = cwhr_gp),
    #           color = "grey60", alpha = 0.85) +
    stat_lineribbon(.width = 0.95, size = 1.5, alpha = .5) +
    scale_fill_brewer(name = "Vegetation", palette = "Set2") +
    scale_color_brewer(name = "Vegetation", palette = "Set2") +
    ylim(0,1) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    ylab(NULL)
}

veg_gps = c("Mixed conifer", "Douglas-fir", "Lowland chaparral", 
            "Oak woodland", "Yellow pine")
## TSLF
p.a = f1(x_var_orig = tslf_l,
   x_var = tslf,
   x_seq = log(seq(1:111)),
   exp_x = F,
   veg_show = veg_gps) +
  scale_x_continuous(breaks = c(0, log(5), log(25), log(50), log(100)),
                     labels = c(1, 5, 25, 50, 100)) +
  xlab('Years Since Last Fire')

## ads mortality
p.b = f1(x_var_orig = ads_l,
   x_var = ads_mort,
   x_seq = seq_range(m$data2$ads_l, n = 100),
   exp_x = F,
   veg_show = veg_gps) +
  scale_x_continuous(breaks = c(-5, log(0.1), 0, log(10), log(100)),
                     labels = c(0, 0.25, 1, 10, 100)) +
  theme(axis.text.y = element_blank()) +
  xlab("Drought Mortality (TPA)")

## vpd
p.c = f1(x_var_orig = vpd,
   x_var = vpd,
   x_seq = seq_range(m$data2$vpd, n = 100),
   exp_x = F,
   veg_show = veg_gps) +
  xlab("Vapor Pressure Deficit (kPa)")


## fm1000
p.d = f1(x_var_orig = fm1000_l,
         x_var = fm1000,
         x_seq = seq_range(m$data2$fm1000_l, n = 100),
         exp_x = T,
         veg_show = veg_gps) +
  theme(axis.text.y = element_blank()) +
  xlab("FM 1000 (%)")

## wind speed
p.e = f1(x_var_orig = windspd,
   x_var = windspd,
   x_seq = seq_range(m$data2$windspd, n = 100),
   exp_x = F,
   veg_show = veg_gps) +
  xlab("Wind Speed (m/s)")

  

p = p.a + p.b + 
  p.c + ylab("High-severity Probability") + p.d + 
  p.e + guide_area() +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') +
  plot_layout(nrow = 3, guides = "collect") 

save_plot("Figures/Marg_VegEffects.png", p,
          base_width = 7, base_height = 7)
  

library(DT)

d = m$data2 %>% 
  dplyr::select(cwhr_gp, tslf, windspd:ads_mort) %>% 
  pivot_longer(cols = tslf:ads_mort, names_to = "term") %>% 
  group_by(cwhr_gp, term) %>% 
  summarise(mean = mean(value),
            median = median(value),
            .groups = "drop")

arrange(d, median) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(term = as.factor(term)) %>% 
  datatable(filter = "top")
