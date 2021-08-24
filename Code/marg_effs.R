## Purpose: Build marginal effects plots 
## Project: 2020_Fires_GEB
## Upstream: model_build.R
## Downstream:

library(tidyverse)
library(tidybayes)
library(brms)
library(ggdist)
library(modelr)

#### Will try modifying so that color and CIs are only shown for a subset of veg types
#### Grey mean curves will be shown for all veg types

## read in model
m = read_rds("Models/model0_veg.rds")

scale2 <- function(x, na.rm = TRUE) (x - mean(x, na.rm = na.rm))

m$data2 = mutate(m$data2, 
            tslf_l = log(tslf + 0.01),
            ads_l = log(ads_mort + 0.01),
            fm1000_l = log(fm1000 + 0.01))

## set up plotting function
f1 <- function(x_var, #model variable
               x_var_orig, #unstandardized, but transformed
               x_seq, #unstandardized scale
               exp_x, #do we need to exponentiate x for plotting?,
               veg_show) {
  ## set up new data, gotta be an easier way to fill "typical" values
  newdata0 <- data.frame(tslf = 0, ads_mort = 0, vpd = 0, windspd = 0, fm1000 = 0,
                         fire_na = "new",
                         cwhr_gp = c({{veg_show}}, "new")) %>% 
    ## drop variable to be expanded below
    dplyr::select(-{{x_var}})
  
  ## get range of values for user-defined variable
  ## Note the use of "glue syntax" and "embracing" https://dplyr.tidyverse.org/articles/programming.html
  
  d_sum <- summarize(m$data2, 
                      sd = sd({{x_var_orig}}, na.rm = T),
                      mn = mean({{x_var_orig}}, na.rm = T))
  
  ## all combos of variable sequence and veg group
  newdata <- data_grid(m$data,
                       .x_us = x_seq,
                       # "{{x_var}}" := seq_range({{x_var}}, n = 100),
                       cwhr_gp = c({{veg_show}})) %>%
    mutate({{x_var}} := (.x_us - d_sum$mn) / d_sum$sd) %>%
  left_join(newdata0, by = "cwhr_gp") %>%
  ## Get fitted draws (expected predictions)
  add_epred_draws(m,
                  #re_formula = NA,
                  allow_new_levels = T) #%>%
  ## Just plotting means by group
  # group_by({{x_var}}, cwhr_gp) %>%
  # summarise(.epred = median(.epred), .groups = "drop") %>%
  # ## get unstandardized values
  # mutate(.x_us = {{x_var}} * d_sum$sd + d_sum$mn ) 

  # ## The average effect
  # newdata1 = data_grid(m$data,
  #                      .x_us = x_seq,
  #                      # "{{x_var}}" := seq_range({{x_var}}, n = 100),
  #                      fire_na = "new", cwhr_gp = "new") %>%
  #   mutate({{x_var}} := (.x_us - d_sum$mn) / d_sum$sd) %>%
  #   left_join(newdata0, by = c("fire_na", "cwhr_gp")) %>%
  #   add_epred_draws(m, re_formula = NA) #%>%
  #   # mutate(.x_us = {{x_var}} * d_sum$sd + d_sum$mn )
  # 
  if(exp_x == T) {
    newdata$.x_us = exp(newdata$.x_us)
    # newdata1$.x_us = exp(newdata1$.x_us)
  }

  ## Plot "new" veg w/ CIs and means for others
  ggplot(newdata, aes(x = .x_us, y = .epred, color = cwhr_gp, fill = cwhr_gp)) +
    stat_lineribbon(.width = 0.9, size = 2, alpha = .5) +
    # geom_line(data = newdata, aes(color = cwhr_gp), alpha = 0.85) +
    # scale_fill_manual(values = "grey80", guide = NULL) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Set2") +
    ylim(0,1) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    ylab("High-severity Probability")
}

## TSLF
f1(x_var_orig = tslf_l,
   x_var = tslf,
   x_seq = log(seq(1:111)),
   exp_x = F,
   veg_show = unique(m$data$cwhr_gp)) +
  # scale_x_continuous(breaks = c(0, 25, 50, 75, 100)) +
  scale_x_continuous(breaks = c(0, log(5), log(25), log(50), log(100)),
                     labels = c(1, 5, 25, 50, 100)) +
  xlab('Years Since Last Fire')

## vpd
f1(x_var_orig = vpd,
   x_var = vpd,
   x_seq = seq_range(m$data2$vpd, n = 100),
   exp_x = F,
   veg_show = unique(m$data$cwhr_gp)) +
  xlab("Vapor Preasure Deficit")

## ads mortality
f1(x_var_orig = ads_l,
   x_var = ads_mort,
   x_seq = seq_range(m$data2$ads_l, n = 100),
   exp_x = T,
   veg_show = unique(m$data$cwhr_gp)) +
  xlab("ADS Mortality")

## wind speed
f1(x_var_orig = windspd,
   x_var = windspd,
   x_seq = seq_range(m$data2$windspd, n = 100),
   exp_x = F,
   veg_show = unique(m$data$cwhr_gp)) +
  xlab("Wind Speed")

## fm1000
f1(x_var_orig = fm1000_l,
   x_var = fm1000,
   x_seq = seq_range(m$data2$fm1000_l, n = 100),
   exp_x = T,
   veg_show = unique(m$data$cwhr_gp)) +
  xlab("FM 1000")
  
  
