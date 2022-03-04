## Purpose: socioeconomic trends figure

library(tidyverse)
library(patchwork)
library(cowplot)

## read in data f
d = read_csv('RawData/CalFireTrends_1980-2020.csv') %>% 
  filter(year > 2002) %>% 
  select(year, cost = 'cost+damage $billions', cost_adj = 'cost inflation adjusted (2020 $s)',
         structures)

p1 = ggplot(d, aes(x = year, y = cost)) +
  geom_point() +
  geom_smooth(formula = 'y ~ x', method = 'loess', se = F) +
  theme_bw() +
  ylab('$ Billions (insured loss)') + xlab(NULL)

p2 = ggplot(d, aes(x = year, y = structures)) +
  geom_point() +
  geom_smooth(formula = 'y ~ x', method = 'loess', se = F) +
  theme_bw() +
  ylab('Structures') + xlab("Year")

p = p1 / p2

save_plot("Figures/losses.png", p,
         base_width = 5, base_height = 6)
