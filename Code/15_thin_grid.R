## Purpose: Load the grid of spatial point samples, reproject to its original projection so points are in precise rows and columns, and then thin by removing every Xth row and column.
## Project: 2020_Fires_GEB
## Upstream: data_prep.R
## Downstream: 20_examine_autocorr.R; model_build.R


library(sf)
library(tidyverse)
library(here)


#### Setup (same for all scripts) ####
data_dir = readLines(here("data_dir.txt"), n=1) # The location of root of the data directory on your computer should be specified in "data_dir.txt" (ignored by git)
source(here("Code/00_shared_functions_and_globals.R")) # This defines the function `datadir()` which makes it easy to refer to data that is stored outside the repo


#### Main script ####

## What was the original projection of the point grid? It was made using the Cal Fire fire perimeter layer.
fire_perims = st_read(datadir("RawData/Historic_Fire_Perimeters/fire20_1.shp"))
# EPSG 3310 CA Albers

## Load the point grid
grid = st_read(datadir("CleanData/grid_90_clean_interpWeather"))

## Reproject to the Cal Fire projection
grid_proj = st_transform(grid,st_crs(fire_perims))

## get X and Y
grid_coords = st_coordinates(grid_proj)
grid_proj$x = grid_coords[,1]
grid_proj$y = grid_coords[,2]


### Thin the grid ###

# need to define a function to get the mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# define function to thin a grid
thin_grid = function(grid, thin_factor) {
  
  grid_foc = grid %>%
    # round x and y to 0.1 m
    mutate(x = round(x,1),
           y = round(y,1))
  
  ## get x and y increment
  xs = grid_foc$x %>% sort
  ys = grid_foc$y %>% sort
  x_diffs = c(xs,0) - c(0,xs)
  y_diffs = c(ys,0) - c(0,ys)
  x_step = x_diffs[x_diffs > 0] %>% getmode
  y_step = y_diffs[y_diffs > 0] %>% getmode
  
  x_min = grid_foc$x %>% min
  y_min = grid_foc$y %>% min
  x_max = grid_foc$x %>% max
  y_max = grid_foc$y %>% max
  
  x_keep = seq(x_min, x_max, by=thin_factor*x_step) %>% round(1)
  y_keep = seq(y_min, y_max, by=thin_factor*y_step) %>% round(1)
  
  grid_thinned = grid_foc %>%
    filter(x %in% x_keep,
           y %in% y_keep)
}
  
# split the CA grid into a list of fire-specific grids (because each one has a different grid origin)
grid_fire_chunks = split(grid_proj,f=grid_proj$FIRE_NA)


## thin it for a few different thin factors (final number in output filename)

thinned_grid_list = map(.x = grid_fire_chunks, .f = thin_grid, thin_factor=2)
grid_thinned = do.call("rbind",thinned_grid_list)
st_write(grid_thinned,datadir("CleanData/grid_thinned_interpWeather_02.gpkg"))

thinned_grid_list = map(.x = grid_fire_chunks, .f = thin_grid, thin_factor=3)
grid_thinned = do.call("rbind",thinned_grid_list)
st_write(grid_thinned,datadir("CleanData/grid_thinned_interpWeather_03.gpkg"))

thinned_grid_list = map(.x = grid_fire_chunks, .f = thin_grid, thin_factor=5)
grid_thinned = do.call("rbind",thinned_grid_list)
st_write(grid_thinned,datadir("CleanData/grid_thinned_interpWeather_05.gpkg"))

thinned_grid_list = map(.x = grid_fire_chunks, .f = thin_grid, thin_factor=10)
grid_thinned = do.call("rbind",thinned_grid_list)
st_write(grid_thinned,datadir("CleanData/grid_thinned_interpWeather_10.gpkg"))