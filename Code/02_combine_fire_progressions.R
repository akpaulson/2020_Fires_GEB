#### Description ####

# Derek Young produced fire progression maps (using Sean Parks's method) for 
  # each of the 2020 fires > 1000 acres. (GitHub repo for that is here:
  # https://github.com/youngdjn/2020-wildfire-mapping). 

# In order to extract the weather data from gridMET, I need to produce a raster
  # that combines the progression information for the 40 largest 2020 CA fires. 

#### Packages ####

library(raster)
library(sf)
library(dplyr)
library(stringr)

# Set Geospatial Data Directory (wherever you placed data downloaded from Box): 

geo_dir <- "../Geospatial_Data/"


#### Get top 40 fire names ####

#I made a shapefile for the perimeters of just the top 40 fires in the
  # get_fire_perim_make_90m_grid.R code. Get list of names for top 40 fires: 
perim_40 <- as.data.frame(st_read("InProcessData/perim_2020_top40.shp")) %>% 
  select(FIRE_NAME)

fires <- unique(perim_40$FIRE_NAME)

#### Bring in fire progression rasters for top 40 fires ####

#Get list of folders in the fire progression folder (these are from Derek Young's
  # fire progression Box folder): 
folder_list <- list.files(path = paste0(geo_dir, "fire_progressions/"), full.names = TRUE)

#subset to the folders associated with our 40 biggest fires: 
folder_list_fires <- str_subset(folder_list, paste(fires, collapse = "|"))
#Note that Red salmon complex and Gold fire are missing - email to DY on 5/18/21 
  # to ask to run these progressions. 

#Now, pull out file paths to the .tif files associated with fire progressions: 
files <- NA

for (i in 1:length(folder_list_fires)){
  
  a <- list.files(path = folder_list_fires[i], pattern = "dob.tif$", full.names = TRUE)
  files = c(files, a)
}

files <- files[-1] #remove NA

files #looks correct

prog <- lapply(files, raster)



#### Mosaic rasters into single raster ####

#(I tried using terra to do this, but it took way more memory and kept getting errors), 
  # using merge instead of mosaic because I don't want to average fire days. 

prog_combo <- do.call(merge, c(prog))

plot(prog_combo)

#Save output: 

writeRaster(prog_combo, filename = "InProcessData/CA_2020_fire_progressions.tif", 
            format = "GTiff", overwrite = FALSE)
