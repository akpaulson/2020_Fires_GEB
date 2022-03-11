#### Description ####

# When trying to process the PFR attribute from the FRID 2019 database,
# (Fire Return Interval Departure, downloaded here for each CalVeg Zone: 
# https://www.fs.usda.gov/detail/r5/landmanagement/gis/?cid=STELPRDB5327836)
# Some of the zones had geometry errors that made processing fail in ArcMap 
# and ArcPro, multiple times over with many different methods used to try to fix
# it. It seems that most of the problems are with the South Sierra zone.  

# So, this code will attempt a simple rbind of all of the features across all
  # zones. I will use the output from this code as the PFR shapefile for future 
  # analyses. 

#### Packages ####

library(sf)
library(dplyr)


# Set Geospatial Data Directory (wherever you placed data downloaded from Box): 

geo_dir <- "../Geospatial_Data/"



#### Data ####

#I started by dissolving each FRID 2019 region by the PFR attribute. I did this in ArcPro. 

#Load all of the dissolved pfr layers for each region: 

central_coast_pfr <- st_read(paste0(geo_dir, "FRID_2019/Processed_Output/FRID_Layers_PFR/FRID_CentralCoast19_1_diss.shp"))
central_valley_pfr <- st_read(paste0(geo_dir, "FRID_2019/Processed_Output/FRID_Layers_PFR/FRID_CentralValley19_1_diss.shp"))
great_basin_pfr <- st_read(paste0(geo_dir, "FRID_2019/Processed_Output/FRID_Layers_PFR/FRID_GreatBasin19_1_diss.shp"))
north_coast_e_pfr <- st_read(paste0(geo_dir, "FRID_2019/Processed_Output/FRID_Layers_PFR/FRID_NorthCoast19_1_East_diss.shp"))
north_coast_mid_pfr <- st_read(paste0(geo_dir, "FRID_2019/Processed_Output/FRID_Layers_PFR/FRID_NorthCoast19_1_Mid_diss.shp"))
north_coast_w_pfr <- st_read(paste0(geo_dir, "FRID_2019/Processed_Output/FRID_Layers_PFR/FRID_NorthCoast19_1_West_diss.shp"))
north_interior_pfr <- st_read(paste0(geo_dir, "FRID_2019/Processed_Output/FRID_Layers_PFR/FRID_NorthInterior19_1_diss.shp"))
north_sierra_pfr <- st_read(paste0(geo_dir, "FRID_2019/Processed_Output/FRID_Layers_PFR/FRID_NorthSierra19_1_diss.shp"))
south_coast_pfr <- st_read(paste0(geo_dir, "FRID_2019/Processed_Output/FRID_Layers_PFR/FRID_SouthCoast19_1_dissolve.shp"))
south_interior_pfr <- st_read(paste0(geo_dir, "FRID_2019/Processed_Output/FRID_Layers_PFR/FRID_SouthInterior19_1_dissolve.shp"))
south_sierra_pfr <- st_read(paste0(geo_dir, "FRID_2019/Processed_Output/FRID_Layers_PFR/FRID_SouthSierra19_1_Dissolve.shp"))



#### Bind each PFR layer together for whole state of CA ####

#Bind them all together: 
test <- rbind(central_coast_pfr, central_valley_pfr, great_basin_pfr, 
              north_coast_e_pfr, north_coast_mid_pfr, north_coast_w_pfr, 
              north_interior_pfr, north_sierra_pfr, south_coast_pfr, 
              south_interior_pfr, south_sierra_pfr)


#### Save bound output for state of CA ####

st_write(test, dsn = paste0(geo_dir, "FRID_2019/Processed_Output/FRID_CA_19_pfr.shp"))

# I viewed this shapefile in ArcMap and it looks correct. 