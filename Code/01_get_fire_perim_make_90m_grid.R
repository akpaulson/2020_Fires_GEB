#### Description ####

# This code will extract the perimeters for the 40 largest 2020 fires. It will
# create a grid of points across the fires at different spatial resolutions, 
# these points will the basis for our analyses - other codes will extract 
# predictors and responses for the point extracted here. 




#### Packages ####

library(sf)
library(dplyr)
library(ggplot2)




#### Data ####

# Set Geospatial Data Directory (wherever you placed data downloaded from Box): 

geo_dir <- "../Geospatial_Data/"


#CALFIRE Fire Perimeters (2020 included, downloaded 5/7/21): https://frap.fire.ca.gov/mapping/gis-data/
#Pull out 2020 fires >1000 acres to match Derek's fire progressions: 

perim_2020 <- st_read(dsn = paste0(geo_dir, "Historic_Fire_Perimeters/fire20_1.shp")) %>% 
  #Select only fires in 2020 that are >1000 acres: 
  filter(ALARM_DATE > "2020-01-01" & ALARM_DATE < "2021-01-01", 
         GIS_ACRES > 1000) %>% 
  arrange(-GIS_ACRES)
#I think warnings are okay - this layer has a lot of ring boundary intersection 
  # problems when I've used it before.  

# plot(st_geometry(perim_2020))

#Prepare list of fires - need to check which ones have severity info via RAVG/
  #which ones need to be completed in GEE. 
#commenting out because I manually enterred whether each fire had ravg, GEE, 
#   # or NA source for fire severity. 

# explore <- st_drop_geometry(perim_2020)
# write.csv(explore, "RawData/fires_2020_summary.csv", row.names = FALSE)







#### Prep shapefile for fires that I need to process using GEE for severity ####

# I need a shapefile with just the 40 largest fires, and only those that I don't
  # already have severity data processed for (this is for processing on 
  # Google Earth Engine - GEE): 

perim_2020_gee <- perim_2020 %>% 
  slice_head(n = 40) %>% 
  #remove RAVG fires (could probably do this programmatically, oh well): 
  filter(!FIRE_NAME %in% c("AUGUST COMPLEX FIRES", 
                           "CREEK", 
                           "NORTH COMPLEX", 
                           "CASTLE", 
                           "SLATER", 
                           "RED SALMON COMPLEX", 
                           "DOLAN", 
                           "BOBCAT", 
                           "W-5 COLD SPRINGS", 
                           "CALDWELL", 
                           "LOYALTON", 
                           "APPLE", 
                           "LAKE", 
                           "SHEEP", 
                           "SLINK", 
                           "GOLD", 
                           "HOG", 
                           "DEVIL")) %>% 
  # remove fires that I've already processed with GEE for USFS: 
  filter(!FIRE_NAME %in% c("RATTLESNAKE", 
                           "STAGECOACH", 
                           "BLUEJAY")) %>% 
  # Add Fire_ID and Year columns for processing in GEE: 
  mutate(Fire_ID = FIRE_NAME, 
         Year = "2020")

#So there are 19 more fires to process in GEE for initial fire severity estimates. 
# Note that a lot of these are probably in Chaparral. 

#Save this shapefile for GEE processing: 

st_write(perim_2020_gee, dsn = "InProcessData/perim_2020_gee.shp", overwrite = FALSE)
#warnings appear to not be a problem: https://github.com/r-spatial/sf/issues/306

#We also want to compare the severity estimates for RAVG vs. Google Earth Engine
  # Is the GEE approach that we've been using comparable to the severity 
  # estimates from RAVG? To answer that, I need a shapefile with the 2020 
  # fires that we had severity estimates for from RAVG. 

perim_2020_ravg <- perim_2020 %>% 
  slice_head(n = 40) %>% 
  #remove RAVG fires (could probably do this programmatically, oh well): 
  filter(FIRE_NAME %in% c("AUGUST COMPLEX FIRES", 
                           "CREEK", 
                           "NORTH COMPLEX", 
                           "CASTLE", 
                           "SLATER", 
                           "RED SALMON COMPLEX", 
                           "DOLAN", 
                           "BOBCAT", 
                           "W-5 COLD SPRINGS", 
                           "CALDWELL", 
                           "LOYALTON", 
                           "APPLE", 
                           "LAKE", 
                           "SHEEP", 
                           "SLINK", 
                           "GOLD", 
                           "HOG", 
                           "DEVIL")) %>% 
  # Add Fire_ID and Year columns for processing in GEE: 
  mutate(Fire_ID = FIRE_NAME, 
         Year = "2020")

#Save this shapefile for GEE processing: 

st_write(perim_2020_ravg, dsn = "InProcessData/perim_2020_ravg.shp", overwrite = FALSE)





#### Create shapefile for fires that you will use in analysis ####

# We are going to start with extracting variables for the 40 biggest fires in 
  # 2020. I was going to start with the top 20, but many of these were in 
  # chaparral/WUI areas and may not ultimately be included in the analyses. So,
  # I expanded to the 40 biggest fires, and we can look at the distribution
  # of points in different landcover types to decide which fires to use
  # for different analyses. 

perim_2020_top40 <- perim_2020 %>% 
  slice_head(n = 40)

#Save this shapefile - these are the 40 fires that our analyses will focus on: 
st_write(perim_2020_top40, dsn = "InProcessData/perim_2020_top40.shp", 
         overwrite = FALSE)
#warnings appear to not be a problem: https://github.com/r-spatial/sf/issues/306









#### Create grid of points across each fire polygon ####


#Create function to build a 90 m grid for a single fire

fire_grid <- function(cellsize_m, fire_perimeters, fire){
  #Pull out fire of interest from larger set of fire perimeters
    # For the CA perimeters, the column name for fire names is "FIRE_NAME"
  fire_of_interest <- fire_perimeters %>% 
    filter(FIRE_NAME == fire)
  
  #Make a point grid of cellsize_m within that fire: 
  grid_90 <- fire_of_interest %>% 
    #Create grid
    st_make_grid(., cellsize = cellsize_m, what = "centers") %>% 
    #Name Points: 
    st_sf(grid_id = paste0(fire, "_", 1:length(.))) %>% 
    #Because st_make_grid works within the bounding box for the fire, I want to 
      # only select points that are actually within the fire boundary: 
    mutate(in_fire = as.vector(st_intersects(., fire_of_interest, sparse = FALSE))) %>% 
    filter(in_fire %in% c("TRUE"))
  
  return(grid_90)
}

## End function


#Test function: 
test <- fire_grid(cellsize_m = 2000, fire_perimeters = perim_2020_top40, 
          fire = "SNOW")
#good

# view the sampled points, polygons and grid
ggplot() +
  geom_sf(data = perim_2020_top40, fill = 'white', lwd = 0.05) +
  geom_sf(data = test, color = 'red', size = 1.7) 
#good


# Now run foreloop with function to get 90 m grid within each fire of interest
# and bind them all together to get our initial set of points. 

#Pull out unique fire names: 
fires <- unique(perim_2020_top40$FIRE_NAME)

#Run for one fire so that we have a sfc dataset to build on top of: 

grid_90 <- fire_grid(cellsize_m = 90, fire_perimeters = perim_2020_top40, 
                           fire = fires[1])

# Run foreloop to fill in points for other fires, starting with fire 2: 
for (i in 2:length(fires)) {
 grid90_add <- fire_grid(cellsize_m = 90, fire_perimeters = perim_2020_top40, 
            fire = fires[i])
 grid_90 <- rbind(grid_90, grid90_add)
}

# view the sampled points, polygons and grid
ggplot() +
  geom_sf(data = perim_2020_top40, fill = 'white', lwd = 0.05) +
  geom_sf(data = grid_90, color = 'red', size = 1.7) 
#Looks like it worked. Confirmed 90 m spacing of points in ArcMap. 



#### Combine fire information with point grid ####

grid_90_fires <- st_join(grid_90, 
                         perim_2020_top40[ , c("AGENCY", "FIRE_NAME", "INC_NUM", "ALARM_DATE", "CONT_DATE", "CAUSE")], 
                         left = TRUE)

#I forgot to remove "in_fire" column in the function, just going to do it here
  # since function took so long to run: 
grid_90_fires <- grid_90_fires %>% 
  select(-in_fire)

head(grid_90_fires)
tail(grid_90_fires)
#Output looks good. 

#### Save grid for further processing ####

st_write(obj = grid_90_fires, dsn = "InProcessData/grid_90m_40fires.shp", overwrite = FALSE)
