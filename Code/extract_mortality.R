#### Description #### 

# This code will extract data from the USFS Aerial Detection Monitoring data
# in California from 2012-2019. The purpose of this is to see if we can Use
# the aerial detection surveys as an indication of additional fuel loading
# on the ground following the 2012-2016 drought and beetle mortality event - 
# something that might have contributed to increased fire severity in 2020. 

#### Set Up ####

library(sf)
library(dplyr)
library(raster)
library(sp)
library(fasterize)
library(ggplot2)

#Increase memory: 
memory.limit(size=86000) #memory allocation was too low so I increased it


#### Data ####

# Bring in 90m grid, this is a grid with 90 m spacing across each of the 40
# largest fires in CA in 2020, produced in "get_fire_perim_make_90m_grid.R" code:

grid_90 <- st_read("InProcessData/grid_90m_40fires.shp")


#Template raster - fire severity
rdnbr = raster("InProcessData/rdnbr_2020_allfires.tif")

# All of the aerial detection monitoring data from 2012-2019 were downloaded 
# in geodatabase format in July 2021 from:
# https://www.fs.usda.gov/detail/r5/forest-grasslandhealth/?cid=fsbdev3_046696

ads12 <- st_read(dsn = "../Geospatial_Data/Beetle_Mortality/ADS_2012/ADS_Regionwide_2012.gdb", 
                          layer = "ADS_12v2")
ads13 <- st_read(dsn = "../Geospatial_Data/Beetle_Mortality/ADS_2013/ADS_R5_2013.gdb", 
                          layer = "R5ADS_13")
ads14 <- st_read(dsn = "../Geospatial_Data/Beetle_Mortality/ADS_2014/ADS2014.gdb", 
                          layer = "ADS14")
ads15 <- st_read(dsn = "../Geospatial_Data/Beetle_Mortality/ADS_2015/ADS2015.gdb", 
                          layer = "ADS15")
ads16 <- st_read(dsn = "../Geospatial_Data/Beetle_Mortality/ADS_2016/ADS2016.gdb", 
                          layer = "ADS16")
ads17 <- st_read(dsn = "../Geospatial_Data/Beetle_Mortality/ADS_2017/ADS2017.gdb", 
                          layer = "ADS17")
ads18 <- st_read(dsn = "../Geospatial_Data/Beetle_Mortality/ADS_2018/ADS2018.gdb", 
                          layer = "ADS18")
ads19 <- st_read(dsn = "../Geospatial_Data/Beetle_Mortality/ADS_2019/ADS2019.gdb", 
                          layer = "ADS2019")

#Check that all layers are in the same projection, change if need be: 

st_crs(ads12) == st_crs(rdnbr) #FALSE
st_crs(ads19) == st_crs(rdnbr) #TRUE
st_crs(ads12) == st_crs(ads13)
st_crs(ads12) == st_crs(ads14)
st_crs(ads12) == st_crs(ads15)
st_crs(ads12) == st_crs(ads16)
st_crs(ads12) == st_crs(ads17) #FALSE
st_crs(ads12) == st_crs(ads18) #FALSE
st_crs(ads12) == st_crs(ads19) #FALSE
st_crs(ads12) == st_crs(grid_90) #TRUE

ads12 <- st_transform(ads12, crs = st_crs(ads19))
ads13 <- st_transform(ads13, crs = st_crs(ads19))
ads14 <- st_transform(ads14, crs = st_crs(ads19))
ads15 <- st_transform(ads15, crs = st_crs(ads19))
ads16 <- st_transform(ads16, crs = st_crs(ads19))
grid_90 <- st_transform(grid_90, crs = st_crs(ads19))

st_crs(ads12) == st_crs(ads19) #TRUE
st_crs(ads13) == st_crs(ads19) #TRUE
st_crs(ads14) == st_crs(ads19) #TRUE
st_crs(ads15) == st_crs(ads19) #TRUE
st_crs(ads16) == st_crs(ads19) #TRUE
st_crs(grid_90) == st_crs(ads19) #TRUE

#### Background on dataset from Derek Young ####

# Email 7/5:
#Here are the mortality survey data (we can use the polygons in the GDBs, 
#and potentially filter out the "background" mortality polygons--
#which we defined as polygons with < 4 dead trees total or < 1 dead TPA). 
#We only used data through 2015. Starting in 2016, they changed the methodology 
#in a way that adds lots of noise and bias (basically, if I remember right, 
#they started estimating in three very coarse mortality density classes, 
#but reporting only the midpoint of the class as though it was estimated precisely).

#Email 7/16: 
# The vast majority of mortality will be in the 2015 layer, but 
# including the others would probably make the relationship a little tighter. 
# An approach could be to turn each annual layer into a raster and then sum all 
# the rasters.
# 
# Also an option could be to classify mortality into classes like 
# "none/background", "low", and "high", and this might allow us to include 2016
# and later. There was a lot of mortality in 2016 too so this could be a way to
# combine the datasets collected using different protocols. When combining the 
# years, maybe you could just take the category with the highest classification.


#### rasterize tpa 2012-2016 ####

#Derek says that the methods changed after 2015, so trees per acre can only 
  # be used pre-2016.  I will rasterize the polygons, then create a mosaic
  # raster that is the sum of the individual 2012-2015 TPA rasters. He 
  # recommended filtering out polygons with background mortality - which 
  # we will classify as TPA < 1

summary(ads12$TPA1)
#Filter out TPA1<1
ads12_filter <- ads12 %>% 
  filter(TPA1 >= 1) 

ads12_rast <- fasterize(ads12_filter, rdnbr, field = "TPA1")
summary(ads12_rast)
res(ads12_rast)

ads13_filter <- ads13 %>%
  filter(TPA1 >= 1)
ads13_rast <- fasterize(ads13_filter, rdnbr, field = "TPA1")
summary(ads13_filter$TPA1)
summary(ads13_rast)
#There is one polygon with a value of 244.9 that is not being pulled through
  #on the raster. I looked in ArcGIS and it's in the very NW corner of CA - 
  # I'm guessing it's not within the boundary of the rdnbr, so it shouldn't
  # matter for our analysis - it would be outside all of the fire boundaries 
  # anyway. 

ads14_filter <- ads14 %>%
  filter(TPA1 >= 1)
ads14_rast <- fasterize(ads14_filter, rdnbr, field = "TPA1")
summary(ads14_filter$TPA1)
summary(ads14_rast)

mort_12_14 <-sum(ads12_rast, ads13_rast, ads14_rast, na.rm = TRUE)
#remove large memory hog objects: 
rm(ads12_rast)
rm(ads13_rast)
rm(ads14_rast)

ads15_filter <- ads15 %>%
  filter(TPA1 >= 1)
ads15_rast <- fasterize(ads15_filter, rdnbr, field = "TPA1")
summary(ads15_filter$TPA1)
summary(ads15_rast)

ads16_filter <- ads16 %>%
  filter(TPA1 >= 1)
ads16_rast <- fasterize(ads16_filter, rdnbr, field = "TPA1")
summary(ads16_filter$TPA1)
summary(ads16_rast)

mort_15_16 <-sum(ads15_rast, ads16_rast, na.rm = TRUE)
#remove large memory hog objects: 
rm(ads15_rast)
rm(ads16_rast)
#Now, we need to add the values together from the original rasters to get a 
  # single layer with trees per acre mortality from 2012-2015
#need to do this in three steps because of computer memory...


mortality_tpa_12_16 <- sum(mort_12_14, mort_15_16, na.rm = TRUE)
#remove large memory hog objects: 
rm(mort_12_14)
rm(mort_15_16)

writeRaster(mortality_tpa_12_16, "InProcessData/mortality_tpa_12_16.tif")

#Extract values for each grid_90 point: 
grid_90_mort_tp1_12_16 <- raster::extract(mortality_tpa_12_16, grid_90, df = TRUE)
slice_sample(grid_90_mort_tp1_12_16, n =20)
grid_90$mortality_tpa_12_16 <- grid_90_mort_tp1_12_16$layer
summary(grid_90$mortality_tpa_12_16)

st_write(grid_90, "InProcessData/grid_90_mortality.shp")

#remove large memory hog objects: 
rm(grid_90_mort_tp1_12_16)
rm(mortality_tpa_12_16)


## Note re: zeroes:
# When I summed the individual year mortality rasters, the function 
# placed a 0 everywhere where there wasn't data because I used na.rm = FALSE, 
# this isn't a real 0.  I think NA is more appropriate in many places because
# they weren't assessed. But, there are also areas where it was assessed
# and no mortality was observed. For our purposes, a 0 is probably fine
# because we would want to include the variable in a model.  


#### Rasterize mortality classification 2012-2019 ####