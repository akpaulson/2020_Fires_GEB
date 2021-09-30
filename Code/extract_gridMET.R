#### Description ####

# This code will extract gridMET weather data for the 2020 fires. 
# gridMET data for were
# downloaded from http://www.climatologylab.org/gridmet.html


#### Packages  & set up  ####

library(ncdf4)
library(raster)
library(chron) #to deal with dates in netCDF
library(lubridate)
library(tidyr)
library(dplyr)
library(stringr)
library(sf)

# Set Geospatial Data Directory (wherever you saved raw data downloaded from Box): 
geo_dir <- "RawData/GeoData/"

#Increase memory allocation: 

memory.limit(size=86000) #memory allocation was too low so I increased it


#### Data ####

#Bring in fire progressions for CA (40 biggest fires in 2020): 
prog <- raster("InProcessData/CA_2020_fire_progressions.tif")
proj4string(prog)



# Bring in 90 m grid (the base one, without veg/severity added to try to save mem for now)
grid_90 <- st_read("InProcessData/grid_90m_40fires.shp")
#Make sure grid is in same projection as raster
grid_90 <- st_transform(grid_90, crs = proj4string(prog))



#bring in CA polygon (for subsetting weather rasters to CA): 
ca <- st_read(paste0(geo_dir, "ca-state-boundary/CA_State_TIGER2016.shp"))
#need 40 km buffer around CA in order to get the SLATER fire points
  # in Oregon
ca_40 <- st_buffer(ca, 40000)


#### Extract day of burn for each point in the 90 m grid ####

prog_extract <- raster::extract(prog, grid_90, df = TRUE)

#Add values into grid_90: 
grid_90$day_of_burn = prog_extract$CA_2020_fire_progressions

# Make sure that it looks correct: 
slice_sample(grid_90, n = 10)

#Remove extra layer to save memory: 
rm(prog)
rm(prog_extract)


#### Which weather variables are available? ####

# I downloaded all of the weather variables from gridMet for 2020. 
# I am not certain what all of the abbreviations mean, and I couldn't
# find a list on their website. So, before I process, let's take
# a quick look and see which weather variables are available. 

#Look at metadata for net CDF file: 
nc_in <- nc_open(paste0(geo_dir, "2020_GridMet/bi_2020.nc"))

#get name: 
nc_in$var[[1]]$name
nc_in$var[[1]]$units

#close connection: 
nc_close(nc_in)

# BI = burning_index_g
# ERC = energy_release_component-g
# ETR = potential_evapotranspiration
# FM100 = dead_fuel_moisture_100hr
# FM1000 = dead_fuel_moisture_1000hr
# PDSI = palmer_drought_severity_index
# PET = potential_evapotranspiration (but this is what ETR was too?)
# PR = precipitation_amount
# RMax = relative_humidity (Max I assume)
# RMin = relative_humidity (Min I assume)
# SPH = specific_humidity
# SRAD = surface_downwelling_shortwave_flux_in_air
# TH = wind_from_direction
# TMMN = air_temperature (Min I assume)
# TMMX = air_temperature (Max I assume)
# VPD = mean_vapor_pressure_deficit
# VS = wind_speed


#Of these variables, the ones I spoke with Derek and Zack about are:
  # wind velocity (VS), Relative humidity (RMax and RMin), Burning index (BI), 
  # mean vapor pressure deficit (VPD), and fuel moisture (fm100 and fm1000..
  # I did not see FM10 in the available data). I would also like to pull 
  # energy release component (ERC). So, I have 8 gridMET variables to 
  # pull out. 






#### Burning Index Data ####

#Look at metadata for net CDF file: 
bi_in <- nc_open(paste0(geo_dir, "2020_GridMet/bi_2020.nc"))

#get name: 
bi_name <- bi_in$var[[1]]$name
bi_in$var[[1]]$units

#get dates: 
bi_dates <- ncvar_get(bi_in, "day")
bi_dates_units <- ncatt_get(bi_in, "day", "units")
bi_dates_n <- dim(bi_dates)

#close connection: 
nc_close(bi_in)


#Convert the time variable, Dates are in days since 1900-01-01: 

# split the time units string into fields
tustr <- strsplit(bi_dates_units$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth = as.integer(unlist(tdstr)[2])
tday = as.integer(unlist(tdstr)[3])
tyear = as.integer(unlist(tdstr)[1])

#use chron package to convert julian dates since 1900-01-01 to 2020 dates:
bi_dates_mdy <- chron(bi_dates, format = "m-d-y", origin = c(tmonth, tday, tyear))
bi_dates_mdy

#Convert to lubridate Date format
bi_dates_mdy <- mdy(bi_dates_mdy, tz = NULL)

#Convert mdy to julian dates in 2020:
bi_dates_julian <- yday(bi_dates_mdy)
bi_dates_julian
#This should be consistent across all gridMet weather variables for
  # 2020, so I should be able to use this throughout the rest of
  # the code. 

# Bring in burning index data as a raster brick: 
#EPSG = 4326 --> GCS WGS 1984
bi <- brick(paste0(geo_dir, "2020_GridMet/bi_2020.nc"))

#Look at layer 205: 
plot(bi[[205]])

#Name each layer of brick by its Julian date in 2020: 
names(bi) <- bi_dates_julian

# crop BI to CA: 
# change projection of CA boundary to match gridMET:
ca_40 <- st_transform(ca_40, proj4string(bi))
# crop raster brick to CA:
bi_ca <- crop(bi, ca_40)
plot(bi_ca[[305]])


#change projection of grid_90_proj data to match bi projection, 
  # should only have to do this once since projection will be 
  # the same for the other gridMET layers: 
grid_90_proj <- st_transform(grid_90, proj4string(bi))
rm(grid_90) #remove to save mem

#Extract burning index values for each grid_90 point, output as matrix: 
bi_extract <- raster::extract(bi_ca, grid_90_proj, method="bilinear")

#Since the extracted data is a matrix with 2,019,660 rows (one row
  # for each grid point) and 366 columns (one column for each day
  # of the year in 2020), we can use matrix indexing to pull out
  # the weather variable for the day of interest for each grid point. 

# Create matrix index to pull out rows/columns for each grid point. 
  # Row numbers represent the grid point number (1:2,019,660)
  # columns represent the day of burn for that grid point. We want
  # to pull out the weather variable corresponding only to the day of 
  # burn for the given grid point: 
bi_index_df <- data.frame(rows = 1:length(grid_90_proj$day_of_burn), 
                          day_of_burn = grid_90_proj$day_of_burn)

#Check output: 
slice_head(bi_index_df, n = 25)
#Looks good. There are some NAs for day_of_burn. Not sure why. 

#Now, use the matrix index to extract the weather variable for each
  # gridpoint/day of burn: 
burning_index <- bi_extract[as.matrix(bi_index_df)]

#Let's double check that output is as expected: 
a <- bi_extract[1:50, ]
bi_index_df[1:50, ]
burning_index[1:50]
#This output looks correct. There are three grid points where
  # day of burn is NA in these first 50 grid points. For the 
  # non-NA values, the day of burn/burning index result looks
  # correct. One more check: 
a <- bi_extract[1000:1025, ]
bi_index_df[1000:1025, ]
burning_index[1000:1025]
#Yes, looks correct. 

#The matrix indexing is much faster than what I was trying to do 
  # with the tidyverse. I will proceed with this for the other
  # weather variables. 

#Add burning_index to the grid_90 data: 
grid_90_proj$burning_index <- burning_index

slater <- grid_90_proj %>% 
  filter(FIRE_NAME == "SLATER") %>% 
  filter(is.na(day_of_burn))

#remove large objects to save mem: 
rm(bi)
rm(bi_ca)
rm(bi_extract)
rm(bi_index_df)


#### Wind velocity ####
vs_in <- nc_open(paste0(geo_dir, "2020_GridMet/vs_2020.nc"))

#get name: 
vs_in$var[[1]]$name
vs_in$var[[1]]$units

#close connection: 
nc_close(vs_in)

# Bring in burning index data as a raster brick: 
#EPSG = 4326 --> GCS WGS 1984
vs <- brick(paste0(geo_dir, "2020_GridMet/vs_2020.nc"))

#Look at layer 205: 
plot(vs[[205]])

#Name each layer of brick by its Julian date in 2020 (should match
  # the julian dates from burning index for 2020: 
names(vs) <- bi_dates_julian

# crop VS raster brick to CA (projection of CA already matched to 
  # gridMET data above): 
vs_ca <- crop(vs, ca_40)
plot(vs_ca[[305]])


#Extract burning index values for each grid_90 point (projection
  # already matched to gridMET data above), output as matrix: 
vs_extract <- raster::extract(vs_ca, grid_90_proj, method="bilinear")

#Since the extracted data is a matrix with 2,019,660 rows (one row
# for each grid point) and 366 columns (one column for each day
# of the year in 2020), we can use matrix indexing to pull out
# the weather variable for the day of interest for each grid point. 

# Create matrix index to pull out rows/columns for each grid point. 
# Row numbers represent the grid point number (1:2,019,660)
# columns represent the day of burn for that grid point. We want
# to pull out the weather variable corresponding only to the day of 
# burn for the given grid point: 
vs_index_df <- data.frame(rows = 1:length(grid_90_proj$day_of_burn), 
                          day_of_burn = grid_90_proj$day_of_burn)

#Check output: 
slice_head(vs_index_df, n = 25)
#Looks good. There are some NAs for day_of_burn. Not sure why. 

#Now, use the matrix index to extract the weather variable for each
# gridpoint/day of burn: 
wind_speed_m_per_s <- vs_extract[as.matrix(vs_index_df)]

#Let's double check that output is as expected: 
a <- vs_extract[1:50, ]
vs_index_df[1:50, ]
wind_speed_m_per_s[1:50]
#This output looks correct. There are three grid points where
# day of burn is NA in these first 50 grid points. For the 
# non-NA values, the day of burn/burning index result looks
# correct. One more check: 
a <- vs_extract[1000:1025, ]
vs_index_df[1000:1025, ]
wind_speed_m_per_s[1000:1025]
#Yes, looks correct. 

#The matrix indexing is much faster than what I was trying to do 
# with the tidyverse. I will proceed with this for the other
# weather variables. 

#Add burning_index to the grid_90 data: 
grid_90_proj$wind_speed_m_per_s <- wind_speed_m_per_s

#remove large objects to save mem: 
rm(vs)
rm(vs_ca)
rm(vs_extract)
rm(vs_index_df)



#### Vapor Pressure Deficit ####
vpd_in <- nc_open(paste0(geo_dir, "2020_GridMet/vpd_2020.nc"))

#get name: 
vpd_in$var[[1]]$name
vpd_in$var[[1]]$units

#close connection: 
nc_close(vpd_in)

# Bring in burning index data as a raster brick: 
#EPSG = 4326 --> GCS WGS 1984
vpd <- brick(paste0(geo_dir, "2020_GridMet/vpd_2020.nc"))

#Look at layer 205: 
plot(vpd[[205]])

#Name each layer of brick by its Julian date in 2020 (should match
# the julian dates from burning index for 2020: 
names(vpd) <- bi_dates_julian

# crop vpd raster brick to CA (projection of CA already matched to 
# gridMET data above): 
vpd_ca <- crop(vpd, ca_40)
plot(vpd_ca[[305]])


#Extract burning index values for each grid_90 point (projection
# already matched to gridMET data above), output as matrix: 
vpd_extract <- raster::extract(vpd_ca, grid_90_proj, method="bilinear")

#Since the extracted data is a matrix with 2,019,660 rows (one row
# for each grid point) and 366 columns (one column for each day
# of the year in 2020), we can use matrix indexing to pull out
# the weather variable for the day of interest for each grid point. 

# Create matrix index to pull out rows/columns for each grid point. 
# Row numbers represent the grid point number (1:2,019,660)
# columns represent the day of burn for that grid point. We want
# to pull out the weather variable corresponding only to the day of 
# burn for the given grid point: 
vpd_index_df <- data.frame(rows = 1:length(grid_90_proj$day_of_burn), 
                          day_of_burn = grid_90_proj$day_of_burn)

#Check output: 
slice_head(vpd_index_df, n = 25)
#Looks good. There are some NAs for day_of_burn. Not sure why. 

#Now, use the matrix index to extract the weather variable for each
# gridpoint/day of burn: 
mean_vapor_pressure_deficit_kpa <- vpd_extract[as.matrix(vpd_index_df)]

#Add burning_index to the grid_90 data: 
grid_90_proj$mean_vapor_pressure_deficit_kpa <- mean_vapor_pressure_deficit_kpa
names(grid_90_proj)
#remove large objects to save mem: 
rm(vpd)
rm(vpd_ca)
rm(vpd_extract)
rm(vpd_index_df)


#### Relative Humidity - Max ####
rmax_in <- nc_open(paste0(geo_dir, "2020_GridMet/rmax_2020.nc"))

#get name: 
rmax_in$var[[1]]$name
rmax_in$var[[1]]$units

#close connection: 
nc_close(rmax_in)

# Bring in burning index data as a raster brick: 
#EPSG = 4326 --> GCS WGS 1984
rmax <- brick(paste0(geo_dir, "2020_GridMet/rmax_2020.nc"))

#Look at layer 205: 
plot(rmax[[205]])

#Name each layer of brick by its Julian date in 2020 (should match
# the julian dates from burning index for 2020: 
names(rmax) <- bi_dates_julian


# crop rmax raster brick to CA (projection of CA already matched to 
# gridMET data above): 
rmax_ca <- crop(rmax, ca_40)
plot(rmax_ca[[305]])


#Extract burning index values for each grid_90 point (projection
# already matched to gridMET data above), output as matrix: 
rmax_extract <- raster::extract(rmax_ca, grid_90_proj)

#Since the extracted data is a matrix with 2,019,660 rows (one row
# for each grid point) and 366 columns (one column for each day
# of the year in 2020), we can use matrix indexing to pull out
# the weather variable for the day of interest for each grid point. 

# Create matrix index to pull out rows/columns for each grid point. 
# Row numbers represent the grid point number (1:2,019,660)
# columns represent the day of burn for that grid point. We want
# to pull out the weather variable corresponding only to the day of 
# burn for the given grid point: 
rmax_index_df <- data.frame(rows = 1:length(grid_90_proj$day_of_burn), 
                           day_of_burn = grid_90_proj$day_of_burn)

#Check output: 
slice_head(rmax_index_df, n = 25)
#Looks good. There are some NAs for day_of_burn. Not sure why. 

#Now, use the matrix index to extract the weather variable for each
# gridpoint/day of burn: 
relative_humidity_max_perc <- rmax_extract[as.matrix(rmax_index_df)]

#Add burning_index to the grid_90 data: 
grid_90_proj$relative_humidity_max_perc <- relative_humidity_max_perc
names(grid_90_proj)

#remove large objects to save mem: 
rm(rmax)
rm(rmax_ca)
rm(rmax_extract)
rm(rmax_index_df)




#### Relative Humidity - Min ####
rmin_in <- nc_open(paste0(geo_dir, "2020_GridMet/rmin_2020.nc"))

#get name: 
rmin_in$var[[1]]$name
rmin_in$var[[1]]$units

#close connection: 
nc_close(rmin_in)

# Bring in burning index data as a raster brick: 
#EPSG = 4326 --> GCS WGS 1984
rmin <- brick(paste0(geo_dir, "2020_GridMet/rmin_2020.nc"))

#Look at layer 205: 
plot(rmin[[205]])

#Name each layer of brick by its Julian date in 2020 (should match
# the julian dates from burning index for 2020: 
names(rmin) <- bi_dates_julian


# crop rmin raster brick to CA (projection of CA already matched to 
# gridMET data above): 
rmin_ca <- crop(rmin, ca_40)
plot(rmin_ca[[305]])


#Extract burning index values for each grid_90 point (projection
# already matched to gridMET data above), output as matrix: 
rmin_extract <- raster::extract(rmin_ca, grid_90_proj)

#Since the extracted data is a matrix with 2,019,660 rows (one row
# for each grid point) and 366 columns (one column for each day
# of the year in 2020), we can use matrix indexing to pull out
# the weather variable for the day of interest for each grid point. 

# Create matrix index to pull out rows/columns for each grid point. 
# Row numbers represent the grid point number (1:2,019,660)
# columns represent the day of burn for that grid point. We want
# to pull out the weather variable corresponding only to the day of 
# burn for the given grid point: 
rmin_index_df <- data.frame(rows = 1:length(grid_90_proj$day_of_burn), 
                            day_of_burn = grid_90_proj$day_of_burn)

#Check output: 
slice_head(rmin_index_df, n = 25)
#Looks good. There are some NAs for day_of_burn. Not sure why. 

#Now, use the matrix index to extract the weather variable for each
# gridpoint/day of burn: 
relative_humidity_min_perc <- rmin_extract[as.matrix(rmin_index_df)]

#Add burning_index to the grid_90 data: 
grid_90_proj$relative_humidity_min_perc <- relative_humidity_min_perc
names(grid_90_proj)

#remove large objects to save mem: 
rm(rmin)
rm(rmin_ca)
rm(rmin_extract)
rm(rmin_index_df)






#### Energy Release Component ####
erc_in <- nc_open(paste0(geo_dir, "2020_GridMet/erc_2020.nc"))

#get name: 
erc_in$var[[1]]$name
erc_in$var[[1]]$units

#close connection: 
nc_close(erc_in)

# Bring in burning index data as a raster brick: 
#EPSG = 4326 --> GCS WGS 1984
erc <- brick(paste0(geo_dir, "2020_GridMet/erc_2020.nc"))

#Look at layer 205: 
plot(erc[[205]])

#Name each layer of brick by its Julian date in 2020 (should match
# the julian dates from burning index for 2020: 
names(erc) <- bi_dates_julian


# crop erc raster brick to CA (projection of CA already matched to 
# gridMET data above): 
erc_ca <- crop(erc, ca_40)
plot(erc_ca[[305]])


#Extract burning index values for each grid_90 point (projection
# already matched to gridMET data above), output as matrix: 
erc_extract <- raster::extract(erc_ca, grid_90_proj)

#Since the extracted data is a matrix with 2,019,660 rows (one row
# for each grid point) and 366 columns (one column for each day
# of the year in 2020), we can use matrix indexing to pull out
# the weather variable for the day of interest for each grid point. 

# Create matrix index to pull out rows/columns for each grid point. 
# Row numbers represent the grid point number (1:2,019,660)
# columns represent the day of burn for that grid point. We want
# to pull out the weather variable corresponding only to the day of 
# burn for the given grid point: 
erc_index_df <- data.frame(rows = 1:length(grid_90_proj$day_of_burn), 
                            day_of_burn = grid_90_proj$day_of_burn)

#Check output: 
slice_head(erc_index_df, n = 25)
#Looks good. There are some NAs for day_of_burn. Not sure why. 

#Now, use the matrix index to extract the weather variable for each
# gridpoint/day of burn: 
energy_release_component <- erc_extract[as.matrix(erc_index_df)]

#Add burning_index to the grid_90 data: 
grid_90_proj$energy_release_component <- energy_release_component
names(grid_90_proj)

#remove large objects to save mem: 
rm(erc)
rm(erc_ca)
rm(erc_extract)
rm(erc_index_df)

head(grid_90_proj)




#### 100 hour fuel moisture ####
fm100_in <- nc_open(paste0(geo_dir, "2020_GridMet/fm100_2020.nc"))

#get name: 
fm100_in$var[[1]]$name
fm100_in$var[[1]]$units

#close connection: 
nc_close(fm100_in)

# Bring in burning index data as a raster brick: 
#EPSG = 4326 --> GCS WGS 1984
fm100 <- brick(paste0(geo_dir, "2020_GridMet/fm100_2020.nc"))

#Look at layer 205: 
plot(fm100[[205]])

#Name each layer of brick by its Julian date in 2020 (should match
# the julian dates from burning index for 2020: 
names(fm100) <- bi_dates_julian


# crop fm100 raster brick to CA (projection of CA already matched to 
# gridMET data above): 
fm100_ca <- crop(fm100, ca_40)
plot(fm100_ca[[305]])


#Extract burning index values for each grid_90 point (projection
# already matched to gridMET data above), output as matrix: 
fm100_extract <- raster::extract(fm100_ca, grid_90_proj)

#Since the extracted data is a matrix with 2,019,660 rows (one row
# for each grid point) and 366 columns (one column for each day
# of the year in 2020), we can use matrix indexing to pull out
# the weather variable for the day of interest for each grid point. 

# Create matrix index to pull out rows/columns for each grid point. 
# Row numbers represent the grid point number (1:2,019,660)
# columns represent the day of burn for that grid point. We want
# to pull out the weather variable corresponding only to the day of 
# burn for the given grid point: 
fm100_index_df <- data.frame(rows = 1:length(grid_90_proj$day_of_burn), 
                            day_of_burn = grid_90_proj$day_of_burn)

#Check output: 
slice_head(fm100_index_df, n = 25)
#Looks good. There are some NAs for day_of_burn. Not sure why. 

#Now, use the matrix index to extract the weather variable for each
# gridpoint/day of burn: 
dead_fuel_moisture_100hr <- fm100_extract[as.matrix(fm100_index_df)]

#Add burning_index to the grid_90 data: 
grid_90_proj$dead_fuel_moisture_100hr <- dead_fuel_moisture_100hr
names(grid_90_proj)

#remove large objects to save mem: 
rm(fm100)
rm(fm100_ca)
rm(fm100_extract)
rm(fm100_index_df)

head(grid_90_proj)


#### 1000 hour fuel moisture ####
fm1000_in <- nc_open(paste0(geo_dir, "2020_GridMet/fm1000_2020.nc"))

#get name: 
fm1000_in$var[[1]]$name
fm1000_in$var[[1]]$units

#close connection: 
nc_close(fm1000_in)

# Bring in burning index data as a raster brick: 
#EPSG = 4326 --> GCS WGS 1984
fm1000 <- brick(paste0(geo_dir, "2020_GridMet/fm1000_2020.nc"))

#Look at layer 205: 
plot(fm1000[[205]])

#Name each layer of brick by its Julian date in 2020 (should match
# the julian dates from burning index for 2020: 
names(fm1000) <- bi_dates_julian


# crop fm1000 raster brick to CA (projection of CA already matched to 
# gridMET data above): 
fm1000_ca <- crop(fm1000, ca_40)
plot(fm1000_ca[[305]])


#Extract burning index values for each grid_90 point (projection
# already matched to gridMET data above), output as matrix: 
fm1000_extract <- raster::extract(fm1000_ca, grid_90_proj,method="bilinear")

#Since the extracted data is a matrix with 2,019,660 rows (one row
# for each grid point) and 366 columns (one column for each day
# of the year in 2020), we can use matrix indexing to pull out
# the weather variable for the day of interest for each grid point. 

# Create matrix index to pull out rows/columns for each grid point. 
# Row numbers represent the grid point number (1:2,019,660)
# columns represent the day of burn for that grid point. We want
# to pull out the weather variable corresponding only to the day of 
# burn for the given grid point: 
fm1000_index_df <- data.frame(rows = 1:length(grid_90_proj$day_of_burn), 
                             day_of_burn = grid_90_proj$day_of_burn)

#Check output: 
slice_head(fm1000_index_df, n = 25)
#Looks good. There are some NAs for day_of_burn. Not sure why. 

#Now, use the matrix index to extract the weather variable for each
# gridpoint/day of burn: 
dead_fuel_moisture_1000hr <- fm1000_extract[as.matrix(fm1000_index_df)]

#Add burning_index to the grid_90 data: 
grid_90_proj$dead_fuel_moisture_1000hr <- dead_fuel_moisture_1000hr

names(grid_90_proj)

#remove large objects to save mem: 
rm(fm1000)
rm(fm1000_ca)
rm(fm1000_extract)
rm(fm1000_index_df)


#Check output
slice_sample(grid_90_proj, n = 10)



#### Save Output ####

#Looks like I made the names too long, shorten them: 
names(grid_90_proj)

grid_90_proj <- grid_90_proj %>% 
  rename(bi = burning_index, 
         windspd = wind_speed_m_per_s, 
         vpd = mean_vapor_pressure_deficit_kpa, 
         #rhmax = relative_humidity_max_perc, 
         #rhmin = relative_humidity_min_perc, 
         #erc = energy_release_component, 
         #fm100 = dead_fuel_moisture_100hr, 
         fm1000 = dead_fuel_moisture_1000hr)

slice_sample(grid_90_proj, n = 10)


st_write(grid_90_proj, "InProcessData/grid_90_grmt_bilinear_subsetvars.shp")
