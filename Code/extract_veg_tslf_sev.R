#### Description ####

# This code will extract the values of vegetation (PFR from FRID, EVT from 
  # Landfire, and calveg), severity, time since last fire (FRID), and 
  # fire regime condition class (from FRID) for each of the points in the 90m
  # grid across the 40 largest fires in CA in 2020. Grid was made in 
  # "get_fire_perim_make_90m_grid.R" code. 


#### Packages ####

library(raster)
library(sf)
library(dplyr)

# Set Geospatial Data Directory (wherever you saved data downloaded from Box): 
geo_dir <- "../Geospatial_Data/"


#### Grid ####

# Bring in 90m grid, this is a grid with 90 m spacing across each of the 40
  # largest fires in CA in 2020, produced in "get_fire_perim_make_90m_grid.R" code:

grid_90 <- st_read("InProcessData/grid_90m_40fires.shp")


#### FRID Data Extract ####

#Let's first pull out the FRID data for each grid point. FRID is the 
  # Fire Return Interval Departure, downloaded here for each CalVeg Zone: 
  # https://www.fs.usda.gov/detail/r5/landmanagement/gis/?cid=STELPRDB5327836

  # I processed these layers in ArcMap and/or ArcPro by first dissolving by each attribute
  # of interest (time since last fire - tslf, fire regime condition class - cc_fri, 
  # and presettlement fire regime - PFR) independently. I then ran the 
  # multipart to single part tool in ArcMap to break apart the multipart polygons
  # into singlepart polygons. I then merged each of the calveg zones for each 
  # attribute across the entire state to create a single shapefile for each
  # of the three variables across the state of California. 

  # Here, let's extract the value for TSLF, PFR, and cc_FRI for each grid point. 

#bring in layers: 
tslf <- st_read(paste0(geo_dir, "FRID_2019/Processed_Output/FRID_CA_19_poly_tslf.shp"))
meancc_fri <- st_read(paste0(geo_dir, "FRID_2019/Processed_Output/FRID_CA_19_meanCC_FRI_poly.shp"))
pfr <- st_read(paste0(geo_dir, "FRID_2019/Processed_Output/FRID_CA_19_pfr.shp"))

#Check projections: 
st_crs(tslf) == st_crs(grid_90)
st_crs(meancc_fri) == st_crs(grid_90)
st_crs(pfr) == st_crs(grid_90)

#Join TSLF, mean CC FRI, and pfr with grid90: 

grid_90_frid <- grid_90 %>% 
  st_join(., tslf[ , c("TSLF")]) %>% 
  st_join(., meancc_fri[ , c("meanCC_FRI")]) %>% 
  st_join(., pfr[ , c("PFR")])

#Check that output looks okay: 
slice_sample(grid_90_frid, n = 100)

#remove layers to try to save memory: 
rm(tslf)
rm(meancc_fri)
rm(pfr)
rm(grid_90)

#### Extract Calveg Existing Vegetation Cover Type ####

# Calveg Existing vegetation downloaded for each region in CA here: 
# https://data.fs.usda.gov/geodata/edw/datasets.php
# Data were downloaded in April 2021

#I used ArcMap to convert the geodatabase to a raster based on the column "COVERTYPE"
  # I reclassified the covertypes using the Reclass_COVERTYPE.cal function that I 
  # wrote in ArcMap: 

# def reclass(COVERTYPE):
#   if (COVERTYPE)=="AGR":
#   COVERTYPE_INTEGER=1
# elif (COVERTYPE)=="BAR":
#   COVERTYPE_INTEGER=2
# elif (COVERTYPE)=="CON":
#   COVERTYPE_INTEGER=3
# elif(COVERTYPE)=="HEB":
#   COVERTYPE_INTEGER=4
# elif(COVERTYPE)=="HDW":
#   COVERTYPE_INTEGER=5
# elif(COVERTYPE)=="MIX":
#   COVERTYPE_INTEGER=6
# elif(COVERTYPE)=="SHB":
#   COVERTYPE_INTEGER=7
# elif(COVERTYPE)=="URB":
#   COVERTYPE_INTEGER=8
# elif(COVERTYPE)=="WAT":
#   COVERTYPE_INTEGER=9
# else:
#   COVERTYPE_INTEGER = -9999
# return COVERTYPE_INTEGER

# 
# __esri_field_calculator_splitter__
# reclass(!COV

## Once I created a raster for each region in CA, I combined all of the 
  # reclassified rasters into a single raster for the state of California

# Calveg existing vegetation type: 

eveg <- raster(paste0(geo_dir, "CalVeg/ProcessedOutput/EVMid_R05_CA_COVERTYPE_30m.tif"))
proj4string(eveg)

#Check that projection matches grid_90_frid: 
st_crs(grid_90_frid) #sf uses different format, hard to tell if the same. 

#Reproject grid_90_frid_proj so that it matches (all of my other rasters also 
  # have same projection as eveg, so I should only have to do this once for this
  # set of data).  
grid_90_frid_proj <- st_transform(grid_90_frid, crs = proj4string(eveg))

st_crs(grid_90_frid) == st_crs(grid_90_frid_proj) #Yes, something changed

rm(grid_90_frid) #remove layer to save mem


#Extract cover type for each point location: 
eveg_extract <- raster::extract(eveg, grid_90_frid_proj, df = TRUE)


#Unclassify the reclassify that I did in ArcMap so we can see actual covertypes: 

eveg_extract_reclass <- eveg_extract %>% 
  mutate(covertype = case_when(
    EVMid_R05_CA_COVERTYPE_30m == 1 ~ "AGR", 
    EVMid_R05_CA_COVERTYPE_30m == 2 ~ "BAR", 
    EVMid_R05_CA_COVERTYPE_30m == 3 ~ "CON", 
    EVMid_R05_CA_COVERTYPE_30m == 4 ~ "HEB", 
    EVMid_R05_CA_COVERTYPE_30m == 5 ~ "HDW", 
    EVMid_R05_CA_COVERTYPE_30m == 6 ~ "MIX", 
    EVMid_R05_CA_COVERTYPE_30m == 7 ~ "SHB", 
    EVMid_R05_CA_COVERTYPE_30m == 8 ~ "URB", 
    EVMid_R05_CA_COVERTYPE_30m == 9 ~ "WAT",
    TRUE ~ NA_character_))

#Add values into grid_90: 
grid_90_frid_proj$covertype = eveg_extract_reclass$covertype

# Make sure that it looks correct: 
slice_sample(grid_90_frid_proj, n = 10)

#Remove extra layers to save memory: 
rm(eveg)
rm(eveg_extract)
rm(eveg_extract_reclass)



#### Extract Landfire EVT ####

#This is LANDFIRE's Existing Vegetation Type, 2016 version: https://landfire.gov/evt.php

# I downloaded the CONUS layer and used ArcMap to extract California. 

# Landfire Raster: 
lfire <- raster(paste0(geo_dir, "LANDFIRE/LF2016_EVT_2000_CA.tif"))
proj4string(lfire)


#Extract landfire for each grid point: 
lfire_extract <- raster::extract(lfire, grid_90_frid_proj, df = TRUE)

#Now, need to associate the LANDFIRE EVT values with their associated 
  # attributes. 
lfire_attr <- read.csv(paste0(geo_dir, "LANDFIRE/CSV_Data/LF16_EVT_200.csv"), 
                       stringsAsFactors = FALSE) %>% 
  select(VALUE, EVT_NAME, EVT_LF, EVT_PHYS, EVT_GP_N, SAF_SRM, EVT_ORDER, EVT_CLASS, EVT_SBCLS)

#Join the landfire attributes with their value code
lfire_extractb <- lfire_extract %>% 
  merge(., lfire_attr, by.x = "LF2016_EVT_2000_CA", by.y = "VALUE", 
        all.x = TRUE)
slice_sample(lfire_extractb, n = 10)


#Add landfire attributes to the grid_90 dataset: 
grid_90_frid_proj_lfire <- cbind(grid_90_frid_proj, lfire_extractb) %>% 
  select(-ID)


# Make sure that it looks correct: 
slice_sample(grid_90_frid_proj_lfire, n = 10)
str(grid_90_frid_proj_lfire)


#Remove extra layers to save memory: 
rm(grid_90_frid_proj)
rm(lfire_extract)
rm(lfire_attr)
rm(lfire_extractb)






#### Extract severity ####

# Severity rasters. These rasters were produced in the 
  # "combine_severity_rasters.R" code. 

sev_rdnbr <- raster("InProcessData/rdnbr_2020_allfires.tif")
proj4string(sev_rdnbr)

sev_dnbr <- raster("InProcessData/dnbr_2020_allfires.tif")
proj4string(sev_dnbr)

#Check projections match: 
proj4string(sev_dnbr) == proj4string(lfire)
proj4string(sev_rdnbr) == proj4string(lfire)
rm(lfire)

#Extract RdNBR using bilinear interpolation: 

memory.limit(size=86000) #memory allocation was too low so I increased it

sev_rdnbr_extract <- raster::extract(sev_rdnbr, grid_90_frid_proj, 
                                     method = "bilinear", df = TRUE)

sev_dnbr_extract <- raster::extract(sev_dnbr, grid_90_frid_proj, 
                                    method = "bilinear", df = TRUE)


#Add rdnbr and dnbr attributes to the grid_90 dataset: 
grid_90_frid_proj_lfire$rdnbr <- sev_rdnbr_extract$rdnbr_2020_allfires
grid_90_frid_proj_lfire$dnbr <- sev_dnbr_extract$dnbr_2020_allfires

#Check output
slice_sample(grid_90_frid_proj_lfire, n = 10)

#Reclassify RdNBR as low, moderate, or high severity fire: 
# Follow Table 1 in Lydersen et al 2016 (Fire Ecology) for Initial RdNBR 
  # Assessments. I believe these thresholds are from Miller & Quayle 2015, but 
  # they don't have a clear table showing thresholds. 
#Also reclassify as high severity (1) or not (0)

grid_90_frid_proj_lfire_reclass <- grid_90_frid_proj_lfire %>% 
  mutate(rdnbr_cat = case_when(
    rdnbr < 79 ~ "unchanged", 
    rdnbr >= 79 & rdnbr < 361 ~ "low", 
    rdnbr >= 361 & rdnbr < 733 ~ "moderate", 
    rdnbr >= 733 ~ "high", 
    TRUE ~ NA_character_)) %>% 
  mutate(rdnbr_high = if_else(rdnbr >= 733, 1, 0))

slice_sample(grid_90_frid_proj_lfire_reclass, n = 10)


#Not sure what thresholds to use for dNBR, so I am leaving that alone for now. 


#### Bring in CalVeg CWHR ####
grid_90_frid_proj_lfire_reclass <- st_read("InProcessData/grid_90_veg_sev.shp")

# Bring in Calveg CWHR Type
# I producecd this raster layer in ArcPro by 1) dissolving the Calveg layer 
# by CWHR_TYPE; 2) creating a lookup table using all of the cwhr codes
# from all of the different regions (I exported the attribute tables from 
# the dissolved shapefiles, then using calveg_cwhr_lookuptable.R to create
# a lookup table). 3) I used the polygon to raster tool after joining the lookup
# table to each dissolved shapefile. I matched the projection, extent, and 
# resolution to the fire severity layer rdnbr_2020_allfires.tif (that I produced
# in the combine_severity_rasters.R code). 4) Lastly, I mosaiced all of the 
# rasters from the individual regions into a single statewide cwhr raster. 

cwhr <- raster(paste0(geo_dir, "CalVeg/ProcessedOutput/EVMid_R05_CA_cwhr.tif"))
proj4string(cwhr)

#Extract values for each grid point: 
cwhr_extract <- raster::extract(cwhr, grid_90_frid_proj_lfire_reclass)

#Use the cwhr lookup table to assign cwhr classes to the raster values
cwhr_lookup <- read.csv(paste0(geo_dir, "CalVeg/ProcessedOutput/LookupTables/cwhr_lookup.csv"), 
                        stringsAsFactors = FALSE) 

cwhr_extract2 <- as.data.frame(cwhr_extract) %>% 
  left_join(., cwhr_lookup, by = c("cwhr_extract" = "cwhr"))

#add values into grid points object:
grid_90_frid_proj_lfire_reclass$cwhr <- cwhr_extract2$CWHR_TYPE
#check output:
slice_sample(grid_90_frid_proj_lfire_reclass, n = 20)

#remove unnecessary layers for mem:
rm(cwhr)
rm(cwhr_extract)
rm(cwhr_extract2)
rm(cwhr_lookup)


#### Add Calveg Ecoregion Section ####
#I dissolved the CalvegTiles_Ecoregions07_5.gdb geodatabase by ecoregion_section
  # to get this shapefile. 

#Codes are here: https://www.fs.usda.gov/detail/r5/landmanagement/resourcemanagement/?cid=fsbdev3_048066
ecoregion <- st_read(paste0(geo_dir, "CalVeg/ProcessedOutput/calveg_ecoregion_section.shp"))
#Put in same coords as the grid_90: 
ecoregion = st_transform(ecoregion, st_crs(grid_90_frid_proj_lfire_reclass))

#Extract ecoregion for each point: 
grid_90_veg_sev <- grid_90_frid_proj_lfire_reclass %>% 
  st_join(., ecoregion[ , c("ECOREGION_")]) %>% 
  rename(ecoregion_section = ECOREGION_)


#### Save Data ####
st_write(grid_90_veg_sev, "InProcessData/grid_90_veg_sev_eco.shp", overwrite = TRUE)

