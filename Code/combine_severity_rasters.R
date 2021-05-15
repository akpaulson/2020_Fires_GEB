#### Description ####

# This code will combine the 1) RAVG rasters from across all of
# the fires (for which RAVG data were available), 2) All fires processed
# with Google Earth Engine using a modification of Sean Parks's code, and 
# 3) A combination of RAVG + GEE fires into single raster images. 
# Anything in between fires will be NA. 

#Because we may want to use dNBR for chaparral areas, if included in the 
  # analysis, I will produce severity layers for both RdNBR and dNBR

#### Packages ####
library(raster)

# Set Geospatial Data Directory (wherever you saved data downloaded from Box): 
geo_dir <- "../Geospatial_Data/"





#### Load and Combine all RAVG RdNBR rasters ####

#Get list of each .tif file that we currently have RAVG available for: 
ravg_files <- list.files(path = paste0(geo_dir, "FireSeverity2020/2020_RAVG_Data/RdNBR_RAVG_2020_Fires/"), 
                         pattern = "*.tif", full.names = TRUE)

#Import those files to R: 
ravg <- lapply(ravg_files, raster)
#Set arguments for mosaic function (want highest severity value in areas that overlap, 
  # in case different time frames were used): 
ravg$fun = "max"
ravg$na.rm = TRUE

#Now merge them together into a single file: 
ravg_combo <- do.call(mosaic, c(ravg))
#Trying mosaic function so that overlapping cells can be assigned max severity
  # value - I think this is more important for the GEE processed ones 
  # e.g. Silverado and Bond fires were right next to each other and I used different
  # processing dates for them because the fires occurred at different times. 
  # Alternative to mosaic is to use the merge function, but note that if there 
  # are areas where the fire severity data overlap, only the values from the 
  # first raster will be obtained. 

plot(ravg_combo)

writeRaster(ravg_combo, filename = "InProcessData/ravg_allfires_2020_RdNBR.tif", 
            format = "GTiff", overwrite = FALSE)







#### Load and combine all GEE RdNBR rasters ####

#For the USFS GEE data, there were some fires that would be better using the 
  #offset or not using the offset, also not all fires I processed were 2020, 
  # select only 2020 fires and make sure you use appropriate offset (or not): 
# •	In most cases you’ll want to use the RdNBR with offset. This is true for: 
#   o	Rattlesnake
# o	Stagecoach
# o	Blue Jay
# o	Moraine

# •	There were a few cases where the offset didn’t seem to be working correctly due to large “fire” (?) effects right outside the fire boundaries. In these cases, I recommend using the RdNBR WITHOUT the offset. This is true for: 
#   o	ALLEN
# o	Wolf

#Select 2020 fires with offset: 
gee_files_usfs_offset <- list.files(path = paste0(geo_dir, "FireSeverity2020/2020_Fire_Severity_Missing_RAVG_USFS/"), 
                                           pattern = "*rdnbr_w_offset.tif$", 
                                           full.names = TRUE)
rattle <- gee_files_usfs_offset[grepl("*RATTLESNAKE*", gee_files_usfs_offset)]
stagecoach <- gee_files_usfs_offset[grepl("*STAGECOACH*", gee_files_usfs_offset)]
bluejay <- gee_files_usfs_offset[grepl("*BlueJay*", gee_files_usfs_offset)]
moraine <- gee_files_usfs_offset[grepl("*Moraine*", gee_files_usfs_offset)]

#Select 2020 fires without offset: 
gee_files_usfs_nooffset <- list.files(path = paste0(geo_dir, "FireSeverity2020/2020_Fire_Severity_Missing_RAVG_USFS/"), 
                                    pattern = "*rdnbr.tif$", 
                                    full.names = TRUE)
allen <- gee_files_usfs_nooffset[grepl("*ALLEN*", gee_files_usfs_nooffset)]
wolf <- gee_files_usfs_nooffset[grepl("*Wolf*", gee_files_usfs_nooffset)]


#I also processed 19 additional missing fires using GEE, primarily from W CA chaparral
  # The offset should be used for ALL of these. 
gee_geb_files <- list.files(path = paste0(geo_dir, "FireSeverity2020/2020_Additional_GEB_fires/"), 
                                          pattern = "*rdnbr_w_offset*", 
                                          full.names = TRUE)


#Combine paths for the 6 2020 fires processed with GEE for USFS: 
gee_files <- c(rattle, stagecoach, bluejay, moraine, allen, wolf, gee_geb_files)

#Import those files to R: 
gee_sev <- lapply(gee_files, raster)
#Set arguments for mosaic function (want highest severity value in areas that overlap, 
# in case different time frames were used): 
gee_sev$fun = "max"
gee_sev$na.rm = TRUE

#Now merge them together into a single file: 
gee_sev_combo <- do.call(mosaic, c(gee_sev))
#See comment above on using merge vs. mosaic

plot(gee_sev_combo)

writeRaster(gee_sev_combo, filename = "InProcessData/gee_allfires_2020_RdNBR.tif", 
            format = "GTiff", overwrite = FALSE)







#### combine RAVG and GEE RdNBR data ####

#Check projection of RAVG and GEE, likely different: 
crs(ravg_combo)
crs(gee_sev_combo)
#They are different. I will reproject the GEE severity data: 

#create template for reprojection: 
# gee_sev_combo_reproj <- projectRaster(from = gee_sev_combo, crs = crs(ravg_combo), 
#                                       res = res(ravg_combo), 
#                                       method = "bilinear")
# crs(gee_sev_combo_reproj)
# crs(ravg_combo)
# compareRaster(gee_sev_combo_reproj, ravg_combo)

#The extent of these two rasters doesn't match, and if I use projectRaster with
  # ravg_combo, several of the fires are actually lost. So, I want to find the 
  # maximum extent and extend each raster to that size: 

#First, create a template for projectRaster - this uses the CRS and resolution
  # from ravg_combo, but the extent from gee_sev_combo to create a template
gee_sev_combo_reproj2 <- projectRaster(from = gee_sev_combo, ravg_combo, 
                                       alignOnly = TRUE, method = "bilinear")
#Now, use that template to project the gee_sev_combo data: 
gee_sev_combo_reproj3 <- projectRaster(from = gee_sev_combo, 
                                       to = gee_sev_combo_reproj2, 
                                       method = "bilinear")
#The extents still don't match, but the resolution and CRS do match: 
compareRaster(gee_sev_combo_reproj3, ravg_combo)
res(gee_sev_combo_reproj3) == res(ravg_combo)
crs(gee_sev_combo_reproj3)
crs(ravg_combo)

#Try to maximize the extent of both rasters (the gee data is wider and the ravg
  #data is taller - so I want the combined greatest extent of both rasters):
xmin <- min(bbox(gee_sev_combo_reproj3)[1,1], bbox(ravg_combo)[1,1])
xmax <- max(bbox(gee_sev_combo_reproj3)[1,2], bbox(ravg_combo)[1,2])
ymin <- min(bbox(gee_sev_combo_reproj3)[2,1], bbox(ravg_combo)[2,1])
ymax <- max(bbox(gee_sev_combo_reproj3)[2,2], bbox(ravg_combo)[2,2])

#Create new extent based on largest extent of the two rasters: 
newextent = c(xmin, xmax, ymin, ymax)
extent(newextent)

#Now extend both rasters to be the size of the newextent: 
gee_sev_combo_reproj_ext <- extend(gee_sev_combo_reproj3, extent(newextent))
extent(gee_sev_combo_reproj_ext)

ravg_combo_ext <- extend(ravg_combo, extent(newextent))
extent(ravg_combo_ext)

#Compare the two rasters - did it work? Looks like it. 
compareRaster(gee_sev_combo_reproj_ext, ravg_combo_ext)


#Now, Combine RAVG with GEE RdNBR estimates to produce composite dataset: 
rdnbr_2020_allfires <- mosaic(ravg_combo_ext, gee_sev_combo_reproj_ext, 
                              fun = "max", na.rm = TRUE)

plot(rdnbr_2020_allfires)

#Save raster output with RdNBR for all 2020 fires of interest (plus a few smaller
  # ones for which RAVG was available / or that I processed for USFS): 
writeRaster(rdnbr_2020_allfires, filename = "InProcessData/rdnbr_2020_allfires.tif", 
            format = "GTiff", overwrite = FALSE)
#I double checked this in ArcMap and it looks like the extent is correct and 
  # no longer cuts off fires. Also, areas where two fires are right next to 
  # each other (Silverado and Bond, for instance), it looks like the RdNBR is
  # what I expected it to be since I used the mosaic function rather thant the 
  # merge function - allowed me to take max RdNBR rather than first raster in line. 








#### Load and Combine all RAVG dNBR rasters ####

#Get list of each .tif file that we currently have RAVG available for: 
ravg_files_dnbr <- list.files(path = paste0(geo_dir, "FireSeverity2020/2020_RAVG_Data/dNBR_RAVG_2020_Fires/"), 
                         pattern = "*.tif", full.names = TRUE)

#Import those files to R: 
ravg_dnbr <- lapply(ravg_files_dnbr, raster)
#Set arguments for mosaic function (want highest severity value in areas that overlap, 
# in case different time frames were used): 
ravg_dnbr$fun = "max"
ravg_dnbr$na.rm = TRUE

#Now merge them together into a single file: 
ravg_dnbr_combo <- do.call(mosaic, c(ravg_dnbr))

plot(ravg_dnbr_combo)

writeRaster(ravg_dnbr_combo, filename = "InProcessData/ravg_allfires_2020_dNBR.tif", 
            format = "GTiff", overwrite = FALSE)







#### Load and combine all GEE dNBR rasters ####

#For the USFS GEE data, there were some fires that would be better using the 
#offset or not using the offset, also not all fires I processed were 2020, 
# select only 2020 fires and make sure you use appropriate offset (or not): 
# •	In most cases you’ll want to use the RdNBR with offset. This is true for: 
#   o	Rattlesnake
# o	Stagecoach
# o	Blue Jay
# o	Moraine

# •	There were a few cases where the offset didn’t seem to be working correctly due to large “fire” (?) effects right outside the fire boundaries. In these cases, I recommend using the RdNBR WITHOUT the offset. This is true for: 
#   o	ALLEN
# o	Wolf

#Select 2020 fires with offset: 
gee_files_usfs_offset_dnbr <- list.files(path = paste0(geo_dir, "FireSeverity2020/2020_Fire_Severity_Missing_RAVG_USFS/"), 
                                    pattern = "*_dnbr_w_offset.tif$", 
                                    full.names = TRUE)
rattle_dnbr <- gee_files_usfs_offset_dnbr[grepl("*RATTLESNAKE*", gee_files_usfs_offset_dnbr)]
stagecoach_dnbr <- gee_files_usfs_offset_dnbr[grepl("*STAGECOACH*", gee_files_usfs_offset_dnbr)]
bluejay_dnbr <- gee_files_usfs_offset_dnbr[grepl("*BlueJay*", gee_files_usfs_offset_dnbr)]
moraine_dnbr <- gee_files_usfs_offset_dnbr[grepl("*Moraine*", gee_files_usfs_offset_dnbr)]

#Select 2020 fires without offset: 
gee_files_usfs_nooffset_dnbr <- list.files(path = paste0(geo_dir, "FireSeverity2020/2020_Fire_Severity_Missing_RAVG_USFS/"), 
                                      pattern = "*_dnbr.tif$", 
                                      full.names = TRUE)
allen_dnbr <- gee_files_usfs_nooffset_dnbr[grepl("*ALLEN*", gee_files_usfs_nooffset_dnbr)]
wolf_dnbr <- gee_files_usfs_nooffset_dnbr[grepl("*Wolf*", gee_files_usfs_nooffset_dnbr)]


#I also processed 19 additional missing fires using GEE, primarily from W CA chaparral
# The offset should be used for ALL of these. 
gee_geb_files_dnbr <- list.files(path = paste0(geo_dir, "FireSeverity2020/2020_Additional_GEB_fires/"), 
                            pattern = "*_dnbr_w_offset*", 
                            full.names = TRUE)


#Combine paths for the 6 2020 fires processed with GEE for USFS: 
gee_files_dnbr <- c(rattle_dnbr, stagecoach_dnbr, bluejay_dnbr, 
                    moraine_dnbr, allen_dnbr, wolf_dnbr, 
                    gee_geb_files_dnbr)

#Import those files to R: 
gee_sev_dnbr <- lapply(gee_files_dnbr, raster)
#Set arguments for mosaic function (want highest severity value in areas that overlap, 
# in case different time frames were used): 
gee_sev_dnbr$fun = "max"
gee_sev_dnbr$na.rm = TRUE

#Now merge them together into a single file: 
gee_sev_combo_dnbr <- do.call(mosaic, c(gee_sev_dnbr))
#See comment above on using merge vs. mosaic

plot(gee_sev_combo_dnbr)

writeRaster(gee_sev_combo_dnbr, filename = "InProcessData/gee_allfires_2020_dNBR.tif", 
            format = "GTiff", overwrite = FALSE)







#### combine RAVG and GEE dNBR data ####

#Check projection of RAVG and GEE, likely different: 
crs(ravg_dnbr_combo)
crs(gee_sev_combo_dnbr)

#They are different. I will reproject the GEE severity data: 

# Based on what I learned with the RdNBR data, I need to do a little extra
  # processing to get these two rasters in the same res, crs, extent, and origin. 

#First, create a template for projectRaster - this uses the CRS and resolution
# from ravg_dnbr_combo, but the extent from gee_sev_combo_dnbr to create a template
gee_sev_combo_dnbr_reproj_template <- projectRaster(from = gee_sev_combo_dnbr, 
                                                    to = ravg_dnbr_combo, 
                                       alignOnly = TRUE, method = "bilinear")

#Now, use that template to project the gee_sev_combo data: 
gee_sev_combo_dnbr_reproj <- projectRaster(from = gee_sev_combo_dnbr, 
                                       to = gee_sev_combo_dnbr_reproj_template, 
                                       method = "bilinear")

#The extents still don't match, but the resolution and CRS do match: 
compareRaster(gee_sev_combo_dnbr_reproj, ravg_dnbr_combo)
res(gee_sev_combo_dnbr_reproj) == res(ravg_dnbr_combo)
crs(gee_sev_combo_dnbr_reproj)
crs(ravg_dnbr_combo)

#Try to maximize the extent of both rasters (the gee data is wider and the ravg
#data is taller - so I want the combined greatest extent of both rasters):
xmin <- min(bbox(gee_sev_combo_dnbr_reproj)[1,1], bbox(ravg_dnbr_combo)[1,1])
xmax <- max(bbox(gee_sev_combo_dnbr_reproj)[1,2], bbox(ravg_dnbr_combo)[1,2])
ymin <- min(bbox(gee_sev_combo_dnbr_reproj)[2,1], bbox(ravg_dnbr_combo)[2,1])
ymax <- max(bbox(gee_sev_combo_dnbr_reproj)[2,2], bbox(ravg_dnbr_combo)[2,2])

#Create new extent based on largest extent of the two rasters: 
newextent = c(xmin, xmax, ymin, ymax)
extent(newextent)

#Now extend both rasters to be the size of the newextent: 
gee_sev_combo_dnbr_reproj_ext <- extend(gee_sev_combo_dnbr_reproj, extent(newextent))
extent(gee_sev_combo_dnbr_reproj_ext)

ravg_dnbr_combo_ext <- extend(ravg_dnbr_combo, extent(newextent))
extent(ravg_dnbr_combo_ext)

#Compare the two rasters - did it work? Looks like it. 
compareRaster(gee_sev_combo_dnbr_reproj_ext, ravg_dnbr_combo_ext)


#Now, Combine RAVG with GEE dNBR estimates to produce composite dataset: 
dnbr_2020_allfires <- mosaic(ravg_dnbr_combo_ext, gee_sev_combo_dnbr_reproj_ext, 
                              fun = "max", na.rm = TRUE)

plot(dnbr_2020_allfires)

#Save raster output with RdNBR for all 2020 fires of interest (plus a few smaller
# ones for which RAVG was available / or that I processed for USFS): 
writeRaster(dnbr_2020_allfires, filename = "InProcessData/dnbr_2020_allfires.tif", 
            format = "GTiff", overwrite = FALSE)