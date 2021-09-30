#### Description ####

# This code will calculate the percent of each fire area that
# burned at high severity. We have initial estimates of this based
# on our point grid, but we would like to include area based 
# calculations for the paper.  


#### Packages ####
library(dplyr)
library(sf)
library(raster)
library(units)

#### Function to extract severity raster within the buffer:####

sev_extract <- function(severity_raster, fire) {
  #1. Crop and mask Severity layer
  sev_inside <- crop(severity_raster, fire)
  sev_inside <- mask(sev_inside, fire)
  
  #Save data frame of raster for later processing, severity classification
   # based on initial RdNBR estimates, from Table 2 in Lydersen 2018: 
  sev_inside_df <- as.data.frame(sev_inside, xy=TRUE) %>% 
    rename(ravg =3) %>%
    filter(!is.na(ravg)) %>% 
    mutate(ravg_reclass = case_when(
      ravg < 79 ~ "Unchanged", 
      ravg >= 79 & ravg <360 ~ "Low", 
      ravg >= 360 & ravg <732 ~ "Moderate", 
      ravg >= 732~ "High", 
      TRUE ~ NA_character_)) %>% 
    mutate(ravg_reclass_combo = case_when(
      ravg < 360 ~ "Unchanged/Low", 
      ravg >= 360 & ravg <732 ~ "Moderate", 
      ravg >= 732~ "High", 
      TRUE ~ NA_character_)) %>% 
    mutate(ravg_reclass = factor(ravg_reclass, 
                                 levels = c("Unchanged", "Low", "Moderate", "High"))) %>% 
    mutate(ravg_reclass_combo = factor(ravg_reclass_combo, 
                                       levels = c("Unchanged/Low", "Moderate", "High")))
  return(sev_inside_df)
}  



#### Data ####

#bring in dataset of fire perimeters for the 40 largest 2020 fires
#this dataset developed with the code get_fire_perim_make_90m_grid.R
perim <- read_sf("InProcessData/perim_2020_top40.shp")


# Bring in 2020 fire severity raster (initial fire severity estimates). 
# Produced with the combine_severity_rasters.R code

sev <- raster("InProcessData/rdnbr_2020_allfires.tif")

# check projections: 
st_crs(perim)[1] == proj4string(sev) #no
perim2 <- st_transform(perim, crs = proj4string(sev))
st_crs(perim2)[1] == proj4string(sev)


#### Area of Fire Perimeters ####

#What is the area of each fire? 
# Calculate treatment area for each polygon: 
perim3 <- perim2 %>% 
  mutate(burn_area_m2 = st_area(.),
         burn_area_ha = burn_area_m2 * 0.0001, 
         burn_area_acre = burn_area_ha * 2.47105381) %>%
  #remove units from fire_area_ha so that I can filter it
  mutate(burn_area_ha = set_units(burn_area_ha, NULL)) %>% 
  mutate(burn_area_acre = set_units(burn_area_acre, NULL))

### Note: GIS acres from FRAP do not match this new 
# calculation of shape area, but it's very close. 
glimpse(perim3)


#### Calculate area of each severity level ####

# Foreloop to Extract severity for each buffer

#Create empty data frame: 
sev_2020_fires <- data.frame()


for (i in 1:nrow(perim3)) {
  #for (i in 1:100) {
  
  #Select single buffer polygon
  single_fire <- perim3 %>% 
    filter(FIRE_NAME == perim3$FIRE_NAME[i]) 
  
  #Run function to extract raster info for each buffer:
  a <- sev_extract(fire = single_fire, severity_raster = sev)
  
  # Calculate proportion of different severity levels
  
  prop_sev <- a %>% 
    group_by(ravg_reclass, .drop = FALSE) %>% 
    tally() %>% 
    mutate(prop = n/sum(n)*100)
  
  prop_low_unchange_combo <- a %>% 
    group_by(ravg_reclass_combo, .drop = FALSE) %>% 
    tally() %>% 
    mutate(prop = n/sum(n)*100)
  
  #Put data into a data.frame
  data_summary <- data.frame(FIRE_NAME = perim3$FIRE_NAME[i], 
                             perc_unchanged = prop_sev$prop[prop_sev$ravg_reclass == "Unchanged"], 
                             perc_low = prop_sev$prop[prop_sev$ravg_reclass == "Low"], 
                             perc_low_unchanged = prop_low_unchange_combo$prop[prop_low_unchange_combo$ravg_reclass_combo == "Unchanged/Low"], 
                             perc_mod = prop_sev$prop[prop_sev$ravg_reclass == "Moderate"], 
                             perc_high = prop_sev$prop[prop_sev$ravg_reclass == "High"])
  
  #Bind to the sev_metrics dataframe: 
  sev_2020_fires <- rbind(sev_2020_fires, data_summary)
}


sev_2020_fires2 <- sev_2020_fires %>% 
  left_join(., as.data.frame(perim3)[ , c("FIRE_NAME", "GIS_ACRES", "burn_area_acre")], 
            by = "FIRE_NAME")

#Save results: 
write.csv(sev_2020_fires2, file = "Results/severity_proportion.csv", row.names = FALSE)
  #GIS Acres is the acreage reported in the FRAP dataset
  # burn_area_acre is the acreage that I calculated in R. 
  # They are fairly close, but differ for a few fires. 

