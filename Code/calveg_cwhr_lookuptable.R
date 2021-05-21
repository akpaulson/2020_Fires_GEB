#### Description ####

# This code creates a lookup table for the CWHR codes
# from CalVeg. I dissolved the calveg layer by cwhr_type, 
# and then exported the attribute tables as csvs. Because
# each region has different cwhr types, I need to create 
# a lookup table for rasterizing the dissolved output. 

#### Packages ####

library(dplyr)

# Set Geospatial Data Directory (wherever you saved data downloaded from Box): 
geo_dir <- "../Geospatial_Data/"


#### Data ####
#Get list of files: 
files <- list.files(path = paste0(geo_dir, "CalVeg/ProcessedOutput/LookupTables/"), 
           pattern = "*.csv$", full.names = TRUE)
#read .csv files in, confirm there are 11 unique files: 
cwhr <- lapply(files, read.csv)

#Bind the rows together
cwhr <- do.call(rbind, cwhr)

cwhr2 <- cwhr %>% 
  select(-`ï..OID_`) %>% 
  distinct() %>% 
  mutate(cwhr = 1:length(unique(CWHR_TYPE)))

#### save lookup table ####

write.csv(cwhr2, paste0(geo_dir, "CalVeg/ProcessedOutput/LookupTables/cwhr_lookup.csv"), 
          row.names = FALSE)
