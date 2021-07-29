#### Description ####

# This code will take the 90 m grid dataset(s) that I prepared that have different
# spatial variables attached to them and prepare it for data analysis. This 
# preparation will include removing points where the fire severity is NA, where
# the day of burn is NA (and thus there are no weather variables), or where the
# FRID data is NA. I will also remove a couple of very late season fires that
# did not have quality severity imagery (The BOND and MOUNTAINVIEW fires).  
# 
# Next, I will assess the different vegetation types available in the dataset and
# break apart the dataset for analysis based on different vegetation types. 

#### Packages ####

library(sf)
library(dplyr)
library(ggplot2)
library(readxl)

#### Data ####

#All three of these datasets are the same set of grid points, with 90 m spacing 
  # across the 40 largest fires in 2020. They have different sets of spatial
  # data associated with them: 

# These shapefiles are huge and take up a lot of memory. 

#This dataset has all of the FRID, CAlVeg, Landfire, and severity data attached, 
  # it was produced in the extract_veg_tslf_sev.R code:
grid_90_veg <- st_read("InProcessData/grid_90_veg_sev_eco.shp")

#This dataset has all of the gridMET weather data attached to it, it was 
  # produced in the extract_gridMET.R code: 
grid_90_gmet <- st_read("InProcessData/grid_90_grmt.shp")

#This dataset has all of the aerial detection survey mortality attached,
  # produced in the extract_mortality.R code: 
grid_90_mort <- st_read("InProcessData/grid_90_mortality.shp")

#Match the projections of the 2 datasets: 
grid_90_gmet <- st_transform(grid_90_gmet, st_crs(grid_90_veg))
st_crs(grid_90_gmet) == st_crs(grid_90_veg)
st_crs(grid_90_gmet) == st_crs(grid_90_mort)
st_crs(grid_90_mort) == st_crs(grid_90_veg)



#Join the three datasets together: 
#have to drop the geometery of one of them to do so: 
grid_90 <- left_join(grid_90_veg, 
                   grid_90_gmet %>% as.data.frame() %>% select(-geometry), 
                   by = c("grid_id", "AGENCY", "FIRE_NA", "INC_NUM",
                          "ALARM_D", "CONT_DA", "CAUSE")) %>% 
  left_join(., grid_90_mort %>% as.data.frame() %>%  select(-geometry), 
            by = c("grid_id", "AGENCY", "FIRE_NA", "INC_NUM",
                   "ALARM_D", "CONT_DA", "CAUSE"))

grid90 <- grid_90 %>% 
  rename(mrt1216 = m__12_1)
#remove layers to save mem: 
rm(grid_90_gmet)
rm(grid_90_veg)
rm(grid_90_mort)


#### Pull out different cwhr types for Hugh to put into different classes ####
cwhr <- grid_90 %>% 
  as.data.frame(.) %>% 
  group_by(cwhr) %>% 
  tally()

#write.csv(cwhr, "InProcessData/cwhr_tally.csv", row.names = FALSE)

#Hugh classified each cwhr type from the calveg dataset into fewer categories 
  #that we can use for analysis. Bring in this dataset and combine it to the 
  #overall dataset: 

cwhr_groups <- read_excel("InProcessData/WHR types_Hugh.xlsx") %>% 
  select(-n) %>% 
  rename(cwhr_name = name, 
         cwhr_group = `New groups`)

grid_90_cwhr <- grid_90 %>% 
  left_join(., cwhr_groups, by = "cwhr") 


slice_sample(grid_90_cwhr, n=20)

#### Remove NAs and problematic fires ####

#The SLATER fire goes into Oregon, and the calveg and PFR datasets do 
# not cover Oregon. I thought it was only a problem in the weather data
# and I fixed that - but these veg datasets do not go beyond the CA boundary
# (Landfire does). So, we will just have to exclude those points from the 
# SLATER fire that are in Oregon and don't have PFR/Calveg data available. 



# There are several steps to take to remove datapoints with NAs, etc: 

grid_90_cull <- grid_90_cwhr %>% 
  rename(cwhr_nm = cwhr_name, 
         cwhr_gp = cwhr_group) %>% 
  #Filter out BOND and MOUNTAIN VIEW fires which were late season fires and
    # had potentially problematic severity imagery: 
  filter(!(FIRE_NA %in% c("BOND", "MOUNTAIN VIEW"))) %>% 
  #remove points where severity rdnbr estimate is NA (this is response variable):
  filter(!is.na(rdnbr)) %>% 
  #remove points where weather variables aren't available: 
  filter(!is.na(bi)) %>% 
  #remove points where PFR/TSLF data aren't available (Slater fire in OR): 
  filter(!is.na(TSLF)) %>% 
  #remove NA, ag, barren, developed, and water, based on cwhr classifications from
    #calveg - classified as "DROP" in the cwhr groupings: 
  filter(!cwhr_gp == "DROP") %>% 
  filter(!is.na(cwhr_gp))


#how many cwhr?
grid_90_cull %>% 
  as.data.frame(.) %>% 
  group_by(cwhr) %>% 
  tally()

#How many cwhr groups?
grid_90_cull %>% 
  as.data.frame(.) %>% 
  group_by(cwhr_gp) %>% 
  tally()


#### Save complete dataset ####

st_write(grid_90_cull, "CleanData/grid_90_clean.shp")



#### Pull out yellow pine and mixed conifer for ESA for Ali ####

# grid_90_esa <- grid_90_cull %>% 
#   filter(cwhr_grp %in% c("Yellow pine", "Mixed conifer"))
# 
# st_write(grid_90_esa, "../../Davis Research/Management-and-Fire-Behavior/Working/grid_90_esa.shp")
