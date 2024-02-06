# Tue Feb  6 15:47:49 2024 ------------------------------
#Script to build full table for mexico-IRSL analysis

#Library----
library(sf)
library(dplyr)

#data----
read_sf("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/GIS/thiessen_IRSL_all_ITRF92-LCC.shp") -> thiessen

##organization----
thiessen %>% 
  glimpse
