# Fri Mar  1 16:18:28 2024 ------------------------------
#Script to estimate missing data in IRSL table

#library----
library(sf)
library(dplyr)
library(GGally)

#data---
read_sf("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/GIS/localities_points/localities_2020_mosaiks.shp") -> localities_2020_mosaiks
read_sf("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/GIS/thiessen_2020_mosaiks.shp") -> thiessen_2020_mosaiks
read.csv("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/GIS/localities_points/locality_IRSL_points.csv") -> IRSL_full

#Analysis----
##organization----
localities_2020_mosaiks %>% 
  mutate(locality_code = as.factor(locality_c),
         mosaiks_HDI = mosaiks_me,
         .keep = "none") %>%
  glimpse -> localities_2020_mosaiks

thiessen_2020_mosaiks %>% 
  select(locality_c, `_mean`) %>% 
  mutate(locality_code = as.factor(locality_c),
         mosaiks_HDI = `_mean`,
         .keep = "none") %>% 
  glimpse -> thiessen_2020_mosaiks

###point-based estimation with mosaiks----
IRSL_full %>% 
  select(locality_code, IRSL_2000:IRSL_2020) %>% 
  mutate(locality_code = as.factor(locality_code)) %>% 
  right_join(y = localities_2020_mosaiks, x = ., by = "locality_code") %>% 
  glimpse %>% 
  ggpairs(columns = 2:6, title = "point-based HDI")

###area-based estimation with mosaiks----
IRSL_full %>% 
  select(locality_code, IRSL_2000:IRSL_2020) %>% 
  mutate(locality_code = as.factor(locality_code)) %>% 
  right_join(y = thiessen_2020_mosaiks, x = ., by = "locality_code") %>% 
  glimpse %>% 
  ggpairs(columns = 2:6, title = "area-based HDI")


