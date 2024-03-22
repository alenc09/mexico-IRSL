# Mon Mar  4 15:18:52 2024 ------------------------------
#Script to estimate missing population and localities from analysis

#Libraries----
library(foreign)
library(forcats)
library(dplyr)
library(tidyr)
library(stringr)
library(sf)

#data---sf#data----
read.dbf("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/raw/Statistical/censo_2000/ITER_NALDBF00.dbf", as.is = T) -> localidades_2000
read.dbf("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/raw/Statistical/conteo_2005/ITER_NALDBF05.dbf") -> localidades_2005
read.dbf("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/raw/Statistical/censo_2010/ITER_NALDBF10 2.dbf", as.is = T) -> localidades_2010
read.csv("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/raw/Statistical/censo_2020/ITER_NALCSV20.csv", 
         colClasses = c("ENTIDAD" = "factor",
                        "MUN" = "factor",
                        "LOC" = "factor")) -> localidades_2020
read.csv("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/GIS/localities_points/locality_IRSL_points.csv") -> IRSL_full
read_sf("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/GIS/localities_points/localities_2000_ambito.shp") -> localidades_ambito_2000
read_sf("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/GIS/localities_points/localities_2005_ambito.shp") -> localidades_ambito_2005
read_sf("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/GIS/localities_points/localities_2010_ambito.shp") -> localidades_ambito_2010
read_sf("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/GIS/localities_points/localities_2020_ambito.shp") -> localidades_ambito_2020

##organization----
localidades_2000 %>%
  select(ENTIDAD, MUN, LOC, POBTOT) %>% 
  filter(!LOC == "0000" , !LOC == "9999" , !LOC == "9998") %>% 
  glimpse -> localidades_pop_2000

localidades_2005 %>% 
  dplyr::select(ENTIDAD, MUN, LOC, P_TOTAL) %>% 
  filter(!LOC == "0000" , !LOC == "9999" , !LOC == "9998") %>% 
  glimpse -> localidades_pop_2005

localidades_2010 %>% 
  dplyr::select(ENTIDAD, MUN, LOC, POBTOT) %>% 
  filter(!LOC == "0000" , !LOC == "9999" , !LOC == "9998") %>% 
  glimpse -> localidades_pop_2010

localidades_2020 %>% 
  select(ENTIDAD, MUN, LOC, POBTOT) %>% 
  filter(!LOC == "0000" , !LOC == "9999" , !LOC == "9998") %>% 
  glimpse -> localidades_pop_2020

IRSL_full %>% 
  select(locality_code, AMBITO) %>% 
  mutate(locality_code = as.character(locality_code),
         ambito = as.factor(AMBITO),
         locality_code = str_pad(.$locality_code, width = 9, side = "left", pad = "0"),
         locality_code = as.factor(locality_code), 
         .keep = "unused") %>% 
  glimpse -> localities_ambito

localidades_ambito_2000 %>% 
  mutate(locality_code = str_pad(.$locality_c, width = 9, side = "left", pad = "0"),
         across(.cols = c("locality_code", "ambito"), .fns = as.factor),
         .keep = "none") %>% 
  glimpse -> localidades_ambito_2000

localidades_ambito_2005 %>% 
  mutate(locality_code = str_pad(.$locality_c, width = 9, side = "left", pad = "0"),
         across(.cols = c("locality_code", "ambito"), .fns = as.factor),
         .keep = "none") %>% 
  glimpse -> localidades_ambito_2005

localidades_ambito_2010 %>% 
  mutate(locality_code = str_pad(.$locality_c, width = 9, side = "left", pad = "0"),
         across(.cols = c("locality_code", "ambito"), .fns = as.factor),
         .keep = "none") %>% 
  glimpse -> localidades_ambito_2010

localidades_ambito_2020 %>% 
  st_drop_geometry() %>% 
  mutate(across(.cols = everything(.), .fns = as.factor)) %>% 
  glimpse -> localidades_ambito_2020

#Analysis----
##rural localities----
localidades_pop_2000 %>% 
  unite(col = "locality_code", ENTIDAD,MUN,LOC, sep = "") %>%
  mutate(locality_code = as.factor(locality_code),
         POBTOT = as.numeric(POBTOT)) %>%
  left_join(y = localidades_ambito_2000, by = "locality_code") %>%
  filter(ambito == "rural") %>%
  summarise(total_pop = sum(POBTOT)) %>%
  glimpse

localidades_pop_2005 %>% 
  unite(col = "locality_code", ENTIDAD,MUN,LOC, sep = "") %>%
  mutate(locality_code = as.factor(locality_code),
         P_TOTAL = as.numeric(P_TOTAL)) %>%
  left_join(y = localidades_ambito_2005, by = "locality_code") %>%
  filter(ambito == "rural") %>%
  summarise(total_pop = sum(P_TOTAL)) %>%
  glimpse

localidades_pop_2010 %>% 
  unite(col = "locality_code", ENTIDAD,MUN,LOC, sep = "") %>%
  mutate(locality_code = as.factor(locality_code),
         POBTOT = as.numeric(POBTOT)) %>%
  left_join(y = localidades_ambito_2010, by = "locality_code") %>%
  filter(ambito == "rural") %>%
  summarise(total_pop = sum(POBTOT)) %>%
  glimpse

localidades_pop_2020 %>% 
  unite(col = "locality_code", ENTIDAD,MUN,LOC, sep = "") %>%
  mutate(locality_code = as.factor(locality_code),
         POBTOT = as.numeric(POBTOT)) %>%
  left_join(y = localidades_ambito_2020, by = c("locality_code" = "locality_c")) %>%
  filter(Ambito == "Rural") %>%
  summarise(total_pop = sum(POBTOT)) %>%
  glimpse

## Localities in forest biomes----

##Localities with IRSL----
rowSums(!is.na(st_drop_geometry(thiessen_census_2020_IRSL[,c("IRSL_2000","IRSL_2005","IRSL_2010","IRSL_2020")]))) -> count

thiessen_census_2020_IRSL[count>=2,] -> thiessen_census_2020_IRSL_analysis
thiessen_census_2020_IRSL_analysis %>% 
  mutate(across(.cols = GRSL_2000:GRSL_2020, .fns = as.factor),
         AMBITO = as.factor(AMBITO),
         viviendas_2020 = as.numeric(viviendas_2020)) %>% 
  select(locality_code, long_dec, lat_dec, pop_2000:pop_2020, AMBITO, viviendas_2020, GRSL_2000:GRSL_2020) %>% 
  summary()
glimpse

## Localities without  IRSL----  
thiessen_census_2020_IRSL[count<2,] -> thiessen_census_2020_IRSLmiss
thiessen_census_2020_IRSLmiss %>% 
  mutate(across(.cols = GRSL_2000:GRSL_2020, .fns = as.factor),
         AMBITO = as.factor(AMBITO),
         viviendas_2020 = as.numeric(viviendas_2020)) %>% 
  select(locality_code, long_dec, lat_dec, pop_2000:pop_2020, AMBITO, viviendas_2020, GRSL_2000:GRSL_2020) %>% 
  summary()
glimpse

##national census----
