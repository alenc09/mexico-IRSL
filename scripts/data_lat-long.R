# Wed Nov 22 18:07:59 2023 ------------------------------
# Script to organize and check data availability to build th IML for 2010 and before

#libraries----
library(readxl)
library(here)
library(dplyr)
library(sf)
library(sp)
library(raster)
library(landscapemetrics)
library(writexl)
library(foreign)

#data----
read_xlsx(path = "/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/statistical/IRSL_localidades_00_20_clean.xlsx", sheet = 1) -> IRSL_1
read_xlsx(path = "/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/statistical/IRSL_localidades_00_20_clean.xlsx", sheet = 2) -> IRSL_2
read.csv(file = "/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/statistical/localidad_locations_clean.csv") -> locality_points
read.csv(file = "/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/raw/Statistical/censo_2020/ITER_NALCSV20.csv") -> localities_2020
read.dbf("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/raw/Statistical/censo_2000/ITER_NALDBF00.dbf", as.is = T) -> localidades_2000
read.dbf("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/raw/Statistical/conteo_2005/ITER_NALDBF05.dbf", as.is = T) -> localidades_2005
read.dbf("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/raw/Statistical/censo_2010/ITER_NALDBF10 2.dbf", as.is = T) -> localidades_2010

##organization----
# IRSL_1 %>% 
#   rbind(IRSL_2) %>% 
#   mutate(locality_code = as.integer(locality_code)) %>%
#   left_join(y = dplyr::select(locality_points, locality_code, AMBITO, LAT_DECIMAL, LON_DECIMAL, ALTITUD, TOTAL.DE.VIVIENDAS.HABITADAS),
#             by = "locality_code") %>%
#   rename("viviendas_2020" = "TOTAL.DE.VIVIENDAS.HABITADAS" ) %>%
#   mutate(locality_code = as.factor(locality_code),
#          across(.cols = c(GRSL_2000:AMBITO), .fns = as.factor),
#          viviendas_2020 = as.double(viviendas_2020)) %>%
#   glimpse -> IRSL_full

convert_to_decimal_degrees <- function(number) {
  degrees <- number %/% 10000
  minutes <- (number %% 10000) %/% 100
  seconds <- number %% 100
  
  # Convert to decimal degrees
  decimal_degrees <- degrees + minutes / 60 + seconds / 3600
  
  return(decimal_degrees)
}

###Localities' points 2000----
localidades_2000 %>% 
  dplyr::select(ENTIDAD, MUN, LOC, LATITUD, LONGITUD) %>% 
  filter(!LOC == "0000" , !LOC == "9999" , !LOC == "9998") %>% 
  unite(col = "locality_code", ENTIDAD,MUN,LOC, sep = "") %>% 
  mutate(across(.cols = 2:3, .fns = as.numeric),
         across(.cols = 2:3, .fns = convert_to_decimal_degrees),
         LONGITUD = if_else(locality_code == 202770101, true = 97.01002944,
                            false = if_else(locality_code == 220120011,
                                            true = 100.15895833,
                                            false = LONGITUD)),
         LATITUD = if_else(locality_code == 202770101, true = 16.53714250,
                            false = if_else(locality_code == 220120011,
                                            true = 20.43633278,
                                            false = LATITUD)),
         lat_dec = LATITUD,
         long_dec = LONGITUD*-1) %>%
  # filter(is.na(LATITUD)) %>%
  glimpse %>% 
  write.csv(file = "/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/statistical/localidades_census_2000.csv", x = .,)

###localities points 2005---
localidades_2005 %>% 
  dplyr::select(ENTIDAD, MUN, LOC, LATITUD, LONGITUD) %>% 
  filter(!LOC == "0000" , !LOC == "9999" , !LOC == "9998") %>%
  unite(col = "locality_code", ENTIDAD,MUN,LOC, sep = "") %>%
  mutate(across(.cols = 2:3, .fns = as.numeric),
        across(.cols = 2:3, .fns = convert_to_decimal_degrees),
        lat_dec = LATITUD,
        long_dec = LONGITUD*-1,
         .keep = "unused") %>%
  glimpse %>% 
  write.csv(file = "/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/statistical/localidades_census_2005.csv", x = .,)

###localities points 2010----
localidades_2010 %>% 
  dplyr::select(ENTIDAD, MUN, LOC, LATITUD, LONGITUD) %>% 
  filter(!LOC == "0000" , !LOC == "9999" , !LOC == "9998") %>%
  unite(col = "locality_code", ENTIDAD,MUN,LOC, sep = "") %>%
  mutate(across(.cols = 2:3, .fns = as.numeric),
         across(.cols = 2:3, .fns = convert_to_decimal_degrees),
         lat_dec = LATITUD,
         long_dec = LONGITUD*-1,
         .keep = "unused") %>%
  glimpse %>% 
  write.csv(file = "/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/statistical/localidades_census_2010.csv", x = .,)
