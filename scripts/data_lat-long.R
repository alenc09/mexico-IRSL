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

#data----
read_xlsx(path = "/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/statistical/IRSL_localidades_00_20_clean.xlsx", sheet = 1) -> IRSL_1
read_xlsx(path = "/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/statistical/IRSL_localidades_00_20_clean.xlsx", sheet = 2) -> IRSL_2
read.csv(file = "/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/statistical/localidad_locations_clean.csv") -> locality_points
read.csv(file = "/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/raw/Statistical/censo_2020/ITER_NALCSV20.csv") -> localities_2020

##organization----
IRSL_1 %>% 
  rbind(IRSL_2) %>% 
  mutate(locality_code = as.integer(locality_code)) %>%
  left_join(y = dplyr::select(locality_points, locality_code, AMBITO, LAT_DECIMAL, LON_DECIMAL, ALTITUD, TOTAL.DE.VIVIENDAS.HABITADAS),
            by = "locality_code") %>%
  rename("viviendas_2020" = "TOTAL.DE.VIVIENDAS.HABITADAS" ) %>%
  mutate(locality_code = as.factor(locality_code),
         across(.cols = c(GRSL_2000:AMBITO), .fns = as.factor),
         viviendas_2020 = as.double(viviendas_2020)) %>%
  glimpse -> IRSL_full

rowSums(!is.na(IRSL_full[,c("IRSL_2000","IRSL_2005","IRSL_2010","IRSL_2020")])) -> count
IRSL_full[count>=2,] -> IRSL


###Localities' points----
localities_2020 %>% 
  glimpse
