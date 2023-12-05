# Wed Nov 22 18:07:59 2023 ------------------------------
# Script to organize and check data availability to build th IML for 2010 and before

#libraries----
library(readxl)
library(here)
library(dplyr)
library(deldir)
library(sf)
library(sp)

#data----
read_xlsx(path = here("data/IRSL_localidades_00_20_clean.xlsx"), sheet = 1) -> IRSL_1
read_xlsx(path = here("data/IRSL_localidades_00_20_clean.xlsx"), sheet = 2) -> IRSL_2
read.csv(file = "/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/statistical/localidad_locations_clean.csv") -> locality_points

##organization----
IRSL_1 %>% 
  rbind(IRSL_2) %>% 
  mutate(locality_code = as.integer(locality_code)) %>% 
  left_join(y = select(locality_points, locality_code, AMBITO, LAT_DECIMAL, LON_DECIMAL, ALTITUD, TOTAL.DE.VIVIENDAS.HABITADAS), 
            by = "locality_code") %>% 
  rename("viviendas_2020" = "TOTAL.DE.VIVIENDAS.HABITADAS" ) %>% 
  mutate(locality_code = as.factor(locality_code),
         across(.cols = c(GRSL_2000:AMBITO), .fns = as.factor),
         viviendas_2020 = as.double(viviendas_2020)) %>% 
  glimpse -> IRSL

###Thiessen polygons----
IRSL %>% 
  filter(LAT_DECIMAL != 'NA') %>% 
  st_as_sf(coords = c('LON_DECIMAL', 'LAT_DECIMAL')) %>%
  st_set_crs('6364') %>%
  st_union %>% 
  glimpse -> points_IRSL

st_voronoi(points_IRSL) -> thiessen_IRSL

plot(thiessen_IRSL)

deldir(x = na.omit(IRSL$LON_DECIMAL), y = na.omit(IRSL$LAT_DECIMAL)) -> thiessen_IRSL
plot(thiessen_IRSL)
