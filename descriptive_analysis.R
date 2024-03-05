# Mon Mar  4 15:18:52 2024 ------------------------------
#Script to estimate missing population and localities from analysis

#Libraries----
library(foreign)

#data----
read.dbf("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/raw/Statistical/censo_2000/ITER_NALDBF00.dbf") -> localidades_2000

##organization----
localidades_2000 %>% 
  select(ENTIDAD, MUN, LOC, POBTOT)
  glimpse

#Analysis----
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