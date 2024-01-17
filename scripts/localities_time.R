# Tue Jan 16 10:50:02 2024 ------------------------------
# script to check how localities change overtime

#Libraries ----
library(readxl)
library(dplyr)
library(writexl)

#Data----
read_xlsx("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/raw/GIS/localities_points/RESLOC2000 - 01 Aguascalientes.xlsx") -> loc2000_ags
read_xls("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/raw/GIS/localities_points/ITER_01XLS05.xls") -> loc2005_ags

##organization----
convert_to_decimal_degrees <- function(number) { #function to convert to decimal degree 
  degrees <- number %/% 10000
  minutes <- (number %% 10000) %/% 100
  seconds <- number %% 100
  
  # Convert to decimal degrees
  decimal_degrees <- degrees + minutes / 60 + seconds / 3600
  
  return(decimal_degrees)
}


loc2000_ags %>%
  filter(Longitud != 'NA') %>% 
  mutate(across(.cols = Longitud:Latitud, .fns = as.numeric),
         across(.cols = Longitud:Latitud, .fns = convert_to_decimal_degrees),
         Longitud = Longitud*-1) %>% 
  glimpse -> loc2000_ags_decdegree

plot(x = loc2000_ags_decdegree$Longitud, y = loc2000_ags_decdegree$Latitud)

loc2005_ags %>% 
  dplyr::select(1:10) %>% 
  filter(LONGITUD !='NA') %>% 
  mutate(across(.cols = LONGITUD:LATITUD, .fns = as.numeric),
         across(.cols = LONGITUD:LATITUD, .fns = convert_to_decimal_degrees),
         LONGITUD = LONGITUD*-1) %>% 
  glimpse -> loc2005_ags_decdegree

plot(x = loc2005_ags_decdegree$LONGITUD, y = loc2005_ags_decdegree$LATITUD)

##export----
write.csv(x = loc2000_ags_decdegree, file = "/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/GIS/localities_points/loc2000_ags_decdegree.csv")
write.csv(x = loc2005_ags_decdegree, file = "/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/GIS/localities_points/loc2005_ags_decdegree.csv")
