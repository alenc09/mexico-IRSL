# Tue Feb  6 15:47:49 2024 ------------------------------
#Script to build full table for mexico-IRSL analysis

#Library----
library(sf)
library(dplyr)
library(tidyr)

#data----
read_sf("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/GIS/thissen_all.shp") -> thiessen_all
read_sf("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/GIS/thiessen_all_LC_2000.shp") -> thiessen_LC_2000
read_sf("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/GIS/thiessen_all_LC_2005.shp") -> thiessen_LC_2005
read_sf("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/GIS/thiessen_all_LC_2010.shp") -> thiessen_LC_2010
read_sf("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/GIS/thiessen_all_LC_2020.shp") -> thiessen_LC_2020

##organization----
###land cover----
list(thiessen_LC_2000, thiessen_LC_2005, thiessen_LC_2010, 
     rename(.data = thiessen_LC_2020, descripcio = DESCRIPCIO)) -> list_LC

apply_code <- function(dataframe) {
  # Apply pivot_wider and replace NA with 0
  thiessen_LC_ <- dataframe %>%
    as.data.frame() %>% 
    pivot_wider(id_cols = "locality_c",
                names_from = c("descripcio", "year"),
                values_from = "area_m",
                values_fn = sum) %>%
    mutate(across(.col = 2:length(.),
                  ~ replace_na(data = ., 0)))
  # Return the modified dataframe and year for naming
  return(list(dataframe = thiessen_LC_))
}

lapply(list_LC, apply_code) -> modified_list

modified_list[[1]][[1]]-> thiessen_LC_2000_wider
modified_list[[2]][[1]]-> thiessen_LC_2005_wider
modified_list[[3]][[1]]-> thiessen_LC_2010_wider
modified_list[[4]][[1]]-> thiessen_LC_2020_wider

thiessen_all %>% 
  unite(col = municipality_code, ENTIDAD, MUN, sep = "", remove = F) %>% 
  mutate(altitud = as.double(ALTITUD),
         state_code = as.factor(ENTIDAD),
         municipality_code = as.factor(municipality_code),
         locality_code = as.factor(locality_c),
         .keep = "unused") %>% 
  select(state_code, NOM_ENT, municipality_code, NOM_MUN, locality_code, NOM_LOC,
         -LOC, -LONGITUD, -LATITUD, x, y, altitud, area_m) %>% 
  rename(state_name = NOM_ENT,
         municipality_name = NOM_MUN,
         locality_name = NOM_LOC,
         long_dec = x,
         lat_dec = y) %>% 
  glimpse
