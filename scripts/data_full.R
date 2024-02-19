# Tue Feb  6 15:47:49 2024 ------------------------------
#Script to build full table for mexico-IRSL analysis

#Library----
library(sf)
library(dplyr)
library(tidyr)

#data----
read_sf("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/GIS/thiessen_IRSL_all_ITRF92-LCC.shp") -> thiessen
read_sf("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/GIS/thiessen_LC_2000.shp") -> thiessen_LC_2000
read_sf("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/GIS/thiessen_LC_2005.shp") -> thiessen_LC_2005
read_sf("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/GIS/thiessen_LC_2010.shp") -> thiessen_LC_2010
read_sf("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/GIS/thiessen_LC_2020.shp") -> thiessen_LC_2020

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

modified_list <- lapply(list_LC, apply_code)



