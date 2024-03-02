# Sat Mar  2 11:35:02 2024 ------------------------------
#Script to organize data for IRSL analysis----

#Libraries----
library(readxl)
library(here)
library(dplyr)
library(tidyr)
library(stringr)

#Data----
read_xlsx(path = here("data/thiessen_census_2020_LC_00_20.xlsx"))-> thiessen_LC
read.csv("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/GIS/localities_points/locality_IRSL_points.csv") -> IRSL_full
read_xlsx(path = "/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Mexico/clean/thiessen_ejidos.xlsx") -> thiessen_ejidos

#organization----
##forest----
thiessen_LC %>% 
  rename_with(function(x) str_replace_all(x, "\\s", ".")) %>%
  rename_with(function(y) str_replace_all(y, "-", ".")) %>% 
  pivot_longer(cols = 12:length(.),
             names_to = c(".value", "year"),
             names_sep = "_") %>%
  mutate(across(.col = 12:length(.),
                ~ replace_na(data = ., 0))) %>% 
  mutate(forest = rowSums(select(., starts_with(c("bosque","selva")))),
         non_forest = area_m - forest,
         across(.cols = 213:214, ~ . /10000)) %>% 
  select(state_code, municipality_code, locality_code, altitud, area_m, year, forest, non_forest) %>% 
  mutate(area_ha = area_m/10000,
         across(.cols = where(is.character), .fns = as.factor),
         forest_perc = (forest/area_ha)*100,
         non_forest_perc = (non_forest/area_ha)*100
         ) %>% 
  glimpse -> thiessen_forest

##IRSL----
IRSL_full %>% 
  select(locality_code, AMBITO, pop_2000:pop_2020, IRSL_2000:GRSL_2020) %>% 
  pivot_longer(cols = 3:length(.),
               names_to = c(".value", "year"),
               names_sep = "_") %>% 
  mutate(across(.cols = where(is.character), .fns = as.factor),
         locality_code = as.factor(locality_code),
         population = as.double(pop)) %>%
  right_join(y = thiessen_forest, by = c("locality_code", "year")) %>% 
  glimpse -> thiessen_outcomes

##Ejidos----
thiessen_ejidos %>% 
  mutate(across(.cols = where(is.character), .fns = as.factor)) %>% 
  rename(locality_code = locality_c,
         ejido_code = Clv_Unica,
         programa = PROGRAMA,
         area_ejido_ha = area_ha) %>% 
  group_by(locality_code, tipo) %>% 
  summarise(area_ejido_ha = sum(area_ejido_ha)) %>% 
  left_join(x = thiessen_outcomes, y =., by = "locality_code") %>% 
  mutate(area_ejido_perc = (area_ejido_ha/area_ha)*100) %>% 
  select(-pop, -area_m) %>% 
  rename(zone = AMBITO,
         forest_area_ha = forest,
         non_forest_area_ha = non_forest,
         thiesse_area_ha = area_ha,
         forest_area_perc = forest_perc,
         non_forest_area_perc = non_forest_perc,
         kind_ejido = tipo,
         ejido_area_ha = area_ejido_ha,
         ejido_area_perc = area_ejido_perc) %>% 
  glimpse -> localities_analysis

##covariates----