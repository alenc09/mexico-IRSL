# Wed Nov 22 15:02:57 2023 ------------------------------
#script to create the "índice de marginación" at the locality level in Mexico

#libraries----
library(readxl)
library(dplyr)
library(here)
library(p2distance)

#data----
read_xls("data/IML_2020/IML_2020.xls", sheet = "IML_2020_AGS-MEX") %>% 
  rbind(., read_xls("data/IML_2020/IML_2020.xls", sheet = "IML_2020_MICH-ZAC")) %>% 
  rbind(., read_xls("data/IML_2020//IML_2020.xls", sheet = "IML_2020_LOC2viv")) -> IML_2020

glimpse(IML_2020)

##organization----
colnames(IML_2020)[9:16]
setNames(c(-100, # ANALF
           -100, # SBASC
           -100, # OVSDE
           -100, # OVSEE
           -100, # OVSAE
           -100, # OVPT
           -100, # OVHAC
           -100), # OVSREF
         nm = c("ANALF", "SBASC", "OVSDE", "OVSEE", "OVSAE", "OVPT", "OVHAC", "OVSREF")) -> minRV

#analysis----
ind_2020 <- p2distance(matriz = as.matrix(-1 * IML_2020[9:16]),
                       reference_vector = minRV,
                       iterations = 50)
