# Wed Nov 22 18:07:59 2023 ------------------------------
# Script to organize and check data availability to build th IML for 2010 and before

#libraries----
library(readxl)
library(here)
library(dplyr)
library(tidyr)

#data----
read.csv(file = "data/IML_90-10/Base_marginacion_localidad_90-10.csv", fileEncoding = "latin1") -> IML_90_10
read_xls("data/IML_2020/IML_2020.xls", sheet = "IML_2020_AGS-MEX") %>% 
  rbind(., read_xls("data/IML_2020/IML_2020.xls", sheet = "IML_2020_MICH-ZAC")) %>% 
  rbind(., read_xls("data/IML_2020//IML_2020.xls", sheet = "IML_2020_LOC2viv")) -> IML_2020

##organization----
IML_90_10 %>% 
  pivot_wider(id_cols = c(CVE_ENT, NOM_ENT, CVE_MUN, NOM_MUN, CVE_LOC, NOM_LOC),
              names_from = AÑO,
              values_from = POB_TOT:GML) %>%
  glimpse()-> IML_90_10_wide

IML_2020 %>% 
  mutate(CVE_LOC = as.numeric(CVE_LOC)) %>% 
  glimpse

IML_90_10 %>% 
  select(CVE_LOC, AÑO) %>% 
  group_by(AÑO) %>% 
  summarise(n = n()) %>% 
  unique()
  glimpse()
