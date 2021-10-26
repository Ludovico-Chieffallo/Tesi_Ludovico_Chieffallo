install.packages("tidyverse")
install.packages("dplyr")


library(tidyverse)
library(dplyr)


setwd("c:/lab/")


Per<-read.csv2("pernice.csv")

per1<- Per%>%
  rename(gbifID=ï..gbifID) #per rinominare qualcosa all'interno del nostro dataframe


Dati_pern<- per1 %>%
  dplyr::select(species,genus, family,order,class,decimalLongitude, decimalLatitude, countryCode, individualCount,gbifID,phylum,taxonRank, coordinateUncertaintyInMeters, year,basisOfRecord, institutionCode)  %>% #controlla che i nomi delle variabili corrispondo con i tuoi
  filter (phylum == "Chordata") %>% 
  filter(!is.na(decimalLongitude))%>%
  filter(!is.na(decimalLatitude)) %>%
  filter(!is.na(individualCount))
  filter(basisOfRecord!='UNKNOWN'& basisOfRecord!='LIVING_SPECIMEN') %>%
  filter(taxonRank!='CLASS' & taxonRank!='ORDER')# filtra le occorrenze non determinate a livello di specie
