library(tidyverse)
plant=db %>%
  dplyr::select(species,genus, family,order,class,decimalLongitude, decimalLatitude, countryCode, individualCount,
         gbifID,phylum,taxonRank, coordinateUncertaintyInMeters, year,
         basisOfRecord, institutionCode, datasetName,verbatimLocality,region)  %>% #controlla che i nomi delle variabili corrispondo con i tuoi
  filter (phylum == "Tracheophyta") %>% #select only vascular plants, valutare se aggiugere filtro temporale (dipende da come sono i dati, magari una volta che li hai decidiamo cosa togliere)
  filter(!is.na(decimalLongitude))%>%
  filter(!is.na(decimalLatitude)) %>%
  filter(basisOfRecord!='UNKNOWN'& basisOfRecord!='LIVING_SPECIMEN') %>%
   filter(taxonRank!='CLASS' & taxonRank!='ORDER')# filtra le occorrenze non determinate a livello di specie
