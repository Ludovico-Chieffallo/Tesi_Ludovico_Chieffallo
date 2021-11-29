#Install.packages----
install.packages("tidyverse")
install.packages("rnaturalearth")
install.packages("dismo")

#Packages----
library(raster)
library(dplyr)
library(knitr)
library(tidyverse)
library(ggplot2)
library(colorist)
library(rnaturalearth)
library(dismo)

#Load dataset----
minionpterus<-read.csv2("miniopterus.csv")
head(minionpterus)
View(minionpterus)

melanitta<-read.csv2("melanitta.csv")
head(melanitta)
View(melanitta)

quercus<-read.csv2("quercus.csv")
head(quercus)
View(quercus)

#Filter----



melanitta<- melanitta%>%
  dplyr::select(kingdom,phylum,class,order,family,genus,species,countryCode,locality,stateProvince,individualCount,decimalLatitude,decimalLongitude,year,	basisOfRecord,issue)%>%                        
  filter(!is.na(decimalLongitude))%>%
  filter(!is.na(decimalLatitude)) %>%
  filter(!is.na(individualCount))

view(melanitta)


minionpterus<- minionpterus%>%
  dplyr::select(kingdom,phylum,class,order,family,genus,species,countryCode,locality,stateProvince,individualCount,decimalLatitude,decimalLongitude,year,	basisOfRecord,issue)%>%                        
  filter(!is.na(decimalLongitude))%>%
  filter(!is.na(decimalLatitude)) 

#ho dovuto eliminare il filtro su individualcount perchè nel dataframe sono presenti molti  NA in quella sezione
#va bene così oppure cambiare specie?

view(minionpterus)


quercus<- quercus%>%
  dplyr::select(kingdom,phylum,class,order,family,genus,species,countryCode,locality,stateProvince,individualCount,decimalLatitude,decimalLongitude,year,	basisOfRecord,issue)%>%                        
  filter(!is.na(decimalLongitude))%>%
  filter(!is.na(decimalLatitude)) %>%
  filter(!is.na(individualCount))

view(quercus)

#Sample size----

minionpterus5000<-sample_n(minionpterus, 6000)

melanitta5000<-sample_n(melanitta,6000)

quercus5000<-sample_n(quercus,6000)

view(minionpterus)
view(melanitta)
view(minionpterus)
