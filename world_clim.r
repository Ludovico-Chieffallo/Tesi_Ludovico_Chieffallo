library(raster)
library(rnaturalearth) #se ti dà errore fammi sapere perchè io ho avuto problemi, nel caso saprei come risolverli
library(tidyverse)
library(dplyr)
  
Europe <- ne_countries(scale="medium", type="map_units", returnclass="sf", continent="Europe")  
Worldclim<-raster::getData('worldclim', var='bio', res=2.5) #Valid resolutions are 0.5, 2.5, 5, and 10 (minutes of a degree). In the case of res=0.5, you must also provide a lon and lat argument for a tile

# envData<-crop(Worldclim, extent(-12, 25, 36, 60)) approssimativo

Europe <- Europe %>%
  dplyr::select(geometry,name_long)  %>%  #Be careful, the class names of your dataset may be different!                         
  filter(name_long!='Russian Federation')
  
envData<-crop(Worldclim, Europe)
EuropePred <- mask(envData, Europe)

