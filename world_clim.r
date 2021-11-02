library(raster)
library(rnaturalearth)
library(tidyverse)
library(dplyr)
  
  
Worldclim<-raster::getData('worldclim', var='bio', res=2.5) #Valid resolutions are 0.5, 2.5, 5, and 10 (minutes of a degree). In the case of res=0.5, you must also provide a lon and lat argument for a tile

# envData<-crop(Worldclim, extent(-12, 25, 36, 60)) approssimativo

Russia <- Europe %>% 
       filter(
        str_detect(name_long, 'Russia')
        ) %>% 
       select(name_long, geometry) %>% 
       {. ->> russia})
envData<-crop(Worldclim, Europe)
plot(envData)
EuropePred <- mask(envData, Europe)

