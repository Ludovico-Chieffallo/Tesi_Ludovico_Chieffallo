#libraries----
library(raster)
library(dplyr)
library(knitr)
library(tidyverse)
library(ggplot2)
library(colorist)
library(rnaturalearth)
library(dismo)
library(rnaturalearthdata)
library(rgeos)
library(sp)
library(rgdal)
library(sf)
library(maptools)
library(fuzzySim)
library(sdm)
library(tidyr)
library(usdm)


#Load data (miniopterus=minio)---- 

gbif("Miniopterus", "schreibersii" , download=F)
minio<- gbif("Miniopterus", "schreibersii" , download=T)

class(minio)
dim(minio)

table(minio$basisOfRecord)

minio<- minio%>% 
  filter(basisOfRecord %in% c("HUMAN_OBSERVATION", "OBSERVATION"))
nrow(minio)

miniogeo<-minio%>%
  select(lon,lat)
head(miniogeo)

miniogeo$species<-1
miniogeo
miniogeo<-miniogeo%>%
  drop_na()

#------

class(miniogeo)
coordinates(miniogeo) <-c("lon","lat")

#-----

Europe <- ne_countries(scale="medium", type="map_units", returnclass="sf", continent="Europe")  
Worldclim<-raster::getData('worldclim', var='bio', res=2.5) #Valid resolutions are 0.5, 2.5, 5, and 10 (minutes of a degree). In the case of res=0.5, you must also provide a lon and lat argument for a tile

plot(Worldclim[[1]]) #plot(worldclim$bio1)

points(miniogeo)

e<-drawExtent() # europe

#or Europe <- Europe %>%
#dplyr::select(geometry,name_long)  %>%  #Be careful, the class names of your dataset may be different!                         
#  filter(name_long!='Russian Federation')


miniogeo<-crop(miniogeo,e)

points(miniogeo)
bio1<-crop(Worldclim,e)
plot(bio1[[1]])
points(miniogeo,pch=20, cex=0.2)

#----
install.packages("rworldxtra")
library(rworldxtra)
library(rworldmap)
newmap <- getMap(resolution = "low")
maphigh<- getMap(resolution = "high")
plot(newmap, xlim = c(-10, 35), ylim = c(40,55), asp = 1)
plot(maphigh, xlim = c(-10, 35), ylim = c(40,55), asp=1)
points(miniogeo,pch=20, cex=0.4, col="red")

str(newmap)
str(maphigh)
