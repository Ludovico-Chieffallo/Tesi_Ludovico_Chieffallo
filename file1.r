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
library(rworldxtra)
library(rworldmap)

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

#create a spatial obj------

class(miniogeo)
coordinates(miniogeo) <-c("lon","lat") #create a spatial obj

#worlcclim data and europe map-----

Europe <- ne_countries(scale="medium", type="map_units", returnclass="sf", continent="Europe")  #PUò ESSERE MODIFICATO?
Worldclim<-raster::getData('worldclim', var='bio', res=2.5) #Valid resolutions are 0.5, 2.5, 5, and 10 (minutes of a degree). In the case of res=0.5, you must also provide a lon and lat argument for a tile

plot(Worldclim[[1]]) #plot(worldclim$bio1)

points(miniogeo)

#envdata and crop Europe----
#PROBLEMA NELLA RAPPRESENTAZIONE GRAFICA, I PUNTI NON COINCIDONO(MENTRE IN MAPHIGH SI)
Europe <- Europe %>%
  dplyr::select(geometry,name_long)  %>%  #Be careful, the class names of your dataset may be different!                         
  filter(name_long!='Russian Federation')

envData<-crop(Worldclim, Europe)
EuropePred <- mask(envData, Europe)


plot(Europe)
plot(envData)


#extend and crop map----
e<-drawExtent() # europe, draw the square on the map

#or Europe <- Europe %>%
#dplyr::select(geometry,name_long)  %>%  #Be careful, the class names of dataset may be different!                         
#  filter(name_long!='Russian Federation')


miniogeo<-crop(miniogeo,e)

points(miniogeo)
bio1<-crop(Worldclim,e)
plot(bio1[[1]])
points(miniogeo,pch=20, cex=0.2, col="red")


#plot points in a map from rworldmap (low and high resolution) -----
library(rworldxtra)
library(rworldmap)
newmap <- getMap(resolution = "low")
maphigh<- getMap(resolution = "high")

lowmap<-plot(newmap, xlim = c(-10, 35), ylim = c(40,55), asp = 1)
highmap<-plot(maphigh, xlim = c(-10, 35), ylim = c(40,55), asp=1)
points(miniogeo,pch=20, cex=0.5, col="red")

str(newmap)
str(maphigh)

#sample 5000----

minioxy<-minio%>%
  select(lon,lat)%>%
  drop_na()

set.seed(999) #?

minio5000<- minioxy%>%
  sample_n(5000)

view(minio5000)
nrow(minio5000)
head(minio5000)


#plot sample5000----

plot(maphigh, xlim = c(-10, 35), ylim = c(40,55), asp=1)

minio5000$species<-1
head(minio5000)

coordinates(minio5000) <-c("lon","lat")

points(minio5000,pch=20, cex=0.5, col="red")

#crop map Spain and Portugal----

e<-drawExtent()
minio5000<-crop(minio5000,e) 
pltminio5000<-crop(maphigh,e)
plot(pltminio5000)
points(minio5000,pch=20, cex=0.2, col="red")

#(Re)create a dataframe----

view(minio5000)
xypMinio<-as.data.frame(minio5000) #convert to a spatial points in a dataframe
view(xypMinio)
nrow(xypMinio)

#absences----
#we set the prevalence to 0,4
#prevalence is 0,4=Presence(5000)/absences(x)= 12.500 
#we need to use randompoints() to find this 12.500 absences
#but before we need to convert a dataframe with lat=y and long=x

head(xypMinio)
colnames(xypMinio)[which(names(xypMinio) == "lon")] <- "x" 
colnames(xypMinio)[which(names(xypMinio) == "lat")] <- "y"
head(xypMinio)

#Delete the previous column (species)
xypMinio$species <- NULL
head(xypMinio)

sample_abxy<- randomPoints(envData, 12500,xypMinio)

plot(sample_abxy)
head(sample_abxy)
nrow(sample_abxy)

#C'E' IL PROBLEMA DEL DATUM, CON EUROPA NON COINCIDONO I DATI MENTRE CON MAPHIGH SI
#HO PROVATO CON #crs(minio5000)<- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
                #crs(envData) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"


#Dataframe di dati uniti Pres/abs----

class(sample_abxy)
sample_abxydf<-as.data.frame(sample_abxy)
class(sample_abxydf)
head(sample_abxydf)
head(xypMinio)



sample_abxydf$presence<-0
xypMinio$presence<-1

#merge 2 dataframe

minPresAbs<-rbind(sample_abxydf, xypMinio)
view(minPresAbs)
