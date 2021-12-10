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
library(maps)
library(maptools)
library(rgbif)

#Load data (miniopterus=minio)---- 

gbif("Miniopterus", "schreibersii" , download=F)
minio<- gbif("Miniopterus", "schreibersii" , download=T)

table(minio$basisOfRecord)

#Filter data minio----

minio<- minio%>%
  filter(!is.na(lat))%>%
  filter(!is.na(lon))%>%
  filter(year>1980)%>%
  filter(basisOfRecord %in% c("HUMAN_OBSERVATION", "OBSERVATION"))

class(minio)
dim(minio)
nrow(minio)

#Create miniogeo (lon, lat)----
miniogeo<-minio%>%
  select(lon,lat)
head(miniogeo)

miniogeo$species<-1
head(miniogeo)
nrow(miniogeo)

#Create a spatial obj------

class(miniogeo)
coordinates(miniogeo) <-c("lon","lat") #create a spatial obj
                                       #or #coordinates(miniogeo) <-  ~ lon + lat 
                                           #crs(minio) <- "+proj=longlat"
head(miniogeo)

#Set correct datum and epsg----
crs(miniogeo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(miniogeo) <- CRS("+init=epsg:4326")

#Worlclim data and Europe map-----
Europe <- ne_countries(scale="medium", type="map_units", returnclass="sf", continent="Europe")
Worldclim<-raster::getData('worldclim', var='bio', res=2.5) #Valid resolutions are 0.5, 2.5, 5, and 10 (minutes of a degree). In the case of res=0.5, you must also provide a lon and lat argument for a tile

Europe <- Europe %>%
  dplyr::select(geometry,name_long)  %>%  #Be careful, the class names of dataset may be different!                         
  filter(name_long!='Russian Federation')


plot(Worldclim[[1]]) #or plot(worldclim$bio1)
plot(st_geometry(Europe))
points(miniogeo, cex=0.1)


#Envdata and Europepred----

envData<-crop(Worldclim, Europe)
EuropePred <- mask(envData, Europe) #we create a new raster without NA value 


par(mfrow=c(1,2))
plot(envData$bio1)
plot(EuropePred$bio1)
dev.off()
#extend and crop map----

plot(EuropePred[[1]]) #example
points(miniogeo, cex=0.2)

#eSpnPrt<-drawExtent() #we can use for check the coordinates
#xmin       : -10.23854 
#xmax       : 9.322413 
#ymin       : 34.29422 
#ymax       : 55.99065 

extSpnPrt<-extent(c(-11,10,35,56))
miniogeo<-crop(miniogeo,extSpnPrt) 
SpainPort<-crop(EuropePred,extSpnPrt)

#points(miniogeo)
#bio1<-crop(Worldclim,e)
#plot(bio1[[1]])
#points(miniogeo,pch=20, cex=0.2, col="red")

plot(SpainPort$bio1)
points(miniogeo, cex=0.1)

#Alternative map rworldmap (low and high resolution)-----
library(rworldxtra)
library(rworldmap)

newmap <- getMap(resolution = "low")
maphigh<- getMap(resolution = "high")

#maphigh<-maphigh%>%
#  as.data.frame()
  
extSpnPrt1<-extent(c(-11,10,35,56))
miniogeo<-crop(miniogeo,extSpnPrt) 
SpainPort1<-crop(maphigh,extSpnPrt)  

plot(SpainPort1)

#sample 5000----

set.seed(999)

minio5000<- miniogeo%>%
  as.data.frame()%>%
  sample_n(5000)

view(minio5000)
nrow(minio5000)
head(minio5000)


#plot sample5000----

coordinates(minio5000) <-c("lon","lat")

crs(minio5000) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(minio5000) <- CRS("+init=epsg:4326")

plot(SpainPort$bio1)
points(minio5000,pch=1, cex=0.09, col="red")

#(Re)create a dataframe----

view(minio5000)
xypMinio<-as.data.frame(minio5000,row.names = NULL) #convert a spatial points in a dataframe
view(xypMinio)
view(minio5000)
nrow(xypMinio)
nrow(minio5000)


#absences----
#we set the prevalence to 0,4
#prevalence is 0,4=Presence(5000)/absences(x)= 12.500 
#we need to use randompoints() to find this 12.500 absences
#but before we need to convert a dataframe with lat=y and long=x


head(xypMinio)
colnames(xypMinio) <- c("x","y","presence")

#or
#colnames(xypminio)[which(names(xypMinio) == "lon")] <- "x" 
#colnames(xypminio)[which(names(xypMinio) == "lat")] <- "y"


sample_abxy<- randomPoints(EuropePred, 12500,ext=SpainPort, p=minio5000)

plot(sample_abxy)
head(sample_abxy)
nrow(sample_abxy)


#Dataframe di dati uniti Pres/abs----

class(sample_abxy)

sample_abxydf<-as.data.frame(sample_abxy)

nrow(sample_abxydf)
class(sample_abxydf)
head(sample_abxydf)
head(xypMinio)


sample_abxydf$presence<-0


#merge 2 dataframe
minPresAbs<-rbind(sample_abxydf, xypMinio)

#coordinates(minPresAbs)<-c("x","y")
view(minPresAbs)


#-----

predictors<- raster::extract(EuropePred, minPresAbs[,1:2], df=FALSE)


sdmData<-data.frame(cbind(minPresAbs, predictors))

sdmData
view(sdmData)

#----

FavModel<-multGLM(sdmData, sp.cols = 3, var.cols=4:22, family = "binomial",step = FALSE, Y.prediction = TRUE, P.prediction = TRUE, Favourability = TRUE)

FavModel
#----

#validation data ??
