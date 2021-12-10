#Libraries----
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
library(openxlsx)

#Load data (quercus)---- 

quercus<-read.csv("quercus.csv")
View(quercus)


table(quercus$basisOfRecord)

#Filter data quercus----

quercus<- quercus%>%
  filter(!is.na(decimalLatitude))%>%
  filter(!is.na(decimalLongitude))%>%
  filter(year>1980)%>%
  filter(basisOfRecord %in% c("HUMAN_OBSERVATION", "OBSERVATION"))

class(quercus)
dim(quercus)
nrow(quercus)

#Create miniogeo (lon, lat)----

quercgeo<-quercus%>%
  select(decimalLongitude,decimalLatitude)
head(quercgeo)

quercgeo$species<-1
head(quercgeo)
nrow(quercgeo)
colnames(quercgeo) <- c("lon","lat","species")
view(quercgeo)
head(quercgeo)

#Convert in numeric class----

#If is not numeric class
#lon is class "character", we need to convert to numeric

quercgeo$lon<- as.numeric(gsub("\\." ,"", quercgeo$lon))


#Create a spatial obj------

class(quercgeo)
coordinates(quercgeo)<-c("lon", "lat")
#coordinates(quercgeo) <-  ~ lon + lat 
head(quercgeo)

#Set correct datum and epsg----

crs(quercgeo) <- "+proj=longlat"
crs(quercgeo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"



#Worlclim data and Europe map-----
Europe <- ne_countries(scale="medium", type="map_units", returnclass="sf", continent="Europe")
Worldclim<-raster::getData('worldclim', var='bio', res=2.5) #Valid resolutions are 0.5, 2.5, 5, and 10 (minutes of a degree). In the case of res=0.5, you must also provide a lon and lat argument for a tile

Europe <- Europe %>%
  dplyr::select(geometry,name_long)  %>%  #Be careful, the class names of dataset may be different!                         
  filter(name_long!='Russian Federation')

#Envdata and Europepred----

envData<-crop(Worldclim, Europe)
EuropePred <- mask(envData, Europe) #we create a new raster without NA value 


par(mfrow=c(1,2))
plot(envData$bio1)
plot(EuropePred$bio1)
dev.off()
#sample 5000----
set.seed(999)

querc5000<- quercgeo%>%
  as.data.frame()%>%
  sample_n(5000)

view(querc5000)
nrow(querc5000)
head(querc5000)





#plot sample5000----
coordinates(querc5000) <-  ~ lon + lat 



crs(querc5000) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(querc5000) <- CRS("+init=epsg:4326")

plot(EuropePred$bio1)
points(querc5000,pch=1, cex=0.1, col="red")
plot(querc5000)
str(querc5000)
plot(st_geometry(Europe))
points(querc5000)



#extend and crop map----

plot(EuropePred[[1]]) #example
points(querc5000, cex=0.2)

#eSpnPrt<-drawExtent() #we can use for check the coordinates
#xmin       : -10.23854 
#xmax       : 9.322413 
#ymin       : 34.29422 
#ymax       : 55.99065 

extSpnPrt<-extent(c(-11,10,35,56))
quercgeo<-crop(quercgeo,extSpnPrt) 
SpainPort<-crop(EuropePred,extSpnPrt)

#points(miniogeo)
#bio1<-crop(Worldclim,e)
#plot(bio1[[1]])
#points(miniogeo,pch=20, cex=0.2, col="red")

plot(SpainPort$bio1)
points(querc5000, cex=0.1)

#Alternative map rworldmap (low and high resolution)-----
library(rworldxtra)
library(rworldmap)

newmap <- getMap(resolution = "low")
maphigh<- getMap(resolution = "high")

#maphigh<-maphigh%>%
#  as.data.frame()

extSpnPrt1<-extent(c(-11,10,35,56))
quercgeo<-crop(quercgeo,extSpnPrt) 
SpainPort1<-crop(maphigh,extSpnPrt)  

plot(SpainPort1)
crs(SpainPort1) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(SpainPort1) <- CRS("+init=epsg:4326")

points(miniogeo)

#(Re)create a dataframe----

xypQuerc<-as.data.frame(querc5000,row.names = NULL) #convert a spatial points in a dataframe
view(xypQuerc)
view(xypQuerc)
nrow(xypQuerc)


#absences----
#we set the prevalence to 0,4
#prevalence is 0,4=Presence(5000)/absences(x)= 12.500 
#we need to use randompoints() to find this 12.500 absences
#but before we need to convert a dataframe with lat=y and long=x


head(xypQuerc)
colnames(xypQuerc) <- c("x","y","presence")

#or
#colnames(xypminio)[which(names(xypMinio) == "lon")] <- "x" 
#colnames(xypminio)[which(names(xypMinio) == "lat")] <- "y"


sample_abxyQuerc<- randomPoints(EuropePred, 12500,ext=SpainPort, p=querc5000)

plot(sample_abxyQuerc)
head(sample_abxyQuerc)
nrow(sample_abxyQuerc)


#Dataframe di dati uniti Pres/abs----

class(sample_abxyQuerc)

sample_abxyQuercdf<-as.data.frame(sample_abxyQuerc)

nrow(sample_abxyQuercdf)
class(sample_abxyQuercdf)
head(sample_abxyQuercdf)


sample_abxyQuercdf$presence<-0
xypQuerc$presence<-1

#merge 2 dataframe
quercPresAbs<-rbind(sample_abxyQuercdf, xypQuerc)

#coordinates(minPresAbs)<-c("x","y")
view(quercPresAbs)


#-----

predictors<- raster::extract(EuropePred, quercPresAbs[,1:2], df=FALSE)


sdmData<-data.frame(cbind(quercPresAbs, predictors))

sdmData
view(sdmData)

#----

FavModel<-multGLM(sdmData, sp.cols = 3, var.cols=4:22, family = "binomial",step = FALSE, Y.prediction = TRUE, P.prediction = TRUE, Favourability = TRUE)

FavModel
#----

#validation data ?

