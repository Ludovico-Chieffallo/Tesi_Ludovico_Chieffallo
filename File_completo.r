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
#Import data miniopterus---- 

gbif("Miniopterus", "schreibersii" , download=F)
minio<- gbif("Miniopterus", "schreibersii" , download=T)
#Import data melanitta----
setwd("c:/tesi/tesi")

mela<-read.csv("melanitta.csv")

#Import data quercus ---- 

quercus<-read.csv("quercus.csv")
View(quercus)

#Filter data miniopterus----

table(minio$basisOfRecord)

minio<- minio%>%
  filter(!is.na(lat))%>%
  filter(!is.na(lon))%>%
  filter(year>1980)%>%
  filter(basisOfRecord %in% c("HUMAN_OBSERVATION", "OBSERVATION"))


#Filter data melanitta----

table(mela$basisOfRecord)

mela<- mela%>%
  filter(!is.na(decimalLatitude))%>%
  filter(!is.na(decimalLongitude))%>%
  filter(!is.na(countryCode))%>%
  filter(year>1980)%>%
  filter(coordinateUncertaintyInMeters<100)%>%
  filter(!is.na(coordinateUncertaintyInMeters))%>%
  filter(countryCode !="US" & countryCode !="PM")%>%
  filter(basisOfRecord %in% c("HUMAN_OBSERVATION", "OBSERVATION"))

#Filter data quercus----

table(quercus$basisOfRecord)

quercus<- quercus%>%
  filter(!is.na(decimalLatitude))%>%
  filter(!is.na(decimalLongitude))%>%
  filter(year>1980)%>%
  filter(basisOfRecord %in% c("HUMAN_OBSERVATION", "OBSERVATION"))

#Create miniogeo (lon, lat)----
miniogeo<-minio%>%
  select(lon,lat)
head(miniogeo)

miniogeo$species<-1
head(miniogeo)


#Create melageo (lon, lat)----
melageo<-mela%>%
  select(decimalLongitude,decimalLatitude)
head(melageo)

melageo$species<-1
head(melageo)


#Create quercgeo (lon, lat)----

quercgeo<-quercus%>%
  select(decimalLongitude,decimalLatitude)
head(quercgeo)

quercgeo$species<-1



head(quercgeo)
#Create a spatial obj miniopterus------

coordinates(miniogeo) <-c("lon","lat") #create a spatial obj
#or #coordinates(miniogeo) <-  ~ lon + lat 
#crs(minio) <- "+proj=longlat"


#Create a spatial obj melanitta------

colnames(melageo) <- c("lon","lat","species")
coordinates(melageo) <-c("lon","lat")



#Create a spatial obj quercus------

colnames(quercgeo) <- c("lon","lat","species")
coordinates(quercgeo)<-c("lon", "lat")


#Set correct datum and epsg miniopterus----
crs(miniogeo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(miniogeo) <- CRS("+init=epsg:4326")
#Set correct datum and epsg melanitta----
crs(melageo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(melageo) <- CRS("+init=epsg:4326")
#Set correct datum and epsg quercus----

crs(quercgeo) <- "+proj=longlat"
crs(quercgeo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#Worlclim data and Europe map-----
Europe <- ne_countries(scale="medium", type="map_units", returnclass="sf", continent="Europe")
Worldclim<-raster::getData('worldclim', var='bio', res=2.5) #Valid resolutions are 0.5, 2.5, 5, and 10 (minutes of a degree). In the case of res=0.5, you must also provide a lon and lat argument for a tile

Europe <- Europe %>%
  dplyr::select(geometry,name_long)  %>%  #Be careful, the class names of dataset may be different!                         
  filter(name_long!='Russian Federation')


plot(Worldclim[[1]]) #or plot(worldclim$bio1)
plot(st_geometry(Europe))
points(miniogeo, cex=0.1)
#or
plot(st_geometry(Europe))
points(melageo, cex=0.1)
#or
plot(st_geometry(Europe))
points(quercgeo, cex=0.1)

#Envdata and Europepred----

envData<-crop(Worldclim, Europe)
EuropePred <- mask(envData, Europe) #we create a new raster without NA value 


par(mfrow=c(1,2))
plot(envData$bio1)
plot(EuropePred$bio1)
dev.off()
#Sample 5000 miniopterus----

set.seed(999)

minio5000<- miniogeo%>%
  as.data.frame()%>%
  sample_n(5000)


#Sample 5000 melanitta----

set.seed(999)

mela5000<- melageo%>%
  as.data.frame()%>%
  sample_n(5000)



#Sample 5000 quercus----
set.seed(999)

querc5000<- quercgeo%>%
  as.data.frame()%>%
  sample_n(5000)

#Plot sample5000 miniopterus----

coordinates(minio5000) <-c("lon","lat")

crs(minio5000) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(minio5000) <- CRS("+init=epsg:4326")

plot(SpainPort$bio1)
points(minio5000,pch=1, cex=0.09, col="red")
#Plot sample5000 melanitta----

coordinates(mela5000) <-c("lon","lat")

crs(mela5000) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(mela5000) <- CRS("+init=epsg:4326")

plot(st_geometry(Europe))
points(mela5000,pch=1, cex=0.09, col="red")

#Plot sample5000 quercus----
coordinates(querc5000) <-  ~ lon + lat 



crs(querc5000) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(querc5000) <- CRS("+init=epsg:4326")

plot(EuropePred$bio1)
points(querc5000,pch=1, cex=0.1, col="red")

str(querc5000)
plot(st_geometry(Europe))
points(querc5000,pch=1, cex=0.1, col="red")

#(Re)create a dataframe miniopterus----

view(minio5000)
xypMinio<-as.data.frame(minio5000,row.names = NULL) #convert a spatial points in a dataframe
view(xypMinio)
view(minio5000)
nrow(xypMinio)
nrow(minio5000)







#(Re)create a dataframe melanitta----

view(mela5000)
xypmela<-as.data.frame(mela5000,row.names = NULL) #convert a spatial points in a dataframe

view(xypmela)
nrow(xypmela)




#(Re)create a dataframe quercus----

xypQuerc<-as.data.frame(querc5000,row.names = NULL) #convert a spatial points in a dataframe
view(xypQuerc)
nrow(xypQuerc)









#Absences miniopterus----
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

#Absences melanitta----

head(xypmela)
colnames(xypmela) <- c("x","y","presence")



sample_abxymela<- randomPoints(EuropePred, 12500, p=mela5000)

plot(sample_abxymela)


#Absences quercus----
head(xypQuerc)
colnames(xypQuerc) <- c("x","y","presence")


sample_abxyQuerc<- randomPoints(EuropePred, 12500, p=querc5000)

plot(sample_abxyQuerc)




#Dataframe di dati uniti Pres/abs miniopterus----

class(sample_abxy)

sample_abxydf<-as.data.frame(sample_abxy)

nrow(sample_abxydf)
class(sample_abxydf)
head(sample_abxydf)
head(xypMinio)

sample_abxydf$presence<-0

#merge 2 dataframe
minPresAbs<-rbind(sample_abxydf, xypMinio)


#Dataframe di dati uniti Pres/abs melanitta----

class(sample_abxymela)

sample_abxymeladf<-as.data.frame(sample_abxymela)

nrow(sample_abxymeladf)
class(sample_abxymeladf)
head(sample_abxymeladf)


xypmela$presence<-1
sample_abxymeladf$presence<-0

#merge 2 dataframe
melaPresAbs<-rbind(sample_abxymeladf, xypmela)


#Dataframe di dati uniti Pres/abs quercus----

class(sample_abxyQuerc)

sample_abxyQuercdf<-as.data.frame(sample_abxyQuerc)

sample_abxyQuercdf$presence<-0
xypQuerc$presence<-1

#merge 2 dataframe
quercPresAbs<-rbind(sample_abxyQuercdf, xypQuerc)

view(quercPresAbs)



#Predictor and sdm data miniopterus-----

predictors_min<- raster::extract(EuropePred, minPresAbs[,1:2], df=FALSE)


sdmData_min<-data.frame(cbind(minPresAbs, predictors_min))

sdmData_min
view(sdmData_min)

#Favourability model miniopterus----

FavModel_min<-multGLM(sdmData, sp.cols = 3, var.cols=4:22, family = "binomial",step = FALSE, Y.prediction = TRUE, P.prediction = TRUE, Favourability = TRUE)

FavModel
#Get pred miniopterus----

#prima di fare getPred bisogna estendere la prediction su tutta l'estensione quindi creo data.frame di values in x e y di tutta l'estensione
EuropePred <- stack(EuropePred) # it needs to be RasterStack, EuropePred is RasterBrick
FavPred_min<- getPreds(EuropePred, models=FavModel_min$models, id.col = NULL, Y = FALSE, P = FALSE, Favourability = TRUE)





#Predictors and sdm data melanitta-----

predictors_mela<- raster::extract(EuropePred, melaPresAbs[,1:2], df=FALSE)


sdmData_mela<-data.frame(cbind(melaPresAbs, predictors))

sdmData_mela
view(sdmData)

#Favourability model melanitta----

FavModel_mela<-multGLM(sdmData_mela, sp.cols = 3, var.cols=4:22, family = "binomial",step = FALSE, Y.prediction = TRUE, P.prediction = TRUE, Favourability = TRUE)

FavModel

#Get pred melanitta----
EuropePred <- stack(EuropePred) # it needs to be RasterStack, EuropePred is RasterBrick
FavPred_mela<- getPreds(EuropePred, models=FavModel_mela$models, id.col = NULL, Y = FALSE, P = FALSE, Favourability = TRUE)





#Predictors and sdm data quercus-----

predictors_quer<- raster::extract(EuropePred, quercPresAbs[,1:2], df=FALSE)


sdmData_quer<-data.frame(cbind(quercPresAbs, predictors_quer))

sdmData_quer


#Favourability model quercus----

FavModel_quer<-multGLM(sdmData, sp.cols = 3, var.cols=4:22, family = "binomial",step = FALSE, Y.prediction = TRUE, P.prediction = TRUE, Favourability = TRUE)

FavModel

#Getpred quercus----
EuropePred <- stack(EuropePred) # it needs to be RasterStack, EuropePred is RasterBrick
FavPred_quer<- getPreds(EuropePred, models=FavModel_quer$models, id.col = NULL, Y = FALSE, P = FALSE, Favourability = TRUE)
