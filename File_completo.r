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
library(RStoolbox)
library(spMaps)
library(caret)

#Import data miniopterus---- 

gbif("Miniopterus", "schreibersii" , download=F)
minio<- gbif("Miniopterus", "schreibersii" , download=T)
#Import data melanitta----
setwd("c:/tesi/tesi")

mela<-read.csv("melanitta.csv")

#Import data quercus ---- 

quercus<-read.csv("quercus.csv")

#Import data Lagopus----
gbif("Lagopus", "muta",download=F)

lagopus<- gbif("Lagopus", "muta",download=T)
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

#Filter data lagopus----
lagopus<- lagopus%>%
  filter(!is.na(lat))%>%
  filter(!is.na(lon))%>%
  filter(year>1980)%>%
  filter(basisOfRecord %in% c("HUMAN_OBSERVATION", "OBSERVATION"))

class(lagopus)
dim(lagopus)
nrow(lagopus)
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

#Create Lugopusgeo----
lagopusgeo<-lagopus%>%
  select(lon,lat)
head(lagopusgeo)

lagopusgeo$species<-1
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


#create a spatial obj lagopus----
class(lagopusgeo)
coordinates(lagopusgeo) <-c("lon","lat") #create a spatial obj
#or #coordinates(miniogeo) <-  ~ lon + lat 
#crs(minio) <- "+proj=longlat"
head(miniogeo)
#Set correct datum and epsg miniopterus----

crs(miniogeo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(miniogeo) <- CRS("+init=epsg:4326")
#Set correct datum and epsg melanitta----
crs(melageo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(melageo) <- CRS("+init=epsg:4326")

#Set correct datum and epsg quercus----
crs(quercgeo) <- "+proj=longlat"
crs(quercgeo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#set correct datum and epsg lagopus----
crs(lagopusgeo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(lagopusgeo) <- CRS("+init=epsg:4326")

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
#or
plot(st_geometry(Europe))
points(lagopusgeo, cex=0.1)

dev.off()
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

#Sample 5000 lagopus----
set.seed(999)

lagopus5000<- lagopusgeo%>%
  as.data.frame()%>%
  sample_n(5000)



#Plot sample5000 miniopterus----

coordinates(minio5000) <-c("lon","lat")

crs(minio5000) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(minio5000) <- CRS("+init=epsg:4326")

#Plot sample5000 melanitta----

coordinates(mela5000) <-c("lon","lat")

crs(mela5000) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(mela5000) <- CRS("+init=epsg:4326")


#Plot sample5000 quercus----
coordinates(querc5000) <-  ~ lon + lat 



crs(querc5000) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(querc5000) <- CRS("+init=epsg:4326")

plot(EuropePred$bio1)
points(querc5000,pch=1, cex=0.1, col="red")

str(querc5000)
plot(st_geometry(Europe))
points(querc5000,pch=1, cex=0.1, col="red")

#Plot sample5000 lagopus----
coordinates(lagopus5000) <-c("lon","lat")

crs(lagopus5000) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(lagopus5000) <- CRS("+init=epsg:4326")


plot(EuropePred$bio1)
points(lagopus5000,pch=1, cex=0.1, col="blue")

#(Re)create a dataframe miniopterus----

xypMinio<-as.data.frame(minio5000,row.names = NULL) #convert a spatial points in a dataframe


#(Re)create a dataframe melanitta----

xypmela<-as.data.frame(mela5000,row.names = NULL) #convert a spatial points in a dataframe


#(Re)create a dataframe quercus----

xypQuerc<-as.data.frame(querc5000,row.names = NULL) #convert a spatial points in a dataframe



#(Re)create a dataframe lagopus----
xypLagopus<-as.data.frame(lagopus5000,row.names = NULL) #convert a spatial points in a dataframe

#Absences miniopterus----
#we set the prevalence to 0,4
#prevalence is 0,4=Presence(5000)/absences(x)= 12.500 
#we need to use randompoints() to find this 12.500 absences
#but before we need to convert a dataframe with lat=y and long=x


head(xypMinio)
colnames(xypMinio) <- c("x","y","presence")
head(xypMinio)

#or
#colnames(xypminio)[which(names(xypMinio) == "lon")] <- "x" 
#colnames(xypminio)[which(names(xypMinio) == "lat")] <- "y"

sample_abxy<- randomPoints(EuropePred, 12500, p=minio5000)

plot(sample_abxy)


#Absences melanitta----

head(xypmela)
colnames(xypmela) <- c("x","y","presence")



sample_abxymela<- randomPoints(EuropePred, 12500, p=mela5000)

plot(sample_abxymela)

#Absences quercus----

colnames(xypQuerc) <- c("x","y","presence")


sample_abxyQuerc<- randomPoints(EuropePred, 12500, p=querc5000)

plot(sample_abxyQuerc)




#Absences lagopus----
colnames(xypLagopus) <- c("x","y","presence")


sample_abxyLagopus<- randomPoints(EuropePred, 12500, p=lagopus5000)

plot(sample_abxyLagopus)


#Dataframe di dati uniti Pres/abs ----

sample_abxydf<-as.data.frame(sample_abxy)
sample_abxymeladf<-as.data.frame(sample_abxymela)
sample_abxyQuercdf<-as.data.frame(sample_abxyQuerc)
sample_abxyLagdf<-as.data.frame(sample_abxyLagopus)


sample_abxydf$presence<-0

xypmela$presence<-1
sample_abxymeladf$presence<-0

sample_abxyQuercdf$presence<-0
xypQuerc$presence<-1

sample_abxyLagdf$presence<-0

#merge 2 dataframe----
minPresAbs<-rbind(sample_abxydf, xypMinio)
melaPresAbs<-rbind(sample_abxymeladf, xypmela)
quercPresAbs<-rbind(sample_abxyQuercdf, xypQuerc)
lagPresAbs<-rbind(sample_abxyLagdf, xypLagopus )



predictors_min<- raster::extract(EuropePred, minPresAbs[,1:2], df=T)
predictors_min<- predictors_min[,-1]

predictors_mela<- raster::extract(EuropePred, melaPresAbs[,1:2], df=T)
predictors_mela<- predictors_mela[,-1]

predictors_quer<- raster::extract(EuropePred, quercPresAbs[,1:2], df=T)
predictors_quer<- predictors_quer[,-1]

predictors_lag<- raster::extract(EuropePred, lagPresAbs[,1:2], df=T)
predictors_lag<- predictors_lag[,-1]


#collinearity----

vif(predictors_min)
vifmin<-vifcor(predictors_min, th=0.9)
vifminstep<-vifstep(predictors_min,th=10)
predictors_min<-exclude(predictors_min,vifminstep)


vif(predictors_mela)
vifmelastep<-vifstep(predictors_mela,th=10)
predictors_mela<-exclude(predictors_mela,vifmelastep)


vif(predictors_min)
vifquerstep<-vifstep(predictors_quer,th=10)
predictors_quer<-exclude(predictors_quer,vifquerstep)


vif(predictors_min)
viflagstep<-vifstep(predictors_lag,th=10)
predictors_lag<-exclude(predictors_lag,viflagstep)


#Sdm data-----



sdmData_min<-data.frame(cbind(minPresAbs, predictors_min))
sdmData_mela<-data.frame(cbind(melaPresAbs, predictors_mela))
sdmData_quer<-data.frame(cbind(quercPresAbs, predictors_quer))
sdmData_lag<-data.frame(cbind(lagPresAbs, predictors_lag))

sdmData_min
sdmData_mela
sdmData_quer
sdmData_lag

#Favourability model miniopterus----

FavModel_min<-multGLM(sdmData_min, sp.cols = 3, var.cols=4:10, family = "binomial",step = FALSE, Y.prediction = TRUE, P.prediction = TRUE, Favourability = TRUE)
FavModel_mela<-multGLM(sdmData_mela, sp.cols = 3, var.cols=4:10, family = "binomial",step = FALSE, Y.prediction = TRUE, P.prediction = TRUE, Favourability = TRUE)
FavModel_quer<-multGLM(sdmData_quer, sp.cols = 3, var.cols=4:10, family = "binomial",step = FALSE, Y.prediction = TRUE, P.prediction = TRUE, Favourability = TRUE)
FavModel_lag<-multGLM(sdmData_lag, sp.cols = 3, var.cols=4:10, family = "binomial",step = FALSE, Y.prediction = TRUE, P.prediction = TRUE, Favourability = TRUE)



#Get pred ----

#prima di fare getPred bisogna estendere la prediction su tutta l'estensione quindi creo data.frame di values in x e y di tutta l'estensione
EuropePred <- stack(EuropePred) # it needs to be RasterStack, EuropePred is RasterBrick

FavPred_min<- getPreds(EuropePred, models=FavModel_min$models, id.col = NULL, Y = FALSE, P = FALSE, Favourability = TRUE)
FavPred_mela<- getPreds(EuropePred, models=FavModel_mela$models, id.col = NULL, Y = FALSE, P = FALSE, Favourability = TRUE)
FavPred_quer<- getPreds(EuropePred, models=FavModel_quer$models, id.col = NULL, Y = FALSE, P = FALSE, Favourability = TRUE)
FavPred_lag<- getPreds(EuropePred, models=FavModel_lag$models, id.col = NULL, Y = FALSE, P = FALSE, Favourability = TRUE)

#We can use colorist----
fav4sp<-stack(FavPred_min,FavPred_mela,FavPred_quer,FavPred_lag)

#Metrics pull----

metrics<-metrics_pull(fav4sp)

#Palette----

#palette<-palette_set(fav4sp)
palette<-palette_set(4, custom_hues = c(54, 130,219, 313))


#Map multiples----
mapmult<-map_multiples(metrics, palette, ncol = 2, lambda_i = -5, labels = names(fav4sp))


#Metrics distill----
metricsdist<- metrics_distill(fav4sp)


#Map single----
mapdist<-map_single(metricsdist,palette, lambda_i = 5) #how can i overlap geometry?
#legend----
legend<-legend_set(palette, group_labels = names(fav4sp))

#Download and import worldclim for the future----

#ssp585

bioc21_40_585<-brick("wc2.1_2.5m_bioc_CNRM-CM6-1_ssp585_2021-2040.tif")

#bio1<-getData('CMIP6', var='bio', res=2.5, rcp=85, model='CN', year=50)#how can i choice the rcp(Representative Concentration Pathway)
#impossible to run because CMPI6 doesn't exist in this function. it is not recognized


#Prediction rasterstack climate change----
predictors_future_min<- raster::extract(bioc21_40_126, minPresAbs[,1:2], df=FALSE) 
predictors_future_mela<- raster::extract(bioc21_40_126, melaPresAbs[,1:2], df=FALSE) 
predictors_future_querc<- raster::extract(bioc21_40_126, quercPresAbs[,1:2], df=FALSE) 
predictors_future_lag<- raster::extract(bioc21_40_585, LagPresAbs[,1:2], df=FALSE) 


#Dataframe pres/abs and predictors----
sdmData_min_future<-data.frame(cbind(minPresAbs, predictors_future_min))
sdmData_mela_future<-data.frame(cbind(melaPresAbs, predictors_future_mela))
sdmData_querc_future<-data.frame(cbind(quercPresAbs, predictors_future_querc))
sdmData_lag_future<-data.frame(cbind(LagPresAbs, predictors_future_lag))


#Multglm----

FavModel_min_future<-multGLM(sdmData_min_future, sp.cols = 3, var.cols=4:8, family = "binomial",step = FALSE, Y.prediction = TRUE, P.prediction = TRUE, Favourability = TRUE)
FavModel_mela_future<-multGLM(sdmData_mela_future, sp.cols = 3, var.cols=4:8, family = "binomial",step = FALSE, Y.prediction = TRUE, P.prediction = TRUE, Favourability = TRUE)
FavModel_querc_future<-multGLM(sdmData_querc_future, sp.cols = 3, var.cols=4:8, family = "binomial",step = FALSE, Y.prediction = TRUE, P.prediction = TRUE, Favourability = TRUE)
FavModel_lag_future<-multGLM(sdmData_lag_future, sp.cols = 3, var.cols=4:8, family = "binomial",step = FALSE, Y.prediction = TRUE, P.prediction = TRUE, Favourability = TRUE)


#Stack bio variables from worldclim----
fut_pred <-stack(bioc21_40_126)

#We can use getPreds----
FuturePred_min<- getPreds(fut_pred,models=FavModel_min_future$models, id.col=NULL, Y=FALSE, P = FALSE, Favourability = TRUE)
FuturePred_mela<- getPreds(fut_pred,models=FavModel_mela_future$models, id.col=NULL, Y=FALSE, P = FALSE, Favourability = TRUE)
FuturePred_querc<- getPreds(fut_pred,models=FavModel_querc_future$models, id.col=NULL, Y=FALSE, P = FALSE, Favourability = TRUE)
FuturePred_lag<- getPreds(fut_pred,models=FavModel_lag_future$models, id.col=NULL, Y=FALSE, P = FALSE, Favourability = TRUE)


#Difference between present and future----

dif_minPresFut <- (FavPred_min-FuturePred_min)
dif_melaPresFut <- (FavPred_mela-FuturePred_mela)
dif_quercPresFut <- (FavPred_quer-FuturePred_querc)
dif_lagPresFut <- (FavPred_lag-FuturePred_lag)

#Warning message:
#  In FavPred_quer - FuturePred_querc :
#  Raster objects have different extents. Result for their intersection is returned

plot(dif_minPresFut)
plot(dif_melaPresFut)
plot(dif_quercPresFut)
plot(dif_lagPresFut)
