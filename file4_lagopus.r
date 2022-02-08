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

#Load data (miniopterus=minio)---- 

gbif("Lagopus", "muta",download=F)

lagopus<- gbif("Lagopus", "muta",download=T)

table(lagopus$basisOfRecord)

#Filter data minio----

lagopus<- lagopus%>%
  filter(!is.na(lat))%>%
  filter(!is.na(lon))%>%
  filter(year>1980)%>%
  filter(basisOfRecord %in% c("HUMAN_OBSERVATION", "OBSERVATION"))

class(lagopus)
dim(lagopus)
nrow(lagopus)

#Create miniogeo (lon, lat)----
lagopusgeo<-lagopus%>%
  select(lon,lat)
head(lagopusgeo)

lagopusgeo$species<-1

#Create a spatial obj------

class(lagopusgeo)
coordinates(lagopusgeo) <-c("lon","lat") #create a spatial obj
#or #coordinates(miniogeo) <-  ~ lon + lat 
#crs(minio) <- "+proj=longlat"
head(miniogeo)

#Set correct datum and epsg----
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
points(lagopusgeo , cex=0.1)


#Envdata and Europepred----

envData<-crop(Worldclim, Europe)
EuropePred <- mask(envData, Europe) #we create a new raster without NA value 


par(mfrow=c(1,2))
plot(envData$bio1)
plot(EuropePred$bio1)
dev.off()

#extend and crop map----

plot(EuropePred[[1]]) #example
points(lagopusgeo, cex=0.2)

#sample 5000----

set.seed(999)

lagopus5000<- lagopusgeo%>%
  as.data.frame()%>%
  sample_n(5000)

view(lagopus5000)
nrow(lagopus5000)
head(lagopus5000)


#plot sample5000----

coordinates(lagopus5000) <-c("lon","lat")

crs(lagopus5000) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(lagopus5000) <- CRS("+init=epsg:4326")


plot(EuropePred$bio1)
points(lagopus5000,pch=1, cex=0.1, col="blue")

#(Re)create a dataframe----


xypLagopus<-as.data.frame(lagopus5000,row.names = NULL) #convert a spatial points in a dataframe


#absences----

colnames(xypLagopus) <- c("x","y","presence")


sample_abxyLagopus<- randomPoints(EuropePred, 12500, p=lagopus5000)

plot(sample_abxyLagopus)


#Dataframe di dati uniti Pres/abs----

sample_abxyLagdf<-as.data.frame(sample_abxyLagopus)
sample_abxyLagdf$presence<-0


#merge 2 dataframe
LagPresAbs<-rbind(sample_abxyLagdf, xypLagopus )

#coordinates(minPresAbs)<-c("x","y")
view(LagPresAbs)



#Predictor and sdm data-----


predictors_lagopus<- raster::extract(EuropePred, LagPresAbs[,1:2], df=FALSE)


sdmData_lag<-data.frame(cbind(LagPresAbs, predictors_lagopus))

sdmData_lag

#favourability model----

FavModel_lag<-multGLM(sdmData_lag, sp.cols = 3, var.cols=4:22, family = "binomial",step = FALSE, Y.prediction = TRUE, P.prediction = TRUE, Favourability = TRUE)

#get pred----

#prima di fare getPred bisogna estendere la prediction su tutta l'estensione quindi creo data.frame di values in x e y di tutta l'estensione
EuropePred <- stack(EuropePred) # it needs to be RasterStack, EuropePred is RasterBrick
FavPred_lag<- getPreds(EuropePred, models=FavModel_lag$models, id.col = NULL, Y = FALSE, P = FALSE, Favourability = TRUE)


#Download and import worldclim for the future----

#ssp126
bioc21_40_126<-brick("wc2.1_2.5m_bioc_CNRM-CM6-1_ssp126_2021-2040.tif")

#ssp585
setwd("C:/tesi/tesi")

bioc21_40_585<-brick("wc2.1_2.5m_bioc_CNRM-CM6-1_ssp585_2021-2040.tif")
plot(bioc21_40_585)

#Stack bio variables from worldclim---- 

cropBioc21_40_585<-crop(bioc21_40_585, Europe)
bioc21_40_585_mask<- mask(cropBioc21_40_585, Europe)
fut_pred <-stack(bioc21_40_585_mask)


#Prediction rasterstack climate change----
predictors_future_lag<- raster::extract(bioc21_40_585, LagPresAbs[,1:2], df=FALSE) 


#Dataframe pres/abs and predictors----
sdmData_lag_future<-data.frame(cbind(LagPresAbs, predictors_future_lag))



#Multglm----

FavModel_lag_future<-multGLM(sdmData_lag_future, sp.cols = 3, var.cols=4:8, family = "binomial",step = FALSE, Y.prediction = TRUE, P.prediction = TRUE, Favourability = TRUE)

#Stack bio variables from worldclim----
fut_pred <-stack(bioc21_40_585_mask)

#We can use getPreds----
FuturePred_lag<- getPreds(fut_pred,models=FavModel_lag_future$models, id.col=NULL, Y=FALSE, P = FALSE, Favourability = TRUE)

#Difference between present and future----

dif_lagPresFut <- (FavPred_lag-FuturePred_lag)

plot(dif_lagPresFut)
