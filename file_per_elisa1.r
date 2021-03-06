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

#Import data miniopterus---- 

gbif("Miniopterus", "schreibersii" , download=F)
minio<- gbif("Miniopterus", "schreibersii" , download=T)
#Import data melanitta----
setwd("c:/tesi/tesi")

mela<-read.csv("melanitta.csv")

#Import data quercus ---- 

quercus<-read.csv("quercus.csv")

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

plot(st_geometry(Europe))
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


#(Re)create a dataframe miniopterus----

xypMinio<-as.data.frame(minio5000,row.names = NULL) #convert a spatial points in a dataframe
xypmela<-as.data.frame(mela5000,row.names = NULL) 
xypQuerc<-as.data.frame(querc5000,row.names = NULL)


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




#Dataframe di dati uniti Pres/abs miniopterus----

sample_abxydf<-as.data.frame(sample_abxy)


sample_abxydf$presence<-0

#merge 2 dataframe    #########################################################################
minPresAbs<-rbind(sample_abxydf, xypMinio)


#Dataframe di dati uniti Pres/abs melanitta----


sample_abxymeladf<-as.data.frame(sample_abxymela)

xypmela$presence<-1
sample_abxymeladf$presence<-0

#merge 2 dataframe   #################### CONTROLLA DA QUI ############################
melaPresAbs<-rbind(sample_abxymeladf, xypmela)
predictors_mela<- raster::extract(EuropePred, melaPresAbs[,1:2], df=T)
predictors_mela <- predictors_mela[,-1] ###### controlla che ci siano tutte le coordinate giuste
## collinearity test
Vars_to_remove <- data.frame(BIO=findCorrelation(cor(predictors_mela), cutoff = .6, names = T))
intersection1 <- colnames(predictors_mela) %in% Vars_to_remove$BIO
## remove correlated variables
predictors_mela <- predictors_mela[!intersection1]
sdmData_mela<- data.frame(pres =melaPresAbs[,1], predictors_mela[1:ncol(predictors_mela)]) #### se pres = melaPresAbs controlla che ci sia solo una colonna pres abs





XY<-as.data.frame(EuropePred[[1]], xy=TRUE, na.rm=TRUE)
PTXY=data.frame("x" = XY$x, "y"=XY$y)
predictorsDF<- raster::extract(Predictors,PTXY)
predictorsDF<-data.frame(predictorsDF)
## remove correlated variables
intersection2 <- colnames(predictorsDF) %in% Vars_to_remove$BIO
preds <- predictorsDF[!intersection2]


## Favourability

Model<-multGLM(sdmData_mela, sp.cols = 1, var.cols=2:ncol(sdmData_mela), family = "binomial",
                  step = FALSE, FDR = FALSE, trim = FALSE, Y.prediction = FALSE, 
                  P.prediction = TRUE, Favourability = TRUE) 

Pred<- getPreds(preds, models=Model$models, id.col = NULL, Y = FALSE, P = FALSE,
                   Favourability = TRUE)
PredDataFav_mela=data.frame("x" = XY$x, "y"=XY$y, "fav" = Pred$pres_F)
fav_mela<- rasterFromXYZ(PredDataFav_mela) 

#Dataframe di dati uniti Pres/abs quercus----


sample_abxyQuercdf<-as.data.frame(sample_abxyQuerc)

sample_abxyQuercdf$presence<-0
xypQuerc$presence<-1

#merge 2 dataframe
quercPresAbs<-rbind(sample_abxyQuercdf, xypQuerc)


#Predictor and sdm data-----

predictors_min<- raster::extract(EuropePred, minPresAbs[,1:2], df=FALSE)
predictors_mela<- raster::extract(EuropePred, melaPresAbs[,1:2], df=FALSE)
predictors_quer<- raster::extract(EuropePred, quercPresAbs[,1:2], df=FALSE)


sdmData_min<-data.frame(cbind(minPresAbs, predictors_min))
sdmData_mela<-data.frame(cbind(melaPresAbs, predictors_mela))
sdmData_quer<-data.frame(cbind(quercPresAbs, predictors_quer))


sdmData_min
sdmData_mela
sdmData_quer

#Favourability models----

FavModel_min<-multGLM(sdmData_min, sp.cols = 3, var.cols=4:22, family = "binomial",step = FALSE, Y.prediction = TRUE, P.prediction = TRUE, Favourability = TRUE)
FavModel_mela<-multGLM(sdmData_mela, sp.cols = 3, var.cols=4:22, family = "binomial",step = FALSE, Y.prediction = TRUE, P.prediction = TRUE, Favourability = TRUE)
FavModel_quer<-multGLM(sdmData_quer, sp.cols = 3, var.cols=4:22, family = "binomial",step = FALSE, Y.prediction = TRUE, P.prediction = TRUE, Favourability = TRUE)

FavModel_min
FavModel_mela
FavModel_quer

#Get pred----

EuropePred <- stack(EuropePred) # it needs to be RasterStack, EuropePred is RasterBrick

FavPred_min<- getPreds(EuropePred, models=FavModel_min$models, id.col = NULL, Y = FALSE, P = FALSE, Favourability = TRUE)
FavPred_mela<- getPreds(EuropePred, models=FavModel_mela$models, id.col = NULL, Y = FALSE, P = FALSE, Favourability = TRUE)
FavPred_quer<- getPreds(EuropePred, models=FavModel_quer$models, id.col = NULL, Y = FALSE, P = FALSE, Favourability = TRUE)


#We can use colorist----
fav3sp<-stack(FavPred_min,FavPred_mela,FavPred_quer)

#Metrics pull----

metrics<-metrics_pull(fav3sp)

#Palette----

palette<-palette_set(fav3sp)

#Map multiples----
map_multiples(metrics, palette, ncol = 2, lambda_i = -5, labels = names(fav3sp))


#Metrics distill----
metricsdist<- metrics_distill(fav3sp)


#Map single----
map_single(metricsdist,palette, lambda_i = 5) #how can i overlap geometry?

#Download and import worldclim for the future----

#ssp126
bioc21_40_126<-brick("wc2.1_2.5m_bioc_CNRM-CM6-1_ssp126_2021-2040.tif")

#ssp585
bioc21_40_585<-brick("wc2.1_2.5m_bioc_CNRM-CM6-1_ssp585_2021-2040.tif")


#Stack bio variables from worldclim---- 
cropBioc21_40_126<-crop(bioc21_40_126, Europe)
bioc21_40_126_mask<- mask(cropBioc21_40_126, Europe)
fut_pred <-stack(bioc21_40_126_mask)

plot(fut_pred$wc2.1_2.5m_bioc_CNRM.CM6.1_ssp126_2021.2040.1)


#We can use getPreds----
FuturePred_min<- getPreds(fut_pred,models=FavModel_min$models, id.col=NULL, Y=FALSE, P = FALSE, Favourability = TRUE)
FuturePred_mela<- getPreds(fut_pred,models=FavModel_mela$models, id.col=NULL, Y=FALSE, P = FALSE, Favourability = TRUE)
FuturePred_querc<- getPreds(fut_pred,models=FavModel_quer$models, id.col=NULL, Y=FALSE, P = FALSE, Favourability = TRUE)


#Difference between present and future----

dif_minPresFut <- (FavPred_min-FuturePred_min)
dif_melaPresFut <- (FavPred_mela-FuturePred_mela)
dif_quercPresFut <- (FavPred_quer-FuturePred_querc)

#Warning message:
#  In FavPred_quer - FuturePred_querc :
#  Raster objects have different extents. Result for their intersection is returned

plot(dif_minPresFut)
plot(dif_melaPresFut)
plot(dif_quercPresFut)

#Fuzzy(!!!!ERROR!!!!)----
min_fuzzyRange <- fuzzyRangeChange(FavPred_min, FuturePred_min)
mela_fuzzyRange <- fuzzyRangeChange(FavPred_mela, FuturePred_mela)
querc_fuzzyRange <- fuzzyRangeChange(FavPred_quer, FuturePred_querc)
#Error in fuzzyRangeChange(FavPred_min, FuturePred_min, ylim = c(-1, 1)) : 
#length(pred1) == length(pred2) is not TRUE
#LAVORO SU CUI NON SI PUO' LAVORARE ANCORA----
#in teoria lo dovrei fare su la prediction di tutta europa quindi....
FavPred_min_DF <- as.data.frame(FavPred_min, xy=TRUE)
FavPred_mela_DF <- as.data.frame(FavPred_mela)
FavPred_quercus_DF <- as.data.frame(FavPred_quercus)

DF_Fav <- cbind(FavPred_min_DF[aggiungo solo colonna dei valori], FavPred_mela_DF, FavPred_quercus_DF)

Fav_intersect <- fuzzyOverlay(DF_Fav, op = "intersection")
Fav_union <- fuzzyOverlay(DF_Fav, op = "union")

Fav_intersect <- cbind(Fav_intersect, FavPred_min_DF[delle colonne x e y]) # ottengo raster usando la funzione rasterfromxyz
Fav_union <- cbind(Fav_intersect, FavPred_min_DF[delle colonne x e y])

# se non ho capito male quello che esce da map_distill layer specificity ?? simile a fav_intersect, forse specificity=0 ?? simila al valore di fav_intersect max ( i due metodi di calcolo probabilmente sono diveri)

# proviamo ad sovrapporre i due layer rispetto a specifity = 0 e fav_ intersect = max ... tipo plotto con ggplot2 sopra a map_single un poligono che riprende l'area di fav_intersect max (sulla vignetta colorist in basso c'?? il tutorial per ggplot2). 
# TROVA UN MODO PER FARLO E RAPPRESENTARLO SULLA MAPPA, nel caso ci guardiamo assieme!!!




######### PERFORMANCE SDM ##########################



RocAucFunz <- function(ModelDatabase=NULL,TrainValue = 0.8, TestValue = 0.2) {
  library(groupdata2)
  library(ROCR)
  set.seed(6949)
  inds <- partition(ModelDatabase, p = c(train = 0.8, test = 0.2))
  train <- as.data.frame(inds[1])
  test <- as.data.frame(inds[2])
  validation<-test
  training<-train
  
  #Favourability
  Model<-multGLM(training, sp.cols = 1, var.cols=2:ncol(training), family = "binomial",
                 step = FALSE, FDR = FALSE, trim = FALSE, Y.prediction = FALSE, 
                 P.prediction = TRUE, Favourability = TRUE)
  Pred<- getPreds(validation, models=Model$models, id.col = NULL, Y = FALSE, P = TRUE,
                  Favourability = TRUE)
  Fav <-prediction(Pred$pres_F, validation$pres, label.ordering = NULL)
  #ROC
  ROCperfFAV<-performance(Fav, measure="tpr", x.measure="fpr")
  #AUC
  AUCperfFAV<-performance(Fav, measure="auc")
  AUCperfFAV<-unlist(slot(AUCperfFAV, "y.values"))
  AUCperfFAV<-round(AUCperfFAV,4)
 
  
  ROC=ROCperfFAV
  AUC=AUCperfFAV
 
  
  
  Outputs=list("ROC" =ROC, "AUC"=AUC)
  
  return(Outputs)
}


#### ModelDatabase= basta che metti il modello, per capirci: Model$models di
#Model<-multGLM(sdmData_mela, sp.cols = 1, var.cols=2:ncol(sdmData_mela), family = "binomial",
                  #step = FALSE, FDR = FALSE, trim = FALSE, Y.prediction = FALSE, 
                  #P.prediction = TRUE, Favourability = TRUE) 

#Pred<- getPreds(preds, models=Model$models, id.col = NULL, Y = FALSE, P = FALSE,
                  # Favourability = TRUE)
