
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
library(fuzzySim)
library(sdm)
library(usdm)
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

#Create miniogeo (lon, lat)----
miniogeo<-minio%>%
  select(lon,lat)


miniogeo$species<-1

#Create a spatial obj------
coordinates(miniogeo) <-c("lon","lat") #create a spatial obj
                                       #or #coordinates(miniogeo) <-  ~ lon + lat 
                                           #crs(minio) <- "+proj=longlat"

#Set correct datum and epsg----
crs(miniogeo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(miniogeo) <- CRS("+init=epsg:4326")

#Worlclim data and Europe map-----
Europe <- ne_countries(scale="medium", type="map_units", returnclass="sf", continent="Europe")
Worldclim<-raster::getData('worldclim', var='bio', res=2.5) #Valid resolutions are 0.5, 2.5, 5, and 10 (minutes of a degree). In the case of res=0.5, you must also provide a lon and lat argument for a tile

Europe <- Europe %>%
  dplyr::select(geometry,name_long)  %>%  #Be careful, the class names of dataset may be different!                         
  filter(name_long!='Russian Federation')


#Envdata and Europepred----

envData<-crop(Worldclim, Europe)
EuropePred <- mask(envData, Europe) #we create a new raster without NA value 


#eSpnPrt<-drawExtent() #we can use for check the coordinates
#xmin       : -10.23854 
#xmax       : 9.322413 
#ymin       : 34.29422 
#ymax       : 55.99065 

extSpnPrt<-extent(c(-11,10,35,56))
miniogeo<-crop(miniogeo,extSpnPrt) 
SpainPort<-crop(EuropePred,extSpnPrt)




#sample 5000----

set.seed(999)

minio5000<- miniogeo%>%
  as.data.frame()%>%
  sample_n(5000)




#plot sample5000----

coordinates(minio5000) <-c("lon","lat")

crs(minio5000) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(minio5000) <- CRS("+init=epsg:4326")

plot(SpainPort$bio1)
points(minio5000,pch=1, cex=0.09, col="red")

#(Re)create a dataframe----


xypMinio<-as.data.frame(minio5000,row.names = NULL) #convert a spatial points in a dataframe
view(minio5000)
nrow(xypMinio)
nrow(minio5000)


#absences----
#we set the prevalence to 0,4
#prevalence is 0,4=Presence(5000)/absences(x)= 12.500 
#we need to use randompoints() to find this 12.500 absences
#but before we need to convert a dataframe with lat=y and long=x


colnames(xypMinio) <- c("x","y","presence") 

#or
#colnames(xypminio)[which(names(xypMinio) == "lon")] <- "x" 
#colnames(xypminio)[which(names(xypMinio) == "lat")] <- "y"


sample_abxy<- randomPoints(EuropePred, 12500,ext=SpainPort, p=minio5000)


#Dataframe di dati uniti Pres/abs----

class(sample_abxy)

sample_abxydf<-as.data.frame(sample_abxy)

nrow(sample_abxydf)
class(sample_abxydf)

sample_abxydf$presence<-0


#merge 2 dataframe
minPresAbs<-rbind(sample_abxydf, xypMinio)

#coordinates(minPresAbs)<-c("x","y")



#-----

predictors_min<- raster::extract(EuropePred, minPresAbs[,1:2], df=FALSE)


sdmData_min<-data.frame(cbind(minPresAbs, predictors_min))


#----

FavModel_min<-multGLM(sdmData_min, sp.cols = 3, var.cols=4:22, family = "binomial",step = FALSE, Y.prediction = TRUE, P.prediction = TRUE, Favourability = TRUE)

# prima di fare getPred bisogna estendere la prediction su tutta l'estensione quindi creo data.frame di values in x e y di tutta l'estensione
EuropePred <- stack(EuropePred) # it needs to be RasterStack, EuropePred is RasterBrick
FavPred_min<- getPreds(EuropePred, models=FavModel_min$models, id.col = NULL, Y = FALSE, P = FALSE, Favourability = TRUE)
 
################# COME USARE COLORIST  ##############

#HAI TUTTE E 3 LE FAVOURABILITY IN RASTERLAYER

#1)FAI UNO STACK DEI 3 LAYER ES: FAV_3SP <- STACK("FAV1", "FAV2" "FAV3") DOVE FAV 1 è FavPred_min
#2) m<- metrics_pull(FAV_3SP)

# assign a color palette
#p <- palette_set(FAV_3SP)

# generate maps for each individual
#map_multiples(m, p, ncol = 2, lambda_i = -5, labels = names(elephant_ud))

#3)distill distribution information across individuals
#m_distill <- metrics_distill(FAV_3SP)

# visualize distilled information on a single map
#map_single(m_distill, p, lambda_i = -5)  colore in comune indica zone equamente condivise dalle specie... vediamo cosa si può ottenere da fuzzyOverlay e se posso confrontare i due dati


################## ORA LAVORIAMO CON LE PROIEZIONI CLIMATE CHANGE  ######################
# TEORIA:    https://www.wcrp-climate.org/images/modelling/WGCM/CMIP/CMIP6FinalDesign_GMD_180329.pdf
#            https://www.worldclim.org/data/cmip6/cmip6_clim2.5m.html

# https://www.worldclim.org/data/cmip6/cmip6_clim2.5m.html sceglierei dal 2021-2040, risoluzione 2.5 (stessa di EuropePred), scaricare tutti e 4 ssp rispetto a bc....scegli il modello che ti sembra più appropriato!



# ora carico i predictors in projection futura + faccio PCA per raster (????) o prima devo trasformare in data.frame e poi raster???


#ESEMPIO PCA CON MODELLI CMIP5 DA EFFETTUARE PER CMIP6

ac_85 <- getData('CMIP5', var='bio', res=10, rcp=85, model='AC', year=50)

ac_45 <- getData('CMIP5', var='bio', res=10, rcp=45, model='AC', year=50)

AC_model <- stack(ac_45, ac_85)

AC_model_Europe <- crop(AC_model, Europe)
library(RStoolbox)
set.seed(999)
rpc <- rasterPCA(AC_model_Europe, nComp=8, spca=TRUE)

summary(rpc$model) # i primi 8 componenti rappresentano circa il 100%...guardare "Proportion of Variance" e settare su rasterPC nComp in base a quanti componenti da 1:n rappresentano quasi la titalità della varinza
rpc$map

# facciamo la prediction rispetto al rasterstack climate change
predictors_future<- raster::extract(rpc$map, minPresAbs[,1:2], df=FALSE)


sdmData_min_future<-data.frame(cbind(minPresAbs, predictors_future))


#----

FavModel_min_future<-multGLM(sdmData_min_future, sp.cols = 3, var.cols=4:8, family = "binomial",step = FALSE, Y.prediction = TRUE, P.prediction = TRUE, Favourability = TRUE)
fut_pred <-stack(rpc$map)
FuturePred_min<- getPreds(fut_pred,models=FavModel_min_future$models, id.col=NULL, Y=FALSE, P = FALSE, Favourability = TRUE)




#######  QUESTI PASSAGGI VANNO RIPETURI PER LE ALTRE SPECIE E PLOTTATI SU COLORIST ALLO STESSO MODO   #######

########## CONFRONTO PREDICTION PRESENTE FUTURO #########

dif_minPresFut <- (FavPred_min-FuturePred_min)
plot(dif_minPresFut)
min_fuzzyRange <- fuzzyRangeChangee(FavPred_min, FuturePred_min)




########## LAVORO CHE VA FATTO UNA VOLTA CHE HAI LA FAV PER TUTTE LE SPECIE ##########
########## CONFRONTO PLOT COLORIST E FUZZYSIM...PROVIAMO UN PLOT UNICO???? ##########


#in teoria lo dovrei fare su la prediction di tutta europa quindi....
FavPred_min_DF <- as.data.frame(FavPred_min, xy=TRUE)
FavPred_mela_DF <- as.data.frame(FavPred_mela)
FavPred_quercus_DF <- as.data.frame(FavPred_quercus)

DF_Fav <- cbind(FavPred_min_DF[aggiungo solo colonna dei valori], FavPred_mela_DF, FavPred_quercus_DF)

Fav_intersect <- fuzzyOverlay(DF_Fav, op = "intersection")
Fav_union <- fuzzyOverlay(DF_Fav, op = "union")

Fav_intersect <- cbind(Fav_intersect, FavPred_min_DF[delle colonne x e y]) # ottengo raster usando la funzione rasterfromxyz
Fav_union <- cbind(Fav_intersect, FavPred_min_DF[delle colonne x e y])

# se non ho capito male quello che esce da map_distill layer specificity è come fav_intersect, forse specificity=0 è il valore di fav_intersect max

# proviamo ad sovrapporre i due layer rispetto a specifity = 0 e fav_ intersect = max ... dovrebbero essere la stessa cosa. 
# TROVA UN MODO PER FARLO E RAPPRESENTARLO SULLA MAPPA, nel caso ci guardiamo assieme!!!

