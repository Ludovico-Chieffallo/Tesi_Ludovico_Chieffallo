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
nrow(miniogeo)

#create a spatial obj------

class(miniogeo)
coordinates(miniogeo) <-c("lon","lat") #create a spatial obj
class(miniogeo)


#worlcclim data and europe map-----

#-----
crs(miniogeo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(miniogeo) <- CRS("+init=epsg:4326")


Europe <- ne_countries(scale="medium", type="map_units", returnclass="sf", continent="Europe")  #PUò ESSERE MODIFICATO?
Worldclim<-raster::getData('worldclim', var='bio', res=2.5) #Valid resolutions are 0.5, 2.5, 5, and 10 (minutes of a degree). In the case of res=0.5, you must also provide a lon and lat argument for a tile
Europe <- Europe %>%
  dplyr::select(geometry,name_long)  %>%  #Be careful, the class names of your dataset may be different!                         
  filter(name_long!='Russian Federation')


plot(Worldclim[[1]]) #plot(worldclim$bio1)
plot(st_geometry(Europe))
points(miniogeo)

#envdata----

envData<-crop(Worldclim, Europe)
EuropePred <- mask(envData, Europe)
#Error in x$.self$finalize() : attempt to apply non-function


#extend and crop map----

plot(Worldclim[[1]]) #example
points(miniogeo)

eSpnPrt<-drawExtent()
#xmin       : -10.23854 
#xmax       : 9.322413 
#ymin       : 34.29422 
#ymax       : 55.99065 

ext<-extent(c(-11,10,35,56))

miniogeo<-crop(miniogeo,ext) #cancellare

#points(miniogeo)
#bio1<-crop(Worldclim,e)
#plot(bio1[[1]])
#points(miniogeo,pch=20, cex=0.2, col="red")


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

set.seed(999) #?

minio5000<- miniogeo%>%
  as.data.frame()%>%
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

eSpnPrt<-drawExtent()
minio5000<-crop(minio5000,e) 
pltminio5000<-crop(maphigh,e)
plot(pltminio5000)
points(minio5000,pch=20, cex=0.2, col="red")

#(Re)create a dataframe----

view(minio5000)
xypMinio<-as.data.frame(minio5000,row.names = NULL,optional = FALSE) #convert to a spatial points in a dataframe
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
colnames(minio5000) <-c("x","y","presence")
#or
#colnames(minio5000)[which(names(xypMinio) == "lon")] <- "x" 
#colnames(minio5000)[which(names(xypMinio) == "lat")] <- "y"
#Delete the previous column (species)



#IL DATAFRAME LO CREO SOLO CON LE COLONNE LON E LAT?
#O MI CONVIENE AGGIUNGERE ANCHE UNA COLONNA "presenCE" E DARE GIà VALORE 1?
#COSì DOPO QUANDO DOVRò UNIRE ANCHE IL DATAFRAME DELLE ASSENZE AVRO' GIA' VALORI 1 E 0


sample_abxy<- randomPoints(envData, 12500,ext=eSpnPrt, p=minio5000)

plot(sample_abxy)
head(sample_abxy)
nrow(sample_abxy)

#C'E' IL PROBLEMA DEL DATUM, CON EUROPA NON COINCIDONO I DATI MENTRE CON MAPHIGH SI
#HO PROVATO CON optional = FALSE
                #crs(envData) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"


#Dataframe di dati uniti Pres/abs----

class(sample_abxy)

sample_abxydf<-as.data.frame(sample_abxy)

nrow(sample_abxydf)
class(sample_abxydf)
head(sample_abxydf)
head(xypMinio)



sample_abxydf$presence<-0
xypMinio$presence<-1

#merge 2 dataframe
minPresAbs<-rbind(sample_abxydf, minio5000)
#coordinates(minPresAbs)<-c("x","y")
view(minPresAbs)


#-----

predictors<- raster::extract(envData, minPresAbs[,1:2], df=FALSE)


sdmData<-data.frame(cbind(minPresAbs, predictors))
sdmData
view(sdmData)

#----

FavModel<-multGLM(sdmData, sp.cols = 3, var.cols=4:22, family = "binomial",step = FALSE, Y.prediction = TRUE, P.prediction = TRUE, Favourability = TRUE)


#----
#validation data 
