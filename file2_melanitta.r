mela<-read.csv("melanitta.csv")





table(mela$basisOfRecord)

#Filter data minio----

mela<- mela%>%
  filter(!is.na(decimalLatitude))%>%
  filter(!is.na(decimalLongitude))%>%
  filter(year>1980)%>%
  filter(coordinateUncertaintyInMeters<100)%>%
  filter(!is.na(coordinateUncertaintyInMeters))%>%
  filter(basisOfRecord %in% c("HUMAN_OBSERVATION", "OBSERVATION"))

class(mela)
dim(mela)
nrow(mela)

#Create miniogeo (lon, lat)----
melageo<-mela%>%
  select(decimalLongitude,decimalLatitude)
head(melageo)

melageo$species<-1
head(melageo)
nrow(melageo)

#Create a spatial obj------

colnames(melageo) <- c("lon","lat","species")
class(melageo)

coordinates(melageo) <-c("lon","lat") #create a spatial obj
#or #coordinates(miniogeo) <-  ~ lon + lat 
#crs(minio) <- "+proj=longlat"


#Set correct datum and epsg----
crs(melageo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(melageo) <- CRS("+init=epsg:4326")

#Worlclim data and Europe map-----
Europe <- ne_countries(scale="medium", type="map_units", returnclass="sf", continent="Europe")
Worldclim<-raster::getData('worldclim', var='bio', res=2.5) #Valid resolutions are 0.5, 2.5, 5, and 10 (minutes of a degree). In the case of res=0.5, you must also provide a lon and lat argument for a tile

Europe <- Europe %>%
  dplyr::select(geometry,name_long)  %>%  #Be careful, the class names of dataset may be different!                         
  filter(name_long!='Russian Federation')


plot(Worldclim[[1]]) #or plot(worldclim$bio1)
plot(st_geometry(Europe))
points(melageo, cex=0.1)

plot(melageo)

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
melageo<-crop(melageo,extSpnPrt) 
SpainPort<-crop(EuropePred,extSpnPrt)

#points(miniogeo)
#bio1<-crop(Worldclim,e)
#plot(bio1[[1]])
#points(miniogeo,pch=20, cex=0.2, col="red")

plot(SpainPort$bio1)
points(melageo, cex=0.01)

#Alternative map rworldmap (low and high resolution)-----
library(rworldxtra)
library(rworldmap)

newmap <- getMap(resolution = "low")
maphigh<- getMap(resolution = "high")

#maphigh<-maphigh%>%
#  as.data.frame()

#sample 5000----

set.seed(999)

mela5000<- melageo%>%
  as.data.frame()%>%
  sample_n(5000)

view(mela5000)
nrow(mela5000)
head(mela5000)


#plot sample5000----

coordinates(mela5000) <-c("lon","lat")

crs(mela5000) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(mela5000) <- CRS("+init=epsg:4326")

plot(SpainPort$bio1)
points(mela5000,pch=1, cex=0.09, col="red")


#(Re)create a dataframe----

view(mela5000)
xypmela<-as.data.frame(mela5000,row.names = NULL) #convert a spatial points in a dataframe
view(xypmela)
view(xypmela)
nrow(xypmela)
nrow(xypmela)


#absences----
#we set the prevalence to 0,4
#prevalence is 0,4=Presence(5000)/absences(x)= 12.500 
#we need to use randompoints() to find this 12.500 absences
#but before we need to convert a dataframe with lat=y and long=x


head(xypmela)
colnames(xypmela) <- c("x","y","presence")

#or
#colnames(xypminio)[which(names(xypMinio) == "lon")] <- "x" 
#colnames(xypminio)[which(names(xypMinio) == "lat")] <- "y"


sample_abxymela<- randomPoints(EuropePred, 12500,ext=SpainPort, p=mela5000)

plot(sample_abxymela)
head(sample_abxymela)
nrow(sample_abxymela)


#Dataframe di dati uniti Pres/abs----

class(sample_abxymela)

sample_abxymeladf<-as.data.frame(sample_abxymela)

nrow(sample_abxymeladf)
class(sample_abxymeladf)
head(sample_abxymeladf)


xypmela$presence<-1
sample_abxymeladf$presence<-0


#merge 2 dataframe
melaPresAbs<-rbind(sample_abxymeladf, xypmela)

#coordinates(minPresAbs)<-c("x","y")
view(melaPresAbs)


#-----

predictors<- raster::extract(EuropePred, melaPresAbs[,1:2], df=FALSE)


sdmData<-data.frame(cbind(melaPresAbs, predictors))

sdmData
view(sdmData)

#----

FavModel<-multGLM(sdmData, sp.cols = 3, var.cols=4:22, family = "binomial",step = FALSE, Y.prediction = TRUE, P.prediction = TRUE, Favourability = TRUE)

FavModel
#----

#validation data ??


