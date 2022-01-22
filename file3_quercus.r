#import data quercus ---- 

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

#Create quercgeo (lon, lat)----

quercgeo<-quercus%>%
  select(decimalLongitude,decimalLatitude)
head(quercgeo)

quercgeo$species<-1

colnames(quercgeo) <- c("lon","lat","species")
class(quercgeo)
head(quercgeo)

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

str(querc5000)
plot(st_geometry(Europe))
points(querc5000,pch=1, cex=0.1, col="red")


#(Re)create a dataframe----

xypQuerc<-as.data.frame(querc5000,row.names = NULL) #convert a spatial points in a dataframe
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
#colnames(xypminio)[which(names(xypQuerc) == "lon")] <- "x" 
#colnames(xypminio)[which(names(xypQuerc) == "lat")] <- "y"


sample_abxyQuerc<- randomPoints(EuropePred, 12500, p=querc5000)

plot(sample_abxyQuerc)
head(sample_abxyQuerc)
nrow(sample_abxyQuerc)


#Dataframe di dati uniti Pres/abs----

class(sample_abxyQuerc)

sample_abxyQuercdf<-as.data.frame(sample_abxyQuerc)

sample_abxyQuercdf$presence<-0
xypQuerc$presence<-1

#merge 2 dataframe
quercPresAbs<-rbind(sample_abxyQuercdf, xypQuerc)

view(quercPresAbs)


#Predictors and sdm data-----

predictors_quer<- raster::extract(EuropePred, quercPresAbs[,1:2], df=FALSE)


sdmData_quer<-data.frame(cbind(quercPresAbs, predictors_quer))

sdmData_quer


#favourability model----

FavModel_quer<-multGLM(sdmData, sp.cols = 3, var.cols=4:22, family = "binomial",step = FALSE, Y.prediction = TRUE, P.prediction = TRUE, Favourability = TRUE)

FavModel

#Getpred quer----
EuropePred <- stack(EuropePred) # it needs to be RasterStack, EuropePred is RasterBrick
FavPred_quer<- getPreds(EuropePred, models=FavModel_quer$models, id.col = NULL, Y = FALSE, P = FALSE, Favourability = TRUE)

