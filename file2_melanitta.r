#Import data from setwd()----
setwd("c:/tesi/tesi")

mela<-read.csv("melanitta.csv")

table(mela$basisOfRecord)

#Filter data melanitta----

mela<- mela%>%
  filter(!is.na(decimalLatitude))%>%
  filter(!is.na(decimalLongitude))%>%
  filter(!is.na(countryCode))%>%
  filter(year>1980)%>%
  filter(coordinateUncertaintyInMeters<100)%>%
  filter(!is.na(coordinateUncertaintyInMeters))%>%
  filter(countryCode !="US" & countryCode !="PM")%>%
  filter(basisOfRecord %in% c("HUMAN_OBSERVATION", "OBSERVATION"))

class(mela)
dim(mela)
nrow(mela)

#Create melageo (lon, lat)----
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
#or #coordinates(melageo) <-  ~ lon + lat 



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

plot(st_geometry(Europe))
points(mela5000,pch=1, cex=0.09, col="red")


#(Re)create a dataframe----

view(mela5000)
xypmela<-as.data.frame(mela5000,row.names = NULL) #convert a spatial points in a dataframe

view(xypmela)
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


sample_abxymela<- randomPoints(EuropePred, 12500, p=mela5000)

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


#Predictors and sdm data-----

predictors_mela<- raster::extract(EuropePred, melaPresAbs[,1:2], df=FALSE)


sdmData_mela<-data.frame(cbind(melaPresAbs, predictors_mela))

sdmData_mela
view(sdmData)

#favourability model----

FavModel_mela<-multGLM(sdmData_mela, sp.cols = 3, var.cols=4:22, family = "binomial",step = FALSE, Y.prediction = TRUE, P.prediction = TRUE, Favourability = TRUE)

FavModel

#Getpred mela----
EuropePred <- stack(EuropePred) # it needs to be RasterStack, EuropePred is RasterBrick
FavPred_mela<- getPreds(EuropePred, models=FavModel_mela$models, id.col = NULL, Y = FALSE, P = FALSE, Favourability = TRUE)
