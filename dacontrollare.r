install.packages("grDevices ")
library(grDevices )
library(raster)
library(dismo)
library(dplyr)
library(rnaturalearth)
library(caret)



#Import data miniopterus
gbif("Miniopterus", "schreibersii" , download=F)
minio<- gbif("Miniopterus", "schreibersii" , download=T)


#Filter data miniopterus

minio<- minio%>%
  filter(!is.na(lat))%>%
  filter(!is.na(lon))%>%
  filter(year>1980)%>%
  filter(basisOfRecord %in% c("HUMAN_OBSERVATION", "OBSERVATION"))


#Create miniogeo (lon, lat)
miniogeo<-minio%>%
  select(lon,lat)
head(miniogeo)

miniogeo$species<-1

coordinates(miniogeo) <-c("lon","lat")


#set  crs
crs(miniogeo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(miniogeo) <- CRS("+init=epsg:4326")

#Worlclim data and Europe map
Europe <- ne_countries(scale="medium", type="map_units", returnclass="sf", continent="Europe")

Worldclim<-raster::getData('worldclim', var='bio', res=2.5) 

Europe <- Europe %>%
  dplyr::select(geometry,name_long)  %>%  #Be careful, the class names of dataset may be different!                         
  filter(name_long!='Russian Federation')


plot(Worldclim[[1]]) #or plot(worldclim$bio1)
plot(st_geometry(Europe))
points(miniogeo, cex=0.1)


envData<-crop(Worldclim, Europe)
EuropePred <- mask(envData, Europe) #we create a new raster without NA value 


#sample 5000
set.seed(999)

minio5000<- miniogeo%>%
  as.data.frame()%>%
  sample_n(5000)

coordinates(minio5000) <-c("lon","lat")

crs(minio5000) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(minio5000) <- CRS("+init=epsg:4326")


#(Re)create a dataframe miniopterus
xypMinio<-as.data.frame(minio5000,row.names = NULL) #convert a spatial points in a dataframe


#Absences miniopterus
colnames(xypMinio) <- c("x","y","presence")

sample_abxy<- randomPoints(EuropePred, 12500, p=minio5000)


#dataframe uniti pres/abs
sample_abxydf<-as.data.frame(sample_abxy)
sample_abxydf$presence<-0

minPresAbs<-rbind(sample_abxydf, xypMinio)


#predictors
predictors_min<- raster::extract(EuropePred, minPresAbs[,1:2], df=T)
predictors_min<- predictors_min[,-1]

crs(minPresAbs)<-crs(EuropePred)

sdmData_min<-data.frame(cbind(minPresAbs, predictors_min))


FavModel_min<-multGLM(sdmData_min, sp.cols = 3, var.cols=4:22, family = "binomial",step = FALSE, Y.prediction = TRUE, P.prediction = TRUE, Favourability = TRUE)
EuropePred <- stack(EuropePred) # it needs to be RasterStack, EuropePred is RasterBrick

FavPred_min<- getPreds(EuropePred, models=FavModel_min$models, id.col = NULL, Y = FALSE, P = FALSE, Favourability = TRUE)

#We can use colorist----
fav4sp<-stack(FavPred_min, FavPred_mela)

#Metrics pull----

metrics<-metrics_pull(fav4sp)

#Palette----

palette<-palette_set(2, custom_hues = c(54, 130))
palette<-palette_set(2, custom_hues = c(219, 313))
colorspace::rainbow_hcl

#Map multiples----
mapmult<-map_multiples(metrics, palette, ncol = 2,labels = c("'Mioniopterus'", "'melanita'"), lambda_i = -5)
mapmult




#Metrics distill----
metricsdist<- metrics_distill(fav4sp)
library(colorist)

#Map single----
mapdist<-map_single(metricsdist,palette, lambda_i = 5)

#legend----
legend<-legend_set(palette, group_labels = c("mlenanitta", "gino"))
legend


as.data.frame(fav4sp)


install.packages("scales")
library(scales)
show_col(viridis_pal()(30))

