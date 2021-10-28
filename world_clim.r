library(raster)
getData()
  
  
Worldclim<-raster::getData('worldclim', var='bio', res=10) #Valid resolutions are 0.5, 2.5, 5, and 10 (minutes of a degree). In the case of res=0.5, you must also provide a lon and lat argument for a tile

plot(Worldclim)


envData<-crop(Worldclim, extent(-12, 25, 36, 60))

plot(envData$bio1)
envData
