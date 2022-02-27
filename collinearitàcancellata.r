#predictors----
predictors_min<- raster::extract(EuropePred, minPresAbs[,1:2], df=T)
predictors_min<- predictors_min[,-1]

predictors_mela<- raster::extract(EuropePred, melaPresAbs, df=T)
predictors_mela<- predictors_mela[,-1]

predictors_quer<- raster::extract(EuropePred, quercPresAbs[,1:2], df=T)
predictors_quer<- predictors_quer[,-1]

predictors_lag<- raster::extract(EuropePred, lagPresAbs[,1:2], df=T)
predictors_lag<- predictors_lag[,-1]

#crs=europepred----
crs(melaPresAbs)<-crs(EuropePred)
crs(minPresAbs)<-crs(EuropePred)
crs(querPresAbs)<-crs(EuropePred)
crs(lagPresAbs)<-crs(EuropePred)
#!!!!!!!DA CONTROLLARE BENE CON ELISA!!!!!!!!
#Collineary test---- 
Vars_to_removemin <- data.frame(BIO=findCorrelation(cor(predictors_min), cutoff = .6, names = T))
intersection1 <- colnames(predictors_min) %in% Vars_to_removemin$BIO

Vars_to_removemela <- data.frame(BIO=findCorrelation(cor(predictors_mela), cutoff = .6, names = T))
intersection2 <- colnames(predictors_mela) %in% Vars_to_removemela$BIO

Vars_to_removequer <- data.frame(BIO=findCorrelation(cor(predictors_quer), cutoff = .6, names = T))
intersection3 <- colnames(predictors_quer) %in% Vars_to_removequer$BIO

Vars_to_removelag <- data.frame(BIO=findCorrelation(cor(predictors_lagopus), cutoff = .6, names = T))
intersection4 <- colnames(predictors_lagopus) %in% Vars_to_removelag$BIO



#Remove correlated variables

predictors_min <- predictors_min[!intersection1]
sdmData_min<- data.frame(pres =minPresAbs[,1], predictors_min[1:ncol(predictors_min)]) #### se pres = melaPresAbs controlla che ci sia solo una colonna pres abs

predictors_mela <- predictors_mela[!intersection2]
sdmData_mela<- data.frame(pres =melaPresAbs[,1], predictors_mela[1:ncol(predictors_mela)]) #### se pres = melaPresAbs controlla che ci sia solo una colonna pres abs

predictors_quer <- predictors_quer[!intersection3]
sdmData_quer<- data.frame(pres =quercPresAbs[,1], predictors_quer[1:ncol(predictors_quer)]) #### se pres = melaPresAbs controlla che ci sia solo una colonna pres abs

predictors_lagopus <- predictors_lagopus[!intersection4]
sdmData_lag<- data.frame(pres =LagPresAbs[,1], predictors_lagopus[1:ncol(predictors_lagopus)]) #### se pres = melaPresAbs controlla che ci sia solo una colonna pres abs




XY<-as.data.frame(EuropePred[[1]], xy=TRUE, na.rm=TRUE)
PTXY=data.frame("x" = XY$x, "y"=XY$y)
predictorsDF<- raster::extract(Predictors,PTXY)
predictorsDF<-data.frame(predictorsDF)
## remove correlated variables
intersection2 <- colnames(predictorsDF) %in% Vars_to_remove$BIO
preds <- predictorsDF[!intersection2]

