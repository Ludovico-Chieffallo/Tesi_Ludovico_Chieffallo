#Uso di colorist----
fav3sp<-stack(FavPred_min,FavPred_mela,FavPred_quer)

#metrics pull----

metrics<-metrics_pull(fav3sp)

#palette----

palette<-palette_set(fav3sp)

#map multiples----
map_multiples(metrics, palette, ncol = 2, lambda_i = -5, labels = names(fav3sp))


#metrics distill----
metricsdist<- metrics_distill(fav3sp)


#Map single
x<-map_single(metricsdist,palette, lambda_i = 5) #how can i overlap geometry?



#download and import worldclim for the future----

#ssp126
prec21_40_126<-brick("wc2.1_2.5m_prec_CNRM-CM6-1_ssp126_2021-2040.tif")
tmin21_40_126<-brick("wc2.1_2.5m_tmin_CNRM-CM6-1_ssp126_2021-2040.tif")
tmax21_40_126<-brick("wc2.1_2.5m_tmax_CNRM-CM6-1_ssp126_2021-2040.tif")
bioc21_40_126<-brick("wc2.1_2.5m_bioc_CNRM-CM6-1_ssp126_2021-2040.tif")

#ssp585

prec21_40_585<-brick("wc2.1_2.5m_prec_CNRM-CM6-1_ssp585_2021-2040.tif")
tmin21_40_585<-brick("wc2.1_2.5m_tmin_CNRM-CM6-1_ssp585_2021-2040.tif")
tmax21_40_585<-brick("wc2.1_2.5m_tmax_CNRM-CM6-1_ssp585_2021-2040.tif")
bioc21_40_585<-brick("wc2.1_2.5m_bioc_CNRM-CM6-1_ssp585_2021-2040.tif")



#bio1<-getData('CMIP6', var='bio', res=2.5, rcp=85, model='CN', year=50)#how can i choice the rcp(Representative Concentration Pathway)
#impossible to run because CMPI6 doesn't exist in this function. it is not recognized

#da non fare----
modelbio<-stack(bioc21_40_126, bioc21_40_585)
modelbio_Europe <- crop(modelbio, Europe)

library(RStoolbox)
set.seed(999)
modelPCA <- rasterPCA(modelbio_Europe, nComp=8, spca=TRUE)

summary(modelPCA$model)

modelPCA$map


 # i primi 8 componenti rappresentano circa il 100%...guardare "Proportion of Variance" e settare su rasterPC nComp in base a quanti componenti da 1:n rappresentano quasi la titalità della varinza

###### TUTTO QUESTO LAVORO SAREBBE FIGO MA DEV'ESSERE CORRETTO E COMPLETTATO PER PRODOTTI MATRICIALI ( SOTTO COSIGLIO DI FRANCESCO SABATINI) QUINDI EVITEREI PCA E SCEGLIEREI SOLO 1 MODELLO E 1 SSP

#

# facciamo la prediction rispetto al rasterstack climate change----
predictors_future<- raster::extract(bioc21_40_126, minPresAbs[,1:2], df=FALSE) # quindi al posto di rcp$map metterai lo stck che avrai dal file scaricato sceglendo 1 modello e uno dei corrispettivi ssp


sdmData_min_future<-data.frame(cbind(minPresAbs, predictors_future))


#----

FavModel_min_future<-multGLM(sdmData_min_future, sp.cols = 3, var.cols=4:8, family = "binomial",step = FALSE, Y.prediction = TRUE, P.prediction = TRUE, Favourability = TRUE)
fut_pred <-stack(bioc21_40_126)
FuturePred_min<- getPreds(fut_pred,models=FavModel_min_future$models, id.col=NULL, Y=FALSE, P = FALSE, Favourability = TRUE)




#######  QUESTI PASSAGGI VANNO RIPETURI PER LE ALTRE SPECIE E PLOTTATI SU COLORIST ALLO STESSO MODO   #######







#da fare quando abbiamo i dati di tutte le specie-----



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

# se non ho capito male quello che esce da map_distill layer specificity è simile a fav_intersect, forse specificity=0 è simila al valore di fav_intersect max ( i due metodi di calcolo probabilmente sono diveri)

# proviamo ad sovrapporre i due layer rispetto a specifity = 0 e fav_ intersect = max ... tipo plotto con ggplot2 sopra a map_single un poligono che riprende l'area di fav_intersect max (sulla vignetta colorist in basso c'è il tutorial per ggplot2). 
# TROVA UN MODO PER FARLO E RAPPRESENTARLO SULLA MAPPA, nel caso ci guardiamo assieme!!!

