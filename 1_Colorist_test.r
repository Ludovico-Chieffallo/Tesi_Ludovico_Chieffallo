#Il pacchetto "colorist" (R) è stato creato per fornire ai ricercatori metodologie e opzioni in più per studiare e comunicare
#informazioni sulla distribuzione della fauna selvatica nello spazio e nel tempo. 
#Per fare questo "colorist" utilizza immagini Rasterstack che descrivono le distribuzioni della fauna selvatica, le elabora
#e le collega alle tavolozze HCL in modi specifici. le visualizzazioni risultanti consentono agli spettatori di confrontare
#in modo significativo i valori di occorrenza, abbondanza, o densità nello spazio e nel tempo

#Cos'è la tavolozza HCL? Lo spazio colore Hue-Chroma-Luminance (HCL) è un'alternativa ad altri spazi colore come RGB, HSV e così via. 
# Ogni colore all'interno dello spazio colore HCL è definito da una tripletta di valori. Le dimensioni sono:

#H ue: definisce il colore (tonalità)
#C hroma: definisce il colore (saturazione o intensità del colore)
#L uminance: definisce la luminosità
#(https://hclwizard.org/images//hclscheme_pic0.png)

#Il flusso di lavoro di base per colorist è il seguente:

#1.)Metriche : gli utenti calcolano le metriche per descrivere le loro distribuzioni.
#2.)Tavolozza dei colori : gli utenti scelgono una tavolozza dei colori per abilitare la visualizzazione delle metriche.
#3.)Mappa : gli utenti combinano metriche e una tavolozza per mappare le distribuzioni in una serie di piccoli multipli o in una singola mappa.
#4.)Legenda : gli utenti generano una legenda per accompagnare la loro mappa.

#Prima di iniziare definiamo la differenza tra RasterBrick (usato in precedenza) e Rasterstack.
#La differenza principale tra RasterBrick e Rasterstack è che a RasterBrick può essere collegato solo a un singolo file (multistrato, come immagini satellitari).
#Al contrario, un RasterStack può essere formato da file separati e / o da pochi livelli ("bande") da un singolo file.

install.packages("colorist")
library(colorist)


#Per iniziare con un esempio mappiamo una distribuzione di specie in un ciclo annuale
#Carichiamo l'esempio usando la funzione data

data("fiespa_occ")
fiespa_occ
#class      : RasterStack 
#dimensions : 193, 225, 43425, 12  (nrow, ncol, ncell, nlayers)
#resolution : 14814.03, 14814.04  (x, y)
#extent     : -1482551, 1850606, -1453281, 1405830  (xmin, xmax, ymin, ymax)
#crs        : +proj=laea +lat_0=38.7476367322638 +lon_0=-90.2379515912106 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 
#names      :       jan,       feb,       mar,       apr,       may,       jun,       jul,       aug,       sep,       oct,       nov,       dec 
#min values :         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0 
#max values : 0.8538026, 0.8272926, 0.7993844, 0.7805922, 0.7799550, 0.7745436, 0.7626938, 0.7867995, 0.7790458, 0.7896419, 0.8158410, 0.8681034



#1)Calcoliamo le metriche 
met1<-metrics_pull(fiespa_occ)  #Questa funzione trasforma i valori dello stack raster che descrivono le distribuzioni individuali o le distribuzioni delle specie in valori di intensità standardizzati.

print(met1)
#class      : RasterBrick 
#dimensions : 193, 225, 43425, 12  (nrow, ncol, ncell, nlayers)
#resolution : 14814.03, 14814.04  (x, y)
#extent     : -1482551, 1850606, -1453281, 1405830  (xmin, xmax, ymin, ymax)
#crs        : +proj=laea +lat_0=38.7476367322638 +lon_0=-90.2379515912106 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 
#source     : memory
#names      :       jan,       feb,       mar,       apr,       may,       jun,       jul,       aug,       sep,       oct,       nov,       dec 
#min values :         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0 
#max values : 0.9835264, 0.9529886, 0.9208400, 0.8991927, 0.8984586, 0.8922251, 0.8785749, 0.9063431, 0.8974113, 0.9096173, 0.9397971, 1.0000000









attr(met1,"maximum")

#2.)Creiamo una palette (HCL)

pal<- palette_timecycle(fiespa_occ)

#3.)Ceiamo una mappa
map_multiples(met1, pal, ncol = 3, labels = names (fiespa_occ))

#4.)Infine creiamo la legenda
legend_timecycle(pal, origin_label = "jan 1")


#
met1_distill<-metrics_distill(fiespa_occ)
map_single(met1_distill,pal)
legend_timecycle(pal, origin_label = "jan 1")


#///


data("elephant_ud")
elephant_ud
met2<-metrics_pull((elephant_ud))
pal2<-palette_set(2)
map_multiples(met2, pal2, ncol = 2,lambda_i = -5,labels = names(elephant_ud))
#//
met2_distt<-metrics_distill(elephant_ud)
map_single(met2_distt,pal2,lambda_i = -5)
legend_set(pal2, group_labels = names(elephant_ud))

