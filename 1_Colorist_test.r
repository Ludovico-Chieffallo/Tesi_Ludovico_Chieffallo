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

#ESEMPIO 1: MAPPARE UNA DISTRIBUZIONE DI SPECIE NEL CICLO ANNUALE

#Qui, utilizziamo dati aggregati sullo stato e sulle tendenze di eBird per Field Sparrow ( Spizella pusilla ) per illustrare una strategia diversa per la creazione di mappe del ciclo annuale,
#che sfrutta i dati di occorrenza continua (piuttosto che i dati categoriali di presenza-assenza) per descrivere dove e quando gli spettatori potrebbero essere in grado di trovare una specie.

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



#2.)Creiamo una palette (HCL). Vogliamo scegliere una tavolozza che aiuti a comunicare informazioni temporali sull'occorrenza di Field Sparrow.
#Utilizzeremo la funzione palette_timecycle() perchè i nostri dati rapprenentano una sequyuenza ordinata e ciclica (tutti i mesi dell'anno)

pal<- palette_timecycle(fiespa_occ)

head(pal)      #usiamo head() per restituirci i primi valori. Pal ha 1212 oggetti, con head ad esempio prendiamo i primi 6

#  specificity layer_id   color
#1           0        1 #6A6A6A
#2           0        2 #6A6A6A
#3           0        3 #6A6A6A
#4           0        4 #6A6A6A
#5           0        5 #6A6A6A
#6           0        6 #6A6A6A    

#Abbiamo visto come la funzione palette_timecycle ci restituisca un frame di dati con tre campi: specificy layer_id e color
#I campi specificity e layer_id verranno utilizzati per assegnare i colori a celle raster specifiche



#3.)Creiamo una mappa. 
#Con la funzione map_multiples() avremo modo di vedere la mappa per ogni layer, quindi per ogni mese dell'anno
#All'interno della parentesi scriveremo 1.)La metrica/ 2.)La palette utilizzata/ 3.) ncol=x per definire il numero di colonne da visualizzare/ 4.)Labels=names(nome file)

map_multiples(met1, pal, ncol = 3, labels = names (fiespa_occ))

#Se vogliamo estrarre un mese di dati per un'analisi più approfondita, possiamo utilizzare map_single()e specificare quale mese di dati vorremmo vedere utilizzando l'argomento layer.
map_single(met1, pal, layer = 6)

#Per generare una singola mappa del ciclo annuale che sintetizzi le informazioni spazio-temporali sull'occorrenza di Field Sparrow,
#abbiamo bisogno di "distillare" le informazioni distributive nel nostro RasterStackutilizzo metrics_distill()

#La metrics_distill()funzione calcola le metriche di distribuzione su tutti i livelli in ogni cella raster e restituisce tre metriche per la visualizzazione successiva:

#-Intensità massima (ovvero, il valore massimo di occorrenza, abbondanza o densità).
#-Strato di massima intensità (ovvero, l'identità dello strato contenente il valore di intensità massima)
#-Specificità del valore di intensità massima per lo strato di intensità massima (cioè, il grado in cui i valori di intensità sono distribuiti in modo non uniforme tra gli strati).

met1_distill<-metrics_distill(fiespa_occ)  #"Distilliamo" quindi le informazioni
map_single(met1_distill,pal)               #Visualizziamo quindi le informazione nella singola immagine con le immagini "distillate" e  la palette creata in precedenza


#4.)Infine creiamo la legenda.
#Avendo utilizzato palette_timecycle() utilizzeremo la legenda da configurare con la funzione legend_timecycle().
#Ci verrà restituita una legenda a cerchi dove Le informazioni su quando inizia (e finisce) il ciclo di tempo possono essere fornite anche nell'argomento origin_label.

legend_timecycle(pal, origin_label = "jan 1")

#CONCLUSIONI
#Con tutte le informazioni descritte, con la mappa e con la legenda vicino, siamo in grado di capire dove e quando trovare questa specie.
#Le parti più colorate indicheranno alta specificità ma alta probabilità di occorrenza solo in alcuni periodi dell'anno
#Le parti grigie invece indicheranno bassa specificità ma alta occorrenza per tutto l'anno
#In questo caso la specificità bassa indica la stagionalità dove 0 indica presenza tutto l'anno



#ESEMPIO 2: MAPPARE IL COMPORTAMENTO INDIVIDUALE NEL TEMPO


#Qui esploriamo come un individuo Fisher ( Pekania pennanti ) che vive nello stato di New York si è spostato nel suo ambiente locale per un periodo di nove notti sequenziali nel 2011. 

#Carichiamo i dati 
data("fisher_ud")   
fisher_ud
#class      : RasterStack 
#dimensions : 176, 177, 31152, 9  (nrow, ncol, ncell, nlayers)
#resolution : 25, 25  (x, y)
#extent     : -2282.343, 2142.657, 5100266, 5104666  (xmin, xmax, ymin, ymax)
#crs        : +proj=moll +lon_0=-73.4137066015374 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs 
#names      :      night1,      night2,      night3,      night4,      night5,      night6,      night7,      night8,      night9 
#min values :           0,           0,           0,           0,           0,           0,           0,           0,           0 
#max values : 0.004695207, 0.007591029, 0.006749434, 0.002891691, 0.002833876, 0.004711692, 0.002017082, 0.002700729, 0.005282948 


#Estraiamo le informazione dal Rasterstack
m2<-metrics_pull(fisher_ud)
m2

#Creiamo la palette ma questa volta usiamo la funzione palette_timeline perchè a differenza dell'esempio precedente, descrivono una sequenza lineare e non ciclica (ad esempio l'annuale)
pal2<-palette_timeline(fisher_ud)
head(pal2)

#Creiamo le mappe (9, una per ogni giorno) che ci descriveranno la posizione dell'individuo in ogni giorno
map_multiples(m2,ncol = 3, pal2)
#Capita spesso che le mappe risultino sbiadite.
#Il parametro lambda_i in map_multiples()e map_single()consente agli utenti di regolare visivamente i pesi relativi dei valori di alta e bassa intensità.
#In questo caso quindi vogliamo ridurre la disparità tra valori di alta e bassa intensità quindi impostiamo il valore a -5.
#vedremo che più basso sarà il valore di lambda_i e più sarà intenso il colore
map_multiples(m2,ncol = 3, pal2, lambda_i = -5)

#A questo punto possiamo distillare le informazioni 
#È importante ricordare che i valori di specificità (e la conseguente visualizzazione dei valori di specificità) per Fisher devono essere interpretati in modo diverso rispetto a Field Sparrow. 
#Qui, i valori di specificità indicano il grado in cui l'individuo ha utilizzato le stesse posizioni all'interno del paesaggio per nove notti. Una bassa specificità suggerisce un uso coerente nel tempo. 

m2_distill<-metrics_distill(fisher_ud)
map_single(m2_distill,pal2,lambda_i = -5)

#Ovviamento avendo utilizzato valori lineari, utilizzeremo legend_timeline e non legend_timecycle
#in questo caso possiamo utilizzare time_labels per determinare il periodo di indagine.
legend_timeline(pal2,time_labels = c("2 aprile", "11 aprile"))





#ESEMPIO 3: MAPPARE LE DISTRIBUZIONI DI PIU' INDIVIDUI DURANTE LO STESSO PERIODO DI TEMPO

#Negli esempi precedenti, abbiamo esplorato le distribuzioni di una singola specie e di un singolo individuo in più periodi di tempo. 
#colorist può essere utilizzato anche per esplorare la distribuzione di più specie o individui in un unico periodo di tempo.

#Qui, utilizziamo i dati di tracciamento GPS raccolti dagli elefanti africani ( Loxodonta africana ) nel Parco nazionale di Etosha (Namibia) durante il 2011 per esplorare come due individui hanno utilizzato il paesaggio nel corso di un anno.

#Carichiamo i dati
data("elephant_ud")
elephant_ud
#class      : RasterBrick 
#dimensions : 208, 193, 40144, 2  (nrow, ncol, ncell, nlayers)
#resolution : 500, 500  (x, y)
#extent     : -58642.18, 37857.82, -2376938, -2272938  (xmin, xmax, ymin, ymax)
#crs        : +proj=moll +lon_0=15.8231920275931 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs 
#source     : memory
#names      :      LA11,      LA14 
#min values :         0,         0 
#max values : 0.8525854, 1.0000000 


#Estraiamo i dati dal Rasterstack
met3<-metrics_pull((elephant_ud))

#Qui incontriamo un'altra funzione, palette_set()
#Utilizziamo questa funzione quando abbiamo un insieme non ordinato (non rientrano in sequenze lineari o cicliche)
pal3<-palette_set(elephant_ud)

#Creiamo le mappe multiple e quindi vedremo i due esemplari, usiamo labels per estrarre i nomi prsenti nel Rasterstack
map_multiples(met3, pal3, ncol = 2,lambda_i = -5,labels = names(elephant_ud))

#Le distribuzioni di utilizzo per LA11 e LA14 sembrano notevolmente simili a prima vista, il che non sorprende dato che i due elefanti appartengono alla stessa mandria. 
#Tuttavia, possiamo comprendere più chiaramente le somiglianze e le differenze nel modo in cui hanno utilizzato lo spazio nel 2011 “distillando” le informazioni distribuzionali dai due livelli raster con metrics_distill()e visualizzando le metriche risultanti con map_single().

#DOVREMO RICALIBRARE LA NOSTRA COMPRENSIONE DEL SIGNIFICATO DI SPECIFICITA'
#Ci troviamo in contrasto con gli esempi Field Sparrow e Fisher, dove la specificità indicava il grado in cui i valori di intensità erano incoerenti (o coerenti) nel tempo.
#QUI POSSIAMO DEFINIRLO COME MISURA DIFFERENZIALE DEL PAESAGGIO.
#QUINDI BASSA SPECIFICITA' EQUIVALE AD UTILIZZO COMUNE DEL PAESAGGIO, ALTA SPECIFICITA' INDICA UN USO DIFFERENZIALE DEL PAESAGGIO


met3_distt<-metrics_distill(elephant_ud)
map_single(met3_distt,pal2,lambda_i = -5)

#Qui utilizzeremo una legenda diversa ancora una volta.
#utilizzeremo legend_set e non legend_timeline o legend_timecycle coerentemente con il codice precedente
legend_set(pal3, group_labels = names(elephant_ud))

