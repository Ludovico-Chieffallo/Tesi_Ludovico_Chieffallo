#Prima di proseguire andiamo su un gestore di testo, e tramite copia/incolla andiamo a riportare il codice che ci interessa inserire nel pdf

#Installiamo il pacchetto knitr

install.packages("knitr")

#Richiamiamo il file

library(knitr)

#Definiamo la nostra cartella di lavoro (qui è stata utilizzata la cartella lad)
setwd("c:/lab/")

#controlliamo che il percorso sia giusto

getwd()

#Usiamo la funzione stitch.
#A cosa serve la funzione stitch? prende il file esterno a R, lo porta dentro e ci restituisce un file in pdf completo
#Il primo attributo da inserire sarà il nome del dile che abbiamo creato (tesi.r)

stitch("tesi.r", template = system.file("misc", "knitr-template.Rnw", package="knitr"))

#In questo modo ci verrà restituito il fil Pdf completo

#A questo punto possiamo passare in latex e compilare il testo del file con estenzione .tex oppure andare su overleaf (online) e copia/incollare il testo .tex e compilare
#Se le immagini non dovessero essere presenti, creare su Overleaf una cartella e rinominarla "figure" e importare le immagini direttamente dal computer (le immagini si genereranno automaticamente dopo la funzione stitch)
