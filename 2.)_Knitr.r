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
