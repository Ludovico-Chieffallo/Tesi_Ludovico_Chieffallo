#mettiamo all'interno della nostra cartlla tutto il necessario (sia i file che il codice)

install.packages("tinytex")
library(tinytex)
library(RStoolbox)
library(knitr)
setwd("c:/lab/")
getwd()
stitch("tesi.r", template = system.file("misc", "knitr-template.Rnw", package="knitr"))
