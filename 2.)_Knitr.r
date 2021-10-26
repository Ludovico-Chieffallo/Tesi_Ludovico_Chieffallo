#Before continuing, let's go to a text manager, and copying / pasting will bring back the code we are interested in inserting in the pdf

#install "knitr"
install.packages("knitr")

#use library for work with knitr
library(knitr)

#use setwd for select correctly the folder
setwd("c:/lab/")

#check folder 
getwd()

#We use the stitch function.
#What is the stitch function? it takes the external file to R, brings it in and gives us a complete pdf file
#The first attribute to insert will be the name of the file we created (thesis.r)
stitch("tesi.r", template = system.file("misc", "knitr-template.Rnw", package="knitr"))


#Now we can see our pdf 

# At this point we can switch to latex and compile the text of the file with .tex extension or go to overleaf (online) and copy / paste the .tex text and compile

#If the images are not present, create a folder on Overleaf and rename it "figure" and import the images directly from the computer (the images will be generated automatically after the stitch function)
