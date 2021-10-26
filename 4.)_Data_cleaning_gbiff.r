#Here we can clean the data. The quality of data is very important for a correct statistical analysis!

#Step 1: We can install the packages for work. We can install only "Tidyverse", however i had some errors. For solve this problem i just installed also "dplyr".
install.packages("tidyverse")
install.packages("dplyr")

#Step 2: We use library to say to R that we will use this packages.
library(tidyverse)
library(dplyr)

#Step 3: Remember always to set your directory.
setwd("c:/lab/")

#Step 4: Here we need to read our data and we can use read.csv2 function.
#I used "Per" because my file called "pernice".
Per<-read.csv2("pernice.csv")


#Step 5: Now we can start to clean our dataset.
#We use %>% (pipe function) for link more function. We can imagine %>%="THEN".
#Here we are saying to R we want create per1 THEN rename in the dataset "ï..gbifID" to "gbifID".
per1<- Per%>%
  rename(gbifID=ï..gbifID) 

#Here we will use a lot of "%>%" because we want multiple filters.
Dati_pern<- per1 %>%
  dplyr::select(species,genus, family,order,class,decimalLongitude, decimalLatitude, countryCode, individualCount,gbifID,phylum,taxonRank, coordinateUncertaintyInMeters, year,basisOfRecord, institutionCode)  %>%  #Be careful, the class names of your dataset may be different!                         
  filter (phylum == "Chordata") %>% 
  filter(!is.na(decimalLongitude))%>%
  filter(!is.na(decimalLatitude)) %>%
  filter(!is.na(individualCount))%>%
  filter(basisOfRecord!='UNKNOWN'& basisOfRecord!='LIVING_SPECIMEN') %>%
  filter(taxonRank!='CLASS' & taxonRank!='ORDER')
