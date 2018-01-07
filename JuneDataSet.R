JuneDataSet <- read.csv(file.choose(),nrow=600000)
columns <- c(1,10,11,12,17,23,49,65,77,92,112,126,127,137,138,139,140,141, 
             142,143,144,145,147,167,168,169,171,182,185,199,200,201,202,203,204,205,206,207,208,209,210,212,213,
             214,215,216,217,218,219,220,221,223,227,231,232)
projectdataJune <- JuneDataSet[,columns]
projectdataJune
####Checking which country where guest stays the most
library(ggplot2)
locationDataJune <- ggplot(projectdataJune,aes(x=Country_PL)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
locationDataJune
### Most of the guests stays at United States

UnitedStatesDataJune <- projectdataJune[which(projectdataJune$Country_PL =="United States"),]
UnitedStatesDataJune
##Checking the States in US where people flow in the most
locationDataUSJune <- ggplot(UnitedStatesDataJune,aes(x=State_PL)) + geom_bar() + coord_flip()
locationDataUSJune

###Cleaning the dataset
Cleaned.UnitedStatesDataJune <- UnitedStatesDataJune
for(i in 1:nrow(Cleaned.UnitedStatesDataJune)){
  tt <- Cleaned.UnitedStatesDataJune[i,]
  tt[sapply(tt,is.null),] <- NA
  Cleaned.UnitedStatesDataJune[i,] <- tt
}
View(Cleaned.UnitedStatesDataJune )

##Eliminating columns e_status_I, e_hy_gss_room_floor_I, DOE_H, ClubLounge_Used_H, Spa_Used_H

Cleaned.UnitedStatesDataJune  <- Cleaned.UnitedStatesDataJune [,-9:-13]
View(Cleaned.UnitedStatesDataJune )

##Most of the people are going to Texas
#Chunking the Data , limited to Texas State
CaliforniaDataJune <- Cleaned.UnitedStatesDataJune[which(Cleaned.UnitedStatesDataJune$State_PL =="California"),]
View(CaliforniaDataJune)

FloridaDataJune <- Cleaned.UnitedStatesDataJune[which(Cleaned.UnitedStatesDataJune$State_PL =="Florida"),]
View(FloridaDataJune)

##Creating a Dataset for both the places

CaliforniaFloridaDataJune <- Cleaned.UnitedStatesDataJune[which(Cleaned.UnitedStatesDataJune$State_PL =="Florida" | Cleaned.UnitedStatesDataJune$State_PL =="California"),]
View(CaliforniaFloridaDataJune)

## CHecking NA values in California DataSet
install.package("Amelia")
library(Amelia)
missmap(CaliforniaDataJune,col=c("yellow","black"),legend=TRUE)

##CHecking NA values in Florida DataSet
missmap(FloridaDataJune,col=c("yellow","black"),legend=TRUE)

##Filtering the California data with Non NA values in Likelihood field
CaliforniaDataJune.NonNA <- subset(CaliforniaDataJune,(!is.na(CaliforniaDataJune[,9])))
View(CaliforniaDataJune.NonNA)
##Filtering the Florida data with Non NA values in Likelihood field

FloridaDataJune.NonNA <- subset(FloridaDataJune,(!is.na(FloridaDataJune[,9])))
View(FloridaDataJune.NonNA)

##Filtering the data of both the places 
CaliforniaFloridaDataJune.NonNA <- subset(CaliforniaFloridaDataJune,(!is.na(CaliforniaFloridaDataJune[,9])))
View(CaliforniaFloridaDataJune.NonNA)
#Checking the purpose of the visit and the brand guests are using  ---- California
CaliforniaHotelBrandJune <- ggplot(CaliforniaDataJune.NonNA,aes(x=Brand_PL)) + geom_bar(aes(fill=POV_CODE_C))
CaliforniaHotelBrandJune

#Checking the purpose of the visit and the brand guests are using  ---- California
FloridaaHotelBrandJune <- ggplot(FloridaDataJune.NonNA,aes(x=Brand_PL)) + geom_bar(aes(fill=POV_CODE_C))
FloridaaHotelBrandJune

#Most of the people are coming in for business purpose

