OctoberDataSet <- read.csv(file.choose(),nrow=600000)
columns <- c(1,10,11,12,17,23,49,65,77,92,112,126,127,137,138,139,140,141, 
             142,143,144,145,147,167,168,169,171,182,185,199,200,201,202,203,204,205,206,207,208,209,210,212,213,
             214,215,216,217,218,219,220,221,223,227,231,232)
projectdataOctober <- OctoberDataSet[,columns]
projectdataOctober
####Checking which country where guest stays the most
library(ggplot2)
locationDataOctober <- ggplot(projectdataAugust,aes(x=Country_PL)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
locationDataOctober
### Most of the guests stays at United States

UnitedStatesDataOctober <- projectdataAugust[which(projectdataAugust$Country_PL =="United States"),]
UnitedStatesDataOctober
##Checking the States in US where people flow in the most
locationDataUSOctober <- ggplot(UnitedStatesDataOctober,aes(x=State_PL)) + geom_bar() + coord_flip()
locationDataUSOctober

###Cleaning the dataset
Cleaned.UnitedStatesDataOctober <- UnitedStatesDataOctober
for(i in 1:nrow(Cleaned.UnitedStatesDataOctober)){
  tt <- Cleaned.UnitedStatesDataOctober[i,]
  tt[sapply(tt,is.null),] <- NA
  Cleaned.UnitedStatesDataOctober[i,] <- tt
}
View(Cleaned.UnitedStatesDataOctober )

##Eliminating columns e_status_I, e_hy_gss_room_floor_I, DOE_H, ClubLounge_Used_H, Spa_Used_H

Cleaned.UnitedStatesDataOctober  <- Cleaned.UnitedStatesDataOctober [,-9:-13]
View(Cleaned.UnitedStatesDataOctober )

##Most of the people are going to Texas
#Chunking the Data , limited to Texas State
CaliforniaDataOctober <- Cleaned.UnitedStatesDataOctober[which(Cleaned.UnitedStatesDataOctober$State_PL =="California"),]
View(CaliforniaDataOctober)

FloridaDataOctober<- Cleaned.UnitedStatesDataOctober[which(Cleaned.UnitedStatesDataOctober$State_PL =="Florida"),]
View(FloridaDataOctober)

##Creating a Dataset for both the places

CaliforniaFloridaDataOctober <- Cleaned.UnitedStatesDataOctober[which(Cleaned.UnitedStatesDataOctober$State_PL =="Florida" | Cleaned.UnitedStatesDataOctober$State_PL =="California"),]
View(CaliforniaFloridaDataOctober)

## CHecking NA values in California DataSet
install.package("Amelia")
library(Amelia)
missmap(CaliforniaDataOctober,col=c("yellow","black"),legend=TRUE)

##CHecking NA values in Florida DataSet
missmap(FloridaDataOctober,col=c("yellow","black"),legend=TRUE)

##Filtering the California data with Non NA values in Likelihood field
CaliforniaDataOctober.NonNA <- subset(CaliforniaDataOctober,(!is.na(CaliforniaDataOctober[,9])))
View(CaliforniaDataOctober.NonNA)
##Filtering the Florida data with Non NA values in Likelihood field

FloridaDataOctober.NonNA <- subset(FloridaDataOctober,(!is.na(FloridaDataOctober[,9])))
View(FloridaDataOctober.NonNA)

##Filtering the data of both the places 
CaliforniaFloridaDataOctober.NonNA <- subset(CaliforniaFloridaDataOctober,(!is.na(CaliforniaFloridaDataOctober[,9])))
View(CaliforniaFloridaDataOctober.NonNA)
#Checking the purpose of the visit and the brand guests are using  ---- California
CaliforniaHotelBrandOctober <- ggplot(CaliforniaDataOctober.NonNA,aes(x=Brand_PL)) + geom_bar(aes(fill=POV_CODE_C))
CaliforniaHotelBrandOctober

#Checking the purpose of the visit and the brand guests are using  ---- California
FloridaaHotelBrandOctober <- ggplot(FloridaDataOctober.NonNA,aes(x=Brand_PL)) + geom_bar(aes(fill=POV_CODE_C))
FloridaaHotelBrandOctober

#Most of the people are coming in for business purpose

