AugustDataSet <- read.csv(file.choose(),nrow=600000)
columns <- c(1,10,11,12,17,23,49,65,77,92,112,126,127,137,138,139,140,141, 
             142,143,144,145,147,167,168,169,171,182,185,199,200,201,202,203,204,205,206,207,208,209,210,212,213,
             214,215,216,217,218,219,220,221,223,227,231,232)
projectdataAugust <- AugustDataSet[,columns]
projectdataAugust
####Checking which country where guest stays the most
library(ggplot2)
locationDataAugust <- ggplot(projectdataAugust,aes(x=Country_PL)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
locationDataAugust
### Most of the guests stays at United States

UnitedStatesDataAugust <- projectdataAugust[which(projectdataAugust$Country_PL =="United States"),]
UnitedStatesDataAugust
##Checking the States in US where people flow in the most
locationDataUSAugust <- ggplot(UnitedStatesDataAugust,aes(x=State_PL)) + geom_bar() + coord_flip()
locationDataUSAugust

###Cleaning the dataset
Cleaned.UnitedStatesDataAugust <- UnitedStatesDataAugust
for(i in 1:nrow(Cleaned.UnitedStatesDataAugust)){
  tt <- Cleaned.UnitedStatesDataAugust[i,]
  tt[sapply(tt,is.null),] <- NA
  Cleaned.UnitedStatesDataAugust[i,] <- tt
}
View(Cleaned.UnitedStatesDataAugust )

##Eliminating columns e_status_I, e_hy_gss_room_floor_I, DOE_H, ClubLounge_Used_H, Spa_Used_H

Cleaned.UnitedStatesDataAugust  <- Cleaned.UnitedStatesDataAugust [,-9:-13]
View(Cleaned.UnitedStatesDataAugust )

##Most of the people are going to Texas
#Chunking the Data , limited to Texas State
CaliforniaDataAugust <- Cleaned.UnitedStatesDataAugust[which(Cleaned.UnitedStatesDataAugust$State_PL =="California"),]
View(CaliforniaDataAugust)

FloridaDataAugust <- Cleaned.UnitedStatesDataAugust[which(Cleaned.UnitedStatesDataAugust$State_PL =="Florida"),]
View(FloridaDataAugust)

##Creating a Dataset for both the places

CaliforniaFloridaDataAugust <- Cleaned.UnitedStatesDataAugust[which(Cleaned.UnitedStatesDataAugust$State_PL =="Florida" | Cleaned.UnitedStatesDataAugust$State_PL =="California"),]
View(CaliforniaFloridaDataAugust)

## CHecking NA values in California DataSet
install.package("Amelia")
library(Amelia)
missmap(CaliforniaDataAugust,col=c("yellow","black"),legend=TRUE)

##CHecking NA values in Florida DataSet
missmap(FloridaDataAugust,col=c("yellow","black"),legend=TRUE)

##Filtering the California data with Non NA values in Likelihood field
CaliforniaDataAugust.NonNA <- subset(CaliforniaDataAugust,(!is.na(CaliforniaDataAugust[,9])))
View(CaliforniaDataAugust.NonNA)
##Filtering the Florida data with Non NA values in Likelihood field

FloridaDataAugust.NonNA <- subset(FloridaDataAugust,(!is.na(FloridaDataAugust[,9])))
View(FloridaDataAugust.NonNA)

##Filtering the data of both the places 
CaliforniaFloridaDataAugust.NonNA <- subset(CaliforniaFloridaDataAugust,(!is.na(CaliforniaFloridaDataAugust[,9])))
View(CaliforniaFloridaDataAugust.NonNA)
#Checking the purpose of the visit and the brand guests are using  ---- California
CaliforniaHotelBrandAugust <- ggplot(CaliforniaDataAugust.NonNA,aes(x=Brand_PL)) + geom_bar(aes(fill=POV_CODE_C))
CaliforniaHotelBrandAugust

#Checking the purpose of the visit and the brand guests are using  ---- California
FloridaaHotelBrandAugust <- ggplot(FloridaDataAugust.NonNA,aes(x=Brand_PL)) + geom_bar(aes(fill=POV_CODE_C))
FloridaaHotelBrandAugust

#Most of the people are coming in for business purpose

