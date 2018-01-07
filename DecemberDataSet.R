DecemberDataSet <- read.csv(file.choose(),nrow=600000)
columns <- c(1,10,11,12,17,23,49,65,77,92,112,126,127,137,138,139,140,141, 
             142,143,144,145,147,167,168,169,171,182,185,199,200,201,202,203,204,205,206,207,208,209,210,212,213,
             214,215,216,217,218,219,220,221,223,227,231,232)
projectdataDecember <- DecemberDataSet[,columns]
projectdataDecember
####Checking which country where guest stays the most
library(ggplot2)
locationDataDecember <- ggplot(projectdataDecember,aes(x=Country_PL)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
locationDataDecember
### Most of the guests stays at United States

UnitedStatesDataDecember <- projectdataDecember[which(projectdataDecember$Country_PL =="United States"),]
UnitedStatesDataDecember
##Checking the States in US where people flow in the most
locationDataUSDecember <- ggplot(UnitedStatesDataDecember,aes(x=State_PL)) + geom_bar() + coord_flip()
locationDataUSDecember

###Cleaning the dataset
Cleaned.UnitedStatesDataDecember <- UnitedStatesDataDecember
for(i in 1:nrow(Cleaned.UnitedStatesDataDecember)){
  tt <- Cleaned.UnitedStatesDataDecember[i,]
  tt[sapply(tt,is.null),] <- NA
  Cleaned.UnitedStatesDataDecember[i,] <- tt
}
View(Cleaned.UnitedStatesDataDecember )

##Eliminating columns e_status_I, e_hy_gss_room_floor_I, DOE_H, ClubLounge_Used_H, Spa_Used_H

Cleaned.UnitedStatesDataDecember  <- Cleaned.UnitedStatesDataDecember [,-9:-13]
View(Cleaned.UnitedStatesDataDecember )

##Most of the people are going to Texas
#Chunking the Data , limited to Texas State
CaliforniaDataDecember <- Cleaned.UnitedStatesDataDecember[which(Cleaned.UnitedStatesDataDecember$State_PL =="California"),]
View(CaliforniaDataDecember)

FloridaDataDecember<- Cleaned.UnitedStatesDataDecember[which(Cleaned.UnitedStatesDataDecember$State_PL =="Florida"),]
View(FloridaDataDecember)

##Creating a Dataset for both the places

CaliforniaFloridaDataDecember <- Cleaned.UnitedStatesDataDecember[which(Cleaned.UnitedStatesDataDecember$State_PL =="Florida" | Cleaned.UnitedStatesDataDecember$State_PL =="California"),]
View(CaliforniaFloridaDataDecember)

## CHecking NA values in California DataSet
install.package("Amelia")
library(Amelia)
missmap(CaliforniaDataDecember,col=c("yellow","black"),legend=TRUE)

##CHecking NA values in Florida DataSet
missmap(FloridaDataDecember,col=c("yellow","black"),legend=TRUE)

##Filtering the California data with Non NA values in Likelihood field
CaliforniaDataDecember.NonNA <- subset(CaliforniaDataDecember,(!is.na(CaliforniaDataDecember[,9])))
View(CaliforniaDataDecember.NonNA)
##Filtering the Florida data with Non NA values in Likelihood field

FloridaDataDecember.NonNA <- subset(FloridaDataDecember,(!is.na(FloridaDataDecember[,9])))
View(FloridaDataDecember.NonNA)

##Filtering the data of both the places 
CaliforniaFloridaDataDecember.NonNA <- subset(CaliforniaFloridaDataDecember,(!is.na(CaliforniaFloridaDataDecember[,9])))
View(CaliforniaFloridaDataDecember.NonNA)
#Checking the purpose of the visit and the brand guests are using  ---- California
CaliforniaHotelBrandDecember <- ggplot(CaliforniaDataDecember.NonNA,aes(x=Brand_PL)) + geom_bar(aes(fill=POV_CODE_C))
CaliforniaHotelBrandDecember

#Checking the purpose of the visit and the brand guests are using  ---- California
FloridaaHotelBrandDecember <- ggplot(FloridaDataDecember.NonNA,aes(x=Brand_PL)) + geom_bar(aes(fill=POV_CODE_C))
FloridaaHotelBrandDecember

#Most of the people are coming in for business purpose

