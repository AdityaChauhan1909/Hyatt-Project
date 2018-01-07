projectdataFeb <- read.csv(file.choose(),nrow=600000)
columns <- c(1,10,11,12,17,23,49,65,77,92,112,126,127,137,138,139,140,141, 
             142,143,144,145,147,167,168,169,171,182,185,199,200,201,202,203,204,205,206,207,208,209,210,212,213,
             214,215,216,217,218,219,220,221,223,227,231,232)
projectdataFeb2 <- projectdataFeb[,columns]
View(projectdataFeb2)
####Checking which country where guest stays the most
library(ggplot2)
locationData <- ggplot(projectdataFeb2,aes(x=Country_PL)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
locationData
### Most of the guests stays at United States

UnitedStatesData <- projectdataFeb2[which(projectdataFeb2$Country_PL =="United States"),]
View(UnitedStatesData)
##Checking the States in US where people flow in the most
locationDataUS <- ggplot(UnitedStatesData,aes(x=State_PL)) + geom_bar() + coord_flip()
locationDataUS

###Cleaning the dataset
Cleaned.LocationDataUS <- UnitedStatesData
for(i in 1:nrow(Cleaned.LocationDataUS)){
  tt <- Cleaned.LocationDataUS[i,]
  tt[sapply(tt,is.null),] <- NA
  Cleaned.LocationDataUS[i,] <- tt
}
View(Cleaned.LocationDataUS )

##Eliminating columns e_status_I, e_hy_gss_room_floor_I, DOE_H, ClubLounge_Used_H, Spa_Used_H

Cleaned.LocationDataUS  <- Cleaned.LocationDataUS [,-9:-13]
View(Cleaned.LocationDataUS )

##Most of the people are going to Texas
#Chunking the Data , limited to Texas State
CaliforniaData <- Cleaned.LocationDataUS[which(Cleaned.LocationDataUS$State_PL =="California"),]
View(CaliforniaData)

FloridaData <- Cleaned.LocationDataUS[which(Cleaned.LocationDataUS$State_PL =="Florida"),]
View(FloridaData)

##Creating a Dataset for both the places

CaliforniaFloridaData <- Cleaned.LocationDataUS[which(Cleaned.LocationDataUS$State_PL =="Florida" | Cleaned.LocationDataUS$State_PL =="California"),]
View(CaliforniaFloridaData)

## CHecking NA values in California DataSet
install.package("Amelia")
library(Amelia)
missmap(CaliforniaData,col=c("yellow","black"),legend=TRUE)

##CHecking NA values in Florida DataSet
missmap(FloridaData.NonNA,col=c("yellow","black"),legend=TRUE)

##Filtering the California data with Non NA values in Likelihood field
CaliforniaData.NonNA <- subset(CaliforniaData,(!is.na(CaliforniaData[,9])))
View(CaliforniaData.NonNA)
##Filtering the Florida data with Non NA values in Likelihood field
                         
FloridaData.NonNA <- subset(FloridaData,(!is.na(FloridaData[,9])))
View(FloridaData.NonNA)

##Filtering the data of both the places 
CaliforniaFloridaData.NonNA <- subset(CaliforniaFloridaData,(!is.na(CaliforniaFloridaData[,9])))
View(CaliforniaFloridaData.NonNA)
write.csv(CaliforniaFloridaData.NonNA,"C F February.csv")
#Checking the purpose of the visit and the brand guests are using  ---- California
CaliforniaHotelBrand <- ggplot(CaliforniaData.NonNA,aes(x=Brand_PL)) + geom_bar(aes(fill=POV_CODE_C))
CaliforniaHotelBrand

#Checking the purpose of the visit and the brand guests are using  ---- California
FloridaaHotelBrand <- ggplot(FloridaData.NonNA,aes(x=Brand_PL)) + geom_bar(aes(fill=POV_CODE_C))
FloridaaHotelBrand

#Most of the people are coming in for business purpose

