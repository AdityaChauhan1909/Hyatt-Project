AprilDataSet <- read.csv(file.choose(),nrow=600000)
columns <- c(1,10,11,12,17,23,49,65,77,92,112,126,127,137,138,139,140,141, 
             142,143,144,145,147,167,168,169,171,182,185,199,200,201,202,203,204,205,206,207,208,209,210,212,213,
             214,215,216,217,218,219,220,221,223,227,231,232)
projectdataApril <- AprilDataSet[,columns]
projectdataApril
####Checking which country where guest stays the most
library(ggplot2)
locationDataApril <- ggplot(projectdataApril,aes(x=Country_PL)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
locationDataApril
### Most of the guests stays at United States

UnitedStatesDataApril <- projectdataApril[which(projectdataApril$Country_PL =="United States"),]
UnitedStatesDataApril
##Checking the States in US where people flow in the most
locationDataUSApril <- ggplot(UnitedStatesDataApril,aes(x=State_PL)) + geom_bar() + coord_flip()
locationDataUSApril

###Cleaning the dataset
Cleaned.LocationDataUSApril <- UnitedStatesDataApril
for(i in 1:nrow(Cleaned.LocationDataUSApril)){
  tt <- Cleaned.LocationDataUSApril[i,]
  tt[sapply(tt,is.null),] <- NA
  Cleaned.LocationDataUSApril[i,] <- tt
}
View(Cleaned.LocationDataUSApril )

##Eliminating columns e_status_I, e_hy_gss_room_floor_I, DOE_H, ClubLounge_Used_H, Spa_Used_H

Cleaned.LocationDataUSApril  <- Cleaned.LocationDataUSApril [,-9:-13]
View(Cleaned.LocationDataUSApril )

##Most of the people are going to Texas
#Chunking the Data , limited to Texas State
CaliforniaDataApril <- Cleaned.LocationDataUSApril[which(Cleaned.LocationDataUSApril$State_PL =="California"),]
View(CaliforniaDataApril)

FloridaDataApril <- Cleaned.LocationDataUSApril[which(Cleaned.LocationDataUSApril$State_PL =="Florida"),]
View(FloridaDataApril)

##Creating a Dataset for both the places

CaliforniaFloridaDataApril <- Cleaned.LocationDataUSApril[which(Cleaned.LocationDataUSApril$State_PL =="Florida" | Cleaned.LocationDataUSApril$State_PL =="California"),]
View(CaliforniaFloridaDataApril)

## CHecking NA values in California DataSet
install.package("Amelia")
library(Amelia)
missmap(CaliforniaDataApril,col=c("yellow","black"),legend=TRUE)

##CHecking NA values in Florida DataSet
missmap(FloridaDataApril,col=c("yellow","black"),legend=TRUE)

##Filtering the California data with Non NA values in Likelihood field
CaliforniaDataApril.NonNA <- subset(CaliforniaDataApril,(!is.na(CaliforniaDataApril[,9])))
View(CaliforniaDataApril.NonNA)
##Filtering the Florida data with Non NA values in Likelihood field

FloridaDataApril.NonNA <- subset(FloridaDataApril,(!is.na(FloridaDataApril[,9])))
View(FloridaDataApril.NonNA)

##Filtering the data of both the places 
CaliforniaFloridaDataApril.NonNA <- subset(CaliforniaFloridaDataApril,(!is.na(CaliforniaFloridaDataApril[,9])))
View(CaliforniaFloridaDataApril.NonNA)
write.csv(CaliforniaFloridaDataApril.NonNA," CF April.csv")
#Checking the purpose of the visit and the brand guests are using  ---- California
CaliforniaHotelBrandApril <- ggplot(CaliforniaDataApril.NonNA,aes(x=Brand_PL)) + geom_bar(aes(fill=POV_CODE_C))
CaliforniaHotelBrandApril

#Checking the purpose of the visit and the brand guests are using  ---- California
FloridaaHotelBrandApril <- ggplot(FloridaDataApril.NonNA,aes(x=Brand_PL)) + geom_bar(aes(fill=POV_CODE_C))
FloridaaHotelBrandApril

#Most of the people are coming in for business purpose

