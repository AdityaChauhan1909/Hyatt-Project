##############DATA FOR FEBRUARY#############

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

#Checking the purpose of the visit and the brand guests are using  ---- Florida
FloridaaHotelBrand <- ggplot(FloridaData.NonNA,aes(x=Brand_PL)) + geom_bar(aes(fill=POV_CODE_C))
FloridaaHotelBrand

#Most of the people are coming in for business purpose


# DATA FOR APRIL###########
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

#Checking the purpose of the visit and the brand guests are using  ---- Florida
FloridaaHotelBrandApril <- ggplot(FloridaDataApril.NonNA,aes(x=Brand_PL)) + geom_bar(aes(fill=POV_CODE_C))
FloridaaHotelBrandApril

#Most of the people are coming in for business purpose


##################DATA FOR JUNE#################

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

#Checking the purpose of the visit and the brand guests are using  ---- Florida
FloridaaHotelBrandJune <- ggplot(FloridaDataJune.NonNA,aes(x=Brand_PL)) + geom_bar(aes(fill=POV_CODE_C))
FloridaaHotelBrandJune

#Most of the people are coming in for business purpose

####################DATA FOR AUGUST##################

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

#Checking the purpose of the visit and the brand guests are using  ---- Florida
FloridaaHotelBrandAugust <- ggplot(FloridaDataAugust.NonNA,aes(x=Brand_PL)) + geom_bar(aes(fill=POV_CODE_C))
FloridaaHotelBrandAugust

#Most of the people are coming in for business purpose

######################DATA FOR OCTOBER#############

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

#Checking the purpose of the visit and the brand guests are using  ---- Florida
FloridaaHotelBrandOctober <- ggplot(FloridaDataOctober.NonNA,aes(x=Brand_PL)) + geom_bar(aes(fill=POV_CODE_C))
FloridaaHotelBrandOctober

#Most of the people are coming in for business purpose



###################### DATA FOR DECEMBER####################

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

#Checking the purpose of the visit and the brand guests are using  ---- Florida
FloridaaHotelBrandDecember <- ggplot(FloridaDataDecember.NonNA,aes(x=Brand_PL)) + geom_bar(aes(fill=POV_CODE_C))
FloridaaHotelBrandDecember

#Most of the people are coming in for business purpose




#########################DATA CLEANING#########

combineddata.New <- rbind(CaliforniaFloridaData.NonNA, CaliforniaFloridaDataApril.NonNA,CaliforniaFloridaDataJune.NonNA ,CaliforniaFloridaDataAugust.NonNA,CaliforniaFloridaDataOctober.NonNA,CaliforniaFloridaDataDecember.NonNA)
HotelData <- combineddata.New
View(HotelData)


##Calculating Percentage of NA Value

# for Guest_Room_H
CountOfNA_GuestRoom <- sum(is.na(HotelData$Guest_Room_H))
CountOfNA_GuestRoom
NaGuestRoom_H <- (CountOfNA_GuestRoom/nrow(HotelData)) * 100
NaGuestRoom_H

# Percentage of NA Value : 18.36454
# for Tranquility_H
CountOfNA_Tranquility_H<- sum(is.na(HotelData$Tranquility_H))
CountOfNA_Tranquility_H
NaTranquility_H <- (CountOfNA_Tranquility_H/nrow(HotelData)) * 100
NaTranquility_H
# Percentage of NA Value : 54.61132

#Condition_Hotel_H
CountOfNA_Condition_Hotel_H<- sum(is.na(HotelData$Condition_Hotel_H))
CountOfNA_Condition_Hotel_H
NaCondition_Hotel_H <- (CountOfNA_Condition_Hotel_H/nrow(HotelData)) * 100
NaCondition_Hotel_H
#Percentage of NA Value : 18.6046

#Customer_SVC_H
CountOfNA_Customer_SVC_H<- sum(is.na(HotelData$Customer_SVC_H))
CountOfNA_Customer_SVC_H
NaCustomer_SVC_H <- (CountOfNA_Customer_SVC_H/nrow(HotelData)) * 100
NaCustomer_SVC_H
#Percentage of NA Value : 18.9516

#Staff_Cared_H
CountOfNA_Staff_Cared_H<- sum(is.na(HotelData$Staff_Cared_H))
CountOfNA_Staff_Cared_H
NaStaff_Cared_H <- (CountOfNA_Staff_Cared_H/nrow(HotelData)) * 100
NaStaff_Cared_H

##Percentage of NA Value : 54.45637

#Internet_Sat_H

CountOfNA_Internet_Sat_H<- sum(is.na(HotelData$Internet_Sat_H))
CountOfNA_Internet_Sat_H
NaInternet_Sat_H<- (CountOfNA_Internet_Sat_H/nrow(HotelData)) * 100
NaInternet_Sat_H

###Percentage of NA Value : 66.90018

#Check_In_H
CountOfNA_Check_In_H<- sum(is.na(HotelData$Check_In_H))
CountOfNA_Check_In_H
NaCheck_In_H<- (CountOfNA_Check_In_H/nrow(HotelData)) * 100
NaCheck_In_H

##Percentage of NA Value : 54.4782

#F.B_Overall_Experience_H
CountOfNA_F.B_Overall_Experience_H<- sum(is.na(HotelData$F.B_Overall_Experience_H))
CountOfNA_F.B_Overall_Experience_H
NaF.B_Overall_Experience_H<- (CountOfNA_F.B_Overall_Experience_H/nrow(HotelData)) * 100
NaF.B_Overall_Experience_H

##Percentage of NA Value : 63.7183

#Overall_Sat_H
CountOfNA_Overall_Sat_H<- sum(is.na(HotelData$Overall_Sat_H))
CountOfNA_Overall_Sat_H
NaOverall_Sat_H<- (CountOfNA_Overall_Sat_H/nrow(HotelData)) * 100
NaOverall_Sat_H

##Percentage of NA Value : 17.58107


# Removing the columns

#Internet_Sat_H ( Column 17)

HotelData <- HotelData[,-17]
View(HotelData)

#Removing F.B_Overall_Experience_H ( Column 18)
HotelData <- HotelData[,-18]
View(HotelData)

write.csv(HotelData,"Cleaned Dataset 6.2.csv")

#Replacing NA with Mean values of the column

for(i in 1:ncol(HotelData)){
  HotelData[is.na(HotelData[,i]), i] <- mean(HotelData[,i], na.rm = TRUE)
}


# Reading the file again to check NA in categorical data

dat2 <- read.csv("Cleaned Dataset 6.2.csv", header=T, na.strings=c("","NA"))
View(dat2)

#Percentage of NA values in All.Suites_PL

CountOfNA_All.Suites_PL<- sum(is.na(dat2$All.Suites_PL))
CountOfNA_All.Suites_PL
NaAll.Suites_PL<- (CountOfNA_All.Suites_PL/nrow(dat2)) * 100
NaAll.Suites_PL

#Percentage of NA Values : 0.3382655

#Bell.Staff_PL

CountOfNA_Bell.Staff_PL<- sum(is.na(dat2$Bell.Staff_PL))
CountOfNA_Bell.Staff_PL
NaBell.Staff_PL<- (CountOfNA_Bell.Staff_PL/nrow(dat2)) * 100
NaBell.Staff_PL

#Percentage of NA Values : 29.86993

#Boutique_PL

CountOfNA_Boutique_PL<- sum(is.na(dat2$Boutique_PL))
CountOfNA_Boutique_PL
NaBoutique_PL<- (CountOfNA_Boutique_PL/nrow(dat2)) * 100
NaBoutique_PL

#Percentage of NA Values : 0.3382655

#Business.Center_PL
CountOfNA_Business.Center_PL<- sum(is.na(dat2$Business.Center_PL))
CountOfNA_Business.Center_PL
NaBusiness.Center_PL<- (CountOfNA_Boutique_PL/nrow(dat2)) * 100
NaBusiness.Center_PL
#Percentage of NA Values : 0.3382655

#Casino_PL
CountOfNA_Casino_PLL<- sum(is.na(dat2$Casino_PL))
CountOfNA_Casino_PLL
NaCasino_PL<- (CountOfNA_Casino_PLL/nrow(dat2)) * 100
NaCasino_PL
#Percentage of NA Values : 0.3382655

#Conference_PL
CountOfNA_Conference_PL<- sum(is.na(dat2$Conference_PL))
CountOfNA_Conference_PL
NaConference_PL<- (CountOfNA_Conference_PL/nrow(dat2)) * 100
NaConference_PL
#Percentage of NA Values : 0.3382655

#Convention_PL

CountOfNA_Convention_PLL<- sum(is.na(dat2$Convention_PL))
CountOfNA_Convention_PLL
NaConvention_PL<- (CountOfNA_Convention_PLL/nrow(dat2)) * 100
NaConvention_PL

#Percentage of NA Values : 0.3382655

#Dry.Cleaning_PL
CountOfNA_Dry.Cleaning_PL<- sum(is.na(dat2$Dry.Cleaning_PL))
CountOfNA_Dry.Cleaning_PL
NaDry.Cleaning_PL<- (CountOfNA_Dry.Cleaning_PL/nrow(dat2)) * 100
NaDry.Cleaning_PL

#Percentage of NA Values : 29.86993

#Elevators_PL
CountOfNA_Elevators_PL<- sum(is.na(dat2$Elevators_PL))
CountOfNA_Elevators_PL
NaElevators_PL<- (CountOfNA_Elevators_PL/nrow(dat2)) * 100
NaElevators_PL

#Percentage of NA Values : 29.86993

#Fitness.Center_PL

CountOfNA_Fitness.Center_PLL<- sum(is.na(dat2$Fitness.Center_PL))
CountOfNA_Fitness.Center_PLL
NaFitness.Center_PL<- (CountOfNA_Fitness.Center_PLL/nrow(dat2)) * 100
NaFitness.Center_PL

#Percentage of NA Values : 29.86993

#Fitness.Trainer_PL
CountOfNA_Fitness.Trainer_PLL<- sum(is.na(dat2$Fitness.Trainer_PL))
CountOfNA_Fitness.Trainer_PLL
NaFitness.Trainer_PL<- (CountOfNA_Fitness.Trainer_PLL/nrow(dat2)) * 100
NaFitness.Trainer_PL

#Percentage of NA Values : 31.38231

#Golf_PL
CountOfNA_Golf_PL<- sum(is.na(dat2$Golf_PL))
CountOfNA_Golf_PL
NaGolf_PL<- (CountOfNA_Golf_PL/nrow(dat2)) * 100
NaGolf_PL

#Percentage of NA Values : 0.3382655

#Laundry_PL
CountOfNA_Laundry_PL<- sum(is.na(dat2$Laundry_PL))
CountOfNA_Laundry_PL
NaLaundry_PL<- (CountOfNA_Laundry_PL/nrow(dat2)) * 100
NaLaundry_PL
#Percentage of NA Values : 29.86993

#Limo.Service_PL
CountOfNA_Limo.Service_PL<- sum(is.na(dat2$Limo.Service_PL))
CountOfNA_Limo.Service_PL
NaLimo.Service_PL<- (CountOfNA_Limo.Service_PL/nrow(dat2)) * 100
NaLimo.Service_PL

#Percentage of NA Values : 29.86993

#Mini.Bar_PL
CountOfNA_Mini.Bar_PL<- sum(is.na(dat2$Mini.Bar_PL))
CountOfNA_Mini.Bar_PL
NaMini.Bar_PL<- (CountOfNA_Mini.Bar_PL/nrow(dat2)) * 100
NaMini.Bar_PL

#Percentage of NA Values : 29.86993

#Pool.Indoor_PL
CountOfNA_Pool.Indoor_PL<- sum(is.na(dat2$Pool.Indoor_PL))
CountOfNA_Pool.Indoor_PL
NaPool.Indoor_PL<- (CountOfNA_Pool.Indoor_PL/nrow(dat2)) * 100
NaPool.Indoor_PL

##Percentage of NA Values : 29.86993

#Pool.Outdoor_PL
CountOfNA_Pool.Outdoor_PL<- sum(is.na(dat2$Pool.Outdoor_PL))
CountOfNA_Pool.Outdoor_PL
NaPool.Outdoor_PL<- (CountOfNA_Pool.Outdoor_PL/nrow(dat2)) * 100
NaPool.Outdoor_PL

##Percentage of NA Values : 29.86993

#Regency.Grand.Club_PL
CountOfNA_Regency.Grand.Club_PL<- sum(is.na(dat2$Regency.Grand.Club_PL))
CountOfNA_Regency.Grand.Club_PL
NaRegency.Grand.Club_PL<- (CountOfNA_Regency.Grand.Club_PL/nrow(dat2)) * 100
NaRegency.Grand.Club_PL

##Percentage of NA Values : 29.86993

#Resort_PL
CountOfNA_Resort_PL<- sum(is.na(dat2$Resort_PL))
CountOfNA_Resort_PL
NaResort_PL<- (CountOfNA_Resort_PL/nrow(dat2)) * 100
NaResort_PL

#Percentage of NA Values : 0.3382655

#Restaurant_PL
CountOfNA_Restaurant_PL<- sum(is.na(dat2$Restaurant_PL))
CountOfNA_Restaurant_PL
NaRestaurant_PL<- (CountOfNA_Restaurant_PL/nrow(dat2)) * 100
NaRestaurant_PL

#Percentage of NA Values : 0.3382655

#Self.Parking_PL
CountOfNA_Self.Parking_PL<- sum(is.na(dat2$Self.Parking_PL))
CountOfNA_Self.Parking_PL
NaSelf.Parking_PL<- (CountOfNA_Self.Parking_PL/nrow(dat2)) * 100
NaSelf.Parking_PL
##Percentage of NA Values : 29.86993

#Shuttle.Service_PL
CountOfNA_Shuttle.Service_PL<- sum(is.na(dat2$Shuttle.Service_PL))
CountOfNA_Shuttle.Service_PL
NaShuttle.Service_PL<- (CountOfNA_Shuttle.Service_PL/nrow(dat2)) * 100
NaShuttle.Service_PL
##Percentage of NA Values : 29.86993

#Spa_PL
CountOfNA_Spa_PL<- sum(is.na(dat2$Spa_PL))
CountOfNA_Spa_PL
NaSpa_PL<- (CountOfNA_Spa_PL/nrow(dat2)) * 100
NaSpa_PL

#Percentage of NA Values : 0.3382655

#Valet.Parking_PL
CountOfNA_Valet.Parking_PL<- sum(is.na(dat2$Valet.Parking_PL))
CountOfNA_Valet.Parking_PL
NaValet.Parking_PL<- (CountOfNA_Valet.Parking_PL/nrow(dat2)) * 100
NaValet.Parking_PL

##Percentage of NA Values : 29.86993

#For Categorical Values

Columns <- c("All.Suites_PL","Bell.Staff_PL",	"Boutique_PL","Business.Center_PL",	"Casino_PL",	"Conference_PL",	"Convention_PL","Dry.Cleaning_PL","Elevators_PL","Fitness.Center_PL","Fitness.Trainer_PL","Golf_PL","Laundry_PL","Limo.Service_PL",	"Mini.Bar_PL",	"Pool.Indoor_PL",	"Pool.Outdoor_PL",	"Regency.Grand.Club_PL","Resort_PL","Restaurant_PL","Self.Parking_PL", "Shuttle.Service_PL","Spa_PL", "Valet.Parking_PL")

PercentageOfNAValues <- c(0.3382655, 29.8699315,0.3382655,0.3382655,0.3382655,0.3382655,0.3382655,29.8699315,29.8699315,29.8699315,31.3823054,0.3382655,29.8699315,29.8699315,29.8699315,29.8699315,29.8699315,29.8699315,0.3382655,0.3382655,29.8699315,29.8699315,0.3382655,29.8699315)
naCategorical <- data.frame(Columns,PercentageOfNAValues)
View(naCategorical)

barplot(naCategorical$PercentageOfNAValues,ylab="Percentage of Values",las=2, ylim=c(0,50),main="Comparison",names.arg = c("All.Suites_PL","Bell.Staff_PL",	"Boutique_PL","Business.Center_PL",	"Casino_PL",	"Conference_PL",	"Convention_PL","Dry.Cleaning_PL","Elevators_PL","Fitness.Center_PL","Fitness.Trainer_PL","Golf_PL","Laundry_PL","Limo.Service_PL",	"Mini.Bar_PL",	"Pool.Indoor_PL",	"Pool.Outdoor_PL",	"Regency.Grand.Club_PL","Resort_PL","Restaurant_PL","Self.Parking_PL", "Shuttle.Service_PL","Spa_PL", "Valet.Parking_PL"),col="#e5c7bf")

NAplot <- ggplot(naCategorical,aes(X=Columns,Y=PercentageOfNAValues)) + geom_boxplot()
NAplot



# Removing Column Fitness.Trainer_PL

HotelData <- HotelData[,-'Fitness.Trainer_PL']

# Removing all NA Values
HotelData <- na.omit(HotelData)


#File Exported 
############################### MODELLING TECHNIQUES ###########################


##### LINEAR MODEL####

ModellingData <- read.csv(file.choose())

View(ModellingData)
ModellingData <- ModellingData[,-1]
# Linear Modelling
### We shall be using Likelihood to recommend as it is a numeric field
## Likelihood_Recommend_H vs Overall_Sat_H, Guest_Room_H,  Tranquility_H
LikeVsOverallGuestTranquility <- lm(Likelihood_Recommend_H ~ Overall_Sat_H + Guest_Room_H +Tranquility_H,ModellingData)
summary(LikeVsOverallGuestTranquility)
##Multiple R-squared:  0.817,	Adjusted R-squared:  0.8169 
## Removing Overall_Sat_H
LikeVsGuestTranquility <- lm(Likelihood_Recommend_H ~ Guest_Room_H +Tranquility_H,ModellingData)
summary(LikeVsGuestTranquility)
#Multiple R-squared:  0.5373,	Adjusted R-squared:  0.5373 
#Likelihood_Recommend_H vs Guest_Room_H,  Tranquility_H, Condition_Hotel_H
LikeVsGuestTranquilityCondition <- lm(Likelihood_Recommend_H ~ Guest_Room_H +Tranquility_H + Condition_Hotel_H,ModellingData)
summary(LikeVsGuestTranquilityCondition)
#Multiple R-squared:  0.5954,	Adjusted R-squared:  0.5954 

#Guest_Room_H +Tranquility_H + Customer_SVC_H + Condition_Hotel_H
LikeVsGuestTranquilityCustomer <- lm(Likelihood_Recommend_H ~ Guest_Room_H +Tranquility_H +  Customer_SVC_H +  Condition_Hotel_H,ModellingData)
summary(LikeVsGuestTranquilityCustomer)
#Multiple R-squared:  0.6777,	Adjusted R-squared:  0.6777

# Customer Service somewhat contributes to NPS 
#Adding Staff_Cared_H
Model3AndStaff <- lm(Likelihood_Recommend_H ~ Guest_Room_H +Tranquility_H +  Customer_SVC_H + Staff_Cared_H + Condition_Hotel_H,ModellingData)
summary(Model3AndStaff)

#Multiple R-squared:  0.6808,	Adjusted R-squared:  0.6807


# Adding Check_In_H

Model3AndCheckIn <- lm(Likelihood_Recommend_H ~ Guest_Room_H +Tranquility_H +  Customer_SVC_H + Check_In_H+ Condition_Hotel_H,ModellingData)
summary(Model3AndCheckIn)

#Multiple R-squared:  0.6779,	Adjusted R-squared:  0.6778 

#Guest_Room_H +Tranquility_H +  Customer_SVC_H + Check_In_H+ Condition_Hotel_H

View(ModellingData)
plot(Model3AndCheckIn)



###########KSVM################

#KSVM FOR VAriables selected in Linear Model

ModellingData2 <- read.csv(file.choose())
View(ModellingData2)

# Removing Checker ID
ModellingData2 <- ModellingData2[,-1]

# Removing Room Number and Room Type Code

ModellingData2 <- ModellingData2[,-1:-2]

#Removing Checkin Date

ModellingData2 <- ModellingData2[,-2]

#Removing Guest Country and Major Market Code

ModellingData2 <- ModellingData2[,-3:-4]

# Removing US Region and COuntry

ModellingData2 <- ModellingData2[,-13:-14]

#Removing Hotel Inventory 

ModellingData2 <- ModellingData2[,-14]


# Checking Summary

summary(ModellingData2)


# Removing Boutique_PL as count is too biased

ModellingData2 <- ModellingData2[,-16]


#Removing  Business.Center_PL, Casino_ Pl and Conference_PL

ModellingData2 <- ModellingData2[,-16:-18]

#Removing Dry.Cleaning_PL as all the values are Y

ModellingData2 <- ModellingData2[,-17]

##Removing Elevators_PL as the count is too biased
ModellingData2 <- ModellingData2[,-17]

#Removing Fitness Center as count is too biased

ModellingData2 <- ModellingData2[,-17]

# Removing Golf_PL as count is too biased

ModellingData2 <- ModellingData2[,-17]

# Removing Pool Indoor and Outdoor
ModellingData2 <- ModellingData2[,-20:-21]

#Removing MiniBar
ModellingData2 <- ModellingData2[,-19]


#Removing Resort

ModellingData2 <- ModellingData2[,-20]

# Removing Self Parking
ModellingData2 <- ModellingData2[,-21]


# Removing Booking Channel
ModellingData2 <- ModellingData2[,-24]


write.csv(ModellingData2, "FinalDatasetFinal.csv")


# Creating Category " High", " Medium"," Low"

for(i in 1:nrow(ModellingData2)){
  if(ModellingData2[i,3] >= 9){
    ModellingData2[i,ncol(ModellingData2)] <- "High"
  } else if (ModellingData2[i,3]==7 | ModellingData2[i,3]==8){
    ModellingData2[i,ncol(ModellingData2)] <- "Medium"
  } else{
    ModellingData2[i,ncol(ModellingData2)] <- "Low"
  }
}

View(ModellingData2)


############################ KSVM for Variables shortlisted in Linear Model#################

random.indexes <- sample(1:nrow(ModellingData2),replace=TRUE)
cutpoint2_3 <- floor(nrow(ModellingData2) /3 * 2)
ModellingData.Train <- ModellingData2[random.indexes[1:cutpoint2_3],]
ModellingData.Test <- ModellingData2[random.indexes[cutpoint2_3 + 1: nrow(ModellingData2)],]
ModellingData.Test <- na.omit(ModellingData.Test)
View(ModellingData.Train)
View(ModellingData.Test)

library(kernlab)

svmOutput.Numeric <- ksvm(Likelihood_Recommend_H ~ Guest_Room_H + Tranquility_H + Customer_SVC_H + Staff_Cared_H + Condition_Hotel_H ,  data=ModellingData.Train, kernel="rbfdot", kpar = "automatic", C= 10, cross= 10, prob.model=TRUE)
svmOutput.Numeric

KsvmPred.Numeric <- predict(svmOutput.Numeric,ModellingData.Test, type="votes")
KsvmPred.Numeric

Rounded.KsvmPred.Numeric <- round(KsvmPred.Numeric)

#Creating Data Frame of Actual and Predicted Value
CompTable1.Numeric <- data.frame(ModellingData.Test$Likelihood_Recommend_H, Rounded.KsvmPred.Numeric)
View(CompTable1.Numeric)
#Renaming column names
colnames(CompTable1.Numeric) <- c("Test", "Predicted")
View(CompTable1.Numeric)
results.Numeric <- table(CompTable1.Numeric)
results.Numeric
CompTableNumeric.ex<- CompTable1.Numeric
CompTableNumeric.ex$TestNPS <- NA
CompTableNumeric.ex$PredictedNPS <- NA
# Putting NPS Type for Test Column on the basis of Value

for(i in 1:nrow(CompTableNumeric.ex)){
  if(CompTableNumeric.ex[i,1] >= 9){
    CompTableNumeric.ex[i,3] <- " Promoter"
  } else if (CompTableNumeric.ex[i,1]==7 | CompTableNumeric.ex[i,1]==8){
    CompTableNumeric.ex[i,3] <- " Passive"
  } else{
    CompTableNumeric.ex[i,3] <- "Detractor"
  }
}

View(CompTableNumeric.ex)

# Putting NPS Type for Predicted Column on the basis of Value

for(i in 1:nrow(CompTableNumeric.ex)){
  if(CompTableNumeric.ex[i,2] >= 9){
    CompTableNumeric.ex[i,4] <- " Promoter"
  } else if (CompTableNumeric.ex[i,2]==7 | CompTableNumeric.ex[i,2]==8){
    CompTableNumeric.ex[i,4] <- " Passive"
  } else{
    CompTableNumeric.ex[i,4] <- "Detractor"
  }
}

View(CompTableNumeric.ex)

# Creating a New Data Frame of the Predicted NPS TYPE 

CompTable1Numeric.NPS <- data.frame(CompTableNumeric.ex$TestNPS,CompTableNumeric.ex$PredictedNPS)
View(CompTable1Numeric.NPS)
head(CompTable1Numeric.NPS)
colnames(CompTable1Numeric.NPS) <- c("TestNPS","PredictedNPS")
resultsNumeric.NPS <- table(CompTable1Numeric.NPS)
resultsNumeric.NPS
AccuracyKSVM.Numeric <- ((resultsNumeric.NPS[1,1]+resultsNumeric.NPS[2,2]+resultsNumeric.NPS[3,3])/(resultsNumeric.NPS[1,1]+resultsNumeric.NPS[1,2]+resultsNumeric.NPS[1,3]+resultsNumeric.NPS[2,1]+resultsNumeric.NPS[2,2]+resultsNumeric.NPS[2,3]+resultsNumeric.NPS[3,1]+resultsNumeric.NPS[3,2]+resultsNumeric.NPS[3,3])) * 100
AccuracyKSVM.Numeric

# Accuracy : # 84.99255





##############KSVM FOR ALL THE VARIABLES


ModellingData.AllVariables <- ModellingData2
View(ModellingData.AllVariables)

ModellingData.AllVariables <- ModellingData.AllVariables[,-1:-2]
ModellingData.AllVariables <- ModellingData.AllVariables[,-9:-11]

############## N =1, Y =2
ModellingData.AllVariables$All.Suites_PL <- as.factor(as.numeric(as.factor(ModellingData.AllVariables$All.Suites_PL)))

for(i in 10:18){
  ModellingData.AllVariables[,i] <- as.factor(as.numeric(as.factor(ModellingData.AllVariables[,i])))
}

# Removing NPS_Type

ModellingData.AllVariables <- ModellingData.AllVariables[,-19]

# Removing Likelihood to Recommend
ModellingData.AllVariables <- ModellingData.AllVariables[,-1]

#Removing Overall Satisfaction
ModellingData.AllVariables <- ModellingData.AllVariables[,-1]


# Converting LikelihoodCategory to Numbers

ModellingData.AllVariables$LikelihoodCategory <- as.factor(as.numeric(as.factor(ModellingData.AllVariables$LikelihoodCategory)))



####### Creating Train and Test Data

random.indexes <- sample(1:nrow(ModellingData.AllVariables),replace=TRUE)
cutpoint2_3 <- floor(nrow(ModellingData.AllVariables) /3 * 2)
ModellingData.TrainAll <- ModellingData.AllVariables[random.indexes[1:cutpoint2_3],]
ModellingData.TestAll <- ModellingData.AllVariables[random.indexes[cutpoint2_3 + 1: nrow(ModellingData.AllVariables)],]
ModellingData.TestAll <- na.omit(ModellingData.TestAll)
View(ModellingData.TrainAll)
View(ModellingData.TestAll)

str(ModellingData.AllVariables)

# Converting all columns to factor
# Converting all the columns to numeric



for (i in 1:ncol(ModellingData.TrainAll)){
  ModellingData.TrainAll[,i] <- as.numeric(ModellingData.TrainAll[,i])
  
}

for (i in 1:ncol(ModellingData.TestAll)){
  ModellingData.TestAll[,i] <- as.numeric(ModellingData.TestAll[,i])
  
}

str(ModellingData.TestAll)
# Converting all to numeric


# Creating KSVM using all variables
library(kernlab)
svmOutput.All <- ksvm(LikelihoodCategory ~.,  data=ModellingData.TrainAll, kernel="rbfdot", kpar = "automatic", C= 10, cross= 10, prob.model=TRUE)
summary(svmOutput.All)
svmOutput.All

KsvmPred.All <- predict(svmOutput.All,ModellingData.TestAll, type="votes")
KsvmPred.All
View(KsvmPred.All)
Rounded.KsvmPred.All <- round(KsvmPred.All)

#Creating Data Frame of Actual and Predicted Value
CompTable1.All <- data.frame(ModellingData.TestAll$LikelihoodCategory, Rounded.KsvmPred.All)
View(CompTable1.All)
#Renaming column names
colnames(CompTable1.All) <- c("Test", "Predicted")
View(CompTable1.All)
results.All <- table(CompTable1.All)
results.All

AccuracyKSVM.All <- ((results.All[1,1]+results.All[2,2]+results.All[3,3])/(results.All[1,1]+results.All[1,2]+results.All[1,3]+results.All[2,1]+results.All[2,2]+results.All[2,3]+results.All[3,1]+results.All[3,2]+results.All[3,3])) * 100
AccuracyKSVM.All

############Accuracy : 84.98%




##########################SVM########################


# SVM FOR VARIABLEs selected in Linear Model




###########################################SVM using Numeric Variables #######################
install.packages("e1071")
library(e1071)


svmOutput2.Numeric <- svm(Likelihood_Recommend_H ~ Guest_Room_H + Tranquility_H + Customer_SVC_H + Staff_Cared_H + Condition_Hotel_H ,  data=ModellingData.Train)
svmOutput2.Numeric
summary(svmOutput2.Numeric)
svmPred.Numeric <- predict(svmOutput2.Numeric,ModellingData.Test, type="votes")
svmPred.Numeric

Rounded.svmPred.Numeric <- round(svmPred.Numeric)

#Creating Data Frame of Actual and Predicted Value
CompTable2.Numeric <- data.frame(ModellingData.Test$Likelihood_Recommend_H, Rounded.svmPred.Numeric)
View(CompTable2.Numeric)
#Renaming column names
colnames(CompTable2.Numeric) <- c("Test", "Predicted")
View(CompTable2.Numeric)
results2.Numeric <- table(CompTable2.Numeric)
results2.Numeric
CompTable2Numeric.ex<- CompTable2.Numeric
CompTable2Numeric.ex$TestNPS <- NA
CompTable2Numeric.ex$PredictedNPS <- NA
# Putting NPS Type for Test Column on the basis of Value

for(i in 1:nrow(CompTable2Numeric.ex)){
  if(CompTable2Numeric.ex[i,1] >= 9){
    CompTable2Numeric.ex[i,3] <- " Promoter"
  } else if (CompTable2Numeric.ex[i,1]==7 | CompTable2Numeric.ex[i,1]==8){
    CompTable2Numeric.ex[i,3] <- " Passive"
  } else{
    CompTable2Numeric.ex[i,3] <- "Detractor"
  }
}

View(CompTable2Numeric.ex)

# Putting NPS Type for Predicted Column on the basis of Value

for(i in 1:nrow(CompTable2Numeric.ex)){
  if(CompTable2Numeric.ex[i,2] >= 9){
    CompTable2Numeric.ex[i,4] <- " Promoter"
  } else if (CompTable2Numeric.ex[i,2]==7 | CompTable2Numeric.ex[i,2]==8){
    CompTable2Numeric.ex[i,4] <- " Passive"
  } else{
    CompTable2Numeric.ex[i,4] <- "Detractor"
  }
}

View(CompTable2Numeric.ex)

# Creating a New Data Frame of the Predicted NPS TYPE 

CompTable2Numeric.NPS <- data.frame(CompTable2Numeric.ex$TestNPS,CompTable2Numeric.ex$PredictedNPS)
View(CompTable2Numeric.NPS)
head(CompTable2Numeric.NPS)
colnames(CompTable2Numeric.NPS) <- c("TestNPS","PredictedNPS")
resultsNumeric2.NPS <- table(CompTable2Numeric.NPS)
resultsNumeric2.NPS
AccuracySVM.Numeric <- ((resultsNumeric2.NPS[1,1]+resultsNumeric2.NPS[2,2]+resultsNumeric2.NPS[3,3])/(resultsNumeric2.NPS[1,1]+resultsNumeric2.NPS[1,2]+resultsNumeric2.NPS[1,3]+resultsNumeric2.NPS[2,1]+resultsNumeric2.NPS[2,2]+resultsNumeric2.NPS[2,3]+resultsNumeric2.NPS[3,1]+resultsNumeric2.NPS[3,2]+resultsNumeric2.NPS[3,3])) * 100
AccuracySVM.Numeric

# Accuracy : # 83.56504





###############SVM USING ALL THE VARIABLES


library(e1071)
svmOutput2.All <- svm(LikelihoodCategory ~.,  data=ModellingData.TrainAll)
summary(svmOutput2.All)

svmPred2.All <- predict(svmOutput2.All,ModellingData.TestAll, type="votes")
svmPred2.All
View(svmPred2.All)
Rounded.svmPred2.All <- round(svmPred2.All)

#Creating Data Frame of Actual and Predicted Value
CompTable2.All <- data.frame(ModellingData.TestAll$LikelihoodCategory, Rounded.svmPred2.All)
View(CompTable2.All)
#Renaming column names
colnames(CompTable2.All) <- c("Test", "Predicted")
View(CompTable2.All)
results2.All <- table(CompTable2.All)

results2.All

AccuracySVM2.All <- ((results2.All[1,1]+results2.All[2,2]+results2.All[3,3])/(results2.All[1,1]+results2.All[1,2]+results2.All[1,3]+results2.All[2,1]+results2.All[2,2]+results2.All[2,3]+results2.All[3,1]+results2.All[3,2]+results2.All[3,3])) * 100
AccuracySVM2.All


## Accuracy : 84.2477




###############ASSOCIATIVE RULE MINING ##########3


library(arules)
library(arulesViz)
ModellingData2.factor <- ModellingData2

for (i in 1:ncol(ModellingData2.factor)){
  ModellingData2.factor[,i] <- as.factor(ModellingData2.factor[,i])
  
}
View(ModellingData2.factor)

# Removing  NPS_Type
ModellingData2.factor <- ModellingData2.factor[,-24]

# Removing Overall Satisfaction and Likelihood to Recommend 

ModellingData2.factor <- ModellingData2.factor[,-3:-4]

ruleset <- apriori(ModellingData2.factor,parameter=list(support=0.3,confidence=0.8,maxlen=10),appearance=list(default="lhs",rhs=("LikelihoodCategory=High")))
summary(ruleset)


goodrules <- ruleset[quality(ruleset)$lift > 1.3]
goodrules
inspect(goodrules)
goodrules <- sort(goodrules,by='lift',decreasing=T)
summary(goodrules)
inspect(goodrules[1:20])
plot(goodrules)

itemFrequencyPlot(ModellingData2.factor, topN =10, type="absolute", main="Item Frequency")
#### For Low
rulesetlow <- apriori(ModellingData2.factor,parameter=list(support=0.01,confidence=0.8,maxlen=5),appearance=list(default="lhs",rhs=("LikelihoodCategory=Low")))
summary(rulesetlow)
goodrulesLow <- rulesetlow[quality(rulesetlow)$lift > 3.4]
goodrulesLow
inspect(goodrulesLow)
goodrulesLow <- sort(goodrulesLow,by='lift',decreasing=T)
summary(goodrulesLow)
inspect(goodrulesLow[1:20])
plot(goodrulesLow)



################Comparison of TWO HOTELS############

#Place with highest Promoters is Orlando and the best hotel is Hyatt Place, we shall compare it with Hyatt House
#Analyzing the factors

ModellingData.Orlando <- ModellingData[which(ModellingData$City_PL=="Orlando" & ModellingData$Brand_PL=='Hyatt Place'),]
View(ModellingData.Orlando)
ModellingData.DaniaBeach <- ModellingData[which(ModellingData$City_PL=="Dania Beach"),]
ModellingData.compare <- rbind(ModellingData.Orlando,ModellingData.DaniaBeach)
View(ModellingData.compare)
#Checking the Factors

# Overall Satisfaction:
BestFactors1 = mean(ModellingData.compare$Overall_Sat_H[(ModellingData.compare$City_PL=="Orlando")], na.rm=T)
print(paste("Desired Overall Satisfaction: ",BestFactors1))

#Guest_Room_H:

BestFactors2 = mean(ModellingData.compare$Guest_Room_H[(ModellingData.compare$City_PL=="Orlando")], na.rm=T)
print(paste("Desired Guest Room Satisfaction: ",BestFactors2))

#Tranquility_H

BestFactors3 = mean(ModellingData.compare$Tranquility_H[(ModellingData.compare$City_PL=="Orlando")], na.rm=T)
print(paste("Desired Tranquility Satisfaction: ",BestFactors3))

#Condition_Hotel_H
BestFactors4 = mean(ModellingData.compare$Condition_Hotel_H[(ModellingData.compare$City_PL=="Orlando")], na.rm=T)
print(paste("Desired Condition Satisfaction: ",BestFactors4))

#Customer_SVC_H

BestFactors5 = mean(ModellingData.compare$Customer_SVC_H[(ModellingData.compare$City_PL=="Orlando")], na.rm=T)
print(paste("Desired Customer Service Satisfaction: ",BestFactors5))

#Staff_Cared_H

BestFactors6 = mean(ModellingData.compare$Staff_Cared_H[(ModellingData.compare$City_PL=="Orlando")], na.rm=T)
print(paste("Desired Staff Service Satisfaction: ",BestFactors6))

#Internet_Sat_H

BestFactors7 = mean(ModellingData.compare$Internet_Sat_H[(ModellingData.compare$City_PL=="Orlando")], na.rm=T)
print(paste("Desired Internet Satisfaction: ",BestFactors7))

#Check_In_H
BestFactors8 = mean(ModellingData.compare$Check_In_H[(ModellingData.compare$City_PL=="Orlando")], na.rm=T)
print(paste("Desired Check In Satisfaction: ",BestFactors8))

#F.B_Overall_Experience_H
BestFactors9 = mean(ModellingData.compare$F.B_Overall_Experience_H[(ModellingData.compare$City_PL=="Orlando")], na.rm=T)
print(paste("Desired Food and Beverages Satisfaction: ",BestFactors9))


#########Hotels in Dania Beach#####


DBFactors1 = mean(ModellingData.compare$Overall_Sat_H[(ModellingData.compare$City_PL=="Dania Beach")], na.rm=T)
print(paste("Current Overall Satisfaction: ",DBFactors1))

#Guest_Room_H:

DBFactors2 = mean(ModellingData.compare$Guest_Room_H[(ModellingData.compare$City_PL=="Dania Beach")], na.rm=T)
print(paste("Current Guest Room Satisfaction: ",DBFactors2))

#Tranquility_H

DBFactors3 = mean(ModellingData.compare$Tranquility_H[(ModellingData.compare$City_PL=="Dania Beach")], na.rm=T)
print(paste("Current Tranquility Satisfaction: ",DBFactors3))

#Condition_Hotel_H
DBFactors4 = mean(ModellingData.compare$Condition_Hotel_H[(ModellingData.compare$City_PL=="Dania Beach")], na.rm=T)
print(paste("Current Condition Satisfaction: ",DBFactors4))

#Customer_SVC_H

DBFactors5 = mean(ModellingData.compare$Customer_SVC_H[(ModellingData.compare$City_PL=="Dania Beach")], na.rm=T)
print(paste("Current Customer Service Satisfaction: ",DBFactors5))

#Staff_Cared_H

DBFactors6 = mean(ModellingData.compare$Staff_Cared_H[(ModellingData.compare$City_PL=="Dania Beach")], na.rm=T)
print(paste("Current Staff Service Satisfaction: ",DBFactors6))

#Internet_Sat_H

DBFactors7 = mean(ModellingData.compare$Internet_Sat_H[(ModellingData.compare$City_PL=="Dania Beach")], na.rm=T)
print(paste("Current Internet Satisfaction: ",DBFactors7))

#Check_In_H
DBFactors8 = mean(ModellingData.compare$Check_In_H[(ModellingData.compare$City_PL=="Dania Beach")], na.rm=T)
print(paste("Current Check In Satisfaction: ",DBFactors8))

#F.B_Overall_Experience_H
DBFactors9 = mean(ModellingData.compare$F.B_Overall_Experience_H[(ModellingData.compare$City_PL=="Dania Beach")], na.rm=T)
print(paste("Current Food and Beverages Satisfaction: ",DBFactors9))


##########Plotting Graph###
Factors <- c("Overall_Sat_H","Guest_Room_H","Tranquility_H","Condition_Hotel_H","Customer_SVC_H","Staff_Cared_H","Internet_Sat_H","Check_In_H","F.B_Overall_Experience_H")
Desired <- c(BestFactors1,BestFactors2,BestFactors3,BestFactors4,BestFactors5,BestFactors6,BestFactors7,BestFactors8,BestFactors9)
Current <- c(DBFactors1,DBFactors2,DBFactors3,DBFactors4,DBFactors5,DBFactors6,DBFactors7,DBFactors8,DBFactors9)
ComparisonDF <- data.frame(Factors,Desired,Current)

library(ggplot2)
library(reshape2)
meltedComparisonDF = melt(ComparisonDF, id = "Factors")
ggplot(meltedComparisonDF, aes(Factors, value)) +geom_bar(aes(fill = variable), position = "dodge", stat="identity")+ggtitle("Orlando vs Dania Beach") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_manual(values=c("#aec6d7","#ffc0cb")) + theme_dark()


##########DATA ANALYSIS#######
install.packages("plotrix")
library(plotrix)

### FOR SPA
Spa_PL_n <- length(FinalDatasetFinal$Likelihood_Recommend_H[((FinalDatasetFinal$Likelihood_Recommend_H) < 8 )  & ((FinalDatasetFinal$Spa_PL)=="N")])
Spa_PL_y <-length(FinalDatasetFinal$Likelihood_Recommend_H[((FinalDatasetFinal$Likelihood_Recommend_H) < 8 )  & ((FinalDatasetFinal$Spa_PL)=="Y")])
pies <- c(Spa_PL_n,Spa_PL_y)
labels <- c("N","Y")
par(mar = rep(2, 4))
pct <- round(pies/sum(pies)*100)
lbls <- paste(labels, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie3D(pies, labels=lbls,explode=0.1, col = rainbow(length(pies)))


# FOR Shuttle Service
# Detracter and Shuttle Service 
Shuttle.Service_PL_n <- length(FinalDatasetFinal$Likelihood_Recommend_H[((FinalDatasetFinal$Likelihood_Recommend_H) < 8 ) &   ((FinalDatasetFinal$Shuttle.Service_PL)=="N")])
Shuttle.Service_PL_y <-length(FinalDatasetFinal$Likelihood_Recommend_H[((FinalDatasetFinal$Likelihood_Recommend_H) < 8 ) & ((FinalDatasetFinal$Shuttle.Service_PL)=="Y")])
piesShuttle.Service_PL <- c(Shuttle.Service_PL_n,Shuttle.Service_PL_y)
labels <- c("N","Y")
par(mar = rep(2, 4))
pctpiesShuttle.Service_PL <- round(piesShuttle.Service_PL/sum(piesShuttle.Service_PL)*100)
lblspiesShuttle.Service_PL <- paste(labels, pctpiesShuttle.Service_PL) # add percents to labels 
lblspiesShuttle.Service_PL <- paste(lblspiesShuttle.Service_PL,"%",sep="") # ad % to labels 
pie3D(piesShuttle.Service_PL, labels=lblspiesShuttle.Service_PL,explode=0.1, col = rainbow(length(piesShuttle.Service_PL)))



####Convention_PL
## Detractor 
Convention_PL_n <- length(FinalDatasetFinal$Likelihood_Recommend_H[((FinalDatasetFinal$Likelihood_Recommend_H) < 8 )  &   ((FinalDatasetFinal$Convention_PL)=="N")])
Convention_PL_y <-length(FinalDatasetFinal$Likelihood_Recommend_H[((FinalDatasetFinal$Likelihood_Recommend_H) < 8 ) & ((FinalDatasetFinal$Convention_PL)=="Y")])
piesConvention_PL <- c(Convention_PL_n,Convention_PL_y)
labels <- c("N","Y")
par(mar = rep(2, 4))
pctpiesConvention_PL <- round(piesConvention_PL/sum(piesConvention_PL)*100)
lblsConvention_PL <- paste(labels, pctpiesConvention_PL) # add percents to labels 
lblsConvention_PL <- paste(lblsConvention_PL,"%",sep="") # ad % to labels 
pie3D(piesConvention_PL, labels=lblsConvention_PL,explode=0.1, col = rainbow(length(piesConvention_PL)))

##### Valet.Parking_PL
######Detractor
Valet.Parking_PL_n <- length(FinalDatasetFinal$Likelihood_Recommend_H[((FinalDatasetFinal$Likelihood_Recommend_H) < 8 )  &   ((FinalDatasetFinal$Valet.Parking_PL)=="N")])
Valet.Parking_PL_y <-length(FinalDatasetFinal$Likelihood_Recommend_H[((FinalDatasetFinal$Likelihood_Recommend_H) < 8 )  & ((FinalDatasetFinal$Valet.Parking_PL)=="Y")])
piesValet.Parking_PL <- c(Valet.Parking_PL_n,Valet.Parking_PL_y)
labels <- c("N","Y")
par(mar = rep(2, 4))
pctValet.Parking_PL <- round(piesValet.Parking_PL/sum(piesValet.Parking_PL)*100)
lblsValet.Parking_PL <- paste(labels, pctValet.Parking_PL) # add percents to labels 
lblsValet.Parking_PL <- paste(lblsValet.Parking_PL,"%",sep="") # ad % to labels 
pie3D(piesValet.Parking_PL, labels=lblsValet.Parking_PL,explode=0.1, col = rainbow(length(piesValet.Parking_PL)))


###All.Suites_PL
##Detractor
All.Suites_PL_n <- length(FinalDatasetFinal$Likelihood_Recommend_H[((FinalDatasetFinal$Likelihood_Recommend_H) < 8 )  & ((FinalDatasetFinal$All.Suites_PL)=="N")])
All.Suites_PL_y <-length(FinalDatasetFinal$Likelihood_Recommend_H[((FinalDatasetFinal$Likelihood_Recommend_H) < 8 )  & ((FinalDatasetFinal$All.Suites_PL)=="Y")])
piesAll.Suites_PL <- c(All.Suites_PL_n,All.Suites_PL_y)
labels <- c("N","Y")
par(mar = rep(2, 4))
pctAll.Suites_PLL <- round(piesAll.Suites_PL/sum(piesAll.Suites_PL)*100)
lblsAll.Suites_PL <- paste(labels, pctAll.Suites_PLL) # add percents to labels 
lblsAll.Suites_PL <- paste(lblsAll.Suites_PL,"%",sep="") # ad % to labels 
pie3D(piesAll.Suites_PL, labels=lblsAll.Suites_PL,explode=0.1, col = rainbow(length(piesAll.Suites_PL)))

#Bell Staff
Bell.Staff_PL_n <- length(FinalDatasetFinal$Likelihood_Recommend_H[((FinalDatasetFinal$Likelihood_Recommend_H) < 8 )  & ((FinalDatasetFinal$Bell.Staff_PL)=="N")])
Bell.Staff_PL_y <-length(FinalDatasetFinal$Likelihood_Recommend_H[((FinalDatasetFinal$Likelihood_Recommend_H) < 8 )  & ((FinalDatasetFinal$Bell.Staff_PL)=="Y")])
piesBell.Staff_PL <- c(Bell.Staff_PL_n,Bell.Staff_PL_y)
labels <- c("N","Y")
par(mar = rep(2, 4))
pctBell.Staff_PL <- round(piesBell.Staff_PL/sum(piesBell.Staff_PL)*100)
lblsBell.Staff_PL <- paste(labels, pctBell.Staff_PL) # add percents to labels 
lblsBell.Staff_PL <- paste(lblsBell.Staff_PL,"%",sep="") # ad % to labels 
pie3D(piesBell.Staff_PL, labels=lblsBell.Staff_PL,explode=0.1, col = rainbow(length(piesBell.Staff_PL)))

###Limo.Service_PL
##Detractor
Limo.Service_PL_n <- length(FinalDatasetFinal$Likelihood_Recommend_H[((FinalDatasetFinal$Likelihood_Recommend_H) < 8 )  & ((FinalDatasetFinal$Limo.Service_PL)=="N")])
Limo.Service_PL_y <-length(FinalDatasetFinal$Likelihood_Recommend_H[((FinalDatasetFinal$Likelihood_Recommend_H) < 8 )  & ((FinalDatasetFinal$Limo.Service_PL)=="Y")])
piesLimo.Service_PL <- c(Limo.Service_PL_n,Limo.Service_PL_y)
labels <- c("N","Y")
par(mar = rep(2, 4))
pctLimo.Service_PL <- round(piesLimo.Service_PL/sum(piesLimo.Service_PL)*100)
lblsLimo.Service_PL <- paste(labels, pctLimo.Service_PL) # add percents to labels 
lblsLimo.Service_PL <- paste(lblsLimo.Service_PL,"%",sep="") # ad % to labels 
pie3D(piesLimo.Service_PL, labels=lblsLimo.Service_PL,explode=0.1, col = rainbow(length(piesLimo.Service_PL)))


###Regency.Grand.Club_PL
##Detractors
Regency.Grand.Club_PL_n <- length(FinalDatasetFinal$Likelihood_Recommend_H[((FinalDatasetFinal$Likelihood_Recommend_H) < 8 )  & ((FinalDatasetFinal$Regency.Grand.Club_PL)=="N")])
Regency.Grand.Club_PL_y <-length(FinalDatasetFinal$Likelihood_Recommend_H[((FinalDatasetFinal$Likelihood_Recommend_H) < 8 )  & ((FinalDatasetFinal$Regency.Grand.Club_PL)=="Y")])
piesRegency.Grand.Club_PL <- c(Regency.Grand.Club_PL_n,Regency.Grand.Club_PL_y)
labels <- c("N","Y")
par(mar = rep(2, 4))
pctRegency.Grand.Club_PL <- round(piesRegency.Grand.Club_PL/sum(piesRegency.Grand.Club_PL)*100)
lblsRegency.Grand.Club_PL <- paste(labels, pctRegency.Grand.Club_PL) # add percents to labels 
lblsRegency.Grand.Club_PL <- paste(lblsRegency.Grand.Club_PL,"%",sep="") # ad % to labels 
pie3D(piesRegency.Grand.Club_PL, labels=lblsRegency.Grand.Club_PL,explode=0.1, col = rainbow(length(piesRegency.Grand.Club_PL)))

##Restaurant_PL
##Detractors
Restaurant_PL_n <- length(FinalDatasetFinal$Likelihood_Recommend_H[((FinalDatasetFinal$Likelihood_Recommend_H) < 8 )  & ((FinalDatasetFinal$Restaurant_PL)=="N")])
Restaurant_PL_y <-length(FinalDatasetFinal$Likelihood_Recommend_H[((FinalDatasetFinal$Likelihood_Recommend_H) < 8 )  & ((FinalDatasetFinal$Restaurant_PL)=="Y")])
piesRestaurant_PL <- c(Restaurant_PL_n,Restaurant_PL_y)
labels <- c("N","Y")
par(mar = rep(2, 4))
pctRestaurant_PL <- round(piesRestaurant_PL/sum(piesRestaurant_PL)*100)
lblsRestaurant_PL <- paste(labels, pctRestaurant_PL) # add percents to labels 
lblsRestaurant_PL <- paste(lblsRestaurant_PL,"%",sep="") # ad % to labels 
pie3D(piesRestaurant_PL, labels=lblsRestaurant_PL,explode=0.1, col = rainbow(length(piesRestaurant_PL)))


#
##Laundry_PL
##Promoter
Laundry_PL_n <- length(FinalDatasetFinal$Likelihood_Recommend_H[((FinalDatasetFinal$Likelihood_Recommend_H) > 8 )  & ((FinalDatasetFinal$Laundry_PL)=="N")])
Laundry_PL_y <-length(FinalDatasetFinal$Likelihood_Recommend_H[((FinalDatasetFinal$Likelihood_Recommend_H) > 8 )  & ((FinalDatasetFinal$Laundry_PL)=="Y")])
piesLaundry_PL <- c(Laundry_PL_n,Laundry_PL_y)
labels <- c("N","Y")
par(mar = rep(2, 4))
pctLaundry_PL <- round(piesLaundry_PL/sum(piesLaundry_PL)*100)
lblsLaundry_PL <- paste(labels, pctLaundry_PL) # add percents to labels 
lblsLaundry_PL <- paste(lblsLaundry_PL,"%",sep="") # ad % to labels 
pie3D(piesLaundry_PL, labels=lblsLaundry_PL,explode=0.1, col = rainbow(length(piesLaundry_PL)))

#
### Count of Customers who visited Hotels in Florida and California ( Hotel Wise)

HotelPlot <- ggplot(FinalDatasetFinal, aes(x=Brand_PL)) + geom_bar(aes(fill=POV_CODE_C), position="dodge") + facet_grid(State_PL ~ .) + scale_fill_manual(values=c("red", "green"))
HotelPlot


##Hotel with Maximum Promoter and Detractor
HotelPromoter <- ggplot(FinalDatasetFinal, aes(x=Brand_PL)) + geom_bar(aes(fill=NPS_Type), position="dodge") + facet_grid(State_PL ~ .) + scale_fill_manual(values=c("red", "green","blue"))
HotelPromoter

# How does having Guest Room effect the promoter and detractor

HotelGuest_Room_H <- ggplot(FinalDatasetFinal, aes(x=Guest_Room_H)) + geom_bar(aes(fill=(NPS_Type),width=0.3)) + scale_fill_manual(values=c("red", "green","blue")) +scale_x_continuous(breaks=seq(0.0, 10.0, 1.0))
HotelGuest_Room_H

# How does having COnidition_Hotel effect the promoter and detractor

HotelCondition_Hotel_H <- ggplot(FinalDatasetFinal, aes(x=Condition_Hotel_H)) + geom_bar(aes(fill=NPS_Type),width=0.3) +  scale_fill_manual(values=c("red", "green","blue"))+scale_x_continuous(breaks=seq(0.0, 10.0, 1.0))
HotelCondition_Hotel_H

# How does having Customer_svc effect the promoter and detractor
HotelCustomer_Service_H <- ggplot(FinalDatasetFinal, aes(x=Customer_SVC_H)) + geom_bar(aes(fill=NPS_Type),width = 0.3)  + scale_fill_manual(values=c("red", "green","blue"))+ scale_x_continuous(breaks=seq(0.0, 10.0, 1.0))
HotelCustomer_Service_H

#Purpose of Visit

library(ggplot2)
HotelPlot <- ggplot(ModellingData, aes(x=Brand_PL)) + geom_bar(aes(fill=POV_CODE_C), position="dodge") + facet_grid(State_PL ~ .) + scale_fill_manual(values=c("red", "green"))
HotelPlot


#Top 2 cities in USA with most number of customers
CityCustomers <-ggplot(ModellingData, aes(x=City_PL)) + geom_bar(aes(fill=X), position ="dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
CityCustomers

#Cities with maximum number of Promoters

Promoters <- ModellingData[which(ModellingData$NPS_Type=="Promoter"),]
View(Promoters)
CityPromoters <-ggplot(Promoters, aes(x=City_PL)) + geom_bar(aes(fill=X), position ="dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
CityPromoters


# Comparison of Two Hotels

Comparison of Two Hotels


#Place with highest Promoters is Orlando and the best hotel is Hyatt Place, we shall compare it with Hyatt House
#Analyzing the factors

ModellingData.Orlando <- ModellingData[which(ModellingData$City_PL=="Orlando" & ModellingData$Brand_PL=='Hyatt Place'),]
View(ModellingData.Orlando)
ModellingData.DaniaBeach <- ModellingData[which(ModellingData$City_PL=="Dania Beach" ),]
ModellingData.compare <- rbind(ModellingData.Orlando,ModellingData.DaniaBeach)
View(ModellingData.compare)
#Checking the Factors

# Overall Satisfaction:
BestFactors1 = mean(ModellingData.compare$Overall_Sat_H[(ModellingData.compare$City_PL=="Orlando")], na.rm=T)
print(paste("Desired Overall Satisfaction: ",BestFactors1))

#Guest_Room_H:

BestFactors2 = mean(ModellingData.compare$Guest_Room_H[(ModellingData.compare$City_PL=="Orlando")], na.rm=T)
print(paste("Desired Guest Room Satisfaction: ",BestFactors2))

#Tranquility_H

BestFactors3 = mean(ModellingData.compare$Tranquility_H[(ModellingData.compare$City_PL=="Orlando")], na.rm=T)
print(paste("Desired Tranquility Satisfaction: ",BestFactors3))

#Condition_Hotel_H
BestFactors4 = mean(ModellingData.compare$Condition_Hotel_H[(ModellingData.compare$City_PL=="Orlando")], na.rm=T)
print(paste("Desired Condition Satisfaction: ",BestFactors4))

#Customer_SVC_H

BestFactors5 = mean(ModellingData.compare$Customer_SVC_H[(ModellingData.compare$City_PL=="Orlando")], na.rm=T)
print(paste("Desired Customer Service Satisfaction: ",BestFactors5))

#Staff_Cared_H

BestFactors6 = mean(ModellingData.compare$Staff_Cared_H[(ModellingData.compare$City_PL=="Orlando")], na.rm=T)
print(paste("Desired Staff Service Satisfaction: ",BestFactors6))


#Check_In_H
BestFactors8 = mean(ModellingData.compare$Check_In_H[(ModellingData.compare$City_PL=="Orlando")], na.rm=T)
print(paste("Desired Check In Satisfaction: ",BestFactors8))



#########Hotels in Dania Beach#####


DBFactors1 = mean(ModellingData.compare$Overall_Sat_H[(ModellingData.compare$City_PL=="Dania Beach")], na.rm=T)
print(paste("Current Overall Satisfaction: ",DBFactors1))

#Guest_Room_H:

DBFactors2 = mean(ModellingData.compare$Guest_Room_H[(ModellingData.compare$City_PL=="Dania Beach")], na.rm=T)
print(paste("Current Guest Room Satisfaction: ",DBFactors2))

#Tranquility_H

DBFactors3 = mean(ModellingData.compare$Tranquility_H[(ModellingData.compare$City_PL=="Dania Beach")], na.rm=T)
print(paste("Current Tranquility Satisfaction: ",DBFactors3))

#Condition_Hotel_H
DBFactors4 = mean(ModellingData.compare$Condition_Hotel_H[(ModellingData.compare$City_PL=="Dania Beach")], na.rm=T)
print(paste("Current Condition Satisfaction: ",DBFactors4))

#Customer_SVC_H

DBFactors5 = mean(ModellingData.compare$Customer_SVC_H[(ModellingData.compare$City_PL=="Dania Beach")], na.rm=T)
print(paste("Current Customer Service Satisfaction: ",DBFactors5))

#Staff_Cared_H

DBFactors6 = mean(ModellingData.compare$Staff_Cared_H[(ModellingData.compare$City_PL=="Dania Beach")], na.rm=T)
print(paste("Current Staff Service Satisfaction: ",DBFactors6))


#Check_In_H
DBFactors8 = mean(ModellingData.compare$Check_In_H[(ModellingData.compare$City_PL=="Dania Beach")], na.rm=T)
print(paste("Current Check In Satisfaction: ",DBFactors8))



##########Plotting Graph###
Factors <- c("Overall_Sat_H","Guest_Room_H","Tranquility_H","Condition_Hotel_H","Customer_SVC_H","Staff_Cared_H","Check_In_H")
Desired <- c(BestFactors1,BestFactors2,BestFactors3,BestFactors4,BestFactors5,BestFactors6,BestFactors8)
Current <- c(DBFactors1,DBFactors2,DBFactors3,DBFactors4,DBFactors5,DBFactors6,DBFactors8)
ComparisonDF <- data.frame(Factors,Desired,Current)

library(ggplot2)
library(reshape2)
meltedComparisonDF = melt(ComparisonDF, id = "Factors")
ggplot(meltedComparisonDF, aes(Factors, value)) +geom_bar(aes(fill = variable), position = "dodge", stat="identity")+ggtitle("Orlando vs Dania Beach") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_manual(values=c("#aec6d7","#ffc0cb")) + theme_dark()


########Hotel Wise NPS

#########################################
library(sqldf)
install.packages("sqldf")
# For Hyatt Regency
HyattRegencyPromoter <- sqldf('Select Count(X) FROM ModellingData WHERE Brand_PL="Hyatt Regency" AND NPS_Type="Promoter"')
HyattRegencyDetractor <- sqldf('Select Count(X) FROM ModellingData WHERE Brand_PL="Hyatt Regency" AND NPS_Type="Detractor"')
HyattRegencyPassive <- sqldf('Select Count(X) FROM ModellingData WHERE Brand_PL="Hyatt Regency" AND NPS_Type="Passive"')

NPSHyattRegency <- ((HyattRegencyPromoter-HyattRegencyDetractor)/(HyattRegencyPromoter+HyattRegencyDetractor+HyattRegencyPassive)) * 100
NPSHyattRegency

# For Hyatt Place
HyattPlacePromoter <- sqldf('Select Count(X) FROM ModellingData WHERE Brand_PL="Hyatt Place" AND NPS_Type="Promoter"')
HyattPlaceDetractor <- sqldf('Select Count(X) FROM ModellingData WHERE Brand_PL="Hyatt Place" AND NPS_Type="Detractor"')
HyattPlacePassive <- sqldf('Select Count(X) FROM ModellingData WHERE Brand_PL="Hyatt Place" AND NPS_Type="Passive"')

NPSHyattPlace <- ((HyattPlacePromoter-HyattPlaceDetractor)/(HyattPlacePromoter+HyattPlaceDetractor+HyattPlacePassive)) * 100
NPSHyattPlace


## For Hyatt House

HyattHousePromoter <- sqldf('Select Count(X) FROM ModellingData WHERE Brand_PL="Hyatt House" AND NPS_Type="Promoter"')
HyattHouseDetractor <- sqldf('Select Count(X) FROM ModellingData WHERE Brand_PL="Hyatt House" AND NPS_Type="Detractor"')
HyattHousePassive <- sqldf('Select Count(X) FROM ModellingData WHERE Brand_PL="Hyatt House" AND NPS_Type="Passive"')

NPSHyattHouse<- ((HyattHousePromoter-HyattHouseDetractor)/(HyattHousePromoter+HyattHouseDetractor+HyattHousePassive)) * 100
NPSHyattHouse

##Andaz

AndazPromoter <- sqldf('Select Count(X) FROM ModellingData WHERE Brand_PL="Andaz" AND NPS_Type="Promoter"')
AndazDetractor <- sqldf('Select Count(X) FROM ModellingData WHERE Brand_PL="Andaz" AND NPS_Type="Detractor"')
AndazPassive <- sqldf('Select Count(X) FROM ModellingData WHERE Brand_PL="Andaz" AND NPS_Type="Passive"')

NPSAndaz<- ((AndazPromoter-AndazDetractor)/(AndazPromoter+AndazDetractor+AndazPassive)) * 100
NPSAndaz

##ParkHyatt

ParkHyattPromoter <- sqldf('Select Count(X) FROM ModellingData WHERE Brand_PL="Park Hyatt" AND NPS_Type="Promoter"')
ParkHyattDetractor <- sqldf('Select Count(X) FROM ModellingData WHERE Brand_PL="Park Hyatt" AND NPS_Type="Detractor"')
ParkHyattPassive <- sqldf('Select Count(X) FROM ModellingData WHERE Brand_PL="Park Hyatt" AND NPS_Type="Passive"')

NPSParkHyatt<- ((ParkHyattPromoter-ParkHyattDetractor)/(ParkHyattPromoter+ParkHyattDetractor+ParkHyattPassive)) * 100
NPSParkHyatt

#Hyatt

HyattPromoter <- sqldf('Select Count(X) FROM ModellingData WHERE Brand_PL="Hyatt" AND NPS_Type="Promoter"')
HyattDetractor <- sqldf('Select Count(X) FROM ModellingData WHERE Brand_PL="Hyatt" AND NPS_Type="Detractor"')
HyattPassive <- sqldf('Select Count(X) FROM ModellingData WHERE Brand_PL="Hyatt" AND NPS_Type="Passive"')

NPSHyatt<- ((HyattPromoter-HyattDetractor)/(HyattPromoter+HyattDetractor+HyattPassive)) * 100
NPSHyatt

#GrandHyatt

GrandHyattPromoter <- sqldf('Select Count(X) FROM ModellingData WHERE Brand_PL="Grand Hyatt" AND NPS_Type="Promoter"')
GrandHyattDetractor <- sqldf('Select Count(X) FROM ModellingData WHERE Brand_PL="Grand Hyatt" AND NPS_Type="Detractor"')
GrandHyattPassive <- sqldf('Select Count(X) FROM ModellingData WHERE Brand_PL="Grand Hyatt" AND NPS_Type="Passive"')

NPSGrandHyatt<- ((GrandHyattPromoter-GrandHyattDetractor)/(GrandHyattPromoter+GrandHyattDetractor+GrandHyattPassive)) * 100
NPSGrandHyatt



### Creating Table

NPSHotel <- data.frame(NPSHyattRegency,NPSHyattPlace, NPSHyattHouse, NPSAndaz, NPSParkHyatt, NPSHyatt, NPSGrandHyatt)
NPSHotel
colnames(NPSHotel) <- c("NPSHyattRegency","NPSHyattPlace", "NPSHyattHouse", "NPSAndaz", "NPSParkHyatt", "NPSHyatt", "NPSGrandHyatt")
View(NPSHotel)

NPSHotel <- t(NPSHotel)
NPSHotel <- as.data.frame(NPSHotel)
colnames(NPSHotel) <- c("NPSPercentage")

barplot(NPSHotel$NPSPercentage,xlab="Comparision of NPS Values",ylab="Percentage of Values", ylim=c(0,100),main="Comparison",names.arg = c("NPSHyattRegency","NPSHyattPlace", "NPSHyattHouse", "NPSAndaz", "NPSParkHyatt", "NPSHyatt", "NPSGrandHyatt"),col="red")


