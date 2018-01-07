combineddata.New <- rbind(CaliforniaFloridaData.NonNA, CaliforniaFloridaDataApril.NonNA,CaliforniaFloridaDataJune.NonNA ,CaliforniaFloridaDataAugust.NonNA,CaliforniaFloridaDataOctober.NonNA,CaliforniaFloridaDataDecember.NonNA)
HotelData <- combineddata.New
View(HotelData)


##Writing the data to the file

write.csv(HotelData,file="Final Hotel Data2.csv")
HotelData <- read.csv("Final Hotel Data.csv")
##Data Cleaning

#Checking the missingness map for the dataset

library(Amelia)
missmap(ModellingDataSet2,col=c("yellow","black"),legend=TRUE)

#Creating Function to Replace NA VALUES

for(i in 1:nrow(HotelData)){
  {
    if(HotelData[i,ncol(HotelData)]=='Promoter'){
   tt <- HotelData[i,]
  tt[sapply(tt,is.na)] <- 9
  HotelData[i,] <- tt
    } else if (HotelData[i,ncol(HotelData)]=='Passive'){
      tt <- HotelData[i,]
      tt[sapply(tt,is.na)] <- 8
      HotelData[i,] <- tt
    }else if(HotelData[i,ncol(HotelData)]=='Detractor'){
      tt <- HotelData[i,]
      tt[sapply(tt,is.na)] <- 6
      HotelData[i,] <- tt
    }
  }
}  
View(HotelData)

write.csv(HotelData, "Cleaned Hotel Data Set final.csv")
HotelData <- read.csv("Cleaned Hotel Data Set final.csv")
Cleaned.HotelDataSet <- na.omit(HotelData)

write.csv(HotelData, "Cleaned Hotel Data Set final 2.csv")

HotelData2 <- read.csv("Cleaned Hotel Data Set final 2.csv")
Cleaned.HotelDataSet2 <- na.omit(HotelData2)

write.csv(Cleaned.HotelDataSet2, "Cleaned Hotel Data Set final 3.csv")
View(Cleaned.HotelDataSet2)


any(is.null(Cleaned.HotelDataSet2$Restaurant_PL))

dat2 <- read.csv("Cleaned Hotel Data Set final 3.csv", header=T, na.strings=c("","NA"))
View(dat2)


for(i in 1:nrow(dat2)){
  {
    if(dat2[i,ncol(dat2)]=='Promoter'){
      tt <- dat2[i,]
      tt[sapply(tt,is.na)] <- Y
      dat2[i,] <- tt
    } else if (dat2[i,ncol(dat2)]=='Passive'){
      tt <- dat2[i,]
      tt[sapply(tt,is.na)] <- 8
      dat2[i,] <- tt
    }else if(dat2[i,ncol(dat2)]=='Detractor'){
      tt <- dat2[i,]
      tt[sapply(tt,is.na)] <- N
      dat2[i,] <- tt
    }
  }
}  

for(i in 1:nrow(dat2)){
  {
    if(dat2[i,ncol(dat2)]=='Promoter'){
      tt <- dat2[i,]
      tt[sapply(tt,is.na)] <- "Y"
      dat2[i,] <- tt
    } else if(dat2[i,ncol(dat2)]=='Detractor'){
      tt <- dat2[i,]
      tt[sapply(tt,is.na)] <- "N"
      dat2[i,] <- tt
    }
  }
}  

View(dat2)

dat2 <- na.omit(dat2)

write.csv(dat2, "Cleaned Hotel Data Set final 5.csv")
