HotelData <- read.csv(file.choose())
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
PercentageOFnaVAlues <- c(NaAll.Suites_PL, NaBell.Staff_PL, NaBoutique_PL, NaBusiness.Center_PL, NaCasino_PL, NaConference_PL, NaConvention_PL, NaDry.Cleaning_PL, NaElevators_PL, NaFitness.Center_PL, NaFitness.Trainer_PL, NaGolf_PL, NaLaundry_PL, NaLimo.Service_PL, NaMini.Bar_PL, NaPool.Indoor_PL, NaPool.Outdoor_PL, NaRegency.Grand.Club_PL, NaResort_PL, NaRestaurant_PL, NaSelf.Parking_PL, NaShuttle.Service_PL, NaSpa_PL, NaValet.Parking_PL)

naCategorical <- data.frame(Columns, PercentageOFnaVAlues)

naCategorical
