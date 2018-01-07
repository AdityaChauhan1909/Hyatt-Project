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

