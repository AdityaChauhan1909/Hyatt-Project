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

abline(Model3AndCheckIn)
