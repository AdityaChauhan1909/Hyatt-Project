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

# Creating Training and Test Dataset
random.indexes <- sample(1:nrow(ModellingData2),replace=TRUE)
cutpoint2_3 <- floor(nrow(ModellingData2) /3 * 2)
ModellingData.Train <- ModellingData2[random.indexes[1:cutpoint2_3],]
ModellingData.Test <- ModellingData2[random.indexes[cutpoint2_3 + 1: nrow(ModellingData2)],]
ModellingData.Test <- na.omit(ModellingData.Test)
View(ModellingData.Train)
View(ModellingData.Test)


# Converting all the columns to factors


for (i in 1:ncol(ModellingData.Train)){
  ModellingData.Train[,i] <- as.factor(ModellingData.Train[,i])
  
}


for (i in 1:ncol(ModellingData.Test)){
  ModellingData.Test[,i] <- as.factor(ModellingData.Test[,i])
  
}
# Creating KSVM using all variables

svmOutput.All <- ksvm(Likelihood_Recommend_H ~.,  data=ModellingData.Train, kernel="rbfdot", kpar = "automatic", C= 10, cross= 10, prob.model=TRUE)
summary(svmOutput.All)
svmOutput.All

KsvmPred.All <- predict(svmOutput.All,ModellingData.Test, type="votes")
KsvmPred.All
View(KsvmPred.All)
Rounded.KsvmPred.All <- round(KsvmPred.All)

Rounded.KsvmPred.All <- as.vector(Rounded.KsvmPred.All)
#Creating Data Frame of Actual and Predicted Value
CompTable1.All <- data.frame(ModellingData.Test$Likelihood_Recommend_H, Rounded.KsvmPred.All)
View(CompTable1.All)
#Renaming column names
colnames(CompTable1.All) <- c("Test", "Predicted")
View(CompTable1.All)
results.All <- table(CompTable1.All)
results.All
CompTableAll.ex<- CompTable1.All
CompTableAll.ex$TestNPS <- NA
CompTableAll.ex$PredictedNPS <- NA
# Putting NPS Type for Test Column on the basis of Value

for(i in 1:nrow(CompTableAll.ex)){
  if(CompTableAll.ex[i,1] >= 9){
    CompTableAll.ex[i,3] <- " Promoter"
  } else if (CompTableAll.ex[i,1]==7 | CompTableAll.ex[i,1]==8){
    CompTableAll.ex[i,3] <- " Passive"
  } else{
    CompTableAll.ex[i,3] <- "Detractor"
  }
}

View(CompTableAll.ex)

# Putting NPS Type for Predicted Column on the basis of Value

for(i in 1:nrow(CompTableAll.ex)){
  if(CompTableAll.ex[i,2] >= 9){
    CompTableAll.ex[i,4] <- " Promoter"
  } else if (CompTableAll.ex[i,2]==7 | CompTableAll.ex[i,2]==8){
    CompTableAll.ex[i,4] <- " Passive"
  } else{
    CompTableAll.ex[i,4] <- "Detractor"
  }
}

View(CompTableAll.ex)

# Creating a New Data Frame of the Predicted NPS TYPE 

CompTable1All.NPS <- data.frame(CompTableAll.ex$TestNPS,CompTableAll.ex$PredictedNPS)
View(CompTable1All.NPS)
head(CompTable1All.NPS)
colnames(CompTable1All.NPS) <- c("TestNPS","PredictedNPS")
resultsAll.NPS <- table(CompTable1All.NPS)
resultsAll.NPS
AccuracyKSVM.All <- ((resultsAll.NPS[1,1]+resultsAll.NPS[2,2]+resultsAll.NPS[3,3])/(resultsAll.NPS[1,1]+resultsAll.NPS[1,2]+resultsAll.NPS[1,3]+resultsAll.NPS[2,1]+resultsAll.NPS[2,2]+resultsAll.NPS[2,3]+resultsAll.NPS[3,1]+resultsAll.NPS[3,2]+resultsAll.NPS[3,3])) * 100
AccuracyKSVM.All






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








