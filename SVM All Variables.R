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


#############################################SVM For all Variables####################3

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




