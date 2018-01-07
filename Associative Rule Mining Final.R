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
