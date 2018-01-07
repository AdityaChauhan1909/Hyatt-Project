View(ModellingData)
ModellingData <- read.csv(file.choose())
### Count of Customers who visited Hotels in Florida and California ( Hotel Wise)

library(ggplot2)
HotelPlot <- ggplot(ModellingData, aes(x=Brand_PL)) + geom_bar(aes(fill=POV_CODE_C), position="dodge") + facet_grid(State_PL ~ .) + scale_fill_manual(values=c("red", "green"))
HotelPlot

#Hotel with Maximum Promoter and Detractor
HotelPromoter <- ggplot(ModellingData, aes(x=Brand_PL)) + geom_bar(aes(fill=NPS_Type), position="dodge") + facet_grid(State_PL ~ .) + scale_fill_manual(values=c("red", "green","blue"))
HotelPromoter

# How does having MiniBar Pool effect the promoter and detractor

HotelMini.Bar_PL <- ggplot(ModellingData, aes(x=NPS_Type)) + geom_bar(aes(fill=Mini.Bar_PL), position="dodge") + facet_grid(State_PL ~ .) + scale_fill_manual(values=c("red", "green","blue"))
HotelMini.Bar_PL

# How does having Shuttle.Service_PL effect the promoter and detractor

HotelShuttle.Service_PL <- ggplot(ModellingData, aes(x=NPS_Type)) + geom_bar(aes(fill=Shuttle.Service_PL), position="dodge") + facet_grid(State_PL ~ .) + scale_fill_manual(values=c("red", "green","blue"))
HotelShuttle.Service_PL

# How does having Regency.Grand.Club_PL effect the promoter and detractor
HotelRegency.Grand.Club_PL <- ggplot(ModellingData, aes(x=NPS_Type)) + geom_bar(aes(fill=Regency.Grand.Club_PL), position="dodge") + facet_grid(State_PL ~ .) + scale_fill_manual(values=c("red", "green","blue"))
HotelRegency.Grand.Club_PL


#####Hotel WISE NPS VALUE
install.packages("thePackage")
library(thePackage)

HotelNPS <- ggplot(ModellingData, aes(x=Brand_PL)) + geom_bar(aes(fill=NPS_Type, y=(..count..)/sum(..count..)), position="dodge")  + scale_y_continuous(labels = scales::percent) + ylab='Percentage'
HotelNPS


#Top 2 cities in USA with most number of customers
CityCustomers <-ggplot(ModellingData, aes(x=City_PL)) + geom_bar(aes(fill=X), position ="dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
CityCustomers

#Top 2 cities in USA with most number of Promoters
Promoters <- ModellingData[which(ModellingData$NPS_Type=="Promoter"),]
View(Promoters)
CityPromoters <-ggplot(Promoters, aes(x=City_PL)) + geom_bar(aes(fill=X), position ="dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
CityPromoters

# Cities with most number of detractors
Detractor <- ModellingData[which(ModellingData$NPS_Type=="Detractor"),]
View(Detractor)
CityDetractor <-ggplot(Detractor, aes(x=City_PL)) + geom_bar(aes(fill=CHECKOUT_HEADER_ID_C), position ="dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
CityDetractor


#State the facilities or services used by most customers in the hotel with maximum NPS

HyattPlace <-ModellingData[which(ModellingData$Brand_PL=='Hyatt Place'),]
View(HyattPlace)

### FOR SPA

install.packages("plotrix")
library(plotrix )
spa_n <- length(ModellingData$Likelihood_Recommend_H[((ModellingData$Likelihood_Recommend_H) < 8 ) & ((ModellingData$POV_CODE_C)=="LEISURE") & ((ModellingData$Spa_PL)=="N")])
spa_y <-length(ModellingData$Likelihood_Recommend_H[((ModellingData$Likelihood_Recommend_H) < 8 ) & ((ModellingData$POV_CODE_C)=="LEISURE") & ((ModellingData$Spa_PL)=="Y")])
pies <- c(spa_n,spa_y)
labels <- c("N","Y")
par(mar = rep(2, 4))
pct <- round(pies/sum(pies)*100)
lbls <- paste(labels, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie3D(pies, labels=lbls,explode=0.1, col = rainbow(length(pies)))

# FOR Shuttle Service
# Promoter and Shuttle Service # Business Purpose
Shuttle.Service_PL_n <- length(ModellingData$Likelihood_Recommend_H[((ModellingData$Likelihood_Recommend_H) > 8 ) & ((ModellingData$POV_CODE_C)=="BUSINESS") &   ((ModellingData$Shuttle.Service_PL)=="N")])
Shuttle.Service_PL_y <-length(ModellingData$Likelihood_Recommend_H[((ModellingData$Likelihood_Recommend_H) > 8 )  & ((ModellingData$POV_CODE_C)=="BUSINESS") & ((ModellingData$Shuttle.Service_PL)=="Y")])
piesShuttle.Service_PL <- c(Shuttle.Service_PL_n,Shuttle.Service_PL_y)
labels <- c("N","Y")
par(mar = rep(2, 4))
pctpiesShuttle.Service_PL <- round(piesShuttle.Service_PL/sum(piesShuttle.Service_PL)*100)
lblspiesShuttle.Service_PL <- paste(labels, pctpiesShuttle.Service_PL) # add percents to labels 
lblspiesShuttle.Service_PL <- paste(lblspiesShuttle.Service_PL,"%",sep="") # ad % to labels 
pie3D(piesShuttle.Service_PL, labels=lblspiesShuttle.Service_PL,explode=0.1, col = rainbow(length(piesShuttle.Service_PL)))


####Mini.Bar_PL
## Detractor 
Mini.Bar_PL_n <- length(ModellingData$Likelihood_Recommend_H[((ModellingData$Likelihood_Recommend_H) < 8 ) & ((ModellingData$POV_CODE_C)=="LEISURE") &   ((ModellingData$Mini.Bar_PL)=="N")])
Mini.Bar_PL_y <-length(ModellingData$Likelihood_Recommend_H[((ModellingData$Likelihood_Recommend_H) < 8 )  & ((ModellingData$POV_CODE_C)=="LEISURE") & ((ModellingData$Mini.Bar_PL)=="Y")])
piesMini.Bar_PL <- c(Mini.Bar_PL_n,Mini.Bar_PL_y)
labels <- c("N","Y")
par(mar = rep(2, 4))
pctpiesMini.Bar_PL <- round(piesMini.Bar_PL/sum(piesMini.Bar_PL)*100)
lblsMini.Bar_PL <- paste(labels, pctpiesMini.Bar_PL) # add percents to labels 
lblsMini.Bar_PL <- paste(lblsMini.Bar_PL,"%",sep="") # ad % to labels 
pie3D(piesMini.Bar_PL, labels=lblsMini.Bar_PL,explode=0.1, col = rainbow(length(piesMini.Bar_PL)))

#### Valet.Parking_PL
######Promoter
Valet.Parking_PL_n <- length(ModellingData$Likelihood_Recommend_H[((ModellingData$Likelihood_Recommend_H) > 8 )  &   ((ModellingData$Valet.Parking_PL)=="N")])
Valet.Parking_PL_y <-length(ModellingData$Likelihood_Recommend_H[((ModellingData$Likelihood_Recommend_H) > 8 )  & ((ModellingData$Valet.Parking_PL)=="Y")])
piesValet.Parking_PL <- c(Valet.Parking_PL_n,Valet.Parking_PL_y)
labels <- c("N","Y")
par(mar = rep(2, 4))
pctValet.Parking_PL <- round(piesValet.Parking_PL/sum(piesValet.Parking_PL)*100)
lblsValet.Parking_PL <- paste(labels, pctValet.Parking_PL) # add percents to labels 
lblsValet.Parking_PL <- paste(lblsValet.Parking_PL,"%",sep="") # ad % to labels 
pie3D(piesValet.Parking_PL, labels=lblsValet.Parking_PL,explode=0.1, col = rainbow(length(piesValet.Parking_PL)))



################

