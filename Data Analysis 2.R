View(ModellingData)

### Count of Customers who visited Hotels in Florida and California ( Hotel Wise)

library(ggplot2)
HotelPlot <- ggplot(ModellingData, aes(x=Brand_PL)) + geom_bar(aes(fill=POV_CODE_C), position="dodge") + facet_grid(State_PL ~ .) + scale_fill_manual(values=c("red", "green"))
HotelPlot

#Hotel with Maximum Promoter and Detractor
HotelPromoter <- ggplot(ModellingData, aes(x=Brand_PL)) + geom_bar(aes(fill=NPS_Type), position="dodge") + facet_grid(State_PL ~ .) + scale_fill_manual(values=c("red", "green","blue"))
HotelPromoter

# Hotel With purpose
library(ggplot2)
HotelBusiness <- ggplot(ModellingData, aes(x=Brand_PL)) + geom_bar(aes(fill=POV_CODE_C), position="dodge")
HotelBusiness
# Effect of Good Customer Service

HotelCustomerService <- ggplot(ModellingData, aes(x=NPS_Type)) + geom_bar(aes(fill=Customer_SVC_H)) +scale_fill_manual(values=c("red"))
HotelCustomerService


#

Promoters <- ModellingData[which(ModellingData$NPS_Type=="Promoter"),]
View(Promoters)
CityPromoters <-ggplot(Promoters, aes(x=City_PL)) + geom_bar(aes(fill=X), position ="dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
CityPromoters
