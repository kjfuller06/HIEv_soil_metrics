#start date
sD<-as.Date("2018-04-01")
#end date
eD<-as.Date("2020-03-31")

#Irrigation####
Irrig = read.csv("data/MarinhoCatunda_Fig2_Irrig_final.csv")
#subset data by treatment
Irrig$Date = as.Date(Irrig$Date)
Irrig1<-subset(Irrig, Treatment == "Con")
Irrig$month = format(Irrig$Date, format = "%b")
drought = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov")
Irrig2 = Irrig %>% 
   filter(month %in% drought) %>% 
   filter(Treatment == "Drt")
Irrig_luc = Irrig2 %>% 
    filter(Date > "2018-08-22")

#Soil Moisture####
#download data from HIEv and only keep soil moisture variables of interest
soil = read.csv("data/MarinhoCatunda_Fig2_soil_final.csv")

#separate data frame by data type and species
RSM<-soil[soil$SensorType=="TDR",]
RSM$value<-RSM$value*100
RSM$upper<-RSM$upper*100
RSM$lower<-RSM$lower*100
RSM$Sp = as.factor(RSM$Sp)
RSM$Treat = as.factor(RSM$Treat)
for(i in levels(RSM$Sp)){
    x<-RSM[RSM$Sp==i,]
    x<-droplevels(x)
    x<-x[order(x$Date,x$Treat),]
    assign(paste(i,1,sep=""),x)
}

#format data for visualisation####
#split data for graphing
FES1$Date = as.Date(FES1$Date)
rFES<-split(FES1,droplevels(FES1$Treat))
LUC1$Date = as.Date(LUC1$Date)
rLUC<-split(LUC1,droplevels(LUC1$Treat))
