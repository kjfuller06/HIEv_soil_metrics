#start date
sD<-as.Date("2018-04-01")
#end date
eD<-as.Date("2020-03-31")

#Irrigation####
Irrig = read.csv("data/MarinhoCatunda_Fig2_Irrig_raw.csv")
Irrig$Date<-as.Date(Irrig$DateTime)

#force Treatment to be a factor
Irrig$Treatment<-as.factor(Irrig$Treatment)
Irrig<-Irrig[order(Irrig$Date,Irrig$Treatment),]
Irrig<-aggregate(data=Irrig,Irrigation~Date+Treatment,FUN=mean)
levels(Irrig$Treatment)<-c("Con","Drt","Con","Drt")

#subset data by treatment
Irrig1<-subset(Irrig, Treatment == "Con")
Irrig$Date = as.Date(Irrig$Date)
Irrig$month = format(Irrig$Date, format = "%b")
drought = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov")
Irrig2 = Irrig %>% 
   filter(month %in% drought) %>% 
   filter(Treatment == "Drt")

#Soil Moisture####
#download data from HIEv and only keep soil moisture variables of interest
soil = read.csv("data/MarinhoCatunda_Fig2_soil_raw.csv")
sensors<-read.csv("soilsensors.csv")
soil<-merge(soil,sensors,by=c("Shelter","SensorCode"))
soil$Treat<-as.factor(paste(soil$Position,soil$Treatment,sep=""))

#change class of variables
soil$Sp<-as.factor(soil$Sp)
soil = soil %>% 
   filter(Sp == "FES" | Sp == "LUC") %>% 
   filter(Position == "Upper")
soil$Plot<-as.factor(soil$Plot)
soil$Shelter<-as.factor(soil$Shelter)
soil$Date<-as.Date(soil$DateTime)

#remove NAs
soil<-na.omit(soil)

#Summarize data
soil<-aggregate(data=soil,value~SensorType+Sp+Date+Position+Treat+Shelter+Plot,FUN=mean,simplify=TRUE,drop=TRUE)
backup1<-soil
soil<-aggregate(data=soil,value~SensorType+Sp+Date+Position+Treat,FUN=function(x) c(avg=mean(x),upper=mean(x)+((sd(x)/sqrt(length(x)))*qt(p=0.05/2, df=(length(x) - 1),lower.tail=F)),lower=mean(x)-((sd(x)/sqrt(length(x)))*qt(p=0.05/2, df=(length(x) - 1),lower.tail=F))),simplify=TRUE,drop=TRUE)

#spit the aggregate function outputs into a DF and reassign so they are variables in the dataframe
##otherwise the output is a list within soil
val<-data.frame(soil[["value"]])
soil$value<-val$avg
soil$upper<-val$upper
soil$lower<-val$lower

#separate data frame by data type and species
RSM<-soil[soil$SensorType=="TDR",]
RSM$value<-RSM$value*100
RSM$upper<-RSM$upper*100
RSM$lower<-RSM$lower*100
for(i in levels(RSM$Sp)){
    x<-RSM[RSM$Sp==i,]
    x<-droplevels(x)
    x<-x[order(x$Date,x$Treat),]
    assign(paste(i,1,sep=""),x)
}
RST<-soil[soil$SensorType=="Temp",]
for(i in c("LUC","FES")){
   x<-RST[RST$Sp==i,]
   x<-droplevels(x)
   x<-x[order(x$Date,x$Treat),]
   assign(paste(i,2,sep=""),x)
}

#format data for visualisation####
#split data for graphing
rBIS<-split(BIS1,droplevels(BIS1$Treat))
rFES<-split(FES1,droplevels(FES1$Treat))
rLUC<-split(LUC1,droplevels(LUC1$Treat))
rRYE<-split(RYE1,droplevels(RYE1$Treat))
