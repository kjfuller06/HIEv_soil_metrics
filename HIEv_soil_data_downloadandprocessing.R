#start date
sD<-as.Date("2018-06-01")
#end date
eD<-as.Date("2019-05-30")

#Irrigation####
Irrig<- (downloadTOA5("PACE_AUTO_ALL_IRRIG_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,4:6)]

#Rename columns
names(Irrig)<-c("DateTime","Plot","Treatment","Irrigation")
Irrig$Date<-as.Date(Irrig$DateTime)

#force Treatment to be a factor
Irrig$Treatment<-as.factor(Irrig$Treatment)
Irrig<-Irrig[order(Irrig$Date,Irrig$Treatment),]
Irrig<-aggregate(data=Irrig,Irrigation~Date+Treatment,FUN=mean)
levels(Irrig$Treatment)<-c("Con","Drt","Con","Drt")

#subset data by treatment
Irrig1<-subset(Irrig, Treatment == "Con")
Irrig2<-subset(Irrig[Irrig$Date>"2018-05-31"&Irrig$Date<"2018-12-01",], Treatment == "Drt")

#Soil Moisture####
#download data from HIEv and only keep soil moisture variables of interest
for(i in c(1:6)){
   s<-(downloadTOA5(paste("PACE_AUTO_S",i,"_BLWGRND_R_",sep=""), startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,3:26)]
   assign(paste("s",i,sep=""),s)
}

#convert to long form, add "shelter" variable
s1<-reshape2::melt(s1,id.vars="DateTime")
s1$Shelter<-1
s2<-reshape2::melt(s2,id.vars="DateTime")
s2$Shelter<-2
s3<-reshape2::melt(s3,id.vars="DateTime")
s3$Shelter<-3
s4<-reshape2::melt(s4,id.vars="DateTime")
s4$Shelter<-4
s5<-reshape2::melt(s5,id.vars="DateTime")
s5$Shelter<-5
s6<-reshape2::melt(s6,id.vars="DateTime")
s6$Shelter<-6

#combine into one DF
soil<-rbind(s1,s2,s3,s4,s5,s6)
names(soil)<-c("DateTime","SensorCode","value","Shelter")
sensors<-read.csv("soilsensors.csv")
soil<-merge(soil,sensors,by=c("Shelter","SensorCode"))
soil$Treat<-as.factor(paste(soil$Position,soil$Treatment,sep=""))

#change class of variables
soil$Sp<-as.factor(soil$Sp)
soil$Plot<-as.factor(soil$Plot)
soil$Shelter<-as.factor(soil$Shelter)
soil$Date<-as.Date(soil$DateTime)

#remove NAs
soil<-na.omit(soil)

#Summarize data
soil<-aggregate(data=soil,value~SensorType+Sp+Date+Position+Treat+Shelter+Plot,FUN=mean,simplify=TRUE,drop=TRUE)
CSM<-soil
backup<-soil
soil<-aggregate(data=soil,value~SensorType+Sp+Date+Position+Treat,FUN=function(x) c(avg=mean(x),upper=mean(x)+sd(x)/sqrt(length(x)),lower=mean(x)-sd(x)/sqrt(length(x))),simplify=TRUE,drop=TRUE)

#spit the aggregate function outputs into a DF and reassign so they are variables in the dataframe
##otherwise the output is a list within soil
val<-data.frame(soil[["value"]])
soil$value<-val$avg
soil$upper<-val$upper
soil$lower<-val$lower

#separate data frame by data type and species
RSM<-soil[soil$SensorType=="TDR",]
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



