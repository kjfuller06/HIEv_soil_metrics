#start date
sD<-as.Date("2018-05-21")
#end date
eD<-as.Date("2019-05-25")

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
df1<-rbind(s1,s2,s3,s4,s5,s6)
names(df1)<-c("DateTime","SensorCode","value","Shelter")
sensors<-read.csv("sensors.csv")
df1<-merge(df1,sensors,by=c("Shelter","SensorCode"))

#change class of variables
df1$Sp<-as.factor(df1$Sp)
df1$Plot<-as.factor(df1$Plot)
df1$Shelter<-as.factor(df1$Shelter)
df1$Date<-as.Date(df1$DateTime)

#remove NAs
df1<-na.omit(df1)

#Summarize data
df1<-aggregate(data=df1,value~SensorType+Sp+Date+Position+Treatment+Shelter+Plot,FUN=mean,simplify=TRUE,drop=TRUE)
CSM<-df1
backup<-df1
df1<-aggregate(data=df1,value~SensorType+Sp+Date+Position+Treatment,FUN=function(x) c(avg=mean(x),upper=mean(x)+sd(x)/sqrt(length(x)),lower=mean(x)-sd(x)/sqrt(length(x))),simplify=TRUE,drop=TRUE)

#spit the aggregate function outputs into a DF and reassign so they are variables in the dataframe
##otherwise the output is a list within df1
val<-data.frame(df1[["value"]])
df1$value<-val$avg
df1$upper<-val$upper
df1$lower<-val$lower

#separate data frame by data type and species
RSM<-df1[df1$SensorType=="TDR",]
for(i in levels(RSM$Sp)){
    x<-RSM[RSM$Sp==i,]
    x<-x[order(x$Date,x$Treatment),]
    assign(paste(i,1,sep=""),x)
}
RST<-df1[df1$SensorType=="Temp",]
for(i in c("LUC","FES")){
   x<-RST[RST$Sp==i,]
   x<-x[order(x$Date,x$Treatment),]
   assign(paste(i,2,sep=""),x)
}



