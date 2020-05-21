# #******MAKE SURE LIBRARY IS IN THE C: DRIVE*******
# #****************set token below******************
# #load packages
library(devtools)
library(data.table)
install_bitbucket("remkoduursma/HIEv")
library(HIEv)
# setToken("")
library(reshape2)
library(tidyverse)

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
CSM<-soil[soil$SensorType=="TDR",]
CSM$value<-CSM$value*100
backup1<-soil
soil<-aggregate(data=soil,value~SensorType+Sp+Date+Position+Treat,FUN=function(x) c(avg=mean(x),upper=mean(x)+sd(x)/sqrt(length(x)),lower=mean(x)-sd(x)/sqrt(length(x))),simplify=TRUE,drop=TRUE)

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

#Calculate differences and format data####
#backup dataframe, select only soil moisture data and combine the position and treatment columns
CSM<-backup1
CSM<-CSM[CSM$SensorType=="TDR",]
CSM<-subset(CSM, select=-c(Position,Plot))

#convert to long form
CSM<-spread(CSM,Treat,value)

#split data back up for calculations
n<-levels(CSM$Sp)
count=1
for(i in n){
   x<-CSM[CSM$Sp==i,]
   assign(paste("df",count,sep=""),x)
   count<-count+1
}

#Calculate the difference between treatments
#Bis
df1$UpperAmbDrt<-df1$UpperAmbDrt-df1$UpperAmbCon
df1$UpperAmbCon<-0
df1<-subset(df1, select=c(Sp,Date,Shelter,UpperAmbCon,UpperAmbDrt))

#Fes
df2$UpperAmbDrt<-df2$UpperAmbDrt-df2$UpperAmbCon
df2$UpperEleCon<-df2$UpperEleCon-df2$UpperAmbCon
df2$UpperEleDrt<-df2$UpperEleDrt-df2$UpperAmbCon
df2$UpperAmbCon<-0
df2<-subset(df2, select=c(Sp,Date,Shelter,UpperAmbCon,UpperAmbDrt,UpperEleCon,UpperEleDrt))

#Luc
df3$UpperAmbDrt<-df3$UpperAmbDrt-df3$UpperAmbCon
df3$UpperEleCon<-df3$UpperEleCon-df3$UpperAmbCon
df3$UpperEleDrt<-df3$UpperEleDrt-df3$UpperAmbCon
df3$UpperAmbCon<-0
df3$LowerAmbDrt<-df3$LowerAmbDrt-df3$LowerAmbCon
df3$LowerEleCon<-df3$LowerEleCon-df3$LowerAmbCon
df3$LowerEleDrt<-df3$LowerEleDrt-df3$LowerAmbCon
df3$LowerAmbCon<-0
df3<-subset(df3, select=-c(SensorType))

#Rye
df4$UpperAmbDrt<-df4$UpperAmbDrt-df4$UpperAmbCon
df4$UpperAmbCon<-0
df4<-subset(df4, select=c(Sp,Date,Shelter,UpperAmbCon,UpperAmbDrt))

#convert back to longform
dfs<-list(df1,df2,df3,df4)
count=1
for(i in c(1:4)){
   x<-melt(dfs[[i]], id.vars=c("Sp","Date","Shelter"))
   #calculate the variance
   x<-aggregate(data=x,value~Date+variable,FUN=function(x) c(avg=mean(x),upper=mean(x)+sd(x)/sqrt(length(x)),lower=mean(x)-sd(x)/sqrt(length(x))),simplify=TRUE,drop=TRUE)
   #spit the aggregate function outputs into a DF and reassign so they are variables in the dataframe
   ##otherwise the output is a list within df1
   val<-data.frame(x[["value"]])
   x$value<-val$avg
   x$upper<-val$upper
   x$lower<-val$lower
   assign(paste("df",count,sep=""),x)
   count<-count+1
}

#rename for convenience
CBIS<-df1
CFES<-df2
CLUC<-df3
CRYE<-df4

#split data back up for graphing
CBIS<-split(CBIS,CBIS$variable)
CFES<-split(CFES,CFES$variable)
CLUC<-split(CLUC,CLUC$variable)
CRYE<-split(CRYE,CRYE$variable)

