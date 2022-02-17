#start date
sD<-as.Date("2019-05-01")
#end date
eD<-as.Date("2019-11-30")

#Irrigation####
# Irrig<- (downloadTOA5("PACE_AUTO_ALL_IRRIG_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,4:6)]
# 
# #Rename columns
# names(Irrig)<-c("DateTime","Plot","Treatment","Irrigation")
# Irrig$Date<-as.Date(Irrig$DateTime)
# 
# #force Treatment to be a factor
# Irrig$Treatment<-as.factor(Irrig$Treatment)
# Irrig<-Irrig[order(Irrig$Date,Irrig$Treatment),]
# Irrig<-aggregate(data=Irrig,Irrigation~Date+Treatment,FUN=mean)
# levels(Irrig$Treatment)<-c("Con","Drt","Con","Drt")
# 
# #subset data by treatment
# Irrig1<-subset(Irrig, Treatment == "Con")
# Irrig2<-subset(Irrig[Irrig$Date>"2018-05-31"&Irrig$Date<"2018-12-01",], Treatment == "Drt")

#Aboveground data####
#download data from HIEv and only keep variables of interest
for(i in c(1:6)){
   s<-(downloadTOA5(paste("PACE_AUTO_S",i,"_ABVGRND_R_",sep=""), startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,4,6,7,12:15)]
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
abv<-rbind(s1,s2,s3,s4,s5,s6)
names(abv)<-c("DateTime","SensorCode","value","Shelter")
sensors<-read.csv("abovegroundsensors.csv")
abv<-merge(abv,sensors,by=c("Shelter","SensorCode"))

#change class of variables
abv$Plot<-as.factor(abv$Plot)
abv$Shelter<-as.factor(abv$Shelter)
abv$Date<-as.Date(abv$DateTime)

#air temp data for plotting
airT<-abv[abv$SensorType=="AirT",]
airT<-subset(airT,select=-c(Treatment,Plot,SensorType,SensorCode))
airT<-na.omit(airT)
airT<-airT[airT$Position=="In",]
airT<-airT %>% 
  dplyr::filter(DateTime != as.POSIXct("2018-10-18 14:05:00", tz="UTC")&DateTime != as.POSIXct("2018-10-18 14:20:00", tz="UTC")&DateTime != as.POSIXct("2018-10-18 14:25:00", tz="UTC"))
airT<-aggregate(data=airT,value~Date,FUN=function(x) c(avg=mean(x),maxT=max(x),minT=min(x)),simplify=TRUE,drop=TRUE)
val<-data.frame(airT[["value"]])
airT$value<-val$avg
airT$maxT<-val$maxT
airT$minT<-val$minT

#processing for surface temperature data####
surf<-abv[abv$SensorType=="SurfaceTemp",]
surf<-na.omit(surf)
#Calculate surface temperature mean and extremes
surf<-aggregate(data=surf,value~Date+Treatment+Shelter+Plot,FUN=function(x) c(avg=mean(x),maxT=max(x),minT=min(x)),simplify=TRUE,drop=TRUE)
backup3<-surf
#spit the aggregate function outputs into a DF and reassign so they are variables in the dataframe
##otherwise the output is a list within surf
val<-data.frame(surf[["value"]])
surf$value<-val$avg
surf$maxT<-val$maxT
surf$minT<-val$minT

#summarise by treatment
vars<-c("value","maxT","minT")
surf<-lapply(vars,function(x){
   aggregate(surf[x],by=c(surf['Date'],surf['Treatment']),FUN=mean, simplify=TRUE,drop=TRUE)
})
backup4<-surf

#bind outputs into useable form
surf2<-merge(as.data.frame(surf[[1]]),as.data.frame(surf[[2]]),by=c("Date","Treatment"))
surf<-merge(surf2,as.data.frame(surf[[3]]),by=c("Date","Treatment"))

#format for graphing
max<-subset(surf,select=c(-value,-minT))
max<-split(max,droplevels(as.factor(max$Treatment)))

min<-subset(surf,select=c(-value,-maxT))
min<-split(min,droplevels(as.factor(min$Treatment)))

