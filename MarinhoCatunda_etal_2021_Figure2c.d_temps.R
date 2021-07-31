#start date
#start date
sD<-as.Date("2018-04-01")
#end date
eD<-as.Date("2020-03-31")

#Aboveground data####
abv = read.csv("data/MarinhoCatunda_Fig2_AirT_raw.csv")
sensors<-read.csv("abovegroundsensors.csv")
abv<-merge(abv,sensors,by=c("Shelter","SensorCode"))

#change class of variables
abv$Plot<-as.factor(abv$Plot)
abv$Shelter<-as.factor(abv$Shelter)
abv$Date<-as.Date(abv$DateTime)

### dates are still fucked
#air temp data for plotting
airT<-abv[abv$SensorType=="AirT",]
airT<-subset(airT,select=-c(Treatment,Plot,SensorType,SensorCode))
airT<-na.omit(airT)
airT<-airT[airT$Position=="In",]
airT1<-airT %>% 
  dplyr::filter(DateTime != as.POSIXct("2018-10-18 14:05:00", tz="UTC")&DateTime != as.POSIXct("2018-10-18 14:20:00", tz="UTC")&DateTime != as.POSIXct("2018-10-18 14:25:00", tz="UTC")) %>% 
  dplyr::filter(Date != "2018-04-20") %>% 
  dplyr::filter(Date != "2018-10-18")
airT2 = airT %>% 
  filter(Date == as.Date("2018-04-20") | Date == as.Date("2018-10-18")) %>% 
  filter(value >= 0)
airT = airT[-(airT$Date == as.Date("2018-04-20") & airT$value < 0),]
  dplyr::filter(-(Date == as.Date("2018-10-18") & value == 0))
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
max$month = format(as.Date(max$Date), format = "%b")
max<-split(max,droplevels(as.factor(max$Treatment)))

min<-subset(surf,select=c(-value,-maxT))
min<-split(min,droplevels(as.factor(min$Treatment)))

