#start date
sD<-as.Date("2019-06-01")
#end date
eD<-as.Date("2019-11-30")

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
airT$month = format(airT$Date, format = "%b")
airT$month = as.factor(airT$month)
airT$month = factor(airT$month, levels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))
airT = airT[order(airT$month),]
airT<-aggregate(data=airT,value~month,FUN=function(x) c(avg=mean(x),maxT=max(x),minT=min(x)),simplify=TRUE,drop=TRUE)
val<-data.frame(airT[["value"]])
airT$value<-val$avg
airT$maxT<-val$maxT
airT$minT<-val$minT

ymax = max(airT$maxT)

# plot
with(airT, barplot(value ~ month,
                   ylab = expression(paste("Mean Air Temperature (",degree~C,")")),
                   xlab = "Month",
                   ylim = c(0, ymax),
                   col = "black"))

tiff(file = paste("Figure1b_AirT_",sD,"_",eD,".tiff",sep=""), width =1100, height = 900, units = "px", res = 200)
ggplot(data=airT, aes(x=month, y=value)) +
  geom_bar(stat="identity", width=0.4, fill="black") +
  theme_classic() +
  labs(x="Month", y = expression(paste("Mean Air Temperature (",degree~C,")"))) +
  ylim(0, 25)
dev.off()
