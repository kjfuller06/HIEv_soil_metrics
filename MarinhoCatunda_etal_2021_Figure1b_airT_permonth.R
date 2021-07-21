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

#air temp data for plotting
airT<-abv[abv$SensorType=="AirT",]
airT<-airT[airT$Position=="In",]
airT = airT %>% 
  dplyr::select(-Plot,
                -Treatment)

# combine
airT$month = format(airT$DateTime, format = "%b")
airT = airT[order(airT$month),]
backup = airT
airT<-aggregate(data=airT,value~month,FUN=function(x) c(avg=mean(x), minT = min(x), maxT = max(x)),simplify=TRUE,drop=TRUE)
val<-data.frame(airT[["value"]])
airT$value<-val$avg
airT$minT<-val$minT
airT$maxT = val$maxT
airT = airT %>% 
  pivot_longer(cols = c("value", "minT", "maxT"))
airT$month = factor(airT$month, levels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))
airT = airT[order(airT$month),]
airT$name = factor(airT$name, levels = c("maxT", "value", "minT"))

# plot
tiff(file = "Figure1b_AirT_permonth.tiff", width =1100, height = 900, units = "px", res = 200)

ggplot(data=airT, aes(x=month, y=value, group = name)) +
  geom_line(aes(linetype = name)) +
  geom_point(aes(shape = name)) +
  scale_shape_manual(values = c(1, 16, 1)) +
  scale_linetype_manual(values = c("dashed", "solid", "dashed")) +
  labs(x="Month", y = expression(paste("Mean Air Temperature (",degree~C,")"))) +
  theme_classic() +
  theme(legend.position = "none") 
dev.off()

# violin test
backup$month = factor(backup$month, levels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))
backup = backup[order(backup$month),]
tiff(file = "Figure1b_AirT_violin.tiff", width =1100, height = 700, units = "px", res = 200)
ggplot(backup, aes(x = month, y = value)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  labs(x="Month", y = expression(paste("Mean Air Temperature (",degree~C,")"))) +
  theme_classic()
dev.off()
