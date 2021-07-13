#start date
sD<-as.Date("2019-06-01")
#end date
eD<-as.Date("2019-11-30")

#Irrigation####
Irrig<- (downloadTOA5("PACE_AUTO_ALL_IRRIG_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,4:6)]

#Rename columns
names(Irrig)<-c("DateTime","Plot","Treatment","Irrigation")
Irrig$month = format(Irrig$DateTime, format = "%b")
Irrig$month = as.factor(Irrig$month)
Irrig$month = factor(Irrig$month, levels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))

#force Treatment to be a factor
Irrig$Treatment<-as.factor(Irrig$Treatment)
levels(Irrig$Treatment)<-c("Control","Drought","Control","Drought")
Irrig<-aggregate(data=Irrig,Irrigation~month+Treatment+Plot,FUN=sum)
Irrig<-aggregate(data=Irrig,Irrigation~month+Treatment,FUN=mean)

Irrig = Irrig[order(Irrig$month, Irrig$Treatment),]

# plot
tiff(file = paste("Figure1a_Irrig_",sD,"_",eD,".tiff",sep=""), width =1100, height = 900, units = "px", res = 200)

ggplot(data=Irrig, aes(x=month, y=Irrigation, fill=Treatment)) +
   geom_bar(stat="identity", width=0.75, color="black", position=position_dodge())+
   theme_classic() +
   scale_fill_manual(values=c('black','white')) +
   labs(x="Month", y = "Irrigation (mm)")

dev.off()

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
soil = soil %>% 
   filter(Position != "Lower" & SensorType == "TDR")
soil$water<-as.factor(substr(soil$Treatment, 4, 6))

#change class of variables
soil$Sp<-as.factor(soil$Sp)
soil$Plot<-as.factor(soil$Plot)
soil$Shelter<-as.factor(soil$Shelter)
soil$Date<-as.Date(soil$DateTime)
soil$month = format(soil$Date, format = "%b")

#remove NAs
soil<-na.omit(soil)

#Summarize data
backup1<-soil
soil<-aggregate(data=soil,value~Sp+month+water,FUN=function(x) c(avg=mean(x),upper=mean(x)+sd(x)/sqrt(length(x)),lower=mean(x)-sd(x)/sqrt(length(x))),simplify=TRUE,drop=TRUE)

#spit the aggregate function outputs into a DF and reassign so they are variables in the dataframe
##otherwise the output is a list within soil
val<-data.frame(soil[["value"]])
soil$value<-val$avg
soil$upper<-val$upper
soil$lower<-val$lower
soil$month = as.factor(soil$month)
soil$month = factor(soil$month, levels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))
soil = soil[order(soil$month),]

#separate data frame by data type and species
soil$value<-soil$value*100
soil$upper<-soil$upper*100
soil$lower<-soil$lower*100

for(i in levels(soil$Sp)){
    x<-soil[soil$Sp==i,]
    x<-droplevels(x)
    x<-x[order(x$month,x$water),]
    assign(paste(i,1,sep=""),x)
}

# plot- need clarification from Karen

tiff(file = paste("Figure1c_soilVWC_",sD,"_",eD,".tiff",sep=""), width =1100, height = 900, units = "px", res = 200)

ggplot(data=soil, aes(x=month, y=value, fill=water)) +
   geom_bar(stat="identity", width=0.75, color="black", position=position_dodge())+
   theme_classic() +
   scale_fill_manual(values=c('black','white')) +
   labs(x="Month", y = "Irrigation (mm)")

dev.off()