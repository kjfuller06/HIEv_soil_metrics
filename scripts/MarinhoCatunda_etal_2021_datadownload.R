### Figure 1 ####
#start date
sD<-as.Date("2019-06-01")
#end date
eD<-as.Date("2019-11-30")

# irrigation ####
Irrig<- (downloadTOA5("PACE_AUTO_ALL_IRRIG_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,4:6)]
names(Irrig)<-c("DateTime","Plot","Treatment","Irrigation")
write.csv(Irrig, "data/MarinhoCatunda_Fig1_Irrig_raw.csv", row.names = FALSE)

# process
Irrig = read.csv("data/MarinhoCatunda_Fig1_Irrig_raw.csv")
Irrig$Shelter = substr(Irrig$Plot,1 ,2)
Irrig$Plot = paste0("P", substr(Irrig$Plot, 4, 4))
plots = read.csv("PACE_treatment_species_reference.csv")
plots$Shelter = substr(plots$Subplot.ID, 1, 2)
plots$Plot = substr(plots$Subplot.ID, 3, 4)
Irrig = left_join(Irrig, plots)
Irrig$DateTime = as.POSIXct(Irrig$DateTime)
Irrig$month = format(Irrig$DateTime, format = "%b")

#force Treatment to be a factor
Irrig$Treatment<-as.factor(Irrig$Treatment)
levels(Irrig$Treatment)<-c("Control","Drought","Control","Drought")
Irrig<-aggregate(data=Irrig,Irrigation~month+Treatment,FUN=mean)

Irrig$month = factor(Irrig$month, levels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))
Irrig = Irrig[order(Irrig$month, Irrig$Treatment),]
write.csv(Irrig, "data/MarinhoCatunda_Fig1_Irrig_final.csv", row.names = FALSE)

# soil moisture ####
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
write.csv(soil, "data/MarinhoCatunda_Fig1_soil_raw.csv", row.names = FALSE)

# process
soil = read.csv("data/MarinhoCatunda_Fig1_soil_raw.csv")
sensors<-read.csv("soilsensors.csv")
soil<-merge(soil,sensors,by=c("Shelter","SensorCode"))
soil = soil %>% 
  filter(Position != "Lower" & SensorType == "TDR")
soil$water<-as.factor(substr(soil$Treatment, 4, 6))
soil$code = paste0("S", soil$Shelter, "P", soil$Plot)
soil = soil %>% 
  dplyr::select(-SensorCode,
                -SensorType,
                -Treatment,
                -Position)
soil = soil[order(soil$DateTime, soil$Shelter, soil$water),]
soil<-aggregate(data=soil,value~Shelter+DateTime+Sp+water,FUN=mean)

#change class of variables
soil$Sp<-as.factor(soil$Sp)
soil$Shelter<-as.factor(soil$Shelter)
soil$DateTime= as.POSIXct(soil$DateTime)
soil$month = format(soil$DateTime, format = "%b")
soil = soil[order(soil$DateTime),]

#remove NAs
soil<-na.omit(soil)

#Summarize data
backup1<-soil
soil$value = soil$value*100
soil<-aggregate(data=soil,value~Sp+month+water,FUN=mean)
soil$month = factor(soil$month, levels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))
soil = soil[order(soil$month),]
soil$group = as.factor(paste0(soil$Sp, soil$water))
soil2 = soil
soil2$Sp = as.factor(soil2$Sp)
levels(soil2$Sp) = c("Bis", "Fes", "Med", "Lol")
soil2$Sp = factor(soil2$Sp, levels = c("Bis", "Fes", "Lol", "Med"))
names(soil2)[1] = "Species"
soil = soil %>% 
  dplyr::select(-Sp,
                -water) %>% 
  pivot_wider(names_from = group, values_from = value)
write.csv(soil, "data/MarinhoCatunda_Fig1_soil_final.csv", row.names = FALSE)
write.csv(soil2, "data/MarinhoCatunda_Fig1_soillong_final.csv", row.names = FALSE)

# air temperature ####
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
write.csv(abv, "data/MarinhoCatunda_Fig1_AirT_raw.csv", row.names = FALSE)
rm(list = ls())

abv = read.csv("data/MarinhoCatunda_Fig1_AirT_raw.csv")
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
airT$DateTime = as.POSIXct(airT$DateTime)
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
write.csv(airT, "data/MarinhoCatunda_Fig1_AirT_final.csv",row.names = FALSE)

### Figure 2 ####
#start date
sD<-as.Date("2018-04-01")
#end date
eD<-as.Date("2020-03-31")

# irrigation ####
Irrig<- (downloadTOA5("PACE_AUTO_ALL_IRRIG_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,4:6)]
names(Irrig)<-c("DateTime","Plot","Treatment","Irrigation")
write.csv(Irrig, "data/MarinhoCatunda_Fig2_Irrig_raw.csv", row.names = FALSE)

# process
Irrig = read.csv("data/MarinhoCatunda_Fig2_Irrig_raw.csv")
Irrig$Date<-as.Date(Irrig$DateTime)

#force Treatment to be a factor
Irrig$Treatment<-as.factor(Irrig$Treatment)
Irrig<-Irrig[order(Irrig$Date,Irrig$Treatment),]
Irrig<-aggregate(data=Irrig,Irrigation~Date+Treatment,FUN=mean)
levels(Irrig$Treatment)<-c("Con","Drt","Con","Drt")
write.csv(Irrig, "data/MarinhoCatunda_Fig2_Irrig_final.csv", row.names = FALSE)

# soil moisture ####
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
write.csv(soil, "data/MarinhoCatunda_Fig2_soil_raw.csv", row.names = FALSE)

# process
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
write.csv(soil, "data/MarinhoCatunda_Fig2_soil_final.csv", row.names = FALSE)

# surface and air temperatures ####
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
write.csv(abv, "data/MarinhoCatunda_Fig2_AirT_raw.csv", row.names = FALSE)

# process
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
airT$ID = c(1:nrow(airT))
airT$month = as.factor(substr(airT$Date, 6, 7))
airT1 = airT[(airT$month == "04" & airT$value < 1),]
airT1 = rbind(airT1, airT[(airT$month == "09" & airT$value < 1),])
airT1 = rbind(airT1, airT[(airT$month == "10" & airT$value < 1),])
airT = airT[!(airT$ID %in% airT1$ID),]
airT<-aggregate(data=airT,value~Date,FUN=function(x) c(avg=mean(x),maxT=max(x),minT=min(x)),simplify=TRUE,drop=TRUE)
val<-data.frame(airT[["value"]])
airT$value<-val$avg
airT$maxT<-val$maxT
airT$minT<-val$minT
write.csv(airT, "data/MarinhoCatunda_Fig2_AirT_final.csv", row.names = FALSE)

#surface temperature
plotsurf<-abv[abv$SensorType=="SurfaceTemp",]
surf<-na.omit(plotsurf)
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
write.csv(surf, "data/MarinhoCatunda_Fig2_surfT_final.csv")
