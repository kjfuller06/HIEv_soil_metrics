### Figure 1 ####
#start date
sD<-as.Date("2019-06-01")
#end date
eD<-as.Date("2019-11-30")

# irrigation ####
Irrig<- (downloadTOA5("PACE_AUTO_ALL_IRRIG_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,4:6)]
names(Irrig)<-c("DateTime","Plot","Treatment","Irrigation")
write.csv(Irrig, "data/MarinhoCatunda_Fig1_Irrig_raw.csv", row.names = FALSE)

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


### Figure 2 ####
#start date
sD<-as.Date("2018-04-01")
#end date
eD<-as.Date("2020-03-31")

# irrigation ####
Irrig<- (downloadTOA5("PACE_AUTO_ALL_IRRIG_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,4:6)]
names(Irrig)<-c("DateTime","Plot","Treatment","Irrigation")
write.csv(Irrig, "data/MarinhoCatunda_Fig2_Irrig_raw.csv", row.names = FALSE)

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