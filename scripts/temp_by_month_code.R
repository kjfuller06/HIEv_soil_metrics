<<<<<<< HEAD
all<-rbind(s1,s2,s3,s4,s5,s6)
all<-separate(data=all, col=variable, c("Plot","Species", "Treatment"), sep = c(1,4), remove = TRUE)
all$Date<-as.Date(all$DateTime)
all2<-aggregate(data=all,value~Treatment+Date,FUN=mean)
all3<-all2
all3$Month<-months(all2$Date)
all3$Year<-format(all3$Date,"%Y")
all3$Month<-format(all3$Date,"%B-%Y")
all3<-aggregate(data=all3,value~Treatment+Month+Year,FUN=mean)
write.csv(all3,file="temp_by_month.csv")
=======
all<-rbind(s1,s2,s3,s4,s5,s6)
all<-separate(data=all, col=variable, c("Plot","Species", "Treatment"), sep = c(1,4), remove = TRUE)
all$Date<-as.Date(all$DateTime)
all2<-aggregate(data=all,value~Treatment+Date,FUN=mean)
all3<-all2
all3$Month<-months(all2$Date)
all3$Year<-format(all3$Date,"%Y")
all3$Month<-format(all3$Date,"%B-%Y")
all3<-aggregate(data=all3,value~Treatment+Month+Year,FUN=mean)
write.csv(all3,file="temp_by_month.csv")
>>>>>>> 319aaf1763ca767e820298cb0eaf6c504234ddcd
