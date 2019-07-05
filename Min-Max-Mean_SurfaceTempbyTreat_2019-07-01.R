#Basic####
#write start date for first analysis
sD<-as.Date("2018-05-01")
#Write end date for first analysis
eD<-as.Date("2019-05-01")

#download from HIEv and processing####
#get data, only keep variables of interest
s1 <- (downloadTOA5("PACE_AUTO_S1_ABVGRND_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,4,6,7,12:15)]
s2 <- (downloadTOA5("PACE_AUTO_S2_ABVGRND_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,4,6,7,12:15)]
s3 <- (downloadTOA5("PACE_AUTO_S3_ABVGRND_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,4,6,7,12:15)]
s4 <- (downloadTOA5("PACE_AUTO_S4_ABVGRND_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,4,6,7,12:15)]
s5 <- (downloadTOA5("PACE_AUTO_S5_ABVGRND_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,4,6,7,12:15)]
s6 <- (downloadTOA5("PACE_AUTO_S6_ABVGRND_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,4,6,7,12:15)]

#Rename columns
names(s1)<-c("DateTime","PARIN",
             "AirTIN",
             "HumidityIN",
             "SurfaceTemp3AmbDrt",
             "SurfaceTemp4EleCon",
             "SurfaceTemp5EleDrt",
             "SurfaceTemp6AmbCon")

names(s2)<-c("DateTime","PAROUT",
             "AirTOUT",
             "HumidityOUT",
             "SurfaceTemp3EleDrt",
             "SurfaceTemp4AmbDrt",
             "SurfaceTemp5AmbCon",
             "SurfaceTemp6EleCon")

names(s3)<-c("DateTime","PARIN",
             "AirTIN",
             "HumidityIN",
             "SurfaceTemp3AmbCon",
             "SurfaceTemp4EleCon",
             "SurfaceTemp5EleDrt",
             "SurfaceTemp6AmbDrt")

names(s4)<-c("DateTime","PAROUT",
             "AirTOUT",
             "HumidityOUT",
             "SurfaceTemp3EleCon",
             "SurfaceTemp4AmbDrt",
             "SurfaceTemp5AmbCon",
             "SurfaceTemp6EleDrt")

names(s5)<-c("DateTime","PARIN",
             "AirTIN",
             "HumidityIN",
             "SurfaceTemp3EleCon",
             "SurfaceTemp4AmbDrt",
             "SurfaceTemp6EleDrt",
             "SurfaceTemp5AmbCon")

names(s6)<-c("DateTime","PAROUT",
             "AirTOUT",
             "HumidityOUT",
             "SurfaceTemp3EleDrt",
             "SurfaceTemp4AmbCon",
             "SurfaceTemp5EleCon",
             "SurfaceTemp6AmbDrt")

#convert to long form, add "shelter" variable
s1<-melt(s1,id.vars="DateTime")
s1$Shelter<-1
s2<-melt(s2,id.vars="DateTime")
s2$Shelter<-2
s3<-melt(s3,id.vars="DateTime")
s3$Shelter<-3
s4<-melt(s4,id.vars="DateTime")
s4$Shelter<-4
s5<-melt(s5,id.vars="DateTime")
s5$Shelter<-5
s6<-melt(s6,id.vars="DateTime")
s6$Shelter<-6

#Surface Temperatures####
x<-"SurfaceTemp"
#create new dfs based on variable
df1<-rbind(s1[grep(x, s1$variable),])
df2<-rbind(s2[grep(x, s2$variable),],df1)
df5<-rbind(s5[grep(x, s5$variable),],df2)
df6<-rbind(s6[grep(x, s6$variable),],df5)

#Separate treatments and other variables into separate columns
df6<-separate(data=df6, col=variable, c("Sensor Type","Plot","Treatment"), sep = c(11,12), remove = TRUE)

#put df in order by date
df6$DateTime<-as.POSIXct(df6$DateTime,format="%Y/%m/%d %h/%m/%s")
df6<-df6[do.call(order,df6),]
#df6$Date<-date(df6$DateTime)

##average and standard error by treatment
allsum = data.table(df6)
allsum<-na.omit(allsum)
allsum$Date<-as.Date(allsum$DateTime)
allsum = allsum[,list(value = mean(value),mini=min(value),maxi=max(value)), 'Plot,Shelter,Treatment,Date']
allsum = allsum[,list(avg_value = mean(value), ste_value = sd(value)/sqrt(length(value)),avg_mini = mean(mini), ste_mini = sd(mini)/sqrt(length(mini)),avg_maxi = mean(maxi), ste_maxi = sd(maxi)/sqrt(length(maxi))), 'Treatment,Date']
allsum$val_upper<-allsum$avg_value+allsum$ste_value
allsum$val_lower<-allsum$avg_value-allsum$ste_value
allsum$mini_upper<-allsum$avg_mini+allsum$ste_mini
allsum$mini_lower<-allsum$avg_mini-allsum$ste_mini
allsum$maxi_upper<-allsum$avg_maxi+allsum$ste_maxi
allsum$maxi_lower<-allsum$avg_maxi-allsum$ste_maxi
allsum<-allsum[,-c(4,6,8)]

#rename columns
names(allsum)<-c("Treatment","Date","valavg","minavg","maxavg","upperval","lowerval","uppermin","lowermin","uppermax","lowermax")
allsum$Treatment<-factor(allsum$Treatment,levels=c("AmbCon","AmbDrt","EleCon","EleDrt"))
allsum<-allsum[order(Date,Treatment),] 

Smax=max(allsum$maxavg,na.rm=T) #use same temperature range across temperature charts
Smin<-min(allsum$minavg,na.rm=T) #there is an NA value in a soil temp sensor data stream that is stopping the script

##subset each treatment to a different data set
df1<-subset(allsum, Treatment == "AmbCon")
df2<-subset(allsum, Treatment == "AmbDrt")
df3<-subset(allsum, Treatment == "EleCon")
df4<-subset(allsum, Treatment == "EleDrt")

##start a tiff file
tiff(file = paste("FIELD",x,sD,"-",eD, ".tiff"), width = 3200, height = 2100, units = "px", res = 400) 
#matrix of three panels
par(mfcol=c(1,3))
#plot Surface Temp-max
plot(df1$maxavg ~ df1$Date, 
     type = "l",
     xaxt='n',
     ylim = c(Smin+1,Smax-1),
     ylab=expression(paste("Temperature (",degree~C,")")),
     xlab="Date",
     las=2,
     col=4)
points(df2$maxavg ~ df2$Date, type = "l",col=4,lty=2)
points(df3$maxavg ~ df2$Date, type = "l",col=2)
points(df4$maxavg ~ df2$Date, type = "l",col=2,lty=2)
mtext(text="Maximum Daily Temperature",side=3,outer=FALSE,cex=0.75)
axis.Date(side=1,at=seq(as.Date("2018-05-01"),as.Date("2019-05-01"),by="months"),labels=TRUE,las=2,cex.axis=0.85)
#plot Surface Temp-min
plot(df1$minavg ~ df1$Date, 
     type = "l",
     xaxt='n',
     ylim = c(Smin+1,Smax-1),
     ylab=expression(paste("Temperature (",degree~C,")")),
     xlab="Date",
     las=2,
     col=4)
points(df2$minavg ~ df2$Date, type = "l",col=4,lty=2)
points(df3$minavg ~ df2$Date, type = "l",col=2)
points(df4$minavg ~ df2$Date, type = "l",col=2,lty=2)
mtext(text="Minimum Daily Temperature",side=3,outer=FALSE,cex=0.75)
mtext(text="Plot Surface Temperatures by Treatment",side=3,line=2,outer=FALSE,cex=0.75)
axis.Date(side=1,at=seq(as.Date("2018-05-01"),as.Date("2019-05-01"),by="months"),labels=TRUE,las=2,cex.axis=0.85)
#plot Surface Temp-mean
plot(df1$valavg ~ df1$Date, 
     type = "l",
     xaxt='n',
     ylim = c(Smin+1,Smax-1),
     ylab=expression(paste("Temperature (",degree~C,")")),
     xlab="Date",
     las=2,
     col=4)
points(df2$valavg ~ df2$Date, type = "l",col=4,lty=2)
points(df3$valavg ~ df2$Date, type = "l",col=2)
points(df4$valavg ~ df2$Date, type = "l",col=2,lty=2)
legend("topright", y = NULL, 
       legend=c("aT-Con","aT-Drt","eT-Con","eT-Drt"), 
       col = c(4,4,2,2),lty=c(1,3,1,3),lwd=2,cex=0.8)
mtext(text="Mean Daily Temperature",side=3,outer=FALSE,cex=0.75)
axis.Date(side=1,at=seq(as.Date("2018-05-01"),as.Date("2019-05-01"),by="months"),labels=TRUE,las=2,cex.axis=0.85)

dev.off()

#summary stats
sumstat<-allsum[,c(1,2,4,5)]
df1<-subset(sumstat, Treatment == "AmbCon")
df2<-subset(sumstat, Treatment == "AmbDrt")
df3<-subset(sumstat, Treatment == "EleCon")
df4<-subset(sumstat, Treatment == "EleDrt")

stats<-data.frame("Treatment"=c("aT-Con","aT-Drt","eT-Con","eT-Drt"),"less0"=c(sum(df1$minavg<0),sum(df2$minavg<0),sum(df3$minavg<0),sum(df4$minavg<0)),"less5"=c(sum(df1$minavg<5),sum(df2$minavg<5),sum(df3$minavg<5),sum(df4$minavg<5)),"over35"=c(sum(df1$maxavg>35),sum(df2$maxavg>35),sum(df3$maxavg>35),sum(df4$maxavg>35)),"over40"=c(sum(df1$maxavg>40),sum(df2$maxavg>40),sum(df3$maxavg>40),sum(df4$maxavg>40)))

write.csv(stats,file="Daily_Temp_Summary_Stats_2019-07-01.csv")
