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

#PAR####
x<-"PAR"
#create new dfs based on variable
df1<-rbind(s1[grep(x, s1$variable),])
df2<-rbind(s2[grep(x, s2$variable),],df1)
df5<-rbind(s5[grep(x, s5$variable),],df2)
df6<-rbind(s6[grep(x, s6$variable),],df5)

#put df in order by date
df6$DateTime<-as.POSIXct(df6$DateTime,format="%Y/%m/%d %h/%m/%s")
df6<-df6[do.call(order,df6),]
df6$Date<-date(df6$DateTime)

PAR = data.table(df6)
PAR = PAR[,list(PAR = mean(value)), 'variable,Date']

Pmax=max(PAR$PAR,na.rm=T) #use same temperature range across temperature charts
Pmin<-min(PAR$PAR,na.rm=T) #there is an NA value in a soil temp sensor data stream that is stopping the script

##subset each treatment to a different data set
PARIN<-subset(PAR, variable == "PARIN")
PAROUT<-subset(PAR, variable == "PAROUT")

##start a tiff file
tiff(file = paste("Figures/FIELD",x,"In_Vs_Outside_Shelters",eD, ".tiff"), width = 3200, height = 2100, units = "px", res = 400) 
#plot PAR
plot(PAROUT$PAR ~ PAROUT$Date, 
     type = "l",
     ylim = c(Pmin-0.01,Pmax+45),
     ylab="PAR",
     xlab="Date",main="PAR Inside and Outside Shelters")
points(PARIN$PAR ~ PARIN$Date, type = "l",col=4)
legend("topleft", y = NULL, 
       legend=c("Outside Shelters","Inside Shelters"), 
       col = c(1,4),lwd=2,cex=0.8)

dev.off()

#Air Temperature####
x<-"AirT"
#create new dfs based on variable
df1<-rbind(s1[grep(x, s1$variable),])
df2<-rbind(s2[grep(x, s2$variable),],df1)
df5<-rbind(s5[grep(x, s5$variable),],df2)
df6<-rbind(s6[grep(x, s6$variable),],df5)

#put df in order by date
df6$DateTime<-as.POSIXct(df6$DateTime,format="%Y/%m/%d %h/%m/%s")
df6<-df6[do.call(order,df6),]
df6$Date<-date(df6$DateTime)

df6<-na.omit(df6)
df6<-df6[df6$Date!="2019-03-31",]

Air = data.table(df6)
Air = Air[,list(Temp = mean(value)), 'variable,Date']

Amax=max(Air$Temp,na.rm=T) #use same temperature range across temperature charts
Amin<-min(Air$Temp,na.rm=T) #there is an NA value in a soil temp sensor data stream that is stopping the script

##subset each treatment to a different data set
AirIN<-subset(Air, variable == "AirTIN")
AirOUT<-subset(Air, variable == "AirTOUT")

##start a tiff file
tiff(file = paste("Figures/FIELD",x,"In_Vs_Outside_Shelters",eD, ".tiff"), width = 3200, height = 2100, units = "px", res = 400) 
#plot PAR
plot(AirOUT$Temp ~ AirOUT$Date, 
     type = "l",
     ylim = c(Amin-0.01,Amax+0.5),
     ylab=expression(paste("Temperature (",degree~C,")")),
     xlab="Date",main="Air Temperature Inside and Outside Shelters")
points(AirIN$Temp ~ AirIN$Date, type = "l",col=4)
legend("topleft", y = NULL, 
       legend=c("Outside Shelters","Inside Shelters"), 
       col = c(1,4),lwd=2,cex=0.8)
dev.off()

#Humidity####
x<-"Humidity"
#create new dfs based on variable
df1<-rbind(s1[grep(x, s1$variable),])
df2<-rbind(s2[grep(x, s2$variable),],df1)
df5<-rbind(s5[grep(x, s5$variable),],df2)
df6<-rbind(s6[grep(x, s6$variable),],df5)

#put df in order by date
df6$DateTime<-as.POSIXct(df6$DateTime,format="%Y/%m/%d %h/%m/%s")
df6<-df6[do.call(order,df6),]
df6$Date<-date(df6$DateTime)

df6<-na.omit(df6)

Hum = data.table(df6)
Hum = Hum[,list(Hum = mean(value)), 'variable,Date']
Hum<-Hum[Date>"2018-06-04",]

Hmax=max(Hum$Hum,na.rm=T) #use same temperature range across temperature charts
Hmin<-min(Hum$Hum,na.rm=T) #there is an NA value in a soil temp sensor data stream that is stopping the script

##subset each treatment to a different data set
HumIN<-subset(Hum, variable == "HumidityIN")
HumOUT<-subset(Hum, variable == "HumidityOUT")

##start a tiff file
tiff(file = paste("Figures/FIELD",x,"In_Vs_Outside_Shelters",eD, ".tiff"), width = 3200, height = 2100, units = "px", res = 400) 
#plot PAR
plot(HumOUT$Hum ~ HumOUT$Date, 
     type = "l",
     ylim = c(Hmin-0.01,Hmax+10),
     ylab="Humidity",
     xlab="Date",main="Humidity Inside and Outside Shelters")
points(HumIN$Hum ~ HumIN$Date, type = "l",col=4)
legend("topleft", y = NULL, 
       legend=c("Outside Shelters","Inside Shelters"), 
       col = c(1,4),lwd=2,cex=0.8)

dev.off()

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
allsum = allsum[,list(value = mean(value)), 'Plot,Shelter,Treatment,Date']
allsum = allsum[,list(avg_value = mean(value), ste_value = sd(value)/sqrt(length(value))), 'Treatment,Date']
allsum$upper<-allsum$avg_value+allsum$ste_value
allsum$lower<-allsum$avg_value-allsum$ste_value

#rename columns
names(allsum)<-c("Treatment","Date","value","stderror","upper","lower")
allsum$Treatment<-factor(allsum$Treatment,levels=c("AmbCon","AmbDrt","EleCon","EleDrt"))
allsum<-allsum[order(Date,Treatment),] 

Smax=max(allsum$value,na.rm=T) #use same temperature range across temperature charts
Smin<-min(allsum$value,na.rm=T) #there is an NA value in a soil temp sensor data stream that is stopping the script

##subset each treatment to a different data set
df1<-subset(allsum, Treatment == "AmbCon")
df2<-subset(allsum, Treatment == "AmbDrt")
df3<-subset(allsum, Treatment == "EleCon")
df4<-subset(allsum, Treatment == "EleDrt")

##start a tiff file
tiff(file = paste("FIELD",x,sD,"-",eD, ".tiff"), width = 3200, height = 2100, units = "px", res = 400) 
#plot Surface Temp
plot(df1$value ~ df1$Date, 
     type = "l",
     xlim=c(min(df1$Date),max(df1$Date)),
     ylim = c(Smin-1,Smax+2),
     ylab=expression(paste("Temperature (",degree~C,")")),
     xlab="Date",main="Plot Surface Temperature by Treatment",
     col=4)
#polygon(c(df1$Date,rev(df1$Date)),c(df1$lower,rev(df1$upper)),col=adjustcolor("blue",alpha.f=0.5),border=NA)
points(df2$value ~ df2$Date, type = "l",col=4,lty=2)
#polygon(c(df2$Date,rev(df2$Date)),c(df2$lower,rev(df2$upper)),col=adjustcolor("blue",alpha.f=0.25),density=25)
points(df3$value ~ df2$Date, type = "l",col=2)
#polygon(c(df3$Date,rev(df3$Date)),c(df3$lower,rev(df3$upper)),col=adjustcolor("red",alpha.f=0.5),border=NA)
points(df4$value ~ df2$Date, type = "l",col=2,lty=2)
#polygon(c(df4$Date,rev(df4$Date)),c(df4$lower,rev(df4$upper)),col=adjustcolor("red",alpha.f=0.25),density=25)
legend("topleft", y = NULL, 
       legend=c("aT-Con","aT-Drt","eT-Con","eT-Drt"), 
       col = c(4,4,2,2),lty=c(1,2,1,2),lwd=2,cex=0.8)
arrows(x0 =as.Date("2018-06-01"),length=0.05, y0 = (Smin+0.5), y1 = (Smin-2))
text(x = as.Date("2018-06-01"), y = (Smin+1), 
     labels=expression(paste("Drought Treatment Initiated")),cex=0.6)
arrows(x0 =as.Date("2018-12-01"),length=0.05, y0 = (Smin+0.5), y1 = (Smin-2))
text(x = as.Date("2018-12-01"), y = (Smin+1), 
     labels=expression(paste("Drought Treatment Completed")),cex=0.6)

dev.off()

#remove all objects####
rm(list=ls())
