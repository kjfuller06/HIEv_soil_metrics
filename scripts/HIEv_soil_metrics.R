#write start date for first analysis
sD<-as.Date("2018-05-21")
#Write end date for first analysis
eD<-as.Date("2019-05-25")

#Irrigation####
Irrig<- (downloadTOA5("PACE_AUTO_ALL_IRRIG_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,4:6)]

#Rename columns
names(Irrig)<-c("DateTime","Plot","Treatment","Irrigation")
Irrig$Date<-as.Date(Irrig$DateTime)

#force Treatment to be a factor
Irrig$Treatment<-as.factor(Irrig$Treatment)
Irrig<-Irrig[order(Irrig$Date,Irrig$Treatment),]
Irrig<-aggregate(data=Irrig,Irrigation~Date+Treatment,FUN=mean)
levels(Irrig$Treatment)<-c("Con","Drt","Con","Drt")

#subset data by treatment
Irrig1<-subset(Irrig, Treatment == "Con")
Irrig2<-subset(Irrig[Irrig$Date>"2018-05-31"&Irrig$Date<"2018-12-01",], Treatment == "Drt")

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
df1<-rbind(s1,s2,s3,s4,s5,s6)
names(df1)<-c("DateTime","SensorCode","value","Shelter")
sensors<-read.csv("sensors.csv")
df1<-merge(df1,sensors,by=c("Shelter","SensorCode"))

#change class of variables
df1$Sp<-as.factor(df1$Sp)
df1$Plot<-as.factor(df1$Plot)
df1$Shelter<-as.factor(df1$Shelter)
df1$Date<-as.Date(df1$DateTime)

#remove NAs
df1<-na.omit(df1)

#Summarize data
df1<-aggregate(data=df1,value~SensorType+Sp+Date+Position+Treatment+Shelter+Plot,FUN=mean,simplify=TRUE,drop=TRUE)
df1<-aggregate(data=df1,value~SensorType+Sp+Date+Position+Treatment,FUN=function(x) c(avg=mean(x),upper=mean(x)+sd(x)/sqrt(length(x)),lower=mean(x)-sd(x)/sqrt(length(x))),simplify=TRUE,drop=TRUE)

#spit the aggregate function outputs into a DF and reassign so they are variables in the dataframe
##otherwise the output is a list within df1
val<-data.frame(df1[["value"]])
df1$value<-val$avg
df1$upper<-val$upper
df1$lower<-val$lower

#separate data frame by data type and species
SM<-df1[df1$SensorType=="TDR",]
for(i in levels(SM$Sp)){
    x<-SM[SM$Sp==i,]
    x<-x[order(x$Date,x$Treatment),]
    assign(paste(i,1,sep=""),x)
}
ST<-df1[df1$SensorType=="Temp",]
for(i in levels(ST$Sp)){
   x<-ST[ST$Sp==i,]
   x<-x[order(x$Date,x$Treatment),]
   assign(paste(i,2,sep=""),x)
}

safety<-df1

#LUC####
#calculate max and min values for ylim, remove NAs
ymax<-max(LUC1$upper,na.rm=T) 
ymin<-min(LUC1$lower,na.rm=T) 

##subset each treatment to a different data set
n1<-levels(LUC1$Position)
n2<-levels(LUC1$Treatment)
count=1
for(i in n1){
   x1<-LUC1[LUC1$Position==i,]
   for(i in n2){
      x2<-subset(x1, Treatment == i)
      assign(paste("df",count,sep=""),x2)
      count=count+1
   }
}

#export graphs to tiff----
tiff(file = paste("FIELD_LUC_Daily_Soil_Moisture_",sD,"_",eD,".tiff",sep=""), width = 3200, height = 2100, units = "px", res = 400) 

#LUC plot----
par(mfrow=c(2,1),oma=c(4,3,3,0))

#Soil moisture plot
#Upper sensors
par(mar =c(0,0,0,3))
plot(df1$value ~ df1$Date, 
     type = "l",lwd=0.75,
     xaxt='n',
     ylim = c(ymin,ymax),
     ylab="",
     xlab="",
     main="",
     col=4,
     cex.axis=0.75)
mtext(side=3,"Surface to 15cm",padj=2,cex=0.75)
polygon(c(df1$Date,rev(df1$Date)),c(df1$lower,rev(df1$upper)),col=adjustcolor("blue",alpha.f=0.5),border=NA)
points(df2$value ~ df2$Date, type = "l",col=4,lwd=0.75,lty=2)
polygon(c(df2$Date,rev(df2$Date)),c(df2$lower,rev(df2$upper)),col=adjustcolor("blue",alpha.f=0.25),density=25)
points(df3$value ~ df3$Date, type = "l",col=2,lwd=0.75)
polygon(c(df3$Date,rev(df3$Date)),c(df3$lower,rev(df3$upper)),col=adjustcolor("red",alpha.f=0.5),border=NA)
points(df4$value ~ df4$Date, type = "l",col=2,lwd=0.75,lty=2)
polygon(c(df4$Date,rev(df4$Date)),c(df4$lower,rev(df4$upper)),col=adjustcolor("red",alpha.f=0.25),density=25)
legend("topright", y = NULL, 
       legend=c("aT-Con","aT-Drt","eT-Con","eT-Drt"), 
       col = c(4,4,2,2),lty=c(1,2,1,2),lwd=1.5,cex=0.75)

#Lower sensors
par(mar = c(1,0,0,3))
plot(df5$value ~ df5$Date, 
     type = "l",lwd=0.75,
     ylim = c(ymin-0.05,ymax),
     ylab="",
     xlab="",
     col=4,
     cex.main=0.75,
     cex.axis=0.75,
     xaxt='n')
axis.Date(side=1,x=df5$Date,cex.axis=0.75)
box(which = "plot", lty = "solid")
mtext(side=3,"15cm to 30cm",padj=2,cex=0.75)
polygon(c(df5$Date,rev(df5$Date)),c(df5$lower,rev(df5$upper)),col=adjustcolor("blue",alpha.f=0.5),border=NA)
points(df6$value ~ df6$Date, type = "l",col=4,lwd=0.75,lty=2)
polygon(c(df6$Date,rev(df6$Date)),c(df6$lower,rev(df6$upper)),col=adjustcolor("blue",alpha.f=0.25),density=25)
points(df7$value ~ df7$Date, type = "l",col=2,lwd=0.75)
polygon(c(df7$Date,rev(df7$Date)),c(df7$lower,rev(df7$upper)),col=adjustcolor("red",alpha.f=0.5),border=NA)
points(df8$value ~ df8$Date, type = "l",col=2,lwd=0.75,lty=2)
polygon(c(df8$Date,rev(df8$Date)),c(df8$lower,rev(df8$upper)),col=adjustcolor("red",alpha.f=0.25),density=25)
# arrows(x0 =as.Date("2018-08-23"),length=0.05, y0 = 0.025, y1 = -0.015)
# text(x = as.Date("2018-08-23"), y = 0.03,labels=expression(paste("Drought Treatment Initiated")),cex=0.6)

#Irrigation plot
par(new=TRUE)
plot(Irrig1$Irrigation~Irrig1$Date,
     type="h",
     axes=FALSE,
     xaxt='n',
     ylab="",
     xlab="",
     ylim = c(0,125))
par(new=TRUE)
with(Irrig2[Irrig2$Date>"2018-08-22",],plot(Irrigation~Date,
                                           type="h",
                                           col=2,
                                           axes=FALSE,
                                           xaxt='n',
                                           ylab="",
                                           xlab="",
                                           ylim = c(0,125),
                                           xlim=c(min(Irrig1$Date),max(Irrig1$Date))))
corners2 = par("usr") #Gets the four corners of plot area (x1, x2, y1, y2)
par(xpd = TRUE) #Draw outside plot area
text(x = corners2[2]+corners2[2]/850, y = mean(corners2[3:4])-52, "Irrigation\n(mm)",srt=270,cex=0.6)
text(x = corners2[2]+corners2[2]/1800, y = mean(corners2[3:4])-52, "20\n10\n0",las=0,cex=0.65)
axis(side=4,at=c(0,10,20),las=2,labels=c("","",""))

mtext(text=paste("Soil Moisture Content in Lucerne by Treatment\n",sD,"-",eD),side=3,line=1,outer=TRUE)
mtext(text="Date",side=1,line=2,outer=TRUE,cex=0.75)
mtext(text="Soil Water Content",side=2,line=2,outer=TRUE,cex=0.75)

dev.off()

#FES####
#calculate max and min values for ylim, remove NAs
ymax<-max(FES1$upper,na.rm=T) 
ymin<-min(FES1$lower,na.rm=T) 

##subset each treatment to a different data set
n<-levels(FES1$Treatment)
count=1
for(i in n){
   x<-subset(FES1, Treatment == i)
   assign(paste("df",count,sep=""),x)
   count=count+1
}

#export graphs to tiff####
tiff(file = paste("FIELD_FES_Daily_Soil_Moisture_",sD,"_",eD,".tiff",sep=""), width = 3200, height = 2100, units = "px", res = 400)
#FES plot####
#Soil moisture plot
par(mar=c(5.1,4.1,4.1,3.1))
plot(df1$value ~ df1$Date, 
     type = "l",
     ylim = c(ymin-0.05,ymax),
     ylab="",
     xlab="",
     main=paste("Soil Water Content in Fescue Plots by Treatment\n",sD,"-",eD),
     col=4)
mtext(side=2,"Soil Water Content",padj=-3.5)
mtext(side=1,"Date",padj=3.5)
polygon(c(df1$Date,rev(df1$Date)),c(df1$lower,rev(df1$upper)),col=adjustcolor("blue",alpha.f=0.5),border=NA)
points(df2$value ~ df2$Date, type = "l",col=4,lty=2)
polygon(c(df2$Date,rev(df2$Date)),c(df2$lower,rev(df2$upper)),col=adjustcolor("blue",alpha.f=0.25),density=25)
points(df3$value ~ df3$Date, type = "l",col=2)
polygon(c(df3$Date,rev(df3$Date)),c(df3$lower,rev(df3$upper)),col=adjustcolor("red",alpha.f=0.5),border=NA)
points(df4$value ~ df4$Date, type = "l",col=2,lty=2)
polygon(c(df4$Date,rev(df4$Date)),c(df4$lower,rev(df4$upper)),col=adjustcolor("red",alpha.f=0.25),density=25)
legend("topleft", y = NULL, 
       legend=c("aT-Con","aT-Drt","eT-Con","eT-Drt"), 
       col = c(4,4,2,2),lty=c(1,2,1,2),lwd=1.5,cex=0.75)
# arrows(x0 =as.Date("2018-06-01"),length=0.05, y0 = 0.03, y1 = 0.01)
# text(x = as.Date("2018-06-01"), y = 0.035,labels=expression(paste("Drought Treatment Initiated")),cex=0.6)

#Irrigation plot
par(new=TRUE)
plot(Irrig1$Irrigation~Irrig1$Date,
     type="h",
     axes=FALSE,
     xaxt='n',
     ylab="",
     xlab="",
     ylim = c(0,150))
par(new=TRUE)
plot(Irrig2$Irrigation~Irrig2$Date,
     type="h",
     col=2,
     axes=FALSE,
     xaxt='n',
     ylab="",
     xlab="",
     ylim = c(0,150),
     xlim=c(min(Irrig1$Date),max(Irrig1$Date)))
corners2 = par("usr") #Gets the four corners of plot area (x1, x2, y1, y2)
par(xpd = TRUE) #Draw outside plot area
text(x = corners2[2]+corners2[2]/850, y = mean(corners2[3:4])-66, "Irrigation\n(mm)",srt=270,cex=0.6)
text(x = corners2[2]+corners2[2]/1800, y = mean(corners2[3:4])-65, "20\n10\n0",las=0,cex=0.65)
axis(side=4,at=c(5,10,15),las=2,labels=c("","",""))

dev.off()

#Bis####
#calculate max and min values for ylim, remove NAs
ymax<-max(BIS1$upper,na.rm=T)
ymin<-min(BIS1$lower,na.rm=T)

##subset each treatment to a different data set
df1<-subset(BIS1, Treatment == "AmbCon")
df2<-subset(BIS1, Treatment == "AmbDrt")

#export graphs to tiff####
tiff(file = paste("FIELD_BIS_Daily_Soil_Moisture_",sD,"_",eD,".tiff",sep=""), width = 3200, height = 2100, units = "px", res = 400)
#BIS plot####
#Soil moisture plot
par(mar=c(5.1,4.1,4.1,3.1))
plot(df1$value ~ df1$Date, 
     type = "l",
     ylim = c(ymin-0.05,ymax),
     ylab="",
     xlab="",
     main=paste("Soil Water Content in Biserrula Plots by Treatment\n",sD,"-",eD),
     col=4)
mtext(side=2,"Soil Water Content",padj=-3.5)
mtext(side=1,"Date",padj=3.5)
polygon(c(df1$Date,rev(df1$Date)),c(df1$lower,rev(df1$upper)),col=adjustcolor("blue",alpha.f=0.5),border=NA)
points(df2$value ~ df2$Date, type = "l",col=4,lty=2)
polygon(c(df2$Date,rev(df2$Date)),c(df2$lower,rev(df2$upper)),col=adjustcolor("blue",alpha.f=0.25),density=25)
legend("topleft", y = NULL, 
       legend=c("aT-Con","aT-Drt"), 
       col = c(4,4),lty=c(1,2),lwd=1.5,cex=0.75)
# arrows(x0 =as.Date("2018-08-22"),length=0.05, y0 = 0.03, y1 = 0.0)
# text(x = as.Date("2018-08-22"), y = 0.035,labels=expression(paste("Drought Treatment Initiated")),cex=0.6)


#Irrigation plot
par(new=TRUE)
plot(Irrig1$Irrigation~Irrig1$Date,
     type="h",
     axes=FALSE,
     xaxt='n',
     ylab="",
     xlab="",
     ylim = c(0,150))
par(new=TRUE)
with(Irrig2[Irrig2$Date>"2018-08-22",],plot(Irrig2$Irrigation~Irrig2$Date,
                                                    type="h",
                                                    col=2,
                                                    axes=FALSE,
                                                    xaxt='n',
                                                    ylab="",
                                                    xlab="",
                                                    ylim = c(0,150),
                                                    xlim=c(min(Irrig1$Date),max(Irrig1$Date))))
corners2 = par("usr") #Gets the four corners of plot area (x1, x2, y1, y2)
par(xpd = TRUE) #Draw outside plot area
text(x = corners2[2]+corners2[2]/850, y = mean(corners2[3:4])-66, "Irrigation\n(mm)",srt=270,cex=0.6)
text(x = corners2[2]+corners2[2]/1800, y = mean(corners2[3:4])-65, "20\n10\n0",las=0,cex=0.65)
axis(side=4,at=c(5,10,15),las=2,labels=c("","",""))

dev.off()

#Rye####
#calculate max and min values for ylim, remove NAs
ymax<-max(RYE1$upper,na.rm=T)
ymin<-min(RYE1$lower,na.rm=T)

##subset each treatment to a different data set
df1<-subset(RYE1, Treatment == "AmbCon")
df2<-subset(RYE1, Treatment == "AmbDrt")

#export graphs to tiff####
tiff(file = paste("FIELD_RYE_Daily_Soil_Moisture_",sD,"_",eD,".tiff",sep=""), width = 3200, height = 2100, units = "px", res = 400)
#RYE plot####
#Soil moisture plot
par(mar=c(5.1,4.1,4.1,3.1))
plot(df1$value ~ df1$Date, 
     type = "l",
     ylim = c(ymin-0.05,ymax),
     ylab="",
     xlab="",
     main=paste("Soil Water Content in Rye Plots by Treatment\n",sD,"-",eD),
     col=4)
mtext(side=2,"Soil Water Content",padj=-3.5)
mtext(side=1,"Date",padj=3.5)
polygon(c(df1$Date,rev(df1$Date)),c(df1$lower,rev(df1$upper)),col=adjustcolor("blue",alpha.f=0.5),border=NA)
points(df2$value ~ df2$Date, type = "l",col=4,lty=2)
polygon(c(df2$Date,rev(df2$Date)),c(df2$lower,rev(df2$upper)),col=adjustcolor("blue",alpha.f=0.25),density=25)
legend("topleft", y = NULL, 
       legend=c("aT-Con","aT-Drt"), 
       col = c(4,4),lty=c(1,2),lwd=1.5,cex=0.75)
# arrows(x0 =as.Date("2018-08-22"),length=0.05, y0 = 0.03, y1 = 0.0)
# text(x = as.Date("2018-08-22"), y = 0.035,labels=expression(paste("Drought Treatment Initiated")),cex=0.6)

#Irrigation plot
par(new=TRUE)
plot(Irrig1$Irrigation~Irrig1$Date,
     type="h",
     axes=FALSE,
     xaxt='n',
     ylab="",
     xlab="",
     ylim = c(0,150))
par(new=TRUE)
with(Irrig2[Irrig2$Date>"2018-08-22",],plot(Irrigation~Date,
                                           type="h",
                                           col=2,
                                           axes=FALSE,
                                           xaxt='n',
                                           ylab="",
                                           xlab="",
                                           ylim = c(0,150),
                                           xlim=c(min(Irrig1$Date),max(Irrig1$Date))))
corners2 = par("usr") #Gets the four corners of plot area (x1, x2, y1, y2)
par(xpd = TRUE) #Draw outside plot area
text(x = corners2[2]+corners2[2]/850, y = mean(corners2[3:4])-66, "Irrigation\n(mm)",srt=270,cex=0.6)
text(x = corners2[2]+corners2[2]/1800, y = mean(corners2[3:4])-65, "20\n10\n0",las=0,cex=0.65)
axis(side=4,at=c(5,10,15),las=2,labels=c("","",""))

dev.off()

#Soil Temperature####
#LUC####
#calculate max and min values for ylim, remove NAs
ymax<-max(LUC2$upper,na.rm=T) 
ymin<-min(LUC2$lower,na.rm=T) 

##subset each treatment to a different data set
n<-levels(LUC2$Treatment)
count=1
for(i in n){
   x<-subset(LUC2, Treatment == i)
   assign(paste("df",count,sep=""),x)
   count=count+1
}

##export to tiff file####
tiff(file = paste("FIELD_LUC_Daily_Soil_Temperature",sD,"_",eD,".tiff",sep=""), width = 3200, height = 2100, units = "px", res = 400) 
#Luc temp plot####
plot(df1$value ~ df1$Date, 
     type = "l",
     ylim = c(ymin,ymax),
     ylab=expression(paste("Temperature (",degree~C,")")),
     xlab="Date",
     main=paste("Soil Temperature in Lucerne Plots by Treatment\n",sD,"-",eD),
     col=4)
polygon(c(df1$Date,rev(df1$Date)),c(df1$lower,rev(df1$upper)),col=adjustcolor("blue",alpha.f=0.5),border=NA)
points(df2$value ~ df2$Date, type = "l",col=4,lty=2)
polygon(c(df2$Date,rev(df2$Date)),c(df2$lower,rev(df2$upper)),col=adjustcolor("blue",alpha.f=0.25),density=25)
points(df3$value ~ df3$Date, type = "l",col=2)
polygon(c(df3$Date,rev(df3$Date)),c(df3$lower,rev(df3$upper)),col=adjustcolor("red",alpha.f=0.5),border=NA)
points(df4$value ~ df4$Date, type = "l",col=2,lty=2)
polygon(c(df4$Date,rev(df4$Date)),c(df4$lower,rev(df4$upper)),col=adjustcolor("red",alpha.f=0.25),density=25)
legend("topleft", y = NULL, 
       legend=c("aT-Con","aT-Drt","eT-Con","eT-Drt"), 
       col = c(4,4,2,2),lty=c(1,2,1,2),lwd=2,cex=0.8)
# arrows(x0 =as.Date("2018-08-23"),length=0.05, y0 = 8, y1 = 6.5)
# text(x = as.Date("2018-08-23"), y = 8.5,labels=expression(paste("Drought Treatment Initiated")),cex=0.6)
# arrows(x0 =as.Date("2018-12-01"),length=0.05, y0 = 8, y1 = 6.5)
# text(x = as.Date("2018-12-01"), y = 8.5,labels=expression(paste("Drought Treatment Completed")),cex=0.6)

dev.off()

#FES####
#calculate max and min values for ylim, remove NAs
ymax<-max(FES2$upper,na.rm=T) 
ymin<-min(FES2$lower,na.rm=T)

##subset each treatment to a different data set
n<-levels(FES2$Treatment)
count=1
for(i in n){
   x<-subset(FES2, Treatment == i)
   assign(paste("df",count,sep=""),x)
   count=count+1
}

##export to tiff file####
tiff(file = paste("FIELD_FES_Daily_Soil_Temperature",sD,"_",eD,".tiff",sep=""), width = 3200, height = 2100, units = "px", res = 400)  
#Fes temp plot####
plot(df1$value ~ df1$Date, 
     type = "l",
     ylim = c(ymin,ymax),
     ylab=expression(paste("Temperature (",degree~C,")")),
     xlab="Date",
     main=paste("Soil Temperature in Fescue Plots by Treatment\n",sD,"-",eD),
     col=4)
polygon(c(df1$Date,rev(df1$Date)),c(df1$lower,rev(df1$upper)),col=adjustcolor("blue",alpha.f=0.5),border=NA)
points(df2$value ~ df2$Date, type = "l",col=4,lty=2)
polygon(c(df2$Date,rev(df2$Date)),c(df2$lower,rev(df2$upper)),col=adjustcolor("blue",alpha.f=0.25),density=25)
points(df3$value ~ df3$Date, type = "l",col=2)
polygon(c(df3$Date,rev(df3$Date)),c(df3$lower,rev(df3$upper)),col=adjustcolor("red",alpha.f=0.5),border=NA)
points(df4$value ~ df4$Date, type = "l",col=2,lty=2)
polygon(c(df4$Date,rev(df4$Date)),c(df4$lower,rev(df4$upper)),col=adjustcolor("red",alpha.f=0.25),density=25)
legend("topleft", y = NULL, 
       legend=c("aT-Con","aT-Drt","eT-Con","eT-Drt"), 
       col = c(4,4,2,2),lty=c(1,2,1,2),lwd=2,cex=0.8)
# arrows(x0 =as.Date("2018-06-01"),length=0.05, y0 = 8, y1 = 6.5)
# text(x = as.Date("2018-06-01"), y = 8.5,labels=expression(paste("Drought Treatment Initiated")),cex=0.6)
# arrows(x0 =as.Date("2018-12-01"),length=0.05, y0 = 8, y1 = 6.5)
# text(x = as.Date("2018-12-01"), y = 8.5,labels=expression(paste("Drought Treatment Completed")),cex=0.6)

dev.off()