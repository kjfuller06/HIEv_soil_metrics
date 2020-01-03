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

#export graphs to tiff
tiff(file = paste("FIELD_LUC_Daily_Soil_Moisture_",sD,"_",eD,".tiff",sep=""), width = 3200, height = 2100, units = "px", res = 400) 
#Raw soil moisture plot
par(mfrow=c(2,1),oma=c(4,3,3,0))
#Upper sensors
par(mar =c(0,0,0,3))
plot(df1$value ~ df1$Date, 
     type = "l",lwd=0.75,
     xaxt='n',
     ylim = c(ymin,ymax),
     ylab="",
     xlab="",
     main="",
     col=1,
     cex.axis=0.75)
mtext(side=3,"Surface to 15cm",padj=2,cex=0.75)
with(df1,polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("blue",alpha.f=0.5),border=NA))
with(df1,points(value ~ Date, type = "l",col=1,lwd=0.75,lty=1))
with(df2,polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("lightskyblue",alpha.f=0.5),border=NA))
with(df2,points(value ~ Date, type = "l",col=1,lwd=0.75,lty=2))
with(df3,polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("red",alpha.f=0.5),border=NA))
with(df3,points(value ~ Date, type = "l",col=1,lwd=0.75))
with(df4,polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("red4",alpha.f=0.5),border=NA))
with(df4,points(value ~ Date, type = "l",col=1,lwd=0.75,lty=2))
legend("topright", y = NULL, 
       legend=c("aT-Con","aT-Drt","eT-Con","eT-Drt"), 
       col = c(adjustcolor("blue",alpha.f=0.5),adjustcolor("lightskyblue",alpha.f=0.5),adjustcolor("red",alpha.f=0.5),adjustcolor("red4",alpha.f=0.5)),lwd=10,cex=0.75)
legend("topright", y = NULL, 
       legend=c("aT-Con","aT-Drt","eT-Con","eT-Drt"), 
       col = c(1,1,1,1),lty=c(2,2,1,2),lwd=1.5,cex=0.75,bty='n')

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
with(df5,polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("blue",alpha.f=0.5),border=NA))
with(df5,points(value ~ Date, type = "l",col=1,lwd=0.75,lty=1))
with(df6,polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("lightskyblue",alpha.f=0.5),border=NA))
with(df6,points(value ~ Date, type = "l",col=1,lwd=0.75,lty=2))
with(df7,polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("red",alpha.f=0.5),border=NA))
with(df7,points(value ~ Date, type = "l",col=1,lwd=0.75))
with(df8,polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("red4",alpha.f=0.5),border=NA))
with(df8,points(value ~ Date, type = "l",col=1,lwd=0.75,lty=2))
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

#export graphs to tiff
tiff(file = paste("FIELD_FES_Daily_Soil_Moisture_",sD,"_",eD,".tiff",sep=""), width = 3200, height = 2100, units = "px", res = 400)
#Raw soil moisture plot
par(mar=c(5.1,4.1,4.1,3.1))
plot(df1$value ~ df1$Date, 
     type = "l",
     ylim = c(ymin-0.05,ymax),
     ylab="",
     xlab="",
     main=paste("Soil Water Content in Fescue Plots by Treatment\n",sD,"-",eD),
     col=1)
mtext(side=2,"Soil Water Content",padj=-3.5)
mtext(side=1,"Date",padj=3.5)
with(df1,polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("blue",alpha.f=0.5),border=NA))
with(df1,points(value ~ Date, type = "l",col=1,lwd=0.75,lty=1))
with(df2,polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("lightskyblue",alpha.f=0.5),border=NA))
with(df2,points(value ~ Date, type = "l",col=1,lwd=0.75,lty=2))
with(df3,polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("red",alpha.f=0.5),border=NA))
with(df3,points(value ~ Date, type = "l",col=1,lwd=0.75))
with(df4,polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("red4",alpha.f=0.5),border=NA))
with(df4,points(value ~ Date, type = "l",col=1,lwd=0.75,lty=2))
legend("topright", y = NULL, 
       legend=c("aT-Con","aT-Drt","eT-Con","eT-Drt"), 
       col = c(adjustcolor("blue",alpha.f=0.5),adjustcolor("lightskyblue",alpha.f=0.5),adjustcolor("red",alpha.f=0.5),adjustcolor("red4",alpha.f=0.5)),lwd=c(10,10,10,10),cex=0.75)
legend("topright", y = NULL, 
       legend=c("aT-Con","aT-Drt","eT-Con","eT-Drt"),lty=c(1,2,1,2),lwd=1.5,cex=0.75,bty='n')
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

#export graphs to tiff
tiff(file = paste("FIELD_BIS_Daily_Soil_Moisture_",sD,"_",eD,".tiff",sep=""), width = 3200, height = 2100, units = "px", res = 400)
#Raw soil moisture plot
par(mar=c(5.1,4.1,4.1,3.1))
plot(df1$value ~ df1$Date, 
     type = "l",
     ylim = c(ymin-0.05,ymax),
     ylab="",
     xlab="",
     main=paste("Soil Water Content in Biserrula Plots by Treatment\n",sD,"-",eD),
     col=1)
mtext(side=2,"Soil Water Content",padj=-3.5)
mtext(side=1,"Date",padj=3.5)
polygon(c(df1$Date,rev(df1$Date)),c(df1$lower,rev(df1$upper)),col=adjustcolor("blue",alpha.f=0.5),border=NA)
points(df1$value ~ df1$Date, type = "l",col=1)
polygon(c(df2$Date,rev(df2$Date)),c(df2$lower,rev(df2$upper)),col=adjustcolor("lightskyblue",alpha.f=0.5),border=NA)
points(df2$value ~ df2$Date, type = "l",col=1,lty=2)
legend("topright", y = NULL, 
       legend=c("aT-Con","aT-Drt"), 
       col = c(adjustcolor("blue",alpha.f=0.5),adjustcolor("lightskyblue",alpha.f=0.5)),lwd=c(10,10),cex=0.75)
legend("topright", y = NULL, 
       legend=c("aT-Con","aT-Drt"), 
       col = c(1,1),lty=c(1,2),lwd=1.5,cex=0.75,bty='n')
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

#export graphs to tiff
tiff(file = paste("FIELD_RYE_Daily_Soil_Moisture_",sD,"_",eD,".tiff",sep=""), width = 3200, height = 2100, units = "px", res = 400)
#Raw soil moisture plot
par(mar=c(5.1,4.1,4.1,3.1))
plot(df1$value ~ df1$Date, 
     type = "l",
     ylim = c(ymin-0.05,ymax),
     ylab="",
     xlab="",
     main=paste("Soil Water Content in Rye Plots by Treatment\n",sD,"-",eD),
     col=1)
mtext(side=2,"Soil Water Content",padj=-3.5)
mtext(side=1,"Date",padj=3.5)
polygon(c(df1$Date,rev(df1$Date)),c(df1$lower,rev(df1$upper)),col=adjustcolor("blue",alpha.f=0.5),border=NA)
points(df1$value ~ df1$Date, type = "l",col=1)
polygon(c(df2$Date,rev(df2$Date)),c(df2$lower,rev(df2$upper)),col=adjustcolor("lightskyblue",alpha.f=0.5),border=NA)
points(df2$value ~ df2$Date, type = "l",col=1,lty=2)
legend("topright", y = NULL, 
       legend=c("aT-Con","aT-Drt"), 
       col = c(adjustcolor("blue",alpha.f=0.5),adjustcolor("lightskyblue",alpha.f=0.5)),lwd=c(10,10),cex=0.75)
legend("topright", y = NULL, 
       legend=c("aT-Con","aT-Drt"), 
       col = c(1,1),lty=c(1,2),lwd=1.5,cex=0.75,bty='n')
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