#visualise raw soil moisture data####
#LUC####
#calculate max and min values for ylim, remove NAs
ymax<-max(LUC1$upper,na.rm=T) 
ymin<-min(LUC1$lower,na.rm=T) 

#export graphs to tiff
tiff(file = paste("FIELD_LUC_Daily_Soil_Moisture_",sD,"_",eD,".tiff",sep=""), width = 3200, height = 2100, units = "px", res = 400) 
#Raw soil moisture plot
par(mfrow=c(2,1),oma=c(5,4.1,3,0))
#Upper sensors
#par(mar=c(bottom,left,top,right))
par(mar=c(0,0,0,3))
with(rLUC[[1]],plot(value ~ Date, 
                    type = "l",
                    bty='l',
                    ylim = c(0.03,0.195),
                    ylab="",
                    xlab="",
                    xaxt='n',
                    col=1,
                    cex.axis=0.65))
with(rLUC[[3]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("red",alpha.f=0.65),border=NA))
with(rLUC[[3]],points(value ~ Date, type = "l",col=1,lwd=0.75))
with(rLUC[[1]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("blue",alpha.f=0.65),border=NA))
with(rLUC[[1]],points(value ~ Date, type = "l",col=1,lwd=0.75,lty=1))
with(rLUC[[4]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("red4",alpha.f=0.65),border=NA))
with(rLUC[[4]],points(value ~ Date, type = "l",col=1,lwd=0.75,lty=2))
with(rLUC[[2]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("lightskyblue",alpha.f=0.85),border=NA))
with(rLUC[[2]],points(value ~ Date, type = "l",col=1,lwd=0.75,lty=2))
legend("topright", y = NULL, 
       legend=c("aT-Con","aT-Drt","eT-Con","eT-Drt"), 
       col = c(adjustcolor("blue",alpha.f=0.65),adjustcolor("lightskyblue",alpha.f=0.85),adjustcolor("red",alpha.f=0.65),adjustcolor("red4",alpha.f=0.65)),lwd=c(5,5,5,5),cex=0.65, bty='n')
legend("topright", y = NULL, 
       legend=c("aT-Con","aT-Drt","eT-Con","eT-Drt"),lty=c(1,3,1,3),lwd=1.5,cex=0.65,bty='n')

#Lower sensors
par(mar=c(0,0,0,3))
with(rLUC[[5]],plot(value ~ Date, 
                    type = "l",
                    bty='l',
                    ylim = c(-0.01,0.165),
                    ylab="",
                    xlab="",
                    col=1,
                    cex.axis=0.65))
with(rLUC[[7]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("red",alpha.f=0.65),border=NA))
with(rLUC[[7]],points(value ~ Date, type = "l",col=1,lwd=0.75))
with(rLUC[[5]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("blue",alpha.f=0.65),border=NA))
with(rLUC[[5]],points(value ~ Date, type = "l",col=1,lwd=0.75,lty=1))
with(rLUC[[8]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("red4",alpha.f=0.65),border=NA))
with(rLUC[[8]],points(value ~ Date, type = "l",col=1,lwd=0.75,lty=2))
with(rLUC[[6]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("lightskyblue",alpha.f=0.85),border=NA))
with(rLUC[[6]],points(value ~ Date, type = "l",col=1,lwd=0.75,lty=2))

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
text(x = corners2[2]+corners2[2]/800, y = mean(corners2[3:4])-mean(corners2[3:4])/1.15, "Irrigation\n(mm)",srt=270,cex=0.55)
text(x = corners2[2]+corners2[2]/1800, y = mean(corners2[3:4])-mean(corners2[3:4])/1.165, "20\n10\n0",las=0,cex=0.55)
axis(side=4,at=c(0,10,20),las=2,labels=c("","",""))

mtext(text="Soil Volumetric Water Content (%)",side=2,line=2,outer=TRUE)

dev.off()

#FES####
#calculate max and min values for ylim, remove NAs
ymax<-max(FES1$upper,na.rm=T) 
ymin<-min(FES1$lower,na.rm=T) 

#export graphs to tiff
tiff(file = paste("FIELD_FES_Daily_Soil_Moisture_",sD,"_",eD,".tiff",sep=""), width = 1600, height = 1050, units = "px", res = 200)
#Raw soil moisture plot
par(mar=c(5.1,4.1,4.1,3.1))
with(rFES[[1]],plot(value ~ Date, 
                    type = "l",
                    bty='l',
                    ylim = c(ymin-0.05,ymax),
                    ylab="",
                    xlab="",
                    col=1,
                    cex.axis=0.65))
mtext(side=2,"Soil Volumetric Water Content (%)",padj=-3.5)
with(rFES[[3]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("red",alpha.f=0.65),border=NA))
with(rFES[[3]],points(value ~ Date, type = "l",col=1,lwd=0.75))
with(rFES[[1]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("blue",alpha.f=0.65),border=NA))
with(rFES[[1]],points(value ~ Date, type = "l",col=1,lwd=0.75,lty=1))
with(rFES[[4]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("red4",alpha.f=0.65),border=NA))
with(rFES[[4]],points(value ~ Date, type = "l",col=1,lwd=0.75,lty=2))
with(rFES[[2]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("lightskyblue",alpha.f=0.85),border=NA))
with(rFES[[2]],points(value ~ Date, type = "l",col=1,lwd=0.75,lty=2))
legend("topleft", y = NULL, 
       legend=c("aT-Con","aT-Drt","eT-Con","eT-Drt"), 
       col = c(adjustcolor("blue",alpha.f=0.65),adjustcolor("lightskyblue",alpha.f=0.85),adjustcolor("red",alpha.f=0.65),adjustcolor("red4",alpha.f=0.65)),lwd=c(5,5,5,5),cex=0.65, bty='n')
legend("topleft", y = NULL, 
       legend=c("aT-Con","aT-Drt","eT-Con","eT-Drt"),lty=c(1,3,1,3),lwd=1.5,cex=0.65,bty='n')
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
text(x = corners2[2]+corners2[2]/850, y = mean(corners2[3:4])-mean(corners2[3:4])/1.15, "Irrigation\n(mm)",srt=270,cex=0.6)
text(x = corners2[2]+corners2[2]/1800, y = mean(corners2[3:4])-mean(corners2[3:4])/1.165, "20\n\n10\n\n0",las=0,cex=0.55)
axis(side=4,at=c(0,10,20),las=2,labels=c("","",""))

dev.off()

#Bis####
#calculate max and min values for ylim, remove NAs
ymax<-max(BIS1$upper,na.rm=T)
ymin<-min(BIS1$lower,na.rm=T)

#export graphs to tiff
tiff(file = paste("FIELD_BIS_Daily_Soil_Moisture_",sD,"_",eD,".tiff",sep=""), width = 3200, height = 2100, units = "px", res = 400)
#Raw soil moisture plot
par(mar=c(5.1,4.1,4.1,3.1))
with(rBIS[[1]],plot(value ~ Date, 
                    type = "l",
                    bty='l',
                    ylim = c(ymin-0.05,ymax),
                    ylab="",
                    xlab="",
                    col=1,
                    cex.axis=0.65))
mtext(side=2,"Soil Volumetric Water Content (%)",padj=-3.5)
with(rBIS[[1]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("blue",alpha.f=0.65),border=NA))
with(rBIS[[1]],points(value ~ Date, type = "l",col=1,lwd=0.75,lty=1))
with(rBIS[[2]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("lightskyblue",alpha.f=0.85),border=NA))
with(rBIS[[2]],points(value ~ Date, type = "l",col=1,lwd=0.75,lty=2))
legend("topleft", y = NULL, 
       legend=c("aT-Con","aT-Drt"), 
       col = c(adjustcolor("blue",alpha.f=0.65),adjustcolor("lightskyblue",alpha.f=0.85)),lwd=c(5,5),cex=0.65, bty='n')
legend("topleft", y = NULL, 
       legend=c("aT-Con","aT-Drt"),lty=c(1,3),lwd=1.5,cex=0.65,bty='n')
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
text(x = corners2[2]+corners2[2]/850, y = mean(corners2[3:4])-mean(corners2[3:4])/1.15, "Irrigation\n(mm)",srt=270,cex=0.6)
text(x = corners2[2]+corners2[2]/1800, y = mean(corners2[3:4])-mean(corners2[3:4])/1.165, "20\n\n10\n\n0",las=0,cex=0.55)
axis(side=4,at=c(0,10,20),las=2,labels=c("","",""))

dev.off()

#Rye####
#calculate max and min values for ylim, remove NAs
ymax<-max(RYE1$upper,na.rm=T)
ymin<-min(RYE1$lower,na.rm=T)

#export graphs to tiff
tiff(file = paste("FIELD_RYE_Daily_Soil_Moisture_",sD,"_",eD,".tiff",sep=""), width = 3200, height = 2100, units = "px", res = 400)
#Raw soil moisture plot
par(mar=c(5.1,4.1,4.1,3.1))
with(rRYE[[1]],plot(value ~ Date, 
                    type = "l",
                    bty='l',
                    ylim = c(ymin-0.05,ymax),
                    ylab="",
                    xlab="",
                    col=1,
                    cex.axis=0.65))
mtext(side=2,"Soil Volumetric Water Content (%)",padj=-3.5)
with(rRYE[[1]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("blue",alpha.f=0.65),border=NA))
with(rRYE[[1]],points(value ~ Date, type = "l",col=1,lwd=0.75,lty=1))
with(rRYE[[2]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("lightskyblue",alpha.f=0.85),border=NA))
with(rRYE[[2]],points(value ~ Date, type = "l",col=1,lwd=0.75,lty=2))
legend("topright", y = NULL, 
       legend=c("aT-Con","aT-Drt"), 
       col = c(adjustcolor("blue",alpha.f=0.65),adjustcolor("lightskyblue",alpha.f=0.85)),lwd=c(5,5),cex=0.65, bty='n')
legend("toprigh", y = NULL, 
       legend=c("aT-Con","aT-Drt"),lty=c(1,3),lwd=1.5,cex=0.65,bty='n')
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
text(x = corners2[2]+corners2[2]/850, y = mean(corners2[3:4])-mean(corners2[3:4])/1.15, "Irrigation\n(mm)",srt=270,cex=0.6)
text(x = corners2[2]+corners2[2]/1800, y = mean(corners2[3:4])-mean(corners2[3:4])/1.165, "20\n\n10\n\n0",las=0,cex=0.55)
axis(side=4,at=c(0,10,20),las=2,labels=c("","",""))

dev.off()
