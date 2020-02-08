#Compare soil moisture between control and treatments####
#LUC####

#export graphs to tiff
tiff(file = paste("FIELD_LUC_Comp_Soil_Moisture_",sD,"_",eD,".tiff",sep=""), width = 1600, height = 1050, units = "px", res = 200) 
#Raw soil moisture plot
par(mfrow=c(2,1),oma=c(4,3,3,0))
#Upper sensors
par(mar =c(0,0,0,3))
with(CLUC[[2]],plot(value ~ Date, 
     type = "l",lwd=0.75,
     xaxt='n',
     ylim = c(-0.10,0.10),
     ylab="",
     xlab="",
     main="",
     col=1,
     lty=2,
     cex.axis=0.75))
mtext(side=3,"Surface to 15cm",padj=2,cex=0.75)
with(CLUC[[2]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("lightskyblue",alpha.f=0.5),border=NA))
with(CLUC[[2]],points(value ~ Date, type = "l",col=1,lwd=0.75,lty=2))
with(CLUC[[3]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("red",alpha.f=0.5),border=NA))
with(CLUC[[3]],points(value ~ Date, type = "l",col=1,lwd=0.75))
with(CLUC[[4]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("red4",alpha.f=0.5),border=NA))
with(CLUC[[4]],points(value ~ Date, type = "l",col=1,lwd=0.75,lty=2))
legend("topright", y = NULL, 
       legend=c("aT-Con","aT-Drt","eT-Con","eT-Drt"), 
       col = c(1,adjustcolor("lightskyblue",alpha.f=0.5),adjustcolor("red",alpha.f=0.5),adjustcolor("red4",alpha.f=0.5)),lty=c(2,1,1,1),lwd=c(1.5,10,10,10),cex=0.75)
legend("topright", y = NULL, 
       legend=c("aT-Con","aT-Drt","eT-Con","eT-Drt"), 
       col = c(1,1,1,1),lty=c(2,2,1,2),lwd=1.5,cex=0.75,bty='n')
abline(h=0,lty=2)

#Lower sensors
par(mar = c(1,0,0,3))
with(CLUC[[6]],plot(value ~ Date, 
     type = "l",lwd=0.75,
     ylim = c(-0.10,0.09),
     ylab="",
     xlab="",
     col=1,
     lty=2,
     cex.main=0.75,
     cex.axis=0.75,
     xaxt='n'))
with(CLUC[[6]],axis.Date(side=1,x=Date,cex.axis=0.75))
box(which = "plot", lty = "solid")
mtext(side=3,"15cm to 30cm",padj=2,cex=0.75)
with(CLUC[[6]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("lightskyblue",alpha.f=0.5),border=NA))
with(CLUC[[6]],points(value ~ Date, type = "l",col=1,lwd=0.75,lty=2))
with(CLUC[[7]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("red",alpha.f=0.5),border=NA))
with(CLUC[[7]],points(value ~ Date, type = "l",col=1,lwd=0.75))
with(CLUC[[8]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("red4",alpha.f=0.5),border=NA))
with(CLUC[[8]],points(value ~ Date, type = "l",col=1,lwd=0.75,lty=2))
# arrows(x0 =as.Date("2018-08-23"),length=0.05, y0 = 0.025, y1 = -0.015)
# text(x = as.Date("2018-08-23"), y = 0.03,labels=expression(paste("Drought Treatment Initiated")),cex=0.6)
abline(h=0,lty=2)

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
# ymax<-max(FES1$upper,na.rm=T) 
# ymin<-min(FES1$lower,na.rm=T) 

#export graphs to tiff
tiff(file = paste("FIELD_FES_Comp_Soil_Moisture_",sD,"_",eD,".tiff",sep=""), width = 1600, height = 1050, units = "px", res = 200)
#Comparative soil moisture plot
par(mar=c(5.1,5.1,4.1,3.1))
with(CFES[[2]],plot(value ~ Date, 
                    type = "l",
                    bty='l',
                    ylim = c(-0.12,0.10),
                    ylab="",
                    xlab="",
                    col=1,
                    lty=2))
mtext(side=2,"Soil Volumetric Water Content (%)",padj=-3.5)
with(CFES[[3]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("red",alpha.f=0.65),border=NA))
with(CFES[[3]],points(value ~ Date, type = "l",col=1,lwd=0.75))
with(CFES[[4]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("red4",alpha.f=0.65),border=NA))
with(CFES[[4]],points(value ~ Date, type = "l",col=1,lwd=0.75,lty=2))
with(CFES[[2]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("lightskyblue",alpha.f=0.85),border=NA))
with(CFES[[2]],points(value ~ Date, type = "l",col=1,lwd=0.75,lty=2))
legend("topleft", y = NULL, 
       legend=c("aT-Con","aT-Drt","eT-Con","eT-Drt"), 
       col = c(1,adjustcolor("lightskyblue",alpha.f=0.85),adjustcolor("red",alpha.f=0.65),adjustcolor("red4",alpha.f=0.65)),lty=c(2,1,1,1),lwd=c(1.5,10,10,10),cex=0.75)
legend("topleft", y = NULL, 
       legend=c("aT-Con","aT-Drt","eT-Con","eT-Drt"), 
       col = c(1,1,1,1),lty=c(2,2,1,2),lwd=1.5,cex=0.75,bty='n')
# arrows(x0 =as.Date("2018-06-01"),length=0.05, y0 = 0.03, y1 = 0.01)
# text(x = as.Date("2018-06-01"), y = 0.035,labels=expression(paste("Drought Treatment Initiated")),cex=0.6)
abline(h=0,lty=2)

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


#BIS####
#calculate max and min values for ylim, remove NAs
# ymax<-max(BIS$upper,na.rm=T)
# ymin<-min(BIS$lower,na.rm=T)

#export graphs to tiff
tiff(file = paste("FIELD_BIS_Comp_Soil_Moisture_",sD,"_",eD,".tiff",sep=""), width = 1600, height = 1050, units = "px", res = 200)
#Raw soil moisture plot
par(mar=c(5.1,4.1,4.1,3.1))
with(CBIS[[2]],plot(value ~ Date, 
     type = "l",
     ylim = c(-0.15,0.05),
     ylab="",
     xlab="",
     main=paste("Soil Water Content in BISerrula Plots by Treatment\n",sD,"-",eD),
     col=1,
     lty=2))
mtext(side=2,"Soil Water Content",padj=-3.5)
mtext(side=1,"Date",padj=3.5)
with(CBIS[[2]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("lightskyblue",alpha.f=0.5),border=NA))
with(CBIS[[2]],points(value ~ Date, type = "l",col=1,lwd=0.75,lty=2))
legend("topright", y = NULL, 
       legend=c("aT-Con","aT-Drt"), 
       col = c(1,adjustcolor("lightskyblue",alpha.f=0.5)),lty=c(2,1),lwd=c(1.5,10),cex=0.75)
legend("topright", y = NULL, 
       legend=c("aT-Con","aT-Drt"), 
       col = c(1,1),lty=c(2,2),lwd=1.5,cex=0.75,bty='n')
# arrows(x0 =as.Date("2018-08-22"),length=0.05, y0 = 0.03, y1 = 0.0)
# text(x = as.Date("2018-08-22"), y = 0.035,labels=expression(paste("Drought Treatment Initiated")),cex=0.6)
abline(h=0,lty=2)

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
# ymax<-max(RYE1$upper,na.rm=T)
# ymin<-min(RYE1$lower,na.rm=T)

#export graphs to tiff
tiff(file = paste("FIELD_RYE_Comp_Soil_Moisture_",sD,"_",eD,".tiff",sep=""), width = 1600, height = 1050, units = "px", res = 200)
#Raw soil moisture plot
par(mar=c(5.1,4.1,4.1,3.1))
with(CRYE[[2]],plot(value ~ Date, 
     type = "l",
     ylim = c(-0.15,0.05),
     ylab="",
     xlab="",
     main=paste("Soil Water Content in Rye Plots by Treatment\n",sD,"-",eD),
     col=1,
     lty=2))
mtext(side=2,"Soil Water Content",padj=-3.5)
mtext(side=1,"Date",padj=3.5)
with(CRYE[[2]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("lightskyblue",alpha.f=0.5),border=NA))
with(CRYE[[2]],points(value ~ Date, type = "l",col=1,lwd=0.75,lty=2))
legend("topright", y = NULL, 
       legend=c("aT-Con","aT-Drt"), 
       col = c(1,adjustcolor("lightskyblue",alpha.f=0.5)),lty=c(2,1),lwd=c(1.5,10),cex=0.75)
legend("topright", y = NULL, 
       legend=c("aT-Con","aT-Drt"), 
       col = c(1,1),lty=c(2,2),lwd=1.5,cex=0.75,bty='n')
# arrows(x0 =as.Date("2018-08-22"),length=0.05, y0 = 0.03, y1 = 0.0)
# text(x = as.Date("2018-08-22"), y = 0.035,labels=expression(paste("Drought Treatment Initiated")),cex=0.6)
abline(h=0,lty=2)

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