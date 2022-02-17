#multipanel
tiff(file = "MarinhoCatunda_etal_2021_Figure2.tiff", width =2200, height = 2800, units = "px", res = 400)

## 2a - Fescue ####
#calculate max and min values for ylim, remove NAs
ymax<-max(FES1$upper,na.rm=T) 
ymin<-min(FES1$lower,na.rm=T) 

par(cex=0.7, mai=c(0.1,0.1,0.1,0.35))
#height increments: 0.23125
par(fig=c(0.1,0.99,0.76875,1),new=TRUE)
with(rFES[[1]],plot(value ~ Date, 
                    type = "l",
                    bty='l',
                    ylim = c(ymin-5,ymax+5),
                    ylab="",
                    xlab="",
                    col=1,
                    xaxt='n',
                    cex.axis=0.65))
mtext(side=2,"Soil Volumetric\nWater Content (%)",padj=-1.5,cex=.5)
mtext(side=2,"A)",line=3.5,cex=0.9,las=2)
with(rFES[[3]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("red4",alpha.f=0.65),border=NA))
with(rFES[[3]],points(value ~ Date, type = "l",col=1,lwd=0.75))
with(rFES[[1]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("blue",alpha.f=0.65),border=NA))
with(rFES[[1]],points(value ~ Date, type = "l",col=1,lwd=0.75,lty=1))
with(rFES[[4]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("red",alpha.f=0.65),border=NA))
with(rFES[[4]],points(value ~ Date, type = "l",col=1,lwd=0.75,lty=2))
with(rFES[[2]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("lightskyblue",alpha.f=0.85),border=NA))
with(rFES[[2]],points(value ~ Date, type = "l",col=1,lwd=0.75,lty=2))
text("Tall Fescue", x = min(rFES[[1]]$Date), y = ymax+5, adj = 0)

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
text(x = corners2[2]+corners2[2]/350, y = mean(corners2[3:4])-mean(corners2[3:4])/1.15, "Irrigation\n(mm)",srt=270,cex=0.6)
text(x = corners2[2]+corners2[2]/750, y = mean(corners2[3:4])-mean(corners2[3:4])/1.165, "20\n10\n0",las=0,cex=0.55)
axis(side=4,at=c(0,10,20),las=2,labels=c("","",""))

## 2b - Lucerne ####
ymax<-max(LUC1$upper,na.rm=T) 
ymin<-min(LUC1$lower,na.rm=T) 

par(fig=c(0.1,0.99,0.5375,0.76875),new=TRUE)
with(rLUC[[1]],plot(value ~ Date, 
                    type = "l",
                    bty='l',
                    ylim = c(ymin-5,ymax+5),
                    ylab="",
                    xlab="",
                    col=1,
                    xaxt='n',
                    cex.axis=0.65))
mtext(side=2,"Soil Volumetric\nWater Content (%)",padj=-1.5,cex=.5)
mtext(side=2,"B)",line=3.5,cex=0.9,las=2)
with(rLUC[[3]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("red4",alpha.f=0.65),border=NA))
with(rLUC[[3]],points(value ~ Date, type = "l",col=1,lwd=0.75))
with(rLUC[[1]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("blue",alpha.f=0.65),border=NA))
with(rLUC[[1]],points(value ~ Date, type = "l",col=1,lwd=0.75,lty=1))
with(rLUC[[4]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("red",alpha.f=0.65),border=NA))
with(rLUC[[4]],points(value ~ Date, type = "l",col=1,lwd=0.75,lty=2))
with(rLUC[[2]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("lightskyblue",alpha.f=0.85),border=NA))
with(rLUC[[2]],points(value ~ Date, type = "l",col=1,lwd=0.75,lty=2))
text("Lucerne", x = min(rLUC[[1]]$Date), y = ymax+5, adj = 0)

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
plot(Irrig_luc$Irrigation~Irrig_luc$Date,
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
text(x = corners2[2]+corners2[2]/350, y = mean(corners2[3:4])-mean(corners2[3:4])/1.15, "Irrigation\n(mm)",srt=270,cex=0.6)
text(x = corners2[2]+corners2[2]/750, y = mean(corners2[3:4])-mean(corners2[3:4])/1.165, "20\n10\n0",las=0,cex=0.55)
axis(side=4,at=c(0,10,20),las=2,labels=c("","",""))

## Figure 2C: Max surface temperatures
ymax<-max(surf$maxT,na.rm=T) 
ymin<-min(airT$value,na.rm=T) 

#Raw surface temperature plot
par(fig=c(0.1,0.99,0.30625,0.5375),new=TRUE)
with(max[[1]],plot(maxT ~ Date, 
                   type = "l",
                   bty='l',
                   ylim = c(ymin-0.05,ymax),
                   ylab="",
                   xlab="",
                   col="blue",
                   xaxt='n',
                   cex.axis=0.65))
mtext(side=2,expression(paste("Temperature (",degree~C,")")),padj=-3,cex=.5)
mtext(side=2,"C)",line=3.5,cex=0.9,las=2)
# with(max[[3]],polygon(c(Date,rev(Date)),c(maxT[,3],rev(maxT[,2])),col=adjustcolor("red",alpha.f=0.65),border=NA))
with(max[[3]],points(maxT ~ Date, type = "l",col="red4"))
# with(max[[4]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("red4",alpha.f=0.65),border=NA))
with(max[[4]],points(maxT ~ Date, type = "l",col="red",lty=2))
# with(max[[2]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("lightskyblue",alpha.f=0.85),border=NA))
with(max[[2]],points(maxT ~ Date, type = "l",col="lightskyblue",lty=2))
with(airT,points(maxT~Date,type="l"))
# arrows(x0 =as.Date("2018-06-01"),length=0.05, y0 = 0.03, y1 = 0.01)
# text(x = as.Date("2018-06-01"), y = 0.035,labels=expression(paste("Drought Treatment Initiated")),cex=0.6)
corners = par("usr") #Gets the four corners of plot area (x1, x2, y1, y2)
clip(corners[1],corners[2],ymin,ymax)
abline(h=40,lty=2)
legend(legend=c("Maximum Air Temp"),lty=c(1),col=c("black"),lwd=1.5,cex=0.5,box.lwd=0,box.col="white",bg="white",x=min(max[[1]]$Date)-7,y=ymax+1)

#Figure 2D: Min surface temperatures
ymax<-max(surf$minT,na.rm=T) 
ymin<-min(surf$minT,na.rm=T) 

#Raw surface temperature plot
par(fig=c(0.1,0.99,0.075,0.30625),new=TRUE)
with(min[[1]],plot(minT ~ Date, 
                   type = "l",
                   bty='l',
                   ylim = c(ymin-5,ymax),
                   ylab="",
                   xlab="",
                   col="blue",
                   xaxt = 'n',
                   cex.axis=0.65))
mtext(side=2,expression(paste("Temperature (",degree~C,")")),padj=-3,cex=.5)
mtext(side=2,"D)",line=3.5,cex=0.9,las=2)
# with(min[[3]],polygon(c(Date,rev(Date)),c(minT[,3],rev(minT[,2])),col=adjustcolor("red",alpha.f=0.65),border=NA))
with(min[[3]],points(minT ~ Date, type = "l",col="red4"))
# with(min[[4]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("red4",alpha.f=0.65),border=NA))
with(min[[4]],points(minT ~ Date, type = "l",col="red",lty=2))
# with(min[[2]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("lightskyblue",alpha.f=0.85),border=NA))
with(min[[2]],points(minT ~ Date, type = "l",col="lightskyblue",lty=2))
with(airT,points(minT~Date,type="l"))
legend(legend=c("Minimum Air Temp"),lty=c(1),col=c("black"),lwd=1.5,cex=0.5,box.lwd=0,box.col="white",bg="white",x=min(max[[1]]$Date)-7,y=ymax)
corners = par("usr") #Gets the four corners of plot area (x1, x2, y1, y2)
clip(corners[1],corners[2],ymin-5,ymax+1)
abline(h=0,lty=2)
axis.Date(side = 1, at = c("2018-07-01", "2019-01-01", "2019-07-01",  "2020-01-2-01"), format = "%b-%y")

dev.off()
