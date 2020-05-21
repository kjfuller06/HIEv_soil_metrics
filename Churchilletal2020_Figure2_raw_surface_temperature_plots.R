#visualise raw surface temperature data####
#format for graphing
max<-subset(surf,select=c(-value,-minT))
max<-split(max,droplevels(max$Treatment))

min<-subset(surf,select=c(-value,-maxT))
min<-split(min,droplevels(min$Treatment))



#max temp graph####
#calculate max and min values for ylim, remove NAs
ymax<-max(surf$maxT,na.rm=T) 
ymin<-min(airT$value,na.rm=T) 

#export graphs to tiff
tiff(file = paste("FIELD_Max_Surface_Temp_",sD,"_",eD,".tiff",sep=""), width = 1600, height = 1050, units = "px", res = 200)
#Raw surface temperature plot
par(mar=c(5.1,4.1,4.1,3.1))
with(max[[1]],plot(maxT[,1] ~ Date, 
     type = "l",
     bty='l',
     ylim = c(ymin-0.05,ymax),
     ylab="",
     xlab="",
     col="blue"))
abline(h=40,lty=2)
# with(max[[1]],rect(min(Date)-25,40,max(Date)+25,45,col=adjustcolor(1,alpha.f=0.05),border=NA))
abline(h=45,lty=2)
# with(max[[1]],rect(min(Date)-25,45,max(Date)+25,65,col=adjustcolor(1,alpha.f=0.15),border=NA))
mtext(side=2,expression(paste("Temperature (",degree~C,")")),padj=-2.5)
# with(max[[3]],polygon(c(Date,rev(Date)),c(maxT[,3],rev(maxT[,2])),col=adjustcolor("red",alpha.f=0.65),border=NA))
with(max[[3]],points(maxT[,1] ~ Date, type = "l",col="red"))
# with(max[[4]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("red4",alpha.f=0.65),border=NA))
with(max[[4]],points(maxT[,1] ~ Date, type = "l",col="red4",lty=2))
# with(max[[2]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("lightskyblue",alpha.f=0.85),border=NA))
with(max[[2]],points(maxT[,1] ~ Date, type = "l",col="lightskyblue",lty=2))
with(airT,points(value~Date,type="l"))
# legend("topleft", y = NULL,
#        legend=c("aT-Con","aT-Drt","eT-Con","eT-Drt"), 
#        col = c(adjustcolor("blue",alpha.f=0.65),adjustcolor("lightskyblue",alpha.f=0.85),adjustcolor("red",alpha.f=0.65),adjustcolor("red4",alpha.f=0.65)),lwd=c(10,10,10,10),cex=0.75, bty='n')
legend("topright", y = NULL, 
       legend=c("aT-Con","aT-Drt","eT-Con","eT-Drt","Air Temp"),lty=c(1,2,1,2,1),col=c("blue","lightskyblue","red","red4","black"),lwd=1.5,cex=0.75,bty='n')
# arrows(x0 =as.Date("2018-06-01"),length=0.05, y0 = 0.03, y1 = 0.01)
# text(x = as.Date("2018-06-01"), y = 0.035,labels=expression(paste("Drought Treatment Initiated")),cex=0.6)

dev.off()


#min temp graph####
#calculate max and min values for ylim, remove NAs
ymax<-max(surf$minT,na.rm=T) 
ymin<-min(surf$minT,na.rm=T) 

#export graphs to tiff
tiff(file = paste("FIELD_Min_Surface_Temp_",sD,"_",eD,".tiff",sep=""), width = 1600, height = 1050, units = "px", res = 200)
#Raw surface temperature plot
par(mar=c(5.1,4.1,4.1,3.1))
with(min[[1]],plot(minT[,1] ~ Date, 
                   type = "l",
                   bty='l',
                   ylim = c(ymin-0.05,ymax),
                   ylab="",
                   xlab="",
                   col="blue"))
abline(h=0,lty=2)
# with(min[[1]],rect(min(Date)-25,0,min(Date)+25,-5,col=adjustcolor(1,alpha.f=0.05),border=NA))
abline(h=-5,lty=2)
# with(min[[1]],rect(min(Date)-25,-5,min(Date)+25,-25,col=adjustcolor(1,alpha.f=0.15),border=NA))
mtext(side=2,expression(paste("Surface Temperature (",degree~C,")")),padj=-2.5)
# with(min[[3]],polygon(c(Date,rev(Date)),c(minT[,3],rev(minT[,2])),col=adjustcolor("red",alpha.f=0.65),border=NA))
with(min[[3]],points(minT[,1] ~ Date, type = "l",col="red"))
# with(min[[4]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("red4",alpha.f=0.65),border=NA))
with(min[[4]],points(minT[,1] ~ Date, type = "l",col="red4",lty=2))
# with(min[[2]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("lightskyblue",alpha.f=0.85),border=NA))
with(min[[2]],points(minT[,1] ~ Date, type = "l",col="lightskyblue",lty=2))
with(airT,points(value~Date,type="l"))
# legend("topleft", y = NULL,
#        legend=c("aT-Con","aT-Drt","eT-Con","eT-Drt"), 
#        col = c(adjustcolor("blue",alpha.f=0.65),adjustcolor("lightskyblue",alpha.f=0.85),adjustcolor("red",alpha.f=0.65),adjustcolor("red4",alpha.f=0.65)),lwd=c(10,10,10,10),cex=0.75, bty='n')
legend("topleft", y = NULL, 
       legend=c("aT-Con","aT-Drt","eT-Con","eT-Drt","Air Temp"),lty=c(1,2,1,2,1),col=c("blue","lightskyblue","red","red4","black"),lwd=1.5,cex=0.75,bty='n')
# arrows(x0 =as.Date("2018-06-01"),length=0.05, y0 = 0.03, y1 = 0.01)
# text(x = as.Date("2018-06-01"), y = 0.035,labels=expression(paste("Drought Treatment Initiated")),cex=0.6)

dev.off()

