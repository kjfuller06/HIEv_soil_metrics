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

##export to tiff file
tiff(file = paste("FIELD_LUC_Daily_Soil_Temperature",sD,"_",eD,".tiff",sep=""), width = 3200, height = 2100, units = "px", res = 400) 
#Temperature plot
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

##export to tiff file
tiff(file = paste("FIELD_FES_Daily_Soil_Temperature",sD,"_",eD,".tiff",sep=""), width = 3200, height = 2100, units = "px", res = 400)  
#Temperature plot
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