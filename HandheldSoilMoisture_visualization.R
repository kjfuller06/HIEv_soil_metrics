dfr<-read.csv(file="PACE_handheldsoilmoisture_master.csv")
levels(dfr$Date)[20]<-"2018-05-21"
dfr$Date<-as.Date(dfr$Date)
treat<-read.csv(file="PACE_treatment_species_reference.csv")
dfr<-merge(dfr,treat)
dfr<-dfr[,-c(3,4)]
dfr$treatment<-paste(dfr$Temp,dfr$Rain,sep="")
dfr<-dfr[order(dfr$Subplot.ID,dfr$Date),]
dfr$moisture.avg<-dfr$moisture.avg/100
dfr$moisture.sterror<-dfr$moisture.sterror/100


for(x in dfr$Species){
  df<-subset(dfr, Species == x)
  assign(paste("df",x,sep=""),df)
  assign(paste("ymin",x,sep=""),min(df$moisture.avg))
  assign(paste("ymax",x,sep=""),max(df$moisture.avg))
  
}

dfr1<-aggregate(data=dfLuc,moisture.avg~treatment+Date,FUN=mean)

tiff(file = paste("FIELD",x,"Daily Soil_Moisture",eD, ".tiff"), width = 3200, height = 2100, units = "px", res = 400) 
with(dfr1[dfr1$treatment=="aTCon",],plot(moisture.avg~Date,
                                  col=2,
                                  ylim=c(yminLuc,ymaxLuc),
                                  type='l',
                                  xaxt='n',
                                  ylab="Volumetric Water Content (%)"))
with(dfr1[dfr1$treatment=="aTDrt",],points(moisture.avg~Date,
                                    col=2,
                                    lty=2,
                                    ylim=c(yminLuc-1,ymaxLuc+1),
                                    type='l',
                                    xaxt='n'))
with(dfr1[dfr1$treatment=="eTCon",],points(moisture.avg~Date,
                                         col=4,
                                         ylim=c(yminLuc,ymaxLuc),
                                         type='l',
                                         xaxt='n',
                                         ylab="Volumetric Water Content (%)"))
with(dfr1[dfr1$treatment=="eTDrt",],points(moisture.avg~Date,
                                           col=4,
                                           lty=2,
                                           ylim=c(yminLuc-1,ymaxLuc+1),
                                           type='l',
                                           xaxt='n'))
axis.Date(side=1,at=seq(min(dfr1$Date),max(dfr1$Date),by="months"),format="%b-%Y")
with(df1,points(value~Date,
                 col=2,
                 type='l',
                 xaxt='n'))
with(df2,points(value~Date,
                col=2,
                lty=2,
                type='l',
                xaxt='n'))
with(df3,points(value~Date,
                col=4,
                type='l',
                xaxt='n'))
with(df4,points(value~Date,
                col=4,
                lty=2,
                type='l',
                xaxt='n'))
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
dev.off()
