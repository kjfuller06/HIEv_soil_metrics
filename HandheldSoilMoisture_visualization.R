library(tidyr)
library(tibble)

dfr<-read.csv(file="PACE_handheldsoilmoisture_master.csv")
levels(dfr$Date)[20]<-"2018-05-21"
dfr$Date<-as.Date(dfr$Date)
treat<-read.csv(file="PACE_treatment_species_reference.csv")
dfr<-merge(dfr,treat)
dfr<-dfr[,-c(3,4)]
dfr$treatment<-paste(dfr$Temp,dfr$Rain,sep="")
dfr<-dfr[order(dfr$Subplot.ID,dfr$Date),] 


for(x in dfr$Species){
  df1<-subset(dfr, Species == x)
  assign(paste("df",x,sep=""),df1)
}

drtxcon<-function(x){
  df1<-aggregate(data=x,moisture.avg~Rain+Date,FUN=mean)
  with(df1[df1$Rain=="Con",],plot(moisture.avg~Date,
                                  col=2,
                                  type='l',
                                  xaxt='n',
                                  ylab="Volumetric Water Content (%)"))
  with(df1[df1$Rain=="Drt",],points(moisture.avg~Date,
                                    col=4,
                                    type='l',
                                    xaxt='n'))
  axis.Date(side=1,at=seq(min(df1$Date),max(df1$Date),by="months"),format="%b-%Y")
}

drtxcon(dfBis)
drtxcon(dfDigBis)
drtxcon(dfDig)
drtxcon(dfKan)
drtxcon(dfPha)
drtxcon(dfRho)
drtxcon(dfRye)
drtxcon(dfWal)

