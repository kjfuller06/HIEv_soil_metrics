sp="PhaSub"
d<-"October 2019"

labels<-read.csv("labels.csv")

start<-function(){
  plot(1,bty='n',xaxt='n',yaxt='n',type="n",xlab="",ylab="",xlim=c(0, 10),ylim=c(0, 10))
}
blanklabel<-function(){
  start()
  text(5,9,labels="PACE-Power",cex=1.5)
  text(5,8,labels="Aboveground Harvest",cex=1.5)
  text(5,7,labels=d,cex=1.5)
  text(5,6,labels=sp,cex=1.5)
  text(0,3,labels="Subplot ID__________",pos=4)
  text(0,2,labels="Sample Type___________",pos=4)
}

df1<-labels[labels$PlotType==sp,]

#blank- no subplot ID
pdf(paste(sp,"blank.pdf"))
par(mar=c(1,0,1,0))
par(mfrow=c(4,2))
for (i in c(1:8)){
  blanklabel()
}
dev.off()

#Live subsample
pdf(paste(sp,"Live.pdf"),onefile = TRUE)
par(mar=c(1,0,1,0))
par(mfrow=c(4,2))
if(df1$NoSp[1]==1){
  for (i in df1$SubplotID){
    start()
    text(5,9,labels="PACE-Power",cex=1.5)
    text(5,8,labels="Aboveground Harvest",cex=1.5)
    text(5,7,labels=d,cex=1.5)
    text(5,6,labels=sp,cex=1.5)
    text(0,3,labels=i,pos=4)
    text(0,2,labels="Live target",pos=4)
    }
  } else{
    for (i in df1$SubplotID){
      start()
      text(5,9,labels="PACE-Power",cex=1.5)
      text(5,8,labels="Aboveground Harvest",cex=1.5)
      text(5,7,labels=d,cex=1.5)
      text(5,6,labels=sp,cex=1.5)
      text(0,3,labels=i,pos=4)
      text(0,2,labels=paste("Live",df1$Sp1[1]),pos=4)
    }
    for (i in df1$SubplotID){
      start()
      text(5,9,labels="PACE-Power",cex=1.5)
      text(5,8,labels="Aboveground Harvest",cex=1.5)
      text(5,7,labels=d,cex=1.5)
      text(5,6,labels=sp,cex=1.5)
      text(0,3,labels=i,pos=4)
      text(0,2,labels=paste("Live",df1$Sp2[1]),pos=4)
    }
    }
if(length(droplevels(df1$SubplotID))==12){
  for (i in c(1:4)){
    blanklabel()
  }
}

dev.off()

#Dead subsample
pdf(paste(sp,"Dead.pdf"),onefile = TRUE)
par(mar=c(1,0,1,0))
par(mfrow=c(4,2))
for (i in df1$SubplotID){
  start()
  text(5,9,labels="PACE-Power",cex=1.5)
  text(5,8,labels="Aboveground Harvest",cex=1.5)
  text(5,7,labels=d,cex=1.5)
  text(5,6,labels=sp,cex=1.5)
  text(0,3,labels=i,pos=4)
  text(0,2,labels="Dead",pos=4)
}
if(length(droplevels(df1$SubplotID))==12){
  for (i in c(1:4)){
    blanklabel()
  }
}

dev.off()

#Karen subsample
pdf(paste(sp,"Catunda.pdf"),onefile = TRUE)
par(mar=c(1,0,1,0))
par(mfrow=c(4,2))
if(df1$NoSp[1]==1){
  for (i in df1$SubplotID){
    start()
    text(5,9,labels="PACE-Power",cex=1.5)
    text(5,8,labels="Aboveground Harvest",cex=1.5)
    text(5,7,labels=d,cex=1.5)
    text(5,6,labels=sp,cex=1.5)
    text(0,3,labels=i,pos=4)
    text(0,2,labels="Catunda",pos=4)
  }
} else{
  for (i in df1$SubplotID){
    start()
    text(5,9,labels="PACE-Power",cex=1.5)
    text(5,8,labels="Aboveground Harvest",cex=1.5)
    text(5,7,labels=d,cex=1.5)
    text(5,6,labels=sp,cex=1.5)
    text(0,3,labels=i,pos=4)
    text(0,2,labels=paste("Catunda",df1$Sp1[1]),pos=4)
  }
  for (i in df1$SubplotID){
    start()
    text(5,9,labels="PACE-Power",cex=1.5)
    text(5,8,labels="Aboveground Harvest",cex=1.5)
    text(5,7,labels=d,cex=1.5)
    text(5,6,labels=sp,cex=1.5)
    text(0,3,labels=i,pos=4)
    text(0,2,labels=paste("Catunda",df1$Sp2[1]),pos=4)
  }
}
if(length(droplevels(df1$SubplotID))==12){
  for (i in c(1:4)){
    blanklabel()
  }
}

dev.off()