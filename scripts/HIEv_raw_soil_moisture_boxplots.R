#visualise raw soil moisture data####
#format data for visualisation####
#split data for graphing
sLUC<-split(LUC1,LUC1$Position)
sLUC[[1]]<-droplevels(sLUC[[1]])
sLUC[[2]]<-droplevels(sLUC[[2]])

#LUC####
#calculate max and min values for ylim, remove NAs
ymax<-max(LUC1$upper,na.rm=T) 
ymin<-min(LUC1$lower,na.rm=T) 

#export graphs to tiff
tiff(file = paste("FIELD_Daily_Soil_Moisture_boxes",sD,"_",eD,".tiff",sep=""), width = 3200, height = 2100, units = "px", res = 400) 
#Raw soil moisture plot
par(layout(matrix(c(1, 2, 3, 4,
                1, 2, 3, 4), nrow=2, byrow=TRUE))
       ,oma=c(1,4,3,2))
par(mar =c(2,0,2,0))
with(BIS1,boxplot(value ~ Treat,
                  xaxt='n',
                  xlab="",
                  ylab="Volumetric Soil Moisture Content"))
title(xlab="Biserrula", line=0, cex.lab=1.2, family="Calibri Light")
with(RYE1,boxplot(value ~ Treat,
                  yaxt='n',
                  xaxt='n',
                  xlab="",
                  ylab=""))
title(xlab="Ryegrass", line=0, cex.lab=1.2, family="Calibri Light")
with(FES1,boxplot(value ~ Treat,
                  yaxt='n',
                  xaxt='n',
                  xlab="",
                  ylab=""))
title(xlab="Fescue", line=0, cex.lab=1.2, family="Calibri Light")
with(sLUC[[1]],boxplot(value ~ Treat,
                       yaxt='n',
                       xaxt='n',
                       xlab="",
                       ylab=""))
title(xlab="Lucerne", line=0, cex.lab=1.2, family="Calibri Light")
mtext(text=paste("Soil Moisture Content by Treatment\n",sD,"-",eD),side=3,line=0,outer=TRUE)
mtext(text="Soil Water Content",side=2,line=2,outer=TRUE,cex=0.75)


dev.off()

