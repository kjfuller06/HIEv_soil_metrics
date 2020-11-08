## This script is for generating heatwave data using historical temperature data, air temperature data from PACE and surface temperature from PACE
# 1) load historical temperature data and generate 90% mark for each day of the year
# 2) load PACE data and generate daily maximum metrics
#       2a) bind historical 90% mark to PACE data
#       2b) subset by PACE temperatures that are at or above the 90% historical value
# 3) load PACE surface temperature data
#       3a) subset surface temperatures by selected dates from the above
#       3b) export surface data

# 1) ####
histtemp = read.csv("RAAF_daily_temp.csv") %>% 
        dplyr::select(-Product.code,
                      -Bureau.of.Meteorology.station.number) %>% 
        filter(is.na(Maximum.temperature..Degree.C.) == FALSE & Quality == "Y")
histtemp = aggregate(data = histtemp, Maximum.temperature..Degree.C. ~ Month + Day, FUN = function(x) c(percent90 = quantile(x, probs = 0.9)), simplify = TRUE, drop = TRUE)

# 2) ####
#write start date for first analysis
sD<-as.Date("2018-06-01")
#Write end date for first analysis
eD<-as.Date("2019-05-31")

#download from HIEv and processing
#get data, only keep variables of interest
s1 <- (downloadTOA5("PACE_AUTO_S1_ABVGRND_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,6,12:15)]
s2 <- (downloadTOA5("PACE_AUTO_S2_ABVGRND_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,6,12:15)]
s3 <- (downloadTOA5("PACE_AUTO_S3_ABVGRND_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,6,12:15)]
s4 <- (downloadTOA5("PACE_AUTO_S4_ABVGRND_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,6,12:15)]
s5 <- (downloadTOA5("PACE_AUTO_S5_ABVGRND_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,6,12:15)]
s6 <- (downloadTOA5("PACE_AUTO_S6_ABVGRND_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,6,12:15)]

#convert to long form, add "shelter" variable
s1<-melt(s1,id.vars="DateTime")
s1$Shelter<-1
s2<-melt(s2,id.vars="DateTime")
s2$Shelter<-2
s3<-melt(s3,id.vars="DateTime")
s3$Shelter<-3
s4<-melt(s4,id.vars="DateTime")
s4$Shelter<-4
s5<-melt(s5,id.vars="DateTime")
s5$Shelter<-5
s6<-melt(s6,id.vars="DateTime")
s6$Shelter<-6

#Rename columns
abv<-rbind(s1,s2,s3,s4,s5,s6)
names(abv)<-c("DateTime","SensorCode","value","Shelter")
sensors<-read.csv("abovegroundsensors.csv") %>% 
        filter(SensorType != "Humidity" & SensorType != "PAR")
abv<-merge(abv,sensors,by=c("Shelter","SensorCode"))

#Air Temperature
x<-"AirT"
#create new dfs based on variable
airtemp<-abv[grep(x, abv$SensorType),]

#put df in order by date
airtemp$DateTime<-as.POSIXct(airtemp$DateTime,format="%Y/%m/%d %h/%m/%s")
airtemp<-airtemp[do.call(order,airtemp),]
airtemp$Date<-date(airtemp$DateTime)
airtemp = airtemp %>% 
        dplyr::select(Shelter, value, Position, Date)
airtemp<-na.omit(airtemp)

# select max values for each date
maxes = aggregate(data = airtemp, value ~ Date, FUN = function(x) c(avg = mean(x), maxT = max(x)), simplify = TRUE, drop = TRUE)
val<-data.frame(maxes[["value"]])
maxes$maxT = val$maxT

# create day and month numbers for max data
maxes$Month = as.numeric(substr(maxes$Date, 6, 7))
maxes$Day = as.numeric(substr(maxes$Date, 9, 10))

# 2a) ####
maxes = maxes %>% 
        left_join(histtemp) %>% 
        dplyr::select(Date, maxT, Month, Day, Maximum.temperature..Degree.C.)
names(maxes) = c("date", "maxT", "month", "day", "hist.maxT")

# 2b) ####
maxes = maxes %>% 
        filter((maxT < hist.maxT) == FALSE)

#Surface Temperatures####
x<-"SurfaceTemp"
#create new dfs based on variable
surf<-rbind(s1[grep(x, s1$variable),])
surf<-rbind(s2[grep(x, s2$variable),],surf)
surf<-rbind(s3[grep(x, s3$variable),],surf)
surf<-rbind(s4[grep(x, s4$variable),],surf)
surf<-rbind(s5[grep(x, s5$variable),],surf)
surf<-rbind(s6[grep(x, s6$variable),],surf)

#Separate treatments and other variables into separate columns
df6<-separate(data=df6, col=variable, c("Sensor Type","Plot","Treatment"), sep = c(11,12), remove = TRUE)

#put df in order by date
df6$DateTime<-as.POSIXct(df6$DateTime,format="%Y/%m/%d %h/%m/%s")
df6<-df6[do.call(order,df6),]
#df6$Date<-date(df6$DateTime)

##average and standard error by treatment
allsum = data.table(df6)
allsum<-na.omit(allsum)
allsum$Date<-as.Date(allsum$DateTime)
allsum = allsum[,list(value = mean(value)), 'Plot,Shelter,Treatment,Date']
allsum = allsum[,list(avg_value = mean(value), ste_value = sd(value)/sqrt(length(value))), 'Treatment,Date']
allsum$upper<-allsum$avg_value+allsum$ste_value
allsum$lower<-allsum$avg_value-allsum$ste_value

#rename columns
names(allsum)<-c("Treatment","Date","value","stderror","upper","lower")
allsum$Treatment<-factor(allsum$Treatment,levels=c("AmbCon","AmbDrt","EleCon","EleDrt"))
allsum<-allsum[order(Date,Treatment),] 

Smax=max(allsum$value,na.rm=T) #use same temperature range across temperature charts
Smin<-min(allsum$value,na.rm=T) #there is an NA value in a soil temp sensor data stream that is stopping the script

##subset each treatment to a different data set
df1<-subset(allsum, Treatment == "AmbCon")
df2<-subset(allsum, Treatment == "AmbDrt")
df3<-subset(allsum, Treatment == "EleCon")
df4<-subset(allsum, Treatment == "EleDrt")

##start a tiff file
tiff(file = paste("FIELD",x,sD,"-",eD, ".tiff"), width = 3200, height = 2100, units = "px", res = 400) 
#plot Surface Temp
plot(df1$value ~ df1$Date, 
     type = "l",
     xlim=c(min(df1$Date),max(df1$Date)),
     ylim = c(Smin-1,Smax+2),
     ylab=expression(paste("Temperature (",degree~C,")")),
     xlab="Date",main="Plot Surface Temperature by Treatment",
     col=4)
#polygon(c(df1$Date,rev(df1$Date)),c(df1$lower,rev(df1$upper)),col=adjustcolor("blue",alpha.f=0.5),border=NA)
points(df2$value ~ df2$Date, type = "l",col=4,lty=2)
#polygon(c(df2$Date,rev(df2$Date)),c(df2$lower,rev(df2$upper)),col=adjustcolor("blue",alpha.f=0.25),density=25)
points(df3$value ~ df2$Date, type = "l",col=2)
#polygon(c(df3$Date,rev(df3$Date)),c(df3$lower,rev(df3$upper)),col=adjustcolor("red",alpha.f=0.5),border=NA)
points(df4$value ~ df2$Date, type = "l",col=2,lty=2)
#polygon(c(df4$Date,rev(df4$Date)),c(df4$lower,rev(df4$upper)),col=adjustcolor("red",alpha.f=0.25),density=25)
legend("topleft", y = NULL, 
       legend=c("aT-Con","aT-Drt","eT-Con","eT-Drt"), 
       col = c(4,4,2,2),lty=c(1,2,1,2),lwd=2,cex=0.8)
arrows(x0 =as.Date("2018-06-01"),length=0.05, y0 = (Smin+0.5), y1 = (Smin-2))
text(x = as.Date("2018-06-01"), y = (Smin+1), 
     labels=expression(paste("Drought Treatment Initiated")),cex=0.6)
arrows(x0 =as.Date("2018-12-01"),length=0.05, y0 = (Smin+0.5), y1 = (Smin-2))
text(x = as.Date("2018-12-01"), y = (Smin+1), 
     labels=expression(paste("Drought Treatment Completed")),cex=0.6)

dev.off()

