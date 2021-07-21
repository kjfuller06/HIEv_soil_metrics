Baug = as.POSIXct("2019-08-20 08:00:00")
Boct = as.POSIXct("2019-09-30 08:00:00")
Faug_13 = as.POSIXct("2019-08-20 08:00:00")
Faug_46 = as.POSIXct("2019-08-21 08:00:00")
Foct = as.POSIXct("2019-10-08 08:00:00")
Fnov = as.POSIXct("2019-11-18 08:00:00")
Laug = as.POSIXct("2019-08-16 08:00:00")
Loct = as.POSIXct("2019-10-02 08:00:00")
Lnov = as.POSIXct("2019-11-15 08:00:00")
Raug = as.POSIXct("2019-08-13 08:00:00")
Roct_14 = as.POSIXct("2019-09-30 08:00:00")
Roct_56 = as.POSIXct("2019-10-02 08:00:00")

#start date
sD<-as.Date("2019-06-01")
#end date
eD<-as.Date("2019-11-30")

#Aboveground data####
#download data from HIEv and only keep variables of interest
for(i in c(1:6)){
   s<-(downloadTOA5(paste("PACE_AUTO_S",i,"_ABVGRND_R_",sep=""), startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,4,6,7,12:15)]
   assign(paste("s",i,sep=""),s)
}

#convert to long form, add "shelter" variable
s1<-reshape2::melt(s1,id.vars="DateTime")
s1$Shelter<-1
s2<-reshape2::melt(s2,id.vars="DateTime")
s2$Shelter<-2
s3<-reshape2::melt(s3,id.vars="DateTime")
s3$Shelter<-3
s4<-reshape2::melt(s4,id.vars="DateTime")
s4$Shelter<-4
s5<-reshape2::melt(s5,id.vars="DateTime")
s5$Shelter<-5
s6<-reshape2::melt(s6,id.vars="DateTime")
s6$Shelter<-6

#combine into one DF
abv<-rbind(s1,s2,s3,s4,s5,s6)
names(abv)<-c("DateTime","SensorCode","value","Shelter")
sensors<-read.csv("abovegroundsensors.csv")
abv<-merge(abv,sensors,by=c("Shelter","SensorCode"))

#change class of variables
abv$Plot<-as.factor(abv$Plot)
abv$Shelter<-as.factor(abv$Shelter)

#air temp data for plotting
airT<-abv[abv$SensorType=="AirT",]
airT<-airT[airT$Position=="In",]
airT = airT %>% 
  dplyr::select(-Plot,
                -Treatment)

# assign harvest month of interest for summary stats
airT$month = NA
airT$species = NA
# Bis
bis = airT
bis$species = "Bis"
bis$month[bis$DateTime <= Baug] = "Aug"
bis$month[bis$DateTime > Baug & bis$DateTime <= Boct] = "Oct"
bis = na.omit(bis)

# Fes
fes = airT
fes$species = "Fes"
fes$month[fes$Shelter %in% c("1", "2", "3") & fes$DateTime <= Faug_13] = "Aug"
fes$month[fes$Shelter %in% c("4", "5", "6") & fes$DateTime <= Faug_46] = "Aug"
fes$month[fes$Shelter %in% c("1", "2", "3") & fes$DateTime > Faug_13 & fes$DateTime <= Foct] = "Oct"
fes$month[fes$Shelter %in% c("4", "5", "6") & fes$DateTime > Faug_46 & fes$DateTime <= Foct] = "Oct"
fes$month[fes$DateTime > Foct & fes$DateTime <= Fnov] = "Nov"
fes = na.omit(fes)

# Luc
luc = airT
luc$species = "Luc"
luc$month[luc$DateTime <= Laug] = "Aug"
luc$month[luc$DateTime > Laug & luc$DateTime <= Loct] = "Oct"
luc$month[luc$DateTime > Loct & luc$DateTime <= Lnov] = "Nov"
luc = na.omit(luc)

# Rye
rye = airT
rye$species = "Rye"
rye$month[rye$DateTime <= Raug] = "Aug"
rye$month[rye$Shelter %in% c("1", "2", "3", "4") & rye$DateTime > Raug & rye$DateTime <= Roct_14] = "Oct"
rye$month[rye$Shelter %in% c("5", "6") & rye$DateTime > Raug & rye$DateTime <= Roct_56] = "Oct"
rye = na.omit(rye)

# combine
airT = rbind(bis, fes, luc, rye)
airT = airT[order(airT$month),]
airT<-aggregate(data=airT,value~month,FUN=function(x) c(avg=mean(x),stder=sd(x)/sqrt(length(x))),simplify=TRUE,drop=TRUE)
val<-data.frame(airT[["value"]])
airT$value<-val$avg
airT$stder<-val$stder
airT = airT[order(airT$month),]

# plot
with(airT, barplot(value ~ month,
                   ylab = expression(paste("Mean Air Temperature (",degree~C,")")),
                   xlab = "Month",
                   ylim = c(0, ymax),
                   col = "black"))

tiff(file = paste("Figure1b_AirT_",sD,"_",eD,".tiff",sep=""), width =1100, height = 900, units = "px", res = 200)
# ggplot(data=airT, aes(x=month, y=value)) +
#   geom_bar(stat="identity", width=0.4, fill="black") +
#   theme_classic() +
#   labs(x="Month", y = expression(paste("Mean Air Temperature (",degree~C,")"))) +
#   ylim(0, 25)

ggplot(data=airT, aes(x=month, y=value)) +
  ylim(c(0, 25)) +
  geom_point(size = .5, stroke = 0) +
  geom_pointrange(aes(ymin=value-stder, ymax=value+stder)) +
  labs(x="Month", y = expression(paste("Mean Air Temperature (",degree~C,")"))) +
  theme_classic() +
  theme(legend.position = "none") 
dev.off()
