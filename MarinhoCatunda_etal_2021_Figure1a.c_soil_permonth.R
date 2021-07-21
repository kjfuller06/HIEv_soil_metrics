#start date
sD<-as.Date("2019-06-01")
#end date
eD<-as.Date("2019-11-30")

#Irrigation####
Irrig<- (downloadTOA5("PACE_AUTO_ALL_IRRIG_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,4:6)]
backup = Irrig

#Rename columns
names(Irrig)<-c("DateTime","Code","Treatment","Irrigation")
Irrig$Shelter = substr(Irrig$Code,1 ,2)
Irrig$Plot = paste0("P", substr(Irrig$Code, 4, 4))
plots = read.csv("PACE_treatment_species_reference.csv")
plots$Shelter = substr(plots$Subplot.ID, 1, 2)
plots$Plot = substr(plots$Subplot.ID, 3, 4)
Irrig = left_join(Irrig, plots)
Irrig$month = format(Irrig$DateTime, format = "%b")

#force Treatment to be a factor
Irrig$Treatment<-as.factor(Irrig$Treatment)
levels(Irrig$Treatment)<-c("Control","Drought","Control","Drought")
Irrig<-aggregate(data=Irrig,Irrigation~month+Treatment,FUN=mean)

Irrig$month = factor(Irrig$month, levels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))
Irrig = Irrig[order(Irrig$month, Irrig$Treatment),]

# plot
tiff(file = "Figure1a_Irrig_permonth.tiff", width =1100, height = 900, units = "px", res = 200)

ggplot(data=Irrig, aes(x=month, y=Irrigation, fill=Treatment)) +
   geom_bar(stat="identity", width=0.75, color="black", position=position_dodge())+
   theme_classic() +
   scale_fill_manual(values=c('black','white')) +
   labs(x="Month", y = "Irrigation (mm)")

dev.off()

#Soil Moisture####
#download data from HIEv and only keep soil moisture variables of interest
for(i in c(1:6)){
   s<-(downloadTOA5(paste("PACE_AUTO_S",i,"_BLWGRND_R_",sep=""), startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,3:26)]
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
soil<-rbind(s1,s2,s3,s4,s5,s6)
names(soil)<-c("DateTime","SensorCode","value","Shelter")
sensors<-read.csv("soilsensors.csv")
soil<-merge(soil,sensors,by=c("Shelter","SensorCode"))
soil = soil %>% 
   filter(Position != "Lower" & SensorType == "TDR")
soil$water<-as.factor(substr(soil$Treatment, 4, 6))
soil$code = paste0("S", soil$Shelter, "P", soil$Plot)
soil = soil %>% 
   dplyr::select(-SensorCode,
                 -SensorType,
                 -Treatment,
                 -Position)
soil = soil[order(soil$DateTime, soil$Shelter, soil$water),]
soil<-aggregate(data=soil,value~Shelter+DateTime+Sp+water,FUN=mean)

#change class of variables
soil$Sp<-as.factor(soil$Sp)
soil$Shelter<-as.factor(soil$Shelter)
soil$month = format(soil$DateTime, format = "%b")
soil = soil[order(soil$DateTime),]

#remove NAs
soil<-na.omit(soil)

#Summarize data
backup1<-soil
soil$value = soil$value*100
soil<-aggregate(data=soil,value~Sp+month+water,FUN=mean)
soil$month = factor(soil$month, levels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))
soil = soil[order(soil$month),]
soil$group = as.factor(paste0(soil$Sp, soil$water))
soil = soil %>% 
   dplyr::select(-Sp,
                 -water) %>% 
   pivot_wider(names_from = group, values_from = value)

# plot
tiff(file = "Figure1c_soilVWC_permonth.tiff", width =1100, height = 900, units = "px", res = 200)

ggplot(data=soil, aes(x=month)) +
   geom_line(aes(y = BISCon, group = 1), colour = "black") +
   geom_point(aes(y = BISCon, group = 1), colour = "black", shape = 19, size = 2) +
   geom_line(aes(y = BISDrt, group = 1), colour = "black") +
   geom_point(aes(y = BISDrt, group = 1), colour = "black", shape = 1, size = 2) +
   geom_line(aes(y = FESCon, group = 1), colour = "blue4") +
   geom_point(aes(y = FESCon, group = 1), colour = "blue4", fill = "blue4", shape = 24, size = 2) +
   geom_line(aes(y = FESDrt, group = 1), colour = "blue4") +
   geom_point(aes(y = FESDrt, group = 1), colour = "blue4", shape = 2, size = 2) +
   geom_line(aes(y = LUCCon, group = 1), colour = "darkgreen") +
   geom_point(aes(y = LUCCon, group = 1), colour = "darkgreen", fill = "darkgreen", shape = 15, size = 2) +
   geom_line(aes(y = LUCDrt, group = 1), colour = "darkgreen") +
   geom_point(aes(y = LUCDrt, group = 1), colour = "darkgreen", shape = 0, size = 2) +
   geom_line(aes(y = RYECon, group = 1), colour = "darkorange") +
   geom_point(aes(y = RYECon, group = 1), colour = "darkorange", fill = "darkorange", shape = 23, size = 2) +
   geom_line(aes(y = RYEDrt, group = 1), colour = "darkorange") +
   geom_point(aes(y = RYEDrt, group = 1), colour = "darkorange", shape = 5, size = 2) +
   labs(x="Month", y = "Volumetric Water Content (%)") +
   theme_classic() 

dev.off()
