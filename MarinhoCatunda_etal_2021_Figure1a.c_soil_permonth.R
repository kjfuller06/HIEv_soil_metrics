#start date
sD<-as.Date("2019-06-01")
#end date
eD<-as.Date("2019-11-30")

#Irrigation####
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
# tiff(file = "Figure1a_Irrig_permonth.tiff", width =1100, height = 900, units = "px", res = 200)

Fig1a = ggplot(data=Irrig, aes(x=month, y=Irrigation, fill=Treatment)) +
   geom_bar(stat="identity", width=0.75, color="black", position=position_dodge())+
   theme_classic() +
   scale_fill_manual(values=c('black','white')) +
   labs(x="Month", y = "Irrigation (mm)") +
   geom_text(x="Jun", y= max(Irrig$Irrigation), label="Max")

# dev.off()
rm(Irrig, plots, backup)

#Soil Moisture####
#download data from HIEv and only keep soil moisture variables of interest
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
# tiff(file = "Figure1c_soilVWC_permonth.tiff", width =1100, height = 900, units = "px", res = 200)

Fig1c = ggplot(data=soil, aes(x=month)) +
   geom_line(aes(y = BISCon, group = 1), colour = "purple") +
   geom_point(aes(y = BISCon, group = 1), colour = "purple", shape = 19, size = 2) +
   geom_line(aes(y = BISDrt, group = 1), colour = "purple") +
   geom_point(aes(y = BISDrt, group = 1), colour = "purple", shape = 1, size = 2) +
   geom_line(aes(y = FESCon, group = 1), colour = "orange") +
   geom_point(aes(y = FESCon, group = 1), colour = "orange", fill = "orange", shape = 24, size = 2) +
   geom_line(aes(y = FESDrt, group = 1), colour = "orange") +
   geom_point(aes(y = FESDrt, group = 1), colour = "orange", shape = 2, size = 2) +
   geom_line(aes(y = LUCCon, group = 1), colour = "pink") +
   geom_point(aes(y = LUCCon, group = 1), colour = "pink", fill = "pink", shape = 15, size = 2) +
   geom_line(aes(y = LUCDrt, group = 1), colour = "pink") +
   geom_point(aes(y = LUCDrt, group = 1), colour = "pink", shape = 0, size = 2) +
   geom_line(aes(y = RYECon, group = 1), colour = "yellow") +
   geom_point(aes(y = RYECon, group = 1), colour = "yellow", fill = "yellow", shape = 23, size = 2) +
   geom_line(aes(y = RYEDrt, group = 1), colour = "yellow") +
   geom_point(aes(y = RYEDrt, group = 1), colour = "yellow", shape = 5, size = 2) +
   labs(x="Month", y = "Soil Volumetric Water Content (%)") +
   theme_classic()

# dev.off()
rm(backup1, s, s1, s2, s3, s4, s5, s6, sensors, soil)