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

#Irrigation####
Irrig<- (downloadTOA5("PACE_AUTO_ALL_IRRIG_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,4:6)]

#Rename columns
names(Irrig)<-c("DateTime","Code","Treatment","Irrigation")
Irrig$Shelter = substr(Irrig$Code,1 ,2)
Irrig$Plot = paste0("P", substr(Irrig$Code, 4, 4))
plots = read.csv("PACE_treatment_species_reference.csv")
plots$Shelter = substr(plots$Subplot.ID, 1, 2)
plots$Plot = substr(plots$Subplot.ID, 3, 4)
Irrig = left_join(Irrig, plots)

# assign harvest month of interest for summary stats
Irrig$month = NA
Irrig$month[Irrig$Species == "Bis" & Irrig$DateTime <= Baug] = "Aug"
Irrig$month[Irrig$Species == "Bis" & Irrig$DateTime > Baug & Irrig$DateTime <= Boct] = "Oct"
Irrig$month[Irrig$Species == "Fes" & Irrig$Shelter %in% c("S1", "S2", "S3") & Irrig$DateTime <= Faug_13] = "Aug"
Irrig$month[Irrig$Species == "Fes" & Irrig$Shelter %in% c("S4", "S5", "S6") & Irrig$DateTime <= Faug_46] = "Aug"
Irrig$month[Irrig$Species == "Fes" & Irrig$Shelter %in% c("S1", "S2", "S3") & Irrig$DateTime > Faug_13 & Irrig$DateTime <= Foct] = "Oct"
Irrig$month[Irrig$Species == "Fes" & Irrig$Shelter %in% c("S4", "S5", "S6") & Irrig$DateTime > Faug_46 & Irrig$DateTime <= Foct] = "Oct"
Irrig$month[Irrig$Species == "Fes" & Irrig$DateTime > Foct & Irrig$DateTime <= Fnov] = "Nov"
Irrig$month[Irrig$Species == "Luc" & Irrig$DateTime <= Laug] = "Aug"
Irrig$month[Irrig$Species == "Luc" & Irrig$DateTime > Laug & Irrig$DateTime <= Loct] = "Oct"
Irrig$month[Irrig$Species == "Luc" & Irrig$DateTime > Loct & Irrig$DateTime <= Lnov] = "Nov"
Irrig$month[Irrig$Species == "Rye" & Irrig$DateTime <= Raug] = "Aug"
Irrig$month[Irrig$Species == "Rye" & Irrig$Shelter %in% c("S1", "S2", "S3", "S4") & Irrig$DateTime > Raug & Irrig$DateTime <= Roct_14] = "Oct"
Irrig$month[Irrig$Species == "Rye" & Irrig$Shelter %in% c("S5", "S6") & Irrig$DateTime > Raug & Irrig$DateTime <= Roct_56] = "Oct"

#force Treatment to be a factor
Irrig$Treatment<-as.factor(Irrig$Treatment)
levels(Irrig$Treatment)<-c("Control","Drought","Control","Drought")
Irrig<-aggregate(data=Irrig,Irrigation~month+Treatment+Plot+Subplot.ID,FUN=sum)
Irrig<-aggregate(data=Irrig,Irrigation~month+Treatment,FUN=mean)

Irrig$month = factor(Irrig$month, levels = c("Aug", "Oct", "Nov"))
Irrig = Irrig[order(Irrig$month, Irrig$Treatment),]

# plot
tiff(file = paste("Figure1a_Irrig_",sD,"_",eD,".tiff",sep=""), width =1100, height = 900, units = "px", res = 200)

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
soil = soil %>%  
   pivot_wider(names_from = water, values_from = value) %>% 
   as.data.frame()
soil$comp = soil$Drt - soil$Con

#change class of variables
soil$Sp<-as.factor(soil$Sp)
soil$Shelter<-as.factor(soil$Shelter)

# assign harvest month of interest for summary stats
soil$month = NA
soil$month[soil$Sp == "BIS" & soil$DateTime <= Baug] = "Aug"
soil$month[soil$Sp == "BIS" & soil$DateTime > Baug & soil$DateTime <= Boct] = "Oct"
soil$month[soil$Sp == "FES" & soil$Shelter %in% c("1", "2", "3") & soil$DateTime <= Faug_13] = "Aug"
soil$month[soil$Sp == "FES" & soil$Shelter %in% c("4", "5", "6") & soil$DateTime <= Faug_46] = "Aug"
soil$month[soil$Sp == "FES" & soil$Shelter %in% c("1", "2", "3") & soil$DateTime > Faug_13 & soil$DateTime <= Foct] = "Oct"
soil$month[soil$Sp == "FES" & soil$Shelter %in% c("4", "5", "6") & soil$DateTime > Faug_46 & soil$DateTime <= Foct] = "Oct"
soil$month[soil$Sp == "FES" & soil$DateTime > Foct & soil$DateTime <= Fnov] = "Nov"
soil$month[soil$Sp == "LUC" & soil$DateTime <= Laug] = "Aug"
soil$month[soil$Sp == "LUC" & soil$DateTime > Laug & soil$DateTime <= Loct] = "Oct"
soil$month[soil$Sp == "LUC" & soil$DateTime > Loct & soil$DateTime <= Lnov] = "Nov"
soil$month[soil$Sp == "RYE" & soil$DateTime <= Raug] = "Aug"
soil$month[soil$Sp == "RYE" & soil$Shelter %in% c("1", "2", "3", "4") & soil$DateTime > Raug & soil$DateTime <= Roct_14] = "Oct"
soil$month[soil$Sp == "RYE" & soil$Shelter %in% c("5", "6") & soil$DateTime > Raug & soil$DateTime <= Roct_56] = "Oct"

soil$month = factor(soil$month, levels = c("Aug", "Oct", "Nov"))
soil = soil[order(soil$DateTime),]

#remove NAs
soil<-na.omit(soil)

#Summarize data
backup1<-soil
soil$comp = soil$comp*100
soil$Con = soil$Con*100
soil$Drt = soil$Drt*100
soil<-aggregate(data=soil,comp~Sp+month,FUN=function(x) c(avg=mean(x),stder=sd(x)/sqrt(length(x))),simplify=TRUE,drop=TRUE)

#spit the aggregate function outputs into a DF and reassign so they are variables in the dataframe
##otherwise the output is a list within soil
val<-data.frame(soil[["comp"]])
soil$value<-val$avg
soil$stder<-val$stder
soil = soil[order(soil$month),]

# plot
tiff(file = paste("Figure1c_soilVWC_",sD,"_",eD,".tiff",sep=""), width =1100, height = 900, units = "px", res = 200)

ggplot(data=soil, aes(x=month, y=value, group = month, color=month)) +
   ylim(c(-4.5, 2)) +
   geom_point(size = .5, stroke = 0) +
   geom_pointrange(aes(ymin=value-stder, ymax=value+stder)) +
   scale_color_manual(values=c('deepskyblue','blue', "grey45")) +
   facet_wrap(~Sp, strip.position = "bottom", nrow = 1) +
   labs(x="Month", y = "Volumetric Water Content (%)") +
   geom_hline(yintercept = 0) +
   theme_classic() +
   theme(legend.position = "none") 

dev.off()
