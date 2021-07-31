#start date
sD<-as.Date("2019-06-01")
#end date
eD<-as.Date("2019-11-30")

#Aboveground data####
#download data from HIEv and only keep variables of interest
abv = read.csv("data/MarinhoCatunda_Fig1_AirT_raw.csv")
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

# combine
airT$month = format(airT$DateTime, format = "%b")
airT = airT[order(airT$month),]
backup = airT
airT<-aggregate(data=airT,value~month,FUN=function(x) c(avg=mean(x), minT = min(x), maxT = max(x)),simplify=TRUE,drop=TRUE)
val<-data.frame(airT[["value"]])
airT$value<-val$avg
airT$minT<-val$minT
airT$maxT = val$maxT
airT = airT %>% 
  pivot_longer(cols = c("value", "minT", "maxT"))
airT$month = factor(airT$month, levels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))
airT = airT[order(airT$month),]
airT$name = factor(airT$name, levels = c("maxT", "value", "minT"))

# plot
# tiff(file = "Figure1b_AirT_permonth.tiff", width =1100, height = 900, units = "px", res = 200)

Fig1b = ggplot(data=airT, aes(x=month, y=value, group = name)) +
  geom_line(aes(linetype = name)) +
  geom_point(aes(shape = name)) +
  scale_shape_manual(values = c(1, 16, 1)) +
  scale_linetype_manual(values = c("dashed", "solid", "dashed")) +
  labs(x="Month", y = expression(paste("Mean Air Temperature (",degree~C,")"))) +
  theme_classic() +
  theme(legend.position = "none") +
  geom_text(x="Nov", y= (airT$value[18] - 2), label="MAX") +
  geom_text(x="Nov", y= (airT$value[17] - 2), label="MIN")
# dev.off()
