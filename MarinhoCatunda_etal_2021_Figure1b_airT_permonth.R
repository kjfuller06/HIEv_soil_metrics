#start date
sD<-as.Date("2019-06-01")
#end date
eD<-as.Date("2019-11-30")

#Aboveground data####
#download data from HIEv and only keep variables of interest
airT = read.csv("data/MarinhoCatunda_Fig1_AirT_final.csv")
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
  geom_text(x="Nov", y= (airT$value[17] - 2), label="MIN") +
  geom_text(x="Jun", y= max(airT$value), label="B)")
# dev.off()
rm(airT)
