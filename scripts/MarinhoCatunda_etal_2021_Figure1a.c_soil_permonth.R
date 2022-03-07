#start date
sD<-as.Date("2019-06-01")
#end date
eD<-as.Date("2019-11-30")

#Irrigation####
Irrig = read.csv("data/MarinhoCatunda_Fig1_Irrig_final.csv")
Irrig$month = factor(Irrig$month, levels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))
Irrig = Irrig[order(Irrig$month, Irrig$Treatment),]
# plot
# tiff(file = "Figure1a_Irrig_permonth.tiff", width =1100, height = 900, units = "px", res = 200)

Fig1a = ggplot(data=Irrig, aes(x=month, y=Irrigation, fill=Treatment)) +
   geom_bar(stat="identity", width=0.75, color="black", position=position_dodge())+
   theme_classic() +
   ylim(c(0, 4)) +
   scale_fill_manual(values=c('black','white')) +
   labs(x="Month", y = expression("Irrigation (mm day"^"-1"*")")) +
   geom_text(x="Jun", y= 4, label="A)") +
   theme(legend.position = c(0.88, 0.82))

# dev.off()
rm(Irrig)

#Soil Moisture####
#download data from HIEv and only keep soil moisture variables of interest
soil = read.csv("data/MarinhoCatunda_Fig1_soil_final.csv")
soil2 = read.csv("data/MarinhoCatunda_Fig1_soillong_final.csv")
soil$month = factor(soil$month, levels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))
soil = soil[order(soil$month),]
soil2$month = factor(soil2$month, levels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))
soil2 = soil2[order(soil2$month),]
soil2$Sp = as.factor(soil2$Sp)
levels(soil2$Sp) = c("Bis", "Fes", "Med", "Lol")
soil2$Sp = factor(soil2$Sp, levels = c("Bis", "Fes", "Lol", "Med"))
names(soil2)[1] = "Species"

# plot
# tiff(file = "Figure1c_soilVWC_permonth.tiff", width =1100, height = 900, units = "px", res = 200)

Fig1c = 
   ggplot(data=soil, aes(x=month)) +
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
   theme_classic() +
   ylim(c(min(soil$LUCDrt), max(soil$RYECon) + 2)) +
   geom_text(x="Jun", y= max(soil$RYECon) + 2, label="C)")
legend = ggplot(data = soil2, aes(x = month, y = value, color = Species, group = Sp)) +
   geom_line(aes(group = Species), size = 3) +
   scale_color_manual(values = c("purple", "orange", "yellow", "pink")) +
   theme_classic()
library(cowplot)
library(grid)
library(gridExtra)
Fig1c_legend <- cowplot::get_legend(legend)
grid.newpage()
grid.draw(Fig1c_legend)

# dev.off()
rm(soil, soil2, legend)
