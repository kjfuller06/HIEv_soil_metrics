#start date
#start date
sD<-as.Date("2018-04-01")
#end date
eD<-as.Date("2020-03-31")

#Aboveground data####
surf = read.csv("data/MarinhoCatunda_Fig2_surfT_final.csv")
surf$Date = as.Date(surf$Date)

#format for graphing
max<-subset(surf,select=c(-value,-minT))
max$month = format(as.Date(max$Date), format = "%b")
max<-split(max,droplevels(as.factor(max$Treatment)))

min<-subset(surf,select=c(-value,-maxT))
min<-split(min,droplevels(as.factor(min$Treatment)))

airT = read.csv("data/MarinhoCatunda_Fig2_AirT_final.csv")
airT$Date = as.Date(airT$Date)
