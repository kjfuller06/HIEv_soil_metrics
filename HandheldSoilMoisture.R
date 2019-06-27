df1<-read.csv("FIELD_PACE_SOIL_MOISTURE_2019-06-17.csv")
#df1$Subplot.ID<-paste("S",df1$Shelter,"P",df1$Plot,df1$Subplot,sep="")
df1<-df1[,-c(1,5:8)]
# df1<-df1[,-1]
# df1$TimeStamp<-as.POSIXct(df1$TimeStamp,format="%d-%m-%y %H:%M")
# df1$Date<-as.Date(df1$TimeStamp)
# df1<-df1[,-1]
# df1$Date<-as.POSIXct(df1,format="%Y-%m-%d")
df1$Date<-as.POSIXct("2019-06-17",format="%Y-%m-%d")
names(df1)<-c("Subplot.ID","Species","moisture","Date")

levels(df1$Species)
#levels(df1$Species)<-c("DigBis","Dig","Fes","KanWal","Kan","Rho")
#levels(df1$Species)<-c("Bis","DigBis","Dig","Fes","KanWal","Kan","Luc","Rho","Rye")
#levels(df1$Species)<-c("Bis","DigBis","Dig","Fes","KanWal","Kan","Luc","PhaSub","Rho","Rye","Wal")
#levels(df1$Species)<-c("Bis","DigBis","Dig","Fes","KanWal","Kan","Luc","PhaSub","Pha","Rho","Rye","Wal")

#levels(df1$Moisture.1)[levels(df1$Moisture.1)=="<1"] <- NA
#levels(df1$Moisture.2)[levels(df1$Moisture.2)=="<1"] <- NA
#levels(df1$Moisture.3)[levels(df1$Moisture.3)=="<1"] <- NA
#levels(df1$Moisture.4)[levels(df1$Moisture.4)=="<1"] <- NA
#levels(df1$Moisture.5)[levels(df1$Moisture.5)=="<1"] <- NA

# library(tidyr)
# df1$Moisture.1<-as.numeric(as.character(df1$Moisture.1))
# df1$Moisture.2<-as.numeric(as.character(df1$Moisture.2))
# df1$Moisture.3<-as.numeric(as.character(df1$Moisture.3))
# df1$Moisture.4<-as.numeric(as.character(df1$Moisture.4))
# df1$Moisture.5<-as.numeric(as.character(df1$Moisture.5))

df1<-df1[c(1:160),]
df1<-droplevels(df1)
df1$moisture[df1$moisture>25]
df1$moisture[df1$moisture>25]<-NA

# df1<-gather(df1,spoint,moisture,Moisture.1:Moisture.5,factor_key = TRUE, na.rm=TRUE)
# df1<-gather(df1[1:24,],spoint,moisture,Moisture.1:Moisture.5,factor_key = TRUE, na.rm=TRUE)
# df1<-df1[,-4]

summary(df1)

df1<-aggregate(data=df1,moisture~Date+Subplot.ID+Species,FUN=function(x) c(avg=mean(x),sterror=sd(x)/sqrt(length(x))))
#df2<-aggregate(data=df1[1:24,],moisture~Date+Subplot.ID+Species,FUN=mean)
#df3<-rbind(df1[73:192,],df2)

summary(df1)
summary(df1$Species)

#write.csv(df1,"FIELD_PACE_SOIL_MOISTURE_2018-08-17(novar).csv")
#write.csv(df1,"FIELD_PACE_SOIL_MOISTURE_2018-08-17(shelter1).csv")
write.csv(df1,"FIELD_PACE_SOIL_MOISTURE_2019-06-17.csv")
