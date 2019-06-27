dfr<-read.csv(file="PACE_handheldsoilmoisture_master.csv")
dfr<-dfr[,-c(1,2)]

write.csv(dfr,"PACE_handheldsoilmoisture_master.csv")
