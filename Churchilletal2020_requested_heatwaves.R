## This script is for generating heatwave data using historical temperature data, air temperature data from PACE and surface temperature from PACE
# 1) load historical temperature data and generate 90% mark for each day of the year
# 2) load PACE data and generate daily maximum metrics
#       2a) bind historical 90% mark to PACE data
#       2b) subset by PACE temperatures that are at or above the 90% historical value
# 3) load PACE surface temperature data
#       3a) subset surface temperatures by selected dates from the above
#       3b) merge all relevant data and export
# 4) for heatwave DURATION, I'll need to do something additional. I may want to create a mock heatwave threshold by estimating the difference in temperature between airT and ambcon surfaceT. Use this relationship to calculate what the estimated airT would be using the surface temperature from difference treatments.

# 1) ####
histtemp = read.csv("RAAF_daily_temp.csv") %>% 
        dplyr::select(-Product.code,
                      -Bureau.of.Meteorology.station.number) %>% 
        filter(is.na(Maximum.temperature..Degree.C.) == FALSE & Quality == "Y")
histtemp = aggregate(data = histtemp, Maximum.temperature..Degree.C. ~ Month + Day, FUN = function(x) c(percent90 = quantile(x, probs = 0.9)), simplify = TRUE, drop = TRUE)

# 2) ####
#write start date for first analysis
sD<-as.Date("2018-06-01")
#Write end date for first analysis
eD<-as.Date("2019-05-31")

#download from HIEv and processing
#get data, only keep variables of interest
s1 <- (downloadTOA5("PACE_AUTO_S1_ABVGRND_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,6,12:15)]
s2 <- (downloadTOA5("PACE_AUTO_S2_ABVGRND_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,6,12:15)]
s3 <- (downloadTOA5("PACE_AUTO_S3_ABVGRND_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,6,12:15)]
s4 <- (downloadTOA5("PACE_AUTO_S4_ABVGRND_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,6,12:15)]
s5 <- (downloadTOA5("PACE_AUTO_S5_ABVGRND_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,6,12:15)]
s6 <- (downloadTOA5("PACE_AUTO_S6_ABVGRND_R_", startDate=sD, endDate=eD, keepFiles=FALSE))[,c(1,6,12:15)]

#convert to long form, add "shelter" variable
s1<-melt(s1,id.vars="DateTime")
s1$Shelter<-1
s2<-melt(s2,id.vars="DateTime")
s2$Shelter<-2
s3<-melt(s3,id.vars="DateTime")
s3$Shelter<-3
s4<-melt(s4,id.vars="DateTime")
s4$Shelter<-4
s5<-melt(s5,id.vars="DateTime")
s5$Shelter<-5
s6<-melt(s6,id.vars="DateTime")
s6$Shelter<-6

#Rename columns
abv<-rbind(s1,s2,s3,s4,s5,s6)
names(abv)<-c("DateTime","SensorCode","value","Shelter")
sensors<-read.csv("abovegroundsensors.csv") %>% 
        filter(SensorType != "Humidity" & SensorType != "PAR")
abv<-merge(abv,sensors,by=c("Shelter","SensorCode"))

#Air Temperature
x<-"AirT"
#create new dfs based on variable
airtemp<-abv[grep(x, abv$SensorType),]

#put df in order by date
airtemp$DateTime<-as.POSIXct(airtemp$DateTime,format="%Y/%m/%d %h/%m/%s")
airtemp<-airtemp[do.call(order,airtemp),]
airtemp$Date<-date(airtemp$DateTime)
airtemp = airtemp %>% 
        dplyr::select(Shelter, value, Position, Date)
airtemp<-na.omit(airtemp)

# select max values for each date
maxes = aggregate(data = airtemp, value ~ Date, FUN = function(x) c(avg = mean(x), maxT = max(x)), simplify = TRUE, drop = TRUE)
val<-data.frame(maxes[["value"]])
maxes$maxT = val$maxT

# create day and month numbers for max data
maxes$Month = as.numeric(substr(maxes$Date, 6, 7))
maxes$Day = as.numeric(substr(maxes$Date, 9, 10))

# 2a) ####
maxes = maxes %>% 
        left_join(histtemp) %>% 
        dplyr::select(Date, maxT, Month, Day, Maximum.temperature..Degree.C.)
names(maxes) = c("date", "maxT", "month", "day", "hist.maxT")

# 2b) ####
maxes = maxes %>% 
        filter((maxT < hist.maxT) == FALSE)

# 3) ####
x<-"SurfaceTemp"
#create new dfs based on variable
surf<-abv[grep(x, abv$SensorType),]

#put df in order by date
surf$DateTime<-as.POSIXct(surf$DateTime,format="%Y/%m/%d %h/%m/%s")
surf<-surf[do.call(order,surf),]
surf$Date<-date(surf$DateTime)
surf = surf %>% 
        dplyr::select(Shelter, value, Plot, Treatment, Date)
surf<-na.omit(surf)

# select max values for each date
surf_agg = aggregate(data = surf, value ~ Date + Treatment, FUN = function(x) c(avg = mean(x), maxT = max(x)), simplify = TRUE, drop = TRUE)
val<-data.frame(surf_agg[["value"]])
surf_agg$maxT = val$maxT
surf_agg$avgT = val$avg
surf_agg = surf_agg %>% 
        dplyr::select(Date, Treatment, maxT, avgT)

# create day and month numbers for max data
surf_agg$Month = as.numeric(substr(surf_agg$Date, 6, 7))
surf_agg$Day = as.numeric(substr(surf_agg$Date, 9, 10))

# 3a) ####
surf_agg = surf_agg[surf_agg$Date %in% maxes$date,]

# 3b) ####
# remove unwanted variables and rename columns for clarity
surf_agg = surf_agg %>% 
        dplyr::select(-Month,
                      -Day)
names(surf_agg) = c("date", "treatment", "PACE_max_surface_temp", "PACE_avg_surface_temp")
write.csv(surf_agg, "Churchilletal2020_surfacetemp_heatwaves.csv", row.names = FALSE)

# remove unwanted variables and rename columns for clarity
maxes = maxes %>% 
        dplyr::select(-month,
                      -day)
names(maxes) = c("date", "PACE_max_air_temp", "RAAF_historical_max_air_temp")
write.csv(maxes, "Churchilletal2020_airtemp_heatwaves.csv", row.names = FALSE)
