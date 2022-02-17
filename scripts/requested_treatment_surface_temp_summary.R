# function for plots with yaxt
df = treats
colours <- rainbow(length(df))
# mins = c()
# maxs = c()
# for(a in c(1:length(df))){
#   mins[a] = min(density(df[[a]][,4])$x)
#   maxs[a] = max(density(df[[a]][,4])$x)
# }
# xmin = min(mins)
# xmax = max(maxs)
# par(mar = c(4, 2, 0.5, 0))
# plot(1, type="n", xlab= "", ylab="", xlim=c(xmin, xmax), ylim=c(0, 0.005))
# legend("topright", legend = paste(names(df)), col = c(colours[1:4]), lty = 1, lwd = 10)
# for(i in c(1:length(df))){
#   a = density(df[[i]][,4])
#   a$y = a$y/sum(a$y)
#   lenya = length(a$y)
#   xmaxa = max(a$x)
#   xmina = min(a$x)
#   polygon(c(seq(xmaxa, xmina, length.out = lenya), seq(xmina, xmaxa, length.out = lenya)), c(rep(0, lenya), a$y), col = adjustcolor(colours[i], alpha = 0.25), border = colours[i])
# }

#calculate max and min values for ylim, remove NAs
ymax<-max(surf$maxT,na.rm=T) 
ymin<-min(surf$maxT,na.rm=T) 

with(df[[1]],plot(maxT ~ Date, 
                   type = "l",
                   bty='l',
                   ylim = c(ymin-0.05,ymax),
                   ylab="",
                   xlab="",
                   col="blue"))
# abline(h=0,lty=2)
# abline(h=-5,lty=2)
mtext(side=2,expression(paste("Surface Temperature (",degree~C,")")),padj=-2.5)
# with(df[[3]],polygon(c(Date,rev(Date)),c(maxT[,3],rev(maxT[,2])),col=adjustcolor("red",alpha.f=0.65),border=NA))
with(df[[3]],points(maxT ~ Date, type = "l",col="red"))
# with(df[[4]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("red4",alpha.f=0.65),border=NA))
with(df[[4]],points(maxT ~ Date, type = "l",col="red4",lty=2))
# with(df[[2]],polygon(c(Date,rev(Date)),c(lower,rev(upper)),col=adjustcolor("lightskyblue",alpha.f=0.85),border=NA))
with(df[[2]],points(maxT ~ Date, type = "l",col="lightskyblue",lty=2))

# # write multipanel tiff to disk
# tiff(file = "outputs/ribboning3.tiff", width =2200, height = 1100, units = "px", res = 200)
# 
# # legend
# plot(1, type="n", xaxt = 'n', yaxt = 'n', bty = 'n', xlim=c(0, 0.01), ylim=c(0, 0.01), ann = FALSE)

stats = read.csv("Daily_Temp_Summary_Stats 2018-06-01 - 2019-05-30 .csv")

##subset each treatment to a different data set
df1<-subset(allsum, Treatment == "AmbCon")
df2<-subset(allsum, Treatment == "AmbDrt")
df3<-subset(allsum, Treatment == "EleCon")
df4<-subset(allsum, Treatment == "EleDrt")

quantile(df1$uppermax, probs = seq(0, 1, 0.05))

df1 = df1[,c(1,2,3,9,10)]
df1.47 = df1
df1.47[df1.47$uppermax <= 47, c(3:5)] = NA
df2 = df2[,c(1,2,3,9,10)]
df2.47 = df2
df2.47[df2.47$uppermax <= 47, c(3:5)] = NA
df3 = df3[,c(1,2,3,9,10)]
df3.47 = df3
df3.47[df3.47$uppermax <= 47, c(3:5)] = NA
df4 = df4[,c(1,2,3,9,10)]
df4.47 = df4
df4.47[df4.47$uppermax <= 47, c(3:5)] = NA

with(df1.47, plot(uppermax ~ Date,
                  type = "l",
                  ylim = c(0, 60),
                  xlim = c(min(df1$Date),
                           max(df1$Date))))
with(df2.47, points(uppermax ~ Date,
                    type = "l"))
with(df3.47, points(uppermax ~ Date,
                    type = "l"))
with(df4.47, points(uppermax ~ Date,
                    type = "l"))

library(tidyverse)

heatwaves = df1.47[,c(2,3,5)] %>% 
  full_join(df2.47[,c(2,3,5)], by = c("Date")) %>% 
  full_join(df3.47[,c(2,3,5)], by = c("Date")) %>% 
  full_join(df4.47[,c(2,3,5)], by = c("Date"))
names(heatwaves)[c(2:9)] = c("mean.ambcon",
                             "uppermax.ambcon",
                             "mean.ambdrt",
                             "uppermax.ambdrt",
                             "mean.elecon",
                             "uppermax.elecon",
                             "mean.eledrt",
                             "uppermax.eledrt")

write.csv(heatwaves, "requested_heatwaves_for_Churchilletal2020.csv")
