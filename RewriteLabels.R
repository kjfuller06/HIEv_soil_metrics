#generate labels for harvest sample bags
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

labels<-read.csv("labels.csv")

s<-list()
for (i in df1$SubplotID){
  a<-textGrob(i)
  s[[i]]<-a
}

pdf("test.pdf")
grid.arrange(grobs=s[1:8],ncol=2)
dev.off()
