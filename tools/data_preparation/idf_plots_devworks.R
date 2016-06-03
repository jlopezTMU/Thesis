## This Script plots the idf* input files idf x probability of keyword per topic

## plotting topic/keyword data
setwd("Z:/DR Miransky/devWorks") ##only RU PC!!!
dat<-read.delim(file = "idf_Sun-May-29_21-50-20_2016_devworks_T1.txt", header = T, quote = "", sep = "\t")
dat <- as.data.frame(dat)
dev.new(width=5, height=15)
plot(dat$idf, dat$PROBABILITY, main="TOPIC 1 dev Works db2 Forum", sub=paste("N=", dat$N[1]), font=4, xlab="idf", ylab="probability", pch = NA)
text(dat$idf, dat$PROBABILITY, labels=dat$KEYWORD, cex= 1.2, col="red")
##lines(dat$idf, dat$PROBABILITY)
grid(nx=NULL, ny=NULL)


 
dat<-read.delim(file = "idf_Sun-May-29_21-50-20_2016_devworks_T2.txt", header = T, quote = "", sep = "\t")
dat <- as.data.frame(dat)
dev.new(width=5, height=15)
plot(dat$idf, dat$PROBABILITY, main="TOPIC 2 dev Works db2 Forum", sub=paste("N=", dat$N[1]), xlab="idf", ylab="probability", pch = NA)
text(dat$idf, dat$PROBABILITY, labels=dat$KEYWORD, cex= 1.2, col="red")
grid()

 
dat<-read.delim(file = "idf_Sun-May-29_21-50-20_2016_devworks_T3.txt", header = T, quote = "", sep = "\t")
dat <- as.data.frame(dat)
dev.new(width=5, height=15)
plot(dat$idf, dat$PROBABILITY, main="TOPIC 3 dev Works db2 Forum", sub=paste("N=", dat$N[1]), xlab="idf", ylab="probability", pch = NA)
text(dat$idf, dat$PROBABILITY, labels=dat$KEYWORD, cex= 1.2, col="red")
grid()

 
dat<-read.delim(file = "idf_Sun-May-29_21-50-20_2016_devworks_T4.txt", header = T, quote = "", sep = "\t")
dat <- as.data.frame(dat)
dev.new(width=5, height=15)
plot(dat$idf, dat$PROBABILITY, main="TOPIC 4 dev Works db2 Forum", sub=paste("N=", dat$N[1]), xlab="idf", ylab="probability", pch = NA)
text(dat$idf, dat$PROBABILITY, labels=dat$KEYWORD, cex= 1.2, col="red")
grid()

 
dat<-read.delim(file = "idf_Sun-May-29_21-50-20_2016_devworks_T5.txt", header = T, quote = "", sep = "\t")
dat <- as.data.frame(dat)
dev.new(width=5, height=15)
plot(dat$idf, dat$PROBABILITY, main="TOPIC 5 dev Works db2 Forum", sub=paste("N=", dat$N[1]), xlab="idf", ylab="probability", pch = NA)
text(dat$idf, dat$PROBABILITY, labels=dat$KEYWORD, cex= 1.2, col="red")
grid()
