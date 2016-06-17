## Reading a whole K&F file of over 800 points
## This program reads the file, file-merged that contains the coordinates for X=x
## plots the points, fits them applying the lm model and summarizes them..
## Author. Jorge Lopez 6-2016

n<-"file-merged"
dat<-read.csv(n, header = TRUE, sep=",")
dat <- as.data.frame(dat, colnames=c("K","F"))
dev.new()

##dat.filt <- dat [1 : as.integer( nrow(dat) * 0.75), ]
dat.filt <- dat [1 : as.integer( nrow(dat) * 0.75), ]


plot(dat$K, dat$F, log="xy", main="F&K for dba-m, dba-q and salesforce-m datasets", pch=".", xlab="K", ylab="F")
##plot(dat$K, dat$F, log="xy", main="F&K ", pch=".", xlab="K", ylab="F")

### lines(dat$K, dat$F) ## don't plot with lines.. too messy for so many points
lm1 <- lm(log(F) ~ log(K), data=dat.filt)
summary(lm1)

lines(dat.filt$K,exp(lm1$fitted), lwd=3, col="red")
nrow(dat)
grid()

plot(lm1)
