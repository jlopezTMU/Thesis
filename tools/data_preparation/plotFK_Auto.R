## This program plots K,F from processed LDA frequency files dba monthly
##
## assumesthat list-fs is the list of files to plot and they are ordered
## gruped by X=5,2- and 50 by year-month
## the path to save the plots needs to be customized
## Author George Lopez. May 2016
library(jpeg)

lf<-read.delim(file = "list-fs.txt", header = T, quote = "", sep = "\t")
lf<-as.data.frame(lf)
i <- 1

##for (i in 1:nrow(lf)) {
  while( i < nrow(lf)) {
    plotfn <- paste("plot-dba-m-", substr(lf[i,1],25,31), ".jpg", sep="")
    pathsave <- file.path("C:","Users","Jorge","OneDrive", "MSc Terms", "Thesis", "saved-plots", "dba-m", plotfn)
    
    jpeg(file=pathsave)
    
    n<-as.character(lf[i,])
    dat<-read.delim(file = n, header = T, quote = "", sep = "\t")
    dat <- as.data.frame(dat)
    ############## dev.new(width=5, height=15)
  
    plot(dat$K, dat$F,log = "xy", main=paste("F & K rel. dba ", substr(lf[i,1],25,31), "for X=5,20,50"), pch=".", xlab="K", ylab="F")
    grid()

    dat<-read.delim(file = n, header = T, quote = "", sep = "\t")
    dat <- as.data.frame(dat)
    lines(dat$K, dat$F, pch=".",xlab="k", ylab = "F", col="blue")
    cat("lines lf[", i, ",]=", n,"\n")

    i<-i+1
    n<-as.character(lf[i,])
    dat<-read.delim(file = n, header = T, quote = "", sep = "\t")
    dat <- as.data.frame(dat)
    lines(dat$K, dat$F, pch=".",xlab="k", ylab = "F", col= "red")
    cat("lines lf[", i, ",]=", n,"\n")

    i<-i+1
    n<-as.character(lf[i,])
    dat<-read.delim(file = n, header = T, quote = "", sep = "\t")
    dat <- as.data.frame(dat)
    lines(dat$K, dat$F, pch=".",xlab="k", ylab = "F", col="green")
    cat("lines lf[", i, ",]=", n,"\n")
    dev.off()
    i <- i+1
  
} 


