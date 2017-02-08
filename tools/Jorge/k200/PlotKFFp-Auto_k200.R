## This program plots K,F from processed LDA frequency files dba monthly
##
## assumesthat list-fs is the list of files to plot and they are ordered
## grouped by X=2,15,30,40 by year-month
## the path to save the plots and the file plots, need to be customized
## These plot are to be eyeballed and see when the fitted line deviates
## Author George Lopez. Aug 2016
##                      Feb 2017 No changes, however program file was changed to PlotkFFp-Auto_k200.R

library(jpeg)

listf <- list.files(pattern='File-toPlot-Fp') 
listf <-as.data.frame(listf)
i <- 1

#############
while( i < nrow(listf)) {
  ## change this!!
  plotfn <- paste("plot-Fp-android-", substr(listf[i,1],23,29), ".jpg", sep="")
  ##plotfn <- paste("plot-Fp-----dba-", substr(listf[i,1],23,29), ".jpg", sep="")
  ##plotfn <- paste("plot-Fp----dba-q", substr(listf[i,1],23,29), ".jpg", sep="")
  ##plotfn <- paste("plot-Fp-salesfc-", substr(listf[i,1],23,29), ".jpg", sep="")
  
  ##pathsave <- file.path("C:","Users","Jorge","OneDrive", "MSc Terms", "Thesis", "Fp Plots", "dba-m", plotfn)
  ##pathsave <- file.path("C:","Users","Jorge","OneDrive", "MSc Terms", "Thesis", "Fp Plots", "dba-q", plotfn)
  ##pathsave <- file.path("C:","Users","Jorge","OneDrive", "MSc Terms", "Thesis", "Fp Plots", "salesforce", plotfn)
  ##pathsave <- file.path("C:","Users","Jorge","OneDrive", "MSc Terms", "Thesis", "bpredicted", "FK-02-15-20-40", "android", plotfn)
  ##pathsave <- file.path("C:","Users","Jorge","OneDrive", "MSc Terms", "Thesis", "bpredicted", "FK-02-15-20-40", "dba-m", plotfn)
  ##pathsave <- file.path("C:","Users","Jorge","OneDrive", "MSc Terms", "Thesis", "bpredicted", "FK-02-15-20-40", "dba-q", plotfn)
  ##pathsave <- file.path("C:","Users","Jorge","OneDrive", "MSc Terms", "Thesis", "bpredicted", "FK-02-15-20-40", "salesforce", plotfn)
  ##pathsave <- file.path("C:","Users","Jorge","OneDrive", "MSc Terms", "Thesis", "bpredicted", "FK-Inflexion", "android", plotfn)
  pathsave <- file.path("C:","Users","Jorge","OneDrive", "MSc Terms", "Thesis", "N200", plotfn) ## this is just a testing path
  cat("This is pathsave=", pathsave, "\n")
  
  jpeg(file=pathsave)
  
  n<-as.character(listf[i,])
  dat<-read.delim(file = n, header = T, quote = "", sep = "\t")
  dat <- as.data.frame(dat)
  ## dat <- dat [1 : as.integer( nrow(dat) * 0.75), ] ## considered only 75% of data. I already cut it from OnlytoFit!! this is N/A here
  ### X=2
  ## title must be changed!
  plot(dat$K, dat$F,log = "xy", 
        main=paste("K, F rel. & Fp fit ds:android", substr(listf[i,1],23,29), "for X=2,15,30,40"), 
        pch=".", xlab="K", ylab="F", col = "orange") ## change ds!
  grid (lty = "dotted", col = "red", equilogs = TRUE)
  #abline(v=c(10,15,20,30,40,50,75,100,150,200,300,400,500),lty=3, col='red')
  abline(v=c(5,10,15,20,25,50,75,100,150,200,250,350,450,500,750,1000),lty=3, col='red')
  lines(dat$K, dat$F,col = "orange" )
  lines(dat$K, dat$Fp, col="green")
  cat("lines listf[", i, ",]=", n,"\n")
  i <- i + 1
 
  ## X=15
  n<-as.character(listf[i,])
  dat<-read.delim(file = n, header = T, quote = "", sep = "\t")
  lines(dat$K, dat$F, pch=".",xlab="k", ylab = "F", col="brown")
  lines(dat$K, dat$Fp, col="green")
  cat("lines listf[", i, ",]=", n,"\n")
  
  i <- i + 1
  
  ## X=30
  n<-as.character(listf[i,])
  dat<-read.delim(file = n, header = T, quote = "", sep = "\t")
  lines(dat$K, dat$F, pch=".",xlab="k", ylab = "F", col="red")
  lines(dat$K, dat$Fp, col="green")
 
  cat("lines listf[", i, ",]=", n,"\n")
  i <- i + 1

  ## X=40
  n<-as.character(listf[i,])
  dat<-read.delim(file = n, header = T, quote = "", sep = "\t")
  lines(dat$K, dat$F, pch=".",xlab="k", ylab = "F", col="brown")
  lines(dat$K, dat$Fp, col="green")
  
  cat("lines listf[", i, ",]=", n,"\n")
  dev.off()
  i <- i + 1  
}
#############








