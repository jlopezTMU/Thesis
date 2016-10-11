## This program plots K,F from processed LDA frequency files dba monthly
##
## assumesthat list-fs is the list of files to plot and they are ordered
## grouped by X=5,2- and 50 by year-month
## the path to save the plots and the file plots, need to be customized
## These plot are to be eyeballed and see when the fitted line deviates
## Author George Lopez. Aug 2016

library(jpeg)

listf <- list.files(pattern='File-toPlot-Fp') 
listf <-as.data.frame(listf)
i <- 1

#############
while( i < nrow(listf)) {
  plotfn <- paste("plot-Fp-android-", substr(listf[i,1],23,29), ".jpg", sep="")
  plotfn <- paste("plot-Fp-----dba-", substr(listf[i,1],23,29), ".jpg", sep="")
  plotfn <- paste("plot-Fp-salesfc-", substr(listf[i,1],23,29), ".jpg", sep="")
  pathsave <- file.path("C:","Users","Jorge","OneDrive", "MSc Terms", "Thesis", "Fp Plots", "android", plotfn)
  pathsave <- file.path("C:","Users","Jorge","OneDrive", "MSc Terms", "Thesis", "Fp Plots", "dba-q", plotfn)
  pathsave <- file.path("C:","Users","Jorge","OneDrive", "MSc Terms", "Thesis", "Fp Plots", "salesforce", plotfn)
  jpeg(file=pathsave)
   jpeg(file=pathsave)
  
  n<-as.character(listf[i,])
  dat<-read.delim(file = n, header = T, quote = "", sep = "\t")
  dat <- as.data.frame(dat)
  ### X=5
  plot(dat$K, dat$F,log = "xy", 
        main=paste("K, F rel. & Fp fit ds:salesforce ", substr(listf[i,1],23,29), "for X=5,20,50"), 
        pch=".", xlab="K", ylab="F", col = "orange")
  grid() 
  
  lines(dat$K, dat$Fp, col="green")
  cat("lines listf[", i, ",]=", n,"\n")
  i <- i + 1
 
  ## X=20
  n<-as.character(listf[i,])
  dat<-read.delim(file = n, header = T, quote = "", sep = "\t")
  lines(dat$K, dat$F, pch=".",xlab="k", ylab = "F", col="brown")
  lines(dat$K, dat$Fp, col="green")
  cat("lines listf[", i, ",]=", n,"\n")
  
  i <- i + 1
  
  ## X=50
  n<-as.character(listf[i,])
  dat<-read.delim(file = n, header = T, quote = "", sep = "\t")
  lines(dat$K, dat$F, pch=".",xlab="k", ylab = "F", col="red")
  lines(dat$K, dat$Fp, col="green")
 
  cat("lines listf[", i, ",]=", n,"\n")
  dev.off()
  i <- i + 1
  
}
#############



##dat.filt <- dat [1 : as.integer( nrow(dat) * 0.75), ]




