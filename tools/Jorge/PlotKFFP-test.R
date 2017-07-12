## This program plots K,F from processed LDA frequency files 
##
## assumesthat list-fs is the list of files to plot and they are ordered
## grouped
## the path to save the plots and the file plots, need to be customized
## These plot are to be eyeballed and see when the fitted line deviates
## Author George Lopez. Aug 2016
##                      Feb 2017 No changes, however program file was changed to PlotkFFp-Auto_k200.R
##                      Mar 2017 add RMSE calculation, writing in a file 

##library(jpeg) dont know if this pkg exists!

listf <- list.files(pattern='File-toPlot-Fp') 
listf <-as.data.frame(listf)
i <- 1

#############
##while( i <= nrow(listf)) {
while( i <= nrow(listf)) {
    ## file names of plots
  ## change this!!
  ##plotfn <- paste("plot-Fp-android-", substr(listf[i,1],23,29), ".pdf", sep="")
  ##plotfn <- paste("plot-Fp-----dba-", substr(listf[i,1],23,29), ".pdf", sep="")
  plotfn <- paste("plot-Fp----dba-q", substr(listf[i,1],23,29), ".pdf", sep="")
  ##plotfn <- paste("plot-Fp-salesfc-", substr(listf[i,1],23,29), ".pdf", sep="")
  
  cat("IN WHILE LOOP")



  ##pathsave <- file.path("C:","Users","Jorge","OneDrive", "MSc Terms", "MSc Winter 2017", "Thesis", "plots", "android-test", plotfn) ## this is the output path!
  ##pathsave <- file.path("C:","Users","Jorge","OneDrive", "MSc Terms", "MSc Winter 2017", "Thesis", "plots", "dba-m-test", plotfn) ## this is the output path!
  pathsave <- file.path("C:","Users","Jorge","OneDrive", "MSc Terms", "MSc Winter 2017", "Thesis", "plots", "dba-q-test", plotfn) ## this is the output path!
  pdf(file=pathsave)
  RMSE<-array(1:5)
  

  ##ds <- "android"
  ##ds <- "dba-m"
  ds <- "dba-q"
  ##ds <- "salesforce"

  per <- substr(listf[i,1],23,29)
  n<-as.character(listf[i,])
  dat<-read.delim(file = n, header = T, quote = "", sep = "\t")
  dat <- as.data.frame(dat)
  ## dat <- dat [1 : as.integer( nrow(dat) * 0.75), ] ## considered only 75% of data. I already cut it from OnlytoFit!! this is N/A here
  
  ### X=5
  ## title must be changed!
  plot(dat$K, dat$F,log = "xy", 
        main=paste("K, F rel. & Fp fit ds:", ds, per, "for X=5,10,25,50"), 
        pch=".", xlab="K", ylab="F", col = "orange") ## change ds!
  grid (lty = "dotted", col = "red", equilogs = TRUE)
  #abline(v=c(10,15,20,30,40,50,75,100,150,200,300,400,500),lty=3, col='red')
  abline(v=c(5,10,15,20,25,50,75,100,150,200,250,350,450,500,750,1000),lty=3, col='red')
  lines(dat$K, dat$F,col = "orange" )
  
  lines(dat$K, dat$Fp, col="green")
  
  cat("lines listf[", i, ",]=", n,"\n")
  RMSE[1] <- sqrt(mean((dat$F-dat$Fp)^2))
  cat(ds, per, X=5, RMSE[1], "\n", sep=",", append = T, file = "outFileRMSE.csv") ## MANUAL!
  i <- i + 1
 
  ## X=10
  n<-as.character(listf[i,])
  dat<-read.delim(file = n, header = T, quote = "", sep = "\t")
  lines(dat$K, dat$F, pch=4, col="brown")
  lines(dat$K, dat$F, pch=".",xlab="k", ylab = "F", col="brown")
  
  
  lines(dat$K, dat$Fp, col="green")
  cat("lines listf[", i, ",]=", n,"\n")
  RMSE[2] <- sqrt(mean((dat$F-dat$Fp)^2))
  cat(ds, per, X=10, RMSE[2], "\n", sep=",", append = T, file = "outFileRMSE.csv") ## MANUAL!
  i <- i + 1
  
  ## X=25
  n<-as.character(listf[i,])
  dat<-read.delim(file = n, header = T, quote = "", sep = "\t")
  lines(dat$K, dat$F, pch=".",xlab="k", ylab = "F", col="red")
  lines(dat$K, dat$Fp, col="green")
 
  cat("lines listf[", i, ",]=", n,"\n")
  RMSE[3] <- sqrt(mean((dat$F-dat$Fp)^2))
  cat(ds, per, X=25, RMSE[3], "\n", sep=",", append = T, file = "outFileRMSE.csv") ## MANUAL!
  i <- i + 1

  ## X=50
  n<-as.character(listf[i,])
  dat<-read.delim(file = n, header = T, quote = "", sep = "\t")
  lines(dat$K, dat$F, pch=".",xlab="k", ylab = "F", col="brown")
  lines(dat$K, dat$Fp, col="green")
  
  cat("lines listf[", i, ",]=", n,"\n")
  RMSE[4] <- sqrt(mean((dat$F-dat$Fp)^2)) 
  cat(ds, per, X=50, RMSE[4], "\n", sep=",", append = T, file = "outFileRMSE.csv") ## MANUAL!

  dev.off()
  i <- i + 1  
}
#############
