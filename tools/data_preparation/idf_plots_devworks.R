## This Script plots the idf* input files (idf x probability) residing in the pathsave directory
## The corpus files are provided by IBM, after processed with do_analysis_db2_idf_devWorks.R it produces the 
## idf files
## the pathsave needs to be customized
## Author. Jorge Lopez June 2016

library(jpeg)

listf <- list.files(pattern = "idf_.*\\.txt")
## Z:\DR Miransky\devWorks\New DevWorks

for(i in 1:length(listf)) {
  ###dev.new(width=16,height=9,noRStudioGD = TRUE)
  ##dev.off()
  plotfn <- paste(substr(listf[i],1,25), ".jpg", sep="")
  ###pathsave <- file.path("Z:","DR Miransky", "devWorks","New DevWorks", plotfn)
  pathsave <- file.path("C:", "Users", "owner", "Documents", "MSC", "db2", "New devWorks", "plots", plotfn)
  jpeg(file=pathsave, width=1500, height=1000)
  dat<-read.csv(listf[i])
  dat <- as.data.frame(dat)
  ##dev.new()
  plot(dat$idf, dat$PROBABILITY, main=paste("TOPIC ", dat$TOPIC[1], "-", dat$YEAR[1], " dev Works db2 Forum",sep=""), sub=paste("N=", dat$N[1]), font=4, xlab="idf", ylab="probability", pch = NA)
  text(dat$idf, dat$PROBABILITY, labels=dat$KEYWORD, cex= 1.2, col="red")
  grid()
  ###lines(dat$idf, dat$PROBABILITY)
  cat("File plotted is ", listf[i], "number=", i,"\n")
  graphics.off()
}
