## 
## Calcs File K, F, Fp to plot
## This program only generates the files to be plotted with K, F, Fp..
## don't forget to customize ds name
## Author. Jorge Lopez
## Date. Aug. 2016
##       Feb. 2017 constraining data to k<=200 and calculating b for power of 6
##                 coefficients recalculated for predicting b
##       May. 2017 clac Fp as a function of b1 and b2
## 

b1 <- function(X,N)
{
   return (
    0.3824803 +
    0.6490938 * (log(X)) +
      0 * N )
} 

b2 <- function(X,N)
{
 
  return (
      -8.408351e-01 +
       2.110667e-03 * X +
       3.397901e-05 * N
  )
    
}



## main ##

dS <- "android" ## you need to change for every directory
##dS <- "----dba" ## you need to change for every directory
##dS <- "salesfc" ## you need to change for every directory

##listf <- list.files(pattern='File-FK-.*\\.topic_frequency') ## changes the prefix of files new file nomenclature
listf <- list.files(pattern='FileFN-.*\\.topic_frequency') ## old file nomenclature!

for (i in 1:length(listf))
{
  readFromfileName <- listf[i]
  sX <- substr(gsub("\\.","-",readFromfileName),8,9)
  X <- strtoi(sX)
  p <- substr(readFromfileName,25,31)
  outFileFp<-paste("File-toPlot-Fp-", dS, p, "-", sX, ".csv", sep="") ## Correction, taking K, F from the input file ensures that they will be the same values in output KFFp file  
  cat(outFileFp,"\n")

  n <- readFromfileName
  dat<-read.csv(n, header=TRUE, sep="\t") ## read input file
  
  nr <- nrow(dat)
  
  N <- dat$K[nr]
  
  if (file.exists(outFileFp)) file.remove(outFileFp) ## remove the output file if already exists
  
  cat("K", "F", "Fp", "\n", sep="\t", append = T, file = outFileFp) # header to file
  
  for (j in 2:nr) {
    K <- dat$K[j]
    F <- dat$F[j]
    
    
    ##Fp <- X ^ b1(X,0) * K ^ b2(X,N)
    
    G <- b1(X,0)*log(X) + b2(X,N)*log(K)
    
    ##Fp <- X ^ b2(X,N) * K ^ b2(X,N)
    
    ##cat("DEBUG G=", G, "\n")
    
    Fp <- exp(G)
    
    ##cat(" F vs Fp ", F, "vs", Fp, "\n")
    
    cat(K, F, Fp, "\n", sep="\t", append = T, file = outFileFp) # detail to file
  }
}
