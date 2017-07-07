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

a <- function(X,N)
  ##  > coef(dat.lm.intercept.complex)
  ## (Intercept)   I(log(X))   I(log(N)) I(log(X)^2) I(log(N)^2) 
  ## -9.04745013  1.08108198  2.69351748 -0.08338687 -0.20200808
  ## lm_intercept ~ I(log(X)) + I(log(N)) + I(log(X)^2) + I(log(N)^2)
{
   return (
     -9.04745013 +
      1.08108198 * log(X) +
      2.69351748 * log(N) + 
     -0.08338687 * log(X)^2 +
     -0.20200808 * log(N)^2
   )
} 

b <- function(X,N)
  ## > coef(dat.lm.b.complex)
  ## (Intercept)             X             N        log(X) 
  ## -8.360828e-01  8.559470e-04  3.118992e-05  3.498468e-02 
  ## dat.lm.b.complex <- lm(b ~ X + N + log(X) , data = dat)
{
  #Fp <- -8.188686e-01 +
   #  7.053422e-04 * X +
   #  1.546972e-05 * N +
    # 3.076297e-02 * log(X)
 
  
  # DS1
  # >  coef(dat.lm.b.complex)
  # (Intercept)             X             N        log(X) 
  # -8.188686e-01  7.053422e-04  1.546972e-05  3.076297e-02
  
 ##cat("DEBUG: X=", X, " N=", N, "Fp = ", Fp ,"\n")
  return (
    -8.188686e-01 +
      7.053422e-04 * X +
      1.546972e-05 * N +
      3.076297e-02 * log(X)
  )
    
}



## main ##

##dS <- "android" ## you need to change for every directory
dS <- "----dba" ## you need to change for every directory
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
  
  ##nr <- nrow(dat) ## ENABLED 
  ##nr <- round(dat$K[nrow(dat)] * 0.75) ## correction
  ##dat <- dat [1 : as.integer( nrow(dat) * 0.75), ] ## FIX
  
  ### ORI nr <- nrow(dat)
  ### ORI N <- dat$K[nr]
  
  ##dat <- dat [1 : as.integer( nrow(dat) * 0.75), ] ## FIX
  
  ##dont need nr <- 200 ## fixed to 200!!!!!!!!!!!!!!!!!!
  
  X <- round(dat$K[nrow(dat)] - (dat$K[nrow(dat)] * 0.75))
  
  Y <- round(X/25)
   
  nr <- dat$K[nrow(dat)] - 25 * (Y) ## This works only if N > 500!!
  
  N <- dat$K[nrow(dat)-Y]
  
  ##dont need N <- 200
  
  if (file.exists(outFileFp)) file.remove(outFileFp) ## remove the output file if already exists
  
  cat("K", "F", "Fp", "\n", sep="\t", append = T, file = outFileFp) # header to file
  
  ##for (j in 2:nr) {
  j <- 1
  while (dat$K[j] <= nr && !is.na(dat$K[j])) {
    ##cat("K, F, J=", K, " ", F, " ", j, "\n")
    K <- dat$K[j]
    F <- dat$F[j]
    
    Fp <- X ^ -b(X,N) * K ^ b(X,N)
    
    cat(K, F, Fp, "\n", sep="\t", append = T, file = outFileFp) # detail to file
    j <- j + 1
  }
}
