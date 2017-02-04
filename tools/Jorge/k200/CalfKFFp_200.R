############################################
## Calcs File K, F, Fp to plot
## This program only generates the files to be plotted with K, F, Fp..
## don't forget to customize ds name
## Author. Jorge Lopez
## Date. Aug. 2016
##       Feb. 2017 constraining data to k<=200 and calculating b for power of 6
##                 coefficients recalculated for predicting b
###########################################


b <- function(X,N)
{

##  Coefficients:
##    Estimate Std. Error t value Pr(>|t|)
##  (Intercept) -1.405e+00  3.451e-02 -40.716  < 2e-16 ***
##    N            2.930e-03  1.709e-04  17.141  < 2e-16 ***
##    I(N^2)      -5.654e-06  3.311e-07 -17.078  < 2e-16 ***
##    I(N^3)       5.367e-09  3.206e-10  16.742  < 2e-16 ***
##    I(N^4)      -2.637e-12  1.644e-13 -16.036  < 2e-16 ***
##    I(N^5)       6.436e-16  4.258e-17  15.116  < 2e-16 ***
##    I(N^6)      -6.173e-20  4.370e-21 -14.125  < 2e-16 ***
##    X            1.882e-02  1.651e-03  11.396  < 2e-16 ***
##    I(X^2)      -1.502e-03  2.503e-04  -6.000 2.13e-09 ***
##    I(X^3)       7.468e-05  1.717e-05   4.350 1.39e-05 ***
##    I(X^4)      -2.057e-06  5.855e-07  -3.513 0.000448 ***
##    I(X^5)       2.907e-08  9.685e-09   3.001 0.002702 **
##    I(X^6)      -1.642e-10  6.189e-11  -2.653 0.008016 **
    
  
##return (-7.783e-01 + 2.191e-07 * N + 5.364e-09 * N^2 + 4.478e-03 * X - -4.218e-05 * X^2)
  
  return (-1.405e+00 + 
            2.930e-03 * N   +
            -5.654e-06 * N^2 +
            5.367e-09 * N^3 +
            -2.637e-12 * N^4 +
            6.436e-16 * N^5 +
            -6.173e-20 * N^6 +
            1.882e-02 * X   +
            -1.502e-03 * X^2 +
            7.468e-05 * X^3 +
            -2.057e-06 * X^4 +
            2.907e-08 * X^5 +
            -1.642e-10 * X^6)
  }

## main ##

dS <- "android" ## you need to change for every directory
##dS <- "----dba" ## you need to change for every directory
##dS <- "salesfc" ## you need to change for every directory

listf <- list.files(pattern='FileFN-.*\\.topic_frequency') 


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
    Fp <- X ^ -b(X,N) * K ^ b(X,N)
    cat(K, F, Fp, "\n", sep="\t", append = T, file = outFileFp) # detail to file
  }
}
