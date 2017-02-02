############################################
## Calcs File K, F, Fp to plot
## This program only generates the files to be plotted with K, F, Fp..
## don't forget to customize ds name
## Author. Jorge Lopez
## Date. Aug. 2016
##       Feb. 2017 constraining data to k<=200 and calculating b for power of 6
###########################################


b <- function(X,N)
{
## Coefficients:
##    Estimate Std. Error t value Pr(>|t|)    
##  (Intercept) -1.540e+00  4.073e-02 -37.799  < 2e-16 ***
##    N            3.558e-03  2.017e-04  17.640  < 2e-16 ***
##    I(N^2)      -6.816e-06  3.907e-07 -17.445  < 2e-16 ***
##    I(N^3)       6.461e-09  3.784e-10  17.078  < 2e-16 ***
##    I(N^4)      -3.188e-12  1.941e-13 -16.430  < 2e-16 ***
##    I(N^5)       7.842e-16  5.025e-17  15.606  < 2e-16 ***
##    I(N^6)      -7.592e-20  5.158e-21 -14.720  < 2e-16 ***
##    X            1.994e-02  1.949e-03  10.231  < 2e-16 ***
##    I(X^2)      -1.555e-03  2.954e-04  -5.263 1.48e-07 ***
##    I(X^3)       7.719e-05  2.026e-05   3.810 0.000141 ***
##    I(X^4)      -2.126e-06  6.910e-07  -3.076 0.002111 ** 
##    I(X^5)       3.005e-08  1.143e-08   2.629 0.008582 ** 
##    I(X^6)      -1.698e-10  7.305e-11  -2.325 0.020141 *  
  
##return (-7.783e-01 + 2.191e-07 * N + 5.364e-09 * N^2 + 4.478e-03 * X - -4.218e-05 * X^2)
  
  return (-1.540e+00 + 
           3.558e-03 * N   +
          -6.816e-06 * N^2 +
           6.461e-09 * N^3 +
          -3.188e-12 * N^4 +
           7.842e-16 * N^5 +
          -7.592e-20 * N^6 +
           1.994e-02 * X   +
          -1.555e-03 * X^2 +
           7.719e-05 * X^3 +
          -2.126e-06 * X^4 +
           3.005e-08 * X^5 +
          -1.698e-10 * X^6)
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
