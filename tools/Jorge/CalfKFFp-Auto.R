############################################
## Calcs File K, F, Fp to plot
## This program only generates the files to be plotted with K, F, Fp..
## don't forget to customize ds name
## Author. Jorge Lopez
## Date. Aug. 2016
###########################################


b <- function(X,N)
{
  return (-7.783e-01 + 2.191e-07 * N + 5.364e-09 * N^2 + 4.478e-03 * X - -4.218e-05 * X^2)
}

## main ##

dS <- "android" ## you need to change for every directory
dS <- "----dba" ## you need to change for every directory
dS <- "salesfc" ## you need to change for every directory

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

  
## file output
 




