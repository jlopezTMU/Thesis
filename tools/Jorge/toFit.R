################################################################################################################
## Program. toFit
## Author. Jorge Lopez
## Date. July 25, 2016
## This program iterates over the specified datasets, fits a nls into a df, rbinds the dfs an then fits b with a lm
## This program assumes that the input file is structured correctly, therefore no exhaustive validations need to
## take place
## mod. Aug 4. only considers 75% of rows...
################################################################################################################

to_fit <- data.frame(b = c(), X=c(), N=c())
topX.nls <- vector()
XSystime <<- format(Sys.time(), "%a-%b-%d_%H-%M-%S_%Y")

##readFromfileName = "topX.5"
readFromfileName = "topX.csv"

#####################################################################################################################
#####                                           main                                                            #####
#####################################################################################################################

##dat <- read.csv(readFromfileName, header = T, sep = "\t", row.names = NULL) ## EXCEL
dat <- read.csv(readFromfileName, header = T, sep = ",", row.names = NULL) ## NOT EXCEL

##for (i in 1:nrow(dat)) {
  i <- 1
  change <- FALSE 
  rGroup <- paste(dat$timeframe_type[i], dat$timeframe[i], dat$dataset_name[i], dat$topXX[i], sep = "")
  cat("rGroup =",rGroup, "\n")
  
  Gi <- i ## initial index of data group
  Gf <- i ## final index of data group
  
  nEOF <- nrow(dat)
  cat("dat=", nEOF, "\n")
  while( i <= nEOF) { ## avoiding i=last row
    ##cat ("i at start of while is", i, "\n")
    
    if(i == nEOF) {EOF <- TRUE} else {EOF <- FALSE}
    
    rGroup_Last <- rGroup
    
    i <- i + 1
    
    if(change & !EOF) { Gi <- i }
    
    ##rGroup <- paste(dat$timeframe_type[i], dat$timeframe[i], dat$dataset_name[i], sep = "")
    rGroup <- paste(dat$timeframe_type[i], dat$timeframe[i], dat$dataset_name[i], dat$topXX[i], sep = "")
    ##cat("rGroup=", rGroup, "\n")
    
    if(rGroup == rGroup_Last) {
       change <- FALSE
       ## cat("Group is the same rGroup = rGroup_Last\n")
    } else 
      {
      ## gotta process here
        cat("Group changed \n")
        change <- TRUE
        
        i <- i - 1; Gf <- i;
        cat("i=", i, " Gi=", Gi, " Gf=", Gf, "\n")
        Gf <- round((Gf-Gi)*0.75)
        ds <- dat[Gi:Gf,] ## only considers 75% of rows...
        topX.nls <- nls(ds$postFraction ~ ds$topXX^(-b) * ds$topicCount^b, data = ds, start = list( b = -1),)
        cat("After topX.nls i=",i, "\n")
        to_fit <- rbind(to_fit, data.frame( b = coef(topX.nls), X = max(ds$topXX), N = max(ds$documentCount) ))
        cat("After to_fit i=", i, "max(ds$topXX)=", max(ds$topXX), "\n")
      }
  }
  


  ## b.lm <- lm(b ~ N + I(N^2) + X + I(X^2), data = to_fit)
  
  b.lm <- lm(b ~ N + I(N^2) + I(N^3) + X + I(X^2) + I(X^3), data = to_fit)
  
  summary(b.lm)
  
  write.csv(to_fit,file="tofit_075_NX")
  
  
