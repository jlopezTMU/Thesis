################################################################################################################
## Program. toFit (redesigned)
## Author. Jorge Lopez
## Date. July 25, 2016
## This program iterates over the specified datasets, fits a nls into a df, rbinds the dfs an then fits b with a lm
## This program assumes that the input file is structured correctly, therefore no exhaustive validations need to
## take place
## mod. Aug 4. only considers 75% of rows...
## mod. Aug 6. Code rewriten to better use R arrays handling capabilities
## mod. Feb 4. modified lines 38 and 39
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
print(XSystime)
dat <- read.csv(readFromfileName, header = T, sep = ",", row.names = NULL) ## NOT EXCEL
validGroups <- unique(paste(dat$timeframe_type, dat$timeframe, dat$dataset_name, sep = " "))
for(iG in 1:length(validGroups)) {
  for(itopXX in 2:50) {
    s <- strsplit(validGroups[iG], " ")
    sdf <- as.data.frame(s)
    p1 <- as.character(sdf[1,]) ## timeframe_type
    p2 <- as.character(sdf[2,]) ## timeframe
    p3 <- as.character(sdf[3,]) ## dataset_name
    ds <- dat[dat$timeframe_type ==  p1 & dat$timeframe == p2 & dat$dataset_name == p3 & dat$topXX == itopXX,]
    cat("p1=", p1, " p2=", p2, " p3=", p3, " itopXX=", itopXX, " iG=", iG, "\n")
 
    ds <- ds[ds$topicCount <= 200, ] ## added
    ds <- ds[ds$topicCount < 0.75 * max(ds$documentCount), ] ## uncommented
    topX.nls <- nls(ds$postFraction ~ ds$topXX^(-b) * ds$topicCount^b, data = ds, start = list( b = -1),)
    to_fit <- rbind(to_fit, data.frame( b = coef(topX.nls), X = max(ds$topXX), N = max(ds$documentCount) ))
  }
}

  ## b.lm <- lm(b ~ N + I(N^2) + X + I(X^2), data = to_fit) ## 
  
  b.lm <- lm(b ~ N + I(N^2) + I(N^3) + I(N^4) + I(N^5) + I(N^6) + 
               X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6), data = to_fit) ## considers power 6 to fit b
  
  ##b.lm <- lm(b ~ N + I(N^2) + I(N^3) + X + I(X^2) + I(X^3), data = to_fit)
  
  summary(b.lm)
  
  fn <- "toFit_file"
  if (file.exists(fn)) file.remove(fn)
  write.csv(to_fit,file=fn)
  
  print(XSystime)
  
