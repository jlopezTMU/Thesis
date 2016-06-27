## This program reads the frequencies files produced by the do_lda_analysis 
## extracts top topics and calculates their corresponding F for X=1..50
## in this way For each topic.frequency file in dataset-i it will be generated 50 files 
## of data points
## 
## Modified to automatically read a list of datasets and generate frequency files 
## for x=1..50
##
##
## Author. Jorge Lopez June 2016

library(plyr) ## to use arrange f


readFile <- function(fName) {
###########################
  dat <- read.delim(file = fName, header = T, fill=T, sep = "\t",
                    row.names=NULL)
  colnames(dat)<-c(colnames(dat)[-1],"x")
  dat$x<-NULL
  
  dat <- arrange(dat, strtoi(topicCount)) ##sorting the vector to prevent df not sorted well
  
  return(dat)
}

calcGroups <- function(dat, outFileFK, X) {
#########################################
## X must be <= nrow(dat)
  
  subCountVec <- c()
  j <- 0 ## initialize j index of subCountVec
  i <- 1
  
  #$# cat("*** X is ", X, "\n ")
  if(X > nrow(dat)) { return(0) } ##nothing to do here 
  
  while (strtoi(dat$topicCount[i]) < X) {
    
    i <- i + 1;
    #$#cat("***dat$topicCount[", i, "]=", dat$topicCount[i], "\n")
    #$#cat("***X=", X, "\n")
    #$#cat("***i=", i, "\n")
  } ## find element >= X 
  
  for (i in i:nrow(dat)) {
    
    #$#cat("i =", i, "\n")
    if (!is.na(dat$topicCount[i+1])) {
      
      if (dat$topicCount[i] == dat$topicCount[i+1]) {
        #$#cat ("topic.Count[", i, "] and topic.Count[", i+1, "] are the same\n")
        j<-j+1
        k<-dat$topicCount[i]
        subCountVec[j]<-dat$topic.frequency[i]
        subCountVec[j+1]<-dat$topic.frequency[i+1]
        
      } else {
        k<-dat$topicCount[i]
        #$#cat("**k is:", k, "\n")
        j <- j+1
        #$#cat("j is", j, "\n")
        subCountVec[j]<-dat$topic.frequency[i]
        #$#cat("Change of TopicCount to ", dat$topicCount[i], "\n")
        j<-0
        #$#cat("subCountVec Sorted is:\n")
        subCountVec <- sort(subCountVec, decreasing=T)
        #$#cat(subCountVec)
        ## calculate F
        F <- sum(subCountVec[1:X])/sum(subCountVec)
        #$#cat(">>>>>>>>>>>>>>>>>>> K, F is", k, F, "\n")
        cat(k, F, "\n", sep="\t", append = T, file = outFileFK[X]) # to file
        ## readline(prompt="press any key to continue ")
      } ## else
      
    } else {
      #$#cat ("EOF reached \n") 
      k<-dat$topicCount[i]
      
      #$#cat("subCountVec Sorted is:\n")
      subCountVec <- sort(subCountVec, decreasing=T)
      #$#cat(subCountVec)
      ## calculate F
      F <- sum(subCountVec[1:X])/sum(subCountVec)
      cat(k, F, "\n", sep="\t", append = T, file = outFileFK[X]) # to file
      #$#cat("**k:", k, " and F:", F, "\n")
    }
    
  } ##for
  
  return(1)
  
} ## function

processAllx <- function(dsName, FKdir, ffDir, listf, i_dsName, i, j) {
######################################################################

    outFileFK <- c()
    
    cat(" ****** In (ProcessAllx) For each file:", listf[i], " i is ",  i, "*************\n")
    outFileFK[j] <- ""
    readFromfileName <- ""
    readFromfileName <- listf[i]
    cat("listf[", i_lf, "]=", listf[i],"\n")
    s <- substr(gsub("\\.","-",readFromfileName),15,21)
    readFromfileName <- gsub(" ", "", readFromfileName, fixed = TRUE)
    print("104")
    cat("readFromFileName>>", readFromfileName,"<<\n")
    dat <- readFile(readFromfileName)
    print("107")
    cat(">>>> Original number of rows for dat:", nrow(dat), "\n")
    dat <- dat [1 : as.integer( nrow(dat) * 0.75), ] ## will trim 25% of the data
    cat(">>>> After reducing the set 25%     :", nrow(dat), "\n")
    
    outFileFK[j] <- paste(FKdir[i_dsName], "FileFN-", j, "-", dsName[i_dsName], s, ".topic_frequency", sep="")
    
    ##outFileFK <- paste("Z:\\Thesis\\plotFreqs\\dba-m\\plots-dba-m\\FileFN-05-", readFromfileName, sep="")
    
    ##outFileFK <- paste("~/thesis/plotFreqs/dba-q/plots-dba-q/FileFN-05-", readFromfileName, sep="")
    
    cat ("READ FILE =", readFromfileName, "\n")
    cat ("OUT FILE x= ", outFileFK[j], "\n")
    print("120")
    cat("outFileFK[", j, "]=", outFileFK[j], "-outListf[", i_dsName, "]=", outListf[i_dsName])
    print("122")
    cat (outFileFK[j], "\n", sep="\t", append = T, file = outListf[i_dsName])
    if (file.exists(outFileFK[j])) file.remove(outFileFK[j]) ## remove the output file if already exists
    Sys.sleep(1)
    cat("K", "F", "\n", sep="\t", append = T, file = outFileFK[j]) # to file
    pg <- calcGroups(dat, outFileFK, j) ## calculate F(x) ## what?????
    print("128")
} ## end function


################################################  main  #############################################
#####################################################################################################

##dsName <- c("salesforce-m-", "dba-m-", "dba-q-", "android-m-") ## to handle the ds names, for now android is empty

##dsName <- c("salesforce-m-", "dba-m-", "dba-q-") ## to handle the ds names
dsName <- c("salesforce-m-") ## to handle the ds names

FKdir <- c()
outListf <- c() ### this is the output directory of the files to be plotted, may not be used
ffDir <- c()
maxX <- 50 ## number of x's to be calculated

for (i_dsName in 1:length(dsName)) {
  cat("Generating FK files for ", dsName[i_dsName], "\n")
  FKdir[i_dsName] <- file.path("~", "Thesis", "Fitting", paste(dsName[i_dsName],"FK", sep=""), "files-FK/")
  
  outListf[i_dsName] <- file.path("~", "Thesis", "Fitting", paste(dsName[i_dsName],"FK", sep=""), "files-frequencies", "list-fs.txt")
  ffDir[i_dsName] <- file.path("~", "Thesis", "Fitting", paste(dsName[i_dsName],"FK", sep=""), "files-frequencies")
  cat("This is the working directory", ffDir[i_dsName], "\n")
  
  setwd(ffDir[i_dsName])
  listf <- list.files(pattern="Posts.xml.csv")
  cat(">>>>>>>>>>>> This is the ListF that will be processed: ", listf, "\n")
  outFileFK <- c()
  
  ## for each topic.freq file
  for(i_lf in 1:length(listf)){ ## for each frequency file 
     for(j in 1:maxX){ ## for each x=1..50
         cat("Iteration number: ", j, "****************************************\n")
         ##cat("These are the parameters for calling processAllx: dsName=", dsName," FKdir=", FKdir, " listf=", listf, " i_dsName=", i_dsName, " i_lf=", i_lf, " j=", j)
         px <- processAllx(dsName, FKdir, ffDir, listf, i_dsName, i_lf, j) ## changed i to i_lf !!!!
     } # for j
  } # for i_lf
} # for i
print("** END OF EXECUTION **")
