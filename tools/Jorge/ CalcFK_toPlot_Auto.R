## This program reads the frequencies files produced by the do_lda_analysis 
## extracts top topics and calculates their corresponding F for X=5, X=20 and X=50
## forms an input file to plot those values
##
##
##
## Author. Jorge Lopez May 2016

library(plyr) ## to use arrange f


readFile <- function(fName) {
###########################
dat <- read.delim(file = readFromfileName, header = T, fill=T, sep = "\t",
                   row.names=NULL)
colnames(dat)<-c(colnames(dat)[-1],"x")
dat$x<-NULL

dat <- arrange(dat, strtoi(topicCount)) ##sorting the vector to prevent df not sorted well

return(dat)
}

calcGroups <- function(dat, X) {
###########################
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
               cat(k, F, "\n", sep="\t", append = T, file = outFileFN) # to file
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
              cat(k, F, "\n", sep="\t", append = T, file = outFileFN) # to file
           #$#cat("**k:", k, " and F:", F, "\n")
         }

} ##for

return(1)

} ## function

################################################  main  #############################################
###dsName <- "-salesforce-" ## unique for each ds
dsName <- "-dba-" ## unique for each ds
###dsName <- "-salesq-" ## unique for each ds
###FNdir <- "~/Thesis/Fitting/salesforce-m-FK/files-FN/" ## This path needs to be customized
FNdir <- "~/Thesis/Fitting/dba-m-FK/files-FN/" ## This path needs to be customized
###FNdir <- "~/Thesis/Fitting/dba-q-FK/files-FN/" ## This path needs to be customized
cat("Generating FN files for ", dsName, "\n")
### setwd("~/Thesis/Fitting/salesforce-m-FK/files-frequencies") ## This path needs to be customized
###setwd("~/Thesis/Fitting/dba-q-FK/files-frequencies") ## This path needs to be customized
###setwd("~/Thesis/Fitting/dba-q-FK/files-frequencies") ## This path needs to be customized
setwd("~/Thesis/Fitting/dba-m-FK/files-frequencies") ## This path needs to be customized
###outListf<-"~/Thesis/Fitting/dba-q-FK/files-frequencies/list-fs.txt"
outListf<-"~/Thesis/Fitting/dba-m-FK/files-frequencies/list-fs.txt"
####################################################################################################

listf <- list.files(pattern="Posts.xml.csv")
cat("This is the ListF will be processed: ", listf, "\n")
for (i in 1:length(listf)){
     outFileFN <- ""
     readFromfileName <- ""
     readFromfileName <- listf[i]
     s <- substr(gsub("\\.","-",readFromfileName),15,21)
     ##s <- substr(gsub("\\.","-",readFromfileName),1,21)
     ##setwd("~/Thesis/Fitting/salesforce-m-FK/files-FN") ## This path needs to be customized
     outFileFN <- paste(FNdir, "FileFN-05", dsName, s, ".topic_frequency", sep="")
     ##outFileFN <- paste("Z:\\Thesis\\plotFreqs\\dba-m\\plots-dba-m\\FileFN-05-", readFromfileName, sep="")
     ##outFileFN <- paste("~/thesis/plotFreqs/dba-q/plots-dba-q/FileFN-05-", readFromfileName, sep="")
     cat ("READ FILE =", readFromfileName, "\n")
     cat ("OUT FILE 05= ", outFileFN, "\n")
     cat (outFileFN, "\n", sep="\t", append = T, file = outListf)
     if (file.exists(outFileFN)) file.remove(outFileFN) ## remove the output file if already exists
     Sys.sleep(3)
     cat("K", "F", "\n", sep="\t", append = T, file = outFileFN) # to file
     dat <- readFile(readFromfileName)
     pg <- calcGroups(dat, 5)

     outFileFN <- ""
     outFileFN <- paste(FNdir, "FileFN-20", dsName, s, ".topic_frequency", sep="")
     ##outFileFN <- paste("Z:\\Thesis\\plotFreqs\\dba-m\\plots-dba-m\\FileFN-20-", readFromfileName, sep="")
     ##outFileFN <- paste("~/thesis/plotFreqs/dba-q/plots-dba-q/FileFN-20-", readFromfileName, sep="")
     cat ("OUT FILE 20= ", outFileFN, "\n")
     cat (outFileFN, "\n", sep="\t", append = T, file = outListf)
     if (file.exists(outFileFN)) file.remove(outFileFN) ## remove the output file if already exists
     Sys.sleep(3)
     cat("K", "F", "\n", sep="\t", append = T, file = outFileFN) # to file
     pg <- calcGroups(dat, 20)

     outFileFN <- ""
     outFileFN <- paste(FNdir, "FileFN-50", dsName, s, ".topic_frequency", sep="")
     ##outFileFN <- paste("Z:\\Thesis\\plotFreqs\\dba-m\\plots-dba-m\\FileFN-50-", readFromfileName, sep="")
     ##outFileFN <- paste("~/thesis/plotFreqs/dba-q/plots-dba-q/FileFN-50-", readFromfileName, sep="")
     cat (outFileFN, "\n", sep="\t", append = T, file = outListf)
     cat ("OUT FILE 50= ", outFileFN, "\n")
     if (file.exists(outFileFN)) file.remove(outFileFN) ## remove the output file if already exists
     Sys.sleep(3)
     cat("K", "F", "\n", sep="\t", append = T, file = outFileFN) # to file
     pg <- calcGroups(dat, 50)
}
