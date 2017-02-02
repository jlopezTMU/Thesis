## This program reads the frequencies files produced by the do_lda_analysis 
## extracts top topics and calculates their corresponding F for X=5, X=20 and X=50
## forms an input file to plot those values
##
##
##
## Author. Jorge Lopez May 2016
##                     Nov 2016 added more X=2,15,30,40
##                     Feb 2017 constrain k<=200

CUT <- 200
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
             if(strtoi(k) <= CUT) {
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
             
               cat(k, F, "\n", sep="\t", append = T, file = outFileFN)
               cat(">>>>>>>>>>>>>>>>>>> K, F are", k, F, "\n")
             }
           } ## else
       
  } else { 
           #$#cat ("EOF reached \n") 
           k<-dat$topicCount[i]
           if(strtoi(k) <= CUT) {
              subCountVec <- sort(subCountVec, decreasing=T)
            
              ## calculate F
              F <- sum(subCountVec[1:X])/sum(subCountVec)
              cat(k, F, "\n", sep="\t", append = T, file = outFileFN)
              #$#cat("**k:", k, " and F:", F, "\n")
           }
         }
  
} ##for

return(1)
  
} ## function

################################################  main  #############################################
outListf<-"list-fs.txt"
listf <- list.files(pattern="Posts.xml.csv.201") ## Process only 2015!!! NEED CUSTOMIZING!!!
for (i in 1:length(listf)){
     outFileFN <- ""
     readFromfileName <- ""
     readFromfileName <- listf[i]
     s <- substr(gsub("\\.","-",readFromfileName),1,21)
     outFileFN <- paste("FileFN-02-", s, ".topic_frequency", sep="")
     ##outFileFN <- paste("Z:\\Thesis\\plotFreqs\\dba-m\\plots-dba-m\\FileFN-05-", readFromfileName, sep="")
     ##outFileFN <- paste("~/thesis/plotFreqs/dba-q/plots-dba-q/FileFN-05-", readFromfileName, sep="")
     cat ("READ FILE =", readFromfileName, "\n")
     cat ("OUT FILE 02= ", outFileFN, "\n")
     cat (outFileFN, "\n", sep="\t", append = T, file = outListf)
     if (file.exists(outFileFN)) file.remove(outFileFN) ## remove the output file if already exists
     Sys.sleep(3)
     cat("K", "F", "\n", sep="\t", append = T, file = outFileFN) # to file
     dat <- readFile(readFromfileName)
     pg <- calcGroups(dat, 2) ## X=2
     
     outFileFN <- ""
     outFileFN <- paste("FileFN-15-", s, ".topic_frequency", sep="")
     ##outFileFN <- paste("Z:\\Thesis\\plotFreqs\\dba-m\\plots-dba-m\\FileFN-20-", readFromfileName, sep="")
     ##outFileFN <- paste("~/thesis/plotFreqs/dba-q/plots-dba-q/FileFN-20-", readFromfileName, sep="")
     cat ("OUT FILE 15= ", outFileFN, "\n")
     cat (outFileFN, "\n", sep="\t", append = T, file = outListf)
     if (file.exists(outFileFN)) file.remove(outFileFN) ## remove the output file if already exists
     Sys.sleep(3)
     cat("K", "F", "\n", sep="\t", append = T, file = outFileFN) # to file
     pg <- calcGroups(dat, 15) ## X=15
     
     outFileFN <- ""
     outFileFN <- paste("FileFN-30-", s, ".topic_frequency", sep="")
     ##outFileFN <- paste("Z:\\Thesis\\plotFreqs\\dba-m\\plots-dba-m\\FileFN-50-", readFromfileName, sep="")
     ##outFileFN <- paste("~/thesis/plotFreqs/dba-q/plots-dba-q/FileFN-50-", readFromfileName, sep="")
     cat (outFileFN, "\n", sep="\t", append = T, file = outListf)
     cat ("OUT FILE 30= ", outFileFN, "\n")
     if (file.exists(outFileFN)) file.remove(outFileFN) ## remove the output file if already exists
     Sys.sleep(3)
     cat("K", "F", "\n", sep="\t", append = T, file = outFileFN) # to file
     pg <- calcGroups(dat, 30) ## X=30
     
     outFileFN <- ""
     outFileFN <- paste("FileFN-40-", s, ".topic_frequency", sep="")
     ##outFileFN <- paste("Z:\\Thesis\\plotFreqs\\dba-m\\plots-dba-m\\FileFN-50-", readFromfileName, sep="")
     ##outFileFN <- paste("~/thesis/plotFreqs/dba-q/plots-dba-q/FileFN-50-", readFromfileName, sep="")
     cat (outFileFN, "\n", sep="\t", append = T, file = outListf)
     cat ("OUT FILE 40= ", outFileFN, "\n")
     if (file.exists(outFileFN)) file.remove(outFileFN) ## remove the output file if already exists
     Sys.sleep(3)
     cat("K", "F", "\n", sep="\t", append = T, file = outFileFN) # to file
     pg <- calcGroups(dat, 40) ## X=40
     
}
