## The script reads posts in delimited format, and generates 
## NA: distribution of LDA topics for a given month year,
## NA: saving the output in in_file_name + ".year-month.topic_frequency"
## Usage: Rscript export_to_lda-c.R in_file_name year month
##
## This version adds a Semaphore in order to slowdown the process of writing in a parallel processing
## Version for running in PC
##
## This version is for extracting the topic keywords not the frequencies
## This is being debugged currently
## 10/8/15 11:34

library(tm)
library(foreach)
library(doParallel)

source("utils_IBM.R")

##args <- commandArgs(trailingOnly = TRUE)
##readFrom <- args[1]
##year  <-  as.integer(args[2]) 
##month <-  as.integer(args[3])

semaphoreFileName <- paste("semaf_", Sys.getpid(), sep='') # append R's process id to semaphore to avoid conflict
if( file.exists(semaphoreFileName) ){ file.remove(semaphoreFileName) } 

 readFrom <- "posts.xml.csv" ## JL Hard coded input parms. for running in non-linux environment
 year  <-  2014              ## JL 
 month <-  12                ## JL

## saveTo <- paste(readFrom, ".", year, "-", month, ".topic_frequency", sep ="")

corp <- createCorp(readFrom, year, month)

## Build a Document-Term Matrix
dtm <- DocumentTermMatrix(corp, control = list(minWordLength = 2)) #keep words of lenght 2 or longer
cat("Before tf-idf: term count =", ncol(dtm), ", doc count =", nrow(dtm), "\n")
## Dr. M. 10/2/15 dtm <- removeFrequentWords(dtm) #removing based on median tf-idf value
cat("After tf-idf: term count =", ncol(dtm), ", doc count =", nrow(dtm), "\n")
## the following line commented out to avoid sparse error
## as per Dr dtm <- removeSparseTerms(dtm, 1 - (1.1/nrow(dtm)) )  #remove terms appearing only in 1 document
## to avoid error dtm <- dtm[row_sums(dtm) > 0,]  #remove docs that have no terms remaining (unlikely event)
cat("After removing terms appearing only in 1 document: term count =", ncol(dtm), ", doc count =", nrow(dtm), "\n")

#setup parallel backend to use 8 processors
cl<-makeCluster(8)
registerDoParallel(cl)

# Header
## NA cat("topicCount\tmdl.alpha\tmdl.beta.mean\tmdl.beta.sd\ttime (sec)\ttopic.frequency\n", sep="\t", append = T
## NA  , file = saveTo) # to file

## original foreach(topicCount = 2:nrow(dtm) #max = 1 topic per document
foreach(topicCount = 1:5 #max = 1 topic per document
        , .packages='topicmodels' #include package
) %do% 
{ #change to %dopar% for parallel execution
  startRun <- Sys.time()
  
  mdl <- LDA(dtm, 5) #LDA model / set topicCounthelp to 5 because this var is not found!
  
  topic.keyword <- terms(mdl[["VEM"]],5) ## topicCount set to 5
  
  ##val <- getTopicsKeyword(dtm, topicCount) ## JL changed to getTopicKeyword
  
  ## NA prefix <- paste(topicCount, val$mdl.alpha, val$mdl.beta.mean, val$mdl.beta.sd, difftime(Sys.time(), startRun, units = "secs"), sep="\t")
  
  cat("TopicCount:", topicCount, "\n") #to screen (no screen output is parallel mode)
  ## while Semaphore is true, sleep for 1 sec. 
  ## create Semaphore
  while (file.exists(semaphoreFileName)==TRUE) 
        {Sys.sleep(1); cat("in while sleeping 1 sec\n");}
  file.create(semaphoreFileName)
  
  topic.keyword ## for now just display the output
  
  ## hold output to a file cat(topic.keyword, "\n", sep="\t", append = T, file = saveTo) ## to file 
  
  ##for (i in 1:length(val$topic.frequency)){
  ##   cat(prefix, val$topic.frequency[i]
  ##      , "\n", sep="\t", append = T
  ##      , file = saveTo) # to file
  ##}
  file.remove(semaphoreFileName)
  cat("Semaphore is",file.exists(semaphoreFileName), "\n")
}
## cat("Saved data to", saveTo, "\n")
cat("Done\n")

