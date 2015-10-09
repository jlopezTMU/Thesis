## The script reads posts in delimited format, and generates a list of keyword
## Usage: Rscript my_Script_name.R in_file_name year month
##

library(tm)
library(foreach)
library(doParallel)

source("utils.R")

##args <- commandArgs(trailingOnly = TRUE)
##readFrom <- args[1]
##year  <-  as.integer(args[2]) 
##month <-  as.integer(args[3])

topKeywordCount <- 20

readFrom <- "../../data/sample_posts/Posts.xml.csv" ## JL Hard coded input parms. for running in non-linux environment
#year  <-  2014              ## JL 
#month <-  12                ## JL

corp <- createCorp(readFrom)

## Build a Document-Term Matrix
dtm <- DocumentTermMatrix(corp, control = list(minWordLength = 2)) #keep words of lenght 2 or longer
cat("Before tf-idf: term count =", ncol(dtm), ", doc count =", nrow(dtm), "\n")
#dtm <- removeFrequentWords(dtm) #removing based on median tf-idf value
cat("After tf-idf: term count =", ncol(dtm), ", doc count =", nrow(dtm), "\n")
## the following line commented out to avoid sparse error
#dtm <- removeSparseTerms(dtm, 1 - (1.1/nrow(dtm)) )  #remove terms appearing only in 1 document
#dtm <- dtm[row_sums(dtm) > 0,]  #remove docs that have no terms remaining (unlikely event)
cat("After removing terms appearing only in 1 document: term count =", ncol(dtm), ", doc count =", nrow(dtm), "\n")

#setup parallel backend to use 8 processors
cl<-makeCluster(8)
registerDoParallel(cl)

## original foreach(topicCount = 2:nrow(dtm) #max = 1 topic per document
foreach(topicCount = c(2, 3) #max = 1 topic per document
        , .packages='topicmodels' #include package
 ) %dopar% 
{ #change to %dopar% for parallel execution
  cat("TopicCount:", topicCount, "\n") #to screen (no screen output is parallel mode)  
  mdl <- LDA(dtm, topicCount) #LDA model / set topicCounthelp to 5 because this var is not found!
  topic.keyword <- terms(mdl, topKeywordCount)
  write.csv(topic.keyword, file = paste("lda_keywords__topic_count_", topicCount, ".csv", sep = "") )
}
cat("Done\n")

