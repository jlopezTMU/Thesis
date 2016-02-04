## The script reads posts in delimited format, and generates a list of keyword
## Usage: Rscript my_Script_name.R out_directory/ in_file_name yyyy


library(tm)
library(foreach)
library(doParallel)

source("utils.R")

args <- commandArgs(trailingOnly = TRUE)
outDir <- args[1]
readFrom <- paste(args[1], args[2],sep="")
year  <-  as.integer(args[3])

##month <-  as.integer(args[3])

topKeywordCount <- 20 ## SET TO 20 FOR THIS RUN

## readFrom <- "Posts.xml.csv"  

## corp <- createCorp(readFrom)
corp <- createCorp(readFrom, year) ## added year
cat("DEBUG after calling createCorp!\n")
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
##foreach(topicCount = c(2, 3) #max = 1 topic per document
foreach(topicCount = c(5) #max = 1 topic per document ## changed topic count=5
        , .packages='topicmodels' #include package
) %do%
{ #change to %dopar% for parallel execution
  cat("TopicCount:", topicCount, "\n") #to screen (no screen output is parallel mode)  
  mdl <- LDA(dtm, topicCount) #LDA model 
  topic.keyword <- terms(mdl, topKeywordCount)
  cat("DEBUG: year is", year, "\n")
  write.csv(topic.keyword, file = paste(outDir,"lda_keywords__topic_count_", year, "-", topicCount, ".csv", sep = "") )
}
cat("Done\n")
