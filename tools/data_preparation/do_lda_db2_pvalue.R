## The script reads posts in delimited format, and generates a list of keyword
## Usage: Rscript my_Script_name.R out_directory/ in_file_name yyyy
## This programs includes the probability per keyword 

library(tm)
library(foreach)
library(doParallel)

source("utilsdb2.R")

args <- commandArgs(trailingOnly = TRUE)
outDir <- args[1]
readFrom <- paste(args[1], args[2],sep="")
year  <-  as.integer(args[3])

##month <-  as.integer(args[3])

topKeywordCount <- 20 ## SET TO 20 FOR THIS RUN

## readFrom <- "Posts.xml.csv"  

## corp <- createCorp(readFrom)
corp <- createCorp(readFrom, year) ## added year

# To access original id of a document in VCorp run corp[[ index_of_document_1_to_N ]]$meta$id
# To access original id of a document in DocumentTermMatrix run dtm$dimnames$Docs[ index_of_document_1_to_N ]

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
cl<-makeCluster(8) ## change to 8 for multiprocessing
registerDoParallel(cl)

## original foreach(topicCount = 2:nrow(dtm) #max = 1 topic per document
##foreach(topicCount = c(2, 3) #max = 1 topic per document
foreach(topicCount = c(5) #max = 1 topic per document ## changed topic count=5
        , .packages='topicmodels' #include package
) %do%
{ #change to %dopar% for parallel execution
  cat("*RUNNING SINGLE PROCESSING*\n")
  cat("TopicCount:", topicCount, "\n") #to screen (no screen output is parallel mode)  
  mdl <- LDA(dtm, topicCount) #LDA model 
  topic.keyword <- terms(mdl, topKeywordCount)
  mdl.post <- posterior(mdl) #get posterior data
} ##end foreach
############################ code added ####################################################################
  for (i in 1:topKeywordCount){
     for (j in 1:topicCount) {
          topic.keyword[i,j] <- paste(topic.keyword[i,j], mdl.post$terms[[j, topic.keyword[i,j]]])
          cat("This is the P-Value for  term:", i, ":", j, ">", topic.keyword[i,j], "\n")
     }
  }

  cat("DEBUG: year is", year, "\n")
  write.csv(topic.keyword, file = paste(outDir,"TEST_lda_keywords_", args[2],"_",year, "-", topicCount, ".csv", sep = "") )
##}

cat("Done\n")
