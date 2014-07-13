## Do basic LDA fit testing: using k-fold validation
## Iterate over topic count (k) from 2 to max number of docs 
## and output average perplexity for each k

## Install and load packages
library(tm)
library(topicmodels)
library(foreach)
library(doParallel)

source("utils.R")

args <- commandArgs(trailingOnly = TRUE)
readFrom <- args[1]

corp <- createCorp(readFrom, 2010, 9)

foldCount <- 10 #number of fold in k-fold validation

## Build a Document-Term Matrix
dtm <- DocumentTermMatrix(corp, control = list(minWordLength = 2)) #keep words of lenght 2 or longer
cat("Before tf-idf: term count =", ncol(dtm), ", doc count =", nrow(dtm), "\n")
dtm <- removeFrequentWords(dtm) #removing based on median tf-idf value
cat("After tf-idf: term count =", ncol(dtm), ", doc count =", nrow(dtm), "\n")

# uncomment if you need to export a dataset to lda-c
#export2lda_c(train, "Posts.xml.w_ts.csv.2012-02.lda-c")

#setup parallel backend to use 8 processors
cl<-makeCluster(8)
registerDoParallel(cl)

cat("topicCount\tperp.mean\tperp.sd\tperp.se\ttime (sec)\n", sep="\t", append = T
    , file = paste(readFrom, ".perplexity", sep="")) # to file

foreach(topicCount = 2:nrow(dtm) #max = 1 topic per document
        , .packages='topicmodels' #include package
) %dopar% { #change to %do% for sequential execution
  startRun <- Sys.time()

  val <- doKfoldValidation(foldCount, dtm, topicCount)

  cat(topicCount, val$perp, "\n", sep="\t") #to screen (no screen output is parallel mode)
  cat(topicCount, perp, mdl.alpha, mdl.beta.mean, mdl.beta.sd, mdl.beta.se
      , difftime(Sys.time(), startRun, units = "secs")
      , "\n", sep="\t", append = T
      , file = paste(readFrom, ".perplexity", sep="")) # to file
}
