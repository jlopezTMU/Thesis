## Do basic LDA fit testing: split the dataset into testing and training (70/30)
## Iterate over topic count (k) from 2 to 1000 and output perplexity for each k

## Install and load packages
library(tm)
library(topicmodels)
library(foreach)
library(doParallel)

source("utils.R")

args <- commandArgs(trailingOnly = TRUE)
readFrom <- args[1]

corp <- createCorp(readFrom)

# split the dataset
# ~70% training / ~30% validation
smpSize <- floor(0.70 * length(corp))

## set the seed to make your partition reproductible
set.seed(123)
trainInd <- sample(seq_len(length(corp)), size = smpSize)

## Build a Document-Term Matrix
train <- DocumentTermMatrix(corp[trainInd])
test <- DocumentTermMatrix(corp[-trainInd])

#setup parallel backend to use 8 processors
cl<-makeCluster(8)
registerDoParallel(cl)

foreach(topicCount = 2:1000
        , .packages='topicmodels' #include package
) %dopar% { #change to %do% for sequential execution
  lda <- LDA(train, topicCount)
  perp <- perplexity(lda, test)
  cat(topicCount, perp, "\n", sep="\t") #to screen (no screen output is parallel mode)
  cat(topicCount, perp, "\n", sep="\t", append = T
      , file = paste(readFrom, ".perplexity", sep="")) # to file
}
