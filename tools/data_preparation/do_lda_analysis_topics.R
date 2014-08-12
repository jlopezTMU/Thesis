## The script reads posts in delimited format, and generates 
## distribution of LDA topics for a given month year,
## saving the output in in_file_name + ".year-month.lda-c"
## Usage: Rscript export_to_lda-c.R in_file_name year month

library(tm)
library(foreach)
library(doParallel)

source("utils.R")

args <- commandArgs(trailingOnly = TRUE)
readFrom <- args[1]
year  <-  as.integer(args[2]) 
month <-  as.integer(args[3])

saveTo <- paste(readFrom, ".", year, "-", month, ".topic_frequency", sep ="")

corp <- createCorp(readFrom, year, month)
fileSuffix <- ".topic_frequency"

## Build a Document-Term Matrix
dtm <- DocumentTermMatrix(corp, control = list(minWordLength = 2)) #keep words of lenght 2 or longer
cat("Before tf-idf: term count =", ncol(dtm), ", doc count =", nrow(dtm), "\n")
dtm <- removeFrequentWords(dtm) #removing based on median tf-idf value
cat("After tf-idf: term count =", ncol(dtm), ", doc count =", nrow(dtm), "\n")

#setup parallel backend to use 8 processors
cl<-makeCluster(8)
registerDoParallel(cl)

cat("topicCount\tmdl.alpha\tmdl.beta.mean\tmdl.beta.sd\ttime (sec)\ttopic.frequency\n", sep="\t", append = T
    , file = saveTo) # to file

foreach(topicCount = 2:nrow(dtm) #max = 1 topic per document
        , .packages='topicmodels' #include package
) %dopar% { #change to %do% for sequential execution
  startRun <- Sys.time()
  
  val <- getTopicsFrequency(dtm, topicCount)
  
  prefix <- paste(topicCount, val$mdl.alpha, val$mdl.beta.mean, val$mdl.beta.sd, difftime(Sys.time(), startRun, units = "secs"), sep="\t")
  
  cat(topicCount, "\n") #to screen (no screen output is parallel mode)
  
  for (i in 1:length(val$topic.frequency)){
    cat(prefix, val$topic.frequency[i]
        , "\n", sep="\t", append = T
        , file = saveTo) # to file
  }
      
}
cat("Saved data to", saveTo, "\n")
cat("Done\n")

