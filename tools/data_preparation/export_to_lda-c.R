## The script reads posts in delimited format, massages them,
## and saves final text corpus in LDA-C format, 
## appending ".year-month.lda-c" to in_file_name
## Usage: Rscript export_to_lda-c.R in_file_name year month

library(tm)

source("utils.R")

args <- commandArgs(trailingOnly = TRUE)
readFrom <- args[1]
year  <-  as.integer(args[2])
month <-  as.integer(args[3])

saveTo <-  paste(readFrom, ".", year, "-", month, ".lda-c", sep ="")

## create text corpus
corp <- createCorp(readFrom, year, month)
## Build a Document-Term Matrix
dtm <- DocumentTermMatrix(corp, control = list(minWordLength = 2)) #keep words of length 2 or longer
cat("Before tf-idf: term count =", ncol(dtm), ", doc count =", nrow(dtm), "\n")
dtm <- removeFrequentWords(dtm) #removing based on median tf-idf value
cat("After tf-idf: term count =", ncol(dtm), ", doc count =", nrow(dtm), "\n")


## Export text corpus to LDA-C format
export2lda_c(dtm, saveTo)
cat("Export to", saveTo, "complete\n")
cat("Done\n")
