library(tm)

source("utils.R")

args <- commandArgs(trailingOnly = TRUE)
readFrom <- args[1]
saveTo <-  args[2]

corp <- createCorp(readFrom, 2012, 2)

## Build a Document-Term Matrix
dtm <- DocumentTermMatrix(corp, control = list(minWordLength = 2)) #keep words of lenght 2 or longer
cat("Before tf-idf: term count =", ncol(dtm), ", doc count =", nrow(dtm), "\n")
dtm <- removeFrequentWords(dtm) #removing based on median tf-idf value
cat("After tf-idf: term count =", ncol(dtm), ", doc count =", nrow(dtm), "\n")


dtm.data_frame <- as.data.frame(as.matrix(dtm))

save(dtm.data_frame, file = saveTo)


