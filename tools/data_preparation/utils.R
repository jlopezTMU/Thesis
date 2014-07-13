## The function takes term document matrix (sparse representation), tdm, and exports it to file
## fileName in the following format "distinctWordCount wordId:WordCount ...", one document per 
## line. For example, "2 15:7 21:4" means that a document contains two distinct words 
## with ids 15 and 21, appearing 7 and 4 times, respectively. 
## This format is used by LDA and HDP-LDA code provided by Blei's team.
##
## The function saves dictionary of words, one word per line to file fileName, suffixed by ".dic".
## Id of a given word is given by word's line number in the file fileName.dic;
## line number count starts from 1.
##
## The function saves document "names", one name per line to file fileName, suffixed by ".doc".
## Id of a given document is equivalent to the document's line number in the file fileName.doc;
## line number count starts from 1.
##
## Usage example: export2lda_c(myTermDocMatrix, "~/foo.txt")
## Note that the existing file will be overwritten
export2lda_c <- function(tdm, fileName){
  # We assume that
  # j - term (a.k.a. word) ids
  # i - doc ids
  # v - word frequency
  
  fid <- file(fileName, "w") # overwrite existing file
  docIds <- unique(tdm$i)
  for(docId in docIds){
    # I am not certain if $j is guaranteed to be consecutive; 
    # hence the inefficient search using which
    wordIndexes <- which( tdm$i == docId)
    cat(length(wordIndexes), file = fid, sep = "", append = T)
    for(wordIndex in  wordIndexes){
      cat(" ", tdm$j[wordIndex], ":", tdm$v[wordIndex], file = fid, sep = "", append = T)
    }
    cat("\n", file = fid, sep = "", append = T)
  }
  close(fid)

  # save dictionary of words
  write(tdm$dimnames$Terms, file = paste(fileName, ".dic", sep=""), sep = "\n")
  
  # save original document names
  write(tdm$dimnames$Docs, file = paste(fileName, ".doc", sep=""), sep = "\n")
}

## This function reads the data from csv file, cleans it and returns tm text Corpus
## readFromfileName - name of the file to read from
## Monthly Filter (optional):
##   year  - optional: year when docs were created, format = YYYY
##   month - optional: month when docs were created
createCorp <- function(readFromfileName, year, month){
  library(tm)  
  ## Load the data
  Posts <- read.delim(file = readFromfileName, header = T, quote = "", sep = "\t")
  
  cat("Read", nrow(Posts), "rows from", readFromfileName, "\n")
  
  if(! missing(year) && ! missing(month)){
    Posts$create_ts <- as.POSIXlt(Posts$create_ts)
    Posts <- subset(Posts, Posts$create_ts$mon == (month - 1) 
           & Posts$create_ts$year == (year - 1900))
    cat("Kept", nrow(Posts), "rows\n")
  }
  
  # Concatenate columns, otherwise DataframeSource gets confused
  Posts <- data.frame(paste(Posts$title, Posts$body, sep =" "))
  
  ## Build a corpus
  corp <- Corpus(DataframeSource(Posts))
  
  ## Transform data
  corp = tm_map(corp, removeWords, stopwords('english')) # Remove Stopwords
  corp = tm_map(corp, stemDocument) # Stemming
  corp = tm_map(corp, stripWhitespace) # Eliminate whitespace char
  
  cat ("Created corpus from", length(corp), "documents\n")
  return (corp)
}

## Removes topics from the Document Term Matrix using tf-idf approach
## dtm - Document Term Matrix to process
## threshold - remove words with tf-idf values smaller than threshold
##    this parameter is optional: if threshold is not defined, we will remove 
##    most frequent words, based on the median value of tf-idf (at least 50%)
##    Setting threshold = 0 will eliminate words appearing in every document
removeFrequentWords <- function(dtm, threshold){
  library("slam")
  termTfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * 
    log2(nDocs(dtm)/col_sums(dtm > 0))
  
  # if no theshold is provided then set the thrshold value to median of tf-idf
  # distribution, removing at least 50% of the words
  if( missing(threshold) ) { 
    threshold <- median(termTfidf)
  } 
  
  # remove terms which have tf-idf smaller than the median of all the tf-idf values
  # this will shrink the dictionay by approximately 50%
  dtm <- dtm[, termTfidf > threshold]
  dtm <- dtm[row_sums(dtm) > 0,]  #remove docs that have no terms remaining (unlikely event)
  return(dtm)
}

## This function performs k-fold validation of an LDA model
## k - The number of validation folds
## dat - Document Term Matrix to feed to the LDA model
## topicCount - Number of topics in the LDA model
doKfoldValidation <- function(k, dat, topicCount){
  library(cvTools)
  library(topicmodels)
  
  set.seed(123) #set seed for reproducibility
  folds <- cvFolds(nrow(dat), K=k)
  dat$holdoutpred <- rep(0,nrow(dat))
  dat$perplexity_per_fold <- rep(NA,k)
  
  for(i in 1:k){
    train <- dat[folds$subsets[folds$which != i], ] #Set the training set
    validation <- dat[folds$subsets[folds$which == i], ] #Set the validation set
    
    train.lda <- LDA(train, topicCount) #train the model
    
    # uncommnet if doc - topics relation for the training set is required
    # train.topics <- topics(train.lda)
    
    # get stats for validation docs
    test.topics <- posterior(train.lda, validation) #get proibabilities per doc and topic
    test.topics <- apply(test.topics$topics, 1, which.max) 
    dat$holdoutpred[folds$subsets[folds$which == i]] <- test.topics #Put the hold out prediction in the data set for later use
  
    # uncomment if model paramaeters are needed
    #mdl.alpha <- train.lda@alpha
    #mdl.beta.mean <- mean(train.lda@beta)
    #mdl.beta.sd <- sd(train.lda@beta)
    
    # get perplexity of the lda, based on validation set
    dat$perplexity_per_fold[i] <- perplexity(train.lda, validation)
  }
  return ( list(
      perplexity.mean = mean(dat$perplexity_per_fold)
    , perplexity.sd = sd(dat$perplexity_per_fold)
    , perplexity.se = sd(dat$perplexity_per_fold) 
        / sqrt(length(dat$perplexity_per_fold))
  ))
}
