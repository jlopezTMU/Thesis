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
createCorp <- function(readFromfileName){
  library(tm)
  
  ###############################################################################
  # Setup Begin
  ###############################################################################
  #mininmal lenght of a word in the dictionary
  minWordLenghtToKeep <- 4
  ###############################################################################
  # Setup End
  ###############################################################################
  
  ## Load the data
  #  Separator is set to \n to force 1 column per row. DataframeSource gets confused otherwise.
  Posts <- read.delim(file = readFromfileName, header = T, quote = "", sep = "\n")
  
  cat("Read", nrow(Posts), "rows from", readFrom, "\n")
  ## Build a corpus
  corp <- Corpus(DataframeSource(data.frame(Posts)))
  
  ## Transform data
  ## TODO: it seems that tm_map parallizes execution via duplication of R instances, 
  ##   consuming lots of memory. If needed, we can use the functions below (e.g., stripWhitespace)
  ##   directly.
  corp = tm_map(corp, stripWhitespace) # Eliminate whitespace char
  corp = tm_map(corp, removePunctuation) # Remove punctuation
  corp = tm_map(corp, removeNumbers) # Eliminate numbers
  corp = tm_map(corp, removeWords, stopwords('english')) # Remove Stopwords
  corp = tm_map(corp, stemDocument) # Stemming

  cat ("Created corpus from", length(corp), "documents\n")
  return (corp)
}
