## The function takes term document matrix (sparse representation) and exports it
## in the following format "distinctWordCount wordId:WordCount ...", one document per line.
## For example, "2 15:7 21:4" means that a document contains two distinct words 
## with ids 15 and 21, appearing 7 and 4 times, respectively. 
## This format is used by LDA and HDP-LDA code provided by Blei's team.
##
## Usage example: export2lda_c(myTermDocMatrix, "~/foo.txt")
## Note that the existing file will be overwritten
export2lda_c <- function(tdm, fileName){
  # We assume that
  # i - term (a.k.a. word) ids
  # j - doc ids
  # v - word frequency
  
  file.create(fileName) # overwrite existing file
  docIds <- unique(tdm$j)
  for(docId in docIds){
    # I am not certain if $j is guaranteed to be consecutive; 
    # hence the inefficient search using which
    wordIndexes <- which( tdm$j == docId)
    cat(length(wordIndexes), file = fileName, sep = "", append = T)
    for(wordIndex in  wordIndexes){
      cat(" ", tdm$i[wordIndex], ":", tdm$v[wordIndex], file = fileName, sep = "", append = T)
    }
    cat("\n", file = fileName, sep = "", append = T)
  }
}
