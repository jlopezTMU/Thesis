## Install and load packages
library(tm)
library(topicmodels)

source("utils.R") # export2lda_c

###############################################################################
# Setup Begin
###############################################################################
#read data from
#readFrom <- "/Users/miranska/Documents/workspace/se_topics/sample.xml.csv"
readFrom <- "~/Downloads/android.stackexchange.com/Posts.xml.csv"

#save data on LDA-C format to
#saveTo   <- "/Users/miranska/Documents/workspace/se_topics/sample.xml.csv.lda-c" 
saveTo   <- "~/Downloads/android.stackexchange.com/Posts.xml.csv.lda-c" 
#mininmal lenght of a word in the dictionary
minWordLenght <- 4
###############################################################################
# Setup End
###############################################################################

## Load the data
#  Separator is set to \n to force 1 column per row. DataframeSource gets confused otherwise.
Posts <- read.delim(file = readFrom, header = T, sep = "\n")

## Build a corpus
corp <- Corpus(DataframeSource(data.frame(Posts)))

## Transform data
corp = tm_map(corp, tolower) # Convert to lower case
corp = tm_map(corp, stripWhitespace) # Eliminate whitespace char
corp = tm_map(corp, removePunctuation) # Remove punctuation
corp = tm_map(corp, removeNumbers) # Eliminate numbers
corp = tm_map(corp, removeWords, stopwords('english')) # Remove Stopwords
corp = tm_map(corp, stemDocument) # Stemming

## Build a Document-Term Matrix
dtm <- DocumentTermMatrix(corp, control = list(minWordLength = 4))

## Export text corpus to LDA-C format
export2lda_c(dtm, saveTo)
