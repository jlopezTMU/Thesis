## The script reads posts in delimeted format, massages them,
## and saves final text corpus in LDA-C format.
## Usage: Rscript export_to_lda-c.R in_file_name out_file_name

## Install and load packages
library(tm)
library(topicmodels)

source("utils.R") # export2lda_c

args <- commandArgs(trailingOnly = TRUE)

###############################################################################
# Setup Begin
###############################################################################
#read data from
#readFrom <- "/Users/miranska/Documents/workspace/se_topics/sample.xml.csv"
#readFrom <- "~/Downloads/android.stackexchange.com/Posts.xml.csv"
readFrom <- args[1]

#save data on LDA-C format to
#saveTo   <- "/Users/miranska/Documents/workspace/se_topics/sample.xml.csv.lda-c" 
#saveTo   <- "~/Downloads/android.stackexchange.com/Posts.xml.csv.lda-c" 
saveTo    <- args[2]
#mininmal lenght of a word in the dictionary
minWordLenghtToKeep <- 4
###############################################################################
# Setup End
###############################################################################

## Load the data
#  Separator is set to \n to force 1 column per row. DataframeSource gets confused otherwise.
Posts <- read.delim(file = readFrom, header = T, quote = "", sep = "\n")

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

## Build a Document-Term Matrix
dtm <- DocumentTermMatrix(corp, control = list(minWordLength = minWordLenghtToKeep))
cat ("Created dtm for", length(unique(dtm$i)), "documents with", length(dtm$i), "elements\n")

## Export text corpus to LDA-C format
export2lda_c(dtm, saveTo)
cat("Export to", saveTo, "complete\n")
cat("Done\n")