## Author. Jorge Lopez June 2016
## The script reads posts in delimited format, and generates a list of keyword
## Usage: Rscript my_Script_name.R out_directory/ in_file_name yyyy
## This program includes the probability per keyword 
##        also includes the specificity by keyword by topic  
## Variation: This program was adapted to read the file provided by IBM that contains questions and answers only,
## does not contain any date or tags, the LDA model is applied to this corpus and gets five topics and 20 keywords
## for each topic, the memory directives are for PC pnly, if the corpus is very large it can run in
## memory problems
## This version process a devWorks file provided by IBM from an specified directory
## usage: Rscript do_analysis_db2_idf_devWorks <i/o_directory></> <inputfile_name>
## generates one file per topic per file for later be used for plotting


library(tm)
library(foreach)
library(doParallel)
library(stringr) ## added to handle string functions

memory.limit(24356)
memory.size(max = TRUE)

source("utilsdb2_devWorks.R")
xURL <- "http://dba.stackexchange.com/questions/"
###xSystime <- format(Sys.time(), "%a-%b-%d_%H-%M-%S_%Y") ## DONT NEED HMS
xSystime <- format(Sys.time(), "%b_%d_%Y")

################################ following lines commented out to run in PC ############################
args <- commandArgs(trailingOnly = TRUE)
outDir <- args[1]
readFrom <- paste(args[1], args[2],sep="")
########################################################################################################

topKeywordCount <- 30 ## SET TO 30 FOR THIS RUN

corp <- createCorp(readFrom)

# To access original id of a document in VCorp run corp[[ index_of_document_1_to_N ]]$meta$id
# To access original id of a document in DocumentTermMatrix run dtm$dimnames$Docs[ index_of_document_1_to_N ]

cat("DEBUG after calling createCorp!\n")
## Build a Document-Term Matrix
dtm <- DocumentTermMatrix(corp, control = list(minWordLength = 2)) #keep words of lenght 2 or longer
cat("Before tf-idf: term count =", ncol(dtm), ", doc count =", nrow(dtm), "\n")
#dtm <- removeFrequentWords(dtm) #removing based on median tf-idf value
cat("After tf-idf: term count =", ncol(dtm), ", doc count =", nrow(dtm), "\n")
## the following line commented out to avoid sparse error
#dtm <- removeSparseTerms(dtm, 1 - (1.1/nrow(dtm)) )  #remove terms appearing only in 1 document
#dtm <- dtm[row_sums(dtm) > 0,]  #remove docs that have no terms remaining (unlikely event)
cat("After removing terms appearing only in 1 document: term count =", ncol(dtm), ", doc count =", nrow(dtm), "\n")


#setup parallel backend to use 8 processors
cl<-makeCluster(8) ## change to 8 for multiprocessing
registerDoParallel(cl)

## original foreach(topicCount = 2:nrow(dtm) #max = 1 topic per document
##foreach(topicCount = c(2, 3) #max = 1 topic per document
foreach(topicCount = c(20) #max = 1 topic per document ## changed topic count=20 as per IBM!!
        , .packages='topicmodels' #include package
) %do%
{ #change to %dopar% for multi execution
  cat("*RUNNING SINGLE PROCESSING*\n")
  cat("TopicCount:", topicCount, "\n") #to screen (no screen output is parallel mode)  
  mdl <- LDA(dtm, topicCount) #LDA model 
  topic.keyword <- terms(mdl, topKeywordCount)
  mdl.post <- posterior(mdl) #get posterior data
} ##end foreach

## the following nested loop was added to append the p-value per topic.keyword
Ttopic.keyword <- topic.keyword ## make a copy

## the following nested loop was added to write the p-values of topics by column

###outPval <- paste(args[1], "pvalues", xSystime, "_", readFrom, "-", topicCount, ".txt", sep = "") ## OKARGS
###for (j in 1:topicCount) {
###  cat("TOPIC ", j, "\n", append = T,
###      file = outPval)
###  cat("KEYWORD,PROBABILITY\n", append = T,
###      file = outPval)
###  for (i in 1:topKeywordCount){
###    
###    Ttopic.keyword[i,j] <-
###      Ttopic.keyword[i,j] <- paste(topic.keyword[i,j], ",", mdl.post$terms[[j, topic.keyword[i,j]]])
###    cat(Ttopic.keyword[[i,j]],"\n", append = T,
###        file = outPval)
###  }
###}

##for (i in 1:topKeywordCount){
##  for (j in 1:topicCount) {
##    Ttopic.keyword[i,j] <- paste(topic.keyword[i,j], "\t,", mdl.post$terms[[j, topic.keyword[i,j]]])
##    ## cat("This is the P-Value for  term:", i, ":", j, ">", topic.keyword[i,j], "\n")
##  }
##}

##cat("DEBUG: year is", year, "\n")
##write.csv(topic.keyword, file = paste(outDir,"TEST_lda_keywords_", args[2],"_",year, "-", topicCount, ".csv", sep = "") )
##write.csv(Ttopic.keyword,
##          file = paste("pvalues_", xSystime, "_", readFrom, "-", topicCount, ".csv", sep = "") )

## code added to support report creation

## Will retrieve all ids and questions
###t <- topics(mdl)
###len <- length(topics(mdl))
###saveTo <- paste(args[1], "Report-", xSystime, "_", readFrom, ".txt") ## OKARGS
###cat("question_id\tquestion_description\ttopic\t\n", sep="\t", append = T, file = saveTo) # to file
###for (i in 1:len){
###  ri <- corp[[i]]$meta$id
###  rq <- corp[[i]]$meta$quest
###  rt <- t[i]
###  r <- paste(xURL, ri, "\t", rq, "\t", rt, sep="")
  ## cat ("@ for cat i=", i, "\n")
  ###cat(r, "\n", append = T, file = saveTo) # to file
###}

## The following code was added for generating the specificity per keyword

## t is number of terms in dtm
## tC is the index of topicCount
## kC in the index of topKeywordCount
## topKeywordCount is the number of keywords
## dC is the index of number of topics
## countKw is the counter of the keyword that appears in the topics

allDocuments <- nrow(dtm) ## Size in rows of dtm
allKeywords <- ncol(dtm)
countKw <- matrix(0, topKeywordCount, topicCount) ## initialize count keyword matrix with 0

## ========================== code added to calculate frequencies ================================
countKw <- array(0,dim=c(topKeywordCount, topicCount)) ## intermediate array to handle the counting of kW in allDocuments
t <- topics(mdl) ## documents and the topics it belongs to
z <- as.matrix(dtm) ## dtm as matrix
dfz <- as.data.frame(z) ## converts z to data frame
tK <- 0 ## array(0,dim=c(topKeywordCount, topicCount))

acumTo <- rep(0,topicCount) ## initialize acum of topics
for(i in 1:topKeywordCount){
  acumKw<-0
  for(j in 1:topicCount){
    xlookup <- topic.keyword[i,j]
    colZ <- which(colnames(z)==topic.keyword[i,j]) ## finds the column in z that the kW is in
    
    for(k in 1:allDocuments){
      if(dfz[[colZ]][k] > 0) {
        tK <- t[[k]]  ## indek tk is the topic number
        
        if(tK==j){
          ##acumTo[j] <- acumTo[j] + 1 ## don't need this!!
          if(dfz[[colZ]][k] > 0){
            acumKw <- acumKw + 1
          }
        } ## if
        
        ###cat("DEBUG lookup=", xlookup, "i=", i ," j=", j ,"k=", k, " acumKw=", acumKw,
        ###    "tK=", tK," and t[[", k, "]]=", t[[k]], "*NO*acumTo=", acumTo, " colZ=", colZ, " and dfz=",dfz[[colZ]][k],
        ###    "\n", append = T, file = paste( "debug-file-",xSystime))
      } ## if
    } ## for k
    cat("acumKw after for k=", acumKw, "\n")
    if(!is.null(acumKw)){
      cat("i j =",i, ",",  j, "and acumKw=", acumKw, "\n")
      countKw[i,j] <- acumKw
      ###cat("DEBUG countKw[",i,",",j,"] acumKw=", acumKw,"\n", append = T, file = "countKwfile")
      acumKw <-0
    } ## if
  } ## for j 
  
} ## for i

## Calculation of N as the number of documents per topic

for(i in 1:nrow(dtm)){
  topic<-t[[i]]
  acumTo[topic]<-acumTo[topic]+1
}
# ===================================================================================================
##for (tC in 1: topicCount ){
##  for (kC in 1: topKeywordCount) {
##    cat("countwK[", kC, ",", tC, "]=", countKw[kC,tC], 
##        " **keyword =", xlookup, "\n", append = T, file = "Term-lookupSum")
##  }
##}

## the following nested loop was added to write the topics by column

###   cannot open file './devWorks/idf_Mon-Jun-27_17-34-29_2016_./devWorks/2002.txt-5.txt': No such file or directory

### add Topic number to file name
outIdf <- c()

for (j in 1:topicCount) {
  outIdf[j] <- paste(args[1], "idf_", xSystime, "_", args[2], "-T", j, ".txt", sep = "")  
## DONT NEED  cat("TOPIC ", j, "\n", append = T,
## DONT NEED      file = outIdf)
  cat("TOPIC,YEAR,KEYWORD,PROBABILITY,N,d,idf\n", append = T,
      file = outIdf[j])
  for (i in 1:topKeywordCount){
    
    idf<-log10(acumTo[j]/countKw[i, j])
    cat("i j", i, " ", j, "idf= log(", acumTo[j], "/", countKw[i,j], ")\n") ## DEBUG
    Ttopic.keyword[i,j] <-
      paste(j, ",", args[2], ",", topic.keyword[i,j], ",", mdl.post$terms[[j, topic.keyword[i,j]]], ",",
            acumTo[j], ",", countKw[i, j], ",", idf) ## going to work with a copy of topic.keywords instead
    cat(Ttopic.keyword[[i,j]],"\n", append = T,
        file = outIdf[j])
  }
}

cat("**Done**\n")
