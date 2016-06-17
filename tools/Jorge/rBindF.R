## this program merges all FN files into a file called files-merged using rbind to later
## perform lm analysis
## Author Jorge, 6-16

cat("Generating  Merging file <file-merged> for: ~/Thesis/Fitting/salesforce-m-FK/files-FN\n")

setwd("~/Thesis/Fitting/all-FK") ## This path needs to be customized
listf <- list.files(pattern="FileFN-05")
cat("files to process are: ", listf, "\n")

## interactive commands doesn't work in ubuntu!

facum <- c(5.0,1.0)
facum <- as.data.frame(facum, colnames=c("K","F"))
outFileFN <- ""
outFileFN <- "file-merged"
if (file.exists(outFileFN)) file.remove(outFileFN) ## remove the output file if already exists
for (i in 1:length(listf)){
  readFromfileName <- ""
  readFromfileName <- listf[i]
  dat<-read.delim(file = listf[i], header = T, quote = "", sep = "\t")
  dat <- as.data.frame(dat)
  facum <- rbind(facum[,],dat[,])
  write.csv(facum, file=outFileFN)
  cat("Processing file:", readFromfileName, "\n")
  }
