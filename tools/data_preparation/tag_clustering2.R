library(mclust)

args <- commandArgs(trailingOnly = TRUE)
readFrom <- args[1]
saveTo <- paste(readFrom, ".cluster_bic", sep = "")

dat <- read.delim( readFrom )
#remove "post id" column
dat <- dat[, names(dat) != "X0" ]

#do BIC analysis
dat.bic <- mclustBIC(dat, G = 1:10)
#save results
save(dat.bic, saveTo)

#plot(dat.bic, legendArgs = list(x = "topleft"))