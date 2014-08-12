library(mclust)

args <- commandArgs(trailingOnly = TRUE)
readFrom <- args[1]
saveTo <- paste(readFrom, ".cluster_bic", sep = "")

dat <- read.csv( readFrom )
#remove "post id" column
dat <- dat[, names(dat) != "X0" ]

#do BIC analysis
dat.bic <- mclustBIC(dat, G = 1:30)
#save results
save(dat.bic, file = saveTo)

cat("Saved data to", saveTo, "\n")
cat("Done\n")

#plot(dat.bic, legendArgs = list(x = "topleft"))
