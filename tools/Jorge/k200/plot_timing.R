dat <- read.table("~/Downloads/Posts.xml.csv.2014-1.topic_frequency"
                  , sep = "\t"
                  , header = T
                  )


to_plot <- unique(data.frame(dat$topicCount, dat$time..sec.))

cat(paste( "Max time",  max(to_plot$dat.time..sec.), "seconds"))

cat(paste( "Total time",  sum(to_plot$dat.time..sec.), "seconds"))

cat(paste( "Total time",  sum(to_plot$dat.time..sec.)/60/60/24, "days"))

pdf(file = "./figures/timing.pdf")
plot(to_plot$dat.topicCount, to_plot$dat.time..sec.
     , xlab = "K"
     , ylab = "Time (seconds)"
     )

grid()

dev.off()
