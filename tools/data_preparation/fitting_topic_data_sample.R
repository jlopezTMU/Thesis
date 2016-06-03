#TODO: 
# * run fitModel in a temp window for Android 2012-1, add Aaron's code there (record stats)
# * in addition to power law, fit lognormal, etc. -- compare using likelyhood? (see paper) and graph (section 2)
#   * http://cran.r-project.org/web/packages/poweRlaw/vignettes/examples.pdf
# * since Section 1.4 on how to freeze x_min. Run computations with free x_min and then with fixed x_min, then compare
# * get estimates for b, do test, compare with other distr (as per 3 step process in ArXiv)
# * add lines to figure (the 2nd figure is in plot_preplexity.R)

library("qpcR")
fractionOfPosts <- 4 #truncate datapoints greater than N / fractionOfPosts

#dat.stats <- data.frame(month = vector(), topicCount = vector(), dataPnts = vector(), topX = vector(), b = vector() )
# for(y in 2012:2013){
#   for(i in 1:12){
#     dat <- read.delim(file = paste("./topic_frequency/Posts.xml.w_ans.csv.", y, "-", i, ".topic_frequency", sep =""))  
#     topicCount <- sort(unique(dat$topicCount))
#     for(x in 5:20){#top X topics
#       topX <- data.frame(topicCount = vector(), postFraction = vector())
#       for( t in topicCount ){
#         d <- dat [which(dat$topicCount == t), ]
#         freq <- sort(d$topic.frequency, decreasing = T)
#         topX <-  rbind(topX, 
#                        data.frame(topicCount = t, postFraction = sum(freq[1:x]) / sum (freq)))
#       }
#       ds <- topX[topX$topicCount >= x & topX$topicCount < max(topicCount) / fractionOfPosts,] #careful with /fractionOfPosts, as we do not have all topics for some
#       topX.nls <- nls(postFraction ~ x^(-b) * topicCount^b, data = ds
#                       ,  start = list( b = -1),)
#   
#       dat.stats <-  rbind(dat.stats, 
#                      data.frame(month = i, topicCount = max(topicCount), dataPnts = length(ds$topicCount)
#                                 , topX = x, b = unlist(coef(topX.nls)[1])))
#     }
#   }
# }

#android - monthly
dat.stats.android.m <- data.frame()
for(y in 2012:2013){
  for(i in 1:12){
    
    dat <- read.delim(file = paste("./topic_frequency_2/android/monthly/Posts.xml.w_ans.csv.", y, "-", i, ".topic_frequency", sep =""))  
    topicCount <- sort(unique(dat$topicCount))
    recordCount <- getRecordCount(dat)
    maxTopicCount = recordCount / fractionOfPosts
    out<- fitModel(dat, topicCount, maxTopicCount) #max = at least fractionOfPosts records per topic (on average)
    dat.stats.android.m <-  rbind(dat.stats.android.m, out)
  }
}

#android - quarterly
#here i is not a month but a quarter
dat.stats.android.q <- data.frame()
for(y in 2012:2013){
  for(i in 1:4){
    if(y == 2012 & i == 3){ next } #TODO: get the data -- 2012 q3 is missing
    dat <- read.delim(file = paste("./topic_frequency_2/android/quarterly/Posts.xml.w_ans.csv.", y, "-q", i, ".topic_frequency", sep =""))  
    topicCount <- sort(unique(dat$topicCount))
    recordCount <- getRecordCount(dat)
    maxTopicCount = recordCount / fractionOfPosts
    out<- fitModel(dat, topicCount, maxTopicCount)  #max = at least fractionOfPosts records per topic (on average)
    dat.stats.android.q <-  rbind(dat.stats.android.q, out)
  }
}

#firefox - monthly ch
dat.stats.firefox.m <- data.frame() 
lst <- rbind(c(2012, 2), c(2013, 2), c(2013, 4))
for (z in 1:3){
  y = lst[z,1]
  i = lst[z,2]
  dat <- read.delim(file = paste("./topic_frequency_2/firefox/monthly/posts.firefox.bugzilla.", y, "-0", i, ".csv.", y, "-", i, ".topic_frequency", sep =""))  
  topicCount <- sort(unique(dat$topicCount))
  recordCount <- getRecordCount(dat) 
  maxTopicCount = 500 #500 #limit to 500
  out<- fitModel(dat, topicCount, maxTopicCount)  
  dat.stats.firefox.m <-  rbind(dat.stats.firefox.m, out)
}

power_law_fit <- rbind(
  cbind( dset = "android", dt = "monthly", dat.stats.android.m),
  cbind( dset = "android", dt = "quarterly", dat.stats.android.q),
  cbind( dset = "firefox", dt = "monthly", dat.stats.firefox.m)
)
write.table(power_law_fit, "./topic_frequency_2/power_law_fit_4.max_eq_0.25N_or_500.txt", sep="\t", row.names = FALSE)

plot(power_law_fit$topX, power_law_fit$R2.nls - power_law_fit$R2.g)
hist(power_law_fit$R2.nls - power_law_fit$R2.g
     , xlab = "R2.custom - R2.general"
     , ylab = "Observations Count"
     , main = ""
     , ylim = c(0, 1000))
grid()
#############
#Try to fit coefficients with linear model
#############
plot(data.frame(power_law_fit$recordCount, power_law_fit$topX, power_law_fit$b))

plot(power_law_fit$recordCount, power_law_fit$b)

b.lm <- lm(b ~ recordCount + I(recordCount^2) + topX + I(topX^2)  + I(topX^3) + recordCount*topX, data = power_law_fit)
b.lm <- lm(b ~ recordCount + topX, data = power_law_fit)
b.lm <- lm(b ~ recordCount + I(recordCount^2) + topX + I(topX^2), data = power_law_fit)
summary(b.lm)
plot(b.lm)


b.nls = nls(b ~  A1 * exp(topX / t1) + y0 + A2 * recordCount, data = power_law_fit
                    , start = list( A1 = -0.16, t1 = -17, y0 = -0.0, A2 = 2E-5)) 
summary(b.nls)
b.nls.r2 <- 1-(deviance(b.nls)/sum((power_law_fit$b-mean(power_law_fit$b))^2))
plot(b.nls)

#############

#Save raw data : android - monthly
raw.stats.android.m = data.frame(year = vector(), month = vector(), topX = vector(), topicCount = vector(), postFraction = vector())
for(y in 2012:2013){
  for(i in 1:12){
    dat <- read.delim(file = paste("./topic_frequency_2/android/monthly/Posts.xml.w_ans.csv.", y, "-", i, ".topic_frequency", sep ="")) 
    topicCount <- sort(unique(dat$topicCount))
    out<- getRawData(dat, topicCount)
    raw.stats.android.m <-  rbind(raw.stats.android.m, out)
  }
}
write.table(raw.stats.android.m, "./topic_frequency_2/android/monthly/topRaw_2.txt", sep="\t", row.names = FALSE)


#Save raw data : android - quarterly
raw.stats.android.q = data.frame(year = vector(), month = vector(), topX = vector(), topicCount = vector(), postFraction = vector())
for(y in 2012:2013){
  for(i in 1:4){
    #TODO: get the data
    if(y == 2012 & i == 3){ next } #TODO: get the data -- 2012 q3 is missing
    dat <- read.delim(file = paste("./topic_frequency_2/android/quarterly/Posts.xml.w_ans.csv.", y, "-q", i, ".topic_frequency", sep =""))  
    topicCount <- sort(unique(dat$topicCount))
    out<- getRawData(dat, topicCount)
    raw.stats.android.q <-  rbind(raw.stats.android.q, out)
  }
}
write.table(raw.stats.android.q, "./topic_frequency_2/android/quarterly/topRaw_2.txt", sep="\t", row.names = FALSE)

#Save raw data : firefox - monthly
raw.stats.firefox.m = data.frame(year = vector(), month = vector(), topX = vector(), topicCount = vector(), postFraction = vector())
lst <- rbind(c(2012, 2), c(2013, 2), c(2013, 4))
for (z in 1:3){
  y = lst[z,1]
  i = lst[z,2]
  dat <- read.delim(file = paste("./topic_frequency_2/firefox/monthly/posts.firefox.bugzilla.", y, "-0", i, ".csv.", y, "-", i, ".topic_frequency", sep =""))  
  
  topicCount <- sort(unique(dat$topicCount))
  out<- getRawData(dat, topicCount)
  raw.stats.firefox.m <-  rbind(raw.stats.firefox.m, out)

}
write.table(raw.stats.firefox.m, "./topic_frequency_2/firefox/monthly/topRaw_2.txt", sep="\t", row.names = FALSE)


#produce a picture for paper : android 2013-4, X=5, 10, 20, 50

pic <- raw.stats.android.m 
pic <- pic[pic$year == 2013 & pic$month == 4,]
top5 <- pic[pic$topX == 5 & pic$topicCount >= 5,]
top10 <- pic[pic$topX == 10 & pic$topicCount >= 10,]
top20 <- pic[pic$topX == 20 & pic$topicCount >= 20,]
top50 <- pic[pic$topX == 50 & pic$topicCount >= 50,]

postCount = max(top5$topicCount)

plot(top5$topicCount, top5$postFraction
     , log = "xy"
     , xlim = c(5, postCount)
     , type = "l"
     , xlab = "K"
     , ylab = "F"
     , col = 1
     )
#lines(top10$topicCount, top10$postFraction, col = 2)
lines(top20$topicCount, top20$postFraction, col = 2)
lines(top50$topicCount, top50$postFraction, col = 3)
abline( v = postCount/4, lty = 4 ) # N/4
#abline( v = postCount/3 ) # N/3

#lines(top5$topicCount, eq2.3(postCount, 5, top5$topicCount), col = 1, lty = 3)
#lines(top5$topicCount, eq2.3(postCount, 10, top10$topicCount), col = 1, lty = 3)
#lines(top5$topicCount, eq2.3(postCount, 20, top20$topicCount), col = 1, lty = 3)
#lines(top5$topicCount, eq2.3(postCount, 50, top50$topicCount), col = 1, lty = 3)

lines(top5$topicCount, eq2(postCount, 5, top5$topicCount), col = 1, lty = 2)
#lines(top10$topicCount, eq2(postCount, 10, top10$topicCount), col = 2, lty = 2)
lines(top20$topicCount, eq2(postCount, 20, top20$topicCount), col = 2, lty = 2)
lines(top50$topicCount, eq2(postCount, 50, top50$topicCount), col = 3, lty = 2)

text(400, 0.06, "X=5")
text(400, 0.18, "X=20")
text(400, 0.38, "X=50")
text(220, 0.9, "N/4=140")
grid()

eq2 <- function(N, X, K){
    b = -7.775e-01 -2.356e-05*N + 5.586e-09*(N^2) +  4.442e-03 * X + -3.450e-05 * (X^2)  
    F = X^(-b) * K^b
    return(F)
}

get_x_topower_minus_b <- function(N, X){
  b = -7.775e-01 -2.356e-05*N + 5.586e-09*(N^2) +  4.442e-03 * X + -3.450e-05 * (X^2)  
  out = X^(-b)
  return(out)
}


plot(dat.stats.firefox.m$topX, dat.stats.firefox.m$a.a)
lines(dat.stats.firefox.m$topX
      , get_x_topower_minus_b(dat.stats.firefox.m$recordCount, dat.stats.firefox.m$topX)
      )

eq2.2 <- function(a, b, K){
  #b = -7.775e-01 -2.356e-05*N + 5.586e-09*(N^2) +  4.442e-03 * X + -3.450e-05 * (X^2)  
  F = a * K^b
  return(F)
}

eq2.3 <- function(N, X, K){
  #a = 8.403e-01 +1.606e-03*N -2.307e-07*(N^2) +  4.565e-01 * X -4.041e-03 * (X^2)  
  #b = -7.813e-01 -2.598e-05*N + 4.823e-09*(N^2) +  3.380e-03 * X +  -2.118e-05 * (X^2)  
  a = 2.747e+00 -6.147e-04*N -6.050e-09*(N^2) +  4.370e-01 * X -5.088e-03 * (X^2)  
  b = -8.236e-01  +1.639e-05*N + 1.057e-09*(N^2) +  4.544e-03 * X -1.713e-05 * (X^2)  
  F = a * K^b
  return(F)
}

####################
# graph for 
# Android 2013-q2
pic <- raw.stats.android.q
pic <- pic[pic$year == 2012 & pic$month == 2,]
top5 <- pic[pic$topX == 5 & pic$topicCount >= 5,]
top10 <- pic[pic$topX == 10 & pic$topicCount >= 10,]
top20 <- pic[pic$topX == 20 & pic$topicCount >= 20,]
top50 <- pic[pic$topX == 50 & pic$topicCount >= 50,]


postCount = max(top5$topicCount)

plot(top5$topicCount, top5$postFraction
     , log = "xy"
     , xlim = c(5, postCount)
     , type = "l"
     , xlab = "K"
     , ylab = "F"
     , col = 1
)
#lines(top10$topicCount, top10$postFraction, col = 2)
lines(top20$topicCount, top20$postFraction, col = 2)
lines(top50$topicCount, top50$postFraction, col = 3)
abline( v = postCount/4, lty = 4 ) # N/4
#abline( v = postCount/3 ) # N/3

lines(top5$topicCount, eq2(postCount, 5, top5$topicCount), col = 1, lty = 2)
#lines(top10$topicCount, eq2(postCount, 10, top10$topicCount), col = 2, lty = 2)
lines(top20$topicCount, eq2(postCount, 20, top20$topicCount), col = 2, lty = 2)
lines(top50$topicCount, eq2(postCount, 50, top50$topicCount), col = 3, lty = 2)



#lines(top5$topicCount, eq2.3(postCount, 5, top5$topicCount), col = 1, lty = 2)
#lines(top10$topicCount, eq2.3(postCount, 10, top10$topicCount), col = 2, lty = 2)
#lines(top20$topicCount, eq2.3(postCount, 20, top20$topicCount), col = 2, lty = 2)
#lines(top50$topicCount, eq2.3(postCount, 50, top50$topicCount), col = 3, lty = 2)

text(450, 0.05, "X=5")
text(450, 0.15, "X=20")
text(450, 0.38, "X=50")
text(530, 0.9, "N/4=318")
grid()


##################




####################
# graph for 
# Firefox 2013-2 
pic <- raw.stats.firefox.m 
pic <- pic[pic$year == 2013 & pic$month == 4,]
top5 <- pic[pic$topX == 5 & pic$topicCount >= 5,]
top10 <- pic[pic$topX == 10 & pic$topicCount >= 10,]
top20 <- pic[pic$topX == 20 & pic$topicCount >= 20,]
top50 <- pic[pic$topX == 50 & pic$topicCount >= 50,]


postCount = max(top5$topicCount)

plot(top5$topicCount, top5$postFraction
     , log = "xy"
     , xlim = c(5, postCount)
     , type = "l"
     , xlab = "K"
     , ylab = "F"
     , col = 1
)
#lines(top10$topicCount, top10$postFraction, col = 2)
lines(top20$topicCount, top20$postFraction, col = 2)
lines(top50$topicCount, top50$postFraction, col = 3)
#abline( v = postCount/4, lty = 4 ) # N/4
#abline( v = postCount/3 ) # N/3

#lines(top5$topicCount, eq2.3(postCount, 5, top5$topicCount), col = 1, lty = 3)
#lines(top5$topicCount, eq2.3(postCount, 10, top10$topicCount), col = 1, lty = 3)
#lines(top5$topicCount, eq2.3(postCount, 20, top20$topicCount), col = 1, lty = 3)
#lines(top5$topicCount, eq2.3(postCount, 50, top50$topicCount), col = 1, lty = 3)

  
#lines(top5$topicCount, eq2.2(4.070978817, -0.697967886, top5$topicCount), col = 3, lty = 2)
#lines(top5$topicCount, eq2.2(3.500543667, -0.636440211, top5$topicCount), col = 2, lty = 2)
#lines(top5$topicCount, eq2.2(3.500543667, -0.636440211, top10$topicCount), col = 2, lty = 2)
  

#lines(top5$topicCount, eq2.2(3.3315, -0.6832, top10$topicCount), col = 2, lty = 2)
#lines(top5$topicCount, eq2.2(3.3315, -0.6832, top20$topicCount), col = 2, lty = 2)

lines(top5$topicCount, eq2(postCount, 5, top5$topicCount), col = 1, lty = 2)
#lines(top10$topicCount, eq2(postCount, 10, top10$topicCount), col = 2, lty = 2)
lines(top20$topicCount, eq2(postCount, 20, top20$topicCount), col = 2, lty = 2)
lines(top50$topicCount, eq2(postCount, 50, top50$topicCount), col = 3, lty = 2)

text(400, 0.08, "X=5")
text(400, 0.22, "X=20")
text(400, 0.44, "X=50")
#text(220, 0.9, "N/4=140")
grid()


##################



#Fit model
fitModel <- function(dat, topicCount, maxTopicCount){
  dat.stats <- data.frame()
  recordCount <- getRecordCount(dat)  
  
  for(x in 5:50){#top X topics
    topX <- data.frame(topicCount = vector(), postFraction = vector())
    for( t in topicCount ){
      d <- dat [which(dat$topicCount == t), ]
      freq <- sort(d$topic.frequency, decreasing = T)
      topX <-  rbind(topX, 
                     data.frame(topicCount = t, postFraction = sum(freq[1:x]) / sum (freq)))
    }
    ds <- topX[topX$topicCount >= x & topX$topicCount < maxTopicCount,]
    #####################
    #get specific nls model
    topX.nls <- nls(postFraction ~ x^(-b) * topicCount^b, data = ds
                    ,  start = list( b = -1),)
    
    
    #computing R^2 for nls is mathematically incorrect, but it can help us to get a feeling
    R2.nls <- 1-(deviance(topX.nls)/sum((ds$postFraction-mean(ds$postFraction))^2))
    rmse.nls <- RMSE(topX.nls)
    #####################
    
    #####################
    #get specific nls model with 2 param
    topX.nls.2 <- nls(postFraction ~ a * topicCount^b, data = ds
                    ,  start = list( a = 1,  b = -1),)
    
  
    #computing R^2 for nls is mathematically incorrect, but it can help us to get a feeling
    R2.nls.2 <- 1-(deviance(topX.nls.2)/sum((ds$postFraction-mean(ds$postFraction))^2))
    rmse.nls.2 <- RMSE(topX.nls.2)
    #####################
    
    #####################
    #get specific nls model with 2 param, b is taken from 1-param model
    g.b = -7.775e-01 -2.356e-05*recordCount + 5.586e-09*(recordCount^2) +  4.442e-03 * x + -3.450e-05 * (x^2)
    topX.nls.a <- nls(postFraction ~ a * topicCount^g.b, data = ds
                      ,  start = list( a = x^(-g.b) ),)
    #computing R^2 for nls is mathematically incorrect, but it can help us to get a feeling
    R2.nls.a <- 1-(deviance(topX.nls.a)/sum((ds$postFraction-mean(ds$postFraction))^2))
    rmse.nls.a <- RMSE(topX.nls.a)
    
    
    
    #####################
    #get estimate based on general model
    g.b = -7.775e-01 -2.356e-05*recordCount + 5.586e-09*(recordCount^2) +  4.442e-03 * x + -3.450e-05 * (x^2)
    postFraction.estimate = x^(-g.b) * ds$topicCount^g.b
    R2.g <- 1 - ( sum ((  postFraction.estimate - ds$postFraction )^2) / sum((ds$postFraction-mean(ds$postFraction))^2))
    rmse.g <- sqrt( sum ((  postFraction.estimate - ds$postFraction )^2)/length(ds$postFraction) )
    #####################
    
    #####################
    #get estimate based on general 2-param model
    g2.a = 8.403e-01 +1.606e-03*recordCount -2.307e-07*(recordCount^2) +  4.565e-01 * x -4.041e-03 * (x^2)  
    g2.b = -7.775e-01 -2.356e-05*recordCount + 5.586e-09*(recordCount^2) +  4.442e-03 * x + -3.450e-05 * (x^2)
    postFraction2.estimate = g2.a * ds$topicCount^g2.b
    R2.g2 <- 1 - ( sum ((  postFraction2.estimate - ds$postFraction )^2) / sum((ds$postFraction-mean(ds$postFraction))^2))
    rmse.g2 <- sqrt( sum ((  postFraction2.estimate - ds$postFraction )^2)/length(ds$postFraction) )
    #####################
    
    #####################
    dat.stats <-  rbind(dat.stats, 
                        data.frame(year = y, month = i, R2.nls = R2.nls, R2.g = R2.g, recordCount = recordCount, topicCountMax = max(ds$topicCount), dataPntsInRegr = length(ds$topicCount)
                                   , topX = x, b = unlist(coef(topX.nls)[1]), rmse.nls = rmse.nls, rmse.g = rmse.g
                                   , a.2 = unlist(coef(topX.nls.2)[1]), b.2 = unlist(coef(topX.nls.2)[2]),  R2.nls.2 = R2.nls.2, rmse.nls.2 = rmse.nls.2
                                   , R2.g2 = R2.g2, rmse.g2 = rmse.g2 
                                   , a.a = unlist(coef(topX.nls.a)[1]), R2.nls.a = R2.nls.a, rmse.nls.a = rmse.nls.a
                                   ))
  }
  return( dat.stats )
}


#get number of post/records
getRecordCount <- function(dat){
  tmp <- dat [which(dat$topicCount == 8), ] #we assume that 8 topics always exist
  recordCount <- sum(tmp$topic.frequency)
  return(recordCount)
}



#save data
getRawData <- function(dat, topicCount){
  topX <- data.frame(year = vector(), month = vector(), topX = vector(), topicCount = vector(), postFraction = vector())
  for(x in 5:50){#top X topics    
    for( t in topicCount ){
      d <- dat [which(dat$topicCount == t), ]
      freq <- sort(d$topic.frequency, decreasing = T)
      topX <-  rbind(topX, 
                     data.frame(year = y, month = i, topX = x, topicCount = t, postFraction = sum(freq[1:x]) / sum (freq)))
    }
  }
  return( topX )
}



plot(dat.stats$topX,  dat.stats$b)

plot(dat.stats)
#log's increase the quality of the prediction just marginally
dat.stats.lm = lm(b ~  log(topX) +  log(topicCount), data = dat.stats) 
summary(dat.stats.lm)

# uncomment to analyse to add topicCount
dat.stats.nls = nls(b ~  A1 * exp(topX / t1) + y0 + A2 * topicCount, data = dat.stats
                    , start = list( A1 = -0.16, t1 = -17, y0 = -0.65, A2 = 1)) 

dat.stats.nls = nls(b ~  A1 * exp(topX / t1) + y0, data = dat.stats
                    , start = list( A1 = -0.16, t1 = -17, y0 = -0.65)) 

yVar <- dat.stats$b
r2 <- 1-(deviance(dat.stats.nls)/sum((yVar-mean(yVar))^2))
summary(dat.stats.nls)


plot(residuals(dat.stats.lm))
plot(dat.stats$topX, residuals(dat.stats.nls))
plot(dat.stats$topicCount, residuals(dat.stats.nls))
