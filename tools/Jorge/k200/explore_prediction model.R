dat <- read.csv( "to_fit.csv")

##
# For every month and top-x we built a model
# log(postFraction) ~  lm_intercept + lm_slope * log(topicCount)
# Let us express lm_intercept and lm_slope as functions of X and N
##


#remove non-numeric columns
dat <- dat[ , !(names(dat) %in% c("X.1", "time_interval", "time_frame", "dataset_name"))]

summary(dat)
cor(dat)


library(corrgram)
corrgram(dat, order=F, lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax, 
         main="Relation between vara")

plot(log(dat$X), dat$lm_intercept)
plot(dat$X, dat$lm_slope)

# The intercept is approximate pretty good:
summary(lm(lm_intercept ~ log(X), data = dat))

# The slope is not so much. Jorge, any ways to improve?
dat.lm.slope <- lm(lm_slope ~ X  + N, data = dat)
summary(dat.lm.slope)
plot(dat.lm.slope)

