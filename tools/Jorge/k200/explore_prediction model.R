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
dat.lm.intercept <- lm(lm_intercept ~ log(X) + log(N), data = dat)
summary(dat.lm.intercept)

# The slope is not so much. Jorge, any ways to improve?
dat.lm.slope <- lm(lm_slope ~ X  + N, data = dat)
summary(dat.lm.slope)
plot(dat.lm.slope)


#output models for LaTex
library(stargazer)
stargazer(dat.lm.intercept, ci=TRUE, ci.level=0.90, single.row=TRUE)
stargazer(dat.lm.slope, ci=TRUE, ci.level=0.90, single.row=TRUE)

