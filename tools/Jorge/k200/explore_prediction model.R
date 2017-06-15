#set to True if you want to enable filtering of K > 200, else set to False
data_filter_200 <- T

if(data_filter_200){
  dat <- read.csv("to_fit.remove_top_25_percent_and_values_gt_200.csv")
}else{
  dat <- read.csv("to_fit.remove_top_25_percent.csv")
}


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
         main="Relation between vars")

################################################################################################
# Let's play around with flexible model
################################################################################################
plot(log(dat$X), dat$lm_intercept)
plot(dat$X, dat$lm_slope)

# The intercept approximate is pretty good
dat.lm.intercept <- lm(lm_intercept ~ I(log(X)) + I(log(N)), data = dat)
summary(dat.lm.intercept)

# The intercept approximate is pretty good:
#plot(dat.lm.intercept)

plot(dat$lm_intercept, predict(dat.lm.intercept)
     , xlab = "Actual"
     , ylab = "Predicted"
     , xlim = c(0, 3)
     , ylim = c(0, 3)
)
segments(0, 0, 3, 3, col = "red")


# The intercept approximate is pretty good; adding quadratic terms improves the situation but not dramatically
dat.lm.intercept.complex <- lm(lm_intercept ~ I(log(X)) + I(log(N)) + I(log(X)^2) + I(log(N)^2), data = dat)
summary(dat.lm.intercept.complex)


# The slope is not so much. 
dat.lm.slope <- lm(lm_slope ~ X  + N, data = dat)
summary(dat.lm.slope)

#plot(dat.lm.slope)

#plot(dat$lm_slope, predict(dat.lm.slope))

# Here is a more complex version with logs and quadratic terms. X^2 is not statistically signifcant and is removed
dat.lm.slope.complex <- lm(lm_slope ~ X  + N + I(N^2) + log(X) + log(N), data = dat)
summary(dat.lm.slope.complex)



################################################################################################
# Now let's analyze constraint model
################################################################################################
dat.lm.b <- lm(b ~ X + N, data = dat)
summary(dat.lm.b)

dat.lm.b.complex <- lm(b ~ X + N + log(X) , data = dat)
summary(dat.lm.b.complex)

# dat.lm.b <- lm(b ~ X + N + I(X^2) + I(N^2), data = dat)
# summary(dat.lm.b)
# 
# dat.lm.b <- lm(b ~ log(X) + log(N), data = dat)
# summary(dat.lm.b)
# 
# 
# dat.lm.b <- lm(b ~ log(X) + log(N) + I(log(X)^2) + I(log(N)^2), data = dat)
# summary(dat.lm.b)
# 
# dat.lm.b <- lm(b ~ log(X) + log(N) + I(log(X)^2) + I(log(N)^2), data = dat)
# summary(dat.lm.b)



################################################################################################
# output models for LaTex
################################################################################################
library(stargazer)

stargazer(dat.lm.intercept, dat.lm.intercept.complex, ci=TRUE, ci.level=0.90, single.row=TRUE)
stargazer(dat.lm.slope, dat.lm.slope.complex, ci=TRUE, ci.level=0.90, single.row=TRUE)
stargazer(dat.lm.b, dat.lm.b.complex, ci=TRUE, ci.level=0.90, single.row=TRUE)

# The code below was used to generate the tables showing stats for models built for datasets with and without filter simultaneously. 
# This is achieved using ugly hack. 
# 1. Create models for the dataset with filter ("data_filter_200 <- T") 
# 2. Save the models into the models with suffix "_t" below
# 3. Create models for the dataset without filter ("data_filter_200 <- F"), but do not repeat step 2 
# 4. Execute stargazer functions

# if(data_filter_200){
#   dat.lm.intercept_t = dat.lm.intercept
#   dat.lm.intercept.complex_t = dat.lm.intercept.complex
#   dat.lm.slope_t = dat.lm.slope
#   dat.lm.slope.complex_t = dat.lm.slope.complex
#   dat.lm.b_t = dat.lm.b
#   dat.lm.b.complex_t = dat.lm.b.complex
# }
# 
# stargazer(dat.lm.intercept, dat.lm.intercept.complex, dat.lm.intercept_t, dat.lm.intercept.complex_t, ci=TRUE, ci.level=0.90, single.row=FALSE)
# stargazer(dat.lm.slope, dat.lm.slope.complex, dat.lm.slope_t, dat.lm.slope.complex_t, ci=TRUE, ci.level=0.90, single.row=FALSE)
# stargazer(dat.lm.b, dat.lm.b.complex, dat.lm.b_t, dat.lm.b.complex_t, ci=TRUE, ci.level=0.90, single.row=FALSE)

################################################################################################
# let us save the models for validation purposes
################################################################################################
if(data_filter_200){
  model_file_name <- "./models/models_fitting.remove_top_25_percent_and_values_gt_200.rda"
}else{
  model_file_name <- "./models/models_fitting.remove_top_25_percent.rda"
}

save(dat.lm.intercept, dat.lm.intercept.complex, dat.lm.slope, dat.lm.slope.complex, 
     dat.lm.b, dat.lm.b.complex,
     file = model_file_name)

