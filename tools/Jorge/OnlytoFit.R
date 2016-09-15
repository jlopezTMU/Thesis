################
##
## program to fit to_fit according to different powers
##
## #############

fn <- "toFit_file.csv"
to_fit <- read.delim(fn, sep=",")
to_fit <- as.data.frame(to_fit)

print("^2 terms")
b.lm <- lm(b ~ N + I(N^2) + X + I(X^2), data = to_fit)
summary(b.lm)

print("^3 terms")
b.lm <- lm(b ~ N + I(N^2) + I(N^3) + X + I(X^2) + I(X^3), data = to_fit)
summary(b.lm)

print("^4 terms")
b.lm <- lm(b ~ N + I(N^2) + I(N^3) + I(N^4) + X + I(X^2) + I(X^3) + I(X^4), data = to_fit)
summary(b.lm)

print("^5 terms")
b.lm <- lm(b ~ N + I(N^2) + I(N^3) + I(N^4) + I(N^5) + X + I(X^2) + I(X^3) + I(X^4) + I(X^5), data = to_fit)
summary(b.lm)

print("^6 terms")
b.lm <- lm(b ~ N + I(N^2) + I(N^3) + I(N^4) + I(N^5) + I(N^6) + X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6), data = to_fit)
summary(b.lm)

print("^7 terms")
b.lm <- lm(b ~ N + I(N^2) + I(N^3) + I(N^4) + I(N^5) + I(N^6) + I(N^7) + X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7), data = to_fit)
summary(b.lm)


