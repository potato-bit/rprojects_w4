library('tidyverse')

# PART 1
dw <- read_csv("wb_warwick.csv")
glimpse(dw)

## Task 1
lm(wb ~ 1, dw)

fit1 <- lm(wb ~ ig, dw)
summary(fit1)
ggplot(dw, mapping=aes(x=ig,y=wb)) + geom_point(position='jitter') + geom_smooth(method='lm')

fit2 <- lm(wb ~ cooked, dw)
summary(fit2)
ggplot(dw, mapping=aes(x=cooked,y=wb)) + geom_point(position='jitter') + geom_smooth(method='lm')

fit3 <- lm(wb ~ rolf, dw)
summary(fit3)
ggplot(dw, mapping=aes(x=rolf,y=wb)) + geom_point(position='jitter') + geom_smooth(method='lm')

### The scatter plot showig well-beinng against number of instagram followers shows that there exists one possible
### outlier with >200 followers

### cooking is the best predictor with an R^2 score of 0.36, and has the strongest bivariate relationship with
### with well-being with a coefficient of 0.793

sigma(fit1)
sigma(fit2)
sigma(fit3)

### fit2 has the lowest residual standard deviation, which makes it also the most accurate estimator.

### wb = 7.237 + -0.018ig
### wb = 5.73 - 5.34*10^-16rolf

### H0: b_1 = 0 v. H1: b_1 != 0

df1 <-  data.frame(ig=c(10,100,200))
predict(fit1,newdata=df1)
### for all (ig,wb): (10,7.05); (100,5.45); (200,3.66)
### this is probably not very accurate due to the low goodness-of-fit and the low coefficient for ig

df2 <- data.frame(cooked=c(1,3,6))
predict(fit2,newdata=df2)
### for all (cooked,wb): (1,2.15); (3,3.74); (6,6.13)
### this might be more accurate with the higher goodness-of-fit 

df3 <- data.frame(rolf=c(0,3,7))
predict(fit3,newdata=df3)
### for all (rolf,wb): (0,5.73); (3,5.73); (7,5.73)
### This might be accurate predictions as there is almost no relationship between `rolf` and `wb`

dw