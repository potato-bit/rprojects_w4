library('tidyverse')
library('cowplot')

# PART 1
dw <- read_csv("wb_warwick.csv")
glimpse(dw)

## Task 1
lm(wb ~ 1, dw)

fit1 <- lm(wb ~ ig, dw)
summary(fit1)
ggplot(dw, mapping=aes(x=ig,y=wb)) + geom_jitter(height=0.1,width=0.1) + geom_smooth(method='lm') + theme_cowplot()

fit2 <- lm(wb ~ cooked, dw)
summary(fit2)
ggplot(dw, mapping=aes(x=cooked,y=wb)) + geom_point(position='jitter') + geom_smooth(method='lm')

fit3 <- lm(wb ~ rolf, dw)
summary(fit3)
ggplot(dw, mapping=aes(x=rolf,y=wb)) + geom_point(position='jitter') + geom_smooth(method='lm')

### The scatter plot showing well-being against number of instagram followers shows that there exists one possible
### outlier with >200 followers

dw2 <- dw %>% filter(ig<200)
fit4 <- lm(wb ~ ig, dw2)
summary(fit4)
ggplot(dw2, mapping=aes(x=ig,y=wb)) + geom_jitter(height=0.1,width=0.1) + geom_smooth(method='lm') + theme_cowplot()

### After removing the outlier, ig is no  longer a statistically significant estimator of well-being, and the
### and the relationship is almost flat.

fit5 <- lm(wb ~ cooked, dw2)
summary(fit5)
ggplot(dw2, mapping=aes(x=cooked,y=wb)) + geom_jitter(height=0.1,width=0.1) + geom_smooth(method='lm') + theme_cowplot()

### the relationship becomes stronger after removing the outlier

fit6 <- lm(wb ~ rolf, dw2)
summary(fit6)
ggplot(dw2, mapping=aes(x=rolf,y=wb)) + geom_jitter(height=0.1,width=0.1) + geom_smooth(method='lm') + theme_cowplot()

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
predict(fit4,newdata=df1)
### for all (ig,wb): (10,7.05); (100,5.45); (200,3.66)
### this is probably not very accurate due to the low goodness-of-fit and the low coefficient for ig

df2 <- data.frame(cooked=c(1,3,6))
predict(fit5,newdata=df2)
### for all (cooked,wb): (1,2.15); (3,3.74); (6,6.13)
### this might be more accurate with the higher goodness-of-fit 

df3 <- data.frame(rolf=c(0,3,7))
predict(fit6,newdata=df3)
### for all (rolf,wb): (0,5.73); (3,5.73); (7,5.73)
### This might be accurate predictions as there is almost no relationship between `rolf` and `wb`

### ultimately, with such a small sample size, the results of the regression analyses are not convincing, and the 
### effectiveness of the models may not translate to data outside the sample.

### Predictions could be improved by improving the sample size

df4 <- data.frame(cooked=c(0,1,2,3,4,5,6,7))
PI <- predict(fit5,newdata=df4,interval='prediction')
colnames(PI) <- c('fit_p','lwr_p','upr_p')
CI <- predict(fit5,newdata=df4,interval='confidence')
colnames(CI) <- c('fit_c','lwr_c','upr_c')
results <- as_tibble(cbind(df4,PI,CI))

ggplot(results,mapping=aes(x=cooked)) + 
  geom_point(aes(y=lwr_p),color='#ffa384',shape=1) + geom_line(aes(y=lwr_p),color='#ffa384') +
  geom_point(aes(y=upr_p),color='#ffa384',shape=1) + geom_line(aes(y=upr_p),color='#ffa384') + 
  geom_point(aes(y=lwr_c),color='#74bdcb',shape=2) + geom_line(aes(y=lwr_c),color='#74bdcb') + 
  geom_point(aes(y=upr_c),color='#74bdcb',shape=2) + geom_line(aes(y=upr_c),color='#74bdcb') +
  geom_point(aes(y=fit_p)) + geom_line(aes(y=fit_p)) + 
  theme_cowplot()

### [explanation of difference]

# PART 2
### [explanation]