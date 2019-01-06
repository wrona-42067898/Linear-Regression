###Linear Regression 2 discrete variables###

library(statsr)
library(dplyr)
library(ggplot2)

data(mlb11)

#Ask whether variable x predicts variable y (y is response variable)

#Scatter plot with linear model overlayed
ggplot(data = mlb11, aes(x=runs, ?=new_onbase)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

#Save the linear model information in a variable, can be accessed with summary()
my_lm <- lm(runs ~ new_onbase, data = mlb11)

#Check 1) Plot of the residuals, are they uniformly di?tributed? 
ggplot(data=my_lm, aes(x=.fitted, y=.resid)) + 
  geom_point() +
  geom_hline(yintercept=0, linetype="dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

#Check 2) Is the histogram of residuals approximately normal?
ggplot(data = my_lm, ae?(x = .resid)) +
  geom_histogram(binwidth = 25) +
  xlab("Residuals")

#Check 3) Does QQ plot suggest a linear relationship?
ggplot(data = my_lm, aes(sample = .resid)) +
  stat_qq()
########################################################

summary(my_lm)
#new_obs R2=0.9349
#new_slug R2=0.8969
#new_onbase R2=0.8491



#Example of comparing 4 different variables to see the best predictor of runs
summary(lm(runs ~ at_bats, data = mlb11))$r.squared
summary(lm(runs ~ hits, data = mlb11))$r.squared
summary(lm(runs?~ wins, data = mlb11))$r.squared
summary(lm(runs ~ bat_avg, data = mlb11))$r.squared


