#Collinearity Notes
#2 predictors are collinear when they are correlated with one another
#Predictor variables (i.e. independent variables) should be that - independent of eachother

cognitive = read.csv("http://bit.ly/dasi_cognitive")

cog_full = lm(kid_score ~ mom_hs + mom_iq + mom_work + mom_age, data=cognitive)

summary(cog_full)

#Inference from the model
  #Null hypothesis: each of our slopes = 0
  #Alt hyupothesis: At least one of our slopes is different than 0
#Use F-statistic for this! (Bottom of summary)

#Model Selection based on p-value / R^2

#Linear model suitability check 1
cog_final = lm(kid_score ~ mom_hs + mom_iq + mom_work, data = cognitive)
plot(cog_final$residuals ~ cognitive$mom_iq)
abline(0,0)

#check 2
hist(cog_final$residuals)
qqnorm(cog_final$residuals)
qqline(cog_final$residuals)


#check 3 #fitted values are the predicted values (y hats)
plot(cog_final$residuals ~ cog_final$fitted)
plot(abs(cog_final$residuals) ~ cog_final$fitted)

par(mfrow=c(1,2))

#check 4 #check residuals are independent observations (probably)
#consider how data would have been sampled to plot this
plot(cog_final$residuals)
abline(0,0)


    