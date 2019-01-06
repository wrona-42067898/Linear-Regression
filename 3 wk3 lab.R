library(statsr)
library(dplyr)
library(ggplot2)
library(GGally)

data(evals)

sum(evals$score < 3)

expected_len <- sum(evals$score > 4.6) / length(evals$score)

sum(evals$score > 4.6)/length(evals$score) == expected_len

hist(evals$score)

plot(evals$ethnicity ~ evals$gender)

plot(evals$ethnicity ~ evals$gender)

ggplot(data = evals, aes(x = bty_avg, y=score)) +
  geom_jitter() +
  geom_smooth(method="lm")

slm = lm(score ~ bty_avg, data=evals)

summary(slm)

qqnorm(slm$residuals)
qqline(slm$residuals)

hist(slm$residuals)

ggplot(data=slm, aes(x=.fitted, y=.resid)) + 
  geom_jitter() +
  geom_hline(yintercept=0, linetype="dashed") +
  xlab("Fitted values") +
  ylab("Residuals")


##Multiple Regression Start##

ggplot(data = evals, aes(x = bty_f1lower, y=bty_avg)) +
  geom_jitter()

evals %>% summarise(cor(bty_avg, bty_f1lower))


ggpairs(evals, columns = 13:19)

m_bty_gen <- lm(score ~ bty_avg + gender, data = evals)
summary(m_bty_gen)


m_bty_rank <- lm(score ~ bty_avg + rank, data = evals)

summary(m_bty_rank)


#Predict the score of Professir Dr Hypo Thetical using m_bty_gen
newprof <- data.frame(gender = "male", bty_avg = 3)

#Could plug this into our model manually, or use the predict() function
predict(m_bty_gen, newprof)

#Uncertainty of this prediction
predict(m_bty_gen, newprof,interval = "prediction", level=0.95)
#"The model predicts with 95% confidence that a male with a beauty score of 3
#is expected to have an evaluation score between 3.1 and 5.18
