attach(luciferase_phylo)
plot(distance,expression_fold)
luciferanova1<-lm(distance~expression_fold)
abline(luciferanova1,col="red")
#GLM assumptions
#Data is independent.
#Normality of error:
hist(luciferanova1$residuals)
qqPlot(luciferanova1$residuals)
shapiro.test(luciferanova1$residuals)
#p=0.4197, residuals are normally distributed
#homoscedasticity:line seems even across Residuals vs fitted plot
par(mfrow=c(2,2))
plot(luciferanova1)
#linearity
#data points seem fairly linear in res-fitted despite spread, intial plot also linear