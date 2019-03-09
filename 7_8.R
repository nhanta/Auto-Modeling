data3 = read.csv(file = "D:/Data Science/Data Science Core/Exercises/R/7_8.csv")
data3
ks.test(data3$s, 'pbinom', length(data3$s), mean(data3$s)/length(data3$s))

model1 =  glm(formula = 's~x', family = binomial (link = "logit"), data = data3 )
model2 =  glm(formula = 's~x', family = binomial (link = "probit"), data = data3 )
model3 =  glm(formula = 's~x', family = binomial (link = "cloglog"), data = data3 )
model1