# Chapter II

data = read.csv('D:/Data Science/Data Science Core/Exercises/R/Statistic Inference, Tung PhD/2_1.csv')
plot(data$x)
plot(data$y)
plot(data$x ~ data$y)
hist(data$x ~ data$y)
summary(data$x)
summary(data$y)
t.test(data$x, data$y)
shapiro.test(data$x)
shapiro.test(data$y)
mean_0 = sum(data$x+data$y)/40
s0 =sum((data$x-mean_0)^2 + (data$y-mean_0)^2)
s0
mean_11 = sum(data$x)/20
mean_12 = sum(data$y)/20
mean_11

s1 =sum((data$x-mean_11)^2 + (data$y-mean_12)^2)
s1
qf (0.95, df1 = 1, df2 = 38)    

# Exercise 2.2

data = read.csv('D:/Data Science/Data Science Core/Exercises/R/Statistic Inference, Tung PhD/2_2.csv')
shapiro.test(data$x)
shapiro.test(data$y)
t.test (data$x, data$y)
mean_11 = sum(data$x)/20
mean_12 = sum(data$y)/20
print(mean_12)
s1 = (sum(data$x-data$y))^2
s1
s0 = 20*sum((data$x-data$y)^2) - s1
s0
(s1/s0)*19
qf (0.95, df1 = 1, df2 = 19) 

# Exercise 3.6

data = read.csv('D:/Data Science/Data Science Core/Exercises/R/Statistic Inference, Tung PhD/3_6.csv'
                )
data$y = data$y*100000/data$n


right = function(b0, b1){
  neta = b0 + b1*data$i
  xt_w_x = matrix(data = c(sum(exp(neta)), sum(data$i*exp(neta)),
                           sum(data$i*exp(neta)), sum(data$i^2*exp(neta))), nrow = 2, ncol = 2)
  xt_w_z = matrix(data = c(sum(neta*exp(neta) + data$y - exp(neta)), 
                           sum(data$i*(neta*exp(neta) + data$y - exp(neta)))
                           ), nrow = 2, ncol = 1)
  
  beta = inv(xt_w_x) %*% xt_w_z
  beta
}

b = function(b0, b1){
  while ((abs(right(b0, b1)[1] - b0) >10e-4) | (abs(right(b0, b1)[2] - b1) > 10e-4)){
    b0 = right(b0, b1)[1]
    b1 = right(b0, b1)[2]
  } 
  a = c(b0, b1)
  a
}

b(1,1)

deviance = function(){
  neta = 2.689 + 0.495*data$i
  d = 2*sum(data$y*log(data$y/exp(neta)) - (data$y - exp(neta)))
  d
}

deviance()

model = glm(formula = data$y*data$n~data$i, family = poisson(link = "log"), offset = log(data$n))
summary(model)

# Exercise 4_1

data = read.csv('D:/Data Science/Data Science Core/Exercises/R/Statistic Inference, Tung PhD/4_1.csv')
plot(data$x, data$y)
par(new = TRUE)
lines(log(data$x), log(data$y))
data$x = log(data$x)

get_matrix_right = function(b0, b1){
  neta = b0 + b1*data$x
  xt_w_x = matrix (data = c(sum(exp(neta)), sum(exp(neta)*data$x),
                           sum(exp(neta)*data$x), sum(exp(neta)*data$x^2)), 
                           nrow = 2, ncol = 2)
  xt_w_z = matrix(data = c(sum(neta*exp(neta) + data$y - exp(neta)), 
                           sum(data$x*(neta*exp(neta) + data$y - exp(neta)))),
                           nrow = 2, ncol = 1)
  
  beta = inv(xt_w_x) %*% xt_w_z
  beta
}

get_b = function(b0, b1){
  while ((abs(get_matrix_right(b0, b1)[1] - b0) >10e-4) | (abs(get_matrix_right(b0, b1)[2] - b1) > 10e-4)){
    b0 = get_matrix_right(b0, b1)[1]
    b1 = get_matrix_right(b0, b1)[2]
  } 
  a = c(b0, b1)
  a
}

get_b (1,1)

get_deviance = function(){
  neta = 0.996 + 1.327*data$x
  d = 2*sum(data$y*log(data$y/exp(neta)) - (data$y - exp(neta)))
  d
}

get_deviance()

qchisq(0.95, 18)

model = glm(formula = data$y ~ data$x, family = poisson(link = "log"))
summary(model)

# Exercise 4_2

data = read.csv('D:/Data Science/Data Science Core/Exercises/R/Statistic Inference, Tung PhD/4_2.csv')
plot(data$x, data$y)

get_matrix_right = function(b0, b1){
  neta = b0 + b1*data$x
  xt_w_x = matrix (data = c(length(data$x), sum(data$x),
                            sum(data$x), sum(data$x^2)), 
                   nrow = 2, ncol = 2)
  xt_w_z = matrix(data = c(sum(neta + data$y/exp(neta) -1),
                           sum(data$x*(neta+data$y/exp(neta)-1))),
                  nrow = 2, ncol = 1)
  
  beta = inv(xt_w_x) %*% xt_w_z
  beta
}

get_b = function(b0, b1){
  while ((abs(get_matrix_right(b0, b1)[1] - b0) >10e-4) | (abs(get_matrix_right(b0, b1)[2] - b1) > 10e-4)){
    b0 = get_matrix_right(b0, b1)[1]
    b1 = get_matrix_right(b0, b1)[2]
  } 
  a = c(b0, b1)
  a
}

get_b (1,1)

get_deviance = function(){
  neta = 8.477 - 1.109*data$x
  #neta = 4.135
  d = 2*sum(data$y/exp(neta) - log(data$y/exp(neta)) -1)
  d
}

get_deviance()

qchisq(0.95, 15)

# Standardized Residual

y_est = exp(8.477 - 1.109*data$x)
r = (data$y - y_est)/y_est
r

model = glm(formula = data$y ~ data$x, family = Gamma (link = "log"))
summary(model)

# + Exercise 5.4
log(sum(data$y/length(data$y)))

# Exercise 5.1

qchisq(0.95, 1)

# Exercise 5.4

qnorm(0.975)
qf(0.95, 1,16)

# Exercise 6_1 + 6_2

data = read.csv('D:/Data Science/Data Science Core/Exercises/R/Statistic Inference, Tung PhD/6_1.csv')
shapiro.test(data$y)
plot(data$x, data$y1)
data
get_matrix_right = function(){
  
  xt_x = matrix (data = c(length(data$x), sum(data$x), 
                          sum(data$x), sum(data$x^2)), 
                   nrow = 2, ncol = 2)
  xt_y = matrix(data = c(sum(data$y1), sum(data$x*data$y1)),
                  nrow = 2, ncol = 1)
  
  beta = inv(xt_x) %*% xt_y
  beta

}

get_matrix_right()

model_1 = lm(data$y1 ~ data$x)
summary(model_1)
model_2 = lm(data$y2 ~ data$x)
summary(model_2)

get_deviance = function() {
  d1 = t(matrix(data$y1)) %*% matrix(data$y1) - sum(data$y1*(39.573 - 4.883*data$x))
  d2 = t(matrix(data$y2)) %*% matrix(data$y2) - sum(data$y2*(13.873 + 3.617*data$x))
  
  (d1-d2)/d2
  
}

get_deviance()
qt(0.975, 5)
plot(data$x, data$y1 + data$y2)

# Exercise 6_2

data = read.csv('D:/Data Science/Data Science Core/Exercises/R/Statistic Inference, Tung PhD/6_2.csv')
shapiro.test(data$y)
plot(data$k, data$y)
data
get_matrix_right = function(){
  
  xt_x = matrix (data = c(length(data$k), sum(data$k), 
                          sum(data$k), sum(data$k^2)), 
                 nrow = 2, ncol = 2)
  xt_y = matrix(data = c(sum(data$y), sum(data$k*data$y)),
                nrow = 2, ncol = 1)
  
  beta = inv(xt_x) %*% xt_y
  beta
}

get_matrix_right()

model_1 = lm(data$y ~ data$k)
summary(model_1)
model_2 = lm(y ~ k +  I(k^2), data)
summary(model_2)

cor.test(data$k, data$y)
cor.test(data$k^2, data$y)
    
# Exercise 6_3

data = read.csv('D:/Data Science/Data Science Core/Exercises/R/Statistic Inference, Tung PhD/6_3.csv')
data = na.omit(data)
shapiro.test(data$y)
plot(data$x1, data$y)
plot(data$x2, data$y)
plot(data$x3, data$y)
cor.test(data$x1, data$y)
cor.test(data$x2, data$y)
cor.test(data$x3, data$y)
cor.test(data$x1,data$x2)
cor.test(data$x1, data$x3)
cor.test(data$x2, data$x3)

model_1 = lm(y~ x1+x2+x3, data)
summary(model_1)
model_2 = lm(y~ x2 + x3, data)
summary(model_2)
model_3 = lm(y~ x3, data)
summary(model_3)

sum((data$y - 12.479 - 1.58*data$x3)^2)
qf(0.95, 1, 17)

# Exercise 6_4

data = read.csv('D:/Data Science/Data Science Core/Exercises/R/Statistic Inference, Tung PhD/6_4.csv')

model_1 = lm(y ~ age, data)
summary(model_1)
model_2 = lm(y ~ age + bmi, data)
summary(model_2)
sum((data$y + 0.74 - 0.04*data$age - 0.201*data$bmi)^2)
qf(0.95, 1, 27)

# Exercise 6_5

data = read.csv('D:/Data Science/Data Science Core/Exercises/R/Statistic Inference, Tung PhD/6_5.csv')
data = na.omit(data)
data
shapiro.test((data$y1))

group = gl(3, 8, length = 24, labels = c('group1', 'group2', 'group3'))
y = c(as.vector(data$y1), as.vector(data$y2), as.vector(data$y3))
dat = data.frame(y, group)
dat
av = aov(y ~ group)
summary(av)
TukeyHSD(av)

t.test(data$y1, data$y2, conf.level = 0.95)
model = lm(data$y3~data$y2)
summary(model)
shapiro.test(data$y2)
hist(data$y3)

# Exercise 6_6

data = read.csv('D:/Data Science/Data Science Core/Exercises/R/Statistic Inference, Tung PhD/6_6.csv')

workers = gl(4, 10, length = 40, labels = c ('group1', 'group2', 'group3', 'group4'))
day = gl(2, 5, length = 40, labels = c('day1', 'day2'))
x = with(data, c(as.vector(x1),as.vector(x2), as.vector(x3), as.vector(x4)))
x
dat = data.frame(x, workers, day)
dat
av = aov(lm(x ~ workers + day ))
summary(av)
TukeyHSD(av)

# Exercise 6_7

data = read.csv('D:/Data Science/Data Science Core/Exercises/R/Statistic Inference, Tung PhD/6_7.csv')
factor_b = gl(2, 6, length = 12, labels =  c('b1', 'b2'))
factor_a = gl(3, 1, length = 12, labels = c('a1', 'a2', 'a3'))
y = c(as.vector(data$b1), as.vector(data$b2))
dat = data.frame(y, factor_b, factor_a)
dat
model_1 = glm(y~factor_a + factor_b + factor_a * factor_b, data = data, family = gaussian)
summary(model_1)

# Exercise 6_8

data = read.csv('D:/Data Science/Data Science Core/Exercises/R/Statistic Inference, Tung PhD/6_8.csv')
factor_b = gl(2, 3, length = 6, labels =  c('b1', 'b2'))
factor_a = gl(3, 1, length = 6, labels = c('a1', 'a2', 'a3'))
y = c(as.vector(data$b1), as.vector(data$b2))
dat = data.frame(y, factor_b, factor_a)
dat
model_1 = glm(y~factor_a + factor_b, data = data, family = gaussian)
summary(model_1)


# Exercise 7_1

data = read.csv('D:/Data Science/Data Science Core/Exercises/R/Statistic Inference, Tung PhD/7_1.csv')
data
model_1 = glm(cbind(y, n-y)~x, family = binomial (link = "logit"), data = data)
summary(model_1)
model2 = glm(cbind(y, n-y)~x, family = binomial (link = "probit"), data = data)
model3 = glm(cbind(y, n-y)~x, family = binomial (link = "cloglog"), data = data)
plot(I(y/n) ~ x, data = data)
lines(fitted(model1)~x, data = data)
lines(fitted(model2)~x, data = data2)
lines(fitted(model3)~x, data = data2)
cbind(data2$y, data2$n-data2$y)

neta = -3.489 + 0.014*data$x
y_   =  data$n * exp(neta)/(1+exp(neta))
Deviance = 2*sum(data$y*log(data$y/y_) + (data$n-data$y)*log((data$n-data$y)/(data$n-y_)))
Deviance 
qchisq(0.95, 4)

# Exercise 7_3

data = read.csv('D:/Data Science/Data Science Core/Exercises/R/Statistic Inference, Tung PhD/7_3_1.csv')
data = na.omit(data)
data
rate = with(data, (s+s.1+s.2+s.3)/(t+t.1+t.2+t.3))
data = cbind(data, rate)

data
av = aov(data$rate~data$y)
summary(av)
data$rate
factor(data$y)

pro_0 = data$s/(data$s+data$t)
pro_1 = data$s.1/(data$s.1+data$t.1)
pro_2 = data$s.2/(data$s.2+data$t.2)
pro_3 = data$s.3/(data$s.3+data$t.3)

av_2 = aov(data$y~pro_0+pro_1+pro_2+pro_3)
summary(av_2)

             