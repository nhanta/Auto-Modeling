# EX_1

library("BSDA")
pcb = c(11.2, 12.4, 10.8, 11.6, 12.5, 10.1, 12.2, 10.6)
mean = mean(pcb)
z.test(pcb, mu = mean, sigma.x = 0.08, conf.level = 0.95)

# EX_2

zsum.test(mean.x = 1.2, sigma.x = 0.2, n.x = 20, conf.level = 0.99)
qnorm(0.995)

# EX_3

tsum.test(mean.x = 1.2, s.x = 0.04, n.x = 20, conf.level = 0.99, var.equal = FALSE)

# EX_4

x = c( 330, 322, 345, 328.6, 331, 342, 342.4, 340, 329.7, 334, 326.5, 325.8, 337.5, 327.3, 322.6, 341, 340, 333, 343.3, 331, 341, 329.5, 332.3, 340)
t.test(x, conf.level = 0.99)
sd(x)
mean(x)
qt(0.995, df = 23)

# EX_5

x = c(140, 136, 150, 144, 148, 152, 138, 141, 143, 151)
mean(x)
sd(x)
minuslogL <- function(mu, sigma2){
  -sum(dnorm(x, mean = mu, sd = sqrt(sigma2), log = TRUE))
}
library(stats4)

MaxLikeEst <- mle(minuslogL, start = list(mu = 143, sigma2 = 33))
summary(MaxLikeEst)
library(MASS)

library("EnvStats")
varTest(x,conf.level = 0.99)


# EX_7+8

prop.test(x = 5016, n = 10000, conf.level = 0.99)
prop.test(x = 16, n = 100, conf.level = 0.95)

# EX_9

x = c(8.18, 8.17, 8.16, 8.15, 8.17, 8.21, 8.22, 8.16, 8.19, 8.18)
z.test(x, mu = 8.2, sigma.x = 0.02, conf.level = 0.9)

# EX_10

x = c(210, 198, 195, 202, 197.5, 196, 199, 195.5)
z.test(x, mu = 200, alternative = "l", sigma.x = 5, conf.level = 0.95)

#EX_11

x = c(183, 173, 176, 185, 181, 183, 180, 188, 168, 179, 179, 193, 184, 188, 182, 177, 192, 179, 194, 196)
z.test(x, mu = 178, sigma.x = 7.6)
z.test(x, mu = 178, sigma.x = 7.6, alternative = "g")

# EX_14,15,16

zsum.test(mean.x = 7.2, sigma.x = 1.2, n.x = 16, alternative = "l", mu = 7.6)
qnorm(0.95)
zsum.test(mean.x = 7.2, sigma.x = 1.2, n.x = 16, alternative = "l", mu = 7.6, conf.level = 0.99)
qnorm(0.8)
zsum.test(mean.x = 105, n.x = 20, sigma.x = 5, mu = 100, alternative = "g")
zsum.test(mean.x = 3, n.x = 2500, sigma.x = 1, mu = 2.95, alternative = "g")
zsum.test(mean.x = 19.7, n.x = 25, sigma.x = 1.3, mu = 20, alternative = "l")
zsum.test(mean.x = 19.7, n.x = 25, sigma.x = 1.3, mu = 20, alternative = "l", conf.level = 0.99)
 
x = c(237, 242, 232, 242, 248, 230, 244, 243, 254,262 ,234, 220, 225, 236, 232, 218, 228, 240)

t.test(x, alternative = "g", mu =  240)
qt(0.95,19)
mean(x)
sd(x)

# ex19

zsum.test(mean.x = 0.162, mu = 0.15, sigma.x = 0.04, alternative = "l", n.x = 40,conf.level = 0.99)

# ex 20

x = c(30.1, 32.7, 22.5, 27.5, 27.7, 29.8, 28.9, 31.4, 31.2, 24.3, 26.4, 22.8, 29.1, 33.4, 32.5, 21.7) 
t.test(x, alternative = "g", mu = 30)

# ex 21

x = c(19, 20, 21, 22, 23)
x = rep(x, c(10, 59, 20, 6, 5))
mean(x)
z.test(x, mu = 20, sigma.x = sd(x))
qnorm(0.995)

# ex 24


x = c(3250, 3268, 4302, 3184, 3266, 3297, 3332, 3502, 3064, 3116)
y = c(3094, 3106, 3004, 3066, 2984, 3124, 3316, 3212, 3380, 3018)
t.test(x, y, var.equal = TRUE)
sd(x)
sd(y)
qt(0.975, 18)

# ex25

qnorm(0.97)
zsum.test(sigma.x = 2400, sigma.y = 2200, mean.x = 47700, mean.y = 46400, n.x = 16, n.y = 16)

# ex 27

x = c(6.1, 7.0, 8.2, 7.6, 6.5, 8.4, 6.9, 6.7, 7.4, 5.8, 6.0, 7.1)
y = c(5.2, 7.9, 3.9, 4.7, 5.3, 5.4, 4.2, 6.1, 3.8, 6.3, 4.1, 5.3)
var(x)
var(y)
z.test(x,y,)

# ex 28
x = c(43, 51, 37, 24, 47, 44, 50, 55, 46)
y = c(41, 49, 44, 32, 46, 42, 47, 51, 49)
var(x)
var(y)

# ex 29
prop.test(x = c(30, 40), n = c(110, 150))
qnorm(0.975)

# ex 30
prop.test(x = c(221.25, 336.7), n = c(375, 481), alternative = "l")

# ex 31

x = c(6.2, 5.8, 5.7, 6.3, 5.9, 6.1, 6.2, 5.7)
y = c(6.3, 5.7, 5.9, 6.4, 5.8, 6.2, 6.3, 5.5)
var.test(x, y)

# ex 32

x = c(1.18, 1.07, 1.13, 1.15, 1.14, 1.13, 1.14, 1.13, 1.03, 1.09)
y = c(1.08, 1.05, 1.19, 1.17, 1.21, 1.12, 1.14, 1.14, 1.13, 1.11)

var.test(x, y, conf.level = 0.99)

# ex 22

x = c(5.5, 6, 7, 6, 7.5, 6, 7.5, 5.5, 7, 6.5, 8.5)
y = c(6.5, 6, 8.5, 7, 6.5, 8, 7.5, 6.5, 7.5, 6, 7)
x
y
jitter(x)
jitter(y)
wilcox.test(x, y, paired = TRUE, alternative = "l")

# EX_26

zsum.test(n.x = 55, mean.x = 3.1, sigma.x = sqrt(1.07), 
          n.y = 48, mean.y = 3.3, sigma.y = sqrt(1.01))

#EX_27

x = c(6.1, 7.0, 8.2, 7.6, 6.5, 8.4, 6.9, 6.7, 7.4, 5.8, 6.0, 7.1)
y = c(5.2, 7.9, 3.9, 4.7, 5.3, 5.4, 4.2, 6.1, 3.8, 6.3, 4.1, 5.3)

var(x)
var(y)

wilcox.test(x,y, alternative = "g", paired = TRUE)