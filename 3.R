# EX_1

x = c(4.05, 4.01, 4.02, 4.04, 3.99, 4.02, 4.01, 3.99, 3.97, 3.98, 3.97, 3.95, 4.00, 4.02, 3.99, 4.01)
length(x)
may = gl(4, 4, length = 16, labels = c('may1', 'may2', 'may3', 'may4'))
may
dat = data.frame(x, may)
dat
av =  aov(x~may)
summary(av)
TukeyHSD(av)
kruskal.test(x~may)

# library('pgirmess')

kruskalmc(x, may)

# EX_4

loaixang = gl(3, 2, length = 18, labels = c('x1', 'x2', 'x3'))
chatphugia = gl(3, 6, length = 18, labels = c('A', 'B', 'c'))
x = c(124.1, 124.8, 126.4, 127.2, 127.2, 124.9, 131.5, 131.6, 130.6, 142.1, 132.7, 133.0,
     127,126.6, 128.4, 142.6, 125.6, 120.9)
dat = data.frame(x, loaixang, chatphugia)
dat
summary(aov(x~loaixang+chatphugia))
k = kruskal.test(x~chatphugia)
k
kruskalmc(x, chatphugia)


#EX_6

x = 500*c(0.93, 0.05, 0.02)
x
y = c(458, 30, 12)
chisq.test(x,y)
qchisq(0.975, 2)

#EX_7

chisq.test(c(4,3,3), c(180/400, 100/400, 120/400))

# EX_8

dbinom(0, 5, 0.4)
dbinom(1, 5, 0.4)
dbinom(2, 5, 0.4)
dbinom(3, 5, 0.4)
dbinom(4, 5, 0.4)
dbinom(5, 5, 0.4)

chisq.test(c(10/140, 41/140, 60/140, 20/140, 6/140, 3/140), 
           c(dbinom(0, 5, 0.4), dbinom(1, 5, 0.4), dbinom(2, 5, 0.4),
             dbinom(3, 5, 0.4),dbinom(4, 5, 0.4),dbinom(5, 5, 0.4))
           )

# EX_9

y = c(dpois())

dpois(0.95, 1)

#EX_12

x = c(63.1, 67.1, 65.5, 68.0, 66.6, 65.7, 69.2, 67.0, 65.2, 60.7)
y = c(57.4, 66.4, 61.8, 65.3, 63.5, 66.4, 64.9, 65.2, 65.1, 62.2)

t.test(x,y, paired = TRUE)
wilcox.test(x,y,paired = TRUE)