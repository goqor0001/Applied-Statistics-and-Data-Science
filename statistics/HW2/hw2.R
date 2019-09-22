#1.c
set.seed(0)
x = sample(1:6, 100, replace = T)

#frequency table of x
hist(x)

#ECDF of x
plot(ecdf(x))

#barplot of x is more appropriate because it gives us the number of every element in dataset
barplot(table(x))



#1.d
exp_data = rexp(1000, rate=0.3)
plot(ecdf(exp_data), xlim = c(0, 20), ylim = c(0, 1))
par(new=T)
x_axis <- seq(0.1, 50, by=0.1)
plot(x_axis, pexp(x_axis, 0.3), type = 'l', , lwd = 2, xlim = c(0, 20), ylim = c(0, 1), col = "green")

#-----------------------------

#r = c(-4.7, 5.7, 5.4, 4.3, 6.1, 5.9, -1.8, -3.8, 2.0, -3.7, 7.6, 8.6, -0.8, 3.3, 8.9, 5.5, 8.9, 2.4, 4.6, 4.5)
#hist(r, breaks=5)
#sort(r)

#2.b
?islands

dput(islands)

head(islands)

less200 <- c()
for(i in names(islands)) {
  
  if(islands[i] < 200) {
    less200 <- c(less200, islands[i])
  }
}
hist(less200, freq=F, xlim = c(0, 200), ylim = c(0,0.02))
rug(less200); par(new=T)
d = density(less200)
plot(d, xlim = c(0, 200), ylim = c(0,0.02), col = "red", lwd=3)

#2.c
n = 1000
set.seed(1)
?rweibull
w_data = rweibull(n, shape=2)
hist(w_data, freq=F, col="cyan", xlim = c(0, 3), ylim = c(0,1))
par(new=T)
wx_axis <- seq(0, 5, by=0.01)
plot(wx_axis, dweibull(wx_axis, shape=2), col="red", lwd=3, type="l", xlim = c(0, 3), ylim = c(0,1))


#2.d
?ChickWeight
head(ChickWeight, 5)
x <- c()
y <- c()
print(ChickWeight$Die[2])
for(i in 1:(length(ChickWeight$Diet))) {
  if(ChickWeight$Die[i] == 1) {
    x <- c(x, ChickWeight$weight[i])
  }
  if(ChickWeight$Die[i] == 2) {
    y <- c(y, ChickWeight$weight[i])
  }
}

library(scales)
hist(x, col = alpha("magenta", 0.2), xlim = c(0, 350), ylim = c(0, 100))
par(new=T)
hist(y, col = alpha("red", 0.5), xlim = c(0, 350), ylim = c(0, 100))
#we can conclude that if Diet = 1 its frequency id higher

