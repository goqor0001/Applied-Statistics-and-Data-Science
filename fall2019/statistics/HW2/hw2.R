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


#3.c

stem(iris$Petal.Length)
stem(iris$Petal.Length, 0.5)
stem(iris$Petal.Length, 2)
stem(iris$Petal.Length, 4)


par(mfcol = c(1,2))
x3_axis = rnorm(10)
plot.new()
out <- capture.output(stem(iris$Petal.Length), family = "mono")
text(0,1, paste(out, collapse='\n'), adj=c(0,1) )
hist(iris$Petal.Length)

#4.a

x4 = c(0, 3, 4, 5, -1)
y4 = c(2, -1, 2, 5, 2)
plot(x4, y4)

#4.b
plot(pressure$temperature, pressure$pressure)

#5
data5 = read.csv("/home/grigor/Applied-Statistics-and-Data-Science/statistics/HW2/AAPL.csv")
head(data5)
adj = data5$Adj.Close
hist(adj)
#in the 100th week it has a lot of return but after that it is lower

#6.e

wins_mean <- function(x, p) {
  n = length(x)
  if(2*p > n) {
    print("enter correct input")
    return(0)
  }
  y = sort(x)
  for(i in 1:p) {
    y[i] = y[p+1]
    y[n-i+1] = y[n-p]
  }
  return(mean(y))
}

print(wins_mean(c(-100, 3, 5), 1))

#6.f
x6f <- c()
for(i in 1:(length(ChickWeight$Diet))) {
  if(ChickWeight$Die[i] == 1) {
    x6f <- c(x6f, ChickWeight$weight[i])
  }
}
mean(x6f)

x6f2 <- c()
for(i in 1:(length(ChickWeight$Diet))) {
  if(ChickWeight$Die[i] == 2) {
    x6f2 <- c(x6f2, ChickWeight$weight[i])
  }
}
mean(x6f2)
#there is no randomness and we can conclude that first diet is more efficient

#7.d
head(mtcars)

mtcars$cyl
x7d = c()
y7d = c()
z7d = c()

for(i in 1:length(mtcars$cyl)) {
  if(mtcars$cyl[i] == 4) {
    x7d <- c(x7d, mtcars$mpg[i])
  }
  if(mtcars$cyl[i] == 6) {
    y7d <- c(y7d, mtcars$mpg[i])
  }
  if(mtcars$cyl[i] == 8) {
    z7d <- c(z7d, mtcars$mpg[i])
  }
}

sd(x7d)
sd(y7d)
sd(z7d)
#the most is 4 cyl then 8 cyl and the least is 6 cyl

#7.e
head(iris)
max(iris$Petal.Width)
min(iris$Petal.Width)
max_sp <- c()
min_sp <- c()
for(i in 1:length(iris$Petal.Width)) {
  if(iris$Petal.Width[i] == max(iris$Petal.Width)) {
    #print(iris$Species[i])
    max_sp <- c(max_sp, iris$Species[i])
    print(max_sp)
  }
  if(iris$Petal.Width[i] == min(iris$Petal.Width)) {
    min_sp <- c(min_sp, iris$Species[i])
  }
}
print(max_sp)
print(min_sp)

#7.f
mad(cars$dist)
?mad
mad(cars$dist, center = mean(cars$dist))

mad1 <- function(x) {
  x_bar = mean(x)
  mad1 = (sum(abs(x - x_bar)))/length(x)
  return(mad1)
}
mad1(cars$dist)

mad2 <- function(data7) {
  x_bar = median(data7)
  mad1 = (sum(abs(data7 - x_bar)))/length(x)
  return(mad1)
}
mad1(cars$dist)

#they are not the same I do not know why

#8.c
x = c(-6, 15, 0, 5, 17, -4, 1, -9, -9, 13)
y = c(0.0, 3.6, 2.7, -1.5, 5.7, 1.5, -3.0, 4.5, 6.0)
quantile(x)
quantile(y)


quar = function(x) {
   x <- sort(x)
  q1 = 0
  q2 = 0
  q3 = 0
  n = length(x)
  m = floor(n/2)
  if (n%%2 == 0) {
    q2 = (x[m] + x[m+1])/2
    q1 = 0
    q3 = 0
    if (m%%2 == 0) {
      k = floor(m/2)
      q1 = (x[k] + x[k+1])/2
      q3 = (x[m+k] + x[m+k+1])/2
      
    } else {
      k = floor(m/2)
      q1 = x[k+1]
      q3 = x[m+k+1]
    }
  } else {
    q2 = x[m+1]
    q1 = 0
    q3 = 0
    if ((m+1)%%2 == 0) {
      k = floor((1+m)/2)
      q1 = (x[k] + x[k+1])/2
      q3 = (x[m+k] + x[m+k+1])/2
      
    } else {
      k = floor(m/2)
      q1 = x[k+1]
      q3 = x[m+k+1]
    }
  }
  return(c(q1, q2, q3))
  
}
quar(x)
sort(x)
quar(y)
sort(y)
#finaly  endddd:)