#1.c
x = c(25, -10, 3, 1, 2, 8, 4, 0, -1, 7, 7, 2, -1, 2, -6, 5, 0)
boxplot(x, col="green", horizontal = T)

#1.d
boxplot(Petal.Width~Species, data=iris, horizontal = T)

#2.b
quant = function (x, alpha) {
  x_sort = sort(x)
  ind = floor(alpha*length(x))
  if(ind == 0) {
    return (x_sort[1])
  }
  return (x_sort[ind])
}

quant(x, 0.15)

#3.c
q = seq(from=0.1, to=0.9, by=0.1)
qnorm(q)

al = seq(from=0.01, to=0.99, by=0.01)
plot(al, qnorm(al))
par(new=T)
plot(1-al, qnorm(al))
#Symetry comes from standard distribution PDF symietric on line y=0

b <- qnorm(0.995)
a <- -b

#4.b
ex = rexp(200, rate=3)
ex1 = rexp(400, rate=0.2)
qqplot(ex, ex1)

qqexp = function(x) {
  qqplot(x, qexp(1))
}

qqunif = function(x) {
  qqplot(x, qunif())
}


