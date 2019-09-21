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

