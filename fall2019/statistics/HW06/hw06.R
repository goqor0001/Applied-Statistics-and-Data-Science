#4.d

x = c(1.14453951, 0.33225379, 4.00416672, -5.04850837, 5.35766860, 0.06489233,
      -5.94509889, -4.03921700, -1.04401996, -1.24168001, -8.06484372, 1.03773668,
      -2.87971666, -0.71685157, 0.45405920, 3.30095388, 0.74257551, 2.13158422,
      -3.58842411, -2.51157687, -0.05550723, -1.12422861, -4.33075098, -5.02908441,
      -1.91539586, 1.00226791, -0.23604246, -4.60662137, -3.35289235, -3.41713159,
      -6.73744316, -2.45756382, -1.15391642, 1.50038591, 3.66853943, -1.05244316,
      4.30821758, -0.99412514, 3.02116765, 3.14134781, -1.06686798, -5.78622173,
      -1.38023764, -1.87756992, 5.05295459, -0.62753128, 0.31867113, 3.79992962,
      -0.07876558, -5.08016167)

n = length(x)
clm = 0.92
am = 1-clm

t <- qt(1-am/2, df=n-1)
s = sd(x)
mem = t*s/sqrt(n) 

cim = c(mean(x)-mem, mean(x)+mem)
cim

mean(x)

cls = 0.945
as = 1-cls
q1 = qchisq(as, df=n-1)
q2 = qchisq(cls/2, df=n-1)
l = (n-1)*s*s/q1
u = (n-1)*s*s/q2
cis = c(l, u)
cis


#4.e
ci.bernoulli <- function(x, a) {
   
}


#5
curve(dt(x, 30), from = -5, to = 5, col = "orange", 
      xlab = "quantile", ylab = "density", lwd = 2)
curve(dt(x, 10), from = -5, to = 5, col = "dark green", add = TRUE, lwd = 2)
curve(dt(x, 5), from = -5, to = 5, col = "sky blue", add = TRUE, lwd = 2)
curve(dt(x, 1), from = -5, to = 5, col = "grey40", add = TRUE, lwd = 2)
curve(dnorm(x), from=-5, to=5, col='red', add=T, lwd=2)
legend("topleft", legend = paste0("DF = ", c(1, 5, 10, 30)),
       col = c("grey40", "sky blue", "dark green", "orange"),
       lty = 1, lwd = 2)


aa = rt(1000, df=3)
qqnorm(aa)

bb = rt(1000, df=15)
qqnorm(bb)

cc = rt(1000, df=30)
qqnorm(cc)

#We can infer if dwgrees of freedom is large then it is almost the standart normal
curve(dchisq(x, 300), from = -500, to = 500, col = "red", 
      xlab = "quantile", ylab = "density", lwd = 2)
curve(dchisq(x, 30), from = -500, to = 500, col = "orange", add=T
      xlab = "quantile", ylab = "density", lwd = 2)
curve(dchisq(x, 10), from = -500, to = 500, col = "dark green", add = TRUE, lwd = 2)
curve(dchisq(x, 5), from = -500, to = 500, col = "sky blue", add = TRUE, lwd = 2)
curve(dchisq(x, 1), from = -500, to = 500, col = "grey40", add = TRUE, lwd = 2)
legend("topleft", legend = paste0("DF = ", c(300, 1, 5, 10, 30)),
       col = c("red", "grey40", "sky blue", "dark green", "orange"),
       lty = 1, lwd = 2)



a = rnorm(10000)
b = rnorm(10000)
c = rnorm(10000)
d = rnorm(10000)
xx = a**2 + b**2 + c**2 + d**2
plot(density(xx))
curve(dchisq(x, df=4), from = -5, to = 25, add=T, col='red')
#alomost the same


