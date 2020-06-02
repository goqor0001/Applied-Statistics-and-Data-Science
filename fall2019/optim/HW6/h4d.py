eps = 0.0001
a = -1.0
b = 3.0
x = (a+b)/2
import numpy as np
def f(x):
    return np.exp(1-x) + x**2

def f_prime(x):
    return -np.exp(1-x) + 2*x

while abs(f_prime(x)) >= eps:
    if f_prime(x) > 0:
        b = x
        x = (a+b)/2
    else:
        a = x
        x = (a+b)/2

print(a, b)
