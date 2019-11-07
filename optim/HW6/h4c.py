import numpy as np
delta = 0.000001

def f(x):
    return np.exp(1-x) + x**2

n = 20
fib = [0, 1, 1]
for i in range(20):
    fib.append(fib[-1] + fib[-2])
#print(fib)
a = -1
b = 3
for k in range(n-3):
    gamma = fib[n-2-k]/(fib[n-k]+0.0)
    A = a + gamma*(b-a)
    B = b - gamma*(b-a)
    if f(A) < f(B):
        b = B
    else:
        a = A

print(a, b)

