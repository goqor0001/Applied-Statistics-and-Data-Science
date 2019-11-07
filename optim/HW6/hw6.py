eps = 0.00001
gamma = (3-5**0.5)/2

def f(x):
    return x**4 + x**2 -2*x

a = -4
b = 5

while b-a >= eps:
    A = a + gamma*(b-a)
    B = b - gamma*(b-a)
    if f(A) < f(B):
        b = B
    else:
        a = A

print(a, b)
