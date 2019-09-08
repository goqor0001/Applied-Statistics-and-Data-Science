"""This script finds prime numbers by the algorithm of eratosfen"""

number = input("enter a number\n")

try:
    n = int(number)
    
    l = [True]*(n+1)
    l[0] = False
    l[1] = False

    index = 2
    while index**2 <= n:
        if l[index]:
            for k in range(2, n//index + 1):
                l[k*index] = False
        index += 1
    
    prime_numbers = []
    for i in range(n+1):
        if l[i]:
            prime_numbers.append(i)
    print(prime_numbers)
except ValueError:
    print("sorry your number is not an integer")

