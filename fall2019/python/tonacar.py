"""This script takes input two integer numbers and construct a tonacar:)"""

h = int(input())
w = int(input())

for i in range(1,h+1):
    print(" "*(h-i) + "* "*i)

for j in range(w):
    print("  " + "* "*(h-2))

