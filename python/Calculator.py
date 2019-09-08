"""This script is a calculator. You can sum, multiply, divide and subtract numbers"""
first_number = eval(input("enter a first number\n"))
operation = input("enter a valid operation\n")
second_number = eval(input("enter a second number\n"))

if operation == '+':
    print("sum of two numbers equals  ", first_number + second_number)
elif operation == '-':
    print("subtruct of two numbers equals ", first_number - second_number)
elif operation == '*':
    print("subtruct of two numbers equals  ", first_number * second_number)
elif operation == '/':
    if second_number == 0:
        print("sorry you cannot divide by zero(((. ")
    print("subtruct of two numbers equals  ", first_number / second_number)
else:
    print("sorry you entered not valid operation:")
