"""
Exercise  1.2: Using any function whose definite integral you can compute
analytically, investigate the accuracy of the various quadrature methods
discussed above for different values of h.

The function of choice is exp(x)/x whose derivative is exactly:
exp(x)(x-1)/x^2. To use a different equation, simply alter myFunction.

Computational Physics: FORTRAN Version
by Steven E. Koonin and Dawn C. Meredith

Solution by Jamison Lahman, June 3, 2017
"""
import math

def myFunction(x):
#This is the function which is to be evaluated.
#Input:  x -- independent variable
#Output: y -- dependent variable
    y = math.exp(x)/x
    return(y)

def trapezoidal(x,h):
#Performs the backward 2-point method on the function defined previously
#Input:  x -- independent variable
#        h -- step-size
#Output: None
    ans = (myFunction(x+h)-myFunction(x))/h
    print(ans)

def simpsons(x,h):
#Performs the forward 2-point method on the function defined previously
#Input:  x -- independent variable
#        h -- step-size
#Output: None
    ans = (myFunction(x)-myFunction(x-h))/h
    print(ans)

def bodes(x,h):
#Performs the symmetric 3-point method on the function defined previously
#Input:  x -- independent variable
#        h -- step-size
#Output: None
    ans = (myFunction(x+h)-myFunction(x-h))/(2*h)
    print(ans)

input_val = input('Enter the value if x: ')
try:
    val = float(input_val)              #Ensures a float is entered
except:
    print('Input was not a float.')
    exit()

input_h = input('Enter the value if h: ')
try:
    h = float(input_h)                  #Ensures a float is entered
except:
    print('Input was not a float.')
    exit()

print('1.Trapezoidal\n2.Simpsons\n3.Bodes')

input_method = input('Which method would you like to use: ')
try:
    method = float(input_method)        #Ensures a float is entered
except:
    print('Invalid method.')
    exit()
    
if method == 1:                         #calls appropiate function
    trapezoidal(val,h)
elif method == 2:
    simpsons(val,h)
elif method == 3:
    bodes(val,h)
#elif method == 4:
#    symmetric5(val,h)
else:
    print('Invalid method.')
    exit()
    
exact = math.exp(val)*(val-1)/val/val
print('The exact solution is:')
print(exact)