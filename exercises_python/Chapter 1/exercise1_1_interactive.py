"""
Exercise  1.2: 

Computational Physics: FORTRAN Version
by Steven E. Koonin and Dawn C. Meredith

Solution by Jamison Lahman, June 3, 2017
"""
import math

def backward2(x,h):
#Performs the backward 2-point method on the function defined by myFunc
#Input:  x -- independent variable
#        h -- step-size
#Output: ans -- dependent variable
    ans = (myFunc(x+h)-myFunc(x))/h
    return(ans)

def forward2(x,h):
#Performs the forward 2-point method on the function defined by myFunc
#Input:  x -- independent variable
#        h -- step-size
#Output: ans -- dependent variable
    ans = (myFunc(x)-myFunc(x-h))/h
    return(ans)

def symmetric3(x,h):
#Performs the symmetric 3-point method on the function defined by myFunc
#Input:  x -- independent variable
#        h -- step-size
#Output: ans -- dependent variable
    ans = (myFunc(x+h)-myFunc(x-h))/(2*h)
    return(ans)

def symmetric4(x,h):
#Performs the symmetric 4-point method on the function defined by myFunc
#Input:  x -- independent variable
#        h -- step-size
#Output: ans -- dependent variable
    ans = (-2*myFunc(x-h)-3*myFunc(x)+6*myFunc(x+h)-myFunc(x+2*h))/(6*h)
    return(ans)

def symmetric5(x,h):
#Performs the symmetric 5-point method on the function defined by myFunc
#Input:  x -- independent variable
#        h -- step-size
#Output: ans -- dependent variable
    ans = (myFunc(x-2*h)-8*myFunc(x-h)+8*myFunc(x+h)-myFunc(x+2*h))/(12*h)
    return(ans)

##########################################################################################

input_val = input('Enter the value of x: ')
try:
    val = float(input_val)              #Ensures a float is entered
except:
    print('Input was not a float.')
    exit()

input_h = input('Enter the value of h: ')
try:
    h = float(input_h)                  #Ensures a float is entered
except:
    print('Input was not a float.')
    exit()

#Outputs available methods to screen
print('1.Backward 2-point\n2.Forward 2-point\n3.Symmetric 3-point\n4.Symmetric 4-point\n5.Symmetric 5-point')

input_method = input('Which method would you like to use: ')
try:
    method = float(input_method)        #Ensures a float is entered
except:
    print('Invalid method.')
    exit()
    
if method == 1:                         #calls appropiate function
    ans = backward2(val,h)
elif method == 2:
    ans = forward2(val,h)
elif method == 3:
    ans = symmetric3(val,h)
elif method == 4:
    ans = symmetric4(val,h)
elif method == 5:
    ans = symmetric5(val,h)
else:
    print('Invalid method.')
    exit()
    
print(ans)                              #prints answer found above
exact = math.exp(val)*(val-1)/val/val   # e^x(x-1)/x
print('The exact solution is:')
print(exact)
print('The error is:')
print(ans - exact)
