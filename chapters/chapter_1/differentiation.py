import math

def backward2(myFunc,x,h):
#Performs the backward 2-point method on the function defined by myFunc
#Input:  x -- independent variable
#        h -- step-size
#Output: dependent variable
    return((myFunc(x+h)-myFunc(x))/h)


def forward2(myFunc,x,h):
#Performs the forward 2-point method on the function defined by myFunc
#Input:  x -- independent variable
#        h -- step-size
#Output: dependent variable
    return((myFunc(x)-myFunc(x-h))/h)

def symmetric3(myFunc,x,h):
#Performs the symmetric 3-point method on the function defined by myFunc
#Input:  x -- independent variable
#        h -- step-size
#Output: dependent variable
    return((myFunc(x+h)-myFunc(x-h))/(2.0*h))


def symmetric4(myFunc,x,h):
#Performs the symmetric 4-point method on the function defined by myFunc
#Input:  x -- independent variable
#        h -- step-size
#Output: dependent variable
    return((-2.0*myFunc(x-h)-3.0*myFunc(x)+6.0*myFunc(x+h)-myFunc(x+2.0*h))/(6.0*h))


def symmetric5(myFunc,x,h):
#Performs the symmetric 5-point method on the function defined by myFunc
#Input:  x -- independent variable
#        h -- step-size
#Output: dependent variable
    return((myFunc(x-2.0*h)-8.0*myFunc(x-h)+8.0*myFunc(x+h)-myFunc(x+2.0*h))/(12.0*h))
