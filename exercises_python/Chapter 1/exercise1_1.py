"""
Exercise  1.1: Using any function for which you can evaluate the 
derivatives analytically, investigate the accuracy of the formulas
in Table 1.2 for various values of h.

Computational Physics: FORTRAN Version
by Steven E. Koonin and Dawn C. Meredith

Solution by Jamison Lahman, June 3, 2017
"""
import math

def mySin(x):
#This is the function which is to be evaluated.
#Input:  x -- independent variable
#Output: y -- dependent variable
    return math.sin(x)   # y = e^x/x
    

def backward2(myFunc,x,h):
#Performs the backward 2-point method on the function defined by myFunc
#Input:  x -- independent variable
#        h -- step-size
#Output: ans -- dependent variable
    return (myFunc(x+h) - myFunc(x)) / h
    

def forward2(myFunc,x,h):
#Performs the forward 2-point method on the function defined by myFunc
#Input:  x -- independent variable
#        h -- step-size
#Output: ans -- dependent variable
    return (myFunc(x) - myFunc(x-h)) / h

def symmetric3(myFunc,x,h):
#Performs the symmetric 3-point method on the function defined by myFunc
#Input:  x -- independent variable
#        h -- step-size
#Output: ans -- dependent variable
    return(myFunc(x+h) - myFunc(x-h)) / (2*h)
    

def symmetric4(myFunc,x,h):
#Performs the symmetric 4-point method on the function defined by myFunc
#Input:  x -- independent variable
#        h -- step-size
#Output: ans -- dependent variable
    return(-2*myFunc(x-h) - 3*myFunc(x) + 6*myFunc(x+h) - myFunc(x+2*h)) / (6*h)
    

def symmetric5(myFunc,x,h):
#Performs the symmetric 5-point method on the function defined by myFunc
#Input:  x -- independent variable
#        h -- step-size
#Output: ans -- dependent variable
    return (myFunc(x-2*h) - 8*myFunc(x-h) + 8*myFunc(x+h) - myFunc(x+2*h)) / (12*h)

##########################################################################################
#                                    Begin main
##########################################################################################
#Initializes the constants
val = 1
h = [.5,.2,.1,.05,.02,.01,.005,.002,.001,.0005,.0002,.0001,.00005,.00002,.00001]
exact = math.cos(val)
#Opens file to output
fout = open('exercise1_1.txt', 'w+')

for i in range(len(h)):
	#Evaluates the error for each method
    error1 = backward2(mySin, val, h[i]) - exact
    error2 = forward2(mySin, val, h[i]) - exact
    error3 = symmetric3(mySin, val, h[i]) - exact
    error4 = symmetric4(mySin, val, h[i]) - exact
    error5 = symmetric5(mySin, val, h[i]) - exact
	#Outputs in a format compatible with LaTex tabular :)
    fout.write(''.join( ('%.5f'%h[i],'&','%.6f'%error1,'&','%.6f'%error2,'&','%.6f'%error3,'&','%.6f'%error4,'&','%.6f'%error5,'\\\ \n') ))
    
fout.close()
