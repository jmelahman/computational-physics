"""
Exercise  1.5: Write programs to solve for the positive root of x^5-5 using the
Newton-Raphson and secant methods. Investigate the behavior of the latter with 
changes in the initial guesses for the root.

Computational Physics: FORTRAN Version
by Steven E. Koonin and Dawn C. Meredith

Solution by Jamison Lahman, August 5, 2018
"""
import math
from math import fabs

def myFunc(x):
#Input:  x -- independent variable
#Output: y -- dependent variable
    y = x*x-5
    return(y)

def myFuncPrime(x):
#Input:  x -- independent variable
#Output: y -- dependent variable
    y = 2.0*x
    return(y)
    
def Search(x,tolX):    
#Uses a binary search to find the root
#Input:  x -- independent variable
#Output: y -- dependent variable

    
def NewtonRaphson(x,tolX):
#Performs the Newton-Raphson for root finding
#Input:  x -- independent variable
#Output: y -- dependent variable
    
def Secant(x,tolX):
#Performs the secant method for root finding
#Input:  x -- independent variable
#Output: y -- dependent variable
    x_1 = x + 1
    while (math.fabs(myFunc(xNew)) > tolX)
        iter = iter + 1
        xOld = x_1                  #Placeholder while x_1 updates
        x_1 = x_1 - myFunc(x_1)*(x_1-x)/(myFunc(x_1)-myFunc(x))
        x = xOld
        
        
######################################################################################
#                               Begin main
######################################################################################
#Declaring constants
xGuess = 1.0                                    #lower bound
tolX = 0.000001                                 #tolerance of x from 0

iter = 0

#Opens file to output
fout = open('exercise1_5.txt','w+')
while ()
search(x,tolx)

iterSecant = 
#Outputs in a format compatible with LaTex tabular :)
fout.write(''.join( ('%f','&','%.5f'%h,'&','%.6f'%error1,'&','%.6f'%error2,'&','%.6f'%error3,'\\\ \n') ))
    
for i in range(len(N)):
fout.close()
  