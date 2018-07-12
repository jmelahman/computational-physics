"""
Exercise  1.2: Using any function whose definite integral you can compute
analytically, investigate the accuracy of the various quadrature methods
discussed above for different values of h.

The function of choice is exp(x) whose antiderivative is simply exp(x).

Computational Physics: FORTRAN Version
by Steven E. Koonin and Dawn C. Meredith

Solution by Jamison Lahman, June 11, 2018
"""
import math

def myFunction(x):
#This is the function which is to be evaluated.
#Input:  x -- independent variable
#Output: y -- dependent variable
    y = math.exp(x)
    return(y)

def trapezoidal(x,h,N):
#Performs the trapezodial rule (equation 1.9)
#Input:  x -- lower bound of the range of integration
#        h -- step-size
#        N -- number of lattices
#Output: ans -- approximate integral    
    for i in N-2:
        sum = sum + (myFunction(x)-2*myFunction(x+i*h)+myFunction(x+2*i*h))/2/h
    return sum 

def simpsons(x,h,N):
#Performs the Simpons rule (equation 1.12)
#Input:  x -- lower bound of the range of integration
#        h -- step-size
#        N -- number of lattices
#Output: sum -- approximate integral
 
#Add the contribution from the first and last points on the lattice points
    sum = myFunction(x) + myFunction(x+N*h)
    for i+1 in N-1:
    j = i+1
#Adds the contribution from the even placed lattice points        
        if (math.mod(j,2) == 1)
            sum = sum + 4.0*myFunction(x+j*h)
#Adds the contribution from the odd placed lattice points            
        else 
            sum = sum + 2.0*myFunction(x+j*h)        
    sum = sum*h/3                       #Apply leading coeffecient
    return sum

def bodes(x,h,N):
#Performs the Simpons rule (equation 1.12)
#Input:  x -- lower bound of the range of integration
#        h -- step-size
#        N -- number of lattices
#Output: sum -- approximate integral 
    ans = (myFunction(x+h)-myFunction(x-h))/(2*h)
    print(ans)

######################################################################################
#Declaring constants
a = 0                                           #lower bound
b = 1                                           #upper bound
h = [0.25,.125,.0625,.03125,.015625,.0078125]   #step sizes
#The exact solution is the antiderivative (exp.math(x) in this case) 
#evaluated at the upper bound minus the value at the lower bound.  
exact = math.exp(b) - math.exp(a)

#Opens file to output
fout = open('exercise1_2.txt','w')
for i in range(len(h)):
    N = (b-a)/h[i]                              #by definition
    error1 = trapezoidal(x, h[i], N) - exact
    error2 = simpsons(x, h[i], N) - exact
    error3 = bodes(x, h[i], N)- exact
    #Outputs in a format compatible with LaTex tabular :)
    fout.write(''.join( (N,'&','%.5f'%h[i],'&','%.6f'%error1,'&','%.6f'%error2,'&','%.6f'%error3,'\\\ \n') ))
    
fout.close()
    