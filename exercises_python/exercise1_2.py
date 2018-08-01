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
#Performs the trapezoidal rule (equation 1.9). The average of the function evaluated at
#the end points of the lattice multiplied by the change in the independent variable.
#Input:  x -- lower bound of the range of integration
#        h -- step-size
#        N -- number of lattices
#Output: ans -- approximate integral    
    sum = 0
    for i in range(N):
        sum = sum + (myFunction(x+i*h)+myFunction(x+(i+1)*h)) * h / 2.0
    return sum 
    
def simpsons(x,h,N):
#Performs Simpons rule (equation 1.12).
#Input:  x -- lower bound of the range of integration
#        h -- step-size
#        N -- number of lattices
#Output: sum -- approximate integral
    
#Add the contribution from the first and last points of the domain
    sum = myFunction(x) + myFunction(x+N*h)
    for i in range(N-1):                        #N-1 ignores last point
        j = i+1                                 #i+1 ignores first point
#Adds the contribution from the even placed lattice points        
        if (j % 2 == 1):
            sum = sum + 4.0*myFunction(x+j*h)
#Adds the contribution from the odd placed lattice points            
        else:
            sum = sum + 2.0*myFunction(x+j*h)                    
    sum = sum*h/3.0                             #Apply leading factor
    return sum

def booles(x,h,N):
#Performs Bode's rule (equation 1.13b)
#Input:  x -- lower bound of the range of integration
#        h -- step-size
#        N -- number of lattices
#Output: sum -- approximate integral 
#Add the contribution from the first and last points of the domain
    sum = 7.0*(myFunction(x) + myFunction(x+N*h))
    for i in range(N-1):                        #N-1 ignores last point
        j = i+1                                 #i+1 ignores first point
#Adds the contribution from the even placed lattice points        
        if (j % 2 == 1):
            sum = sum + 32.0*myFunction(x+j*h)
        elif(j % 4 == 2):
            sum = sum + 12.0*myFunction(x+j*h)
#Adds the contribution from the odd placed lattice points            
        else:
            sum = sum + 14.0*myFunction(x+j*h)                    
    sum = sum * 2.0 * h / 45.0                  #Apply leading factor
    return sum

######################################################################################
#                               Begin main
######################################################################################
#Declaring constants
a = 0.0                                         #lower bound
b = 1.0                                         #upper bound
N = [4,8,16,32,64,128]                          #number of lattices

#The exact solution is the antiderivative (exp.math(x) in this case) 
#evaluated at the upper bound minus the value at the lower bound.  
exact = math.exp(b) - math.exp(a)

#Opens file to output
fout = open('exercise1_2.txt','w+')
for i in range(len(N)):

#Step size is the range of the area of integration divided by number of lattices
    h = (b-a)/N[i]
    error1 = trapezoidal(a, h, N[i]) - exact
    error2 = simpsons(a, h, N[i]) - exact
    error3 = booles(a, h, N[i]) - exact
    #Outputs in a format compatible with LaTex tabular :)
    fout.write(''.join( ('%f'%N[i],'&','%.5f'%h,'&','%.6f'%error1,'&','%.6f'%error2,'&','%.6f'%error3,'\\\ \n') ))
    
fout.close()
    