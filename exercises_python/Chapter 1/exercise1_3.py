"""
Exercise  1.3: Write a program to calculate
\int^1_0 t^{-2/3}(1-t)^{-1/3}dt = 2\pi/3^{1/2}
Using one of the quadrature formulas discussed above and investigate its accuracy
for various values of h. (Hint: Split the range of integration into two parts and 
make a different change of variable in each integral to handle the singularities

Computational Physics: FORTRAN Version
by Steven E. Koonin and Dawn C. Meredith

Solution by Jamison Lahman, August 5, 2018
"""
import math


def trig_sub(t):
#Substitues t=sin^3(t) which is well-behaved over the interval of integration.
#Input:  t -- independent variable
#Output: y -- dependent variable
    y = 3.*(math.cos(t)*((1.+math.sin(t))/(1.+math.sin(t)+math.sin(t)**2.)))**(1./3.)
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
        sum = sum + (trig_sub(x+i*h)+trig_sub(x+(i+1)*h)) * h / 2.0
    return sum 
    
def simpsons(x,h,N):
#Performs Simpons rule (equation 1.12).
#Input:  x -- lower bound of the range of integration
#        h -- step-size
#        N -- number of lattices
#Output: sum -- approximate integral
    
#Add the contribution from the first and last points of the domain
    sum = trig_sub(x) + trig_sub(x+N*h)
    for i in range(N-1):                        #N-1 ignores last point
        j = i+1                                 #i+1 ignores first point
#Adds the contribution from the even placed lattice points        
        if (j % 2 == 1):
            sum = sum + 4.0*trig_sub(x+j*h)
#Adds the contribution from the odd placed lattice points            
        else:
            sum = sum + 2.0*trig_sub(x+j*h)                    
    sum = sum*h/3.0                             #Apply leading factor
    return sum

def booles(x,h,N):
#Performs Bode's rule (equation 1.13b)
#Input:  x -- lower bound of the range of integration
#        h -- step-size
#        N -- number of lattices
#Output: sum -- approximate integral 
#Add the contribution from the first and last points of the domain
    sum = 7.0*(trig_sub(x) + trig_sub(x+N*h))
    for i in range(N-1):                        #N-1 ignores last point
        j = i+1                                 #i+1 ignores first point
#Adds the contribution from the even placed lattice points        
        if (j % 2 == 1):
            sum = sum + 32.0*trig_sub(x+j*h)
        elif(j % 4 == 2):
            sum = sum + 12.0*trig_sub(x+j*h)
#Adds the contribution from the odd placed lattice points            
        else:
            sum = sum + 14.0*trig_sub(x+j*h)                    
    sum = sum * 2.0 * h / 45.0                  #Apply leading factor
    return sum

######################################################################################
#                               Begin main
######################################################################################
#Declaring constants
a = 0.0                                         #lower bound
b = math.pi/2.0                                 #upper bound
N = [4,8,16,32,64,128]                          #number of lattices

#Exact solution is given
exact = 2.0*math.pi/math.sqrt(3)

#Opens file to output
fout = open('exercise1_3.txt','w+')
for i in range(len(N)):

#Step size is the range of the area of integration divided by number of lattices
    h = (b-a)/N[i]
    error1 = trapezoidal(a, h, N[i]) - exact
    error2 = simpsons(a, h, N[i]) - exact
    error3 = booles(a, h, N[i]) - exact
    #Outputs in a format compatible with LaTex tabular :)
    fout.write(''.join( ('%f'%N[i],'&','%.5f'%h,'&','%.6f'%error1,'&','%.6f'%error2,'&','%.6f'%error3,'\\\ \n') ))
    
fout.close()
    