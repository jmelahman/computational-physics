"""
Exercise  1.3: Write a program to calculate
\int^1_0 t^{-2/3}(1-t)^{-1/3}dt = 2\pi/3^{1/2}
Using one of the quadrature formulas discussed above and investigate its accuracy
for various values of h. (Hint: Split the range of integration into two parts and 
make a different change of variable in each integral to handle the singularities

Computational Physics: FORTRAN Version
by Steven E. Koonin and Dawn C. Meredith

Solution by Jamison Lahman, August 5, 2018

TODO: u-substitution method
"""
import math


def trig_sub(t):
#Substitues t=sin^3(t) which is well-behaved over the interval of integration.
#Input:  t -- independent variable
#Output: y -- dependent variable
    return(3.0*(math.cos(t)*((1.0+math.sin(t))/(1.0+math.sin(t)+math.sin(t)**2)))**(1.0/3.0))

def trapezoidal(myFunc,x,h,N):
#Performs the trapezoidal rule (equation 1.9). The average of the function evaluated at
#the end points of the lattice multiplied by the change in the independent variable.
#Input:  x -- lower bound of the range of integration
#        h -- step-size
#        N -- number of lattices
#Output: ans -- approximate integral
#JL TODO: This evaluates myFunc twice at x+(i+1)*h then again at x+i_{+}*h. Could be improved 
#to use results from previous iteration.
    return(sum((myFunc(x+i*h)+myFunc(x+(i+1)*h))*h/2.0 for i in range(N)))
    
def simpsons(myFunc,x,h,N):
#Performs Simpons rule (equation 1.12).
#Input:  x -- lower bound of the range of integration
#        h -- step-size
#        N -- number of lattices
#Output: sum -- approximate integral
    
#Add the contribution from the first and last points of the domain
    sum = myFunc(x)+myFunc(x+N*h)
    for i in range(1,N):                        #1,N-1 ignores first and last points
#Adds the contribution from the even placed lattice points        
        if(i%2 == 1):
            sum = sum+4.0*myFunc(x+i*h)
#Adds the contribution from the odd placed lattice points            
        else: sum=sum+2.0*myFunc(x+i*h)                               
    return(sum*h/3.0)                         #Apply leading factor

def booles(myFunc,x,h,N):
#Performs Bode's rule (equation 1.13b)
#Input:  x -- lower bound of the range of integration
#        h -- step-size
#        N -- number of lattices
#Output: sum -- approximate integral 
#Add the contribution from the first and last points of the domain
    sum = 7.0*(myFunc(x)+myFunc(x+N*h))
    for i in range(1,N):                        #N-1 ignores last point
#Adds the contribution from the even placed lattice points        
        if(i%2 == 1):
            sum = sum+32.0*myFunc(x+i*h)
        elif(i%4 == 2):
            sum = sum+12.0*myFunc(x+i*h)
#Adds the contribution from the odd placed lattice points            
        else: sum = sum+14.0*myFunc(x+i*h)                    
    return(sum*2.0*h/45.0)                  #Apply leading factor

#=====================================================================================
#                               Begin main
#=====================================================================================
#Declaring constants
trig_a = 0.0                                    #lower bound
trig_b = math.pi/2.0                            #upper bound
N = (4,8,16,32,64,128)                          #number of lattices

#Exact solution is given
exact = 2.0*math.pi/math.sqrt(3)

#Opens file to output
fout = open('exercise1_3.txt','w+')
for i in range(len(N)):

#Step size is the range of the area of integration divided by number of lattices
    trig_h = (trig_b-trig_a)/N[i]

    trigErrorTrap = trapezoidal(trig_sub,trig_a, trig_h, N[i]) - exact
    trigErrorSimp = simpsons(trig_sub,trig_a, trig_h, N[i]) - exact
    trigErrorBool = booles(trig_sub, trig_a, trig_h, N[i]) - exact
    
    #Outputs in a format compatible with LaTex tabular :)
    fout.write('{0} & {1:.5f} & {2:.6f} & {3:.6f} & {4:.6f} \n'.format(N[i], trig_h, trigErrorTrap, trigErrorSimp, trigErrorBool))
fout.close()
    
