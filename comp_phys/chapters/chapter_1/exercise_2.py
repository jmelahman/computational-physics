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

from chapters.chapter_1 import quadrature

def my_function(x):
    """
    Function to be evaluated.
    Input:  x -- independent variable
    Output: y -- dependent variable
    """
    return math.exp(x)

def main():
    a = 0.0                                         # lower bound
    b = 1.0                                         # upper bound
    N = [4, 8, 16, 32, 64, 128]                     # number of lattices

    #The exact solution is the antiderivative (exp.math(x) in this case)
    #evaluated at the upper bound minus the value at the lower bound.
    exact = math.exp(b) - math.exp(a)

    with open('exercise1_2.txt','w+') as f:
        for i in range(len(N)):
            #Step size is the range of the area of integration divided by number of lattices
            h = (b - a) / N[i]

            error1 = quadrature.trapezoidal(my_function,a, h, N[i]) - exact
            error2 = quadrature.simpsons(my_function, a, h, N[i]) - exact
            error3 = quadrature.booles(my_function, a, h, N[i]) - exact

            #Outputs in a format compatible with LaTex tabular :)
            fout.write('{0:.5f} & {1:.6f} & {2:.6f} & {3:.6f} & {4:.6f} \n'.format(N[i],h,error1,error2,error3))

if __name__ == '__mian__':
    main()
