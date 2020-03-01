"""
Exercise  1.1: Using any function for which you can evaluate the
derivatives analytically, investigate the accuracy of the formulas
in Table 1.2 for various values of h.

Computational Physics: FORTRAN Version
by Steven E. Koonin and Dawn C. Meredith

Solution by Jamison Lahman, June 3, 2017
"""
import math

import chapters.chapter_1.differentiation as differentiation

def my_sin(x):
#This is the function which is to be evaluated.
#Input:  x -- independent variable
#Output: dependent variable
    return(math.sin(x))


def main():
    val = 1.0
    h = (.5,.2,.1,.05,.02,.01,.005,.002,.001,.0005,.0002,.0001,.00005,.00002,.00001)
    exact = math.cos(val)
    f = open('chapters/chapter_1/exercise_1.txt', 'w+')

    for i in range(len(h)):
        #Evaluates the error for each method
        error1 = differentiation.backward2(my_sin, val, h[i]) - exact
        error2 = differentiation.forward2(my_sin, val, h[i]) - exact
        error3 = differentiation.symmetric3(my_sin, val, h[i]) - exact
        error4 = differentiation.symmetric4(my_sin, val, h[i]) - exact
        error5 = differentiation.symmetric5(my_sin, val, h[i]) - exact
        #Outputs in a format compatible with LaTex tabular :)
        f.write('{0:.5f} & {1:.6f} & {2:.6f} & {3:.6f} & {4:.6f} & {5:.6f}\n'.format(h[i],error1,error2,error3,error4,error5))

    f.close()

if __name__ == '__main__':
    main()
