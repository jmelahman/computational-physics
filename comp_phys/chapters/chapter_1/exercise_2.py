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
import os

from comp_phys.library import quadrature


def my_function(x):
    """
    Function to be evaluated.
    Input:  x -- independent variable
    Output: y -- dependent variable
    """
    return math.exp(x)


def main():
    """Executes exercise 1.2"""

    file_directory = os.path.dirname(os.path.realpath(__file__))
    output_filepath = os.path.join(file_directory, 'output/exercise_2.txt')

    a = 0.0                                         # lower bound
    b = 1.0                                         # upper bound
    N_list = [4, 8, 16, 32, 64, 128]                # number of lattices

    # The exact solution is the antiderivative (exp.math(x) in this case)
    # evaluated at the upper bound minus the value at the lower bound.
    exact = math.exp(b) - math.exp(a)

    with open(output_filepath, 'w+') as out_file:
        for N in N_list:
            # Step size is the range of the area of integration divided by number of lattices
            h = (b - a) / N

            error1 = quadrature.trapezoidal(my_function, a, h, N) - exact
            error2 = quadrature.simpsons(my_function, a, h, N) - exact
            error3 = quadrature.booles(my_function, a, h, N) - exact

            # Outputs in a format compatible with LaTex tabular :)
            out_file.write('{0:.5f} & {1:.6f} & {2:.6f} & {3:.6f} & {4:.6f}\n'.format(
                N, h, error1, error2, error3))


if __name__ == '__mian__':
    main()
