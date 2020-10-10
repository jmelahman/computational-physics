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

from lib import quadrature
from lib import definitions

def my_function(x):
    """
    Function to be evaluated.
    Input:  x -- independent variable
    Output: y -- dependent variable
    """

    return math.exp(x)

def create_output():
    output = ''
    upper_bound = 1.0
    lower_bound = 0.0
    lattices = [4, 8, 16, 32, 64, 128]

    # The exact solution is the antiderivative (exp.math(x) in this case)
    # evaluated at the upper bound minus the value at the lower bound.
    exact = math.exp(upper_bound) - math.exp(lower_bound)

    for N in lattices:
        h = definitions.get_step_size(upper_bound, lower_bound, N)

        trapezoidal_solution = quadrature.trapezoidal(upper_bound,
            lower_bound, my_function, N)
        error_trapezoidal = trapezoidal_solution - exact

        simpsons_solution = quadrature.simpsons(upper_bound, lower_bound,
            my_function, N)
        error_simpsons = simpsons_solution - exact

        booles_solution = quadrature.booles(upper_bound, lower_bound,
            my_function, N)
        error_booles = booles_solution - exact

        # Outputs in a format compatible with LaTex tabular :)
        output += '{0} & {1:.7f} & {2:.6f} & {3:.6f} & {4:.6f}\n'.format(
            N, h, error_trapezoidal, error_simpsons, error_booles)

    return output

def main():
    """Executes exercise 1.2"""

    file_directory = os.path.dirname(os.path.realpath(__file__))
    output_filepath = os.path.join(file_directory, 'output/exercise_2.txt')

    output = create_output()

    with open(output_filepath, 'w+') as out_file:
        out_file.write(output)

if __name__ == '__mian__':
    main()
