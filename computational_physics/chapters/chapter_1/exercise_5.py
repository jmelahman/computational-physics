"""
Exercise 1.5: Write programs to solve for the positive root of x^2 - 5 using the
Newton-Raphson and secant methods. Investigate the behavior of the latter with
changes in the initial guesses for the root.

Computational Physics: FORTRAN Version
by Steven E. Koonin and Dawn C. Meredith

Solution by Jamison Lahman, August 5, 2018
"""
import itertools
import math
import os

from computational_physics import root_finding

def my_function(x):
    """
    Function to be evaluatedi, x^2 - 5
    Input:  x -- independent variable
    Output: y -- dependent variable
    """

    return math.pow(x, 2.0) - 5.0

def my_function_prime(x):
    """
    The derivative of my_function(), 2x
    Input:  x -- independent variable
    Output: y -- dependent variable
    """

    return 2.0 * x

def calculate_error(value, exact):
    if value:
        return exact - value
    return 0

def main():
    """Executes exercise 1.5"""

    file_directory = os.path.dirname(os.path.realpath(__file__))
    output_filepath = os.path.join(file_directory, 'output/exercise_5.txt')
    initial_guess = 1.0                                # initial guess of root
    x_tolerance = 0.0001
    exact = math.sqrt(5)
    output_string = ''

    newton_raphson_results = root_finding.newton_raphson(initial_guess,
        my_function, my_function_prime, x_tolerance, True)
    secant_results = root_finding.secant(initial_guess, my_function,
        x_tolerance, True)

    for iteration, (newton_raphson, secant) in \
        enumerate(itertools.zip_longest(newton_raphson_results,
        secant_results)):

        newton_raphson_error = calculate_error(newton_raphson, exact)
        secant_error = calculate_error(secant, exact)
        # Outputs in a format compatible with LaTex tabular :)
        output_string += '{0} & {1:.6f} & {2:.6f}\n'.format(iteration,
            newton_raphson_error, secant_error)

    with open(output_filepath, 'w+') as out_file:
        out_file.write(output_string)


if __name__ == '__main__':
    main()
