"""
Exercise 1.5: Write programs to solve for the positive root of x^2 - 5 using the
Newton-Raphson and secant methods. Investigate the behavior of the latter with
changes in the initial guesses for the root.

Computational Physics: FORTRAN Version
by Steven E. Koonin and Dawn C. Meredith

Solution by Jamison Lahman, August 5, 2018
"""
import math

from comp_phys.library import root_finding

def my_function(x):
    """
    Function to be evaluated
    Input:  x -- independent variable
    Output: y -- dependent variable
    """

    return math.pow(x, 2.0) - 5.0

def my_function_prime(x):
    """
    Input:  x -- independent variable
    Output: y -- dependent variable
    """

    return 2.0 * x

def main():
    """Executes exercise 1.5"""

    file_directory = os.path.dirname(os.path.realpath(__file__))
    output_filepath = os.path.join(file_directory, 'output/exercise_5.txt')
    initial_guess = 1.67                                # initial guess of x

    with open(output_filepath, 'w+') as out_file:
        newton_raphson_iterations = root_finding.newton_raphson(my_function, initial_guess)
        secant_iterations = root_finding.secant(my_function, initial_guess)

        #Outputs in a format compatible with LaTex tabular :)
        out_file.write('{0:.5f} & {1:.6f} & {2:.6f} & {3:.6f} & {4:.6f} & {5:.6f}\n'.format(
            h, newton_raphson_iterations, secant_iterations)


if __name__ == '__main__':
    main()
