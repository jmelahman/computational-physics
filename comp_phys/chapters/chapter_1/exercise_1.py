"""
Exercise  1.1: Using any function for which you can evaluate the
derivatives analytically, investigate the accuracy of the formulas
in Table 1.2 for various values of h.

Computational Physics: FORTRAN Version
by Steven E. Koonin and Dawn C. Meredith

Solution by Jamison Lahman, June 3, 2017
"""
import math
import os

from comp_phys.lib import differentiation


def my_sin(x):
    """
    Function to be evaluated.
    Input:  x -- independent variable
    Output: y -- dependent variable
    """

    return math.sin(x)


def main():
    """Executes exercise 1.1"""

    file_directory = os.path.dirname(os.path.realpath(__file__))
    output_filepath = os.path.join(file_directory, 'output/exercise_1.txt')
    value = 1.0
    h_list = [.5, .2, .1, .05, .02, .01, .005, .002,
              .001, .0005, .0002, .0001, .00005, .00002, .00001]
    exact = math.cos(value)

    with open(output_filepath, 'w+') as out_file:
        for h in h_list:
            error_backward = differentiation.backward2(my_sin, value, h) - exact
            error_forward = differentiation.forward2(my_sin, value, h) - exact
            error_symmetric3 = differentiation.symmetric3(my_sin, value, h) - exact
            error_symmetric4 = differentiation.symmetric4(my_sin, value, h) - exact
            error_symmetric5 = differentiation.symmetric5(my_sin, value, h) - exact
            # Outputs in a format compatible with LaTex tabular :)
            out_file.write('{0:.5f} & {1:.6f} & {2:.6f} & {3:.6f} & {4:.6f} & {5:.6f}\n'.format(
                h, error_backward, error_forward, error_symmetric3, error_symmetric4,
                error_symmetric5))


if __name__ == '__main__':
    main()
