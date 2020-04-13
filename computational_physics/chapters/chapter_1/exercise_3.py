"""
Exercise  1.3: Write a program to calculate
\int^1_0 t^{-2/3} (1 - t)^{-1/3} dt = 2\pi / 3^{1/2}
Using one of the quadrature formulas discussed above and investigate its accuracy
for various values of h. (Hint: Split the range of integration into two parts and
make a different change of variable in each integral to handle the singularities

Computational Physics: FORTRAN Version
by Steven E. Koonin and Dawn C. Meredith

Solution by Jamison Lahman, August 5, 2018

TODO: u-substitution method
"""
import math
import os

from computational_physics import quadrature
from computational_physics import definitions

def trig_sub(t):
    """
    Substitues t=sin^3(t) which is well-behaved over the interval of integration.
    Input:  t -- independent variable
    Output: y -- dependent variable
    """
    numerator = math.cos(t) * (1.0 + math.sin(t))
    denominator = 1.0 + math.sin(t) + math.pow(math.sin(t), 2)
    return 3.0 * math.pow(numerator / denominator, 1.0 / 3.0)

def main():
    """Executes exercise 1.3"""

    file_directory = os.path.dirname(os.path.realpath(__file__))
    output_filepath = os.path.join(file_directory, 'output/exercise_3.txt')
    trig_upper_bound = math.pi / 2.0
    trig_lower_bound = 0.0
    lattices = [4, 8, 16, 32, 64, 128]

    exact = (2.0 * math.pi) / math.sqrt(3)

    with open(output_filepath, 'w+') as out_file:
        for N in lattices:
            trig_h = definitions.get_step_size(trig_upper_bound,
                trig_lower_bound, N)

            trig_trapezoidal = quadrature.trapezoidal(trig_upper_bound,
                trig_lower_bound, trig_sub, N)
            trig_error_trapezoidal = trig_trapezoidal - exact
            trig_simpsons = quadrature.simpsons(trig_upper_bound,
                trig_lower_bound, trig_sub, N)
            trig_error_simpsons = trig_simpsons - exact
            trig_booles = quadrature.booles(trig_upper_bound,
                trig_lower_bound, trig_sub, N)
            trig_error_booles = trig_booles - exact

            #Outputs in a format compatible with LaTex tabular :)
            out_file.write('{0} & {1:.5f} & {2:.6f} & {3:.6f} & {4:.6f}\n'.format(
                N, trig_h, trig_error_trapezoidal, trig_error_simpsons, trig_error_booles))

if __name__ == '__main__':
    main()
