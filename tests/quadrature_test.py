import math
import unittest

from computational_physics.basic import quadrature

class TestQuadrature(unittest.TestCase):
    def setUp(self):
        self.upper_bound = 1.0
        self.lower_bound = 0.0
        self.expected = (self.my_function(self.upper_bound) -
            self.my_function(self.lower_bound))
        self.number_of_lattices = 128

    def test_trapezoidal(self):
        observed = quadrature.trapezoidal(self.upper_bound, self.lower_bound,
            self.my_function, self.number_of_lattices)
        self.check_error(observed)

    def test_simpsons(self):
        observed = quadrature.simpsons(self.upper_bound, self.lower_bound,
            self.my_function, self.number_of_lattices)
        self.check_error(observed)

    def test_booles(self):
        observed = quadrature.booles(self.upper_bound, self.lower_bound,
            self.my_function, self.number_of_lattices)
        self.check_error(observed)

    def check_error(self, observed_value):
        error = round(observed_value - self.expected, 4)
        self.assertEqual(error, 0,
            "Final value of {} does not match expected value {}."
            .format(observed_value, self.expected))

    @staticmethod
    def my_function(x):
        return math.exp(x)

if __name__ == '__main__':
    unittest.main()
