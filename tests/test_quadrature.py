import math
import unittest

from computational_physics import quadrature

class TestQuadrature(unittest.TestCase):
    def setUp(self):
        self.upper_bound = 1.0
        self.lower_bound = 0.0
        self.expected = (self.my_function(self.upper_bound) -
            self.my_function(self.lower_bound))
        self.low_number_of_lattices = 4
        self.high_number_of_lattices = 128

    def test_trapezoidal_small_h(self):
        observed = quadrature.trapezoidal(self.upper_bound, self.lower_bound,
            self.my_function, self.low_number_of_lattices)
        expected_error = -0.00894
        self.check_error(observed, expected_error)

    def test_trapezoidal_large_h(self):
        observed = quadrature.trapezoidal(self.upper_bound, self.lower_bound,
            self.my_function, self.high_number_of_lattices)
        expected_error = -0.00001
        self.check_error(observed, expected_error)

    def test_simpsons_small_h(self):
        observed = quadrature.simpsons(self.upper_bound, self.lower_bound,
            self.my_function, self.high_number_of_lattices)
        self.check_error(observed)

    def test_simpsons_large_h(self):
        observed = quadrature.simpsons(self.upper_bound, self.lower_bound,
            self.my_function, self.low_number_of_lattices)
        expected_error = -0.00004
        self.check_error(observed, expected_error)

    def test_booles_small_h(self):
        observed = quadrature.booles(self.upper_bound, self.lower_bound,
            self.my_function, self.high_number_of_lattices)
        self.check_error(observed)

    def test_booles_large_h(self):
        observed = quadrature.booles(self.upper_bound, self.lower_bound,
            self.my_function, self.low_number_of_lattices)
        self.check_error(observed)

    def check_error(self, observed_value, error_offset = 0):
        error = round(observed_value - self.expected + error_offset, 5)
        self.assertEqual(error, 0,
            "Final value of {} does not match expected value {}."
            .format(observed_value, self.expected + error_offset))

    @staticmethod
    def my_function(x):
        return math.exp(x)

if __name__ == '__main__':
    unittest.main()
