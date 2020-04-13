import math
import unittest

from computational_physics.basic import root_finding

class TestRootFinding(unittest.TestCase):
    def setUp(self):
        self.expected = math.sqrt(5)
        self.initial_guess = 1
        self.x_tolerance = 0.0001

    def test_search(self):
        guesses = root_finding.search(self.initial_guess, self.my_function,
            self.x_tolerance)
        observed = guesses[-1]
        self.check_error(observed)

    def test_secant(self):
        guesses = root_finding.secant(self.initial_guess, self.my_function,
            self.x_tolerance)
        observed = guesses[-1]
        self.check_error(observed)

    def test_newton(self):
        guesses = root_finding.newton_raphson(self.initial_guess,
            self.my_function, self.my_function_prime, self.x_tolerance)
        observed = guesses[-1]
        self.check_error(observed)

    def check_error(self, observed_value):
        error = round(observed_value - self.expected, 4)
        self.assertEqual(error, 0,
            "Final value of {} does not match expected value {}."
            .format(observed_value, self.expected))

    @staticmethod
    def my_function(x):
        return math.pow(x, 2) - 5

    @staticmethod
    def my_function_prime(x):
        return 2.0 * x

if __name__ == '__main__':
    unittest.main()