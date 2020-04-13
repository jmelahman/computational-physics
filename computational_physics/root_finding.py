import math

DEFAULT_X_TOLERANCE = 0.0001

def search(x, function, step_size=0.5, x_tolerance=DEFAULT_X_TOLERANCE):
    """
    Performs a binary search for root finding
    Input:  x -- independent variable
            function -- Function to be evaluated
            step_size -- Initial amount to step; dx
            x_tolerance -- Accuracy of guessed root to the actual
    Output: x_values -- List guesses for root
    """
    x_values = [x]
    while (math.fabs(function(x)) > x_tolerance):
        x_1 = x + step_size
        # If the sign changes, back up and halve the step
        if function(x) * function(x_1) < 0:
            step_size = step_size / 2.0
        else:
            x_values.append(x_1)
        x = x_values[-1]
    return x_values

def newton_raphson(x, function, function_prime, x_tolerance=DEFAULT_X_TOLERANCE):
    """
    Performs the Newton-Raphson for root finding
    Input:  x -- independent variable
            function -- Function to be evaluated
            function_prime -- Derivative of function()
            x_tolerance -- Accuracy of guessed root to the actual
    Output: x_values -- List guesses for root
    """
    x_values = [x]
    while (math.fabs(function(x)) > x_tolerance):
        x_values.append(evaluate_newton_raphson(x, function, function_prime))
        x = x_values[-1]
    return x_values

def evaluate_newton_raphson(x_i, function, function_prime):
    return x_i - (function(x_i) / function_prime(x_i))

def secant(x, function, x_tolerance=DEFAULT_X_TOLERANCE):
    """
    Performs the secant method for root finding
    Input:  x -- independent variable
            function -- Function to be evaluated
            x_tolerance -- Accuracy of guessed root to the actual
    Output: x_values -- List guesses for root
    """
    x_1 = x + 1
    x_values = [x, x_1]
    while (math.fabs(function(x_values[-1])) > x_tolerance):
        x = x_values[-2]
        x_1 = x_values[-1]
        x_1 = evaluate_secant(x, x_1, function)
        x_values.append(x_1)
    return x_values

def evaluate_secant(x_i, x_i_minus_1, function):
    return x_i - (function(x_i) * (x_i - x_i_minus_1) / (function(x_i) - function(x_i_minus_1)))

