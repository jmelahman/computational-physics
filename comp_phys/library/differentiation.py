import math

def backward2(input_function, x, h):
    """
    Performs the backward 2-point method on the input_function
    Input:  x -- independent variable
            h -- step-size
    Output: dependent variable
    """

    f_0 = input_function(x)
    f_1 = input_function(x + h)
    return (f_1 - f_0) / h


def forward2(input_function, x, h):
    """
    Performs the forward 2-point method on the input_function
    Input:  x -- independent variable
            h -- step-size
    Output: dependent variable
    """

    f_minus_1 = input_function(x - h)
    f_0 = input_function(x)
    return (f_0 - f_minus_1) / h

def symmetric3(input_function, x, h):
    """
    Performs the symmetric 3-point method on the input_function
    Input:  x -- independent variable
            h -- step-size
    Output: dependent variable
    """

    f_minus_1 = input_function(x - h)
    f_1 = input_function(x + h)
    return (f_1 - f_minus_1) / (2.0 * h)


def symmetric4(input_function, x, h):
    """
    Performs the symmetric 4-point method on the input_function
    Input:  x -- independent variable
            h -- step-size
    Output: dependent variable
    """

    f_minus_1 = input_function(x - h)
    f_0 = input_function(x)
    f_1 = input_function(x + h)
    f_2 = input_function(x + (2.0 * h))
    return ((-2.0 * f_minus_1) - (3.0 * f_0) + (6.0 * f_1) - f_2) / (6.0 * h)


def symmetric5(input_function, x, h):
    """
    Performs the symmetric 5-point method on the input_function
    Input:  x -- independent variable
            h -- step-size
    Output: dependent variable
    """

    f_minus_2 = input_function(x - (2.0 * h))
    f_minus_1 = input_function(x - h)
    f_0 = input_function(x)
    f_1 = input_function(x + h)
    f_2 = input_function(x + (2.0 * h))
    return (f_minus_2 - (8.0 * f_minus_1) + (8.0 * f_1) - f_2) / (12.0 * h)
