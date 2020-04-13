DEFAULT_NUMBER_OF_LATTICES = 128

def trapezoidal(upper_bound, lower_bound, function,
             number_of_lattices=DEFAULT_NUMBER_OF_LATTICES):
    """
    Performs the trapezoidal rule (equation 1.9). The average of the function evaluated at
    the end points of the lattice multiplied by the change in the independent variable.
    Input:  x -- lower bound of the range of integration
            h -- step-size
            N -- number of lattices
    Output: ans -- approximate integral
    """
    h = get_step_size(upper_bound, lower_bound, number_of_lattices)
    x = lower_bound
    sum = 0

    f_minus_1 = function(lower_bound)
    x += h
    f_0 = function(x)

    for iteration in range(2, number_of_lattices):
      x += h
      f_1 = function(x)
      sum += f_minus_1 + (2.0 * f_0) + f_1
      f_minus_1 = f_0
      f_0 = f_1
    return (h / 2.0) * sum


def simpsons(upper_bound, lower_bound, function,
             number_of_lattices=DEFAULT_NUMBER_OF_LATTICES):
    """
    Performs Simpons rule (equation 1.12).
    Input:  upper_bound -- upper bound of the range of integration; a
            lower_bound -- lower bound of the range of integration; b
            function -- function to be evaluated
            number_of_lattices -- number of lattices; N
    Output: sum -- approximate integral
    """
    h = get_step_size(upper_bound, lower_bound, number_of_lattices)
    x = lower_bound

    sum = function(upper_bound) + function(lower_bound)
    for iteration in range(1, number_of_lattices):
        x += h
        if (iteration % 2 == 1):
            sum += 4.0 * function(x)
        else:
            sum += 2.0 * function(x)
    return (h / 3.0) * sum

def booles(upper_bound, lower_bound, function,
           number_of_lattices=DEFAULT_NUMBER_OF_LATTICES):
    """
    Performs Bode's rule (equation 1.13b)
    Input:  upper_bound -- upper bound of the range of integration; a
            lower_bound -- lower bound of the range of integration; b
            function -- function to be evaluated
            number_of_lattices -- number of lattices; N
    Output: sum -- approximate integral
    """
    h = get_step_size(upper_bound, lower_bound, number_of_lattices)
    x = lower_bound

    # Handle special case for first and last points
    sum = 7.0 * (function(lower_bound) + function(upper_bound))
    for iteration in range(1, number_of_lattices):
        x += h
        if (iteration % 2 == 1):
            sum += 32.0 * function(x)
        elif (iteration % 4 == 2):
            sum += 12.0 * function(x)
        else:
            sum += 14.0 * function(x)
    return ((2.0 * h) / 45.0) * sum

def get_step_size(upper_bound, lower_bound, number_of_lattices):
    h = (upper_bound - lower_bound) / number_of_lattices
    return h
