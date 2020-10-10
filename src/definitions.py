def get_step_size(upper_bound, lower_bound, number_of_lattices):
    h = (upper_bound - lower_bound) / number_of_lattices
    return h
