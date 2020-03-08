def trapezoidal(myFunc,x,h,N):
    """
    Performs the trapezoidal rule (equation 1.9). The average of the function evaluated at
    the end points of the lattice multiplied by the change in the independent variable.
    Input:  x -- lower bound of the range of integration
            h -- step-size
            N -- number of lattices
    Output: ans -- approximate integral
    """
#JL TODO: This evaluates myFunc twice at x+(i+1)*h then again at x+i_{+}*h. Could be improved·
#to use results from previous iteration.·
    return sum((myFunc(x+i*h)+myFunc(x+(i+1)*h))*h/2.0 for i in range(N))


def simpsons(myFunc,x,h,N):
#Performs Simpons rule (equation 1.12).
#Input:  x -- lower bound of the range of integration
#        h -- step-size
#        N -- number of lattices
#Output: sum -- approximate integral

#Add the contribution from the first and last points of the domain
    sum = myFunc(x) + myFunc(x+N*h)
    for i in range(1,N):                        #1,N ignores first and last points
#Adds the contribution from the even placed lattice points
        if (i%2 == 1):
            sum = sum+4.0*myFunc(x+i*h)
#Adds the contribution from the odd placed lattice points
        else: sum = sum+2.0*myFunc(x+i*h)
    return sum*h/3.0                             #Apply leading factor

def booles(myFunc,x,h,N):
#Performs Bode's rule (equation 1.13b)
#Input:  x -- lower bound of the range of integration
#        h -- step-size
#        N -- number of lattices
#Output: sum -- approximate integral·
#Add the contribution from the first and last points of the domain
    sum = 7.0*(myFunc(x) + myFunc(x+N*h))
    for i in range(1,N):                        #1,N-1 ignores first and last points
#Adds the contribution from the even placed lattice points
        if (i%2 == 1):
            sum = sum+32.0*myFunc(x+i*h)
        elif(i%4 == 2):
            sum = sum+12.0*myFunc(x+i*h)
#Adds the contribution from the odd placed lattice points
        else: sum = sum+14.0*myFunc(x+i*h)
    return sum * 2.0*h/45.0                  #Apply leading factor
