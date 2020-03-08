def search(x,tolX):
#Uses a binary search to find the root
#Input:  x -- independent variable
#Output: y -- dependent variable
    raise NotImplementedError

def newton_raphson(x,tolX):
#Performs the Newton-Raphson for root finding
#Input:  x -- independent variable
#Output: y -- dependent variable
    raise NotImplementedError

def secant(x,tolX):
#Performs the secant method for root finding
#Input:  x -- independent variable
#Output: y -- dependent variable
    x_1 = x + 1
    while (math.fabs(myFunc(xNew)) > tolX):
        iter = iter + 1
        xOld = x_1                  #Placeholder while x_1 updates
        x_1 = x_1 - myFunc(x_1)*(x_1 - x)/(myFunc(x_1) - myFunc(x))
        x = xOld

