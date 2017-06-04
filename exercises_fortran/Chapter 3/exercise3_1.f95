PROGRAM EXERCISE3_1
!   Uses the Numerov algorithm to solve the ODE
!   d^2y/dx^2 = -4pi^2y with initial condidtions
!   y(0)=1 and y'(0)=0
IMPLICIT NONE
    REAL, PARAMETER :: PI=4.0*ATAN(1.0)
    REAL :: X                   !INDEPENDENT VARIABLE
    REAL :: Y1,Y2,Y3,YOLD       !DEPENDENT VARIABLE
    REAL :: P                   !MOMENTUM
    REAL :: H                   !INTEGRATION STEP
    REAL :: KS                  !K SQUARED
    REAL :: NC                  !NUMEROV CONSTANT
    INTEGER :: I                !COUNTER
    INTEGER :: IMAX             !COUNT-STOPPER

10  PRINT *,'Enter the number of steps'
    READ *,IMAX

    X = 1.0
    Y2 = 1.0
    P = 0.0
    H = X/IMAX
    KS = 4.0*PI*PI
    NC = H*H*KS/12.0
!Taylor Series expansion for y(0)_n-1
    Y1 = Y2-P*H-H*H*KS*Y2/2.0

    DO I=1,IMAX
        YOLD = Y2
        Y3 = (-(NC+1.0)*Y1+(2.0-10.0*NC)*Y2)/(1.0+NC)
        Y1 = YOLD
        Y2 = Y3
    END DO
    PRINT *,Y3
    GOTO 10
END PROGRAM
