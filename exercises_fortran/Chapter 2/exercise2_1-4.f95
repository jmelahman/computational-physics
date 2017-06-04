PROGRAM EXERCISE2
!Uses an assortment of techniques to solve the
!the differential equation dy/dx=-xy with
!an initial value of y(0)=1 & using 16 steps
IMPLICIT NONE
    REAL :: X,Y
    REAL :: H

10  PRINT *,'ENTER A VALUE OF X'
    READ *,X
    Y = 1.0
    H = X/1000.0

    PRINT *,'EXACT:   ',EXP(-X**2/2.)

    CALL EULER(X,Y,H)
    Y = 1.0
    CALL ADAMS2(X,Y,H)
    Y = 1.0
    CALL ADAMS4(X,Y,H)
    Y = 1.0
    CALL TAYLOR(X,Y,H)
    Y = 1.0
    CALL IMP(X,Y,H)
    Y = 1.0
    CALL RK2(X,Y,H)
    Y = 1.0
    CALL RK3(X,Y,H)
    Y = 1.0
    CALL RK4(X,Y,H)

    GOTO 10
END PROGRAM

SUBROUTINE RK4(X,Y,H)
!Uses the Runge-Kutta 4th-order method
IMPLICIT NONE
    REAL :: X,H
    REAL :: Y
    REAL :: K1,K2,K3,K4
    REAL ::FUNC
    INTEGER :: I

    DO I=0,999
        K1 = H*FUNC(I*H,Y)
        K2 = H*FUNC((I+.5)*H,Y+K1/2.)
        K3 = H*FUNC((I+.5)*H,Y+K2/2.)
        K4 = H*FUNC((I+1)*H,Y+K3)
        Y = Y+(K1+2.*K2+2.*K3+K4)/6.
    END DO
    PRINT *,'RUNGE4:  ',Y

END SUBROUTINE


SUBROUTINE RK3(X,Y,H)
!Uses the Runge-Kutta 3rd-order method
IMPLICIT NONE
    REAL :: X,H
    REAL :: Y
    REAL :: K1,K2,K3
    REAL ::FUNC
    INTEGER :: I

    DO I=0,999
        K1 = H*FUNC(I*H,Y)
        K2 = H*FUNC((I+.5)*H,Y+K1/2.)
        K3 = H*FUNC((I+1)*H,Y-K1+2.*K2)
        Y = Y+(K1+4.*K2+K3)/6.
    END DO
    PRINT *,'RUNGE3:  ',Y

END SUBROUTINE

SUBROUTINE RK2(X,Y,H)
!Uses the Runge-Kutta 2nd-order method
IMPLICIT NONE
    REAL :: X,H
    REAL :: Y
    REAL ::FUNC
    INTEGER :: I

    DO I=0,999
        Y = Y+H*FUNC((I+.5)*H,Y+H*FUNC(I*H,Y)/2.)
    END DO
    PRINT *,'RUNGE2:  ',Y

END SUBROUTINE

SUBROUTINE IMP(X,Y,H)
!Uses the implicit method. g(x) = -x = -i*h
IMPLICIT NONE
    REAL :: X,H
    REAL :: Y
    REAL ::FUNC
    INTEGER :: I

    DO I=0,999
        Y = Y*(1.+(-I*H)*H/2.)/(1.-(-(I+1)*H)*H/2.)
    END DO
    PRINT *,'IMPLICIT:',Y
END SUBROUTINE

SUBROUTINE TAYLOR(X,Y,H)
!Uses the Taylor Series method. partial
!derivatives are df/dx = -y and df/dy = -x
IMPLICIT NONE
    REAL :: X,H
    REAL :: Y
    REAL ::FUNC
    INTEGER :: I

    DO I=0,999
        Y = Y+H*FUNC(H*I,Y)-H**2*(Y+FUNC(H*I,Y)*(I*H))/2.
    END DO
    PRINT *,'TAYLOR:  ',Y
END SUBROUTINE

SUBROUTINE ADAMS4(X,Y4,H)
!Passes the inital value as y4. uses Euler's
!method to estimate n-1,,-2,-3 values then
!proceeds with the Adams-Bashforth 4-step
IMPLICIT NONE
    REAL :: X,H
    REAL :: Y1,Y2,Y3,Y4,YOLD
    REAL ::FUNC
    INTEGER :: I

    Y3 = Y4+H*FUNC(-H,Y4)
    Y2 = Y3+H*FUNC(-H*2.,Y3)
    Y1 = Y3+H*FUNC(-H*3.,Y2)
    DO I=0,999
        YOLD = Y4
        Y4 = Y4+H*(55.*FUNC(H*I,Y4)-59.*FUNC(H*(I-1),Y3)+37.*FUNC(H*(I-2),Y2)-9.*FUNC(H*(I-3),Y1))/24.
        Y1 = Y2
        Y2 = Y3
        Y3 = YOLD
    END DO
    PRINT *,'ADAMS4:  ',Y4

!Goes backwards to estimate the intital value
!and determine the accuracy
    DO I=999,0,-1
        YOLD = Y4
        Y4 = Y4-H*(55.*FUNC(H*I,Y4)-59.*FUNC(H*(I-1),Y3)+37.*FUNC(H*(I-2),Y2)-9.*FUNC(H*(I-3),Y1))/24.
        Y1 = Y2
        Y2 = Y3
        Y3 = YOLD
    END DO
    PRINT *,'DOING ADAMS-BASHFORTH 4-STEP BACKWARDS YIELDS',Y4
END SUBROUTINE

SUBROUTINE ADAMS2(X,Y2,H)
!Passes the inital value as y2. uses Euler's
!method to estimate n-1 value then
!proceeds with the Adams-Bashforth 2-step
IMPLICIT NONE
    REAL :: X,H
    REAL :: Y1,Y2,YOLD
    REAL ::FUNC
    INTEGER :: I

    Y1 = Y2+H*FUNC(-H,Y1)
    DO I=1,999
        YOLD = Y2
        Y2 = Y2+H*(3.*FUNC(H*I,Y2)/2.-FUNC(H*(I-1),Y1)/2.)
        Y1 = YOLD
    END DO
    PRINT *,'ADAMS2:  ',Y2

!Goes backwards to estimate the intital value
!and determine the accuracy
    DO I=999,1,-1
        YOLD = Y2
        Y2 = Y2-H*(3.*FUNC(H*I,Y2)/2.-FUNC(H*(I-1),Y1)/2.)
        Y1 = YOLD
    END DO
    PRINT *,'DOING ADAMS-BASHFORTH 2-STEP BACKWARDS YIELDS',Y2
END SUBROUTINE

SUBROUTINE EULER(X,Y,H)
!Uses Euler's method
IMPLICIT NONE
    REAL :: X,Y,H
    REAL ::FUNC
    INTEGER :: I

    DO I=0,999
        Y = Y+H*FUNC(H*I,Y)
    END DO
    PRINT *,'EULER:   ',Y

!Goes backwards to estimate the intital value
!and determine the accuracy
    DO I=999,0,-1
        Y = Y-H*FUNC(H*I,Y)
    END DO
    PRINT *,'DOING EULER BACKWARDS YIELDS',Y
END SUBROUTINE

REAL FUNCTION FUNC(X,Y)
IMPLICIT NONE
    REAL :: X,Y
    FUNC = -X*Y
    RETURN
END FUNCTION
