PROGRAM EXERCISE1_2
!CALCULATES THE INTEGRAL FOR 4*X**3+X**2-4
IMPLICIT NONE
    REAL :: EXACT
    REAL :: H               !STEP SIZE
    REAL :: LB,UB           !LOWER AND UPPER BOUND
    INTEGER :: N            !NUMBER OF STEPS

10  PRINT *,'ENTER THE LOWER BOUND'
    READ *,LB
    PRINT *,'ENTER THE UPPER BOUND'
    READ *,UB
    EXACT = UB**4+UB**3/3-4*UB-(LB**4+LB**3/3-4*LB)
11  PRINT *,'ENTER THE VALUE OF N (MUST BE MULTIPLE OF 4)'
    READ *,N
    IF (MOD(N,4) > 0) GOTO 11

    H = SQRT(UB-LB)/N
!Jamison: I am not sure this was intended to be sqrt(UB-LB)
    PRINT *,'THE VALUE OF H IS ',H
    PRINT *,'THE EXACT VALUE IS',EXACT
    CALL SIMPSON(LB,N,H)
    CALL BOOLE(LB,N,H)
    CALL DURAND(LB,N,H)
END PROGRAM

SUBROUTINE SIMPSON(LB,N,H)
!   USES SIMPSON'S RULE (EQUATION 1.12)
IMPLICIT NONE
    REAL :: SUM,VAL,H,F
    REAL :: LB,U
    INTEGER :: N,I
    SUM = 0
    DO I=0,N
        U = LB+I*H
        F = 4*U**3+U**2-4
        VAL = 0
        IF (I == 0 .OR. I == N) THEN
            VAL = H*F/3.0
        ELSE IF (MOD(I,2) /= 0) THEN
            VAL = 2.0*H*F/3.0
        ELSE
            VAL = 4.0*H*F/3.0
        END IF
        SUM = SUM + VAL
    END DO
    PRINT *,"SIMPSON'S METHOD YIELDS:",SUM
END SUBROUTINE SIMPSON

SUBROUTINE BOOLE(LB,N,H)
!   USES BOOLE'S RULE (ALSO CALLED BODE'S EQUATION, EQUATION 1.13B)
IMPLICIT NONE
    REAL :: VAL,H,F,SUM
    REAL :: LB,U
    INTEGER :: N,I
    SUM = 0
    DO I=0,N
        U = LB+I*H
        F = 4*U**3+U**2-4
        VAL = 0
        IF (I == 0 .OR. I == N) THEN
            VAL = 14.0*H*F/45.0
        ELSE IF (MOD(I,2) == 1) THEN
            VAL = 64.0*H*F/45.0
        ELSE IF (MOD(I,4) == 2) THEN
            VAL = 24.0*H*F/45.0
        ELSE
            VAL = 28.0*H*F/45.0
        END IF
        SUM = SUM + VAL
    END DO
    PRINT *,"BOOLE'S METHOD YIELDS:",SUM
END SUBROUTINE BOOLE

SUBROUTINE DURAND(LB,N,H)
!   USES A NEWTON-COTES FORMULA CALLED DURAND'S RULE
IMPLICIT NONE
    REAL :: VAL,H,F,SUM
    REAL :: LB,U
    INTEGER :: N,I
    SUM = 0
    DO I=0,N
        U = LB+I*H
        F = 4*U**3+U**2-4
        VAL = 0
        IF (I == 0 .OR. I == N) THEN
            VAL = 2.0*H*F/5.0
        ELSE IF (I == 1 .OR. I == N-1) THEN
            VAL = 11.0*H*F/12.0
        ELSE
            VAL = H*F
        END IF
    SUM = SUM + VAL
    END DO
    PRINT *,"DURAND'S METHOD YIELDS:",SUM
END SUBROUTINE DURAND
