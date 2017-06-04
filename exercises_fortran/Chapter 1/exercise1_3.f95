PROGRAM EXERCISE1_3
!CALCULATES THE INTEGRAL FOR (T)**(-2/3)*(1-T)**(-1/3)
!USING BOTH A TRIG SUBSTIUTION AND AN U SUBSTITUTION.
!BOTH SUBROUTINES USE BODE'S EQUATION.
IMPLICIT NONE
    REAL :: PI=4.*ATAN(1.)
    INTEGER :: N

11  PRINT *,'PLEASE ENTER THE VALUE OF N (MUST BE MULTIPLE OF 4)'
    PRINT *,'ENTER A VALUE < 4 TO EXIT'
    READ *,N

    IF (MOD(N,4) > 0) GOTO 11

    PRINT *,'THE EXACT ANSWER IS ',2.*PI/SQRT(3.)
    CALL TRIG_INT(N,PI)
    CALL ALG_INT(N)

    GOTO 11
END PROGRAM

SUBROUTINE ALG_INT(N)
!   SPLITS THE INTEGRAL IN HALF, USES TWO DIFFERENT U
!   SUBSITUTIONS TO SOLVE. SUM 1 IS THE INTEGRAL FROM
!   .5-1 WITH T=1-U^(3/2). SUM 2 IS THE INTEGRAL FROM
!   0-.5 WITH T=U^3. H*I = U
IMPLICIT NONE
    REAL :: VAL,H,F,SUM1,SUM2
    INTEGER :: N,I

    H = 2.**(-2./3.)/N

    SUM1 = 0
    SUM2 = 0

    DO I=0,N
        F= 3./2.*(1-(H*I)**(3./2.))**(-2./3.)
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
        SUM1 = SUM1 + VAL
    END DO

    H = 2.0**(-1.0/3.0)/N

    DO I=0,N
        F= 3.0*(1-(H*I)**3.0)**(-1.0/3.0)
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
        SUM2 = SUM2 + VAL
    END DO


    PRINT *,'USING AN ALGEBRAIC SUBSTITUTION ',SUM1+SUM2
END SUBROUTINE ALG_INT


SUBROUTINE TRIG_INT(N,PI)
!   SUBSITUTES T=SIN^3(X) WHICH PREVENTS THE NEED
!   TO SPLIT THE INTERVAL IN TWO THUS CUTTING CALCULATION
!   TIME IN HALF.
IMPLICIT NONE
    REAL :: VAL,H,F,SUM,PI
    INTEGER :: N,I

    !INTEGRAL BOUNDS BECOME U=0,PI/2
    H = PI/(2.*N)

    SUM = 0

    DO I=0,N
        F= 3.0*(COS(I*H)*((1.+SIN(I*H))/(1.+SIN(I*H)+SIN(I*H)**2)))**(1./3.)
        VAL = 0
        IF (I == 0) THEN
            VAL = 14.0*H*F/45.0
        ELSE IF (I == N) THEN
            VAL = 0
        ELSE IF (MOD(I,2) == 1) THEN
            VAL = 64.0*H*F/45.0
        ELSE IF (MOD(I,4) == 2) THEN
            VAL = 24.0*H*F/45.0
        ELSE
            VAL = 28.0*H*F/45.0
        END IF
        SUM = SUM + VAL
    END DO
    PRINT *,'USING A TRIG SUBSTITUTION: ',SUM
END SUBROUTINE TRIG_INT
