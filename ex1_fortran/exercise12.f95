!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	EXAMPLE 1: SEMICLASSICAL QUATIZATION OF MOLECULAR VIBRATIONS
!	WRITTEN BY JAMISON LAHMAN WITH REFERENCE TO STEVEN KOONIN
!	AND DAWN MEREDITH'S COMPUTATIONAL PHYSICS.
!
!	FINDS THE BOUND STATES OF A LENNARD-JONES POTENTIAL
! 	USING THE BOHR-SOMMERFELD QUANTIZATION RULE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! https://arxiv.org/pdf/0706.2371.pdf reference for the 1.440558

PROGRAM EXAMPLE1
IMPLICIT NONE
    REAL, PARAMETER :: PI=4.*ATAN(1.)
	REAL :: GAMMA						!INPUT PARAMETER
	REAL :: E							!SCALED ENERGY
	REAL :: XIN,XOUT				    !VALUES OF X
	REAL :: S                           !AREA OF PHASE TRAJECTORY
	REAL :: F 							!(N+1/2)*PI
	REAL :: XMIN						!X VALUE OF POTENIAL MIN
	REAL :: POT							!POTENTIAL FUNCTION
	INTEGER :: N						!ENERGY STATE
	INTEGER :: NMAX              		!MAX # OF ENERGY STATES

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	MAKE SURE THAT THE BOTTOM OF THE WELL IS NORMALIZED TO -1 AND
!	XMIN OCCURS AT THE MINIMUM OF POT(X).
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

50	E = -1.0
	XMIN = .74166

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	SINCE THE BOTTOM OF THE WELL IS NORMALIZED TO -1 (V=-1), BEGINNING
!   WITH AN ENERGY OF -1 (E=-1) ENSURES THE ACTION, S, IS ZERO.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!    PRINT *,'ENTER THE VALUE OF GAMMA:'
!    READ *,GAMMA
    GAMMA = 21.7

    CALL FIND_NMAX(XMIN,GAMMA,NMAX)

    IF (NMAX < 0) THEN
        PRINT *,'PLEASE ENTER A LARGER GAMMA'
        GO TO 50
    END IF

    DO N=0,NMAX
        F = (N+0.5)*PI
        CALL FIND_E(E,XIN,XOUT,XMIN,GAMMA,F)
        PRINT *,'N',N,'ENERGY',4.747*E,'XIN',XIN,'XOUT',XOUT
    END DO
    READ *,GAMMA
    GO TO 50
END PROGRAM EXAMPLE1

SUBROUTINE FIND_NMAX(XMIN,GAMMA,NMAX)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	FINDS THE NUMBER OF ENERGY STATES WHICH CAN BE SATISFIED WITH
!   GIVEN GAMMA VALUES. IF NO PHASE SPACE TRAJECTORY CAN SATISFY
!   THE INTITAL VALUE OF PI/2, NMAX IS PASSED AS -1 & USER IS ASKED
!   TO ENTER A NEW VALUE FOR GAMMA.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    REAL, PARAMETER :: PI=4.0*ATAN(1.0)
    REAL :: XIN,XOUT              !INTEGRAL LIMITS
    REAL :: XMIN                  !MINIMUM OF POTENTIAL FUNCTION
    REAL :: S                     !AREA OF PHASE SPACE TRAJECTORY
    REAL :: E                     !ENERY OF THE BOUND STATE I/O
    REAL :: F                     !(N+1/2)*PI
    REAL :: POT                   !POTENTIAL FUNCTION
    REAL :: GAMMA                 !GAMMA (INPUT)
    INTEGER :: I                  !COUNTER

    E = -0.0001                   !MINIMUM ENERGY ALLOWED

    CALL TURNINGPOINT(E,XIN,XOUT,XMIN)
    CALL CALC_S(E,XIN,XOUT,GAMMA,S)

    DO I=0,100
        F = (I+0.5)*PI
            IF (F > S) THEN
                NMAX = I-1
                RETURN
            END IF
    END DO
END SUBROUTINE FIND_NMAX

SUBROUTINE FIND_E(E,XIN,XOUT,XMIN,GAMMA,F)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   GUESSES AN ENERGY, FINDS THE TURNING POINTS,SOLVES EQUATION 1.22
!   THEN EITHER GUESSES ANOTHER ENERGY BASED OFF THE PREVIOUS OR
!   RETURNS IF S(E) IS CLOSE ENOUGH TO (N+.5)*PI
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE
    REAL :: XIN,XOUT              !INTEGRAL LIMITS
    REAL :: XMIN                  !MINIMUM OF POTENTIAL FUNCTION
    REAL :: S                     !AREA OF PHASE SPACE TRAJECTORY
    REAL :: E                     !ENERY OF THE BOUND STATE I/O
    REAL :: F                     !(N+1/2)*PI
    REAL :: POT                   !POTENTIAL FUNCTION
    REAL :: GAMMA                 !GAMMA (INPUT)
    REAL :: DE                    !INITIAL STEP FOR FINDING E
    INTEGER :: I                  !COUNTER

    DE = ABS(E)

30  CONTINUE
    DE = DE-DE/1.79
    E = E+DE

    IF (ABS(E) < .00000001) STOP

    CALL TURNINGPOINT(E,XIN,XOUT,XMIN)
!    CALL NEWTONRAPHSON(E,XIN,XOUT)
    CALL CALC_S(E,XIN,XOUT,GAMMA,S)

    IF (S-F > .00001) THEN         !ACCURACY FOR FINDING S(E)
        E = E-DE
    END IF

    IF (ABS(DE) > .00000001) GOTO 30

END SUBROUTINE FIND_E

SUBROUTINE CALC_S(E,XIN,XOUT,GAMMA,S)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   USES BODE'S EQUATION TO ESTIMATE THE STANDARDIZED ACTION GIVEN
!   BY EQUATION 1.22 IN THE TEXT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE
    REAL :: XIN,XOUT              !INTEGRAL LIMITS
    REAL :: H,X,VAL,SUM           !INTEGRAL VARIABLES
    REAL :: S                     !AREA OF PHASE SPACE TRAJECTORY
    REAL :: E                     !ENERY OF THE BOUND STATE (INPUT)
    REAL :: POT                   !POTENTIAL FUNCTION
    REAL :: GAMMA                 !GAMMA (INPUT)
    INTEGER :: I                  !COUNTER

    H = (XOUT-XIN)/2000.0
    SUM = 0.0
    X = XIN

    DO I=0,2000
        X = XIN + I*H
        S = SQRT(E-POT(X))
        VAL = 0.0

        IF (I == 0 .OR. I == 2000) THEN
            VAL = 14.0*H*S/45.0
        ELSE IF (MOD(I,2) == 1) THEN
            VAL = 64.0*H*S/45.0
        ELSE IF (MOD(I,4) == 2) THEN
            VAL = 24.0*H*S/45.0
        ELSE
            VAL = 28.0*H*S/45.0
        END IF
        SUM = SUM + VAL
    END DO
    S = GAMMA * SUM
    RETURN
END SUBROUTINE CALC_S

SUBROUTINE TURNINGPOINT(E,X1,X2,XMIN)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   SINCE P(R) = SQRT(2M(E-V(R))) AND THE TURNING POINTS OCCUR
!   WHEN P(R) = 0, THAT IMPLIES E = V. THIS SUBROUTINE FINDS WHEN
!   THE POTENTIAL FUNCTION EQUALS THE ENERGY WHICH IN TURN GIVES
!   THE VALUE OF X_IN AND X_OUTUSING THE SEARCH METHOD.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE
    REAL :: DX                  !STEP FOR INTITAL SEARCH
    REAL :: XMIN                !X VALUE FOR POTENTAL MINIMUM
    REAL :: E                   !ENERGY (INPUT)
    REAL :: X1,X2               !TURNING POINTS (OUTPUT)
    REAL :: POT                 !POTENTIAL FUNCTION

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   USES THE SEARCH ALGORITHM TO FIND THE ROOT OF P(X_IN)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    DX = 0.002
    X1=XMIN
10  CONTINUE
    X1 = X1-DX
    IF (POT(X1) > E) THEN
        X1 = X1+DX
        DX = DX/2.0
    END IF
    IF (ABS(DX) > .000001) GOTO 10  !THE TOLERANCE OF THE SEARCH

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   USES THE SEARCH ALGORITH TO FIND THE ROOT OF P(X_OUT)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    DX = 0.002
    X2=XMIN
20  CONTINUE
    X2 = X2+DX
    IF (POT(X2) > E) THEN
        X2 = X2-DX
        DX = DX/2.0
    END IF
    IF (ABS(DX) > .000001) GOTO 20  !THE TOLERANCE OF THE SEARCH
    RETURN
END SUBROUTINE TURNINGPOINT

REAL FUNCTION POT (X)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   FUNCTION FOR THE POTENTIAL. RETURNS POT FOR A GIVEN VALUE OF X
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    REAL :: X
    POT = (1.-EXP(-1.25231934*(X-.74166)))**2-1.
!    POT = (1.-EXP(-1.440558*(X-.74166)))**2-1.
    RETURN
END FUNCTION
