PROGRAM EXERCISE3_5
!   Uses the program chap3b.for to find the eigenvalue
!   for the wave equation using the Numerov and
!   secant search methods
IMPLICIT NONE
    REAL :: K1,K2,KOLD
    REAL :: DK
    REAL :: TOLK
    REAL :: PHIP
    REAL :: PHIOLD
    REAL :: EXACT

    K1 = 1.1
    K2 = K1+1.0
    TOLK = 1E-05
10  CONTINUE
        KOLD = K2
        CALL INTGRT(K1,PHIOLD)
        CALL INTGRT(K2,PHIP)
        K2 = K2-PHIP*(K2-K1)/(PHIP-PHIOLD)
        K1 = KOLD
    IF (ABS(PHIP) > TOLK)GOTO 10
    EXACT = 2.0*ATAN(1.0)
    PRINT *, 'EIGENVALUE | ERROR',K2,EXACT-K2
    STOP
END PROGRAM

SUBROUTINE INTGRT(K,PHIP)
    REAL :: K
    REAL :: PHIP,PHIZ,PHIM
    REAL :: CONST
    REAL :: H
    INTEGER :: IX
    INTEGER :: NSTEP

    DATA NSTEP/100/
    H = 1.0/NSTEP
    PHIM = 1.0
    PHIZ = 1.00001
    CONST = (K*H)**2/12.0
    DO 10 IX=1,NSTEP-1
        PHIP=2.0*(1.-5.*CONST)*PHIZ-(1.0+CONST)*PHIM
        PHIP=PHIP/(1+CONST)
        PHIM=PHIZ
        PHIZ=PHIP
10  CONTINUE
    PRINT *,K,PHIP
    RETURN
END SUBROUTINE
