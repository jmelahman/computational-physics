PROGRAM EXERCISE3_6
!   Uses the program chap3b.for to find the
!   eigenvalue for the wave equation in spherical
!   coordinates using the Numerov and secant
!   search methods. The equation being solved is
!   d^2(phi)/dr^2+(1/4r^2+k^2)phi=0
IMPLICIT NONE
    REAL :: K               !Eigenvalue
    REAL :: DK              !Search step
    REAL :: TOLK            !Search tolerance
    REAL :: PHI             !Phi(r=1)
    REAL :: PHIOLD          !Check if sign change
    REAL :: EXACT           !Exact answers

    K = 2.0
    DK = .1
    TOLK = 1E-5
    CALL INTGRT(K,PHI)
    PHIOLD = PHI
10  CONTINUE
        K = K+DK
        CALL INTGRT(K,PHI)
        IF (PHI*PHIOLD < 0) THEN
            K = K-DK
            DK = DK-DK/2.0
        END IF
    IF (ABS(DK) > TOLK) GOTO 10
    EXACT = 2.404826
    PRINT *, 'EIGENVALUE',K,EXACT
    STOP
END PROGRAM

SUBROUTINE INTGRT(K,PHIP)
!   Uses Numerov's method to solve the differential
!   for each input eigenvalue guess and returns
!   the value at r=1 to see if boundary conditions
!   are met. k^2 in numerov's equation is =
!   1/4r^2+K^2 where K is the eigenvalue guess
IMPLICIT NONE
    REAL :: K               !Eigenvalue guess
    REAL :: KSM             !k minus 1 squared
    REAL :: KSN             !k n squared
    REAL :: KSP             !k plus 1 squared
    REAL :: PHIM            !Phi minus 1
    REAL :: PHIN            !Phi n
    REAL :: PHIP            !Phi plus 1
    REAL :: CONST           !Numerov constant
    REAL :: H               !Integration step
    INTEGER :: IR           !Counter
    INTEGER :: NSTEP        !Total iterations
    NSTEP = 10000
    H = 1.0/NSTEP           !Numerator = R_final
    CONST = (H**2)/12.0
    PHIM = .01              !Initial conditions
    PHIN = 0.0
    KSM = 1./(-H)**(2)/4.+K*K
    KSN = 0.0               !Irrelevant placeholder
    DO 10 IR=0,NSTEP-1      !Outward integration
        KSP = 1./((IR+1)*H)**(2)/4.+K*K
        PHIP = (2.-10*CONST*KSN)*PHIN-(1+CONST*KSM)*PHIM
        PHIP = PHIP/(1.0+CONST*KSP)
        PHIM = PHIN         !Rolls values
        PHIN = PHIP
        KSM = KSN
        KSN = KSP
10  CONTINUE
    PRINT *,K,PHIP
    RETURN
END SUBROUTINE
