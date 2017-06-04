PROGRAM EXERCISE3_2
!   Uses Numerov integration and the asymptopic
!   behavior to integrate inward from r=20
IMPLICIT NONE
    REAL, DIMENSION (0:21) :: PHI
    REAL :: EXACT           !Eaxct value
    REAL :: SOURCE          !Source function
    REAL :: CONST           !Numerov constant
    REAL :: H
    REAL :: R
    REAL :: SM              !Source minus 1
    REAL :: SN              !Source n
    REAL :: SP              !Source plus 1
    INTEGER :: IR
    INTEGER :: NSTEP
    EXACT(R) = 1.0-(R+2)*EXP(-R)/2.0
    SOURCE(R) = -R*EXP(-R)/2.0
    H = 1.
    NSTEP = 20.0/H
    CONST = H*H/12.0
    SN = SOURCE(20.)
    SP = SOURCE(21.)
!Uses asymptopic behvaior for initial values
    PHI(20) = 1.0
    PHI(21) = 1.0
!Works backgrounds, inward integration
    DO IR=NSTEP,1,-1
        R = (IR-1)*H        !Defines R
        SM = SOURCE(R)
        PHI(IR-1)=2.0*PHI(IR)-PHI(IR+1)+CONST*(SM+SP+10.0*SN)
        SP = SN             !Rolls values
        SN = SM
        PRINT *,R+1,EXACT(R+1),PHI(R+1),EXACT(R+1)-PHI(R+1)
    END DO
    STOP
END PROGRAM
