PROGRAM EXERCISE3_4
!   Uses Green's function to solve the differential in
!   exercise 3.4. The homogenious solutions are
!   exp(-ar) and exp(ar) where exp(ar) dominates for
!   small r and exp(-ar) dominates for large r
IMPLICIT NONE
    REAL :: PHI             !Dependent variable
    REAL :: R               !Independent variable
    REAL :: PART1           !Integral from 0-r
    REAL :: PART2           !Integral from r-infinity
    REAL :: H               !Counter step
    REAL :: A               !Constant
    REAL :: EXACT           !Exact solution
    INTEGER :: I            !Counter
    INTEGER :: IMAX         !Relates H to Rmax
    H = .1
    PHI = 0.0
    IMAX = 20.0/H
    A = 2.4
    PRINT *,'       R               EXACT           GREEN             ERROR'
    DO I=1,IMAX+1
        R = I*H
        EXACT = (1.-A*A)**(-2)*(EXP(-A*R)-EXP(-R)*(1+(1-A*A)*R/2.))
        CALL INTG1(R,A,PART1)

        CALL INTG2(R,A,PART2)
        PHI = PART1+PART2
        PRINT *,R,EXACT,PHI,EXACT-PHI
    END DO
    STOP
END PROGRAM

SUBROUTINE INTG1(R,A,PART1)
!   Uses Green's function and Gaussian quad
!   to solve the first part of equation. Uses -1/2a
!   to scale the Wronksian to unity
IMPLICIT NONE
    REAL, DIMENSION(64) :: XI,WI!abscissas and weights
    REAL :: A                   !Constant
    REAL :: R                   !independent variable
    REAL :: RD                  !dummy R variable
    REAL :: PART1               !sum of integral
    REAL :: PHILOW              !PHI <
    REAL :: PHIHI               !PHI >
    REAL :: SOURCE              !source function
    REAL :: VAL                 !local variable
    INTEGER :: I                !counter
!Declares functions
    SOURCE(R) = -R*EXP(-R)/2.0
    PHILOW(R) = EXP(A*R)
    PHIHI(R) = -EXP(-A*R)/A/2.0
!Fills arrays for Gaussian quadrature
    CALL FILL64(XI,WI)
    PART1 = 0.0
!dR = dRD*R/2
    DO I=1,64
        RD = (XI(I)+1.0)*R/2.0
        VAL = PHILOW(RD)*SOURCE(RD)*R/2.0
        PART1 = PART1+VAL*WI(I)
    END DO
    PART1 = PART1*PHIHI(R)
    RETURN
END SUBROUTINE

SUBROUTINE INTG2(R,A,PART2)
!   Uses the identity integral f(x)dx from a to b
!   = integral of t^-2f(1/t)dt from 1/b to 1/a to
!   solve the second part of green's function with
!   an infitine boundary
IMPLICIT NONE
    REAL, DIMENSION(64) :: XI,WI!abscissas and weights
    REAL :: A                   !Constant
    REAL :: R                   !independent variable
    REAL :: T                   !T subsitution
    REAL :: PART2               !sum of integral
    REAL :: PHILOW              !PHI <
    REAL :: PHIHI               !PHI >
    REAL :: SOURCE              !source function
    REAL :: VAL                 !local variable
    INTEGER :: I                !counter
!Declares functions
    SOURCE(R) = -R*EXP(-R)/2.0
    PHILOW(R) = EXP(A*R)
    PHIHI(R) = -EXP(-A*R)/A/2.0
!Fills array for Gaussian quadrature
    CALL FILL64(XI,WI)
    PART2 = 0.0
!dR = dT/2R
    DO I=1,64
        T = (XI(I)+1.0)/2.0/R
        VAL = PHIHI(1./T)*SOURCE(1./T)/(T*T*R*2.)
        PART2 = PART2+VAL*WI(I)
    END DO
    PART2 = PART2*PHILOW(R)
    RETURN
END SUBROUTINE

SUBROUTINE FILL64(XI,WI)
!   Fills the two arrays with values for the Gauss-Legendre
!   for N = 64
    real, dimension(64)  :: XI,WI!abscissas and weights

    XI(1) = -0.0243502926634244
    XI(2) = 0.0243502926634244
    XI(3) = -0.0729931217877990
    XI(4) = 0.0729931217877990
    XI(5) = -0.1214628192961206
    XI(6) = 0.1214628192961206
    XI(7) = -0.1696444204239928
    XI(8) = 0.1696444204239928
    XI(9) = -0.2174236437400071
    XI(10) = 0.2174236437400071
    XI(11) = -0.2646871622087674
    XI(12) = 0.2646871622087674
    XI(13) = -0.3113228719902110
    XI(14) = 0.3113228719902110
    XI(15) = -0.3572201583376681
    XI(16) = 0.3572201583376681
    XI(17) = -0.4022701579639916
    XI(18) = 0.4022701579639916
    XI(19) = -0.4463660172534641
    XI(20) = 0.4463660172534641
    XI(21) = -0.4894031457070530
    XI(22) = 0.4894031457070530
    XI(23) = -0.5312794640198946
    XI(24) = 0.5312794640198946
    XI(25) = -0.5718956462026340
    XI(26) = 0.5718956462026340
    XI(27) = -0.6111553551723933
    XI(28) = 0.6111553551723933
    XI(29) = -0.6489654712546573
    XI(30) = 0.6489654712546573
    XI(31) = -0.6852363130542333
    XI(32) = 0.6852363130542333
    XI(33) = -0.7198818501716109
    XI(34) = 0.7198818501716109
    XI(35) = -0.7528199072605319
    XI(36) = 0.7528199072605319
    XI(37) = -0.7839723589433414
    XI(38) = 0.7839723589433414
    XI(39) = -0.8132653151227975
    XI(40) = 0.8132653151227975
    XI(41) = -0.8406292962525803
    XI(42) = 0.8406292962525803
    XI(43) = -0.8659993981540928
    XI(44) = 0.8659993981540928
    XI(45) = -0.8893154459951141
    XI(46) = 0.8893154459951141
    XI(47) = -0.9105221370785028
    XI(48) = 0.9105221370785028
    XI(49) = -0.9295691721319396
    XI(50) = 0.9295691721319396
    XI(51) = -0.9464113748584028
    XI(52) = 0.9464113748584028
    XI(53) = -0.9610087996520538
    XI(54) = 0.9610087996520538
    XI(55) = -0.9733268277899110
    XI(56) = 0.9733268277899110
    XI(57) = -0.9833362538846260
    XI(58) = 0.9833362538846260
    XI(59) = -0.9910133714767443
    XI(60) = 0.9910133714767443
    XI(61) = -0.9963401167719553
    XI(62) = 0.9963401167719553
    XI(63) = -0.9993050417357722
    XI(64) = 0.9993050417357722

    WI(1) = 0.0486909570091397
    WI(2) = 0.0486909570091397
    WI(3) = 0.0485754674415034
    WI(4) = 0.0485754674415034
    WI(5) = 0.0483447622348030
    WI(6) = 0.0483447622348030
    WI(7) = 0.0479993885964583
    WI(8) = 0.0479993885964583
    WI(9) = 0.0475401657148303
    WI(10) = 0.0475401657148303
    WI(11) = 0.0469681828162100
    WI(12) = 0.0469681828162100
    WI(13) = 0.0462847965813144
    WI(14) = 0.0462847965813144
    WI(15) = 0.0454916279274181
    WI(16) = 0.0454916279274181
    WI(17) = 0.0445905581637566
    WI(18) = 0.0445905581637566
    WI(19) = 0.0435837245293235
    WI(20) = 0.0435837245293235
    WI(21) = 0.0424735151236536
    WI(22) = 0.0424735151236536
    WI(23) = 0.0412625632426235
    WI(24) = 0.0412625632426235
    WI(25) = 0.0399537411327203
    WI(26) = 0.0399537411327203
    WI(27) = 0.0385501531786156
    WI(28) = 0.0385501531786156
    WI(29) = 0.0370551285402400
    WI(30) = 0.0370551285402400
    WI(31) = 0.0354722132568824
    WI(32) = 0.0354722132568824
    WI(33) = 0.0338051618371416
    WI(34) = 0.0338051618371416
    WI(35) = 0.0320579283548516
    WI(36) = 0.0320579283548516
    WI(37) = 0.0302346570724025
    WI(38) = 0.0302346570724025
    WI(39) = 0.0283396726142595
    WI(40) = 0.0283396726142595
    WI(41) = 0.0263774697150547
    WI(42) = 0.0263774697150547
    WI(43) = 0.0243527025687109
    WI(44) = 0.0243527025687109
    WI(45) = 0.0222701738083833
    WI(46) = 0.0222701738083833
    WI(47) = 0.0201348231535302
    WI(48) = 0.0201348231535302
    WI(49) = 0.0179517157756973
    WI(50) = 0.0179517157756973
    WI(51) = 0.0157260304760247
    WI(52) = 0.0157260304760247
    WI(53) = 0.0134630478967186
    WI(54) = 0.0134630478967186
    WI(55) = 0.0111681394601311
    WI(56) = 0.0111681394601311
    WI(57) = 0.0088467598263639
    WI(58) = 0.0088467598263639
    WI(59) = 0.0065044579689784
    WI(60) = 0.0065044579689784
    WI(61) = 0.0041470332605625
    WI(62) = 0.0041470332605625
    WI(63) = 0.0017832807216964
    WI(64) = 0.0017832807216964
    RETURN
END SUBROUTINE
