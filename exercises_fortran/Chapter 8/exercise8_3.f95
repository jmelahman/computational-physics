PROGRAM EXERCISE8_3
IMPLICIT NONE
    REAL :: XI
    REAL :: ETA
    REAL :: FUNC
    REAL :: SUMF,SUMG
    REAL :: FAVE,GAVE
    REAL :: WEIGHT,WEIGHTP
    INTEGER :: I
    INTEGER :: N
    INTEGER :: J
!
    FUNC(XI) = 1./(1.+XI*XI)
    WEIGHT(XI) = 6.*(1.-XI*XI/2.)/5.
    WEIGHTP(XI) = 6.2*(1.-XI*XI/2.)/5.
    PRINT*,'ACTUAL     ',.78540
10  PRINT *,'ENTER NUMBER OF POINTS (0 TO STOP)'
    READ *,N
    IF (N .EQ. 0) STOP
    SUMF = 0.0
    SUMG= 0.0
    J = 0
    DO I=1,N
        CALL INIT_RANDOM_SEED()
        CALL RANDOM_NUMBER(XI)
        CALL RANDOM_NUMBER(ETA)
        SUMG = SUMG+FUNC(XI)/WEIGHT(XI)
        IF (ETA < WEIGHT(XI)/WEIGHTP(XI)) THEN
            J = J+1
            SUMF = SUMF+FUNC(XI)
        END IF
    END DO
    GAVE = SUMG/N
    FAVE = SUMF/J
    PRINT*,'WEIGHTED   ',GAVE,'% ERROR',(GAVE-.78540)/.0078540
    PRINT*,'VON NEUMANN',FAVE,'% ERROR',(FAVE-.78540)/.0078540
    PRINT*,'REJECTED VALUES:',N-J
    GOTO 10
END PROGRAM

SUBROUTINE INIT_RANDOM_SEED()
USE ISO_FORTRAN_ENV, ONLY: INT64
IMPLICIT NONE
    INTEGER, ALLOCATABLE :: SEED(:)
    INTEGER :: I, N, UN, ISTAT, DT(8), PID
    INTEGER(INT64) :: T

    CALL RANDOM_SEED(SIZE = N)
    ALLOCATE(SEED(N))
! FALLBACK TO XOR:ING THE CURRENT TIME AND PID. THE PID IS
! USEFUL IN CASE ONE LAUNCHES MULTIPLE INSTANCES OF THE SAME
! PROGRAM IN PARALLEL.
    CALL SYSTEM_CLOCK(T)
    PID = GETPID()
    T = IEOR(T, INT(PID, KIND(T)))
    DO I = 1, N
        SEED(I) = LCG(T)
    END DO
    CALL RANDOM_SEED(PUT=SEED)
CONTAINS
! THIS SIMPLE PRNG MIGHT NOT BE GOOD ENOUGH FOR REAL WORK, BUT IS
! SUFFICIENT FOR SEEDING A BETTER PRNG.
FUNCTION LCG(S)
    INTEGER :: LCG
    INTEGER(INT64) :: S

    IF (S == 0) THEN
        S = 104729
    ELSE
        S = MOD(S, 4294967296_INT64)
    END IF
    S = MOD(S * 279470273_INT64, 4294967291_INT64)
    LCG = INT(MOD(S, INT(HUGE(0), INT64)), KIND(0))
END FUNCTION LCG
END SUBROUTINE INIT_RANDOM_SEED
