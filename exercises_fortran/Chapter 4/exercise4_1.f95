PROGRAM EXERCISE4_1
IMPLICIT NONE
    REAL :: X
    REAL :: PZ,PM,PP,PL
    INTEGER :: L
    INTEGER :: IL
!
20  PRINT *, 'ENTER X,L (L < 0 TO STOP'
    READ *,X,L
    IF (L < 0.0) THEN
        STOP
    ELSE IF (L == 0) THEN
        PL=0.
    ELSE IF (L == 1) THEN
        PL=X
    ELSE
        PM=1.
        PZ=X
        DO IL=1,L-1
            PP=((2*IL+1)*X*PZ-IL*PM)/(IL+1)
            PM=PZ
            PZ=PP
        END DO
    END IF
    PL=PZ
    PRINT *,X,L,PL
    PZ=PM
    DO IL=L-1,1,-1
        PM=((2*IL+1)*X*PZ-(IL+1)*PP)/IL
        PP=PZ
        PZ=PM
    END DO
    PL=PZ
    PRINT *,X,L,PL,PP
    GO TO 20
END PROGRAM
