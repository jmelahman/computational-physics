PROGRAM EXERCISE5_5
IMPLICIT NONE
    REAL,DIMENSION(:,:),ALLOCATABLE :: A,P
    REAL,DIMENSION(:,:),ALLOCATABLE :: V
    REAL :: ALPHA
    REAL :: R
    INTEGER :: ROW,COL
    INTEGER :: N
    INTEGER :: K
    INTEGER :: J
    N=4
    ALLOCATE(A(N,N),P(N,N),V(N,N))
    DO ROW=1,N
        DO COL=1,N
            PRINT*,'ROW',ROW,'COL',COL
            READ*,A(ROW,COL)
        END DO
    END DO
    PRINT *,'ORIGINAL MATRIX'
    DO ROW=1,N
        PRINT *,A(ROW,:)
    END DO
    DO K=1,N-1
        CALL FILLIDENTITY(P,N)
        ALPHA=0.0
        V=0.0
        DO J=K+1,N
            ALPHA=ALPHA+A(J,K)**2
        END DO
        ALPHA=-SIGN(1.,A(K+1,K))*SQRT(ALPHA)
        R=SQRT((ALPHA*ALPHA-A(K+1,K)*ALPHA)/2.)
        DO ROW=K,N
            IF(ROW==K)THEN
                V(ROW,K)=0
                V(K,ROW)=0
            ELSE IF(ROW==K+1)THEN
                V(ROW,K)=(A(ROW,K)-ALPHA)/(2.*R)
                V(K,ROW)=V(ROW,K)
            ELSE
                V(ROW,K)=A(ROW,K)/(2.*R)
                V(K,ROW)=V(ROW,K)
            END IF
        END DO
        DO ROW=K+1,N
            DO COL=K+1,N
                V(ROW,COL)=V(ROW,K)*V(K,COL)
            END DO
        END DO
        DO ROW=1,K
            DO COL=1,N
                V(ROW,COL)=0.0
            END DO
        END DO
        DO COL=1,K
            DO ROW=1,N
                V(ROW,COL)=0.0
            END DO
        END DO

        DO ROW=1,N
            DO COL=1,N
                P(ROW,COL)=P(ROW,COL)-2*V(ROW,COL)
            END DO
        END DO
        A=MATMUL(P,A)
        A=MATMUL(A,P)
    END DO
    PRINT *,'RESULTING MATRIX'
    DO ROW=1,N
        PRINT *,A(ROW,:)
    END DO
    PRINT *,'EXACT ANSWER'
    PRINT *,4.,-3.,0.,0.
    PRINT *,-3.,2.,3.16227770,0.
    PRINT *,0.,3.1622777,-1.4,-.2
    PRINT *,0.,0.,-.2,1.4
END PROGRAM

SUBROUTINE FILLIDENTITY(I,N)
IMPLICIT NONE
    REAL,DIMENSION(N,N) :: I
    INTEGER :: N
    INTEGER :: ROW,COL
    DO ROW=1,N
        DO COL=1,N
            IF(ROW == COL)THEN
                I(ROW,COL)=1.
            ELSE
                I(ROW,COL)=0.
            END IF
        END DO
    END DO
    RETURN
END SUBROUTINE
