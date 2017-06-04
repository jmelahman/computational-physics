!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   USES GAUSS-JORDAN METHOD TO FIND THE INVERSE OF A NxN MATRIX
    PROGRAM EXERCISE5_2
!   INCORPORTATES PIVOTING IN SUBROUTINE 'PIVOT'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE
    REAL,ALLOCATABLE,DIMENSION(:,:) :: A    !INPUT MATRIX
    REAL,ALLOCATABLE,DIMENSION(:,:) :: I    !IDENTITY/INVERSE MATRIX
    REAL :: X               !DUMMY VARIABLE
    INTEGER :: N            !MATRIX SIZE
    INTEGER :: J            !COUNTER
    INTEGER :: P            !PIVOT COUNTER
    INTEGER :: ROW          !ROW COUNTER
    INTEGER :: COL          !COLUMN COUNTER
!
10  PRINT *,'N'
    READ *,N
    P=0
    ALLOCATE(A(N,N))
    ALLOCATE(I(N,N))
!   CREATES THE IDENTITY MATRIX
    DO ROW=1,N
        DO COL=1,N
            IF(ROW == COL)THEN
                I(ROW,COL)=1.
            ELSE
                I(ROW,COL)=0.
            END IF
        END DO
    END DO
    PRINT *,'ENTER VALUES FOR MATRIX A'
    DO COL=1,N
        DO ROW=1,N
            PRINT *,'ROW',ROW,'COLUMN',COL
            READ *,A(ROW,COL)
        END DO
    END DO
    PRINT *,'ORIGINAL'
    PRINT *,'A'
    DO COL=1,N
        PRINT *,A(:,COL)
    END DO
!   BEGINS CALCULATIONS
    DO J=1,N-1
        DO COL=J+1,N
            IF (ABS(A(J,J)) < .0000001) THEN
                CALL PIVOT(J,A,P,I,N)
            END IF
            X = A(J,COL)/A(J,J)
!   REMOVES ROW J FROM ROWS >J
            DO ROW=1,N
                A(ROW,COL)=A(ROW,COL)-(A(ROW,J)*X)
                I(ROW,COL)=I(ROW,COL)-(I(ROW,J)*X)
            END DO
        END DO
        IF (ABS(A(J+1,J+1)) < .0000001) THEN
            CALL PIVOT(J+1,A,P,I,N)
        END IF
        X=A(J+1,J+1)
!   SETS LEADING TERM OF COLUMN J+1 TO UNITY
        DO ROW=1,N
            A(ROW,J+1)=A(ROW,J+1)/X
            I(ROW,J+1)=I(ROW,J+1)/X
        END DO
    END DO
!   MATRIX A IS NOW A LOWER TRIANGULAR WITH UNITY DIAGNOL
    DO J=N,2,-1
        DO COL=J-1,1,-1
            X=A(J,COL)
            DO ROW=1,N
                I(ROW,COL)=I(ROW,COL)-I(ROW,J)*X
                A(ROW,COL)=A(ROW,COL)-A(ROW,J)*X
            END DO
        END DO
    END DO
!   ENSURES A(1,1) IS 1 AND THUS COMPLETING THE IDENTITY
    X=A(1,1)
    DO ROW=1,N
        A(ROW,1)=A(ROW,1)/X
        I(ROW,1)=I(ROW,1)/X
    END DO
    PRINT *,'FINAL'
    PRINT *,'A'
    DO COL=1,N
        PRINT *,A(:,COL)
    END DO
    PRINT *,'I'
    DO COL=1,N
        PRINT *,I(:,COL)
    END DO
    PRINT *,'NUMBER OF ROW INTERCHANGES',P
    DEALLOCATE(A)
    DEALLOCATE(I)
!    GOTO 10
END PROGRAM

SUBROUTINE PIVOT(J,A,P,I,N)
IMPLICIT NONE
    REAL,DIMENSION(N,N) :: A,I
    REAL,DIMENSION(N) :: D
    REAL :: X
    INTEGER :: J
    INTEGER :: B
    INTEGER :: P
    INTEGER :: N
    INTEGER :: COL
    INTEGER :: ROW
    P=P+1
    X=ABS(A(J,J))
    DO ROW=J+1,N
        IF (ABS(A(ROW,J))>X)THEN
            X=ABS(A(ROW,J))
            B=ROW
        END IF
    END DO
    IF (ABS(X)<.0000001) THEN
        PRINT *,'NO INTERCHANGE POSSIBLE'
        PRINT *,'MATRIX A IS SINGULAR'
        STOP
    END IF
    PRINT *,'SWITCHING ROW',J,'WITH ROW',B
    DO COL=1,N
        D(COL)=A(J,COL)
        A(J,COL)=A(B,COL)
        A(B,COL)=D(COL)
        D(COL)=I(J,COL)
        I(J,COL)=I(B,COL)
        I(B,COL)=D(COL)
    END DO
    RETURN
END SUBROUTINE

