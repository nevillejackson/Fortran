C=======================================================================
      SUBROUTINE DKMWHF(A,V,W,DET,ZERO,IFLAG,NRANK,N,IOPT)
C=======================================================================

!     arguments
      real(8), dimension (n*(n+1)/2), intent(inout) :: a
      real(8), dimension (n), intent(out)           :: v,w
      real(8), intent(out)                          :: det
      real(8), intent(in)                           :: zero
      integer, dimension(n), intent(out)            :: iflag
      integer, intent(out)                          :: nrank
      integer, intent(in)                           :: n,iopt

      real(8) :: WW,XX,DMAX,AMAX,BMAX, DIMAX
      integer :: NEG, I,II,N1,IL,IMAX
      integer, external :: ihmii,ihmssf

C     ------------------
C     MATRIX IS A SCALAR
C     ------------------

      IF(N.EQ.1)THEN
         XX=A(1)
         IF(DABS(XX).GT.ZERO)THEN
            A(1)=1.D0/XX
            NRANK=1
         ELSE
            A(1)=0.D0
            NRANK=0
         END IF
         W(1)=A(1)
         IF(XX.GT.ZERO)THEN
            DET=DLOG(XX)
         ELSE
            DET=0.D0
         END IF
         RETURN
      END IF

C     ----------
C     INITIALIZE
C     ----------

      NEG=0
      DET=0.D0
      N1=N+1
      DO 1 I=1,N
1     IFLAG(I)=0

C     PICK OUT DIAGONAL ELEMENTS
      II=-N
      DO 101 I=1,N
      II=II+N1
      W(I)=A(II)
 101  II=II-I

C     --------------------------
C     GAUSSIAN ELIMINATION STEPS
C     --------------------------

      nrank=0
      DO 100 II=1,N

C     FIND DIAG. ELEMENT WITH LARGEST ABSOLUTE VALUE (PIVOT)
      DMAX=0.D0
      AMAX=0.D0
      DO  I=1,N
      IF(IFLAG(I).Eq.0)then
         BMAX=DABS(W(I))
         IF(BMAX.GT.AMAX)THEN
            DMAX=W(I)
            AMAX=BMAX
            IMAX=I
         END IF
      end if
      end do

C     CHECK FOR SINGULARITY
      IF(AMAX.LE.ZERO)GO TO 11
      IF(IOPT.EQ.1.AND.AMAX.LT.1.D-5)PRINT *,'SMALL PIVOT ',II,DMAX
C     SET FLAG
      IFLAG(IMAX)=II
C     ACCUMULATE LOG DETERMINANT
      nrank=nrank+1
      DET=DET+DLOG(AMAX)
      IF(DMAX.LT.0.D0) NEG=NEG+1
      DIMAX=1.D0/DMAX
      IMAXM1=IMAX-1
      IMAXP1=IMAX+1

C     PICK OUT ELEMENTS FOR ROW IMAX
      IL=IMAX-N
      DO 3 I=1,IMAXM1
      IL=IL+N1-I
 3    V(I)=A(IL)
      IL=IL+N1-IMAX
      DO 4 I=IMAXP1,N
      IL=IL+1
 4    V(I)=A(IL)

C     TRANSFORM MATRIX
      IJ=0
      DO 7 I=1,IMAXM1
      WW=V(I)
      IF(WW.EQ.0.D0)THEN
         IJ=IJ+N1-I
      ELSE
         XX=WW*DIMAX
         IJ=IJ+1
         W(I)=W(I)-WW*XX
         DO 71 J=I+1,IMAXM1
         IJ=IJ+1
71       A(IJ)=A(IJ)-XX*V(J)
C        ELEMENT A(I,IMAX)
         IJ=IJ+1
         A(IJ)=XX
         DO 72 J=IMAXP1,N
         IJ=IJ+1
72       A(IJ)=A(IJ)-XX*V(J)
      END IF
 7    CONTINUE

C     ROW IMAX
      IJ=IJ+1
      W(IMAX)=-DIMAX
      DO 73 J=IMAXP1,N
      IJ=IJ+1
 73   A(IJ)=V(J)*DIMAX

      DO 74 I=IMAXP1,N
      WW=V(I)
      IF(WW.EQ.0.D0)THEN
         IJ=IJ+N1-I
      ELSE
         XX=WW*DIMAX
         IJ=IJ+1
         W(I)=W(I)-WW*XX
         DO 75 J=I+1,N
         IJ=IJ+1
 75      A(IJ)=A(IJ)-XX*V(J)
      END IF
 74   CONTINUE

 100  CONTINUE

C     ------------------------------------
C     STORE DIAGONALS BACK & REVERSE SIGNS
C     ------------------------------------

      IJ=0
      DO 9 I=1,N
      IJ=IJ+1
      WW=-W(I)
      W(I)=WW
      A(IJ)=WW
      DO 90 J=I+1,N
      IJ=IJ+1
 90   A(IJ)=-A(IJ)
 9    CONTINUE
c      NRANK=N
 300  IF(IOPT.EQ.1.AND.NEG.GT.0)PRINT *,'NO. OF NEGATIVE PIVOTS =',
     *                                                         NEG
      RETURN

C      ---------------------------------------------------
C      MATRIX NOT OF FULL RANK, RETURN GENERALISED INVERSE
C      ---------------------------------------------------

c   11 NRANK=II-1
 11   IJ=0

      DO 14 I=1,N
      IF(IFLAG(I).EQ.0)THEN
C        ... ZERO OUT ROW/COLUMN
         W(I)=0.D0
         DO 12 J=I,N
         IJ=IJ+1
 12      A(IJ)=0.D0
      ELSE
         IJ=IJ+1
         WW=-W(I)
         W(I)=WW
         A(IJ)=WW
         DO 13 J=I+1,N
         IJ=IJ+1
         IF(IFLAG(J).NE.0)THEN
            A(IJ)=-A(IJ)
         ELSE
            A(IJ)=0.D0
         END IF
 13      CONTINUE
      END IF
 14   CONTINUE

      IF(IOPT.EQ.1)PRINT 15,N,NRANK
   15 FORMAT(' GENERALISED INVERSE OF MATRIX WITH ORDER =',I5,
     *                                     '   AND RANK =',I5)
      GO TO 300
      END subroutine dkmwhf
