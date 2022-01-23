C=======================================================================
      SUBROUTINE DKMBND(A,V,W,DET,ZERO,IFLAG,NRANK,N,NB)
C=======================================================================

!     arguments
      real(8), dimension (n*(n+1)/2), intent(inout) :: a
      real(8), dimension (n), intent(out)           :: v,w
      real(8), intent(out)                          :: det
      real(8), intent(in)                           :: zero
      integer, dimension(n), intent(out)            :: iflag
      integer, intent(out)                          :: nrank
      integer, intent(in)                           :: n,nb

      real(8)                                       :: WW,XX,DMAX,AMAX,BMAX
      integer                                       :: I,II,IL,IMAX
      integer, external                             :: ihmii,ihmssf

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

      DET=0.D0
      IFLAG(:n)=0

C     PICK OUT DIAGONAL ELEMENTS
      w(:n)=a( (/ (ihmii(i,n),i=1,n) /) )

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
      IF(AMAX.LE.ZERO)then
         print *,'matrix singular -> should not happen'
         stop 'dkmbnd'
      end if

C     SET FLAG
      IFLAG(IMAX)=II
C     ACCUMULATE LOG DETERMINANT
      nrank=nrank+1
      DET=DET+DLOG(AMAX)

C     PICK OUT ELEMENTS FOR ROW IMAX
      v(:n)=0.d0
      DO  I=max0(1,imax-nb),min0(imax+nb,n)
      if(i.eq.imax)cycle
      V(I)=A(ihmssf(i,imax,n) )
      end do

C     TRANSFORM MATRIX
      DO I=max0(1,imax-nb),IMAX-1
      XX=v(i)/DMAX
      W(I)=W(I)-v(i)*XX
      DO J=I+1,IMAX-1
      IJ=ihmssf(i,j,n)
      A(IJ)=A(IJ)-XX*V(J)
      end do
C     ELEMENT A(I,IMAX)
      A(ihmssf(i,imax,n))=XX
      DO J=IMAX+1,min0(imax+nb,n)
      IJ=ihmssf(i,j,n)
      A(IJ)=A(IJ)-XX*V(J)
      end do
      end do
C     ROW IMAX
      W(IMAX)=-1.d0/DMAX
      IJ=ihmii(imax,n)
      DO  J=IMAX+1,min0(imax+nb,n)
      ij=ij+1
      A(IJ)=V(J)/DMAX
      end do
      DO I=IMAX+1,min0(imax+nb,n)
      XX=v(i)/DMAX
      W(I)=W(I)-v(i)*XX
      ij=ihmii(i,n)
      DO J=I+1,min0(imax+nb,n)
      ij=ij+1
      A(IJ)=A(IJ)-XX*V(J)
      end do
      end do

 100  CONTINUE

C     ------------------------------------
C     STORE DIAGONALS BACK & REVERSE SIGNS
C     ------------------------------------

      a(:n*(n+1)/2)=-a(:n*(n+1)/2)
      w(:n)=-w(:n)
      a( (/ (ihmii(i,n),i=1,n) /) )=w(:n)
      RETURN

      END subroutine dkmbnd


