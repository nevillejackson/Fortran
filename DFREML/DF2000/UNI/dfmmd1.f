C============================================================================
      SUBROUTINE  DFMMD1 (nparm,parvec,fvalue)
C============================================================================

      use sparse
      use xsprse
      use lnk
      use diagonal
      use order
      use rows
      use like
      use levels
      use constants
      use units
      use numbers
      
      real(8), dimension(nparm), intent(in) :: parvec
      real(8), intent(inout)                :: fvalue
      integer, intent(in)                   :: nparm

C     -------------------------------------
C     ASSEMBLE AUGMENTED COEFFICIENT MATRIX
C     -------------------------------------

      REWIND(IUN52)
      IVEC(:lim1a)=IEQNEW(:lim1a)
      ROW=1.D0

      XSPARS=0.D0
      DIA=0.D0
      RHS=0.D0
      XHX=0.D0

150   READ(IUN52,END=199)KABS,IVEC(LIM1a+1:KABS),ROW(:LIM1b)

C     SS/CP OF TRAITS
      DO I=1,NQ
      XHX(:i,I)=XHX(:i,I)+ROW(I)*ROW(:i)
      end do

      DO IABS=NQ+1,KABS
      IROW=IVEC(IABS)

      IF(IABS.LE.LIM1b)THEN           ! RIGHT HAND SIDES
         RHS(:nq,IROW)=RHS(:nq,IROW)+ROW(:nq)*ROW(IABS)
      ELSE
         RHS(:nq,IROW)=RHS(:nq,IROW)+ROW(:nq)
      END IF

      DO JABS=NQ+1,IABS-1
      k=isploc(irow,ivec(jabs))
      IF(IABS.LE.LIM1b .AND. JABS.LE.LIM1b)THEN
         XSPARS(K)= XSPARS(K)+ROW(IABS)*ROW(JABS)
      ELSE IF(JABS.LE.LIM1b)THEN
         XSPARS(K)= XSPARS(K)+ROW(JABS)
      ELSE
         XSPARS(K)= XSPARS(K)+1.d0
      END IF
      end do
      DIA(IROW)=DIA(IROW)+ROW(IABS)*ROW(IABS)
      end do ! irow
      GO TO 150

 199  RETURN

      END subroutine dfmmd1

c=============================================================================
      integer function isploc(irow,icol)
c=============================================================================

      use sparse
      integer, intent(in) :: irow,icol
      integer             :: ii,jj,ksub,k

      II=MAX0(IROW,ICOL)
      JJ=MIN0(IROW,ICOL)
      KSUB=IXVEC2(JJ)
      DO K=IXVEC1(JJ),IXVEC1(JJ+1)-1
      IF(IXSUB(KSUB).EQ.II)then
        isploc=k
!        if(k.eq.0)print *,ii,jj,ixvec1(jj),ixvec1(jj+1),ksub        
        return
      end if
      KSUB=KSUB+1
      end do
      PRINT *,'ERR ISPLOC : KSUB, K',KSUB,K
      print *,'irow, icol',irow,icol,'  ii=',ii,'  jj=',jj
      print *,'range',ixvec1(jj),ixvec1(jj+1)-1
      print *,'orig',ivperm(ii),ivperm(jj)
      STOP
      end function isploc







