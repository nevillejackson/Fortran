C=======================================================================
      SUBROUTINE CANON(SIGW,SIGS,Q,QI,EIG,IQ,NQ)
C=======================================================================

C     PURPOSE : ROUTINE TO DETERMINE TRANSFORMATION TO CANONICAL SCALE,
C               Q, SUCH THAT :
C                  Q' SIGW Q = IDENTITY MATRIX
C                  Q' SIGS Q = DIAGONAL MATRIX

C     STRATEGY : SEE H. SEAL "MULTIVARIATE ANALYSIS FOR BIOLOGISTS"

C     PARAMETERS
C
C     - SIGW : POSITIVE DEFINITE SYMMETRIC MATRIX OF ORDER NQ,
C              HALFSTORED (UPPER TRIANGLE) IN VECTOR OF LENGTH
C              AT LEAST NQ*(NQ+1)/2, DOUBLE PRECISION
C              INPUT
C     - SIGS : POSITIVE SEMI-DEFINITE SYMMETRIC MATRIX OF ORDER
C              NQ, HALFSTORED (UPPER TRIANGLE) IN VECTOR OF LENGTH
C              AT LEAST NQ*(NQ+1)/2, DOUBLE PRECISION
C              INPUT
C     - Q    : DOUBLE PRECISION MATRIX OF DIMENSION IQ*IQ,
C              CONTAINING THE NORMALIZED EIGENVECTORS OF
C              SIGW**(-1) * SIGS ON EXIT,
C              OUTPUT
C     - QI   : DOUBLE PRECISION MATRIX OF DIMENSION IQ*IQ,
C              CONTAINING THE DIRECT INVERSE OF Q ON EXIT,
C              OUTPUT
C     - EIG  : DOUBLE PRECISION VECTOR OF LENGTH NQ
C              CONTAINING THE EIGENVALUES OF SIGW**(-1)*SIGS ON EXIT,
C              OUTPUT
C     - IQ   : INTEGER VARIABLE, GIVING THE DIMENSION OF Q AND QI,
C              AS DECLARED IN THE CALLING PROGRAM; IQ MUST BE GREATER
C              OR EQUAL TO NQ
C              INPUT
C     - NQ   : INTEGER VARIABLE, GIVING THE NUMBER OF TRAITS, I.E. THE
C              ORDER OF MATRICES,
C              INPUT

C     ERROR STOPS : - DIMENSIONS EXCEEDED
C                   - SIGW NOT POSITIVE DEFINITE

C     SUBROUTINES REQUIRED : EIGNHF
C                            IROWHF
C                            INVRT

C     KARIN MEYER  5/83
C     UPDATE INDEPENDENT OF SYSTEM ROUTINES 12/85
C--------------------------------------------------f90 version -- 9/96  -------

!     arguments
      real(8), dimension(nq*(nq+1)/2), intent(in) :: sigw,sigs
      real(8), dimension(nq), intent(out)         :: eig
      real(8), dimension(iq,iq), intent(out)      :: q,qi
      integer, intent(in)                         :: iq,nq

!     local variables
      real(8), dimension(:,:), allocatable :: V,TEMP, Q1
      real(8), dimension(:), allocatable   :: r
      integer :: max=333,icode
      real(8) :: eps=1.d-6

      IF(IQ.LT.NQ)stop 'ROUTINE "CANON" : IQ<NQ'

      NQQ=NQ*(NQ+1)/2
      allocate(v(nq,nq),temp(nq,nq),q1(nq,nq),r(nqq),stat=ii)
      if(ii>0)stop 'routine "canon" : alloc'

C     DETERMINE EIGENVALUES OF SIGW & CHECK FOR P.D.
      R(:nqq)=SIGW(:nqq)
      ICODE=1
      CALL EIGNHF(R,EIG,EPS,V,NQ,NQ,MAX,ICODE)
      DO I=1,NQ
      IF(EIG(I).LE.0.)THEN
         WRITE(*,*)'ROUTINE "CANON" : "SIGW" IS NOT POSITIVE DEFINITE'
         write(*,*)sigw(:nqq)
         STOP
      END IF
      end do

C     DECOMPOSE SIGW INTO PRODUCT OF TWO MATRICES SIGW**0.5
C     AFTER THIS STEP Q IS A MATRIX SUCH THAT Q * SIGW * Q' IS IDENTITY
      DO J=1,NQ
      DO I=1,NQ
      XX=0.D0
      DO  M=1,NQ
      XX=XX+V(I,M)*V(J,M)*1.D0/DSQRT(EIG(M))
      end do
      Q1(I,J)=XX
      end do
      end do

C     APPLY THE SAME TRANSFORMATION TO THE BETWEEN MATRIX
C     I.E. PRE- AND POSTMULTIPLY SIGS WITH Q (OR Q')
      DO J=1,NQ
      DO I=1,NQ
      XX=0.D0
      DO M=1,NQ
      XX=XX+Q1(M,J)*SIGS( ihmssf(i,m,nq) )
      end do
      TEMP(I,J)=XX
      end do
      end do
      IJ=0
      DO I=1,NQ
      DO J=I,NQ
      IJ=IJ+1
      R(IJ)=dot_product(q1(:,i),temp(:,j))
      end do
      end do

C     DETERMINE EIGENVALUES OF TRANSFORMED BETWEEN MATRIX (Q*SIGS*Q')
      CALL EIGNHF(R,EIG,EPS,V,NQ,NQ,MAX,ICODE)

C     CALCULATE TRANSFORMATION MATRIX
      q=matmul(q1,v)

C     DETERMINE INVERSE OF TRANSFORMATION MATRIX
      qi=q
      CALL INVRT(QI,IQ,NQ)

      deallocate(q1,temp,v,r,stat=ii)
      if(ii>0)stop 'routine "canon" : dealloc'

      RETURN
      END subroutine canon




