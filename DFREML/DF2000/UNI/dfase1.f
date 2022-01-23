C========================================================================
      SUBROUTINE DFAPSE(NPARM,NCALL,FMIN)
C========================================================================

      use likelihoods
      use names, only : param
      use parameters
      use constants
      use units

      integer, intent(in)                   :: nparm,ncall
      real(8), intent(in)                   :: fmin
      real(8), dimension(:), allocatable    :: qvec,rvec,qhy,qhq,w,q,
     &                                         xmin,xmax,dnvec,gg
      real(8), dimension(:,:), allocatable  :: qq
      integer, dimension(:), allocatable    :: iv,nnpvec
      real(8)                               :: xx,ww,xlike,zz,wdet,dnorm
     &,                                        dnmin,det

      WRITE(IUN66,*)' '
      WRITE(IUN66,*)'-----------------------------------'
      WRITE(IUN66,*)'APPROXIMATION OF SAMPLING VARIANCES'
      WRITE(IUN66,*)'-----------------------------------'
      WRITE(IUN66,*)' '
      WRITE(IUN66,900)'NO. OF LIKELIHOOD VALUES AVAILABLE',NCALL

      NRANGE=12
      NZ=NPARM*(NPARM+3)/2
      mz=NZ+NPARM*(NPARM+1)*(NPARM+2)/6
      npp=nparm*(nparm+1)/2

      allocate (rvec(nrange),qhy(mz),qhq(mz*(mz+1)/2),
     &          q(mz),w(mz),iv(mz),xmin(nparm),xmax(nparm),
     &          dnvec(nrange),qq(mz,nrange),nnpvec(nrange),
     &          gg(npp), stat=ii)
      if(ii>0)stop 'alloc qvec'

      RVEC(1)=0.D0
      XX=1.D0
      DO 1 I=2,11
      RVEC(I)=XX
1     XX=XX-0.1D0
      RVEC(12)=0.05D0

C     QUADRATIC APPROXIMATION OF LIKELIHOOD SURFACE
      NZ=NPARM*(NPARM+3)/2
      IF(NZ.GT.NCALL)stop 'too few points for quad. approx. '
      NFIT=2
      CALL DFQHQ

C     CUBIC APPROXIMATION OF LIKELIHOOD SURFACE
      NFIT=3
      NZ=NZ+NPARM*(NPARM+1)*(NPARM+2)/6
      IF(NZ.GT.NCALL)stop 'too few points for cubic approx.'
      CALL DFQHQ

900   FORMAT(1X,A,T50,'=',I16)

      contains

C     ================
      SUBROUTINE DFQHQ
C     ================
   
      allocate(qvec(nz),stat=ii)
      if(ii>0)stop 'alloc dfqhq'

      NPARM1=NPARM+1
      DNVEC=BIG

      DO 1000 IRANGE=1,NRANGE

C     INITIALIZE
      qhy=0.d0
      qhq=0.d0
      NPOINT=0

C     RANGE OF POINTS TO BE USED
      XMIN=XVEC(:nparm)*( 1.d0-rvec(irange) )
      XMAX=XVEC(:nparm)*( 1.d0+rvec(irange) )

C     SET UP LEAST-SQUARES EQUATIONS FOR COEFFICIENTS OF FUNCTION
      DO  I=1,min0(NCALL,mxfunc)
      IF(dabs(FEVAL(NPARM1,I)-BIG)<10.d-9 .OR.FEVAL(NPARM1,I).EQ.0)
     & cycle
      IF( DABS(FEVAL(NPARM1,I)-FMIN) .LE.ZERO )cycle
      ind=0
      DO  J=1,NPARM
      IF(FEVAL(J,I)<XMIN(J) .OR. FEVAL(J,I)>XMAX(J))ind=1
      end do
      if(ind>0)cycle
C     ... VECTOR OF CONTRIBUTIONS
      NPOINT=NPOINT+1
      W(:nparm)=(FEVAL(:nparm,I)-XVEC(:nparm))
      IJ=NPARM
      DO J=1,NPARM
      IJ=IJ+1
      WW=W(J)
      W(IJ)=WW*WW
      DO  K=J+1,NPARM
      IJ=IJ+1
      W(IJ)=WW*W(K)*2.D0
      end do
      end do
      IF(nfit.EQ.3)THEN
         DO  II=1,NPARM
         DO  JJ=II,NPARM
         DO  KK=JJ,NPARM
         IJ=IJ+1
         IF(II.EQ.JJ.AND.JJ.EQ.KK)THEN
            W(IJ)=W(II)*W(JJ)*W(KK)
         ELSE IF(II.NE.JJ.AND.II.NE.KK.AND.JJ.NE.KK)THEN
            W(IJ)=6.d0*W(II)*W(JJ)*W(KK)
         ELSE
            W(IJ)=3.d0*W(II)*W(JJ)*W(KK)
         END IF
         end do
         end do
         end do
      END IF

C     ... COEFFICIENT MATRIX AND RHS
      XLIKE=-0.5D0*(FEVAL(NPARM1,I)-FMIN)
      IJ=0
      DO J=1,NZ
      DO K=J,NZ
      IJ=IJ+1
      QHQ(IJ)=QHQ(IJ)+W(j)*W(K)
      end do
      end do
      QHY(:nz)=QHY(:nz)+W(:nz)*XLIKE

      end do ! ncall

      IF(NPOINT.LT.NZ)GO TO 1000
      NNPVEC(IRANGE)=NPOINT

C     SOLVE LSQ EQUATIONS
      ZZ=1.D-11
      CALL DKMWHF(QHQ,Q,W,WDET,ZZ,IV,MRANK,NZ,0)
      IF(MRANK.LT.NZ)GO TO 1000
      q=0.d0
      IJ=0
      DO I=1,NZ
      IJ=IJ+1
      WW=QHY(I)
      XX=Q(I)+QHQ(IJ)*WW
      DO  J=I+1,NZ
      IJ=IJ+1
      XX=XX+QHQ(IJ)*QHY(J)
      Q(J)=Q(J)+QHQ(IJ)*WW
      end do
      Q(I)=XX
      end do

      DNORM=dot_product(q(:nparm),q(:nparm))
      DNVEC(IRANGE)=DSQRT(DNORM)
      QQ(:nz,IRANGE)=Q(:nz)

1000  CONTINUE

C     FIND FUNCTION WITH BEST FIT AT MAXIMUM
2099  DNMIN=BIG
      DO I=1,NRANGE
      IF(DNVEC(I).LT.DNMIN)THEN
        DNMIN=DNVEC(I)
        JJMIN=I
      END IF
      end do
      IF(dabs(DNMIN-BIG)<10.d-9)THEN
         WRITE(IUN66,*)'APPROXIMATION OF SAMPLING VARIANCES FAILED '
         go to 1999
      END IF
      QVEC(:nz)=QQ(:nz,JJMIN)

C     APPROXIMATE INFORMATION MATRIX & ITS INVERSE
      GG(:npp)=-2.D0*QQ(NPARM+1:nparm+npp,JJMIN)
      CALL DKMWHF(GG,Q,W,DET,ZERO,IV,KRANK,NPARM,0)
      IF(KRANK.LT.NPARM.OR.DEXP(DET).LT.ZERO)THEN
         IJ=0
         DNVEC(JJMIN)=BIG
         GO TO 2099
      END IF

      IF(nfit.EQ.2)THEN
         WRITE(IUN66,*)'QUADRATIC APPROXIMATION OF LIKELIHOOD :'
      ELSE
         WRITE(IUN66,*)'CUBIC APPROXIMATION OF LIKELIHOOD :'
      END IF
      WRITE(IUN66,900)'NO. OF PARAMETERS OF FUNCTION',NZ
      IF(dabs(DNMIN-BIG)<10.d-9)THEN
         WRITE(IUN66,*)'NO VALID APPROXIMATION FOUND'
         go to 1999
      END IF
      WRITE(IUN66,901)'NORM OF GRADIENT VECTOR',DNMIN
      WRITE(*,901)'NORM OF GRADIENT VECTOR',DNMIN
      WRITE(IUN66,901)'"RANGE" PARAMETER (IN %)',RVEC(JJMIN)*100.D0
      WRITE(IUN66,900)'NO. OF POINTS USED',NNPVEC(JJMIN)

      WRITE(IUN66,*)' '
      WRITE(IUN66,900)'RANK OF APPROXIMATE INFORMATION MATRIX',KRANK
      WRITE(IUN66,901)'LOG DETERMINANT ......................',DET
      WRITE(IUN66,*)' '
      IF(KRANK.EQ.NPARM .AND. DEXP(DET).GT.ZERO)THEN
         INEG=0
         WRITE(IUN66,*)'PARAMETER ESTIMATES WITH THEIR APPROX. S.E.'
         DO  I=1,NPARM
         XX=GG(IHMII(I,NPARM))
         IF(XX.GT.ZERO)THEN
            XX=DSQRT(XX)
         ELSE
            INEG=INEG+1
         END IF
         WRITE(*    ,905)I,PARAM(I),XVEC(I),XX
         WRITE(IUN66,905)I,PARAM(I),XVEC(I),XX
         end do
         WRITE(IUN66,*)' '
         IF(INEG.GT.0)THEN
            WRITE(IUN66,*)'APPROXIMATION OF SAMPLING ERRORS FAILED !'
            WRITE(IUN66,*)'NO. OF NEGATIVE DIAG.S OF I**(-1) =',INEG
         ELSE
            WRITE(IUN66,901)'CONSTANT',-0.5D0*FMIN
            DO I=1,NPARM
            WRITE(IUN66,902)'LINEAR    COEFFCIENT',I,QQ(I,JJMIN)
            end do
            IJ=NPARM
            DO  I=1,NPARM
            DO  J=I,NPARM
            IJ=IJ+1
            WRITE(IUN66,903)'QUADRATIC COEFFICIENT',I,J,QQ(IJ,JJMIN)
            end do
            end do
            IF(nfit.EQ.3)THEN
               DO  I=1,NPARM
               DO  J=I,NPARM
               DO  K=J,NPARM
               IJ=IJ+1
               WRITE(IUN66,913)'CUBIC COEFFICIENT',I,J,K,QQ(IJ,JJMIN)
               end do
               end do
               end do
            END IF
         END IF
      ELSE
         WRITE(IUN66,*)'APPROXIMATE INFORMATION MATRIX ILL-CONDITIONED'
         WRITE(IUN66,*)'- APPROXIMATION OF SAMPLING ERRORS UNSUCCESSFUL'
      END IF
      WRITE(IUN66,*)' '

 1999 deallocate(qvec,stat=ii)
      if(ii>0)stop 'dealloc'
      RETURN

900   FORMAT(1X,A,T50,'=',I16)
901   FORMAT(1X,A,T50,'=',G18.10)
902   FORMAT(1X,A,T36,I4, T50,'=',G16.8)
903   FORMAT(1X,A,T36,2I4,T50,'=',G16.8)
913   FORMAT(1X,A,T36,3I4,T50,'=',G16.8)
904   FORMAT(I4,5G15.6,(/4X,5G15.6))
905   FORMAT(I4,2X,A,T40,':',2G15.6)
908   FORMAT(2I4,2X,A,2X,A,T40,':',F15.3)
      END subroutine dfqhq

      end subroutine dfapse








