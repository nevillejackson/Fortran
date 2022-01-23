!============================================================================
      SUBROUTINE DFCOR3 (nparm,xvec)
!============================================================================

      use params
      use names
      use units
      use parmap
      use sigmas
      use correlations
      use numbers
      use like_components

!     arguments
      integer, intent(in) :: nparm
      real(8), dimension(mxparm),intent(in) :: xvec

!     local variables
      character(len=8)                      :: PP1
      real(8), dimension(:), allocatable    :: work
      real(8), dimension(:,:), allocatable  :: vv
      real(8)                               :: es,cwrong,xx,xxi,fvalue
      integer                               :: i,j
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      allocate (vv(nq,nq),work(nqq),stat=ii)
      if(ii>0)stop 'dfcor3 : all vv'

C     SET CORRELATIONS TO THIS VALUE IF NOT CALCULABLE
      CWRONG=2.D0

C     -------------------
C     VARIANCE COMPONENTS
C     -------------------

C     ... ADDITIV-GENETIC (DIRECT)
      CALL DxCOVM(NPARM,0,NQ,NQQ,IOUT,3, NQ,SIGA,XVEC,WORK,FVALUE,DETA)
      CALL EIGEm(WORK,EIGA,ZERO,ES,nq,VV,NQ,NNEG)

C     ... RESIDUAL ERROR
      sige=0.d0
      DO I=ISTRT(4)+1,MIN0(ISTRT(4)+NQQ,NPARM)
         READ(PARVAR(I),201)PP1,I1,J1
         IF(PP1.EQ.'SIG E   ')SIGE(J1,I1)=XVEC(I)
      END DO
      work(:nqq)=(/ ((sige(j,i),j=i,nq),i=1,nq) /) 
      SIGP=SIGA+SIGE
      CALL EIGEm(WORK,EIGE,ZERO,ES,nq,VV,NQ,NNEG)

C     ... SECOND ANIMAL EFFECT (MATERNAL)
      IF(IOPRN2.EQ.1)THEN
         CALL DxCOVM(NPARM,ISTRT(1),NQ,NQQ,IOUT,3,NQ,SIGM,XVEC,WORK,
     &                                                  FVALUE,DETM)
         CALL EIGEm(WORK,EIGM,ZERO,ES,nq,VV,NQ,NNEG)
         SIGP=SIGP+SIGM
C        ... COVARIANCE BETWEEN FIRST AND SECOND ANIMAL EFFECTS
         IF(IOPCOV.EQ.2)THEN
            SIGAM=0.D0
 20         FORMAT(A8,2I2)
            DO 4 I=ISTRT(2)+1,MIN0(ISTRT(2)+NQSQ,NPARM)
            READ(PARVAR(I),20)PP1,I1,J1
            IF(PP1.NE.'SIG AM  ')GO TO 5
            SIGP(J1,I1)=SIGP(J1,I1)+XVEC(I)*0.5D0
            SIGP(I1,J1)=SIGP(I1,J1)+XVEC(I)*0.5D0
 4          SIGAM(J1,I1)=XVEC(I)
         END IF
      END IF

C     ... ADDITIONAL RANDOM EFFECT
 5    IF(IOPRN1.EQ.1)THEN
         CALL DxCOVM(NPARM,ISTRT(3),NQ,NQQ,IOUT,3,NQ,
     *               SIGC,XVEC,WORK,FVALUE,DETC)
         CALL EIGEm(WORK,EIGC,ZERO,ES,nq,VV,NQ,NNEG)
         SIGP=SIGP+SIGC
      END IF
      IF(IOPRN3.EQ.1)THEN
         CALL DxCOVM(NPARM,ISTRT(7),NQ,NQQ,IOUT,3,NQ,
     *               SIGq,XVEC,WORK,FVALUE,DETq)
         CALL EIGEm(WORK,EIGQ,ZERO,ES,nq,VV,NQ,NNEG)
         SIGP=SIGP+SIGQ
      END IF

C     ... PERMANENT ENVIRONMENTAL VARIANCE
      IF(IRPMOD.EQ.2)THEN
         DO I=ISTRT(4)+1,MIN0(ISTRT(4)+NQQ,NPARM)
            READ(PARVAR(I),201)PP1,I1,J1
            IF(PP1.EQ.'SIG R+E ')THEN
               SIGE(J1,I1)=SIGE(J1,I1)+XVEC(I)
               SIGP(J1,I1)=SIGP(J1,I1)+XVEC(I)
            END IF
         END DO
         SIGR=0.D0
         DO I=ISTRT(5)+1,MIN0(ISTRT(5)+NQQ,NPARM)
            READ(PARVAR(I),201)PP1,I1,J1
            IF(PP1.EQ.'SIG R   ')SIGR(J1,I1)=XVEC(I)
         END DO
         SIGE=SIGE+SIGR
         SIGP=SIGP+SIGR
         work(:nqq)=(/ ((sigr(j,i),j=i,nq),i=1,nq) /) 
         CALL EIGEm(WORK,EIGR,ZERO,ES,nq,VV,NQ,NNEG)
      END IF

c     measurement errors
      if(iomease.eq.1)then
        do i=1,nq
        sigp(i,i)=sigp(i,i)+xvec(nparm+i)
        end do
      end if
      work(:nqq)=(/ ((sigp(j,i),j=i,nq),i=1,nq) /) 
      CALL EIGEm(WORK,EIGP,ZERO,ES,nq,VV,NQ,NNEG)

C     ----------------------
C     CALCULATE CORRELATIONS
C     ----------------------

      allocate(rra(nqq),rrm(nqq),rram(nq,nq),rrr(nqq),rrc(nqq), 
     &            rre(nqq),rrp(nqq),rrq(nqq),stat=ii)
      if(ii>0)stop 'alloc : correlations'

      IJ=0
      DO I=1,NQ
      IJ=IJ+1
c     ... check that phenotypic variances are sensible
      XX=SIGP(I,I)
      if(XX.GT.ZERO .AND. XX.LT.1.D0/ZERO)THEN
         XXI=1.D0/XX
      ELSE
         WRITE(*,*)'ROUTINE "DFCORR" : PHENOTYPIC VARIANCE ZERO ?'
         WRITE(*,*)'                   TRAIT NO. =',I
         WRITE(*,*)'                   VARIANCE  =',XX,ZERO
         XXI=0.D0
      END IF
c     ... calculate heritabilities etc.
      RRA(IJ)=SIGA(I,I)*XXI
      RRE(IJ)=SIGE(I,I)*XXI
      RRP(IJ)=1.D0
      IF(IOPRN2.EQ.1)RRM(IJ)=SIGM(I,I)*XXI
      IF(IOPRN1.EQ.1)RRC(IJ)=SIGC(I,I)*XXI
      IF(IOPRN3.EQ.1)RRQ(IJ)=SIGQ(I,I)*XXI
c     ... calculate correlations
      DO J=I+1,NQ
         IJ=IJ+1
         RRA(IJ)=CORRL(SIGA(I,I),SIGA(J,J),SIGA(J,I),ZERO,CWRONG)
         RRE(IJ)=CORRL(SIGE(I,I),SIGE(J,J),SIGE(J,I),ZERO,CWRONG)
         RRP(IJ)=CORRL(XX,SIGP(J,J),SIGP(J,I),ZERO,CWRONG)
         IF(IOPRN2.EQ.1)RRM(IJ)=CORRL(SIGM(I,I),SIGM(J,J),SIGM(J,I),
     *                                                   ZERO,CWRONG)
         IF(IOPRN1.EQ.1)RRC(IJ)=CORRL(SIGC(I,I),SIGC(J,J),SIGC(J,I),
     *                                                   ZERO,CWRONG)
         IF(IOPRN3.EQ.1)RRQ(IJ)=CORRL(SIGQ(I,I),SIGQ(J,J),SIGQ(J,I),
     *                                                   ZERO,CWRONG)
         IF(IRPMOD.EQ.2)RRR(IJ)=CORRL(SIGR(I,I),SIGR(J,J),SIGR(J,I),
     *                                                   ZERO,CWRONG)
      END DO
      IF(IOPCOV.EQ.2)THEN
         DO  J=1,NQ
         RRAM(J,I)=CORRL(SIGA(I,I),SIGM(J,J),SIGAM(J,I),ZERO,CWRONG)
         end do
      END IF
      END DO
      deallocate(vv,work,stat=ii)
      if(ii>0)stop 'dealloc dfcor3'
      return
 201  FORMAT(A8,2I2)

      contains

!     ==========================================================
      double precision function corrl (var1,var2,cov,zero,errval)
!     ==========================================================

      real(8), intent(in) :: var1,var2,zero,errval,cov

      if(var1.lt.zero.or.var2.lt.zero)then
         corrl=errval
      else
         corrl=cov/dsqrt(var1*var2)
      end if
      return
      end function corrl

      END subroutine dfcor3








