!===========================================================================
      SUBROUTINE  DXLIK3 (nparm,parvec,fvalue,joptlk)
!===========================================================================

      use params
      use names
      use comments
      use units
      use times
      use sparse
      use xsprse 
      use derivs
      use parmap
      use combinations
      use likelihoods
      use numbers
      use traces
      use like_components
      use fixcor
      use current
      use dmatrices
      USE solutions
      use today, only : machine

!     arguments
      integer, intent(in)                       :: nparm, joptlk
      real(8), dimension(nparm), intent(inout)  :: parvec
      real(8), intent(out)                      :: fvalue

!     local variables
      real(8)                                   :: xx,vv,xlike,time,
     &                                             xsecs,dnorm2
      real(8), dimension(mxparm)                :: varvec
      integer, save                             :: kkopt
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if(ipvrun.eq.0)then
         ilkpre=-1
         kkopt=0
      end if
      ioptlk=joptlk
      if(ioptlk.eq.2)ioptlk=1

      IF(NTIME(ioptlk+1).LT.3)CALL DFTIME(time,xsecs,isecs,0,'DXLIK3')

      VARVEC(:kparm)=PARVEC(:kparm)*FFVEC(:kparm) ! reverse scaling
      if(kopt.gt.0)call dxsca3 (kparm,kopt,varvec) ! transform back to varcomps

C     ALLOW FOR FIXED UNITY CORRELATIONS
      IF(JPARAM.EQ.1.and.kopt.eq.0)THEN
         DO I=2,NPARM-1
         IF(IPM(I).EQ.2)THEN
            VV=VARVEC(IVARVC(1,I)) * VARVEC(IVARVC(2,I))
            IF(VV.LT.ZERO)THEN
               FVALUE=BIG
               RETURN
            END IF
            XX=CORFIX(I)
            IF(DABS(XX-1.D0)<zz0)XX=XX*0.99999D0
            VARVEC(I)= XX*DSQRT(VV)
            PARVEC(I)= VARVEC(I)/FFVEC(I)
            PRINT 11,'FIX CORR',I,IVARVC(1,I),IVARVC(2,I),xx,varvec(i)
 11         format(1x,a,3i4,2g12.4)
         END IF
         END DO
      END IF

      IF(IOPTLK.EQ.0)THEN
C        LOG L ONLY REQUIRED; CHECK WHETHER POINT HAS BEEN EVALUATED BEFORE
         IPVRUN=1
         DO 400 I=MIN0(NCALL,MXFUNC),1,-1
         DO J=1,KPARM
         IF( DABS(VARVEC(J)-FEVAL(J+1,I)).GT.ZERO )GO TO 400
         END DO
         FVALUE=FEVAL(1,I)
         XLIKE=-0.5d0*fvalue 
         PRINT *,-I,XLIKE
         RETURN
 400     CONTINUE
      END IF
      FVALUE=0.D0

C     TRANSFORM PARAMETERS : FROM K-MATRICES TO COV.MATRICES
      IF(JPARAM.EQ.2)THEN
         CALL DXKTG3 (VARVEC,FVALUE,NPARM,IOUT,0)
         IF(IOUT.EQ.1.OR.dabs(FVALUE-BIG)<zz0) RETURN
      END IF
 
C     SET UP RANDOM EFFECTS  COVARIANCE MATRICES
      CALL DXRAN3 (VARVEC,FVALUE,NPARM,IOUT,IOPTLK)
      IF(IOUT.EQ.1.OR.dabs(FVALUE-BIG)<zz0) RETURN

C     SET UP RESIDUAL COVARIANCE MATRICES
      CALL DxRES3 (VARVEC,FVALUE,NPARM,IOUT,IOPTLK)
      IF(IOUT.EQ.1.OR.dabs(FVALUE-BIG)<zz0) RETURN

c     check if last call to dxlike was a likelihood eval. only and
c     whether it might have been for the current parameters ->
c     if so, can skip cholesky factorisation ! (saves about 20% cpu)
      if(ioptlk.eq.1.and.ilkpre.eq.0)then
        do i=1,nparm
        if( dabs(currnt(i)-varvec(i))>zz0 )go to 90
        end do
        nreuse=nreuse+1
        go to 92
      end if

 90   ILKPRE=IOPTLK
      CURRNT(:nparm)=VARVEC(:nparm)
      if(kkopt.eq.0)kkopt=1
      if(kkopt.gt.10)kkopt=kkopt-10

C     SET UP DATA PART OF MMM
 100  CALL DxMMD3(0)

C     ADD INVERSE COVARIANCE MATRIX OF RANDOM EFFECTS
      CALL DFMMP3(DETL)

C     CARRY OUT CHOLESKY FACTORISATION
      NRANK=NSROW
      CALL DFSPA3 (kkopt)
      if(kkopt.eq.2)go to 100

C     CALCULATE LIKELIHOOD
 92   FVALUE=DET+YPY+dot_product(nncom(:ncomb),dete(:ncomb))
     &                           +NANIM*DETAM+2.D0*DETL*KFIT(1)
      IF(IOPCOV.EQ.1.OR.IOPCOV.EQ.2)FVALUE=FVALUE+2.D0*DETL*KFIT(2)
      IF(IOPCOV.GT.2)FVALUE=FVALUE+NANIM*DETM
      IF(IOPRN1.EQ.1)FVALUE=FVALUE+NRAND1*DETC
      IF(IOPRN3.EQ.1)FVALUE=FVALUE+NRAND3*DETQ
      XLIKE=-0.5d0*fvalue 

!     solutions
      if(joptlk>0 .or. fvalue<ffmin)call back_solve
      if(fvalue<ffmin)then
         ffmin=fvalue
         savsol=solns
      end if

c     derivatives part ...
      if(ioptlk.eq.1)then

c        calculate average information matrix     
         CALL  DXYPY3 (nparm)

         devl2=-0.5d0*dypy2

         if(joptlk.eq.2)go to 900 ! skip 1st deriv.s -> s.e. only

c        calculate first derivatives of the likelihood
         call dxspa3(nparm)

         devl1=0.d0

c        ... contributions from log |R|
         do  ipar=ngparm+1,nparm
         devl1(ipar)=dot_product(nncom(:ncomb),trres1(ipar,:ncomb))
         end do

c        ... contributions from log |G|
         devl1(:ngpar1)=nanim*trran1(:ngpar1)
         if(ioprn1.gt.0)devl1(ngpar1+1:ngpar2)=
     &                               nrand1*trran1(ngpar1+1:ngpar2)
         if(ioprn3.gt.0)devl1(ngpar2+1:ngparm)=
     &                               nrand3*trran1(ngpar2+1:ngparm)

c        ... contributions from log |C| and yPy
         devl1(:nparm)=-0.5d0*(devl1(:nparm)+devc(:nparm))

c        transform derivatives from var. comp. scale to k matrix scale
 900     if(jparam.eq.2)then
            dypy2=devl2 ! app. hess. matrix on var. comp. scale 
            call dxkdv3 (nparm,dnorm2)
          end if
      end if

      ncall=ncall+1
      WRITE(*,1112)NCALL,xLIKE,(PARVEC(L)*ffvec(l),L=1,KPARM)
 1112 FORMAT(I5,F19.9,(1X,T26,4F12.6))
      xl=-0.5d0*(fvalue-2.d0*detl*(kfit(1)+kfit(2)) )

      call house_keeping (ioptlk)

      RETURN

      contains

c     =================================
      subroutine house_keeping (ioptlk)
c     =================================

      integer, intent(in) :: ioptlk
      logical             :: lexist,lopen

      ncalls(ioptlk+1)=ncalls(ioptlk+1)+1
      IF(NTIME(ioptlk+1).LT.3)THEN
         CALL DFTIME(time,xsecs,isecs,14,'DXLIK3')
         BTIME(ioptlk+1)=BTIME(ioptlk+1)+XSECS
         NTIME(ioptlk+1)=NTIME(ioptlk+1)+1
         if(ntime(ioptlk+1).eq.1)then
            INQUIRE(unit=28,opened=lopen)
            if(.not.lopen)open(28,file='Iterates',status='unknown',
     &                           form='formatted',position='append')
            write(28,'(a,f12.3,i4,3x,a)')
     &                      ' time/dxlik3',xsecs,ioptlk,machine
            write(*,'(a,f12.3,i4,3x,a)')
     &                      ' time/dxlik3',xsecs,ioptlk,machine
         end if
      END IF

C     STORE LIKELIHOOD VALUE WITH PERTAINING PARAMETERS
      IF(NCALL.LE.MXFUNC)THEN
         FEVAL(2:kparm+1,NCALL)=vARVEC(:kparm)
         FEVAL(1,NCALL)=FVALUE
      END IF

C     SAVE ALL LIKELIHOODS FOR RETRIEVAL FILE "59"
      OPEN(IUN59,FILE='DF59#DAT',STATUS='unknown',FORM='UNFORMATTED',
     &                                              position='append')

      INQUIRE(unit=iun59,exist=lexist)
      if(.not.lexist)then  ! file does not exits, write out abbreviated header
         II59=-1
         WRITE(IUN59)II59
         write(iun59)kfit,jparam
      end if

      WRITE(IUN59)NCALL,FVALUE,VARVEC(:nparm)
      CLOSE(IUN59)
      ipvrun=1
      return
      end subroutine house_keeping

      END subroutine dxlik3





















