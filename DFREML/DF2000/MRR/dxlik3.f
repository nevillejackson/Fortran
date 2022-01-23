!===========================================================================
      SUBROUTINE DXLIK3 (parvec,fvalue,xlike,joptlk)
!===========================================================================

      use parameters, only : mxparm
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
      use current
      use dmatrices
      use eigen_decomp
      use solutions
      use ages
      use residuals, only : eevec
      use today, only : machine
      use order

!     arguments
!      implicit none
      integer, intent(in)                        :: joptlk
      real(8), dimension(kparm), intent(inout)   :: parvec
      real(8), intent(out)                       :: fvalue,xlike

!     local variables
      real(8)                                    :: time,xsecs,sige,xx,
     &                                              qq
      real(8), dimension(mxparm)                 :: varvec
      integer, save                              :: kkopt
      integer                                    :: iout,ll,i,j,ipar,ii,
     &                                              ii59,ndf1,mcall,im,
     &                                              ndf2,krank,ioptlk
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if(ipvrun.eq.0)call first_time
      ioptlk=0
      if(joptlk.eq.1.or.joptlk.eq.2)ioptlk=1
      IF(NTIME(ioptlk+1).LT.3)CALL DFTIME(time,xsecs,isecs,0,'DXLIK3')

      IF(IOPTLK.EQ.0)THEN
C        LOG L ONLY REQUIRED; CHECK WHETHER POINT HAS BEEN EVALUATED BEFORE
         DO I=MIN0(NCALL,MXFUNC),1,-1
         DO J=1,kPARM
         IF( DABS(PARVEC(J)-FEVAL(J+1,I)).GT.ZZ0 )go to 400
         END DO
         FVALUE=FEVAL(1,I)
         XLIKE=-0.5d0*fvalue -DETL*KSFIT(1)
         IF(IOPCOV.EQ.1.OR.IOPCOV.EQ.2)xLIKE=xLIKE-DETL*KSFIT(2)
         write(*,'(i6,f20.9)') -I,XLIKE
         RETURN
 400     continue
         end do
      END IF

!     transform back to var comp scale
      VARVEC(:kparm)=PARVEC(:kparm)*FFVEC(:kparm)
      if(kopt.eq.3.or.kopt.eq.1)then
         call dxsca33(varvec)
      else if(kopt>0)then          ! kopt=2 -> s.p.smith routines
         call dxsca3(kopt,varvec)
      end if
      FVALUE=0.D0
      iout=0

C     SET UP RANDOM EFFECTS  COVARIANCE MATRICES
      CALL DXRAN3 (VARVEC,FVALUE,IOUT,IOPTLK)

      IF(IOUT.EQ.1.OR.dabs(FVALUE-BIG)<zz0) RETURN
C     SET UP RESIDUAL COVARIANCE MATRICES
      if(nq.eq.1.and.ieqmod.eq.0 .and. kfoume.eq.0)then
         CALL DxRES3 (VARVEC,FVALUE,IOUT,IOPTLK)
      else if(nq.eq.1)then
         CALL DxRAZ3 (VARVEC,FVALUE,IOUT,IOPTLK)
      else
         CALL DXREZ3 (VARVEC,FVALUE,IOUT,IOPTLK)
      end if
      IF(IOUT.EQ.1.OR.dabs(FVALUE-BIG)<zz0) RETURN

c     check if last call to dxlike was a likelihood eval. only and
c     whether it might have been for the current parameters ->
c     if so, can skip cholesky factorisation ! (saves about 20% cpu)
      if(ioptlk.eq.1.and.ilkpre.eq.0)then
        do i=1,nparm
        if(dabs(currnt(i)-varvec(i)).gt.1.d-8)go to 90
        end do
        nreuse=nreuse+1
        go to 92
      end if

 90   ILKPRE=IOPTLK
      CURRNT(:nparm)=VARVEC(:nparm)
      if(mfitmx < kfitmx)kkopt=2
      if(kkopt.eq.0)kkopt=1
      if(kkopt.gt.10)kkopt=kkopt-10

C     SET UP DATA PART OF MMM
100   CALL DxMMD3(VARVEC,0,0)
C     ADD INVERSE COVARIANCE MATRIX OF RANDOM EFFECTS
      CALL DFMMP3(DETL)

C     CARRY OUT CHOLESKY FACTORISATION
      KRANK=0
      NRANK=NSROW
      CALL DFSPA3 (kkopt)
      if(kkopt.eq.2)go to 100

C     CALCULATE LIKELIHOOD
 92   FVALUE=DET+YPY
      if(nanim>0)FVALUE=FVALUE+NANIM*DETAM
      IF(IOPCOV.GT.2)FVALUE=FVALUE+NANIM*DETM
      IF(IOPRN1.EQ.1 .and. ieqmod.eq.0 )FVALUE=FVALUE+NRAND1*DETC
      IF(IOPRN3.EQ.1)FVALUE=FVALUE+NRAND3*DETQ

!     estimate sigeps directly for DF algorithm 
      qq=1.d0
      IF(iopeps.EQ.1)THEN          ! only for nq=1 !!
         NDF2=NREC-NRANK+KRANK
         NDF1=NDF2+(ksfit(1)+ksfit(2))*NANIM+ksfit(4)*NRAND1
     &                                      +ksfit(5)*nrand3
         qq=ypy/dble(ndf1)
         sige=qq*varvec(nparm)
         varvec(nparm)=sige
         if(kopt.eq.0.or.kopt.eq.4)then
            parvec(kparm)=sige/ffvec(kparm)
         else if(kopt.eq.1.or.kopt.eq.3)then
            parvec(kparm)=dsqrt(sige)/ffvec(kparm)
         else if(kopt.eq.2)then
            parvec(kparm)=dlog( dsqrt(sige) ) /ffvec(kparm)
         end if
!         eevec=eevec*qq ! do not use updated sigeps -> convergence problems !
         im=1
         do i=1,nage(im)
         fvalue=fvalue+nnage(i,1,im)*dlog(eevec(i,1)) ! for diagonal m.e. var!
         end do

      else if(nq.eq.1. and. ieqmod.eq.0 .and.kfoume.eq.0 )then
         xx=0.d0
         do i=1,ncomb
         FVALUE=fvalue+nncom(i)*dete(i)
         xx=xx+nncom(i)*dete(i)
         end do
      else if(nq>1 .or. ieqmod.eq.1 .or.kfoume>0)then
         fvalue=fvalue+dete(1)
      else
         print *,'dete not included !'
         print *,'dete',dete(1),fvalue,nq,ieqmod,kfoume
      end if

      XLIKE=-0.5d0*fvalue -DETL*KSFIT(1)
      IF(IOPCOV.EQ.1.OR.IOPCOV.EQ.2)xLIKE=xLIKE-DETL*KSFIT(2)

!     solutions
      if(joptlk>0 .or. fvalue<ffmin)call back_solve
      if(fvalue<ffmin)then
         ffmin=fvalue
         savsol=solns
      end if

!     derivatives part ...
      if(ioptlk.eq.1)then

!        calculate average information matrix
         if(nq>1.or.ieqmod>0)trres1=0.d0
         call  dxypy3 (varvec)
         devl2=-0.5d0*dypy2
         if(joptlk.eq.2)go to 900      ! skip 1st deriv.s -> s.e. only

!        calculate first derivatives of the likelihood
         call dxspa3(varvec)

         devl1=0.d0
!        ... contributions from log |R|
         if(nq.eq.1.and.ieqmod.eq.0)then
            do  ipar=ngparm+1,nparm
            devl1(ipar)=dot_product(nncom(:ncomb),trres1(ipar,:ncomb))
            end do
         else ! if(nq>1)then
            devl1(ngparm+1:nparm)=trres1(ngparm+1:nparm,1)
         end if
         if(ioprn1>0.and.ieqmod.eq.1 )devl1(ngpar1+1:ngpar2)=
     &                                     trres1(ngpar1+1:ngpar2,1)

!        ... contributions from log |G|
         if(nanim>0)devl1(:ngpar1)=nanim*trran1(:ngpar1)

         if(ioprn1>0.and.ieqmod.eq.0 )devl1(ngpar1+1:ngpar2)=
     &                               nrand1*trran1(ngpar1+1:ngpar2)
         if(ioprn3.gt.0)devl1(ngpar2+1:ngparm)=
     &                               nrand3*trran1(ngpar2+1:ngparm)

!        ... contributions from log |C| and yPy
         devl1(:nparm)=-0.5d0*(devl1(:nparm)+devc(:nparm))
      end if

 900  ncall=ncall+1
      call house_keeping (ioptlk)
      ll=0
      do i=1,kparm                   ! condense param vector for printing
      if(ipm(i).ne.0)cycle
      ll=ll+1
      varvec(ll)=parvec(i)*ffvec(i)
      end do
      if(ll.le.44.or.ioptlk.eq.1)WRITE(*,1112)NCALL,xLIKE,varvec(:ll)
 1112 FORMAT(I5,F19.9,(1X,T26,4F12.6))

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
            INQUIRE(unit=iun28,opened=lopen)
            if(.not.lopen)open(iun28,file='Iterates',status='unknown',
     &                      form='formatted',position='append')
            write(iun28,'(a,f12.3,i4,3x,a)')
     &                      ' time/dxlik3',xsecs,ioptlk,machine
            write(iun28,'(a,f19.9)')'First log L =',xlike
            close(iun28)
         end if
         if(ntime(ioptlk+1).eq.1)write(*,'(a,f12.3,i4,3x,a)')
     &                      ' time/dxlik3',xsecs,ioptlk,machine
      END IF

C     STORE LIKELIHOOD VALUE WITH PERTAINING PARAMETERS
      
      mcall=ncall
      do while (mcall>mxfunc)
         mcall=mcall-mxfunc
      end do
      FEVAL(2:kparm+1,mCALL)=parvec(:kparm)
      FEVAL(1,mCALL)=FVALUE

C     SAVE ALL LIKELIHOODS FOR RETRIEVAL FILE "59"
      INQUIRE(unit=iun59,opened=lopen)
      if(lopen) close(iun59)

!     re-open file
      OPEN(IUN59,FILE='DF59#DAT',STATUS='unknown',FORM='UNFORMATTED',
     &      position='append')
      INQUIRE(FILE='DF59#DAT',exist=lexist)
      if(.not.lexist)then
!        file does not exits, write out abbreviated header
         II59=-1
         REWIND(iun59) 
         WRITE(IUN59)II59
         write(iun59)kfit,jparam
      END if
      WRITE(iun59)ncall,fvalue,varvec(:nparm),kfteig
      CLOSE(IUN59)
      return

      end subroutine house_keeping

!     =====================
      subroutine first_time
!     =====================

      ilkpre=-1
      kkopt=0
      allocate(currnt(nparm+nq),stat=ii)
      if(ii>0)stop 'alloc : current'
      currnt=0.d0
      nreuse=0
      ipvrun=1
      end subroutine first_time

      END subroutine dxlik3
































