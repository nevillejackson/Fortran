!=========================================================================
      SUBROUTINE DFINP3 (iinput,ioprun)
!=========================================================================

      use parameters
      use names
      use units
      use parmap
      use means
      use combinations
      use numbers
      use levels
      use ages
      use platform
      use residuals
      use phimatrix, only : irropt

!     arguments
      implicit none
      INTEGER, intent(INOUT)             :: ioprun
      INTEGER, intent(out)               :: iinput

!     local variables
      real(8), dimension(:), allocatable :: ssig
      character (len=25)                 :: OLD='old',FORMA='formatted'
     &,                                     FSTAND
      integer                            :: iq,jq,iopsav,isilen,ii,jj,i,
     &                                      jjput, mlag, nbb, iun, jun, 
     &                                      j1, k, kk, i1
      real(8)                            :: xx,tt
      logical                            :: dfinex=.false.
      character(len=10)                  :: dfinp='VF_options'
      integer, external                  :: ihmssf, ihmii
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if(iopmod.ne.5 .and.iopmod.ne.6)stop 'dfinp iopmod'

      allocate (nparno(mage*nq,mage*nq,7),stat=ii)
      if(ii>0)stop 'dfinp3 : alloc'
      nbb=max0(mage,kfitmx*(2*kfitmx+1))
      allocate (ipm(mxparm),ffvec(mxparm),ssig(nbb),stat=ii)
      if(ii>0)stop 'dfinp3 : alloc'
    
      NPARM=0
      jparam=3
      IOPSAV=IOPRUN

      IF(IOPRUN.LE.-2.OR.ioprun.eq.1.or.ioprun.eq.8)THEN
         ISILEN=1
         IF(IOPRUN.EQ.8)THEN
            WRITE(*,*)'FULL SET OF PARAMETERS FITTED ?'
            CALL YNDEF(JJ,1)
            IF(JJ.EQ.0)IOPRUN=18
         END IF

      ELSE 
         ISILEN=0
         if(ioprun.eq.10)then
            iinput=4
         else
            WRITE(*,*)' '
            WRITE(*,*)'Read starting values from ... ? '
            WRITE(*,fmt9(1))'1  ...  Standard input      '
            WRITE(*,fmt9(1))'2  ...  File "DF18#DAT"     '
            WRITE(*,fmt9(ipltf))'3  ...  Other File      '
            CALL OPTION(IINPUT,1,3)
         end if

         if(iinput.eq.1)then
            iun=0
         else
            IUN=18
            if(iinput.eq.2)then
               FSTAND='DF18#DAT'
            else
 300          write(*,fmt(ipltf))'Give name for file of starting values'
              read(*,'(a)')fstand
              if(fstand(1:6).eq.'      ')then
                  write(*,*)'blank file name not valid - try again !'
                  go to 300
              end if
            end if
            CALL FCONCT(IUN,FSTAND,FORMA,OLD)
         END IF
      END IF

      if(ioprun.eq.0.or.ioprun.eq.1.or.ioprun.eq.8)then
!         inquire(file=dfinp,exist=dfinex)
!         open(1,file=dfinex,form='formatted',status='unknown')
      end if

      NPARNO=0

C     DIRECT ADDITIVE GENETIC COVARIANCES
      if(nanim>0)then
         IF(IOPRUN.NE.-2.or.iopsav.ne.ioprun )WRITE(*,'(1x,a,5i4)')
     &         'ORDER(S) OF POLYNOMIAL FIT FOR "SIG A" =',kfit(1,:nq)
         K=0
         DO Iq=1,ksfit(1)
         DO Jq=Iq,ksfit(1)
         CALL RSTVAL('KMAT A  ','SIG A   ',K,1,iq,jq)
         END DO
         END DO
         CALL CHKSIG('KMAT A  ',ZERO,XVEC,SSIG,NPARM,ksfit(1),ioprun,1)
         NPARM=NPARM+K
      end if

C     MATERNAL GENETIC COVARIANCE COMPONENTS
      IF( ioprn2 .EQ.1 )THEN
         IF(ISILEN.EQ.0)WRITE(*,*)' '
         ISTRT(1)=NPARM
         IF(IOPRUN.NE.-2.or.iopsav.ne.ioprun )WRITE(*,'(1x,a,5i4)')
     &         'ORDER OF POLYNOMIAL FIT FOR "SIG M"  =',kfit(2,:nq)
         K=0
         ssig=0.d0
         DO Iq=1,ksfit(2)
         DO Jq=Iq,ksfit(2)
         CALL RSTVAL('KMAT M  ','SIG M   ',K,2,iq,jq)
         END DO
         END DO
         CALL CHKSIG('SIG M   ',ZERO,XVEC,SSIG,NPARM,ksfit(2),ioprun,1)
         NPARM=NPARM+K

C        ... DIRECT-MATERNAL GENETIC COVARIANCES
         IF( iopcov .EQ.2) stop 'option not implemented'
      END IF
      NGPAR1=NPARM

C     ADDITIONAL RANDOM EFFECT COVARIANCE COMPONENTS
      if(ioprn1.eq.1)ISTRT(3)=NPARM
      icorrf=0

!     estimate covariances among RR coefficients
      IF(ioprn1.EQ.1 .and. irropt(4,1)<4 )THEN
         IF(ISILEN.EQ.0)WRITE(*,*)' '
         IF(IOPRUN.NE.-2.or.iopsav.ne.ioprun)WRITE(*,'(1x,a,5i4)')
     &            'ORDER OF POLYNOMIAL FIT FOR "SIG C" =',kfit(4,:nq)
         ssig=0.d0
         K=0
         DO Iq=1,ksfit(4)
         DO Jq=Iq,ksfit(4)
         CALL RSTVAL('KMAT C  ','SIG C   ' , K,4,iq,jq)
         END DO
         END DO
         CALL CHKSIG('KMAT C  ',ZERO,XVEC,SSIG,NPARM,ksfit(4),ioprun,1)
         NPARM=NPARM+K
         if(ieqmod>0)then
            allocate(rxkk(ksfit(4)*(ksfit(4)+1)/2),stat=ii)
            if(ii>0)stop 'alloc rxkk'
         end if

!     estimate variance function & correlation function
      else if(ioprn1.eq.1 .and.nq.eq.1)then  ! only for univariate
         IF(IOPRUN.NE.-2.or.iopsav.ne.ioprun )then
            write(*,*)'you have selected to fit a parametric covariance'
     &,               ' structure !'
            if(dfinex)then
!               call read_dfinp
            else
               call varfun_form (kvfpe,kfoupe,omegape,period)
               if(kvfpe>0)then
                  call varfun_opts(ilgpe)
               else
                  ilgpe=0
               end if
               call corr_menu (icorrf)
               if(icorrf>19.and.kfoupe.eq.0)then
                   write(*,*)'Periodicity for correlation ? '
                   call option(ii,0,99999)
                   period=ii
!                  ... assume SAME periodicity for VF and CorrF if VF
!                      has a fourier series part !
               end if
            end if
         else
            kvfpe=0
            ilgpe=0
            icorrf=0
         end if

         if(kvfpe.ge.0)then
            kfit(4,1)=1+kvfpe+kfoupe*2
         else if(kvfpe.eq.-2)then
            kfit(4,1)=2
         end if

         call corr_setup(icorrf)

         kfit(4,1)=kfit(4,1)+ncpno

!        Read parameters : regression coefficients first ...
         k=0
         do iq=1,iabs(kvfpe)+2*kfoupe
         call read_regcoeff ('REG.CF.-PE  ',k,iq,4)
         if(iq.le.iabs(kvfpe) .and.ilgpe<100)then
            regtyp(nparm+iq)='POL'
         else if(iq.le.iabs(kvfpe) .and.ilgpe.ge.100)then
            regtyp(nparm+iq)='LEG'
         else if(iq>iabs(kvfpe))then
            regtyp(nparm+iq)='FOU'
         end if
         end do ! reg coeff

!        ... then variance/SD at mean
         if(kvfpe.ge.0)then
            call rstval('sigc  00','sigc  00',k,4,1,1)
            write(param(nparm+kvfpe+kfoupe*2+1),'(a11,i3)')
     &                                              'SIG C  0 -L',ilgpe
            parvar(nparm+kvfpe+kfoupe*2+1)=param(nparm+kvfpe+kfoupe*2+1)
            write(*,'(a)')param(nparm+kvfpe+kfoupe*2+1)
         end if

!        ... correlation function parameters last
         do i=1,ncpno
         if(ioprun<8 .and.ioprun>-2.and.ioprun.ne.1)then
            IF(IUN.EQ.0)write(*,'(1x,a,i3,a)')
     &                  'correlation parameter no.',i,' ?'
            CALL RVALFL(XX,-1.d9,1.d9,IUN)
            IF(IUN>0)WRITE(*,*)' READ : Correlation parameter = ',XX
         else
            xx=0.5d0
         end if
         k=k+1
         IPM(NPARM+k)=0
         XVEC(NPARM+k)=XX
         write(PARVAR(NPARM+k),'(a,i2,a,i2)')'CorF',icorrf,' PAR.',i
         PARAm(NPARM+k)=parvar(nparm+k)
         NPARNO(k,k,4)=NPARM+k
         end do
         NPARM=NPARM+K
         allocate(rxkk(ncpno),pevec(nage(1) ),stat=ii)
         if(ii>0)stop 'alloc pevec'
      END IF
      ngpar2= nparm


      IF(ioprn3.EQ.1)THEN
         IF(ISILEN.EQ.0)WRITE(*,*)' '
         IF(IOPRUN.NE.-2.or.iopsav.ne.ioprun)WRITE(*,'(1x,a,5i4)')
     &          'ORDER OF POLYNOMIAL FIT FOR "SIG Q" =',kfit(5,:nq)
         ISTRT(5)=NPARM
         ssig=0.d0
         K=0
         DO Iq=1,ksfit(5)
         DO Jq=Iq,ksfit(5)
         CALL RSTVAL('KMAT Q  ','SIG Q   ' , K,5,iq,jq)
         END DO
         END DO
         CALL CHKSIG('KMAT Q  ',ZERO,XVEC,SSIG,NPARM,ksfit(5),ioprun,1)
         NPARM=NPARM+K
      END IF

C     NO. OF PARAMETERS DUE TO RANDOM EFFECTS
      NGPARM=NPARM

C     measurement error variances
      IF(ISILEN.EQ.0)WRITE(*,*)' '
      IF(IOPRUN.NE.-2.or.iopsav.ne.ioprun )THEN
         WRITE(*,*)'Measurement error variances : '
         write(*,*)'  Individual variances - give no. to be fitted '
         if(nq.eq.1)then
            write(*,*)'  Single variance + function '
            write(*,*)'  -> regression : give order as *negative* no. '
            write(*,*)'     (-1 = linear, -2 = quadratic, etc.)       '
         end if
         CALL OPTDEF(KK,-9999,mage,1)
         if(kvfpe.eq.-2.and.kk>0)stop 'must fit VF !'
         if(kk<-99)then
            kk=-mod(iabs(kk),100)
            write(*,*)'Order of fit for Fourier series approximation ?'
            write(*,*)'(1 = 1st (2 coeff), 2 = 2nd (4 coeff)...)'
            call optdef(kfoume,0,99,1)
            write(*,*)'Periodicity ? (give in same units as meta-meter)'
            call option(ii,0,99999)
            tt=ii
            omegame=2.d0*pi/tt
         else
            kfoume=0
         end if
         kfit(7,:nq)=kk      ! assume same no of me var for all traits
      else
         kfit(7,:nq)=1
      end if 
      ksfit(7)=kfit(7,1)*nq*(nq+1)/2

      nrparm=0
      iopeps=0
      if(kfit(7,1)>0)then          ! allow for temp env covariances for
         do i=1,kfit(7,1)          ! records taken at the same age
         do i1=1,nq
         iq=(i1-1)*kfit(7,i1)+i    ! need to check for pairs of recs!!
         do j1=i1,nq
         jq=(j1-1)*kfit(7,j1)+i
         CALL RSTVAL('SIG ME  ','SIG ME  ',nrparm,7,iq,jq) 
!        read nq*(nq+1)/2 components sequentially for diff. me var classes
         end do
         end do
         end do

      else if(kfit(7,1).le.0)then   ! var. func. model for single trait only
         do iq=1,iabs(kfit(7,1))+2*kfoume
         call read_regcoeff ('REG.CF.-ME  ',nrparm,iq,7)
         if(iq.le.iabs(kfit(7,1)) .and.ilgpe<100)then
            regtyp(nparm+iq)='POL'
         else if(iq.le.iabs(kfit(7,1)) .and.ilgpe.ge.100)then
            regtyp(nparm+iq)='LEG'
         else if(iq>iabs(kfit(7,1)))then
            regtyp(nparm+iq)='FOU'
         end if
         end do
         write(*,*)'measurement error variance (at mean age) ?'
         CALL RSTVAL('SIG ME  ','SIG ME  ',nrparm,7,iq,iq) 
         if(ioprun.eq.0.or.ioprun.eq.8.or.ioprun.eq.5.or.ioprun.eq.1)
     &                                                            then
            call varfun_opts(ilgeps)
         else
            ilgeps=0
         end if
         write(param(nparm+nrparm),'(a11,i3)')'SIG ME 0 -L',ilgeps
         parvar(nparm+nrparm)=param(nparm+nrparm)
         write(*,'(a)')param(nparm+nrparm)
      end if
      ISTRT(4)=NPARM
      NPARM=NPARM+nrparm

!     step function for ME variances ....
      if(kfit(7,1) >1 .and. kfit(7,1) < nage(1))then
         WRITE(*,*)' '
         WRITE(*,*)'Assign measurement errors to ages - read from ?'
         WRITE(*,fmt9(1))'1  ...  Standard input '
         WRITE(*,fmt9(1))'2  ...  "DF20#DAT"     '
         CALL OPTDEF(JJPUT,1,2,2)
         if(jjput.eq.1)then
            jun=0
         else
            jUN=20
            FSTAND='DF20#DAT'
            CALL FCONCT(jUN,FSTAND,FORMA,OLD)
         END IF
         do i=1,nage(1)                    
         IF(jUN.EQ.0)then
             if(ipltf.eq.1)then
             WRITE(*,'(a,i3,i6,a)')' age no.',i,iiage(i,1),' m.e. no. ?'
             else
             WRITE(*,'(a,i3,i6,a/)')' age no.',i,iiage(i,1),
     &                                                    ' m.e. no. ? '
             end if
             call option(ii,1,kfit(7,1))
         else
             read(jun,*)ii
             WRITE(*,*)' READ : age no.',i,' m.e. no. =',ii
             if(ii.lt.1.or.ii.gt.kfit(7,1))stop 'value not allowed'
         end if
         meage(i)=ii
         end do
      else if(kfit(7,1).eq.nage(1))then
         meage=(/ (i,i=1,nage(1)) /)
      else
         meage=1
      end if

      ffvec=1.d0

      IF(IOPRUN.EQ.18)IOPRUN=8
      ioprun=iopsav
      RETURN

      contains

C     =======================================
      SUBROUTINE RSTVAL (pp,qq,k,ic,iq,jq)
C     =======================================

      character(len=8),intent(in) :: pp,qq
      integer, intent(inout)      :: k
      integer, intent(in)         :: ic,iq,jq

      CHARACTER(len=14)           :: PARA
      real(8)                     :: x1,x2,xx1,xx

      WRITE(PARA,729)PP,Iq,Jq
      IF(IOPRUN.NE.-2.and.ioprun.ne.1.AND.IOPRUN.LE.7)THEN
!        X2=20.D0*SDEV(1)*SDEV(1)
         x2=10.d12
         IF(Iq.EQ.Jq .and .PP.NE.'SIG AM  ')THEN
            X1=0.D0
         ELSE
            X1=-X2
         END IF
         x1=-x2
         IF(IUN.EQ.0)WRITE(*,719)PARA
         CALL RVALFL(XX,X1,X2,IUN)
         IF(IUN.GT.0)WRITE(*,*)' READ : ',PARA,' = ',XX
      ELSE
         XX=Iq*10+Jq
         IF(PP.EQ.'SIG AM  ')then
            XX=0.5d0
         else IF(PP.EQ.'KMAT M  ' .and. iq.eq.jq)then
            xx=xx+1.5d0
         else IF(PP.EQ.'KMAT M  ' .and. iq.ne.jq)then
            xx=xx-0.5
         else IF(PP.EQ.'KMAT C  ' .and. iq.eq.jq)then
            xx=xx+5.d0
         else IF(PP.EQ.'KMAT C  ' .and. iq.ne.jq)then
            xx=xx-1.d0
         else IF(PP.EQ.'KMAT Q  ' .and. iq.eq.jq)then
            xx=xx+4.d0
         else IF(PP.EQ.'KMAT Q  ' .and. iq.ne.jq)then
            xx=xx-0.8d0
         else IF(PP.EQ.'SIG E   ' .and. iq.ne.jq)then
            xx=(xx-3.d0)/20.d0
         end if
         XX1=XX
         IF(IOPRUN.EQ.18)THEN
            WRITE(*,719)PARA
            CALL RVLDEF(XX,-99999.d0,99999.d0,XX1)
         END IF
      END IF

      K=K+1
      IF(NPARM+K.GT.MXPARM)THEN
         WRITE(*,*)'ROUTINE "RSTVAL" : DIMENSION EXCEEDED !!!'
         WRITE(*,*)'CURRENT MAXIMUM OF PARAMETERS =',MXPARM
         STOP 'RESET PARAMETER "MXPARM" !'
      END IF

C     FIX ZERO COVARIANCES
      IPM(NPARM+K)=0
      if(ic.eq.7.and.nq>1.and.iq<jq .and.xx.eq.0)then
          if(nboth(ihmssf(i1,j1,nq)).eq.0)then
             ipm(nparm+k)=1
             print *,'fix cov at zero',i1,j1
          end if
!      else if(xx.eq.0)then  
!         ipm(nparm+k)=-1
!         print *,' ipm ',nparm+k,ipm(nparm+k),xx
      end if
      SSIG(K)=XX
      XVEC(NPARM+K)=XX
      PARVAR(NPARM+K)=PARA
      PARAM(NPARM+K)=PARA
      NPARNO(Iq,Jq,IC)=NPARM+K
      IF(IC.NE.3.AND.IC.NE.7)NPARNO(JQ,IQ,IC)=NPARM+K

!     store type of polynomial in regtyp
      if(para(1:4).eq.'KMAT')then
        if(irropt(ic,1).eq.1)then
           regtyp(nparm+k)='LEG'
        else if(irropt(ic,1).eq.2)then
           regtyp(nparm+k)='USR'
        else if(irropt(ic,1).eq.3)then
           regtyp(nparm+k)='LRS'
        else if(irropt(ic,1).eq.0)then
           regtyp(nparm+k)='POL'
        end if
      end if

      RETURN
719   FORMAT(' GUESS FOR :  ',A,'  ?      ')
729   FORMAT(A8,2I3)
      END subroutine rstval
 
!     =============================
      subroutine varfun_opts (ilgpe)
!     =============================

      integer, intent(out) :: ilgpe     ! option to specify form of VF
      integer              :: ii

      write(*,*)' 0  ... normal scale ' ! sig*(1+sum_j (reg_j*a^j)
      write(*,*)' 1  ... log-linear 1 ' ! sig*exp{1+sum_j(reg_j*a^j}
      write(*,*)' 2  ... log-linear 2 ' ! exp{sig +sum_j(reg_j*a^j)}
      call optdef(ilgpe,0,2,2)

      write(*,*)'Model variances or standard deviations ? '
      write(*,*)' 0  ... variances '
      write(*,*)' 1  ... standard deviations '
      call optdef(ii,0,1,0)
      ilgpe=ilgpe+10*ii

      write(*,*)'What kind of polynomials ? '
      write(*,*)' 0  ... ordinary'
      write(*,*)' 1  ... orthogonal (Legendre) '
      call optdef(ii,0,1,0)
      ilgpe=ilgpe+100*ii

      return
      end subroutine varfun_opts

!     ===============================================
      subroutine varfun_form (kvfpe,kfoupe,omega,tt)
!     ===============================================
      integer, intent(inout) :: kvfpe
      integer, intent(out)   :: kfoupe
      real(8), intent(out)   :: omega,tt

      integer                :: ii

      write(*,*)'Order of fit for polynomial variance function ?  '
      write(*,*)'(0=constant, 1 = linear, 2 = quadratic, etc.)   '
      write(*,*)'(add 100 for additional special curve)   '
      call optdef(kvfpe,-2,9999,0)

      if(kvfpe>100)then
         kvfpe=mod(kvfpe,100)
         write(*,*)'Order of fit for Fourier series approximation ?'
         write(*,*)'(0=none, 1 = 1st (2 coeff), 2 = 2nd (4 coeff) etc.)'
         call optdef(kfoupe,0,99,1)
         write(*,*)'Periodicity ? (give in same units as meta-meter 1)'
         call option(ii,0,99999)
         tt=ii
         omega=2.d0*pi/tt
         print *,omega,tt
      else
         kfoupe=0
         omega=0.d0
         tt=0.d0
      end if

      return
      end subroutine varfun_form

!     =============================
      subroutine corr_menu (icorrf)
!     =============================

      integer, intent(out)   :: icorrf
      integer, parameter     :: nopts=10   ! must be even
      character(len=30), dimension(nopts) :: aa =(/
     &               '   0  ... compound symmetry   ',
     &               '   1  ... autocorrelation     ',
     &               '   2  ... exponential         ',
     &               '   3  ... gaussian            ',
     &               '   4  ... damped auto-correl. ',
     &               '   5  ... damped exponential  ',
     &               '   6  ... 1st order SAD       ',
     &               '   7  ... 2nd order AR (corr) ',
     &               '   8  ... 1st order ARMA      ',
     &               '   9  ... 2nd order SAD       '  /)
      integer                :: ii, i

      ii=nopts/2
      write(*,*)'Correlation function ?  '
      do i=1,ii
      write(*,*)aa(i),aa(ii+i)
      end do
      write(*,*)'Add 10 for age-independ. correlation param. '
      write(*,*)'Add 20 for overlaid periodic part '
      call optdef(icorrf,0,29,2)
      return
      end subroutine corr_menu

!     ==============================
      subroutine corr_setup (icorrf)
!     ==============================

      integer, intent(in) :: icorrf
      integer             :: ii, iicf, mm

!      no. of parameters
       if(mod(icorrf,10).le.3)then
          ncpno=1
       else if(mod(icorrf,10).le.8)then
          ncpno=2
       else if(mod(icorrf,10).eq.9)then
          ncpno=4   ! 2nd order SAD
       end if
       if(icorrf>10)ncpno=ncpno+1       ! age-independent/periodic part
                                        ! add additional parameter
!      valid ranges
       allocate(rhomin(ncpno),rhomax(ncpno),stat=ii)
       if(ii>0)stop 'rhorange'

       iicf=mod(icorrf,10)
       if(iicf.eq.1 .or. iicf.eq.4 .or.iicf.eq.6 .or.
     &               iicf.eq.7.or. iicf.eq.8.or. iicf.eq.0 )then
          rhomin(1)=-1.d0+zero*10.d0
          rhomax(1)=1.d0-zero*10.d0
       else if(iicf.eq.2 .or.iicf.eq.3 .or.iicf.eq.5)then
          rhomin(1)=zero*10.d0
          rhomax(1)=9999.d0
       end if
       if(iicf>3.and. iicf<7)then
          rhomin(2)=-99.d0
          rhomax(2)=99.d0
       else if (iicf .eq.7.or.iicf.eq.8)then
          rhomin(2)=-1.d0+zero*10.d0
          rhomax(2)=1.d0-zero*10.d0
       else if(iicf.eq.9)then
          rhomin(3)=-1.d0+zero*10.d0
          rhomax(3)=1.d0-zero*10.d0
          rhomin(4)=-99.d0
          rhomax(4)=99.d0
       end if
       if(icorrf>10)then
          rhomin(ncpno)=0.d0+zero
          rhomax(ncpno)=1.d0-zero
       end if

!      allocate correlation matrix ...
       if(mod(icorrf,10).ge.6)then
          allocate(corrvec(nage(1)*(nage(1)+1)/2),stat=ii)
          if(ii>0)stop 'DFINP3 : alloc corrvec'
          do i=1,nage(1)
          corrvec(ihmii(i,nage(1)))=1.d0
          end do
          mlag=iiage(nage(1),1)-iiage(1,1)
          mm=max0(mlag,nage(1))
          allocate (ww(mm),stat=ii)
       end if
       return
       end subroutine corr_setup

!     ======================================
      subroutine read_regcoeff (pp, k,ii,jj)
!     ======================================

      character(len=12), intent(in)  :: pp
      integer, intent(in)            :: ii,jj
      integer, intent(inout)         :: k
      real(8)                        :: xx

      if(ioprun<8.and.ioprun.ne.1)then
         if(iun.eq.0)write(*,'(1x,a,i3,a)')
     &                  'variance function coefficicient no.',ii,' ?'
         call rvalfl(xx,-1.d9,1.d9,iun)
         if(iun>0)write(*,*)' READ : Var. func. coefficient = ',XX
      else
         xx=0.1d0
      end if

      k=k+1
      ipm(nparm+k)=0
      xvec(nparm+k)=xx
      write(parvar(nparm+k),'(a,i2)')pp,ii
      param(nparm+k)=parvar(nparm+k)
      nparno(k,k,jj)=nparm+k
      return
      end subroutine read_regcoeff

      END subroutine dfinp3



 











