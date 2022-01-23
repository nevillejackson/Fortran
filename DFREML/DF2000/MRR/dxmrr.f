!===========================================================================
      PROGRAM  DxMRR
!===========================================================================

      use params
      use numbers
      use names
      use units
      use sparse
      use xsprse
      use times
      use parmap
      use sigmas
      use likelihoods
      use like_components
      use traces
      use combinations
      use derivs
      use solutions
      use ages
      use phimatrix
      use dmatrices
      use residuals
      use fmatrix
      use parameters
      use eigen_decomp
      use platform
      use version
      use legendre
      use today

      implicit none
      character(len=15) :: aa
      real(8)           :: fmin,fstart,xlike,xx,fvalue,detzhz,time,xsecs
      integer           :: itime,lim0,ioprun,kq,iopslv,nsrow1,isilen,k,
     &                     nparm1,ii,i,iopt19,imin,nq2,k5,m,iq,jj,mm,
     &                     kvf,iconv,ipartm,iinput
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      CALL DFTIME(time,xsecs,isecs,1,'DXLIK3')

!     allocate parameter vectors
      call maxparm

!     initialise
      call start_up

!     read run option
      call dxmenu

!     open DF_#DAT files
      CALL DxOPEN
      call dxversion(iun66)
      call today_is(iun66)

      IF(IOPRUN.EQ.-1)GO TO 7000

C     READ INFORMATION ON MODEL OF ANALYSIS & DATA STRUCTURE FROM "51"
      CALL DF51R3(IUN51)
      IF(IOPCOV.EQ.4)call dx_open45

9     FORMAT(7X,A)

C     --------------------------------------------------
C     READ STARTING VALUES & SET UP VECTOR OF PARAMETERS
C     --------------------------------------------------

      CALL OPZERO(ZERO,0)
      CALL DFWRH3(IOPRUN)
      CALL DFINP3(iinput,IOPRUN)

      mfitmx=maxval( ksfit(:5) )
      call all_sigmas(mfitmx,kfit(7,1))

      ICONV=0
      ipartm=0
      ONE=1.D0-ZERO
      FMIN=BIG
      FSTART=FMIN

7000  NTRAIT=1
      KQ=1

      IF(IOPRUN.EQ.-1)THEN             ! set-up step

         CALL DxPRE3
         CALL DFWRH3(IOPRUN)
         CALL DFLSP3(LIM0)
         DETL=0.d0
         nsrow=0
         CALL DF51W3(IUN51)
         WRITE(IUN66,901)
         CALL DFTIME(time,xsecs,isecs,11,'DXLIK3')
         STOP 'run op -1 done'

      ELSE IF(IOPRUN.EQ.-2)THEN       ! re-ordering step

         EIGZER=ZERO
         CALL DxORD3
         CALL DF51W3(IUN51)
         CALL DFWR59
         WRITE(IUN66,901)
         CALL DFTIME(time,xsecs,isecs,11,'DXLIK3')
         STOP 'run op -2 done'

      ELSE 

         allocate(solns(0:nsrow),savsol(0:nsrow),stat=ii)
         if(ii>0)stop 'alloc : solns'

c        operational zero for eigenvalues of reduced rank matrices
         if(kfit(1,kq).lt.nage(1))then
            CALL OPZERO(eigzer,2)
         else
            eigzer=1.1d0*zero
         end if

!        set up phi-matrix
         if(jparam.ge.2)then
            k5=5
            k=maxval(kftfix)
            m=max0(mfitmx,k)
            if(ilgeps>99.or.ilgpe>99)then
              kvf=1+max0(kvfpe, -kfit(7,1))
              k=max0(m,kvf)
              m=k
              k5=6
            end if
            allocate(phi(mage,m,0:k5,nq),stat=ii)
            if(ii>0)stop 'alloc : phimatrix'
            call all_legendre (m)
            phi=0.d0
            do iq=1,nq
            if(kftfix(iq)>0)CALL PHIMAT (nage(irrmet(0,iq)),kftfix(iq),
     &                                                            0,iq)
            call phimat(nage(irrmet(1,iq)),kfit(1,iq),1,iq)
            if(kfit(2,iq)>0)call phimat(nage(irrmet(2,iq)),kfit(2,iq),
     &                                                            2,iq)
            if(kfit(4,iq)>0)call phimat(nage(irrmet(4,iq)),kfit(4,iq),
     &                                                            4,iq)
            if(kfit(5,iq)>0)call phimat(nage(irrmet(5,iq)),kfit(5,iq),
     &                                                            5,iq)
            if(ilgeps>99.or.ilgpe>99)call phimat(nage(1),kvf,6,iq)
            
            end do
         END IF

C        RECOVER ANY FUNCTION VALUES FROM CRASHED PREVIOUS RUN
         FFMIN=BIG
         KPARM=NPARM
         allocate (feval(nparm+1,mxfunc), stat=ii)
         if(ii>0)stop 'alloc : likelihoods '
         MM=18
         CALL DFRC59 (IOPRUN,KPARM,KQ,MM,FMIN)
         IF(FMIN.LT.FSTART)FSTART=FMIN
         START(:kparm)=XVEC(:kparm)

C        PICK OUT BEST POINT OR CALCULATE SOLUTIONS ONLY
         IF(IOPRUN.EQ.8 .OR. IOPRUN.EQ.5 )THEN
           XLIKE=-0.5D0*FMIN
           if(nanim>0)xlike=xlike -DETL*KSFIT(1)
           IF(IOPCOV.EQ.1.OR.IOPCOV.EQ.2)xLIKE=xLIKE-DETL*KSFIT(2)
           GO TO 1000

!        SET UP GRID
         ELSE IF(IOPRUN.EQ.10)THEN
           CALL DXGRD3 (ZERO)
         END IF

         WRITE(*,*)'PARAMETERS TO BE ESTIMATED :'
         DO  I=1,KPARM
         WRITE(*,2412)I,PARAM(I),XVEC(I)*FFVEC(I)
         END DO
         ISILEN=0

c        -----------------------
C        MAXIMISE THE LIKELIHOOD
c        -----------------------

         call all_xsprse (nsrow,maxlnz)
         call all_derivs(nparm)

         WRITE(*,*)' '
         WRITE(*,*)'SEARCH PROCEDURE TO BE USED (negative value to',
     &                                         ' modify defaults):'
         WRITE(*,9)'(-)1   ...  SIMPLEX '
         WRITE(*,9)'(-)2   ...  POWELL  '
!         if(icorrf<6 .and. nanim>0)then
         if(irropt(4,1)<4 .and.nanim>0)then
            WRITE(*,9)'(-)3   ...  AI-REML '
            CALL OPTDEF(IOSRCH,-3,3,3)
         else
            CALL OPTDEF(IOSRCH,-2,2,2)
         end if
         if(iosrch.eq.0)stop 'iosrch invalid'

         call sigeps_direct(iopeps)

         write(*,*)' '
         write(*,*)'transform for estimation ?'
         write(*,*)'        0  ...  original scale '
         write(*,*)'        1  ...  cholesky decomp. '
         if(kfit(7,1)>0)write(*,*)
     &             '        2  ...  cholesky d. + log(diag.s)'
         if( iabs(iosrch).ne.3 )
     &   write(*,*)'        3  ...  cholesky : fix part at zero'
         jj=3
         if(iabs(iosrch).eq.3)jj=2
         if(iabs(iosrch).eq.3.and.kfit(7,1)<0)jj=1
         call optdef(kopt,0,3,jj)
         if(kopt.gt.0.and.iopcov.eq.2)stop 'option not implemented'
         if(kopt.eq.2.and.kfit(7,1)<0)stop 'option not implemented'
        
         open(28,file='Iterates',status='unknown',form='formatted',
     *                                            position='append')
!        transformation to cholesky scale
         kfteig=0
         if(kopt.eq.3 .or. kopt.eq.1 )then
            call dxchl33
         else if(kopt>0)then
            call sindex 
            call setup 
            call dxkhn3(kopt,xvec)
         end if

c        "AI-REML"
         IF( iabs(IOSRCH) .EQ.3)THEN

            nq2=mfitmx+ioprn2*mfitmx
            call all_dmatrices (nq,nq2,mobs,nrparm,ngparm,ncomb,
     &                                                      max_comb)
            allocate(trran1(nparm),trres1(nparm,ncomb),stat=ii)
            if(ii>0)stop 'alloc : traces'
            if(irropt(4,1).eq.4)then
               allocate (pe1vec(nage(1),kvfpe+1),stat=ii)
               if(ii>0)stop 'alloc pe1vec'
            end if
            if(kfit(7,1)<0)then
               allocate (ee1vec(nage(1),1-kfit(7,1)),stat=ii)
               if(ii>0)stop 'alloc ee1vec'
            end if
            CALL DXNRP3 ( IOPRUN, ISILEN, FMIN)
      
         ELSE

            ipartm=0
            if(kopt.eq.3 .and. iosrch<0 )then
               write(*,*)'Maximise w.r.t to a subset of parameters ?'
               call yndef(ipartm,0)
               if(ipartm.gt.0)call dxipm3 (xvec)
            end if

C           RESCALE
            FFVEC(:kparm)=XVEC(:kparm)
            XVEC(:kparm)=1.D0
!           ... make sure there are no "0" starting values
!            do i=1,kparm
!            if(FFVEC(i).EQ.0.AND.IPM(i).EQ.0)FFVEC(i)=0.0001D0
!            if(ipm(i).eq.-1)ipm(i)=0
!            end do

!           SIMPLEX
            IF( iabs(IOSRCH) .EQ.1)THEN         
               CALL DFSPX3(IOPRUN,ISILEN,IMIN,ICONV,FMIN)

!           POWELL
            ELSE
               CALL DFPOW3 (IOPRUN,ISILEN,ICONV,FMIN)
            END IF
            if(ipartm.gt.0)call dxlik3(XVEC,FMIN,xlike,0)
         END IF

         deallocate(xspars,dia,stat=ii)
         if(ii>0)stop 'DxMRR : dealloc'

      END IF

C     --------------------------------------
C     ESTIMATES OF VARIANCE COMPONENTS, ETC.
C     --------------------------------------

      IF(NTIME(1)>0)write(*,'(a,g16.6)')'AV. TIME / DFLIK =',BTIME(1)/
     &                                                         NTIME(1)
      IF(NTIME(2).GT.0)write(*,'(a,g16.6)')'AV. TIME / DXLIK=',
     *                                                BTIME(2)/NTIME(2)

      XLIKE=-0.5D0*FMIN
      if(nanim>0)then
         XLIKE=xlike -DETL*KSFIT(1)
         IF(IOPCOV.EQ.1.OR.IOPCOV.EQ.2)XLIKE=XLIKE-DETL*KSFIT(2)
      end if
      IF(IOPRUN.EQ.0 .AND. dabs(FSTART-BIG)<zz0)FSTART=FEVAL(1,1)

!     write out solutions
      if(ffmin<big)then
         write(iun54)ffmin,nsrow,nparm
         write(iun54)savsol
         call dxslw3(2,0,iun88)
      end if

!     transform back to original scale
      save=xvec
      XVEC(:kparm)=XVEC(:kparm)*FFVEC(:kparm)
      if(kopt.eq.3.or.kopt.eq.1)then
         call dxsca33 (xvec)
      else if(kopt.gt.0)then
         call dxsca3 (kopt,xvec)
      end if 

      WRITE(*,*)'STARTING VALUES & ESTIMATES FOR THIS RUN :'
      DO  I=1,NPARM
      if( iabs(iosrch) .eq. 3)then
         WRITE(*,2412)I,PARAM(I),START(I),XVEC(I),devl1(i)
         WRITE(IUN66,2412)I,PARAM(I),START(I),XVEC(I),devl1(i)
      else
         WRITE(*,2412)I,PARAM(I),START(I),XVEC(I)
         WRITE(IUN66,2412)I,PARAM(I),START(I),XVEC(I)
      end if
      END DO
      WRITE(*,2412)0,'-2 LOG L    ',FSTART,FMIN
      WRITE(iun66,2412)0,'-2 LOG L    ',FSTART,FMIN
      WRITE(*,*)'CURRENT MAXIMUM OF LOG L',XLIKE
      WRITE(*,*)'IMPROVEMENT IN LOG L    ',0.5D0*(FMIN-FSTART)
      WRITE(iun66,*)'IMPROVEMENT IN LOG L    ',0.5D0*(FMIN-FSTART)

!     ------------------------------
C     WRITE OUT RESULTS TO UNIT "66"
!     ------------------------------

 1000 CALL DFWRT3(KPARM,IOPRUN,XLIKE,FSTART)

!     calculate ML log likelihood
      if((ioprun.eq.0.or.ioprun.eq.1) .and. sum(kftfix(:nq))>0)then
         write(*,*)'Calculate full (ML) likelihood ?'
         call yndef(ii,0)
         if(ii.eq.1)then
            call all_xsprse (nsrow,maxlnz)
            call dxdet3(save,xlike,detzhz)
            deallocate(xspars,dia,stat=ii)
            if(ii>0)stop 'DxMRR : dealloc'
         end if
      end if

C     WRITE OUT COVARIANCE FUNCTIONS
      CALL DXCOVF(xlike,ioprun)
     
C     SET UP COVARIANCE MATRICES & CALCULATE CORRELATIONS (for ages in data)
      call deall_sigmas(0,0)
      if(ioprun.eq.0.or.ioprun.eq.1.or.ioprun.eq.8 )then
         write(*,*)'write out cov. matrices for ages in the data ?'
         call yndef(iopt19,0)
         if(iopt19.eq.1)then
            call all_sigmas(manq,manq)
            CALL DFCOR3
            CALL DFWRV3 
            call deall_sigmas(0,0)
         end if
      else
         iopt19=0
      end if

!     calculate backsolutions
      if(ioprun.eq.5)then
         ii=0
         read(iun54,end=199,err=199)ffmin,nsrow1,nparm1
         if(dabs(ffmin-fmin)<zz0.and.nsrow.eq.nsrow1.and.
     &   nparm.eq.nparm1)then
            ii=1
            read(iun54)solns
            write(*,*)'vector of solutions recovered from DF54#DAT'
         end if
 199     if(ii.eq.0)then
            call all_xsprse(nsrow,maxlnz)
            call all_sigmas(mfitmx,ksfit(7))
            CALL DXLIK3(xvec,FVALUE,xlike,3)
            deallocate(xspars,dia,stat=ii)
            if(ii>0)stop 'DxMRR : dealloc ioprun=5'
            rewind(iun54)
            write(iun54)fvalue,nsrow,nparm
            write(iun54)solns
         end if
         call dxslw3(iopslv,1,iun66)
         go to 900

!     calculate average information matrix 
      else IF(IOPRUN.EQ.8 .OR. iabs(IOSRCH).NE.3 )THEN
         IF(dabs(fmin-big)<zz0)go to 900
         WRITE(*,*)' '
         WRITE(*,*)'CALCULATE AVERAGE INFORMATION MATRIX/ S.E. ?'
         CALL YNDEF(MM,0)
         IF(MM.EQ.1)then
            call all_xsprse(nsrow,maxlnz)
            call all_derivs(nparm)
            nq2=mfitmx+ioprn2*mfitmx
            call all_dmatrices (nq,nq2,mobs,nrparm,ngparm,ncomb,
     &                                                  max_comb)
            allocate(trran1(nparm),trres1(nparm,ncomb),stat=ii)
            if(ii>0)stop 'alloc : traces'
            call all_sigmas(mfitmx,kfit(7,1))
            if(ioprun.ne.8)then
               CALL DXLIK3(save,FVALUE,xlike,2)
             else
               call dxlik3(xvec,fvalue,xlike,2)
            end if
            deallocate(xspars,dia,stat=ii)
            if(ii>0)stop 'DxMRR : dealloc'
         else
            fvalue=big
         end if
      END IF
         
C     write out approximate hessian matrix
      if(dabs(fvalue-big)>zz0)call dxapse

c     write out fixed effects solutions
      IF(NCALLs(2).GT.0)CALL DxSLW3 (1,0,iun66)

 900  WRITE(IUN66,901)

!     write out string of estimates at end of output file 
      call write_vectors

      WRITE(IUN66,901)
      CALL DFTIME(time,xsecs,isecs,11,'DXLIK3')
      stop 'end of "DxMRR" '

901   FORMAT(/80('*')/)
2412  FORMAT(I4,4X,A,3G17.9)

      contains

C     =================
      SUBROUTINE DxOPEN
C     =================

      character(len=25) :: fstand,UNFOR='UNFORMATTED',FORMA='FORMATTED',
     &                     OLD='OLD', UNKN='UNKNOWN', STA

C     SET UNIT NO.S FOR INPUT/OUTPUT FILES
      CALL SET_UNITNOS 

      FSTAND='DF11#DAT'                       ! output from dfprep :
      CALL FCONCT(IUN11,FSTAND,UNFOR,OLD)     ! no.s of effects, etc.
      IF(IOPRUN.LE.-1)THEN
         FSTAND='DF22#DAT'                    ! output from dfprep :
         CALL FCONCT(IUN22,FSTAND,UNFOR,OLD)  ! data file
         FSTAND='DF23#DAT'                    ! output from dfprep :
         CALL FCONCT(IUN23,FSTAND,UNFOR,OLD)  ! means & sdev.s
      END IF    
      IF(IOPRUN.EQ.-1)THEN
         STA=UNKN
       ELSE
         STA=OLD
         IF(IOPRUN.LE.5 )THEN
            FSTAND='DF54#DAT'
            CALL FCONCT(IUN54,FSTAND,UNFOR,UNKN)
         END IF
      END IF
      FSTAND='DF13#DAT'                    ! output from dflsq3 :
      CALL FCONCT(IUN13,FSTAND,UNFOR,STA)  ! lsq solutions fix eff.s
      FSTAND='DF51#DAT'
      CALL FCONCT(IUN51,FSTAND,UNFOR,STA)
      FSTAND='DF52#DAT'                    ! output from dfmme3 :
      CALL FCONCT(IUN52,FSTAND,UNFOR,STA)  ! recoded data
      FSTAND='DF44#DAT'                    ! output from dfprep :
      CALL FCONCT(IUN44,FSTAND,UNFOR,OLD)  ! NRM inverse
      FSTAND='DF66#DAT'                    ! output file
      IF(IOPRUN.EQ.8)FSTAND='DF77#DAT'     ! reduced output file
      CALL FCONCT(IUN66,FSTAND,FORMA,UNKN)
      FSTAND='DF88#DAT'
      CALL FCONCT(IUN88,FSTAND,FORMA,UNKN)
      RETURN
      END subroutine dxopen

!     ====================
      subroutine dx_open45
!     ====================

      CHARACTER(LEN=25)::FSTAND='DF45#DAT',UNFOR='UNFORMATTED',OLD='OLD'
      CALL FCONCT(IUN45,FSTAND,UNFOR,OLD)  ! file with user-supplied
      RETURN                               ! cov matrix for add. RE
      END subroutine dx_open45

!     =================
      subroutine dxmenu
!     =================

      WRITE(*,*)'GIVE RUN OPTION :'
      WRITE(*,9)'-1    ... PREPARE : Read Info on Model etc.           '
      WRITE(*,9)'-2    ... ORDER : Re-order equations (reduce fill-in) '
      WRITE(*,9)' 0/1  ... ESTIMATE : 0 - First run/ 1 - continue      '
!     WRITE(*,9)' 5    ... SOLVE : Backsolutions Fixed/Random Effects  '
      WRITE(*,9)' 8    ... BEST : Pick out best point so far           '
      WRITE(*,fmt9(ipltf))'10    ... GRID : Generate covariances    '
9     FORMAT(7X,A)
      CALL OPTION(IOPRUN,-2,10)
      if(ioprun.eq.-1.or.ioprun.eq.-2.or.ioprun.eq.0.or.ioprun.eq.5
     &   .or.ioprun.eq.8.or.ioprun.eq.10.or.ioprun.eq.1 )return
      write(*,*)'* * * * invalid option for DxMRR ! * * * * '
      stop '"DxMRR"'
      return
      end subroutine dxmenu

!     ========================
      subroutine write_vectors
!     ========================

!     write out estimated coefficient matrices of covariance functions
      if(jparam.ge.2)then
         rewind(iun17)
         do i=1,kparm
         read(iun17,*)xx
         write(iun66,'(g15.7,10x,a)')xx,param(i)
         end do
      end if

!     write out estimated/reconstructed covariance matrices
      if(iopt19.eq.1)then
         rewind(iun19)
         write(iun66,*)' '
 51      read(iun19,'(g20.8,10x,a)',end=91)xx,aa
         write(iun66,'(g15.7,10x,a)')xx,aa
         go to 51
      end if
 91   return
      end subroutine write_vectors

!     ===================
      subroutine start_up
!     ===================

      fvalue=0.d0
      BIG=10.D37
      NNEG=0
      IORDER=0
      lim0=1
      atime=0.d0
      btime=0.d0
      ntime=0
      ncall=0
      ncalls=0
      save=0.d0
      xvec=0.d0
      LCALL=0
      LROUND=0
      IPVRUN=0
      NRANK=0
      kopt=0

!     version no. & program title
      CALL DxVERSion(0)
      WRITE(*,909)
909   FORMAT(/1X,T20,31('$')/
     * 1X,T20,'$',T50,'$'/
     * 1X,T20,'$',4X,'PROGRAM " D x M R R " ',T50,'$'/
     * 1X,T20,'$',T50,'$'/
     * 1X,T20,25('$'),'*KM*$$'/)

!     time and date
      call today_is(0)
      return
      end subroutine start_up

!     ==================
      subroutine maxparm
!     ==================

      logical :: lexist

      inquire(file='MAXPARM',exist=lexist)
      open(1,file='MAXPARM',status='unknown',form='formatted')

!     read max. value for no. of parameters fitted
      if(lexist)then
         read(1,*)mxparm
      else
         write(*,fmt2(ipltf))'maximum no. of parameters to be',
     &                                                  ' estimated ? '
         call option(mxparm,1,999)
         write(1,*)mxparm
      end if         
      close(1)

!     allocate arrays
      allocate(xvec(mxparm),save(mxparm),save1(mxparm),start(mxparm),
     &         stat=ii)
      if(ii>0)stop 'DXMUX : alloc xvec'

      return
      end subroutine maxparm

!     =================================
      subroutine sigeps_direct (iopeps)
!     =================================

!     read option which allows a single measurement error variance
!     to be estimated directly from residual SS

      integer, intent(out) :: iopeps
      iopeps=0

!     not an option in these cases ...
      if(iosrch>0 .or. kfit(7,1).eq.0 .or. irropt(4,1).eq.4)return

      write(*,*)'Estimate "sig_eps" directly from residual SS ?'
      if(nrparm.eq.1 .or. (nrparm<4 .and.iosrch.eq.1))then
         call yndef(iopeps,1)
      else
         call yndef(iopeps,0)
      end if
      if(iopeps.eq.1 .and.nrparm>1.and.iosrch.eq.-2)write(*,*)
     &   'direct estimation with powell & several sigeps sometimes '
     &,  'causes convergence problems     !  '

!     express m.e. var as ratio of last component
      if(iopeps.eq.1.and.kfit(7,1)>0)then
         do i=1,nrparm-1   
         xvec(nparm-nrparm+i)=xvec(nparm-nrparm+i)/xvec(nparm)
         end do
      end if
      return
      END subroutine sigeps_direct

      END program DxMRR






















