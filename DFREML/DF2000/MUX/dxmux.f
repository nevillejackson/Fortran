!============================================================================
      PROGRAM DxMuX
!============================================================================

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
      use platform
      use version
      use today
      use dmatrices

      real(8)                     :: fmin,fstart,xlike,xx,x1,x2,fvalue,
     &                               time,xsecs
      integer                     :: nparm,itime,lim0=1,ioprun,kq,jjzero
      real(8), dimension (mxparm) :: xvec,start,save
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      CALL DFTIME(time,xsecs,isecs,1,'DXLIK3')

c     initialise
      big=1.d37
      IORDER=0
      btime=0.d0
      ntime=0
      ncalls=0
      call today_is(0)
      CALL DxVERSion(0)
      WRITE(*,909)

!     read run option
      call dxmenu

!     open DFij#DAT files
      CALL DxOPEN
      call today_is(iun66)

      IF(IOPRUN.EQ.-1)GO TO 7000

C     READ INFORMATION ON MODEL OF ANALYSIS & DATA STRUCTURE FROM "51"
      CALL DF51R3(IUN51)
      IF(iopmod.eq.3.and.ioprun.eq.10)then
         write(*,*)'invalid run option - this analysis does not fit '
         write(*,*)'covariance functions !!!                        '
         stop
      end if
    
      IF(IOPCOV.EQ.4)call dx_open45

C     --------------------------------------------------
C     READ STARTING VALUES & SET UP VECTOR OF PARAMETERS
C     --------------------------------------------------

      CALL OPZERO(ZERO,0)
      CALL DFWRH3(IOPRUN)
      CALL DFINP3(iinput,NPARM,IOPRUN,XVEC)
      NCALL=0
      IPVRUN=0
      NRANK=0
      FMIN=BIG

      allocate(siga(nq,nq),sigm(nq,nq),sigam(nq+nq,nq+nq),sigq(nq,nq),
     &   sigc(nq,nq),sige(nq,nq),sigr(nq,nq),sigp(nq,nq),eiga(nq),
     &   eigm(nq),eigam(nq+nq),eigc(nq),eigr(nq),eige(nq),eigp(nq),
     &   eigq(nq),stat=ii)
      if(ii>0)stop 'alloc : sigmas'
      allocate(solns(0:neqns),savsol(0:neqns),stat=ii)
      if(ii>0)stop 'alloc : solns'

      solns(0)=0.d0
      ICONV=0
      ONE=1.D0-ZERO
      FSTART=FMIN

7000  NTRAIT=1
      KQ=1
      IF(IOPRUN.EQ.-1)THEN
C        SET-UP STEP
         CALL DFPRE3
         CALL DFWRH3(IOPRUN)
         CALL DFLSP3(LIM0)
         CALL DF51W3(IUN51)
         go to 999

      ELSE IF(IOPRUN.EQ.-2)THEN

         EIGZER=ZERO
         CALL DVORD3(NPARM,XVEC)
         CALL DF51W3(IUN51)
         call dfwr59
         go to 999

      ELSE 

c        operational zero for eigenvalues of reduced rank matrices
         if(kfit(1).lt.nq)then
            write(*,*)' '
            write(*,*)'Operational zero for eigenvalues ?'
            WRITE(*,*)'       3  ...  0.001      '
            WRITE(*,*)'       4  ...  0.0001     '
            WRITE(*,*)'          ...             '
            WRITE(*,*)'       N  ...  10**(-N)   '
            CALL OPTDEF(JJZERO,1,10,4)
            EIGZER=10.D0**(-JJZERO)
         else
            eigzer=1.1d0*zero
         end if

         if(jparam.eq.2)then

C           SET UP MATRIX OF COEFFICIENTS FOR LEGENDRE POLYNOMIALS
            CALL DXLGND (NQ)

C           EVALUATE AT GIVEN AGES
            CALL PHIMAT (NQ,NQ)

C           TRANSFORM FROM VAR. COMP. SCALE TO K MATRIX SCALE
            CALL DXKFG3 (XVEC,NPARM)
c           READ STARTING VALUES OF K-MATRICES
            IF(IINPUT.GT.2)THEN
               WRITE(*,*)'STARTING VALUES FOR COVARIANCE FUNCTIONS '
               X1=-1.D12
               X2=1.D12
               IUN=0
               IF(IINPUT.EQ.4)IUN=16
               DO I=1,KPARM
               IF(IUN.EQ.0)WRITE(*,*)' ',PARAM(I)
               CALL RVALFL(XX,X1,X2,IUN)
               IF(IUN.GT.0)WRITE(*,*)' READ : ',PARAM(I),' = ',XX
               XVEC(I)=XX
               END DO
C              ... MAKE SURE K-MATRICES ARE P.S.D.
               CALL DXKTG3 (XVEC,FVALUE,NPARM,IOUT,2)
               IF(IOPRUN.EQ.10)CALL DXGRD3 (XVEC)
            END IF
         ELSE
            KPARM=NPARM
         END IF

         allocate (feval(nparm+1,mxfunc), stat=ii)
         if(ii>0)stop 'alloc : likelihoods '

C        RECOVER ANY FUNCTION VALUES FROM CRASHED PREVIOUS RUN
         MM=18
         CALL DFRC59 (KPARM,KQ,MM,FMIN,XVEC)
         IF(FMIN.LT.FSTART)FSTART=FMIN

         START(:kparm)=XVEC(:kparm)

C        PICK OUT BEST POINT ONLY
         IF(IOPRUN.EQ.8 .or. ioprun.eq.5)THEN
           XLIKE=-0.5D0*FMIN
           GO TO 1000
         END IF

         FFMIN=BIG
         WRITE(*,*)'PARAMETERS TO BE ESTIMATED :'
         DO I=1,kPARM
         WRITE(*,2412)I,PARAM(I),XVEC(I)*FFVEC(I)
         END DO

c        -----------------------
C        MAXIMISE THE LIKELIHOOD
c        -----------------------

         open(28,file='Iterates',status='unknown',form='formatted',
     *                                         position='append')
         call all_xsprse (nsrow,maxlnz)

         WRITE(*,*)' '
         WRITE(*,*)'SEARCH PROCEDURE TO BE USED',
     &             ' (negative value to modify defaults) :'
         WRITE(*,9)'(-)1   ...  SIMPLEX'
         WRITE(*,9)'(-)2   ...  POWELL '
         WRITE(*,9)'(-)3   ...  AI-REML '
         CALL OPTDEF(IOSRCH,-3,3,3)
         if(iosrch.eq.0)stop 'iosrch invalid'

         write(*,*)' '
         write(*,*)'transform for estimation ?'
         write(*,*)'        0  ...  original scale '
         write(*,*)'        1  ...  cholesky decomp. '
         write(*,*)'        2  ...  cholesky d. + log(diag.s)'
         call optdef(kopt,0,2,2)
         if(kopt.gt.0.and.iopcov.eq.2)stop 'option not implemented'

C        initialise for transformation to cholesky scale
         if(kopt.gt.0)then
            call sindex 
            call setup (mxparm)
            call dxkhn3(nparm,kopt,xvec)
         end if

c        "AI-REML"
         IF( iabs(IOSRCH).EQ.3)THEN

            allocate(trran1(nparm),trres1(nparm,ncomb),stat=ii)
            if(ii>0)stop 'alloc : traces'
            call all_derivs(nparm)
            nq2=nq
!            if(ioprn2.eq.1.and.iopcov.le.2)nq2=nq222+nq
            if(ioprn2.eq.1.and.iopcov.le.2)nq2=nq+nq
            call all_dmatrices (nq2,mobs,mxparm,ncomb)

            CALL DXNRP3 (IOPRUN,NPARM,iconv,XVEC,FMIN)
      
         ELSE

            ipartm=0
            if(jparam.eq.1.and.kopt.eq.0 .and.iosrch<0)then
               write(*,*)'Maximise w.r.t to a subset of parameters ?'
               write(*,9)'0  ...  NO                  '
               write(*,9)'1  ...  fix (co)variance(s) '
               write(*,9)'2  ...  fix correlation(s)  '
               call optdef(ipartm,0,2,0)
               if(ipartm.gt.0)call dfipm3 (ipartm,nparm,xvec)
            end if

C           RESCALE
            FFVEC(:kparm)=XVEC(:kparm)
            XVEC(:kparm)=1.D0
            where(FFVEC(:kparm).EQ.0.AND.IPM(:kparm).EQ.0)FFVEC(:kparm)=
     &                                                           0.001D0

C           SIMPLEX
            IF(iabs(IOSRCH).EQ.1)THEN         
               CALL DFSPX3(IOPRUN,NPARM,IMIN,ICONV,XVEC,FMIN)

C           POWELL
            ELSE
               CALL DFPOW3 (IOPRUN,NPARM,KPARM,ICONV,XVEC,FMIN)
            END IF
            if(ipartm.gt.0)call dxlik3(NPARM,XVEC,FMIN,0)
         END IF

         deallocate(xspars,dia,stat=ii)
         if(ii>0)stop 'dxmux : dealloc'

      END IF

C     --------------------------------------
C     ESTIMATES OF VARIANCE COMPONENTS, ETC.
C     --------------------------------------

      IF(NTIME(1).GT.0)PRINT *,'AV. TIME / DFLIK =',BTIME(1)/NTIME(1),
     *                                                       NTIME(1)
      IF(NTIME(2).GT.0)PRINT *,'AV. TIME / DXLIK =',BTIME(2)/NTIME(2),
     *                                                       NTIME(2)

      XLIKE=-0.5D0*FMIN
      IF(IOPRUN.EQ.0 .AND. dabs(FSTART-BIG)<zz0)FSTART=FEVAL(NPARM+1,1)

      WRITE(*,*)'STARTING VALUES & ESTIMATES FOR THIS RUN :'
      XVEC(:kparm)=XVEC(:kparm)*FFVEC(:kparm)
      if(kopt.gt.0)call dxsca3(kparm,kopt,xvec)
      DO  I=1,kPARM
      WRITE(*,2412)I,PARAM(I),START(I),XVEC(I)
      WRITE(iun66,2412)I,PARAM(I),START(I),XVEC(I)
      END DO
      WRITE(*,2412)0,'-2 LOG L    ',FSTART,FMIN
      WRITE(iun66,2412)0,'-2 LOG L    ',FSTART,FMIN
      WRITE(*,*)'CURRENT MAXIMUM OF LOG L',XLIKE
      WRITE(*,*)'IMPROVEMENT IN LOG L    ',0.5D0*(FMIN-FSTART)
      WRITE(iun66,*)'IMPROVEMENT IN LOG L    ',0.5D0*(FMIN-FSTART)

!     write out solutions
      if(ffmin<big)then
         write(iun54)ffmin,nsrow,nparm
         write(iun54)savsol
      end if

C     WRITE OUT RESULTS TO UNIT "66"

 1000 CALL DFWRT3(KPARM,IOPRUN,XLIKE,FSTART,START)

C     WRITE OUT COVARIANCE FUNCTIONS
      IF(JPARAM.EQ.2)THEN
          CALL DXCOVF (XVEC,kparm)
          write(iun17,*)xlike

C         TRANSFORM TO VAR/COV
          save=xvec
          call dxktg3 (xvec,fvalue,nparm,iout,1)
      end if

C     SET UP COVARIANCE MATRICES & CALCULATE CORRELATIONS 
      CALL DFCOR3(NPARM,XVEC)

      CALL DFWRV3 (XVEC,NPARM)
      write(iun19,*)xlike

      IF(dabs(fmin-big)<zz0)go to 900

!     calculate backsolutions
      if(ioprun.eq.5)then
         ii=0
         read(iun54,end=199,err=199)ffmin,nsrow1,nparm1
         if(dabs(ffmin-fmin)<zz0.and.nsrow.eq.nsrow1.and.
     &                                         nparm.eq.nparm1)then
            ii=1
            read(iun54)solns
            write(*,*)'vector of solutions recovered from DF54#DAT'
         end if
 199     if(ii.eq.0)then
            call all_xsprse(nsrow,maxlnz)
            CALL DXLIK3(nparm,xvec,FVALUE,3)
            deallocate(xspars,dia,stat=ii)
            if(ii>0)stop 'DxMux : dealloc ioprun=5'
            rewind(iun54)
            write(iun54)fvalue,nsrow,nparm
            write(iun54)solns
         end if
         call dxslw3(iopslv,1)
         go to 900

!     calculate average information matrix 
      else IF(IOPRUN.EQ.8 .OR. IOSRCH.NE.3 )THEN
         WRITE(*,*)' '
         WRITE(*,*)'CALCULATE AVERAGE INFORMATION MATRIX/ S.E. ?'
         CALL YNDEF(MM,0)
         IF(MM.EQ.0)GO TO 919
         FFVEC=1.d0
         call all_xsprse(nsrow,maxlnz)
         nq2=nq
         if(ioprn2.eq.1.and.iopcov.le.2)nq2=nq222+nq
         call all_dmatrices (nq2,mobs,mxparm,ncomb)
         allocate(trran1(nparm),trres1(nparm,ncomb),stat=ii)
         if(ii>0)stop 'alloc : traces'
         call all_derivs(nparm)

         kopt=0
         if(jparam.eq.1)then
            CALL DXLIK3(NPARM,XVEC,FVALUE,2)
         else
            CALL DXLIK3(NPARM,save,FVALUE,2)
         end if
         deallocate(xspars,dia,stat=ii)
         if(ii>0)stop 'dxmux : dealloc'
      END IF
         
C     write out approximate hessian matrix
      if(dabs(fmin-big)>zz0)call  DxAPSE (nparm,xvec,save)

c     write out fixed effects solutions
 919  IF(ioprun.eq.0.and.ffmin<big)CALL DxSLW3 (1,0)

 900  WRITE(IUN66,901)

c     write out string of estimates at end of output file 
      if(jparam.eq.2)then
         rewind(iun17)
         do i=1,kparm
         read(iun17,*)xx
         write(iun66,902)xx,param(i)
         end do
      end if
      rewind(iun19)
      write(iun66,*)' '
      do i=1,nparm
      read(iun19,*,end=999,err=999)xx
      write(iun66,902)xx,parvar(i)
      end do
 999  WRITE(IUN66,901)
      CALL DFTIME(time,xsecs,isecs,11,'DXLIK3')
      STOP 'end of "DXMUX"'

c.........................................................................
9     FORMAT(7X,A)
901   FORMAT(/80(1H*)/)
902   FORMAT(g15.7,10x,a)
909   FORMAT(/1X,T20,31('$')/
     * 1X,T20,'$',T50,'$'/
     * 1X,T20,'$',4X,'PROGRAM " D x M u X " ',T50,'$'/
     * 1X,T20,'$',T50,'$'/
     * 1X,T20,25('$'),'*KM*$$'/)
2412  FORMAT(I4,4X,A,3G18.10)

      contains

C     =================
      SUBROUTINE DxOPEN
C     =================

      LOGICAL           :: LEXIST
      CHARACTER(LEN=25) :: FSTAND,UNFOR='UNFORMATTED',FORMA='FORMATTED',
     &                     OLD='OLD', UNKN='UNKNOWN',sta

C     SET UNIT NO.S FOR INPUT/OUTPUT FILES
      call set_unitnos 

      FSTAND='DF11#DAT'                       ! output from dfprep
      CALL FCONCT(IUN11,FSTAND,UNFOR,OLD)
      IF(IOPRUN.LE.-1)THEN
         FSTAND='DF22#DAT'
         CALL FCONCT(IUN22,FSTAND,UNFOR,OLD)  ! output from dfprep
         FSTAND='DF23#DAT'
         CALL FCONCT(IUN23,FSTAND,UNFOR,OLD)
      END IF    
      IF(IOPRUN.EQ.-1)THEN
         sta=unkn
         FSTAND='DF59#DAT'
         INQUIRE(FILE=FSTAND,EXIST=LEXIST)
         IF(LEXIST)THEN
            WRITE(*,*)'UNIT "59" : FILE ',FSTAND,' ALREADY EXISTS !'
            WRITE(*,*)'OVERWRITE ?'
            CALL YNDEF(I59,1)
            IF(I59.NE.1)STOP 'RENAME FILE & TRY AGAIN ...'
         END IF
!         CALL FCONCT(IUN59,FSTAND,UNFOR,UNKN)
      ELSE
         sta=old
         IF(IOPRUN.LE.5 )THEN
            FSTAND='DF54#DAT'
            CALL FCONCT(IUN54,FSTAND,UNFOR,UNKN)
         END IF
      END IF
      FSTAND='DF13#DAT'                      ! lsq solns FE
      CALL FCONCT(IUN13,FSTAND,UNFOR,sta)
      FSTAND='DF51#DAT'
      CALL FCONCT(IUN51,FSTAND,UNFOR,sta)
      FSTAND='DF52#DAT'
      CALL FCONCT(IUN52,FSTAND,UNFOR,sta)
      FSTAND='DF44#DAT'
      CALL FCONCT(IUN44,FSTAND,UNFOR,OLD)
      FSTAND='DF66#DAT'
      IF(IOPRUN.EQ.8)FSTAND='DF77#DAT'
      CALL FCONCT(IUN66,FSTAND,FORMA,UNKN)
      CALL DxVERSion(iun66)
      RETURN
      END subroutine dxopen

!     ====================
      subroutine dx_open45
!     ===================

      CHARACTER(LEN=25)::FSTAND='DF45#DAT',UNFOR='UNFORMATTED',OLD='OLD'
      CALL FCONCT(IUN45,FSTAND,UNFOR,OLD)
      RETURN
      END subroutine dx_open45

!     =================
      subroutine dxmenu
!     =================

      WRITE(*,*)'GIVE RUN OPTION :'
      WRITE(*,9)'-1  ...  PREPARE : Read info on model etc.    '
      WRITE(*,9)'-2  ...  ORDER : re-order equations '
      WRITE(*,9)' 0/1...  ESTIMATE : 0=1st run/ 1=Continue   '
      WRITE(*,9)' 5  ...  SOLVE : Backsolutions              '
      WRITE(*,fmt(ipltf))' 8  ...  BEST : Best point do far  '
      CALL OPTION(IOPRUN,-2,8)
9     FORMAT(7X,A)
      if(ioprun.eq.-1.or.ioprun.eq.-2.or.ioprun.eq.0.or.ioprun.eq.5
     &   .or.ioprun.eq.8.or.ioprun.eq.1 )return
      write(*,*)'* * * * invalid option for DxMuX ! * * * * '
      stop '"DxMuX"'
      end subroutine dxmenu

      END program dxmux










