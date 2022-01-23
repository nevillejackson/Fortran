!===========================================================================
      PROGRAM dfuni
!===========================================================================

      use parameters
      use xsprse
      use sparse
      use lnk
      use likelihoods
      use names
      use simplex
      use powell
      use spasol
      use diagonal
      use order
      use works
      use rows
      use solve
      use means
      use variance_ratios
      use iterates
      use units
      use constants
      use numbers
      use platform
      use version
      use today

      implicit double precision (a-h,o-z)
      real(8)           :: time, xsecs
      CHARACTER(len=25) :: FSTAND,UNFOR='UNFORMATTED',OLD='OLD',
     &                     FORMA='FORMATTED',UNKN='UNKNOWN'

      CALL DFTIME(time,xsecs,isecs,1,'     ')
      CALL DxVERSION(0)
      WRITE(*,908)
      call today_is(0)
      call dfhost(iun28)
      btime=0.d0
      ntime=0

      WRITE(*,*)' '
      WRITE(*,*)'GIVE RUN OPTION :'
      WRITE(*,9)'-1   ...  PREPARE : Read Info on Model of Analysis '
      WRITE(*,9)'-2   ...  ORDER : Re-order equations               '
      WRITE(*,9)' 0/1 ...  ESTIMATE : Carry out estimation step(s)  '
      WRITE(*,9)' 5   ...  SOLVE : Obtain Backsolutions             '
      WRITE(*,fmt9(ipltf))' 8  ...  BEST : Pick out best point so far'
      CALL OPTION(IOPRUN,-2,8)
      if(ioprun>1.and.ioprun.ne.5.and.ioprun.ne.6.and.ioprun<8)
     &                                          stop 'invalid option'
9     FORMAT(7X,A)

333   CALL DFOPEN
      CALL DxVERSION(iun66)

!     set-up setp
      IF(IOPRUN.EQ.-1)then
         CALL DFPRE1(IOPMOD)
         CALL DFWRH1(IOPRUN)
         CALL DFLSP1
         CALL DF51W1(IOPMOD)
         CALL DFWR59(NPARM,KPARM,NQ,MM,FMIN,sige)
         go to 7000
      end if

C     READ INFO ON MODEL OF ANALYSIS & DATA STRUCTURE FROM UNIT "51"
      CALL DF51R1(IOPMOD)

      allocate(dia(neqns),savsol(0:nsrow),iwork(neqns),work(neqns),
     *         row(neqns),ivec(neqns),gamma(ioprn1),xlamb(nainv),
     &                                                     stat=ii)
      if(ii>0)stop 'err alloc savsol'
      SAVSOL(0)=0.d0

      IF(IOPRUN.NE.-1 .AND. IOPRUN.LT.8)THEN
         IF(IOPCOV.EQ.4)THEN
            FSTAND='DF45#DAT'
            CALL FCONCT(IUN45,FSTAND,UNFOR,OLD)
         END IF
         IF(jopcov.EQ.1)THEN
           FSTAND='DF47#DAT'
           CALL FCONCT(IUN47,FSTAND,UNFOR,OLD)
         END IF        
      END IF

C     -----------------
C     INTERACTIVE INPUT
C     -----------------

      CALL OPZERO(ZERO,0)
      ONE=1.D0-ZERO
      CALL DFINP1(IOPRUN,NPARM,KPARM)
      FMIN=BIG
      FFMIN=BIG
      sige=1.d0
         
      NCALL=0
      IPVRUN=0
      START(:nparm)=XVEC(:nparm)
      ICONV=0

      allocate(xhx(nq,nq),stat=ii)
      if(ii>0)stop 'alloc fxhx'

      CALL DFWRH1(IOPRUN)

      IF(IOPRUN.eq.-2)THEN
         CALL DFORD1
         CALL DF51W1(IOPMOD)
         GO TO 7000
      end if

C     RECOVER LIKELIHOOD VALUES FROM UNFINISHED RUN ...
      allocate(feval(nparm+2,mxfunc),stat=ii)
      if(ii>0)stop 'alloc feval'
      if(ioprun.eq.6)go to 6000  ! backsolutions only, use input values

      CALL DFRC59(NPARM,KPARM,NQ,FMIN,sige)
      START=XVEC
      FSTART=FMIN
      if(ioprun.eq.1 .and. fmin.eq.big)then
         write(*,*)'no previous likelihood points found',
     &                            ' - cannot "continue"'
         stop 'dfuni'
      end if
      IF(IOPRUN.eq.8)then
C        WRITE OUT RESULTS FILE only
         XLIKE=-0.5D0*FMIN
         CALL DFWRT1(IOPRUN,IOSRCH,NPARM,KPARM,XLIKE,Fmin,sige)
         WRITE(*,*)'*** PRELIMINARY RESULTS FILE IS "DF77#DAT" ! ***'
         go to 7000
      END IF

C     -----------------------
C     MAXIMIZE LOG LIKELIHOOD
C     -----------------------

       if(ioprun.eq.0.or.ioprun.eq.1)then

         IF(KPARM.EQ.1)THEN
            IOSRCH=3
         ELSE
            WRITE(*,*)'SEARCH PROCEDURE TO BE USED (negative value to',
     &                                         ' modify defaults):'
            WRITE(*,9)'(-)1   ...  SIMPLEX '
            WRITE(*,9)'(-)2   ...  POWELL  '
            if(kparm.eq.nparm)then
               WRITE(*,9)'(-1)11  ...  SIMPLEX - PARTIAL MAXIMIZATION'
               WRITE(*,9)'(-1)12  ...  POWELL  - PARTIAL MAXIMIZATION'
            end if
            CALL OPTDEF(II,-12,12,2)
            if(iabs(ii)>2.and.iabs(ii)<11)stop 'invalid option'
            IOSRCH=MOD(iabs(II),10)
            IF(ii>10)CALL DFIPM1(NPARM,KPARM,XVEC)
            IF(KPARM.EQ.1)iosrch=3
         END IF

         allocate(xspars(maxlnz),rhs(nq,neqns),stat=iii)
         if(iii>0)then
             write(*,*)'no. of elements in "xspars" =',maxlnz
             jj=(maxlnz*8.d0)/1024.d0
             write(*,*)' ... size in Kilobytes      =',jj
             stop 'err alloc xspars'
         end if
         print *,'xspars allocated',maxlnz

C        CARRY OUT SEARCH
         IF(IOSRCH.EQ.1)THEN
            CALL DFSPX1(ii,NPARM,KPARM,ICONV,FMIN,SIGE)
         ELSE IF(IOSRCH.EQ.2)THEN
            CALL DFPOW1(ii,NPARM,KPARM,ICONV,FMIN,SIGE)
         ELSE IF(IOSRCH.EQ.3)THEN
            CALL DFQUA1(ii,NPARM,KPARM,IVAR,ICONV,FMIN,SIGE)
         END IF
         if(ntime.gt.0)print *,'av. time / DFLIK1     =',btime/ntime

!        write out solutions
         if(ffmin<big)then
            write(iun54)ffmin,nsrow,nparm,ntrait
            write(iun54)savsol
         end if

      END IF

      XLIKE=-0.5D0*FMIN
      IF(IOPRUN.EQ.0)FSTART=FEVAL(NPARM+1,1)

C     WRITE OUT RESULTS TO UNIT "66"
 6000 CALL DFWRT1(IOPRUN,IOSRCH,NPARM,KPARM,XLIKE,FSTART,sige)

C     AT CONVERGENCE ...
      IF(ICONV.EQ.1)THEN

C        ATTEMPT TO APPROXIMATE SAMPLING ERRORS
         CALL DFAPSE(NPARM,NCALL,FMIN)

C        BACKSOLUTIONS FOR FIXED EFFECTS & COVARIABLES
         CALL DFSLW1(1)

      ELSE IF(IOPRUN.eq.5.or.ioprun.eq.6)THEN

         print *,'backsolutions'
         ii=0
         if(ioprun.eq.5)then
         read(iun54,end=199,err=199)ffmin,nsrow1,np1,ntr1
         if(dabs(ffmin-fmin)<1.d-5.and.nsrow.eq.nsrow1.and.nparm.
     &           eq.np1.and. ntrait.eq.ntr1)then
            ii=1
            read(iun54)savsol
            write(*,*)'vector of solutions recovered from DF54#DAT'
         end if
         end if
 199     if(ii.eq.0)then
            allocate(xspars(maxlnz),rhs(nq,neqns),stat=ii)
            if(ii>0)stop 'err alloc xspars'
            print *,'xspars allocated - sol',maxlnz
            ffmin=big
            ncall=0
            CALL DFLIK1(NPARM,XVEC,FVAL,SIGE)
            rewind(iun54)
            write(iun54)fval,nsrow,nparm,ntrait
            write(iun54)savsol
         end if

         CALL DFSLW1(2)

      end if

7000  WRITE(IUN66,901)
      CALL DFTIME(time,xsecs,isecs,11,subrou)
      stop 'End of "DFUNI" '

901   FORMAT(/80(1H*)/)
908   FORMAT(/1X,T20,31(1H$)/
     * 1X,T20,1H$,T50,1H$/
     * 1X,T20,1H$,4X,'PROGRAM " D F U N I " ',T50,1H$/
     * 1X,T20,1H$,T50,1H$/
     * 1X,T20,25(1H$),'*KM*$$'/)

      contains

C     =================
      SUBROUTINE DFOPEN
c     =================

      LOGICAL  :: LEXIST

      FSTAND='DF11#DAT'
      CALL FCONCT(IUN11,FSTAND,UNFOR,OLD)
      IF(IOPRUN.LE.-1)THEN
         FSTAND='DF22#DAT'
         CALL FCONCT(IUN22,FSTAND,UNFOR,OLD)
         FSTAND='DF23#DAT'
         CALL FCONCT(IUN23,FSTAND,UNFOR,OLD)
      END IF    
      IF(IOPRUN.EQ.-1)THEN
         FSTAND='DF13#DAT'
         CALL FCONCT(IUN13,FSTAND,UNFOR,UNKN)
         FSTAND='DF51#DAT'
         CALL FCONCT(IUN51,FSTAND,UNFOR,UNKN)
         FSTAND='DF52#DAT'
         CALL FCONCT(IUN52,FSTAND,UNFOR,UNKN)
         FSTAND='DF59#DAT'
         INQUIRE(FILE=FSTAND,EXIST=LEXIST)
         IF(LEXIST)THEN
            WRITE(*,*)'UNIT "59" : FILE ',FSTAND,' ALREADY EXISTS !'
            WRITE(*,*)'OVERWRITE ?'
            CALL YNDEF(I59,1)
            IF(I59.NE.1)STOP 'RENAME FILE & TRY AGAIN ...'
         END IF
      ELSE
         FSTAND='DF13#DAT'
         CALL FCONCT(IUN13,FSTAND,UNFOR,OLD)
         FSTAND='DF51#DAT'
         CALL FCONCT(IUN51,FSTAND,UNFOR,OLD)
         FSTAND='DF52#DAT'
         CALL FCONCT(IUN52,FSTAND,UNFOR,OLD)
         FSTAND='DF54#DAT'
         CALL FCONCT(IUN54,FSTAND,UNFOR,UNKN)
      END IF
      FSTAND='DF44#DAT'
      CALL FCONCT(IUN44,FSTAND,UNFOR,OLD)
      FSTAND='DF59#DAT'
      CALL FCONCT(IUN59,FSTAND,UNFOR,UNKN)
      FSTAND='DF66#DAT'
      IF(IOPRUN.EQ.8)FSTAND='DF77#DAT'
      CALL FCONCT(IUN66,FSTAND,FORMA,UNKN)
      RETURN
      END subroutine dfopen

      END program dfuni






