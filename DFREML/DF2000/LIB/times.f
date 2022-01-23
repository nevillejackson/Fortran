!============================================================================
!     Collection of DFREML subroutines to make change between systems easy
!     - all are pure ornamental & the body of the routimes cam be commented
!       out to leave a dummy routine only if features are not available.
!                Edit as required before building library !!!!!
!===================================================================KM=======

!     ------------------------------------------------
      SUBROUTINE DFTIME (time,xsecs,isecs,iopt,subrou)
!     ------------------------------------------------

!     purpose : get system time & write out CPU time used

      REAL(8), INTENT(INOUT)       :: time
      REAL(8), INTENT(INOUT)       :: xsecs
      INTEGER, INTENT(INOUT)       :: isecs
      INTEGER, INTENT(IN)          :: iopt
      CHARACTER(LEN=6), INTENT(IN) :: SUBROU

      REAL, DIMENSION(2)           :: TVEC
      integer                      :: itime
      real                         :: tt

      if(iopt>9)tt=time

!     ----------------- customize this section  -------------------------
!     get current CPU time - comment out section not applicable !!
!     UNIX
!      CALL ETIME(TVEC)
!      TIME=TVEC(1)

!     FORTRAN 95 intrinsic subroutine
      call cpu_time(time)

!     lahey lf90
!     call timer(itime)
!     time=itime/100.d0
!     ---------------- end of customizable section ---------------------

      if(iopt>9)then
         xsecs=time-tt
         isecs=nint(xsecs)
      end if

!     write out 
      IF(IOPT.EQ.1)then
         WRITE(*,'(1X,A,I6)')'TIME AT START =',nint(time)
      ELSE IF(IOPT.EQ.2)then
          WRITE(*,'(1x,3a,f10.2)')'Routine "',subrou,
     &                                '" : time at start =',time

      else if(iopt.eq.11)then
         WRITE(*,'(1X,A,I6)')'TIME AT END           =',nint(time)
      else if(iopt.eq.12)then
         WRITE(*,'(1X,A,I6)')'TIME AT END           =',nint(time)
         WRITE(*,'(1X,A,I6)')'CPU TIME (SEC.S) USED =',nint(xsecs)
      else if(iopt.eq.13)then
         WRITE(*,'(1X,3a,f10.2)')'Routine "',subrou,
     &                                '" : time at end   =',time
         WRITE(*,'(1X,a,f10.4)')'                 : cpu time used =',
     &                                                         XSECS
      else if(iopt.eq.14)then
         WRITE(*,'(1X,3a,f10.4)')'Routine "',subrou,
     &                           '" : cpu time used =',xsecs
      END IF

      RETURN
      end subroutine dftime

!     ======================
      subroutine dfhost(iun)
!     ======================

!     purpose : get name of host & write out
      character(len=40)        :: machine='not determined             '
      integer                  :: ii,  hostnm  
      external  hostnm         ! unix specific intrinsic function

!     ----------------- customize this section  -------------------------
!     UNIX specific 
!     ii=hostnm(machine)
!     other systems
       ii=0
!     ---------------- end of customizable section ---------------------

      if(ii.eq.0.and.iun>0)then
         write(iun,'(3a)')' Running on host : "',trim(machine),'" '
      else if(ii.eq.0)then
          write(*,'(3a)')' Running on host : "',trim(machine),'" '
      end if
      return
      end subroutine dfhost

!     ========================
      subroutine dfcwd (cwdir)
!     ========================

!     purpose : get name of current working directory 

      character(len=50), intent(out) :: cwdir
      integer                        :: getcwd,ii
      external getcwd                ! unix specific intrinsic function

      cwdir='not determined                                   '

!     ----------------- customize this section  -------------------------
!     UNIX specific - comment out for other systems
!     ii=getcwd(cwdir)
!     -- -------------- end of customizable section ---------------------
      return
      end subroutine dfcwd





