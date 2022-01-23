!==========================================================================
      SUBROUTINE DFRC59 (ioprun,kparm,kq,mm,fmin)
C==========================================================================

      use params
      use parameters
      use comments
      use units
      use likelihoods
      use numbers
      use eigen_decomp

!     arguments
      integer, intent(in)                    :: kparm,kq,mm,ioprun
      real(8), intent(inout)                 :: fmin

!     local variables
      character(len=25)                      :: fstand,unkn='unknown',
     &                                          unfor='unformatted'
      logical                                :: lexist
      real(8), dimension (:), allocatable    :: wvec
      real(8)                                :: ffv
      integer                                :: ii59,n,j,i,llines,ii
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C     RECOVER ANY LIKELIHOOD EVALUATIONS FROM UNFINISHED RUNS
      fmin=big
      II59=-1
      N=-1
C     CHECK THAT WE'VE GOT A FILE FOR THIS DATA SET & ANALYSIS !
      FSTAND='DF59#DAT'
      CALL FCONCT(IUN59,FSTAND,UNFOR,UNKN)

      INQUIRE(FILE=FSTAND,EXIST=LEXIST)
      IF(LEXIST)THEN
         READ(IUN59,END=789)II59
         N=0
      END if
      IF(II59.EQ.0)THEN
         READ(IUN59)KOSVEC
         DO  I=1,MM
         if(i.eq.2.or.i.eq.10.or.i.eq.22)cycle
         IF(KOSVEC(I).NE.NOSVEC(I))THEN
            WRITE(*,*)'WRONG FILE "59" ??!!!'
            WRITE(*,*)'DISCREPANCY FOUND IN VECTOR "NOSVEC" :'
            DO J=1,mm
            WRITE(*,*)J,NOSVEC(J),KOSVEC(J)
            END DO
!           STOP 'STRAIGHTEN OUT FILES & MODELS !'
         END IF
         end do

         READ(IUN59)LLINES
         IF(LLINES.NE.KLINES)THEN
            WRITE(*,*)'WRONG FILE "59" ??!!!'
            WRITE(*,*)'DISCREPANCY FOUND IN NO. OF COMMENT LINES'
            STOP 'STRAIGHTEN OUT FILES & MODELS !'
         END IF

         IF(KLINES.GT.0)THEN
            DO I=1,KLINES
            READ(IUN59)TTEXT
            IF(TTEXT.NE.TEXT(I))THEN
               WRITE(*,*)'WRONG FILE "59" ??!!!'
               WRITE(*,*)'DISCREPANCY FOUND IN COMMENT LINE',I
               WRITE(*,*)'FOUND :  ',TTEXT
               WRITE(*,*)'EXPECT : ',TEXT(I)
c              STOP 'STRAIGHTEN OUT FILES & MODELS !'
            END IF
            end do
         END IF
      END IF

      read(iun59,end=789)ii ! ksfit

c     read previously evaluated points
      allocate(wvec(kparm),stat=ii)
      if(ii>0)stop 'alloc wvec'
777   READ(IUN59,END=769,ERR=779)N,ffv,WVEC!,kfteig
      IF(ffv.LT.FMIN)THEN
         FMIN=ffv
         XVEC(:kparm)=WVEC
      END IF

      GO TO 777

 769  deallocate(wvec,stat=ii)
      if(ii>0)stop 'de-alloc wvec'

789   if(fmin<big)then
         WRITE(*,*)' '
         write(*,'(a,f20.10)')
     &       ' Point recovered from "DF59#DAT" - log L =',-0.5d0*fmin
         write(*,'((5g15.6))')xvec(:kparm)
      end if
      close(iun59)
      RETURN

 779  write(*,*)'routine "dfrc59" : read error when picking up ',
     *                           'previously evaluated  points!'
      write(*,*)'** likely reason is discrepancy in no. of parameters'
      write(*,*)'** between this and a previous run'
      write(*,'(a,i50)')'   no. of values expected for current run =',
     &                                                          kparm+kq
      write(*,'(i6,g20.10)')n,ffv
      write(*,'((5g15.6))')wvec
      if(ioprun.eq.8.or.ioprun.eq.1)go to 789
      stop
      END subroutine dfrc59

C==========================================================================
      subroutine DFWR59
C==========================================================================

      use numbers
      use comments
      use units

      integer              :: ii59=0,i
      character(len=25)    :: fstand='DF59#DAT',unkn='unknown',
     &                        unfor='unformatted'

      CALL FCONCT(IUN59,FSTAND,UNFOR,UNKN)
      rewind(iun59)

      WRITE(iun59)ii59
      WRITE(IUN59)NOSVEC  
      WRITE(IUN59)KLINES
      DO I=1,KLINES
      WRITE(IUN59)TEXT(I)
      END DO
      write(iun59)ksfit,nosvec(22)
      CLOSE(iun59)
      RETURN

      END subroutine dfwr59






