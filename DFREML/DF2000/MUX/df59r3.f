C==========================================================================
      SUBROUTINE DFRC59 (kparm,kq,mm,fmin,xvec)
C==========================================================================

      use params
      use comments
      use units
      use likelihoods
      use numbers

!     arguments
      integer, intent(in)                    :: kparm,kq,mm
      real(8), intent(inout)                 :: fmin
      real(8), dimension(mxparm),intent(out) :: xvec

!     local variables
      LOGICAL                                :: LEXIST
      integer, dimension (maxnos)            :: kosvec=0
      integer, dimension (6)                 :: kkfit=0
      real(8), dimension (:), allocatable    :: wvec
      real(8)                                :: ffv
      integer                                :: ii59,n,j,i,llines!,jj
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!     RECOVER ANY LIKELIHOOD EVALUATIONS FROM UNFINISHED RUNS
      open(iun59,file='DF59#DAT',status='unknown',form='unformatted')

!     CHECK THAT WE'VE GOT correct FILE FOR THIS DATA SET & ANALYSIS !
      INQUIRE(FILE='DF59#DAT',EXIST=LEXIST)
790   IF(LEXIST)THEN
         READ(IUN59,END=789)II59
         N=0
      else
         REWIND(iun59)
         ii59=-1
         WRITE(IUN59)II59
         write(iun59)kfit,nosvec(22)
         CLOSE(iun59)
         return
      END if

      IF(II59.EQ.0)THEN
         call chk_nosvec
         call chk_comments
      END IF

      read(iun59,end=789,err=789)kkfit!,jj
      if(kkfit(1).ne.-99)then
         do i=1,5
         if(i.eq.3)cycle
         if(kfit(i).ne.kkfit(i))then
            WRITE(*,*)'WRONG FILE "59" ??!!!'
            WRITE(*,*)'DISCREPANCY FOUND vector "kfit" !'
            do j=1,6
            write(*,*)j,kfit(j),kkfit(j)
            end do
!           stop 'remove old file "DF59#DAT" '
         end if
         end do
      end if

c     read previously evaluated points
      allocate(wvec(kparm),stat=ii)
      if(ii>0)stop 'DF59R3 : alloc wvec'

777   READ(IUN59,END=799,ERR=779)N,ffv,WVEC
      IF(N.GT.NCALL)THEN
         NCALL=NCALL+1
         IF(NCALL.LE.MXFUNC)THEN
            FEVAL(2:kparm+1,NCALL)=WVEC
            FEVAL(1,NCALL)=ffv
         END IF
         IF(ffv.LT.FMIN)THEN
            FMIN=ffv
            XVEC(:kparm)=WVEC
         END IF
      END IF
      GO TO 777

 799  deallocate(wvec,stat=ii)
      if(ii>0)stop 'DF59R3 : dealloc wvec'

      if(fmin<big)then
         WRITE(*,*)' '
         write(*,'(a,f20.10)')
     &       ' Point recovered from "DF59#DAT" - log L =',-0.5d0*fmin
         write(*,'((5g15.6))')xvec(:kparm)
      end if
      close(iun59)
      RETURN

789   lexist=.false.
      GO TO 790
 779  write(*,*)'routine "dfrc59" : read error when picking up ',
     *                           'previously evaluated  points!'
      write(*,*)'** likely reason is discrepancy in no. of parameters'
      write(*,*)'** between this and a previous run'
      write(*,*)'   no. of values expected for current run =',kparm+kq
      stop

      contains

!     =====================
      subroutine chk_nosvec
!     =====================

      READ(IUN59,end=99,err=99)KOSVEC
      DO I=1,MM
      if(i.eq.2.or.i.eq.10.or.i.eq.22.or.i.eq.16.or.i.eq.17)cycle
      IF(KOSVEC(I).NE.NOSVEC(I))THEN
         WRITE(*,*)'WRONG FILE "59" ??!!!'
         WRITE(*,*)'DISCREPANCY FOUND IN VECTOR "NOSVEC" :'
         DO J=1,MAXNOS
         WRITE(*,*)J,NOSVEC(J),KOSVEC(J)
         END DO
!         STOP 'STRAIGHTEN OUT FILES & MODELS !'
      END IF
      END do
 99   return
      end subroutine chk_nosvec

!     =======================
      subroutine chk_comments
!     =======================

      READ(IUN59,end=99,err=99)LLINES
      IF(LLINES.NE.KLINES)THEN
          WRITE(*,*)'WRONG FILE "59" ??!!!'
          WRITE(*,*)'DISCREPANCY FOUND IN NO. OF COMMENT LINES'
!          STOP 'STRAIGHTEN OUT FILES & MODELS !'
      END IF

      IF(KLINES.GT.0)THEN
         DO I=1,KLINES
         READ(IUN59)TTEXT
         IF(TTEXT.NE.TEXT(I))THEN
            WRITE(*,*)'WRONG FILE "59" ??!!!'
            WRITE(*,*)'DISCREPANCY FOUND IN COMMENT LINE',I
            WRITE(*,*)'FOUND :  ',TTEXT
            WRITE(*,*)'EXPECT : ',TEXT(I)
!            STOP 'STRAIGHTEN OUT FILES & MODELS !'
         END IF
         end do
      END IF
 99   return
      end subroutine chk_comments

      END subroutine dfrc59

!==========================================================================
      subroutine DFWR59
!==========================================================================

      use comments
      use units
      use numbers

      integer                                :: ii59=0,i

      open(iun59,file='DF59#DAT',status='unknown',form='unformatted')
      WRITE(IUN59)II59
      WRITE(IUN59)NOSVEC  
      WRITE(IUN59)KLINES
      DO I=1,KLINES
      WRITE(IUN59)TEXT(I)
      END DO
      write(iun59)kfit,nosvec(22)
      CLOSE(IUN59)
      RETURN
      END subroutine dfwr59
















