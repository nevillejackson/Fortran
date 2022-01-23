!=======================
      PROGRAM read_df59
!=======================

!    Auxiliary program to DFREML -> read & analyse file DF59#DAT

      character(len=25)                      :: fstand='DF59#DAT',
     &                                          old='old',
     &                                          unfor='unformatted'
      logical                                :: lexist
      real(8), dimension (:), allocatable    :: wvec,xvec
      real(8)                                :: ffv, fmin=1.d37, xx
      integer                                :: ii59=-1,n=-1,kparm=0,
     &                                          j,i,llines,ii, nlik
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

 100  INQUIRE(FILE=FSTAND,EXIST=LEXIST)
      if(lexist)then
          open(59,file=fstand,status='old',form='unformatted')
      else
          write(*,*)'file  "',trim(fstand),'"  does not exist !'
          write(*,*)' -> give alternative file name  '
          read(*,'(a)')fstand
          go to 100
      end if

!     read lines written before points on likelihood surface
 200  READ(59,iostat=ii)II59
      if(ii.ne.0)stop 'end of file/ error'
      IF(II59.EQ.0)THEN
         READ(59)KOSVEC
         READ(59)LLINES
         IF(LLINES.GT.0)THEN
            DO I=1,LLINES
            READ(59)TTEXT
            write(*,*)ttext
            end do
         END IF
      END IF
      read(59,iostat=ii)ii ! ksfit
      if(ii.ne.0)stop 'end of file/ error'

!     find out how many parameters there are
      if(kparm.eq.0)then
         ii=0
         do while (ii.eq.0)
         kparm=kparm+1
         read(59,iostat=ii)n,ffv,(xx,l=1,kparm)
         if(dabs(xx)<1.d-12.or.dabs(xx)>1.d20)then
            kparm=kparm-1
            exit
         end if
         end do
         write(*,*)'no. of parameters =',kparm
         rewind(59)
         ii59=-1
         go to 200
      end if

!     read previously evaluated points
      allocate(wvec(kparm),xvec(kparm), stat=ii)
      if(ii>0)stop 'alloc wvec'
      ii=0
      nlik=0
      do while( ii.eq.0)
         READ(59,iostat=ii)N,ffv,WVEC(:kparm)
         nlik=nlik+1
         IF(ffv.LT.FMIN)THEN
            mlik=nlik
            FMIN=ffv
            XVEC(:kparm)=WVEC
         END IF
      end do
      write(*,*)'no. of likelihoods read =',nlik
      if(fmin<1.d37)then
         write(*,'(a,f20.10)')' "Best" point found - log L =',
     &                         -0.5d0*fmin
         write(*,*)'running no.',mlik
         write(*,'((5g15.6))')xvec(:kparm)
      end if

      END PROGRAM read_df59




