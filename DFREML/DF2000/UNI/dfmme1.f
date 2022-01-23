C==========================================================================
      SUBROUTINE  DFMME1 (iposfx)
C==========================================================================

      use sparse
      use order
      use means
      use levels
      use constants
      use units
      use numbers

      integer, dimension(nfix1), intent(in)    :: IPOSFX
      real(8), dimension(:), allocatable       :: yy,xvec
      integer, dimension(:), allocatable       :: ivec,ifix,iposcv
      real(8)                                  :: c,cc
      integer                                  :: neff,i,j,k,jj,kabs,iq

      mfix2=count(nlev(:nfix2)>0)
      neff=nq+nfr+nfl+nfix2+1
      allocate (yy(nq+ncov),xvec(neff),ivec(neff),
     &          ifix(0:nfix2),iposcv(ncov),stat=ii)
      if(ii>0)stop 'alloc dfmme1'

C     INITIALIZE
      NREC=0
      lim1a=nq+nfr1
      lim1b=lim1a
      lim1c=lim1a
      do i=ncov1+1,ncov
      iposcv(i)=lim1c
      lim1b=lim1b+npow(i)
      lim1c=lim1c+npow(i)*nlnest(i)
      end do
      IVEC(:lim1a)=(/ (i,i=1,lim1a) /)
      NABS=LIM1b+MFIX2+1

50    READ(IUN22,END=99)iq,IFIX,YY
      CALL KOUNT(NREC,5000)

C     CHECK CODES
      IF(NFILL.EQ.0)THEN
         DO I=1,NFIX2
         if(nlev(i).eq.0)cycle  ! code on record but effect not fitted
         if(i>nfix.and.i.le.nfix1.and.ifix(i).eq.0)cycle ! allow "0" add RE 
         IF(IFIX(I).LT.1.OR.IFIX(I).GT.NLEV(I))THEN
            WRITE(*,*)'CODE FOR EFFECT NO.',I,'  OUT OF RANGE !'
            WRITE(*,*)'RECORD NO.     =',NREC
            WRITE(*,*)'CODE FOUND     =',IFIX(I)
            WRITE(*,*)'MAX. SPECIFIED =',NLEV(I)
            STOP
         END IF
         end do
         if(ifix(0)<1.or.ifix(0)>nanim)then
            write(*,*)'animal code out of range found',ifix(0),nanim
            stop
         end if
      END IF

c     position of nested covariables
      k=lim1a
      do i=ncov1+1,ncov
      do j=1,npow(i)
      k=k+1
      ivec(k)=iposcv(i)+(ifix(icnest(i))-1)*npow(i)+j
      end do
      end do

C     RECODE EFFECTS
      where(nlev(1:nfix1)>0.and.ifix(1:nfix1)>0)IFIX(1:nfix1)=
     &                               IPOSFX(1:nfix1)+IFIX(1:nfix1) 
      IF(nfix2>nfix1.and.nlev(nfix2)>0)THEN
         Ifix(0)=LIM3+Ifix(0)*2-1     ! animal
         IFIX(NFIX2)=LIM3+IFIX(NFIX2)*2 ! 2nd anim
      ELSE
         Ifix(0)=LIM3+Ifix(0)   ! animal only
      END IF

C     CHANGE EQUATION NO.S
      IF(NFILL.GT.0)THEN
         IFIX(0)=IEQNEW( IFIX(0) )
         DO  I=1,NFIX2
         if(nlev(i)>0)IFIX(i)=IEQNEW( IFIX(I) )
         end do
!         ivec(lim1a+1:lim1b)=ieqnew( (/ (ivec(l),l=lim1a+1,lim1b) /) )
         do i=lim1a+1,lim1b
         ivec(i)=ieqnew(ivec(i))
         end do
      END IF

C     TRAITS
      XVEC(:nq)=YY(1+NCOV:nq+ncov)-YBAR(:nq)

C     COVARIABLES
      K=NQ
      DO I=1,NCOV
      N=NPOW(I)
      C=yy(I)-CBAR(I)
      K=K+1
      XVEC(K)=C
      IF(N.GT.1)THEN
         CC=C
         DO J=2,N
         CC=CC*C
         K=K+1
         XVEC(K)=CC
         end do
      END IF
      end do

      IF(IORDER.LT.3)THEN
C        ARRANGE FIXED & RANDOM EFECTS CODES IN ASCENDING ORDER
         ivec(lim1b+1:nabs)=0
         KABS=LIM1b+1
         IVEC(KABS)=ifix(0)
         DO 54 I=1,NFIX2
         if(nlev(i).eq.0)go to 54
         JJ=IFIX(I)
         if(i>nfix.and.i<=nfix1.and.jj==0)go to 54
         DO L=LIM1b,KABS
         IF(JJ.LT.IVEC(L))THEN
            KABS=KABS+1
            if(kabs>neff)stop 'err dfmme1 kabs'
            DO M=KABS,L+1,-1
            IVEC(M)=IVEC(M-1)
            end do
            IVEC(L)=JJ
            GO TO 54
         ELSE IF(JJ.EQ.IVEC(L))THEN
            PRINT *,'ERROR : DFMME1'
            PRINT *,'NREC',NREC
            PRINT *,'I=',I,' JJ=',JJ
            PRINT *,'K,L',KABS,L
            PRINT *,'IVEC',(IVEC(LL),LL=1,KABS)
            PRINT *,'IFIX',(IFIX(LL),LL=0,NFIX2)
            STOP
         END IF
         end do
         KABS=KABS+1
         if(kabs>neff)stop 'err 2 dfmme1 kabs'
         IVEC(KABS)=JJ
54       CONTINUE
         WRITE(IUN52)IVEC(LIM1a+1:nABS),XVEC(:LIM1b)

      ELSE

         KABS=LIM1b+1
         IVEC(KABS)=ifix(0)
         DO  I=1,NFIX2
         if(nlev(i).eq.0)cycle                 ! effect not fitted
         JJ=IFIX(I)
         IF(JJ.EQ.0)cycle                      ! ignore codes of "0" 
         KABS=KABS+1
         if(kabs>neff)stop 'err 3 dfmme1 kabs'
         IVEC(KABS)=JJ
         end do
         WRITE(IUN52)KABS,IVEC(LIM1a+1:KABS),XVEC(:LIM1b)
      END IF
      GO TO 50

C----------------------------------------------------------------------------
C      ALL DATA PROCESSED
C--------------------------------------------------------------------------

99    WRITE(*,*)'"DFMME1" : NO. OF RECORDS PROCESSED    =',NREC
      WRITE(*,*)'           TOTAL NO. OF EQUATIONS      =',NEQNS

      RETURN
      END subroutine dfmme1








