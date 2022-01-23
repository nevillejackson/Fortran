!==========================================================================
      SUBROUTINE DFPRE1 (iopmod)
!==========================================================================

      use names
      use order
      use solve
      use means
      use levels
      use comments
      use units
      use numbers
      use platform

!     limits for interactive input
      integer, parameter   :: maxnfl=30000, maxre1=5, mxrnd1=100000,
     &                        maxrhs=99, maxcov=99, maxfix=99
      integer, intent(out) :: iopmod

      CHARACTER(len=12)                      :: blank12='            '
      integer, dimension(:), allocatable     :: knsert,kfix,kcov,
     &                                          iposfx
      integer, dimension(:), allocatable     :: nlrnd1
      integer, dimension(:,:), allocatable   :: nl
      integer, dimension(8)                  :: nnped

!     read info collected in dfprep
      call read_iun11

!     -----------------
!     INTERACTIVE INPUT
!     -----------------

      CALL DFCMMT

      WRITE(*,910)'TRAITS TO BE ANALYSED'
      WRITE(*,*)'NO. OF TRAITS (=RHS) ?     '
      CALL OPTDEF(NQ,1,maxrhs,MMQ)

      allocate(trait(nq),stat=ii)
      if(ii>0)stop 'alloc levs'

      DO I=1,NQ
      WRITE(*,fmt3(ipltf))' GIVE NAME (MAX. 12 CHAR.S) FOR TRAIT',I
      READ(*,'(a)')TRAIT(I)
      if(trait(i).eq.blank12)write(trait(i),'(''*trait'',i3,''*  '')')i
      end do

      WRITE(*,910)'FIXED PART OF THE MODEL'
      WRITE(*,*)'*Total* no. of covariables to be fitted ?     '
      CALL OPTDEF( NCOV,0,maxcov,KCOV(1) )
      if(ncov>0)then
         write(*,fmt(ipltf))'No. of covar.s nested in a fixed eff. ?'
         CALL OPTION( NCOV2,0,NCOV )
         allocate(npow(ncov),covar(ncov),icnest(ncov),nlnest(ncov),
     &            irropt(ncov),                            stat=ii)
         if(ii>0)stop 'alloc covar'
         irropt=1
      else 
         ncov2=0
      end if
      NCOV1=ncov-ncov2
      NFR1=0
      DO I=1,NCOV1
      WRITE(*,fmt3(ipltf))
     &          ' NAME (MAX 12 CHAR.S) FOR OVERALL COVARIABLE NO.',I
      READ(*,'(a)')COVAR(I)
      if(covar(i).eq.blank12)write(covar(i),'(''*covar'',i2,''*'')')i

      CALL DxPOPT(N,n2,COVAR(I))
      NPOW(I)=N
      irropt(i)=n2
      ICNEST(I)=0
      NLNEST(I)=1
      NFR1=NFR1+N
      end do
 
 600  NFR2=0
      DO I=NCOV1+1,NCOV
      WRITE(*,fmt3(ipltf))
     &    'NAME (MAX 12 CHAR) FOR *nested* COVARIABLE NO.',I
      READ(*,'(a)')COVAR(I)
      if(covar(i).eq.blank12)write(covar(i),'(''*covar'',i2,''*'')')i
      CALL DxPOPT(N,n2,COVAR(I))
      NPOW(I)=N
      irropt(i)=n2
      WRITE(*,fmt2(ipltf))COVAR(I),
     &                       ' TO BE NESTED WITHIN FIXED EFFECT NO. ?'
      CALL OPTION(II,1,kfix(1)+10)
      ICNEST(I)=II
      WRITE(*,*)'NO. OF LEVELS OF FIXED EFFECT ?'
      CALL OPTDEF(JJ,1,nl(ii,1)+10,NL(II,1))
      NLNEST(I)=JJ
      NFR2=NFR2+N*JJ
      END DO
      NFR=NFR1+NFR2
      LIM1=NQ+NFR
      if(nfr>0)write(*,*)
     &           'total no. of regression coefficients to be fitted',nfr

      WRITE(*,*)'  '
      WRITE(*,*)'NO. OF FIXED EFFECTS TO BE FITTED ?     '
      CALL OPTDEF(NFIX,0,maxfix,KFIX(1))
      allocate(fixed(nfix+6),nlev(nfix+6),nsize1(nfix+6),iposfx(nfix+6),
     *                                                        stat=ii)
      NRZERO=0
      IF(NFIX.GT.0)THEN
         K=LIM1
         DO  I=1,NFIX
         WRITE(*,fmt3(ipltf))
     &              'NAME (MAX. 12 CHAR.S) FOR FIXED EFFECT NO.',I
         READ(*,'(a)')FIXED(I)
         if(fixed(i).eq.blank12)write(fixed(i),'(''*fixed'',i2,''*'')')i
         WRITE(*,*)'NO. OF LEVELS FOR  ',FIXED(I),'  ?'
         CALL OPTDEF( N,0,MAXNFL,NL(I,1) )
         IPOSFX(I)=K
         K=K+N
         NLEV(I)=N
         end do
         LIM2=K
         NFL=K-LIM1
         allocate(krzero(nfl),diazer(nfl),rhszer(nfl,nq),stat=ii)
         if(ii>0)stop 'alloc krzero'
         if(nfix>1)CALL DFIZR1
      ELSE
         NFL=0
      END IF
      IF(NRZERO.GT.1)CALL DFKZER(NRZERO,KRZERO)
      do i=ncov1+1,ncov
      j=icnest(i)
      if(nlnest(i).ne.nlev(j))then
          write(*,*)covar(i),' nested within fixed effect',j
          write(*,*)'discrepancy in no. of levels for',fixed(j)
          write(*,*)'specified for covariable   :',nlnest(i)
          write(*,*)'specified for fixed effect :',nlev(j)
          write(*,*)'try again ...'
          go to 600
      end if
      end do

      WRITE(*,910)'RANDOM PART OF THE MODEL'
      KANIM=999999
      CALL DFANIM(NANIM,KANIM,IUN44)

      WRITE(*,*)'NO. OF ADDITIONAL RANDOM EFFECTS CODES =', krand1
      if(krand1>maxre1)stop 'Reset parameter "MAXRE1" '
      ioprn1=krand1
      LIM3=LIM2
      NFIX1=NFIX 
      NRAND1=0
      do ii=1,ioprn1
      nfix1=nfix1+1
      WRITE(*,fmt3(ipltf))
     &              'NAME (MAX 12 CHAR.S) FOR *add. RE* ? NO. =',ii
      READ(*,'(a)')FIXED(NFIX1)
      if(fixed(nfix1).eq.blank12)write(fixed(nfix1),'(a8,i2,a2)')
     &                                          '*add. RE',ii,'* '
      WRITE(*,*)'NO. OF LEVELS FOR  ',FIXED(NFIX1),' ?  (0 to omit) '
      CALL OPTDEF(NR1,0,MXRND1,NLrnd1(ii))

      NLEV(NFIX1)=NR1
      IPOSFX(NFIX1)=LIM3
      LIM3=LIM3+NR1
      NRAND1=NRAND1+NR1
      IF(II.EQ.1)THEN
!          WRITE(*,*)'USER-SPECIFIED COVARIANCE MATRIX FOR ',
!     *               FIXED(NFIX1),' ?  '
!          CALL YNDEF(JOPCOV,0)
!          IF(JOPCOV.EQ.1)WRITE(*,*)'USER *MUST* SUPPLY INVERSE OF ',
!     *                   'MATRIX; TO BE READ FROM UNIT "47" !'
      jopcov=0
      END IF
      END DO

!      WRITE(*,*)' '
!      WRITE(*,*)'IS THERE A SECOND RANDOM EFFECT FOR EACH ANIMAL ? '
      KK=0
      IF(KNSERT(1).NE.0)KK=1
!      CALL YNDEF(IOPRN2,KK)
      ioprn2=kk
      NFIX2=NFIX1+IOPRN2
      IOPCOV=0
      IF(IOPRN2.EQ.1)THEN
         WRITE(*,*)'Found code for 2nd random animal effect '
         WRITE(*,fmt(ipltf))
     &                'NAME FOR *2ND animal* EFFECT ? (MAX 12 CHARS)'
         READ(*,'(a)')FIXED(NFIX2 )
         if(fixed(nfix2).eq.blank12)write(fixed(nfix2),
     &                                             '("*2nd anim.*")')
         WRITE(*,*)'NO. OF LEVELS FOR  ',FIXED(NFIX2),'  ? (0 to omit)'
         CALL OPTDEF(NRAND2,0,nanim,NANIM)
         NLEV(NFIX2 )=NRAND2
         if(nrand2.ne.nanim.and.nrand2.ne.0)stop 'nrand2'
         if(nrand2>0)then
            WRITE(*,*)'COVARIANCE STRUCTURE FOR "SECOND ANIMAL" EFFECT'
            WRITE(*,9)'1  ...  PROPORTIONAL TO NUMERATOR RELATIONSHIP'
            WRITE(*,9)'        MATRIX, UNCORRELATED WITH DIRECT '
            WRITE(*,9)'        ADDITIV GENETIC EFFECTS'
            if(nainv.eq.1)then
               WRITE(*,9)'2  ...  AS (1) BUT CORRELATED TO DIRECT'
               WRITE(*,9)'        ADDITIV-GENETIC EFFECTS'
            end if
            WRITE(*,9)'3  ...  PROPORTIONAL TO IDENTITY MATRIX,'
            WRITE(*,9)'        UNCORRELATED TO ADDIT.-GENETIC EFFECTS'
            WRITE(*,9)'4  ...  PROPORTIONAL TO USER-SPECIFIED MATRIX,'
            WRITE(*,9)'        UNCORRELATED TO ADDIT.-GENETIC EFFECTS'
            CALL OPTdef(IOPCOV,1,4,1)
         END if
      ELSE
         NRAND2=0
      END IF

!     DETERMINE MODEL TYPE
      NEQNS=NQ+NFR+NFL+NRAND1+NRAND2+NANIM
      IF(IOPRN2.EQ.0)THEN
         IMODEL=1+min0(IOPRN1,1)
      ELSE 
         IMODEL=2+IOPCOV+min0(IOPRN1,1)*4
      END IF
      WRITE(*,*)' '
      WRITE(*,9)'NO. OF MODEL TO BE FITTED =',IMODEL
      WRITE(*,*)' '

C     READ PHENOTYPIC MEANS (from DF23#DAT)
      allocate(ybar(nq),cbar(ncov),ysdev(nq),csdev(ncov),ycv(nq),
     &     ccv(ncov),ymin(nq),cmin(ncov),ymax(nq),cmax(ncov),stat=ii)
      if(ii>0)stop 'alloc means'
      READ(IUN23)cbar,ybar
      READ(IUN23)csdev,ysdev
      READ(IUN23)ccv,ycv
      READ(IUN23)cmin,ymin
      READ(IUN23)cmax,ymax

!     process data
      NFILL=0
      IORDER=0
      allocate (ieqnew(neqns),stat=ii)
      if(ii>0)stop 'alloc ieqnew'
      ieqnew=(/ (i,i=1,neqns) /)

      CALL DFMME1(IPOSFX)

!     carry out lsq analysis for fixed effects
      call dflsq1

      RETURN
9     FORMAT(8X,A,I10)
910   FORMAT(/40(1H*)/10X,A/40(1H*)/)

      contains

C     =================
      SUBROUTINE DFCMMT
C     =================

      klines=0
      WRITE(*,'(/40(1H*)/10X,A/40(1H*)/)')'DESCRIPTION FOR DATA SET '
      WRITE(*,*)'TYPE COMMENTS - max. 6 lines & 80 characters per line'
      write(*,fmt(ipltf))'terminate with "*" in col. 1 or blank line !'
      DO  I=1,6
      READ(*,'(a)')ttext
      IF(ttext(1:1).EQ.'*')GO TO 20
      do k=1,80
      if(ttext(k:k).ne.' ')go to 10
      end do
      go to 20
 10   klines=klines+1
      TEXT(klines)=ttext
      end do
 20   WRITE(*,*)'NO. OF COMMENT LINES READ =',KLINES
      RETURN
      END subroutine dfcmmt

C     =================
      SUBROUTINE DFIZR1
C     =================

C     SET 1ST LEVEL OF EACH ADD. FIXED EFFECT TO ZERO
      DO I=2,NFIX
      NRZERO=NRZERO+1
      KRZERO(NRZERO)=IPOSFX(I)+1
      end do

      WRITE(*,111)
      CALL OPTDEF(N0ADD,0,NFL,0)
      II=1
      DO I=1,N0ADD
      WRITE(*,*)'FOR ADDITIONAL DEPENDENCY NO.',I
      WRITE(*,fmt(ipltf))'  ... RUNNING NO. OF FIXED EFFECT ?      '
      IF(NFIX.GT.1)CALL OPTION(II,1,NFIX)
      WRITE(*,fmt4(ipltf))'  ... LEVEL NO. OF   ',FIXED(II),'   ?'
      CALL OPTION(JJ,1,NLEV(II) )
      NRZERO=NRZERO+1
      if(nrzero>nfl)stop 'err dfizr1'
      KRZERO(NRZERO)=IPOSFX(II)+JJ
      end do
      RETURN
111   FORMAT(
     *   /1X,'COEFFICIENCT MATRIX FOR FIXED EFFECTS NOT OF FULL RANK'
     *   /1X,' --> PROGRAM SETS *FIRST* LEVEL OF EACH FIXED EFFECT  '
     *   /1X,'     TO ZERO (EXCEPT 1ST FE)                          '
     *   /1X,' --> SPECIFY ANY ADDITIONAL KNOWN DEPENDENCIES  :     '
     *   /1X,'     (E.G. DUE TO HIERARCHICAL STRUCTURE OF FE)       '/
     *   /1X,'NO. OF ADDITIONAL DEPENDENCIES ? (GIVE 0 IF NONE)     ') 
      END subroutine dfizr1

!     =====================
      subroutine read_iun11
!     =====================

      rewind(iun11)
      read(iun11)fped,fdata,cwdir
      read(iun11)nnped
      read(iun11)iopmod,llq,mmq
      if(iopmod.ne.1)stop 'mismatch with "dfprep" !'
 
      allocate(kcov(llq),kfix(llq),knsert(llq),stat=ii)
      if(ii>0)stop 'alloc kvecs'
      READ(IUN11)KCOV
      READ(IUN11)KFIX(:llq)
      read(iun11)(ii, i=1,llq)
      read(iun11)(ii, i=1,llq)
      READ(IUN11)KNSERT

      m=maxval(kfix)
      allocate(nl(m,llq),stat=ii)
      if(ii>0)stop 'alloc nl'
      nl=0
      read(iun11)((nl(l,k),l=1,kfix(k)), k=1,llq)
      READ(IUN11)((( ii ,M=1,NL(L,K)), L=1,KFIX(K)), K=1,LLQ)
      READ(IUN11)((( ii ,M=1,NL(L,K)), L=1,KFIX(K)), K=1,LLQ)
      read(iun11)krand1
      IF(krand1>0)THEN
         allocate(nlrnd1(krand1),stat=ii)
         if(ii>0)stop 'read_iun11 : alloc 2'
         read(iun11)nlrnd1
         read(iun11)((ii ,m=1,nlrnd1(l)),l=1,krand1)
      END IF

      READ(IUN11)KANIM,ii,IOPIBR,nainv,detll,nna(:nainv)
      return
      end subroutine read_iun11

      END subroutine dfpre1

!=============================================================================
      SUBROUTINE DxPOPT(N,n2,COVAR)
!=============================================================================

      USE platform

      INTEGER, INTENT(OUT)          :: n,n2
      CHARACTER(LEN=12), INTENT(IN) :: COVAR

      n2=1
      WRITE(*,*)'NO. OF REGRESSION COEFFICIENTS FOR  ',COVAR,' ?'
      WRITE(*,FMT9(1))'1   ...  LINEAR'
      WRITE(*,FMT9(1))'2   ...  LINEAR & QUADRATIC'
      WRITE(*,FMT9(1))'3   ...  LINEAR & QUADRATIC & CUBIC'
      WRITE(*,fmt9(1))'     .          ....                          '
      WRITE(*,FMT9(ipltf))'N   ...  LINEAR TO POWER N'
      CALL OPTION(N,-19,19)
      if(n<0)then
         n=-n
         write(*,*)'user supplied form of regression assumed ! '
         write(*,*)'this only work for integer valued covariables !!'
         write(*,*)'you must supply a file "DF21#DAT" with : '
         write(*,*)'-> one *row* for each value of covariable ',covar
         write(*,*)'-> each row containing '
         write(*,*)'   * the integer value of the covariable '
         write(*,*)'   * the N evaluated coefficients of the regression'
         n2=2
      end if
      RETURN
      END subroutine dxpopt
