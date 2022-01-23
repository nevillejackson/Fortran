!=============================================================================
      SUBROUTINE DFPRE3 
!=============================================================================

      use params
      use names
      use comments
      use units
      use ages
      use means
      use combinations
      use levels
      use numbers
      use order
      use zero_rows
      use mme3
      use platform

      CHARACTER(len=12)                 :: fname,blank12='            '
      integer, dimension (8)              :: nnped
      integer, dimension (maxnq)          :: kint,kfix,kfix1,knsert,kcov
      integer, dimension (maxfix+2,maxnq) :: nl
      integer, dimension (maxfix,maxnq)   :: iposfx
      real(8), dimension (:), allocatable :: yy
      integer                             :: maxobs=29
      integer                             :: maxmq=11
      integer, dimension(:), allocatable  :: nlrnd1
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C     OUTPUT FROM PROGRAM "DFPREP"
      call read_iun11 (iopmod,llq,iequal)

C     -----------------
C     INTERACTIVE INPUT
C     -----------------

      CALL DFCMMT   ! read comments

      WRITE(*,910)'TRAITS TO BE ANALYSED'
      WRITE(*,*)'NO. OF TRAITS IN MULTIVARIATE ANALYSIS ?'
      CALL OPTDEF(NQ,1,MAXNQ,LLQ)
      if(nq.eq.1)then
         write(*,*)'Univariate analysis has been chosen !'
         if(iopmod.ne.1.and.llq.ne.1)then
            write(*,fmt(ipltf))'Trait to be analysed ? (no.)'
            call option(nquni,1,llq)
         else
            nquni=1
         end if
      else
         nquni=0
      end if

      NQQ=NQ*(NQ+1)/2

      call all_levels_1(nq)
      call all_levels_2(nq,maxcov,maxfix)
      call all_numbers(nq,nqq)
      call all_names (nq,mxparm,maxfix,maxcov)
      call all_combis(nq,mxcomb)
      call all_mme3(nq)

      LIM0=1
      if(iopmod.le.3)then
         DO  I=1,NQ
         WRITE(*,fmt3(ipltf))'GIVE NAME (MAX. 12 CHAR.S) FOR TRAIT',I
         READ(*,'(a)')TRAIT(I)
         if(trait(i).eq.blank12)write(trait(i),'(''*trait'',i3,
     &                                                     ''*  '')')i
         end do
      else
         DO I=1,NQ
         write(TRAIT(I),' (''age ='',i5,2x)' )iiage(i)
         end do
      end if

      WRITE(*,*)' '
      WRITE(*,*)'ARE THERE REPEATED RECORDS PER ANIMAL ?'
      CALL YNDEF(IOP,0)
      IF(IOP.EQ.1)THEN
         DO I=1,NQ
         WRITE(*,fmt4(ipltf))' MAX. NO. OF RECORDS FOR ',TRAIT(I),' ?'
         if(i.eq.1)then
            CALL OPTION(MXOBS(I),1,MAXOBS)
         else
            CALL OPTDEF(MXOBS(I),1,MAXOBS,mxobs(i-1))
         end if
         END DO

         if(iopmod.le.3)then
            WRITE(*,*)' '
            WRITE(*,*)'OPTION : ACCOUNT FOR REPEATED RECORDS BY        '
            WRITE(*,9)'1  ...  FITTING A PERMANENT ENVIRONMENTAL EFFECT'
            WRITE(*,9)'        AS RANDOM EFFECT EXPLICITLY             '
            WRITE(*,9)'2  ...  FITTING AN EQUIVALENT MODEL WITH        '
            WRITE(*,fmt(ipltf))'               CORRELATED RESIDUALS    '
            CALL OPTION(IRPMOD,1,2)
         else
            irpmod=1
         end if
      ELSE
         mxobs=1 ! array op
         IRPMOD=0
      END IF
      MOBS=SUM(MXOBS)
      nxobs=0

      do iq=1,nq
      if(iopmod.eq.3.or.iq.eq.1)then
         WRITE(*,*)'... NO. OF RIGHT HAND SIDES PER RECORD ?',iq
         CALL OPTDEF(MQ,1,MAXMQ,1)
         IF(MQ.EQ.1)THEN
            jj=1
         ELSE 
            WRITE(*,*)' '
            WRITE(*,fmt(ipltf))'... RUNNING NO. OF RHS TO BE PICKED ?'
            CALL OPTION(JJ,1,MQ)
         END IF
      end if
      ITRAIT(IQ)=JJ
      NR8(IQ)=MQ
      END DO

      WRITE(*,910)'FIXED PART OF THE MODEL'
      NFRM=0
      NFRM=0
      DO 1001 IQ=1,NQ
      IF(IQ.EQ.1.OR.IOPMOD.eq.3)THEN
         IF(IOPMOD.EQ.3)THEN
            WRITE(*,*)'NO. OF COVARIABLES TO BE FITTED FOR ',TRAIT(IQ),
     *                                                             ' ?'
         ELSE
            WRITE(*,*)'NO. OF COVARIABLES TO BE FITTED  ?'
         END if
         CALL OPTDEF(NNCOV,0,MAXCOV,KCOV(IQ))
         NCOV(IQ)=NNCOV
         NR8(IQ)=NR8(IQ)+NNCOV
         NNFR=0
         IF(NNCOV.GT.0)THEN
            CALL CHKLEV(NNCOV,MAXCOV,'MAXCOV','COVARIABLES',11)
            ITRAIT(IQ)=ITRAIT(IQ)+NNCOV
            DO I=1,NNCOV
            IF(iopmod.eq.3)then
              WRITE(*,'(a,a,i3)')' GIVE NAME (MAX. 12 CHAR.S) FOR',
     &                                        ' COVARIABLE NO.',I
              WRITE(*,fmt2(ipltf))'FOR TRAIT  ',TRAIT(IQ)
            else
              WRITE(*,fmt3(ipltf))
     &           'GIVE NAME (MAX. 12 CHAR.S) FOR COVARIABLE NO.',I
            END if
            READ(*,'(a)')COVAR(I,IQ)
            if(covar(i,iq).eq.blank12)write(covar(i,iq),
     &                   '(''*covar'',i2,''-'',i2,''*'')')i,iq
            WRITE(*,*)'NO. OF REGRESSION COEFFICIENTS FOR  ',COVAR(i,iq)
     &,                                                             ' ?'
            WRITE(*,fmt9(1))'1   ...  LINEAR'
            WRITE(*,fmt9(1))'2   ...  LINEAR & QUADRATIC'
            WRITE(*,fmt9(1))'3   ...  LINEAR & QUADRATIC & CUBIC'
            WRITE(*,fmt9(1))'     .          ....               '
            if(iq.eq.1.or.iequal.eq.0)then
               WRITE(*,fmt9(ipltf))'N   ...  LINEAR TO POWER N'
               CALL OPTION(N,1,19)
            else
               WRITE(*,fmt9(1))'N   ...  LINEAR TO POWER N'
               call optdef(n,1,19,npow(i,iq-1))
            end if
            NPOW(I,IQ)=N
            NNFR=NNFR+N
            end do
         END IF
         NFR(IQ)=NNFR
      ELSE
         NCOV(IQ)=Ncov(1)
         NR8(IQ)=NR8(IQ)+Ncov(iq)
         ITRAIT(IQ)=ITRAIT(1)
         COVAR(:ncov(iq),IQ)=COVAR(I,1)
         NPOW(:ncov(iq),IQ)=NPOW(:ncov(1),1)
         NFR(IQ)=NFR(1)
      END IF
      NFRST(IQ)=NFRM+LIM0
 1001 NFRM=NFRM+NNFR
      LIM1=LIM0+NFRM

      WRITE(*,*)'  '
      nfix=0
      NRZERO=0
      NFLM=0
      LIM2=LIM1
      DO  IQ=1,NQ
      IF(IQ.EQ.1.OR.IOPMOD.EQ.3)THEN
         IF(IOPMOD.EQ.3)THEN
            WRITE(*,*)'NO. OF FIXED EFFECTS TO BE FITTED FOR ',
     *                                         TRAIT(IQ),'  ?'
         ELSE
            WRITE(*,*)'NO. OF FIXED EFFECTS TO BE FITTED ?'
         END IF
         CALL OPTDEF(NNFIX,0,MAXFIX,KFIX(IQ))
         NFIX(IQ)=NNFIX
         NNFL=0

         IF(NNFIX.GT.0)THEN
            CALL CHKLEV(NNFIX,MAXFIX,'MAXFIX','FIXED EFFECTS',13)
            DO 2 I=1,NNFIX
            IPOSFX(I,IQ)=LIM2+NNFL
            IF(IOPMOD.EQ.3)then
               WRITE(*,'(a,a,i3)')' GIVE NAME (MAX 12 CHAR.S) FOR FIXED'
     &,                           ' EFFECT NO.',I
               WRITE(*,fmt2(ipltf))' FOR TRAIT :  ',TRAIT(IQ)
            else
               WRITE(*,fmt2(ipltf))
     &               'GIVE NAME (MAX 12 CHARS) FOR FIXED EFFECT NO.',I
            END if
            READ(*,'(a)')FIXED(I,IQ)
            if(fixed(i,iq).eq.blank12)write(fixed(i,iq),
     &                           '(''*fixed'',i2,''-'',i2,''*'')')i,iq
            IF(IOPMOD.le.3)THEN
               WRITE(*,*)'NO. OF LEVELS FOR  ',FIXED(I,IQ),'  ?'
            ELSE
               WRITE(*,*)'NO. OF LEVELS FOR  ',FIXED(I,IQ),' FOR AGE ',
     *                                                 TRAIT(IQ),'  ?'
            END IF
            CALL OPTDEF( N,1,99999,NL(I,IQ) )
            NNFL=NNFL+N
2           NLEV(I,IQ)=N
            NFL(IQ)=NNFL
            LIM2=LIM2+NNFL
            if(iq.eq.1)then
               allocate(krzero(nq*nnfl),stat=ii)
               if(ii>0)stop 'alloc krzero'
            end if
            CALL DFIZR3(IQ,NRZERO,NNFL,NNFIX)
         END IF
      else
         NFIX(IQ)=NFIX(1)
         NNFL=0
         DO I=1,NFIX(iq)
         IPOSFX(I,IQ)=LIM2+NNFL
         fixed(i,iq)=fixed(i,1)
         WRITE(*,*)'NO. OF LEVELS FOR  ',FIXED(I,IQ),' FOR AGE ',
     *                                            TRAIT(IQ),'  ?'
         CALL OPTDEF( N,1,99999,NL(I,IQ) )
         NNFL=NNFL+N
         NLEV(I,IQ)=N
         end do
         NFL(IQ)=NNFL
         LIM2=LIM2+NNFL
         CALL DFIZR3(IQ,NRZERO,NNFL,NNFIX)
      end if
      NFLM=NFLM+NNFL
      end do ! loop iq
      IF(NRZERO.GT.1)CALL DFKZER(NRZERO,KRZERO)

      mcov=maxval(ncov)
      mfix=maxval(nfix)
      mnfr=maxval(nfr)
      if(mnfr.eq.0)mnfr=1

      WRITE(*,910)'RANDOM PART OF THE MODEL'
      KANIM=444444
      CALL DFANIM(NANIM,KANIM,IUN44)

      NFIX1=NFIX
      NRAND1=0
      NQ111=0
      lim3=lim2
      kk1=maxval(kfix1(:nq))
      kk2=minval(kfix1(:nq))
      WRITE(*,*)'FIT AN ADDITIONAL (UNCORRELATED) RANDOM EFFECT ?'
      CALL YNDEF(IOPRN1,min0(1,kk1))
      IF(ioprn1.eq.1)then
         WRITE(*,fmt(ipltf))'NAME FOR THIS EFFECT ? (MAX. 12 CHAR.S) '
         READ(*,'(a)')FNAME
         if(fname.eq.blank12)write( fname,'(''*Add. RE *  '')')
         WRITE(*,*)'NO. OF LEVELS FOR  ',FNAME,'  ? '
         CALL OPTDEF(Nrand1,1,99999, NLRND1(1) )
         ii=1
         if(iopmod.eq.3.and.kk1.eq.1.and.nquni<1)then
            WRITE(*,*)'IS ',FNAME,' FITTED FOR ALL TRAITS ?'
            if(kk1.eq.kk2)then
               CALL YNDEF(II,1)
            else 
               CALL YNDEF(II,0)
            end if
         end if
         JJ=1
         DO IQ=1,NQ
         IF(II.NE.1)THEN
            WRITE(*,*)'FIT ',FNAME,'  FOR  TRAIT NO.',IQ,'  ?'
            KK=Min0( KINT(IQ)-NFIX1(IQ),1)
            CALL YNDEF(JJ,KK)
         END IF
         IF(JJ.EQ.1)THEN
            NFIX1(IQ)=NFIX1(IQ)+JJ
            FIXED(NFIX1(iq),IQ)=FNAME
            NLEV(NFIX1(iq),IQ)=Nrand1
            NQ111=NQ111+1
         END IF
         end do  !iq
         LIM3=LIM3+NQ111*Nrand1
      END if
      lim3a=lim3
      nq333=0
      nrand3=0
      nfix3=nfix1
      if(ioprn1.eq.1.and.kk1>1)then
         WRITE(*,*)'FIT A 2nd ADDITIONAL (UNCORRELATED) RANDOM EFFECT ?'
         CALL YNDEF(IOPRN3,1)
         WRITE(*,fmt(ipltf))'NAME FOR THIS EFFECT ? (MAX. 12 CHAR.S) '
         READ(*,'(a)')FNAME
         if(fname.eq.blank12)write( fname,'(''*Add. RE * '')')
         WRITE(*,*)'NO. OF LEVELS FOR  ',FNAME,'  ?        '
         CALL OPTDEF(Nrand3,1,99999, NLRND1(2) )
         ii=1
         if(iopmod.eq.3.and.nquni<1)then
            WRITE(*,*)'IS ',FNAME,' FITTED FOR ALL TRAITS ?'
            if(kk1.eq.kk2)then
               CALL YNDEF(II,1)
            else 
               CALL YNDEF(II,0)
            end if
         end if
         JJ=1
         DO IQ=1,NQ
         IF(II.NE.1)THEN
            WRITE(*,*)'FIT ',FNAME,'  FOR  TRAIT NO.',IQ,'  ?'
            KK=Min0( KINT(IQ)-NFIX1(IQ),1)
            CALL YNDEF(JJ,KK)
         END IF
         IF(JJ.EQ.1)THEN
            NFIX3(IQ)=NFIX3(IQ)+JJ
            FIXED(NFIX3(iq),IQ)=FNAME
            NLEV(NFIX3(iq),IQ)=Nrand3
            NQ333=NQ333+1
         END IF
         end do  !iq
         LIM3=LIM3+NQ333*NRAND3
      END IF

      NQ222=0
      NFIX2=NFIX3
      WRITE(*,*)' '
      WRITE(*,*)'IS THERE A SECOND RANDOM EFFECT FOR EACH ANIMAL ?   '
      km=min0(1,maxval(knsert(:nq)))
      CALL YNDEF(IOPRN2,KM)
      IF(IOPRN2.EQ.1)THEN
         WRITE(*,*)'COVARIANCE STRUCTURE FOR "SECOND ANIMAL" EFFECT'
         WRITE(*,9)'1  ...  NUMERATOR RELATIONSHIP MATRIX '
         WRITE(*,9)'2  ...  IDENTITY MATRIX               '
         WRITE(*,9)'3  ...  USER-SPECIFIED MATRIX         '
         CALL OPTdef(IOPCOV,1,3,1)
         IOPCOV=IOPCOV+1
         NRAND2=NANIM
         WRITE(*,fmt2(ipltf))'NAME FOR 2nd animal EFFECT ?',
     &                                         ' (MAX. 12 CHAR.S)'
         READ(*,'(a)')FNAME
         II=1
         if(iopmod.eq.3.and.nquni<1)then
            WRITE(*,*)'IS ',FNAME,' TO BE FITTED FOR ALL TRAITS ?'
            if(minval(knsert).eq.maxval(knsert))then
               CALL YNDEF(II,1)
            else
               CALL YNDEF(II,0)
            end if
         end if
         JJ=1
         DO IQ=1,NQ
         IF(II.EQ.0)THEN
            WRITE(*,*)'FIT ',FNAME,' FOR TRAIT NO.',IQ
            KK=0
            IF(KNSERT(IQ).GT.0)KK=1
            CALL YNDEF(JJ,KK)
         END IF
         IF(JJ.EQ.1)THEN
            nfix2(iq)=nfix2(iq)+jj
            nlev(nfix2(iq),iq)=nrand2
            fixed(nfix2(iq),iq)=fname
            if(fixed(nfix2(iq),iq).eq.blank12)write(fixed(nfix2(iq),iq),
     &                           '(''*fixed'',i2,''-'',i2,''*'')')n,iq
            NQ222=NQ222+1
         END IF
         end do !iq
      ELSE
         NRAND2=0
         IOPCOV=0
      END IF
      NEFF(:NQ)=NFIX2(:NQ)+1
      NEQNS=LIM3+(NQ+NQ222)*nanim

C     DETERMINE MODEL NUMBER
      IF(IOPRN2.EQ.0)THEN
         IMODEL=1+ioprn1
      ELSE
         IMODEL=2+IOPCOV+ioprn1*4
      END IF
      jparam=1
      WRITE(*,*)' '
      WRITE(*,9)'NO. OF MODEL TO BE FITTED =',IMODEL
      WRITE(*,*)' '

      allocate(ieqnew(neqns),stat=ii)
      if(ii>0)stop 'dfpre3 : alloc IEQNEW'     
      NFILL=0
      IEQNEW =(/ (i,i=1,neqns) /)

C     READ PHENOTYPIC MEANS FOR ALL TRAITS & RIGHT HAND SIDES
      call read_iun23 

C     PROCESS DATA
      CALL DFMME3(IPOSFX)

c     least squares solutions for fixed effects only
      call dflsq3(lim0)
      close(iun52)

C     WRITE HEADER TO FILE "59"
      call set_nosvec
      CALL DFWR59
      RETURN
910   FORMAT(/40('*')/10X,A/40('*')/)
9     FORMAT(8X,A,I8)

      contains

c     ========================================
      subroutine read_iun11 (iopmod,llq,iequal)
c     ========================================
 
      integer, intent(out)                   :: iopmod,llq,iequal
      integer                                :: k,l,m,krand1

      read(iun11)fped,fdata,cwdir
      READ(iun11)nnped
      READ(IUN11)IOPMOD,LLQ
!      IF(IOPMOD.lt.3)STOP 'MISMATCH WITH OUTPUT FROM "DFPREP" !'
c     ... iopmod=3 -> multivariate an., =4/5 cov. function model

      CALL CHKLEV(LLQ,MAXNQ,'MAXNQ ','TRAITS',6)

      READ(IUN11)KCOV(:LLQ)
      READ(IUN11)KFIX(:LLQ)
      READ(IUN11)KFIX1(:LLQ)
      READ(IUN11)KINT(:LLQ)
      READ(IUN11)KNSERT(:LLQ)
      nl=0
      READ(IUN11)((NL(L,K),L=1,KFIX(K)), K=1,LLQ)
      
      mfix=maxval(kfix(:llq))
      READ(IUN11)((( ii ,M=1,NL(L,K)), L=1,KFIX(K)), K=1,LLQ) ! idfix
      READ(IUN11)((( ii ,M=1,NL(L,K)), L=1,KFIX(K)), K=1,LLQ) ! nnfix

      read(iun11)krand1
      IF(krand1>0)THEN
         allocate(nlrnd1(krand1),stat=ii)
         read(iun11)nlrnd1
         print *,'nlrnd1',nlrnd1
         read(iun11)(( ii ,m=1,nlrnd1(l)),l=1,krand1) ! idrnd1
      END IF

      if(iopmod.eq.4)then
         read(iun11)nage
         allocate(iiage(nage),nnage(nage,llq),stat=ii)
         if(ii>0)stop 'dfpre3 : alloc ages'
         read(iun11)iiage
         read(iun11)nnage
      end if

      iequal=0
      do i=2,llq
      if(kcov(i).ne.kcov(1))return
      end do
      iequal=1

      return
      end subroutine read_iun11

c     =====================
      subroutine read_iun23
c     =====================

      mcov=maxval(ncov)
      call all_means (nq,mcov)
      mnr8=maxval(nr8)
      allocate(yy(mnr8),stat=ii)
      if(ii>0)stop 'alloc read_23'

      DO IQ=1,NQ
      II=ITRAIT(IQ)

c     ... means
      READ(IUN23)YY(:NR8(IQ))
      CBAR(:ncov(iq),IQ)=YY(:ncov(iq))
      YBAR(IQ)=YY(II)

c     ... st devs
      READ(IUN23)YY( :NR8(IQ))
      CSDEV(:ncov(iq),IQ)=YY(:ncov(iq))
      SDEV(IQ)=YY(II)

c     ... coeff var
      READ(IUN23)YY(:NR8(IQ))
      CVAR(IQ)=YY(II)

c     ... range
      READ(IUN23)YY(:NR8(IQ))
      YMIN(IQ)=YY(II)
      READ(IUN23)YY(:NR8(IQ))
      YMAX(IQ)=YY(II)

c     ... standardised range
      READ(IUN23)YY(:NR8(IQ))
      SMIN(IQ)=YY(II)
      READ(IUN23)YY(:NR8(IQ))
      SMAX(IQ)=YY(II)

      end do
      return
      end subroutine read_iun23

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

C     ====================================================================
      SUBROUTINE DFIZR3 (IQ,NRZERO,NFL,NNFIX)
C     ====================================================================

      integer,intent(in)     :: iq,nfl,nnfix
      integer,intent(inout)  :: nrzero

      integer                :: i,ii,jj

C     SET 1ST LEVEL OF EACH ADD. FIXED EFFECT TO ZERO
      DO I=2,NNFIX
      NRZERO=NRZERO+1
      KRZERO(NRZERO)=IPOSFX(I,IQ)+1
      end do
      IF(NFL.EQ.1)RETURN

      WRITE(*,111)
      CALL OPTDEF(N0ADD,0,NFL,0)
      II=1
      DO I=1,N0ADD
      WRITE(*,*)'FOR ADDITIONAL DEPENDENCY NO.',I
      WRITE(*,fmt(ipltf))'... RUNNING NO. OF FIXED EFFECT ?      '
      IF(NNFIX.GT.1)CALL OPTION(II,1,NNFIX)
      WRITE(*,fmt4(ipltf))'... LEVEL NO. OF   ',FIXED(II,IQ),'   ?'
      CALL OPTION(JJ,1,NLEV(II,IQ) )
      NRZERO=NRZERO+1
      KRZERO(NRZERO)=IPOSFX(II,IQ)+JJ
      end do
      RETURN

111   FORMAT(
     *   /1X,'COEFFICIENCT MATRIX FOR FIXED EFFECTS NOT OF FULL RANK'
     *   /1X,' --> PROGRAM SETS *FIRST* LEVEL OF EACH FIXED EFFECT  '
     *   /1X,'     TO ZERO (EXCEPT 1ST FE)                          '
     *   /1X,' --> SPECIFY ANY ADDITIONAL KNOWN DEPENDENCIES  :     '
     *   /1X,'     (E.G. DUE TO HIERARCHICAL STRUCTURE OF FE)       '/
     *   /1X,'NO. OF ADDITIONAL DEPENDENCIES ? (GIVE 0 IF NONE)     ') 
      END subroutine dfizr3

      END subroutine dfpre3








