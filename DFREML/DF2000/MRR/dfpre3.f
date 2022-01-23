!===========================================================================
      SUBROUTINE  DxPRE3
!===========================================================================

      use params
      use parameters, only : mxparm
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
      use read_iun52
      use phimatrix
      use platform

      CHARACTER(len=12)                   :: fname,
     &                                       blank12='            '
      integer, dimension (8)              :: nnped
      integer, dimension (:),allocatable  :: kint,kfix,kfix1,knsert,kcov
      integer, dimension (maxfix+7,maxnq) :: nl
      integer, dimension (maxfix,maxnq)   :: iposfx
      real(8), dimension (:), allocatable :: yy
      real(8)                             :: detl
      integer, dimension(:), allocatable  :: nlrnd1

      integer                             :: maxmq=19      ! max. no. RHS
      integer                             :: maxan=999999  ! max. no. animals
      integer                             :: ii
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C     OUTPUT FROM PROGRAM "DFPREP"
      call read_iun11 (iopmod,llq)

C     -----------------
!     INTERACTIVE INPUT
C     -----------------

      CALL DFCMMT

      WRITE(*,910)'TRAIT(S) TO BE ANALYSED'

      WRITE(*,*)'NO. OF TRAITS IN ANALYSIS ?'
      CALL OPTDEF(NQ,1,MAXNQ,LLQ)

      manq=mage*nq
      nanq=nage(1)*nq
      allocate(mmark(manq,mxcomb),stat=ii)
      if(ii>0)stop 'alloc mmark'
      NQQ=NQ*(NQ+1)/2

!     allocate arrays ...
      call all_levels_1(nq)
      call all_levels_2(nq,maxcov,maxfix)
      call all_numbers(nq,nqq)
      call all_names (nq,mxparm,maxfix,maxcov)
      call all_combis (nq,mxcomb)
      call all_mme3(nq)

      LIM0=1
      mxobs=0
      irpmod=1
    
      DO  IQ=1,NQ
      if(nq>1)write(*,'(1x,a,i3)')'DETAILS FOR TRAIT NO. =',iq
      WRITE(*,fmt3(ipltf))'Give name (max. 12 char.s) for trait',IQ
      READ(*,'(a)')TRAIT(IQ)
      if(trait(iq).eq.blank12)write(trait(iq),
     &                                     '(''*trait'',i3,''*  '')')iq

      write(*,*)'MAXIMUM NO. OF RECORDS PER ANIMAL FOR ',trait(iq),' ?'
      if(iq.eq.1)then
         call optdef(n,1,999,nage(1))
      else
         call optdef(n,1,999,mxobs(iq-1))
      end if
      mxobs(iq)=n 

      WRITE(*,*)'... NO. OF RIGHT HAND SIDES PER RECORD for ',
     &                                                  trait(iq),'?'
      if(iq.eq.1)then
         CALL OPTDEF(MQ,1,MAXMQ,1)
      else
         CALL OPTDEF(MQ,1,MAXMQ,NR8(iq-1))
      end if
      IF(MQ.EQ.1)THEN
         jj=1
      ELSE 
         WRITE(*,*)' '
         if(iq.eq.1)then
            WRITE(*,fmt(ipltf))' ... RUNNING NO. OF RHS FOR THIS RUN ?'
            CALL OPTION(JJ,1,MQ)
         else
            WRITE(*,*)' ... RUNNING NO. OF RHS FOR THIS RUN ?'
            CALL OPTDEF(JJ,1,MQ,itrait(iq-1))
         end if
      END IF
      ITRAIT(iq)=JJ
      NR8(iq)=MQ
      end do ! iq
      nxobs=0

      WRITE(*,910)'FIXED PART OF THE MODEL'
      NFRM=0
      kftfix=0
      irrmet=1
      do iq=1,nq
      write(*,*)
      write(*,'(a,a,i3,a)')'Fit fixed regression on "age" to model ',
     &          'population trajectory for trait no.',iq,'?'
      write(*,fmt9(1))'0  ...  No '
      write(*,fmt9(ipltf))
     &                'N  ...  Yes, to order "N" - give value for N'
      if(iq.eq.1 .or. iopmod.eq.6)then
         call option( kftfix(iq),0,mage)
      else
         call optdef( kftfix(iq),0,mage,kftfix(iq-1))
      end if
      if( kftfix(iq)>0 )then
         write(*,*)'Fit *fixed* regression on ... ?           '
         write(*,9)'0  ...  Ordinary polynomial of "age"    '
         write(*,9)'1  ...  Orthogonal (Legendre) polynomials of "age" '
         write(*,9)'2  ...  User defined fuction of "age"         '
         if(kftfix(iq).eq.5)
     &   write(*,9)'3  ...  L.R. Schaeffer''s dairy model (5 coeff.s) '
         if(iq.eq.1)then
            call optdef(irropt(0,iq),0,3,1)
         else
            call optdef( irropt(0,iq),0,3,irropt(0,iq-1) )
         end if
         if(irropt(0,iq).eq.2)write(*,*)'File "DF21#DAT" must exist ! '
         if(nmeta>1)then
            write(*,*)'use "meta-meter" no. ? '
            if(iq.eq.1)then
               call optdef(irrmet(0,iq),1,nmeta,1)
            else
               call optdef( irrmet(0,iq),1,nmeta,irrmet(0,iq-1) )
            end if
            if(irropt(0,iq)>nage(irrmet(0,iq)))then
               write(*,*)'Order of fit greater than no. of "ages" ??'
               write(*,*)'Meta-meter no.         =',irrmet(0,iq)
               write(*,*)'No. of "ages" found    =',nage(irrmet(0,iq))
               write(*,*)'Order of fit specified =',irropt(0,iq)
               write(*,*)'Continue ?'
               call yndef(ii,0)
               if(ii.eq.0)stop
            end if
         end if
      end if
      end do ! iq

      DO IQ=1,NQ
      WRITE(*,*)'NO. OF COVARIABLES TO BE FITTED FOR ',TRAIT(IQ),' ?'
      CALL OPTDEF(NNCOV,0,MAXCOV,KCOV(IQ))
      NCOV(IQ)=NNCOV
      NR8(IQ)=NR8(IQ)+NNCOV
      NNFR=kftfix(iq)
      IF(NNCOV.GT.0)THEN
         CALL CHKLEV(NNCOV,MAXCOV,'MAXCOV','COVARIABLES',11)
         ITRAIT(IQ)=ITRAIT(IQ)+NNCOV
         DO I=1,NNCOV
         WRITE(*,fmt3(ipltf))
     &          'GIVE NAME (MAX. 12 CHAR.S) FOR COVARIABLE',i
         READ(*,'(a)')COVAR(I,IQ)
         if(covar(i,iq).eq.blank12)write(covar(i,iq),124)i,iq
 124     format('*covar',i2,'-',i2,'*')
         CALL DFPOPT(N,COVAR(I,IQ))
         NPOW(I,IQ)=N
         NNFR=NNFR+N
         end do
      END IF
      NFR(IQ)=NNFR
      NFRST(IQ)=NFRM+LIM0
      NFRM=NFRM+NNFR
      end do          ! iq
      LIM1=LIM0+NFRM

      WRITE(*,*)'  '
      NRZERO=0
      NFLM=0
      LIM2=LIM1
      DO IQ=1,NQ
      WRITE(*,*)'NO. OF FIXED EFFECTS TO BE FITTED FOR ',
     *                                         TRAIT(IQ),'  ?'
      CALL OPTDEF(NNFIX,0,MAXFIX,KFIX(IQ))
      NFIX(IQ)=NNFIX
      NNFL=0
      IF(NNFIX.GT.0)THEN
         CALL CHKLEV(NNFIX,MAXFIX,'MAXFIX','FIXED EFFECTS',13)
         DO I=1,NNFIX
         IPOSFX(I,IQ)=LIM2+NNFL
         WRITE(*,fmt3(ipltf))
     &        'GIVE NAME (MAX 12 CHAR.S) FOR FIXED EFFECT',i
         READ(*,'(a)')FIXED(I,IQ)
         if(fixed(i,iq).eq.blank12)write(fixed(i,iq),125)i,iq
 125     format('*effct',i2,'-',i2,'*')
         WRITE(*,*)'NO. OF LEVELS FOR  ',FIXED(I,IQ),' FOR AGE ',
     *                                                 TRAIT(IQ),'  ?'
         CALL OPTDEF( N,1,99999,NL(I,IQ) )
         NNFL=NNFL+N
         NLEV(I,IQ)=N
         end do  ! i=1,nnfix
         NFL(IQ)=NNFL
         LIM2=LIM2+NNFL
         if(iq.eq.1)then
            allocate(krzero(nq*nnfl),stat=ii)
            if(ii>0)stop 'alloc krzero'
         end if
         CALL DFIZR3(IQ,NRZERO,NNFL,NNFIX)
      END IF
      NFLM=NFLM+NNFL
      end do      ! iq
      IF(NRZERO.GT.1)CALL DFKZER(NRZERO,KRZERO)

      mcov=maxval(ncov)
      mfix=maxval(nfix)+nfxreg
      mobs=sum(mxobs)
      mnfr=maxval(nfr)
      if(mnfr.eq.0)mnfr=1
      kfit=0

      WRITE(*,910)'RANDOM PART OF THE MODEL'
      READ(IUN44)DETL,II,KANIM
899   WRITE(*,*)'NO. OF LEVELS OF MAIN RANDOM EFFECT (ANIMALS) ?    '
      IF(fped(1:6).eq. '      ')write(*,*)
     &  'Give "0" to omit animal effects & fit equivalent model'
      CALL OPTDEF(NANIM,0,MAXAN,KANIM)

      IF( fped(1:6) .ne. '      ' .and. KANIM.NE.NANIM)THEN
         WRITE(*,*)'DISCREPANCY WITH NO.S FROM NRM INVERSE FOUND !'
         WRITE(*,*)'TOTAL NO. OF ANIMALS THERE WAS =',KANIM
         WRITE(*,*)'NO. GIVEN NOW WAS              =',NANIM
         WRITE(*,*)'=> 2 POSSIBILITIES :                '
         WRITE(*,9)' *  WRONG UNIT "44" CONNECTED          -> STOP RUN'
         WRITE(*,9)' *  INCORRECT NO. OF ANIMALS SPECIFIED -> TRY AGAIN'
         WRITE(*,*)'  '
         GO TO 899
      END IF
      if(nanim>0)then
         WRITE(*,*)' '
         do iq=1,nq
         call rr_type (1,1,iq)
         write(*,fmt6(ipltf))'Order of fit for "animal" CF',
     &           ' (2=lin,3=quad,4=cub,..) for trait',iq,' ?'
         if(iq.eq.1 .or. iopmod.eq.6)then
            call optdef(kfit(1,iq),1,mage,kftfix(iq))
         else
            call optdef(kfit(1,iq),1,mage,kfit(1,iq-1))
         end if
         end do ! iq
         NEFF(:nq)=nfix(:nq)+kfit(1,:nq)
      else 
         NEFF(:nq)=nfix(:nq)
      end if

      NQ111=0
      NFIX1(:nQ)=NFIX(:nQ)
      NRAND1=0
      ioprn1=0
      kk1=maxval(kfix1(:nq))
      ieqmod=0         
      mm2=0
      IF( fped(1:6) .ne. '      ' .or. nanim.eq.0 )THEN

          if(nanim.eq.0)then
             WRITE(*,*)'Fit (phenotypic) animal effects implicitly'
             ii=2
             irropt(1,:nq)=1
             kfit(1,:nq)=kftfix(:nq)
          else
             WRITE(*,*)'Fit permanent environmental effects for animals'
             write(*,'(8x,a)')'0  ... N0  '
             write(*,'(8x,a)')'1  ... YES - explicitely '
             write(*,fmt9(ipltf))' 2  ... YES - implicitely '
             call option(ii,0,2)
          end if

          if(ii.eq.1)then
             fname='P.env.animal'
             WRITE(*,*)'NO. OF LEVELS FOR  ',FNAME,'  ?        '
             CALL OPTDEF(NRAND1,1,199999,NLRND1(1) )
             WRITE(*,*)' '
             mm2=1
          else if(ii.eq.2)then
             ieqmod=1
          end if
          if(ii>0)then
             ioprn1=1
             do iq=1,nq
             call rr_type (4,irropt(1,iq),iq)
             if(irropt(4,iq)<4)then
             write(*,fmt6(ipltf))'Order of fit for "perm. env." CF',
     &                 ' (2=lin,3=quad,4=cub,..) for trait',iq,' ?'
             if(iq.eq.1 .or. iopmod.eq.6 )then
                call optdef(kfit(4,iq),1,mage,kfit(1,iq))
             else
                call optdef(kfit(4,iq),1,mage,kfit(4,iq-1))
             end if
             end if
             if(ii.eq.1)then   ! fit explicitely
                N=NFIX(IQ)+1
                NEFF(iq)=neff(iq)+kfit(4,iq)
                FIXED(N,IQ)=FNAME
                NLEV(N,IQ)=NRAND1
                NQ111=NQ111+1
                NFIX1(IQ)=N
              end if
              end do ! iq
          end if
          if(nanim.eq.0)kfit(1,:nq)=0

      ELSE ! no pedigree file
          write(*,*)'No pedigree file given ! '
          write(*,*)'"DxMrr" assumes that you want to fit a phenotypic',
     &              ' animal effect only '
      END IF
      lim3=lim2+ sum( kfit(4,:nq) )*nrand1

      lim3a=lim3
      NQ333=0
      nrand3=0
      ioprn3=0
      if(kk1>mm2)then
          WRITE(*,*)'FIT AN ADDITIONAL (UNCORRELATED) RANDOM EFFECT ?'
          CALL YNDEF(IOPRN3,kk1-mm2)
          IF(IOPRN3.EQ.1)THEN
             WRITE(*,fmt(ipltf))'NAME FOR THIS EFFECT ?(MAX. 12 CHAR.S)'
             READ(*,'(a)')FNAME
             if(fname.eq.blank12)write(fname,'(a12)')'*add. R.E.* '
             WRITE(*,*)'NO. OF LEVELS FOR  ',FNAME,'  ?        '
             CALL OPTDEF(NRAND3,1,199999,NLRND1(mm2+1) )

             WRITE(*,*)' '
             DO IQ=1,NQ
             call rr_type (5,irropt(1,iq),iq)
             write(*,fmt6(ipltf))'Order of fit for "add. R.E." CF',
     &                  ' (2=lin,3=quad,4=cub,..) for trait',iq,' ?'
             if(iq.eq.1 .or. iopmod.eq.6 )then
                call optdef(kfit(5,iq),1,mage,kfit(1,iq))
             else
                call optdef(kfit(5,iq),1,mage,kfit(5,iq-1))
             end if

             N=NFIX1(IQ)+1
             NEFF(iq)=neff(iq)+kfit(5,iq)
             FIXED(N,IQ)=FNAME
             NLEV(N,IQ)=NRAND3
             NQ333=NQ333+1
             NFIX1(IQ)=N
             end do
         END IF
         lim3=lim3+sum( kfit(5,:nq) )*nrand3
      end if

      NQ222=0
      NRAND2=0
      IOPCOV=0
      NFIX2(:nQ)=NFIX1(:nQ)
      if(nanim>0.and.fped(1:6).ne.'      ')then
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
         if(iopcov>1)IOPCOV=IOPCOV+1
         NRAND2=NANIM
         WRITE(*,fmt2(ipltf))'NAME FOR SECOND RAND EFFECT ? ',
     &                                               '(MAX 12 CHAR.S)'
         READ(*,'(a)')FNAME
         if(fname.eq.blank12)write(fname,'(a12)')'*2nd an. E.*'

         WRITE(*,*)' '
         DO IQ=1,NQ
         call rr_type (2,irropt(1,iq),iq)
         write(*,fmt6(ipltf))'Order of fit for "2nd animal" CF',
     &                 ' (2=lin,3=quad,4=cub,..) for trait',iq,' ?'
         call optdef (kfit(2,iq),1,mage,kfit(1,iq))

         N=NFIX1(IQ)+1
         NEFF(iq)=neff(iq)+kfit(2,iq)
         NLEV(N,IQ)=NRAND2
         FIXED(N,IQ)=FNAME
         NQ222=NQ222+1
         NFIX2(IQ)=N
         end do
      END IF
      END IF

C     DETERMINE MODEL NUMBER
      IF(IOPRN2.EQ.0)THEN
         IMODEL=1+ieqmod
      ELSE
         IMODEL=2+IOPCOV+Ieqmod*4
      END IF
      jparam=1
      WRITE(*,*)' '
      WRITE(*,9)'NO. OF MODEL TO BE FITTED =',IMODEL
      WRITE(*,*)' '

      ksfit=0
      do i=1,5
      ksfit(i)=sum(kfit(i,:nq))
      end do
      kfitmx=maxval(ksfit)

      NEQNS=LIM0+NFRM+NFLM+NANIM*(ksfit(1)+ksfit(2))
     &                    +NRAND1*ksfit(4)
     &                    +nrand3*ksfit(5)

       print *,'neq',neqns
      allocate(ieqnew(neqns),stat=ii)
      if(ii>0)stop 'dfpre3 : alloc IEQNEW'     
      NFILL=0
      IEQNEW =(/ (i,i=1,neqns) /)

C     READ PHENOTYPIC MEANS FOR ALL TRAITS & RIGHT HAND SIDES
      call read_iun23 

C     PROCESS DATA
      iall52=0
      m=max0(mage,mobs)
      allocate( nnrec(mage,nq),mmr(m), stat=ii)
      print *,'mme3'
      CALL DFMME3(maxfix,maxnq,IPOSFX)
      print *,'mme3'

c     least squares solutions for fixed effects only
      call dflsq3(lim0)
      close(iun52)

C     WRITE HEADER TO FILE "59"
      iosrch=0
      kopt=0
      call set_nosvec
      CALL DFWR59
      RETURN
910   FORMAT(/40('*')/10X,A/40('*')/)
9     FORMAT(8X,A,I8)

      contains

c     ==================================
      subroutine read_iun11 (iopmod,llq)
c     ==================================
 
      integer, intent(out)                   :: iopmod,llq

      integer                                :: mnfl,k,l,m,krand1

      READ(IUN11)FPED,FDATA,CWDIR
      READ(IUN11)NNPED
      READ(IUN11)IOPMOD,LLQ
      IF(IOPMOD.ne.5 .and.iopmod.ne.6)STOP 
     &                       'MISMATCH WITH OUTPUT FROM "DFPREP" !'

      allocate(kcov(llq),kfix(llq),kfix1(llq),kint(llq),knsert(llq),
     &         stat=ii)
      if(ii>0)stop 'read_iun11 : alloc 0'

      READ(IUN11)KCOV
      READ(IUN11)KFIX,nfxreg
      READ(IUN11)KFIX1
      READ(IUN11)KINT
      READ(IUN11)KNSERT
      READ(IUN11)((NL(L,K),L=1,KFIX(K)+nfxreg), K=1,LLQ)
      READ(IUN11)((( ii ,M=1,NL(L,K)), L=1,KFIX(K)+nfxreg), K=1,LLQ)
      READ(IUN11)((( ii ,m=1,NL(L,K)), L=1,KFIX(K)+nfxreg), K=1,LLQ)

      read(iun11)krand1
      IF(krand1>0)THEN
          allocate(nlrnd1(krand1),stat=ii)
          if(ii>0)stop 'read_iun11 : alloc 2'
          read(iun11)nlrnd1
          READ(iun11) ii               ! idrnd1
      END IF

      if(iopmod.ge.4)then
         read(iun11)nmeta
         allocate(nage(nmeta),iavec(nmeta),stat=ii)
         read(iun11)nage
         mage=maxval(nage)
          print *,'mage =',mage
         allocate(iiage(mage,nmeta),nnage(mage,llq,nmeta),stat=ii)
         if(ii>0)stop 'dfpre3 : alloc ages'
         read(iun11)iiage
         read(iun11)nnage
      end if
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

      READ(IUN23)YY(:NR8(IQ))            ! means
      CBAR(:ncov(iq),IQ)=YY(:ncov(iq))
      YBAR(IQ)=YY(II)

      READ(IUN23)YY( :NR8(IQ))           ! standard deviations
      CSDEV(:ncov(iq),IQ)=YY(:ncov(iq))
      SDEV(IQ)=YY(II)

      READ(IUN23)YY(:NR8(IQ))            ! coefficient of variation
      CVAR(IQ)=YY(II)

      READ(IUN23)YY(:NR8(IQ))            ! range
      YMIN(IQ)=YY(II)
      READ(IUN23)YY(:NR8(IQ))
      YMAX(IQ)=YY(II)

      READ(IUN23)YY(:NR8(IQ))            ! standardised range
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
      WRITE(*,'(a)')'TYPE COMMENTS - max. 6 lines & 80 char.s per line'
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

C     ============================================
      SUBROUTINE  DFIZR3 (IQ,NRZERO,NFL,NNFIX)
C     ============================================

      integer,intent(in)    :: iq,nfl,nnfix
      integer,intent(inout) :: nrzero

      integer               :: i,ii,jj,i1=2

C     SET 1ST LEVEL OF EACH ADD. FIXED EFFECT TO ZERO
      if(kftfix(iq)>0)i1=1 ! allow for intercept of fixed orthog. polynom.
      DO  I=i1,NNFIX
      NRZERO=NRZERO+1
      KRZERO(NRZERO)=IPOSFX(I,IQ)+1
      end do
      IF(NFL.EQ.1)RETURN

      WRITE(*,111)
      CALL OPTDEF(N0ADD,0,NFL,0)
      II=1
      DO I=1,N0ADD
      WRITE(*,*)'FOR ADDITIONAL DEPENDENCY NO.',I
      IF(NNFIX.GT.1)then
         WRITE(*,fmt(ipltf))' ... RUNNING NO. OF FIXED EFFECT ? '
         CALL OPTION(II,1,NNFIX)
      end if
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

!     =================================
      subroutine rr_type (ipar,idef,iq)
!     =================================

      integer, intent(in) :: ipar,idef,iq

      write(*,*)'Fit *random* regression on ... ?           '
      write(*,9)'0  ...  Ordinary polynomial of "age"    '
      write(*,9)'1  ...  Orthogonal (Legendre) polynomials of "age" '
      write(*,9)'2  ...  User defined fuction of "age"         '
      if(kfit(ipar,iq).eq.5)write(*,9)
     &                         '3  ...  L.R. Schaeffer''s dairy model '
      if(iq.eq.1.and.ipar.eq.4.and.ieqmod>0)write(*,9)
     &                         '4  ...  VF + parametric cov. structure'
      if(iq.eq.1)then
         call optdef(irropt(ipar,iq),0,4,idef)
      else    ! must fit same type of model for all traits !
         irropt(ipar,iq)=irropt(ipar,1)
!        call optdef(irropt(ipar,iq),0,3,irropt(ipar,iq-1) )
      end if
      if(irropt(ipar,iq).eq.2)write(*,*)'File "DF21#DAT" must exist ! '
      if(nmeta>1)then
         write(*,*)'use "meta-meter" no. ? '
         call optdef(irrmet(ipar,iq),1,nmeta,1)
         if(irropt(ipar,iq)>nage(irrmet(ipar,iq)))then
            write(*,*)'Order of fit greater than no. of "ages" ??'
            write(*,*)'Meta-meter no.         =',irrmet(ipar,iq)
            write(*,*)'No. of "ages" found    =',nage(irrmet(ipar,iq))
            write(*,*)'Order of fit specified =',irropt(ipar,iq)
            write(*,*)'Continue ?'
            call yndef(ii,0)
            if(ii.eq.0)stop
         end if
      end if
      return
9     FORMAT(8X,A,I8)
      end subroutine rr_type

      END subroutine dxpre3















