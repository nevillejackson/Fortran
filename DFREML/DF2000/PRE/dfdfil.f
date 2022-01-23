!=========================================================================
      SUBROUTINE DFDFIL (iopt,iopibr,ivlong,jospec,iopbas,iprune)
!=========================================================================

      use platform
      use form_of_inputs
      use c_numbers
      use parameters
      use units, only : iun33, iun34
      use nrm_inverse, only : nainv

!     arguments
      integer, intent(out)           :: iopt,iopibr,ivlong,jospec,iopbas
     &,                                 iprune

      CHARACTER(len=25)              :: OLD='old',FORMA='formatted',
     &                                  UNFOR='unformatted',fped2

      integer, dimension(:), allocatable :: kmq
      integer                        :: ncov,nfix,nint,insert,i,m

 7100 WRITE(*,910)'DATA FILE :'
      WRITE(*,*)'RECORD LAYOUT : DATA CODED FOR '
      WRITE(*,*)'    1  ...  UNIVARIATE ANALYSIS   ("DFUNI")     '
      WRITE(*,*)'    2  ...  MULTIVARIATE ANALYSIS ("DxMUX") ',
     &                                   '- same model all traits'
      WRITE(*,*)'    3  ...  MULTIVARIATE ANALYSIS ("DxMUX") ',
     &                                            '- different models'
!      WRITE(*,*)'    4  ...  COVARIANCE FUNCTION   ("DxMUX")     '
      WRITE(*,fmt(ipltf))'    5  ...  COVARIANCE FUNCTION   ("DxMRR") '
!      WRITE(*,*)'    6  ...  COVARIANCE FUNCTION   ("DxMRR") '
!     &                                            '- different models'
      if(fped(1:6).eq.'      ')then
         CALL OPTdef(IOPT,5,6,5)
      else
         CALL OPTION(IOPT,1,6)
         if(iopt.eq.4)then
             write(*,*)'Invalid option - try again !'
             go to 7100
         end if
      end if
      
      WRITE(*,*)' '

200   WRITE(*,fmt(ipltf))'NAME OF DATA FILE ?'
!      if(iopt.eq.1)write(*,fmt2(ipltf))'<Return> for ',fped
      READ(*,'(a)')FDATA

!      if(iopt.eq.1.and.(fdata(1:6).eq.'      '.or.fdata.eq.fped))then
      if( iopt.eq.1 .and. fdata.eq.fped )then
         IUN34=IUN33
!         FDATA=FPED
         IOFFLD=IOFFLP
      else if(fdata.eq.fped)then
         write(*,*)'Data and pedigree file cannot be the same for ',
     &             'multivariate analyses !'
         stop
      else if(fdata(1:6).eq.'      ')then
         WRITE(*,*)'"Blank" file name not valid !'
         GO TO 200
      else
         WRITE(*,*)'FORMAT OF ',FDATA,'  :'
         WRITE(*,*)'    1  ...  UNFORMATTED (BINARY)              '
         WRITE(*,*)'    2  ...  FORMATTED - LIST DIRECTED INPUT   '
         WRITE(*,*)'    3  ...            - TO BE SPECIFIED       '
         CALL OPTDEF(IOFFLD,1,3,iofflp)
         IF(IOFFLD.EQ.1)THEN
            CALL FCONCT(IUN34,FDATA,UNFOR,OLD)
         ELSE
            CALL FCONCT(IUN34,FDATA,FORMA,OLD)
         END IF
      END IF         

      IF(IOFFLD.EQ.3)THEN
         WRITE(*,fmt2(ipltf))'GIVE FORTRAN FORMAT STATEMENT',
     &                                   ' (INCL. BRACKETS)'
         READ(*,100)INFMTD
      END IF
      WRITE(*,*)' '

      IF(IOPT.EQ.1)THEN
         NQ=1
      ELSE if(iopt.eq.5 .or.iopt.eq.6)then
         WRITE(*,*)'NO. OF TRAITS IN MULTIVARIATE ANALYSIS  ?     '
         CALL OPTDEF(NQ,1,MAXNQ,1)
      ELSE
         WRITE(*,*)'NO. OF TRAITS IN MULTIVARIATE ANALYSIS  ?     '
         CALL OPTDEF(NQ,1,MAXNQ,2)
      END IF

      IF(IOPT.EQ.3 .or. iopt.eq.6)THEN
         ALLOCATE(KMQ(NQ),STAT=II)
         if(ii>0)stop 'alloc kmq'
         m=1
         DO IQ=1,NQ
         WRITE(*,*)'NO. OF RHS PER RECORD FOR TRAIT NO.',IQ,'  ??'
         CALL OPTDEF(KMQ(IQ),1,MAXR8,M)
         M=KMQ(IQ)
         END DO

      ELSE
         WRITE(*,'(a)')' NO. OF RIGHT HAND SIDES PER RECORD ?     '
         CALL OPTDEF(MQ,1,MAXR8,1)
      END IF

      allocate(kint(nq),kreal8(nq),kfix(nq),kcov(nq),knsert(nq),
     *         kfix1(nq),stat=ii)
      if(ii>0)stop 'alloc dfdfil'

      IOSPEC=0
      JOSPEC=-1
      nmeta=0
      IF(IOPT.NE.3. and.iopt.ne.6)THEN
         CALL LAYOUT(1,MQ)
         KCOV(:nq)=NCOV
         KFIX(:nq)=NFIX
         kfix1(:nq)=nfix1
         KINT(:nq)=NINT
         KNSERT(:nq)=INSERT
         KREAL8(:nq)=NREAL8
         IF(INSERT.GT.3)JOSPEC=4
         IF(INSERT.GT.1)IOSPEC=3
      ELSE
         DO IQ=1,NQ
         WRITE(*,*)'  '
         WRITE(*,*)'* * * TRAIT NO. * * * ',IQ
         WRITE(*,*)' '
         CALL LAYOUT(iq,KMQ(IQ))
         KCOV(IQ)=NCOV
         KFIX(IQ)=NFIX
         kfix1(iq)=nfix1
         KINT(IQ)=NINT
         KNSERT(IQ)=INSERT
         IF(INSERT.GT.3)JOSPEC=4
         IF(INSERT.GT.1)IOSPEC=3
         KREAL8(IQ)=NREAL8
         end do
         if(maxval(kfix1(:nq))>1)then
            do i=2,nq
            if(kfix1(i).ne.kfix1(1))then
               write(*,'(a)')' Different no.s of add. RE for traits'
               write(*,'(a)')' Must fit "corresponding" RE first !!'

            end if
            end do
         end if
      END IF
      if( iopt.eq.5 .or. iopt.eq.6 )then
         write(*,*)'no. of "meta-meters" (age, ...) ?'
         call optdef(nmeta,1,5,1)
!         write(*,*)'no. of fixed regression within classes ?'
!         call optdef(nfxreg,0,5,0)
         nfxreg=0
      else
         nfxreg=0
      end if

      mint=maxval(kint)+nmeta+nfxreg
      mnr8=maxval(kreal8)
      allocate (jvec(mint), xvec(mnr8),stat=ii)
      if(ii>0)stop 'alloc dfdfil 2'

!     special options ...
      iopsm1=0
      IOPIBR=0
      IOPBAS=0
      IPRUNE=0
      IVLONG=3
      llev=0
      nainv=1
      IF(IOSPEC.GT.0)call special_opts (iospec)
      iospec=9
      do while (iospec>0)
       WRITE(*,*)' '
       WRITE(*,*)'SPECIAL EFFECTS OPTION'
       WRITE(*,*)'       0  ...  NONE  '
!      WRITE(*,*)'       1  ...  CALCULATE AVERAGE INBREEDING WITHIN'
!      WRITE(*,*)'               LEVELS OF A FIXED EFFECT           '
!      WRITE(*,*)'       2  ...  TREAT SOME ANIMALS AS FIXED        '
       WRITE(*,*)'       3  ...  DO NOT "PRUNE" ALL SINGLE-LINK PARENTS'
       WRITE(*,*)'       4  ...  FIT SIRE MODEL                       '
       IF(IOPT.EQ.1)THEN
          WRITE(*,*)'       5  ...  MULTIPLE NRM            '
!         WRITE(*,*)'       6  ...  MULTIPLE RESIDUAL VARIANCES '
       END if
       CALL OPTDEF(IOSPEC,0,6,0)
       IF(IOSPEC>4)IOSPEC=IOSPEC+1
       CALL SPECIAL_OPTS (IOSPEC)
      end do

!     write summary for layout of data file expected
      call wrsum_dfile

      WRITE(*,*)'CONTINUE ?'
      CALL YNDEF(LL,1)
      IF(LL.EQ.0)GO TO 7100
      if(fped(1:6).eq.'      '.and.iospec.ne.5)then
          write(*,*)'No pedigree file specified - have set option to ',
     &              'ignore pedigrees !'
          iospec=5
      end if

      return

100   FORMAT(A)
910   FORMAT(/40('*')/10X,A/40('*')/)
918   FORMAT(12X,A,I10)
919   FORMAT(12X,A,A)
929   FORMAT(12X,I4,3X,A,T33,A,A,i5)

      contains

!     ========================
      SUBROUTINE LAYOUT(iq,MQ)
!     ========================

      integer, intent(in)  :: iq,mq

      IF(iq.eq.1)then
         WRITE(*,fmt(ipltf))'NO. OF COVARIABLES TO BE FITTED ?'
         CALL OPTION(NCOV,0,MAXR8)
      else
         WRITE(*,*)'NO. OF COVARIABLES TO BE FITTED ?'
         call optdef(ncov,0,maxr8,kcov(iq-1))
      END if

      IF(iq.eq.1)then
         WRITE(*,fmt(ipltf))'NO. OF FIXED EFFECTS TO BE FITTED ?'
         CALL OPTION(NFIX,1,MAXINT)
      else
         WRITE(*,*)'NO. OF FIXED EFFECTS TO BE FITTED ?'
         call optdef(nfix,1,maxint,kfix(iq-1))
      END if

      IF(iq.eq.1)then
         WRITE(*,fmt(ipltf))
     &           'NO. OF "ADDITIONAL" (uncorrelated) RANDOM EFFECTS ?'
         if(iopt.ge.4.and. fped(1:6).eq.'      ')then
            CALL OPTdef(nfix1,0,1,0)
         else if(iopt.ge.4)then
            CALL OPTdef(nfix1,0,2,1)
         else
            CALL OPTION(nfix1,0,4)
         end if
      else
         WRITE(*,*)'NO. OF "ADDITIONAL" (uncorrelated) RANDOM EFFECTS ?'
         call optdef(nfix1,0,4,kfix1(iq-1))
      END if
      NINT=NFIX+nfix1

      WRITE(*,*)'Fit A  "SECOND ANIMAL" EFFECT ?'
      WRITE(*,10)'0  ...  NO  '
      WRITE(*,10)'1  ...  ANIMAL (insert code) '
      WRITE(*,10)'2  ...  SIRE   (insert)','4  ...  SIRE   (recode)'
      WRITE(*,10)'3  ...  DAM    (insert)','5  ...  DAM    (recode)'
 10   format(6x,a,9x,a)
      IF(iq.eq.1)then
         CALL OPTDEF(INSERT,0,5,0)
      else
         call optdef(insert,0,5,knsert(iq-1))
      END if
      IF( INSERT>3 )NINT=NINT+1
      NREAL8=NCOV+MQ
      RETURN
      END subroutine layout

!     =====================
      subroutine wrsum_dfile
!     =====================

      WRITE(*,*)' '
      WRITE(*,919)'SUMMARY OF DETAILS GIVEN FOR DATA FILE :'
      WRITE(*,919)'----------------------------------------'
      WRITE(*,919)'NAME                          = ',FDATA
      WRITE(*,918)'CONNECTED AS FORTRAN UNIT NO. = ',IUN34
      IF(IOFFLD.EQ.1)THEN
         WRITE(*,919)'INPUT MODE                    = ','UNFORMATTED'
      ELSE IF(IOFFLD.EQ.2)THEN
         WRITE(*,919)'INPUT MODE                    = ','LIST DIRECTED'
      ELSE IF(IOFFLD.EQ.3)THEN
         WRITE(*,919)'INPUT MODE                    = ','FORMAT GIVEN'
         WRITE(*,919)'  ',INFMTD
      END IF
      WRITE(*,*)' '
      WRITE(*,919)'RECORD LAYOUT '
      IF(IOPT.ge.2)THEN
         WRITE(*,929)1,'INTEGER','TRAIT NO.'
         K0=1
         LQ=NQ
         if(iopt.eq.4)lq=1
      ELSE
         K0=0
         LQ=1
      END IF

      WRITE(*,929)K0+1,'INTEGER','ANIMAL ID'
      WRITE(*,929)K0+2,'INTEGER','SIRE ID'
      WRITE(*,929)K0+3,'INTEGER','DAM ID'
      DO IIQ=1,LQ
      K=K0+3
      IF(LQ.GT.1)WRITE(*,918)'FOR TRAIT NO. =',IIQ
      DO I=K+1,K+KFIX(IIQ)
      WRITE(*,929)I,'INTEGER','FIXED EFFECT CODE'
      end do
      K=K+KFIX(IIQ)
      do i=1,kfix1(iiq)
      K=K+1
      WRITE(*,929)K,'INTEGER','ADD. RANDOM EFFECT CODE'
      END do
      IF(KNSERT(IIQ).GT.3)THEN
         K=K+1
         WRITE(*,929)K,'INTEGER','SECOND ANIMAL EFFECT CODE'
      END IF
      DO I=K+1,K+KCOV(IIQ)
      WRITE(*,929)I,'REAL*8','COVARIABLE'
      end do
      DO I=K+KCOV(IIQ)+1,K+KREAL8(IIQ)
      WRITE(*,929)I,'REAL*8','TRAIT/RHS '
      end do
      K=K+KREAL8(IIQ)+1
      IF(IOPT.ge.4)then
         do i=k+1,k+nmeta
         WRITE(*,929)K,'INTEGER','METAMETER FOR COV.F. (AGE)',i
         end do
         k=k+nmeta
         do i=k+1,k+nfxreg
         WRITE(*,929)K,'INTEGER','CLASS CODE FOR FIXED REGRESSION',I
         end do
      end if
      END DO
      WRITE(*,919)'----------------------------------------'
      WRITE(*,*)' '
      return

918   FORMAT(12X,A,I10)
919   FORMAT(12X,A,A)
929   FORMAT(12X,I4,3X,A,T33,A,A,i5)
      end subroutine wrsum_dfile


!     ================================
      subroutine special_opts (iospec)
!     ================================
      integer, intent(in)  :: iospec

      IF(IOSPEC.EQ.1)THEN
         IOPIBR=1
         WRITE(*,fmt(ipltf))'NAME FOR THE FIXED EFFECT (MAX. 12 CHAR) ?'
         READ(*,100)GNAME
         WRITE(*,fmt(ipltf))'NO. OF LEVELS FOR ',GNAME,'  ? '
         CALL OPTION(LLEV,1,MAXLEV)
         WRITE(*,*)'PEDIGREE RECORDS *MUST* CONTAIN CODE FOR THIS'
         WRITE(*,*)'EFFECT AS 4-TH INTEGER VARIABLE !'
         IVLONG=IVLONG+1

       ELSE IF(IOSPEC.EQ.2)THEN
         WRITE(*,*)'TREAT WHICH ANIMALS AS FIXED ?'
         WRITE(*,*)'       0  ...  NONE - ALL ANIMALS ARE RANDOM  '
         WRITE(*,*)'       1  ...  "BASE" ANIMALS ( = NO RECORDS,'
         WRITE(*,*)'               PARENTS UNKNOWN) ONLY '
         WRITE(*,*)'       2  ...  ALL ANIMALS *NOT* IN THE DATA '
         WRITE(*,*)'       3  ...  SOME ANIMALS, AS MARKED ON '
         WRITE(*,*)'               PEDIGREE FILE '
         CALL OPTDEF(IOPBAS,0,3,1)
         IF(IOPBAS.EQ.3)THEN
	            IVLONG=IVLONG+1
            WRITE(*,*)'CODE FOR STATUS (FIXED VS RANDOM) WILL BE READ'
            WRITE(*,*)'FROM PEDIGREE FILE AS VARIABLE NO.',IVLONG
         END IF

      ELSE IF(IOSPEC.EQ.3)THEN
         WRITE(*,*)'ELIMINATE WHICH PARENTS WITH SINGLE ADDITIVE ',
     *                                           ' GENETIC LINKS ?'
         WRITE(*,*)'       1  ...  SIRES ONLY'
         WRITE(*,*)'       2  ...  DAMS ONLY'
         WRITE(*,*)'       3  ...  NONE  '
         CALL OPTDEF(IPRUNE,1,3,3)

      ELSE IF(IOSPEC.EQ.4)THEN
         write(*,*)'You have chosen a "sire model"                '
         write(*,*)'-> this requires a pedigree file with columns '
         write(*,*)'        1:bull, 2:sire of bull, 3:MGS of bull '
         write(*,fmt(ipltf))'   give name for this pedigree file  '
         write(*,fmt2(ipltf))'<Return> for ',fped
         read(*,'(a)')fped2
         if( fped2(1:6).eq.'      ')fped2=fped
         if(fped2.ne.fped)then
            close(iun33)
            fped=fped2
            WRITE(*,*)'FORMAT OF ',FPED,'  :'
            WRITE(*,*)'    1  ...  UNFORMATTED (BINARY)              '
            WRITE(*,*)'    2  ...  FORMATTED - LIST DIRECTED INPUT   '
            WRITE(*,*)'    3  ...            - TO BE SPECIFIED       '
            CALL OPTDEF(IOFFLP,1,3,2)
            IF(IOFFLP.EQ.3)THEN
               WRITE(*,fmt(ipltf))
     &         'GIVE FORTRAN FORMAT STATEMENT (INCL BRACKETS)'
               READ(*,100)INFMTP
            END IF
            WRITE(*,*)' '
            WRITE(*,*)'HIGHEST ANIMAL IDENTITY ?'
            CALL OPTDEF(IDMAX,1,IBIG,999999)
            WRITE(*,*)' '
            WRITE(*,919)'SUMMARY OF DETAILS GIVEN FOR PEDIGREE FILE :'
            WRITE(*,919)'--------------------------------------------'
            WRITE(*,919)'NAME                          = ',FPED
            WRITE(*,918)'CONNECTED AS FORTRAN UNIT NO. = ',IUN33
            IF(IOFFLP.EQ.1)THEN
            WRITE(*,919)'INPUT MODE              = ','UNFORMATTED'
            ELSE IF(IOFFLP.EQ.2)THEN
            WRITE(*,919)'INPUT MODE              = ','LIST DIRECTED'
            ELSE IF(IOFFLP.EQ.3)THEN
            WRITE(*,919)'INPUT MODE              = ','FORMAT GIVEN'
            WRITE(*,919)'  ',INFMTP
            END IF
            WRITE(*,918)'HIGHEST ANIMAL ID ALLOWED     = ',IDMAX
            WRITE(*,*)' '
            WRITE(*,919)'RECORD LAYOUT '
            WRITE(*,929)1,'INTEGER','SIRE (of animal) = bull ID'
            WRITE(*,929)2,'INTEGER','SIRE (of bull) ID'
            WRITE(*,929)3,'INTEGER','Maternal grandsire (of bull) ID'
            WRITE(*,919)'--------------------------------------------'
            WRITE(*,*)' '
            WRITE(*,*)'CONTINUE ?'
            CALL YNDEF(LL,1)
            IF(LL.EQ.0)stop 'pedigree : "sire" model'
            IF(IOFFLP.EQ.1)THEN
               CALL FCONCT(IUN33,FPED,UNFOR,OLD)
            ELSE
               CALL FCONCT(IUN33,FPED,FORMA,OLD)
            END IF         
         if(iopt.ge.5. and. kfix1(1)>0)then
            write(*,*)'You are fitting an additional RE in a CF ',
     &                'analysis'
            write(*,*)'Presumably this is the perm.env. effect ',
     &                'for animals'
            write(*,*)'Replace animal code with sire code ?'
            call yndef(iopsm1,0)
            if(iopsm1.eq.1.and.kfix1(1)>1)then
                write(*,*)'there is more than one add. RE code '
                write(*,*)'give running no. of code to be replaced'
                call optdef(iopsm1,1,kfix1(1),1)
            end if
         end if
         END IF

      ELSE IF(IOSPEC.EQ.6)THEN
         write(*,*)'No. of NRM/ genetic variance components ?'
         call option(nainv,1,mxainv)
         write(*,*)'Expect "type of animal" indicator as 4-th ',
     &                             'variable in pedigree file '
      END IF

      return
100   FORMAT(A)
910   FORMAT(/40('*')/10X,A/40('*')/)
918   FORMAT(12X,A,I10)
919   FORMAT(12X,A,A)
929   FORMAT(12X,I4,3X,A,T33,A,A,i5)
      end subroutine special_opts

      end subroutine dfdfil

c=========================================================================
      subroutine dfpfil(iun33,idmax,iowrt)
c=========================================================================

      use form_of_inputs
      use platform
      use parameters

      integer, intent(in)            :: iun33
      integer, intent(out)           :: idmax,iowrt


      CHARACTER(len=25)              :: OLD='OLD',UNFOR='UNFORMATTED',
     &                                  FORMA='FORMATTED'

      if(ipltf.eq.1)call dfcwd(cwdir)

 7000 WRITE(*,910)'PEDIGREE FILE : '

 200  WRITE(*,fmt(ipltf))'FILE NAME ?  '
      READ(*,100)FPED
      if(fped(1:6).eq.'      ')then
         WRITE(*,*)'"Blank" file name given - only valid for ',
     &             'Cov. Function analyses on phenotypic level '
         write(*,*)'Confirm that that is your intention ! '
         call yndef(ii,0)
         if(ii.eq.0)go to 200
         iofflp=2
         idmax=ibig
         return
      end if
      WRITE(*,*)' '

      WRITE(*,*)'FORMAT OF ',FPED,'  :'
      WRITE(*,*)'    1  ...  UNFORMATTED (BINARY)              '
      WRITE(*,*)'    2  ...  FORMATTED - LIST DIRECTED INPUT   '
      WRITE(*,*)'    3  ...            - TO BE SPECIFIED       '
      CALL OPTDEF(IOFFLP,1,3,2)
      IF(IOFFLP.EQ.3)THEN
         WRITE(*,fmt(ipltf))
     &                 'GIVE FORTRAN FORMAT STATEMENT (INCL BRACKETS)'
         READ(*,100)INFMTP
      END IF
      WRITE(*,*)' '

      WRITE(*,*)'HIGHEST ANIMAL IDENTITY ?'
      CALL OPTDEF(IDMAX,1,IBIG,999999)
      IOWRT=0

      WRITE(*,*)' '
      WRITE(*,919)'SUMMARY OF DETAILS GIVEN FOR PEDIGREE FILE :'
      WRITE(*,919)'--------------------------------------------'
      WRITE(*,919)'NAME                          = ',FPED
      WRITE(*,918)'CONNECTED AS FORTRAN UNIT NO. = ',IUN33
      IF(IOFFLP.EQ.1)THEN
         WRITE(*,919)'INPUT MODE                    = ','UNFORMATTED'
      ELSE IF(IOFFLP.EQ.2)THEN
         WRITE(*,919)'INPUT MODE                    = ','LIST DIRECTED'
      ELSE IF(IOFFLP.EQ.3)THEN
         WRITE(*,919)'INPUT MODE                    = ','FORMAT GIVEN'
         WRITE(*,919)'  ',INFMTP
      END IF
      WRITE(*,918)'HIGHEST ANIMAL ID ALLOWED     = ',IDMAX
      WRITE(*,*)' '
      WRITE(*,919)'RECORD LAYOUT '
      WRITE(*,929)1,'INTEGER','ANIMAL ID'
      WRITE(*,929)2,'INTEGER','SIRE ID'
      WRITE(*,929)3,'INTEGER','DAM ID'
      WRITE(*,919)'--------------------------------------------'
      WRITE(*,*)' '
      WRITE(*,*)'CONTINUE ?'
      CALL YNDEF(LL,1)
      IF(LL.EQ.0)GO TO 7000

      IF(IOFFLP.EQ.1)THEN
         CALL FCONCT(IUN33,FPED,UNFOR,OLD)
      ELSE
         CALL FCONCT(IUN33,FPED,FORMA,OLD)
      END IF         
      return

100   FORMAT(A)
910   FORMAT(/40(1H*)/10X,A/40(1H*)/)
918   FORMAT(12X,A,I10)
919   FORMAT(12X,A,A)
929   FORMAT(12X,I4,3X,A,T33,A,A)
      end subroutine dfpfil

