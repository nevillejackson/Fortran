!============================================================================
      PROGRAM  DFPREP
!============================================================================

      use parameters
      use pedigrees
      use list_of_ids
      use form_of_inputs
      use nrm_inverse
      use c_numbers
      use c_fixed
      use ages
      use gen_groups
      use linkrow
      use work
      use version
      use today
      use units

      integer, dimension(5)                :: IIVEC
      integer, dimension(3)                :: MVEC
      integer, dimension(:), allocatable   :: nnn
      real(8), dimension(:), allocatable   :: smaxv,sminv,sdv,cvv
      real(8), dimension(:,:), allocatable :: xsum,xss,xmin,xmax
      real(8)                              :: var,xx,detl,xinbr,sfval,
     &                                        time,xsecs
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      CALL DFTIME(time,xsecs,isecs,1,'DFPREP')
      call dxversion(0)

C     -----------------
C     INTERACTIVE INPUT
C     -----------------

      WRITE(*,909)
      call today_is(0)
      WRITE(*,910)'INPUT/OUTPUT FILES'
      call dfpopen

      CALL DFPFIL(IUN33,IDMAX,IOWRT)

      CALL DFDFIL (IOPT,IOPIBR,IVLONG,JOSPEC,IOPBAS,IPRUNE)

C     INITIALIZE
      WRITE(*,*)'MAXIMUM NO. OF ANIMALS ?'
      CALL OPTDEF(MANIM,1,MAXAN,50000)
      MNRM=MANIM*5
      ALLOCATE ( IDVEC(MANIM), KFIRST(MANIM), KNEXT(MNRM), KVCOL(MNRM),
     &           STAT=II)
      IF(II>0)STOP 'ALLOCATE IDVEC'
      
      IFAC=(IDMAX/MANIM +2)
      MXREC2=MNRM
      NANIM2=0
      
C     FIRST RUN THROUGH PEDIGRE FILE : COLLECT ID.S
      CALL DFPIDS (JOSPEC,IOPT,NPEDR)

C     SORT VECTOR OF ID.S IN ASCENDING ORDER
      CALL DFISRT

C     FIRST RUN THROUGH DATA FILE : MARK ANIMALS WITH RECORDS & count
c                                   ages in the data

      allocate(nown(nanim2),idsire(nanim2),iddam(nanim2),isex(nanim2),
     &         nn(0:nanim2),npro(0:nanim2),stat=ii)
      if(ii>0)stop 'alloc nown'

      if(iopibr.gt.0)then
         allocate(igvec(nanim2),stat=ii)
         if(ii>0)stop 'alloc igvec'
      END if
      if(iopbas.gt.1)then
         allocate(ifxmrk(nanim2),stat=ii)
         ifxmrk=0
         if(ii>0)stop 'alloc ifxmrk'
      end if
      if(iopt.ge.4)then
         allocate(nage(nmeta),iavec(nmeta),iiage(maxage,nmeta),stat=ii)
         if(ii>0)stop 'alloc iiage'
      END if

      CALL DFOWN (IOPT,IUN34,NNOWN)

C     SECOND RUN THROUGH PEDIGREE FILE : RECODE PARENTAL ID.S
      CALL DFPREC (NPEDR,IUN33,IOPIBR,iopbas,IVLONG)

C     "PRUNE" PEDIGREES
      call prune_peds (iloop,ntpru)

C     RECODE TO NEW RUNNING ORDER
      NANIM=0
      NN=0
      DO I=1,NANIM2
      IF(NPRO(I).eq.-1)cycle
      NANIM=NANIM+1
      NN(I)=NANIM     
      end do
      isex=0  ! use as work vector
      do i=1,nanim2
      ii=nn(i)
      if(ii.eq.0)cycle  
      idad=idsire(i)
      isex(ii)=NN(IDad) 
      end do
      idsire=0
      idsire(:nanim)=isex(:nanim)
      isex=0  
      do i=1,nanim2
      ii=nn(i)
      if(ii.eq.0)cycle  
      imum=iddam(i)
      isex(ii)=NN(imum)
      end do
      iddam=0
      iddam(:nanim)=isex(:nanim)
      IF(IOPIBR.EQ.1)then
         do i=1,nanim2
         ii=nn(i)
         if(ii>0)IGVEC(ii)=IGVEC(I)
         end do
      end if

      WRITE(*,111)'NO. OF PEDIGREE LOOPS CARRIED OUT ',ILOOP
      if(iloop>0)then
         WRITE(*,111)'NO. OF PARENTS "PRUNED" ',NTPRU
         WRITE(*,111)'NEW NO. OF ANIMALS IN ANALYSIS',NANIM
      end if
      
      deallocate(isex,nown,knext,kvcol,stat=ii)
      if(ii>0)stop 'err deall'

!     -----------------------------------------------
!     SET UP INVERSE OF NUMERATOR RELATIONSHIP MATRIX
!     -----------------------------------------------
      if(iospec.eq.4)then      ! sire model
        call b_inverse(nanim,ninbr,detl,xinbr,sfval)
      else if(nainv>1)then    ! multiple NRM
        call aa_inverse(nanim,iopbas,iopibr,ninbr,detl,xinbr,sfval)
      else                     ! "normal" NRM
        call a_inverse(nanim,iopbas,iopibr,ninbr,detl,xinbr,sfval)
      end if

!     -------------------------------------------------
!     RECODE DATA & CALCULATE PHENOTYPIC MEANS & S.DEV.
!     -------------------------------------------------

!     COLLECT FIXED EFFECTS CODES
      CALL DFCFIX(IOPT,IUN34)

!     recode and calculate means; write out unit "22"
      call recode_and_means

C     ------------------------------
C     WRITE INFORMATION TO UNIT "66"
C     ------------------------------

      call dxversion (iun66)
      WRITE(IUN66,901)
      WRITE(IUN66,*)'                   PROGRAM " D F P R E P " '
      WRITE(IUN66,*)' '
      WRITE(IUN66,*)'                 SET UP RELATIONSHIP MATRIX      '
      WRITE(IUN66,*)'                             &                   '
      WRITE(IUN66,*)'              RECODE ANIMAL IDS IN RUNNING ORDER '
      WRITE(IUN66,911)
      call today_is(iun66)
      WRITE(IUN66,*)'---------------------------'
      WRITE(IUN66,*)'DATA AND PEDIGREE STRUCTURE'
      WRITE(IUN66,*)'---------------------------'
      WRITE(IUN66,*)' '
      WRITE(IUN66,*)'READ DATA FROM FILE : "',FDATA,'" '
      WRITE(IUN66,111)'NO. OF RECORDS IN DATA',NDATA
      WRITE(IUN66,111)'NO. OF ANIMALS WITH RECORDS',NNOWN
      WRITE(IUN66,*)' '
      WRITE(IUN66,*)'READ PEDIGREES FROM FILE : "',FPED,'"  '
      WRITE(IUN66,111)'NO. OF PEDIGREE RECORDS',NPEDR
      WRITE(IUN66,111)'NO. OF ANIMALS IN TOTAL',NANIM2
      WRITE(IUN66,111)'NO. OF "SINGLE CONNECTION" PARENTS',NTPRU
      WRITE(IUN66,111)'NO. OF ANIMALS IN ANIMAL MODEL',NANIM
      WRITE(IUN66,*)' '
      WRITE(IUN66,111)'NO. OF RECORDS IN NRM INVERSE (HALFST.)',NREC1
      WRITE(IUN66,112)'LOG DETERMINANT OF NRM',DETL+DETL
      WRITE(IUN66,112)'AVERAGE INBREEDING COEFFICIENT',XINBR
      WRITE(IUN66,111)'NO. OF INBRED ANIMALS',NINBR
      IF(NINBR.GT.0)WRITE(IUN66,112)
     *             '... WITH AVERAGE INBREEDING COEFFICIENT',SFVAL
      IF(IOPIBR.EQ.1)THEN
         WRITE(IUN66,*)' '
         WRITE(IUN66,*)'AVERAGE INBREEDING COEFFICIENT FOR LEVELS OF '
     *                                                         ,GNAME
         DO I=0,LLEV
         IF(NG(I).GT.0)WRITE(IUN66,*)I,NG(I),SG(I)/NG(I)
         end do
      END IF
      WRITE(IUN66,*)' '
      WRITE(IUN66,*)'----------------------------------------------'
      WRITE(IUN66,*)'DATA STRUCTURE : OFFSPRING-PARENT COMBINATIONS'
      WRITE(IUN66,*)'----------------------------------------------'
      WRITE(IUN66,*)' '
      if(fped(1:6).ne.'      ')CALL DFPCNT(nanim,IOPT,NTPRU,IUN22,IUN33,
     &                                             IUN66,nmeta)
     
      WRITE(IUN66,*)' '
      WRITE(IUN66,*)'----------------------'
      WRITE(IUN66,*)'TRAITS AND COVARIABLES'
      WRITE(IUN66,*)'----------------------'
      WRITE(IUN66,*)' '
      ZZ=1.D0/NDATA
      ZZ1=1.D0/(NDATA-1)

      IF(IOPT.EQ.1)THEN
         WRITE(IUN66,*)'DATA FOR UNIVARIATE ANALYSIS'
         WRITE(*,*)'DATA FOR "DFUNI" '
      ELSE IF(IOPT.EQ.2)THEN
         WRITE(*,*)'DATA FOR "DXMUX" '
         WRITE(IUN66,*)'DATA FOR MULTIVARIATE ANALYSIS WITH EQUAL ',
     *                 'MODELS FOR ALL TRAITS'
      ELSE IF(IOPT.EQ.3)THEN
         WRITE(*,*)'DATA FOR "DXMUX" '
         WRITE(IUN66,*)'DATA FOR MULTIVARIATE ANALYSIS WITH ',
     *                 'DIFFERENT MODELS FOR DIFF. TRAITS'
      ELSE IF(IOPT.EQ.4)THEN
         WRITE(*,*)'DATA FOR "DxCoF" '
         WRITE(IUN66,*)'DATA FOR MULTIVARIATE ANALYSIS ',
     *                 'ESTIMATING COVARIANCE FUNCTIONS'
      ELSE IF(IOPT.EQ.5 .or. iopt.eq.6 )THEN
         WRITE(*,*)'DATA FOR "DxMRR" '
         WRITE(IUN66,*)'DATA FOR COVARIANCE FUNCTION ANALYSIS ',
     *                 'USING RANDOM REGRESSIONS '
      END IF
      WRITE(IUN66,*)' '

      DO I=1,NQ
      WRITE(IUN66,*)'  '
      WRITE(IUN66,*)'TRAIT NO.',I,'   NO. OF RECORDS =',NNN(I)
      WRITE(*,*)'TRAIT NO.',I,'   NO. OF RECORDS =',NNN(I)
      ZZ=1.D0/NNN(I)
      ZZ1=1.D0/(NNN(I)-1)

      do  j=1,kreal8(i)
      XX=XSUM(J,I)*ZZ
      VAR=(XSS(J,I)-XX*XSUM(J,I))*ZZ1
      if(var.gt.zero)SDV(J)=DSQRT(VAR)
      CVV(J)=SDV(J)*100.D0/XX
      SMINV(J)=(XMIN(J,I)-XX)/SDV(J)
      SMAXV(J)=(XMAX(J,I)-XX)/SDV(J)
      WRITE(*,902)I,J,XX,SDV(J),CVV(J),XMIN(J,I),XMAX(J,I)
      WRITE(IUN66,902)I,J,XX,SDV(J),CVV(J),XMIN(J,I),XMAX(J,I),
     *                   SMINV(J),SMAXV(J)
      XSUM(J,I)=XX
      if(abs(sminv(j))>outlie .or.abs(smaxv(j))>outlie)call warn_sdev
      end do

      WRITE(IUN23)XSUM(:kreal8(i),i)
      WRITE(IUN23)SDV(:kreal8(i))
      WRITE(IUN23)CVV(:kreal8(i))
      WRITE(IUN23)XMIN(:kreal8(i),i)
      WRITE(IUN23)XMAX(:kreal8(i),i)
      WRITE(IUN23)SMINV(:kreal8(i))
      WRITE(IUN23)SMAXV(:kreal8(i))
      end do
      WRITE(*,*)' '

C     WRITE OUT INFO TO UNIT "11"
      write(iun11)fped,fdata,cwdir
      CALL DFSFIX(IOPT,IUN11,IUN66)
      IF(IOPT.ge.4)CALL DFAGEW(IQ,IUN11,IUN66)

C     WRITE OUT FILE WITH ORIGINAL IDS ETC. FOR USE WITH ANIMAL SOLUTIONS
      DO I=1,NANIM2
      IF(NPRO(I).ne.-1)IVEC(NN(I))=IDVEC(I)
      end do
      WRITE(IUN11)NANIM,NANIM2,IOPIBR,nainv,detl,nna(:nainv)
      write(iun11)ivec(:nanim)
      write(iun11)u(1:)
      IF(IOPIBR.EQ.1)THEN
         write(iun11)IGVEC(:NANIM)
         WRITE(IUN11)LLEV,IGFIX,GNAME
      END IF
      WRITE(IUN66,901)
      CALL DFTIME(time,xsecs,isecs,11,'DFPREP')
      STOP 'end of "DFPREP"'

111   FORMAT(1X,A,T45,' = ',I15)
112   FORMAT(1X,A,T45,' = ',F15.5)
901   FORMAT(/80('*')/)
902   FORMAT(2I4,2G15.6,F8.2,2G14.5,2F8.2)
909   FORMAT(/1X,T20,31('$')/
     $ 1X,T20,'$',T50,'$'/
     $ 1X,T20,'$',4X,'PROGRAM " D F P R E P " ',T50,'$'/
     $ 1X,T20,'$',T50,'$'/
     $ 1X,T20,25('$'),'*KM*$$'/)
910   FORMAT(/40('*')/10X,A/40('*')/)
911   FORMAT(/76('*'),'KM**'/)

      contains

!     ==================
      subroutine dfpopen
!     ==================

      CHARACTER(len=25) :: FSTAND,UNFOR='UNFORMATTED',FORMA='FORMATTED',
     &                     UNKN='UNKNOWN '

      FSTAND='DF11#DAT'
      CALL FCONCT(IUN11,FSTAND,UNFOR,UNKN)
      FSTAND='DF22#DAT'
      CALL FCONCT(IUN22,FSTAND,UNFOR,UNKN)
      FSTAND='DF23#DAT'
      CALL FCONCT(IUN23,FSTAND,UNFOR,UNKN)
      FSTAND='DF44#DAT'
      CALL FCONCT(IUN44,FSTAND,UNFOR,UNKN)
      FSTAND='DF66#DAT'
      CALL FCONCT(IUN66,FSTAND,FORMA,UNKN)
      RETURN
      END subroutine dfpopen

!     ===========================
      subroutine recode_and_means
!     ===========================

      allocate (xsum(mnr8,nq),xss(mnr8,nq),xmin(mnr8,nq),
     &          xmax(mnr8,nq),smaxv(mnr8),sminv(mnr8),sdv(mnr8),
     &          cvv(mnr8),nnn(nq),stat=ii)
      if(ii>0)stop 'alloc means'

      if(iopt.ge.4)then
         mage=maxval(nage)
         allocate(nnage(mage,nq,nmeta),xxage(mage,nq,nmeta),stat=ii)
         if(ii>0)stop 'alloc ages'
         nnage=0
         xxage=0.d0
      end if

      NDATA=0
      nnn=0
      XMIN=1.D12
      XMAX=-1.D12
      XSS=0.D0
      XSUM=0.D0
      REWIND IUN34

      iend=0
 155  call dfrd34 (iun34,ioffld,iopt,iivec,iq,iend,infmtd)
      if(iend.eq.99)go to 599

c     recode animals 
      if(iospec.ne.4)then
         CALL LNKFND(iivec(1),IA)
      else
         if(iivec(2).eq.0)go to 155
         CALL LNKFND(iivec(2),IA)
      end if
      mvec(1)=NN(IA)      ! ianim
      mvec(2)=IDSIRE(IA)  ! idad
      mvec(3)=IDDAM(IA)   ! imum

      CALL KOUNT(NDATA,10000)

C     REPLACE TRAIT NO. WITH AGE ...
      IF(IOPT.ge.4)then
         CALL DFAGEC(IQ,IUN11,IUN66)
         if(iopt.eq.4) jage=iq
!         IF(IOPT.eq.5)iq=1
      end if

c     recode FE/RE codes if necessary
      CALL DFIFIX(IQ)

c     accummulate trait totals ...
      NNN(IQ)=NNN(IQ)+1
      DO  L=1,KREAL8(IQ)
      XX=XVEC(L)
      IF(XX.GT.XMAX(L,IQ))XMAX(L,IQ)=XX
      IF(XX.LT.XMIN(L,IQ))XMIN(L,IQ)=XX
      XSS(L,IQ)=XSS(L,IQ)+XX*XX
      XSUM(L,IQ)=XSUM(L,IQ)+XX
      end do

!     write out data file ...
      IF(IOPT.ge.5 .AND. KNSERT(IQ).EQ.0)THEN
         WRITE(IUN22)iq,iavec,NN(IA),JVEC(:KINT(IQ)),XVEC(:KREAL8(IQ)),
     &               jvec(kint(iq)+1:kint(iq)+nmeta+nfxreg)

      ELSE IF( KNSERT(IQ).EQ.0)THEN 
         WRITE(IUN22)IQ,NN(IA),JVEC(:KINT(IQ)),XVEC(:KREAL8(IQ))

      else
         if(knsert(iq).le.3)then
            mm=mvec(knsert(iq))
            nni=kint(iq)
         else 
            call lnkfnd(jvec(kint(iq)),mm)
            nni=kint(iq)-1
         end if
         if(mm.le.0)call DFERR0(MM,knsert(iq),iivec(1))
         if(iopt.ge.5)then
            WRITE(IUN22)iq,iavec,NN(IA),JVEC(:nni),MM,XVEC(:KREAL8(IQ)),
     &               jvec(kint(iq)+1:kint(iq)+nmeta+nfxreg)
         else 
            WRITE(IUN22)iq,NN(IA),JVEC(:nni),MM,XVEC(:KREAL8(IQ))
         end if
      END IF
      GO TO 155
599   WRITE(*,'(1X,A,''='',I15)')'DATA FILE : NO. OF RECORDS PROCESSED',
     &                                                          NDATA
      return
      end subroutine recode_and_means

!     =================
      SUBROUTINE DFISRT
!     =================

      integer :: newplc, irow, iplace, nrow,icol,jplace,isave,k,n

      NEWPLC=0
      DO IROW=1,MROW2
      IPLACE=KFIRST(IROW)
      NROW=0
      do while (iplace>0)
      NEWPLC=NEWPLC+1
      ICOL=KVCOL(IPLACE)
      do while( (IPLACE.GT.0) .AND. (IPLACE.LT.NEWPLC) )
         IPLACE=KNEXT(IPLACE)
      END do
      JPLACE=KNEXT(IPLACE)
      IF(IPLACE.NE.NEWPLC)THEN
         ISAVE=IDVEC(NEWPLC)
         IDVEC(NEWPLC)=IDVEC(IPLACE)
         IDVEC(IPLACE)=ISAVE
         KNEXT(IPLACE)=KNEXT(NEWPLC)
         KNEXT(NEWPLC)=IPLACE
         ICOL=KVCOL(IPLACE)
         KVCOL(IPLACE)=KVCOL(NEWPLC)
         KVCOL(NEWPLC)=ICOL
      END IF
      NROW=NROW+1
      IPLACE=JPLACE
      end do
      KFIRST(IROW)=NROW
      end do

C     STORE NEW POSITION OF FIRST ELEMENT IN EACH ROW IN "KFIRST"
      K=1
      DO IROW=1,MROW2
      N=KFIRST(IROW)
      KFIRST(IROW)=K
      K=K+N
      end do
      KFIRST(MROW2+1)=K
      WRITE(*,'(1X,A,T45,'' = '',I15)')'HIGHEST ID',IDVEC(Nanim2)

      RETURN
      END subroutine dfisrt

!     ==================================
      subroutine prune_peds(iloop,ntpru)
!     ==================================

      INTEGER, INTENT(OUT) :: iloop,ntpru
      INTEGER              :: npru,i

      ntpru=0
      iloop=0
      IF(IPRUNE.eq.3.or.iospec.eq.4)return

      do iloop=1,250
      npru=0

      DO  I=1,NANIM2
      IF(IDSIRE(I)>0 .or.IDDAM(I)>0.or.NOWN(I)>0.or.NPRO(I)>1)cycle
      NPRO(I)=-1
      NPRU=NPRU+1
      end do

      IF(NPRU.eq.0)return  ! no more animals pruned

      DO  I=1,NANIM2
      IF(IPRUNE.NE.2 .AND. NPRO(IDSIRE(I)).EQ.-1)then
         IDSIRE(I)=0
         NTPRU=NTPRU+1
      END if
      IF(IPRUNE.NE.1 .AND. NPRO(IDDAM(I)).EQ.-1)then
         IDDAM(I)=0
         NTPRU=NTPRU+1
      END if
      end do

      END do  ! iloop

      end subroutine prune_peds

!     ====================
      subroutine warn_sdev
!     ====================

      integer :: ll

      write(*,'(1x,a,f8.1,a)')'Value more than',outlie, 
     &       ' standard deviations from mean found !!!'
      write(*,*)'trait             =',i
      write(*,*)'real variable no. =',j
      write(*,*)'mean & SD         =',xx,sdv(j)
      write(*,*)'range             =',xmin(j,i),xmax(j,i)
      write(*,*)'... standardised  =',sminv(j),smaxv(j)
      write(*,*)
      write(*,*)'This looks like a dangerous outlier ! '
      write(*,*)'Continue analysis ?'
      call yndef(ll,0)
      if(ll.eq.1)return
      stop '"DFPREP" : Outlier '
      end subroutine warn_sdev
     
      END program dfprep





















