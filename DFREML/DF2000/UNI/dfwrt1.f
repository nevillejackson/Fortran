!===========================================================================
      SUBROUTINE DFWRT1 (ioprun,iosrch,nparm,kparm,xlike,fstart,zige)
!===========================================================================

      use names, only : param
      use simplex
      use powell
      use like
      use means
      use levels
      use parameters
      use iterates
      use units
      use numbers
      use sparse
      use names, only : trait
 
!      implicit double precision(a-h,o-z)
      real(8), intent(in) :: xlike,fstart,zige
      integer, intent(in) :: ioprun,iosrch,nparm,kparm
      real(8)             :: cv,rr,sigm,sigam,cam,esq
      real(8), dimension(:), allocatable  :: hsq,xmsq,siga,sigc,sigp
      real(8), dimension(:,:),allocatable :: csq


      IF(IOPRUN.LE.3)THEN
         WRITE(IUN66,*)'-------------------------------------'
         WRITE(IUN66,*)'OPTIONS SET IN OPTIMIZATION ROUTINE '
         WRITE(IUN66,*)'-------------------------------------'
         WRITE(IUN66,*)' '
         IF(IOSRCH.EQ.1)THEN
            WRITE(IUN66,*)'USE SIMPLEX PROCEDURE TO MINIMIZE -2 LOG L'
            WRITE(IUN66,*)' '
            WRITE(IUN66,902)'MAXIMUM NO. OF SIMPLEX ITERATES ALLOWED',
     *                                                        MROUND
            WRITE(IUN66,906)
     *       'MINIMUM VARIANCE OF FUNCTION VALUES IN SIMPLEX',XCONV
         ELSE IF(IOSRCH.EQ.2)THEN
            WRITE(IUN66,*)'USE POWELL''S PROCEDURE TO MINIMIZE -2 LOG L'
            WRITE(IUN66,902)'MAXIMUM NO. OF ITERATES ALLOWED ',MROUND
            WRITE(IUN66,906)'ACCURACY CRTERION (ESCALE) USED',ESCALE
         ELSE IF(IOSRCH.EQ.3)THEN
            WRITE(IUN66,*)'USE QUADRATIC APPROXIMATION OF LOG L'
            WRITE(IUN66,902)'MAXIMUM NO. OF ITERATES ALLOWED ',MROUND
C            WRITE(IUN66,906)'ACCURACY CRTERION  USED',XCONV
         END IF
         WRITE(IUN66,*)' '
         WRITE(IUN66,*)'-----------------------'
         WRITE(IUN66,*)'CHARACTERISTICS OF RUN :'
         WRITE(IUN66,*)'-----------------------'
         WRITE(IUN66,*)' '
         WRITE(IUN66,902)'RUN WITH OPTION',IOPRUN
         WRITE(IUN66,*)' '
         WRITE(IUN66,902)'NO. OF NON-ZERO PIVOTS ENCOUNTERED',Nsrow
         WRITE(IUN66,902)'NO. OF DEGREES OF FREEDOM 1',NDF1
         WRITE(IUN66,902)'NO. OF DEGREES OF FREEDOM 2',NDF2
         WRITE(IUN66,*)' '
         WRITE(IUN66,902)'NO. OF ITERATES CARRIED OUT',NROUND
         WRITE(IUN66,902)'NO. OF LIKELIHOODS EVALUATED',NCALL
         WRITE(IUN66,*)' '
         WRITE(IUN66,902)'PARAMETERS : STARTING VALUES AND ESTIMATES'
         DO 1 I=1,NPARM
1        WRITE(IUN66,5912)I,PARAM(I),START(I),XVEC(I)
         WRITE(IUN66,5912)0,'LOG L',-0.5D0*FSTART,XLIKE
        
         IF(IOSRCH.EQ.1) WRITE(IUN66,906)
     &                 'VARIANCE OF SIMPLEX FUNCTION VALUES',FSS
      END IF
      WRITE(IUN66,*)' '
      WRITE(IUN66,*)'-------------------------------------------'
      WRITE(IUN66,*)'ESTIMATES OF VARIANCES & GENETIC PARAMETERS'
      WRITE(IUN66,*)'-------------------------------------------'
      WRITE(IUN66,*)' '
      WRITE(IUN66,902)'UNIVARIATE ANALYSIS FOR TRAIT NO.',NTRAIT
      IF(IOPRUN.LE.3)THEN
         WRITE(IUN66,*)' '
         WRITE(IUN66,906)'TOTAL SUMS OF SQUARES (Y''Y)',YRY
         WRITE(IUN66,906)'SUMS OF SQUARES FOR RESIDUAL (Y''PY)',YPY
         WRITE(IUN66,906)'LOG DETERMINANT OF COEFFICIENT MATRIX',DETC
         WRITE(IUN66,906)'LOG DETERMINANT OF NRM',(DETL+DETL)
         IF(KPARM.NE.NPARM)THEN
            WRITE(IUN66,*)' '
            WRITE(IUN66,902)'TOTAL NO. OF PARAMETERS ',NPARM
            WRITE(IUN66,902)'LOG L MAXIMIZED W.R.T. ONLY',KPARM
            WRITE(IUN66,902)'PARAMETERS HELD CONSTANT :'
            DO I=1,NPARM
            IF(IPM(I)>0)WRITE(IUN66,*)I,'    ',PARAM(I),'  =',XVEC(I)
            end do
         END IF
      END IF
      IF(IOPRUN.LE.5.or.ioprun.eq.8)THEN
         WRITE(IUN66,906)'LOG LIKELIHOOD (WITH NRM)',xLIKE
         WRITE(*    ,906)'LOG LIKELIHOOD (WITH NRM)',xLIKE
         IF(IOPCOV.EQ.4)WRITE(IUN66,*)
     *              'LOG DETERMINANT OF "D" STILL TO BE INCORPORATED'
         WRITE(IUN66,*)' '

         call estimates

         do i=1,nainv
         WRITE(IUN66,9061)'ADDITIV-GENETIC (DIRECT) VARIANCE',i,siga(i)
         end do
         IF(nfix2>nfix1.and.nlev(nfix2)>0)THEN
            WRITE(IUN66,906)'SECOND "ANIMAL" VARIANCE',sigm
            IF(IOPCOV.EQ.2)WRITE(IUN66,906)
     *                'COVARIANCE BETWEEN ANIMAL EFFECTS',sigam
         END if
         DO I=1,IOPRN1
         if(nlev(nfix+i)>0)WRITE(IUN66,9061)
     &                   'VARIANCE DUE TO ADD. RANDOM EFFECT',I,sigc(i)
         end do
         WRITE(IUN66,906)'ERROR VARIANCE',ZIGE
         do i=1,nainv
         WRITE(IUN66,9061)'PHENOTYPIC VARIANCE',i,SIGP(i)
         WRITE(*,9061)'PHENOTYPIC VARIANCE',i,SIGP(i)
         end do
         if(nainv.eq.1)then
            CV=DSQRT(SIGP(1))*100.D0/YBAR(NTRAIT)
            WRITE(IUN66,907)'PHENOTYPIC STANDARD DEVIATION',
     &                                                 DSQRT(SIGP(1))
            WRITE(IUN66,907)'PHENOTYPIC COEFFICIENT OF VARIATION (%)',
     &                                                             CV
         end if
      END IF
      WRITE(IUN66,*)' '
      do i=1,nainv
      WRITE(IUN66,9071)'HERITABILITY',i,hsq(i),nna(i)
      end do
      IF(nfix2>nfix1.and.nlev(nfix2)>0)THEN
         do i=1,nainv
         WRITE(IUN66,9071)'"M-SQUARED" VALUE',i,xmsq(i)
         end do
         IF(IOPCOV.EQ.2)THEN
            WRITE(IUN66,907)'COVARIANCE AS PROPORTION OF TOTAL',cam
            RR=XVEC(3)/DSQRT(XVEC(1)*XVEC(2))
            WRITE(IUN66,907)'CORRELATION BETWEEN ANIMAL EFFECTS',RR
         END IF
      END IF
      DO II=1,IOPRN1
      if(nlev(nfix+ii)>0)then
         do i=1,nainv
         WRITE(IUN66,9072)'"C-SQUARED" VALUE',ii,I,csq(ii,i)
         end do
      end if
      end do

!     write out short file with summary
      call write_iun19

      RETURN
902   FORMAT(1X,A,T50,'=',I15)
906   FORMAT(1X,A,T50,'=',G18.10)
9061  FORMAT(1X,A,i4,T50,'=',G18.10)
907   FORMAT(1X,A,T50,'=',F15.4)
9071  FORMAT(1X,A,I4,T50,'=',F15.4,i12)
 9072 FORMAT(1X,A,2I4,T50,'=',F15.4)
915   FORMAT(I4,G20.10,G16.8,4F10.6)
916   FORMAT(I4,G20.10,6G14.6/(24X,6G14.6))
5912  FORMAT(I5,4X,A,T26,2G20.10)

      contains

!     ====================
      subroutine estimates
!     ====================

      allocate(sigc(ioprn1+1),siga(nainv),hsq(nainv),sigp(nainv),
     &             xmsq(nainv),csq(ioprn1+1,nainv),stat=ii)


      ESQ=1.D0-xvec(1)-sum(xvec(nainv+1:nparm))
      sigp(1)=zige/esq
      HSQ(1)=XVEC(1)
      do i=1,nainv
      siga(i)=sigp(1)*xvec(i)
      end do
      m=nainv
      IF(nrand2>0)THEN
         XMSQ(1)=XVEC(nainv+1)
         sigm=xmsq(1)*sigp(1)
         IF(IOPCOV.EQ.2)then
            CAM=XVEC(nainv+2)
            sigam=cam*sigp(1)
            m=m+2
         else
            m=m+1
         end if
      END IF
      sigc=0.d0
      do ii=1,ioprn1
      if(nlev(nfix+ii).eq.0)cycle
      m=m+1
      CSQ(ii,1)=XVEC(m)
      sigc(ii)=csq(ii,1)*sigp(1)
      END DO

      if(nainv>1)then
         do i=2,nainv
         sigp(i)=siga(i)+sigm+sum(sigc)+zige
         hsq(i)=siga(i)/sigp(i)
         if(nrand2>0)xmsq(i)=sigm/sigp(i)
         do ii=1,ioprn1
         if(nlev(nfix+ii)>0)csq(ii,i)=sigc(ii)/sigp(i)
         end do
         end do
      end if
      return
      end subroutine estimates

!     ======================
      subroutine write_iun19
!     ======================

      iun19=19
      open(iun19,file='short.out',status='unknown',form='formatted',
     &                                               position='append')

      WRITE(IUN19,'(i10,t40,a)')nrec,'No. of records'
      WRITE(IUN19,'(i10,t40,a)')nanim,'No. of animals'
      do i=1,nfix1-nfix
      WRITE(IUN19,'(i10,t40,a,i3)')nlev(i+nfix),'No. of add. RE levels'
     &,                                                              i
      end do
      do i=1,nfix
      WRITE(IUN19,'(i10,t40,a,i3)')nlev(i),'No. of FE levels',i
      end do
      WRITE(IUN19,'(2g13.6,t40,2a)')YBAR(ntrait),YSDEV(ntrait),
     &                              'Mean & SD ',trait(ntrait)
      write(iun19,'(2i10,t40,a)')neqns,maxlnz,
     &                                 'No. of equations & MMM elements'
      write(iun19,'(g20.10,t40,a)')zige,'Residual variance'
      do i=1,nainv
      if(nainv>1)write(iun19,*)'type of animal no.',i,nna(i)
      write(iun19,'(g20.10,t40,a)')SIGP(i),'Phenotypic variance'
      write(iun19,'(f10.4,g20.10,t40,a)')hsq(i),siga(i),'Heritability'
      if(ioprn2.eq.1)then
         write(iun19,'(f10.4,g20.10,t40,a)')xmsq(i),sigm,
     &                                    '"Maternal" heritability etc.'
         if(iopcov.eq.2)then
            write(iun19,'(f10.4,g20.10,t40,a)')sigam,
     &                                        '"Direct-mat." covariance'
            write(iun19,'(f10.4,t40,a)')rr,'"Direct-mat." correlation'
         end if
      end if
      DO II=1,IOPRN1
      if(nlev(nfix+ii).eq.0)cycle
      write(iun19,'(f10.4,g20.10,t40,a,i2)')csq(ii,i),sigc(ii),
     &                                   '"c-squared" for add. RE',i
      end do
      end do ! nainv
      write(iun19,'(g20.10,t40,a)')xlike,'REML log likelihood'
      close(iun19)
      return
      end subroutine write_iun19

      END subroutine dfwrt1

C===========================================================================
      SUBROUTINE  DFWRH1 (ioprun)
C===========================================================================

      use names
      use means
      use levels
      use comments
      use units
      use numbers
      use today

      integer, intent(in) :: ioprun

      WRITE(IUN66,901)
      WRITE(IUN66,*)'                    PROGRAM " D F U N I" '
      WRITE(IUN66,*)' '
      WRITE(IUN66,*)'    ESTIMATE VARIANCE COMPONENTS FOR AN INDIVIDUAL'
     *,                                 ' ANIMAL MODEL'
      WRITE(IUN66,910)

      call today_is(iun66)
      IF(KLINES.GT.0)THEN
         WRITE(IUN66,*)'-----------------------------------'
         WRITE(IUN66,*)'DESCRIPTION OF DATA SET'
         WRITE(IUN66,*)'-----------------------------------'
         WRITE(IUN66,*)' '
         DO I=1,KLINES
         WRITE(IUN66,900)TEXT(I)
         END DO
         WRITE(IUN66,*)' '
      END IF
      IF(IOPRUN.GE.0)WRITE(IUN66,*)
     *     'ANALYSIS FOR TRAIT :',NTRAIT,'     ',TRAIT(NTRAIT)
      WRITE(IUN66,*)' '
      write(iun66,*)'Data file used      : "',trim(fdata),'"'
      write(iun66,*)'Pedigree file used  : "',trim(fped),'"'
      write(iun66,*)'Data directory      : "',trim(cwdir),'"'
      WRITE(IUN66,*)' '
      WRITE(IUN66,*)'----------------------------------'
      WRITE(IUN66,*)'MODEL OF ANALYSIS & DATA STRUCTURE'
      WRITE(IUN66,*)'----------------------------------'
      WRITE(IUN66,*)' '
      WRITE(IUN66,902)'ANALYSIS FITTING MODEL NO.',imodel
      WRITE(IUN66,*)' '
      WRITE(IUN66,902)'NO. OF RECORDS',nrec
      WRITE(IUN66,902)'NO. OF ANIMALS',NANIM
      if(nainv>1)then
          do i=1,nainv
          write(iun66,'(1X,A,i4,T50,a,I15)')'  ... "Type"',i,'=',nna(i)
          end do
      end if
      WRITE(IUN66,*)' '
      WRITE(IUN66,902)'NO. OF TRAITS',NQ
      DO I=1,NQ
      WRITE(IUN66,903)I,TRAIT(I),YBAR(I),YSDEV(I)
      end do
      WRITE(IUN66,*)' '
      WRITE(IUN66,902)'NO. OF FIXED EFFECTS',NFIX
      WRITE(IUN66,902)'... WITH TOTAL NO. OF LEVELS',nfl
      DO I=1,NFIX
      if(nsize1(i).eq.0)then
         WRITE(IUN66,904)I,FIXED(I),NLEV(I)
      else
         WRITE(IUN66,904)I,FIXED(I),NLEV(I),
     *                         'no. of size 1 subclasses =',nsize1(i)
      end if
      end do
      WRITE(IUN66,*)' '
      WRITE(IUN66,902)'NO. OF COVARIABLES ',NCOV
      if(ncov>0)WRITE(IUN66,902)
     &   '... WITH TOTAL NO. OF REGRESSION COEFFICIENTS',NFR
      DO I=1,NCOV
      WRITE(IUN66,905)I,COVAR(I),NPOW(I),CBAR(I),CSDEV(I)
      end do
      IF(NFIX1.GT.NFIX)THEN
         WRITE(IUN66,*)' '
         WRITE(IUN66,902)'NO. OF ADD. RANDOM EFFECTS',NFIX1-NFIX
         WRITE(IUN66,902)'... WITH TOTAL NO. OF LEVELS',NRAND1
         DO I=NFIX+1,NFIX1
         WRITE(IUN66,904)I,FIXED(I),NLEV(I)
         end do
      END IF
      IF(IOPRN2.EQ.1)THEN
        WRITE(IUN66,*)' '
        WRITE(IUN66,*)' SECOND RANDOM ANIMAL EFFECT :    ',FIXED(NFIX2)
        WRITE(IUN66,902)' ... WITH NO. OF LEVELS',NLEV(NFIX2)
      END IF
      WRITE(IUN66,*)' '
      WRITE(IUN66,902)'NO. OF EQUATIONS IN TOTAL',NEQNS
      CALL PEDSUM(IUN11,IUN66)

      RETURN

900   FORMAT(1X,A)
901   FORMAT(/80(1H*)/)
902   FORMAT(1X,A,T50,'=',I15)
903   FORMAT(I5,4X,A12,3X,'MEAN =',G15.6,4X,'SDEV =',G15.6)
904   FORMAT(I5,4X,A12,3X,'NO. OF LEVELS =',I6,2x,a,i5)
905   FORMAT(I5,4X,A12,3X,'ORDER FITTED =',I4,4X,'MEAN =',G15.6,2X,
     *                                            'SDEV =',G15.6)
910   FORMAT(/76(1H*),'KM**'/)
      END subroutine dfwrh1
