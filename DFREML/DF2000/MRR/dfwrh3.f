C============================================================================
      SUBROUTINE DFWRH3 (ioprun)
C============================================================================

      use names
      use comments
      use means
      use combinations
      use levels
      use numbers
      use like_components
      use ages
      use units
      use residuals

!     arguments
      integer, intent(in)                 :: ioprun

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      m=max0(mage,mobs)

      WRITE(IUN66,901)
      WRITE(IUN66,*)'                    PROGRAM " D x M R R " '
      WRITE(IUN66,*)' '
      WRITE(IUN66,*)'    ESTIMATE COVARIANCE FUNCTIONS FOR AN'
     *,                                 ' ANIMAL MODEL'
      WRITE(IUN66,*)' '
      WRITE(IUN66,914)
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
      write(iun66,*)'Data file used      : "',trim(fdata),'"'
      write(iun66,*)'Pedigree file used  : "',trim(fped),'"'
      write(iun66,*)'Data directory      : "',trim(cwdir),'"'
      WRITE(IUN66,*)' '
      WRITE(IUN66,*)'----------------------------------'
      WRITE(IUN66,*)'MODEL OF ANALYSIS & DATA STRUCTURE'
      WRITE(IUN66,*)'----------------------------------'
      WRITE(IUN66,*)' '
      WRITE(IUN66,902)'ANALYSIS UNDER MODEL NUMBER',imodel
      WRITE(*,*)'ANALYSIS UNDER MODEL NUMBER',imodel
      WRITE(IUN66,*)' '
      WRITE(IUN66,902)'NO. OF RECORDS',nrec
      WRITE(IUN66,902)'NO. OF ANIMALS',nanim
      WRITE(IUN66,902)'... WITH RECORDS',ndata
      WRITE(IUN66,*)' '

      WRITE(IUN66,902)'NO. OF TRAITS IN MULTIVARIATE ANALYSIS',NQ
      if(nq.eq.1)then
         WRITE(IUN66,902)'NO. OF COMBINATIONS OF TRAITS RECORDED',
     &                                                       NCOMB
         if(ncomb.le.max_comb)then
            DO I=1,NCOMB
            WRITE(IUN66,913)I,NNCOM(I),MMARK(:nage(1),I)
            end do
         end if
      else
         WRITE(IUN66,908)
     &              'NO. OF ANIMALS WITH RECORDS FOR PAIRS OF TRAITS'
         DO I=1,NQ
         DO J=I+1,NQ
         WRITE(*,1791)I,J,TRAIT(I),TRAIT(J),
     &                                    IABS(NBOTH(IHMIJ(I,J,NQ)))
         end do
         end do
1791     FORMAT(2I3,2(3X,A),I10)
      end if
      IF(irpmod.EQ.2)THEN
        WRITE(IUN66,908)'FIT EQUIVALENT MODEL WITH CORRELATED RESIDUALS'
        WRITE(IUN66,*)  'TO ACCOUNT FOR REPEATED RECORDS PER TRAIT'
      END IF

      WRITE(IUN66,908)'NO. OF ANIMALS WITH N RECORDS'
      WRITE(*,908)'NO. OF ANIMALS WITH N RECORDS'
      DO I=1,m
      IF(MMR(I).GT.0)WRITE(IUN66,*)'N=',I,'  NO. OF ANIMALS =',MMR(I)
      IF(MMR(I).GT.0)WRITE(*,*)'N=',I,'  NO. OF ANIMALS =',MMR(I)
      END DO

      DO IQ=1,NQ
      WRITE(IUN66,*)' '
      if(nq>1)WRITE(IUN66,903)'TRAIT NO.',IQ,TRAIT(IQ)
      WRITE(IUN66,902)'NO. OF RECORDS',sum( NNREC(:,iq) )
      WRITE(IUN66,902)'MAX. NO. OF RECORDS PER ANIMAL',MXOBS(IQ)
      WRITE(IUN66,906)'MEAN',YBAR(IQ)
      WRITE(IUN66,906)'STANDARD DEVIATION',SDEV(IQ)
      WRITE(IUN66,906)'COEFFICIENT OF VARIATION',CVAR(IQ)
      WRITE(IUN66,916)'PHENOTYPIC RANGE',YMIN(IQ),YMAX(IQ)
      WRITE(IUN66,916)'STANDARDIZED RANGE',SMIN(IQ),SMAX(IQ)
      WRITE(IUN66,902)'NO. OF FIXED EFFECTS FITTED',NFIX(IQ)
      IF(NFL(IQ).GT.0)THEN
         DO I=1,NFIX(IQ)
         WRITE(IUN66,904)I,FIXED(I,IQ),NLEV(I,IQ)
         end do
         WRITE(IUN66,902)'TOTAL NO. OF LEVELS FOR THIS TRAIT',NFL(IQ)
      END IF
      WRITE(IUN66,902)'NO. OF COVARIABLES FITTED',NCOV(IQ)
      IF(NFR(IQ).GT.0)THEN
         DO I=1,NCOV(IQ)
         WRITE(IUN66,905)I,COVAR(I,IQ),NPOW(I,IQ),CBAR(I,IQ),CSDEV(I,IQ)
         end do
         WRITE(IUN66,902)'NO. OF REGRESSION COEFFICIENTS FOR THIS TRAIT'
     *,               NFR(IQ)
      END IF
      mm2=0
      IF(ioprn1.eq.1 .and.ieqmod.eq.0)THEN
         WRITE(IUN66,*)' '
         WRITE(IUN66,*)'PERMANENT ENVIRON. EFFECT FITTED :  "',
     *                                 FIXED(NFIX(IQ)+1,IQ),'"'
         WRITE(IUN66,902)'... WITH NO. OF LEVELS',NLEV(NFIX(IQ)+1,IQ)
         mm2=1
      END IF
      IF(ioprn3.eq.1)THEN
         WRITE(IUN66,*)' '
         WRITE(IUN66,*)'ADDITIONAL RANDOM EFFECT FITTED :  "',
     *                        FIXED(NFIX(IQ)+mm2+1,IQ),'"'
         WRITE(IUN66,902)'... WITH NO. OF LEVELS',
     *                                     NLEV(NFIX(IQ)+mm2+1,IQ)
      END IF
      IF(NFIX2(IQ).GT.NFIX1(IQ))THEN
        WRITE(IUN66,*)' '
        WRITE(IUN66,*)'SECOND RANDOM ANIMAL EFFECT FITTED :   "',
     *                 FIXED(NFIX2(IQ),IQ),'"'
        WRITE(IUN66,902)' ... WITH NO. OF LEVELS',NLEV(NFIX2(IQ),IQ)
      END IF
      end do

      WRITE(IUN66,*)' '
      WRITE(IUN66,902)'NO. OF FIXED EFFECTS LEVELS IN TOTAL',nflm
      WRITE(IUN66,902)'NO. OF REGRESSION COEFFICIENTS IN TOTAL',nfrm
      IF(IOPRUN.GT.0 .AND. IOPRUN.NE.6)WRITE(IUN66,902)
     *          'RANK OF COEFFICIENT MATRIX FOR FIXED EFFECTS',NRNKX
      WRITE(IUN66,902)'NO. OF ANIMAL EFFECTS IN TOTAL',
     *                   (NQ+Nq222)*nanim
      WRITE(IUN66,902)'NO. OF ADDITIONAL RANDOM EFFECTS LEVELS IN TOTAL'
     *,                nq111*nrand1+nq333*nrand3
      WRITE(IUN66,902)'NO. OF EQUATIONS IN TOTAL',neqns
      WRITE(*,902)'NO. OF EQUATIONS IN TOTAL',neqns
      CALL PEDSUM(IUN11,IUN66)
      RETURN

900   FORMAT(1X,A)
901   FORMAT(/72('*')/)
902   FORMAT(1X,A,T50,'=',I20)
903   FORMAT(1X,A,I4,6X,A)
904   FORMAT(I4,3X,'"',A12,'"',4X,'NO. OF LEVELS =',I6)
905   FORMAT(I4,3X,'"',A12,'"',4X,'ORDER =',I3,4X,'MEAN =',
     *                              F12.4,3X,'SD =',F12.4)
906   FORMAT(1X,A,T50,'=',G20.12)
908   FORMAT(/1X,A)
913   FORMAT(I4,3X,'NO. ANIMALS',I8,4X,'COMBINATION :',(T45,16I2))
914   FORMAT(/68('*'),'KM**'/)
916   FORMAT(1X,A,T50,'=',2G16.6)

      END subroutine dfwrh3









