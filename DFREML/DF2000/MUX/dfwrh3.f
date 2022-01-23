!============================================================================
      SUBROUTINE DFWRH3 (ioprun)
!============================================================================

      use params
      use names
      use comments
      use means
      use combinations
      use levels
      use numbers
      use like_components
      use units

!     arguments
      integer, intent(in)    :: ioprun

!     local variables
      integer, dimension(nq) :: nnrec
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C     COMBINATIONS OF TRAITS & NO. OF RECORDS PER TRAIT
      nnrec=0
      DO I=1,NCOMB
      NNREC=NNREC+NNCOM(I)*mmark(:nq,i)
      end do

      nboth=0
      DO I=1,NCOMB
      IJ=0
      DO J=1,NQ
      DO K=J,NQ
      IJ=IJ+1
      IF( (MMARK(J,I).GT.0) .AND. (MMARK(K,I).GT.0) )NBOTH(IJ)=
     *                       NBOTH(IJ)+NNCOM(I)
      end do
      end do
      end do

      WRITE(IUN66,901)
      WRITE(IUN66,*)'                    PROGRAM " D x M u X " '
      WRITE(IUN66,*)' '
      WRITE(IUN66,*)'    ESTIMATE COVARIANCES AND FUNCTIONS FOR AN'
     *,                                 ' ANIMAL MODEL'
      WRITE(IUN66,*)' '
      WRITE(IUN66,914)
      WRITE(IUN66,910)
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
      write(iun66,*)'Data file used      : ',fdata
      write(iun66,*)'Pedigree file used  : ',fped
      write(iun66,*)'Data directory      : ',cwdir
      WRITE(IUN66,*)' '
      WRITE(IUN66,*)'----------------------------------'
      WRITE(IUN66,*)'MODEL OF ANALYSIS & DATA STRUCTURE'
      WRITE(IUN66,*)'----------------------------------'
      WRITE(IUN66,*)' '
      WRITE(IUN66,902)'ANALYSIS UNDER MODEL NUMBER',NOSVEC(17)
      WRITE(IUN66,*)' '
      WRITE(IUN66,902)'NO. OF RECORDS',NOSVEC(1)
      WRITE(IUN66,902)'NO. OF ANIMALS',NOSVEC(6)
      WRITE(IUN66,902)'... WITH RECORDS',NOSVEC(2)
      WRITE(IUN66,*)' '

      WRITE(IUN66,902)'NO. OF TRAITS IN MULTIVARIATE ANALYSIS',NQ
      WRITE(IUN66,902)'NO. OF COMBINATIONS OF TRAITS RECORDED',NCOMB
      DO I=1,NCOMB
      WRITE(IUN66,913)I,NNCOM(I),MMARK(:nq,I)
      end do
      WRITE(IUN66,908)'NO. OF ANIMALS WITH RECORDS FOR PAIRS OF TRAITS'
      DO  I=1,NQ
      DO  J=I+1,NQ
      WRITE(IUN66,1791)I,J,TRAIT(I),TRAIT(J),IABS(NBOTH(IHMIJ(I,J,NQ)))
      end do
      end do
1791  FORMAT(2I3,2(3X,A),I10)
      IF(NOSVEC(9).EQ.2)THEN
        WRITE(IUN66,908)'FIT EQUIVALENT MODEL WITH CORRELATED RESIDUALS'
        WRITE(IUN66,*)  'TO ACCOUNT FOR REPEATED RECORDS PER TRAIT'
      END IF

      DO IQ=1,NQ
      WRITE(IUN66,*)' '
      WRITE(IUN66,903)'TRAIT NO.',IQ,TRAIT(IQ)
      WRITE(IUN66,902)'NO. OF RECORDS',NNREC(IQ)
      WRITE(IUN66,902)'MAX. NO. OF RECORDS PER ANIMAL',MXOBS(IQ)
      WRITE(IUN66,906)'MEAN',YBAR(IQ)
      WRITE(IUN66,906)'STANDARD DEVIATION',SDEV(IQ)
      WRITE(IUN66,906)'COEFFICIENT OF VARIATION',CVAR(IQ)
      WRITE(IUN66,916)'PHENOTYPIC RANGE',YMIN(IQ),YMAX(IQ)
      WRITE(IUN66,916)'STANDARDIZED RANGE',SMIN(IQ),SMAX(IQ)
      WRITE(IUN66,902)'NO. OF FIXED EFFECTS FITTED',NFIX(IQ)
      IF(NFL(IQ).GT.0)THEN
         DO  I=1,NFIX(IQ)
         WRITE(IUN66,904)I,FIXED(I,IQ),NLEV(I,IQ)
         end do
         WRITE(IUN66,902)'TOTAL NO. OF LEVELS FOR THIS TRAIT',NFL(IQ)
      END IF
      WRITE(IUN66,902)'NO. OF COVARIABLES FITTED',NCOV(IQ)
      IF(NFR(IQ).GT.0)THEN
         DO I=1,NCOV(IQ)
         WRITE(IUN66,905)I,COVAR(I,IQ),NPOW(I,IQ),CBAR(I,IQ),CSDEV(I,IQ)
         WRITE(IUN66,902)'NO. OF REGRESSION COEFFICIENTS FOR THIS TRAIT'
     *,               NFR(IQ)
         end do
      END IF
      IF(NFIX1(IQ).GT.NFIX(IQ))THEN
         WRITE(IUN66,*)' '
         WRITE(IUN66,*)'ADDITIONAL RANDOM EFFECT FITTED :  "',
     *                                 FIXED(NFIX(IQ)+1,IQ),'"'
         WRITE(IUN66,902)'... WITH NO. OF LEVELS',NLEV(NFIX(IQ)+1,IQ)
         IF(NFIX3(IQ).GT.NFIX1(IQ))THEN
            WRITE(IUN66,*)' '
            WRITE(IUN66,*)'2nd ADD. RANDOM EFFECT FITTED :  "',
     *                                  FIXED(NFIX3(IQ),IQ),'"'
            WRITE(IUN66,902)'... WITH NO. OF LEVELS',NLEV(NFIX3(IQ),IQ)
         END IF
      END IF
      IF(NFIX2(IQ).GT.NFIX3(IQ))THEN
        WRITE(IUN66,*)' '
        WRITE(IUN66,*)'SECOND RANDOM ANIMAL EFFECT FITTED :   "',
     *                 FIXED(NFIX2(IQ),IQ),'"'
        WRITE(IUN66,902)' ... WITH NO. OF LEVELS',NLEV(NFIX2(IQ),IQ)
      END IF
      end do
      WRITE(IUN66,*)' '
      WRITE(IUN66,902)'NO. OF FIXED EFFECTS LEVELS IN TOTAL',NOSVEC(7)
      WRITE(IUN66,902)'NO. OF REGRESSION COEFFICIENTS IN TOTAL',
     *                 NOSVEC(8)
      WRITE(IUN66,902)'NO. OF ANIMAL EFFECTS IN TOTAL',
     *                   (NQ+NOSVEC(5))*NOSVEC(6)
      WRITE(IUN66,902)'NO. OF ADDITIONAL RANDOM EFFECTS LEVELS IN TOTAL'
     *,                nrand1+nrand3
      WRITE(IUN66,902)'NO. OF EQUATIONS IN TOTAL',NOSVEC(13)
      CALL PEDSUM(IUN11,IUN66)
      RETURN

900   FORMAT(1X,A)
901   FORMAT(/80('*')/)
902   FORMAT(1X,A,T50,'=',I20)
903   FORMAT(1X,A,I4,6X,A)
904   FORMAT(I4,3X,'"',A12,'"',4X,'NO. OF LEVELS =',I6)
905   FORMAT(I4,3X,'"',A12,'"',4X,'ORDER =',I3,4X,'MEAN =',
     *                              F12.4,3X,'SD =',F12.4)
906   FORMAT(1X,A,T50,'=',G20.12)
908   FORMAT(/1X,A)
910   FORMAT(/40('*')/10X,A/40('*')/)
913   FORMAT(I4,3X,'NO. ANIMALS',I8,4X,'COMBINATION :',(T45,11I3))
914   FORMAT(/76('*'),'KM**'/)
916   FORMAT(1X,A,T50,'=',2G16.6)

      END subroutine dfwrh3

