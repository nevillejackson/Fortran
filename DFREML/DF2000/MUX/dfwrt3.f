C============================================================================
      SUBROUTINE  DFWRT3 (kparm,ioprun, xlike,fsstart,sstart)
C============================================================================

      use params
      use sparse
      use names
      use units
      use likelihoods
      use numbers
      use like_components

!     arguments
      integer, intent(in)                    :: kparm,ioprun
      real(8), intent(in)                    :: xlike,fsstart
      real(8), dimension(mxparm), intent(in) :: sstart
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      IF(IOPRUN.LE.3)THEN
         WRITE(IUN66,908)'-----------------------------'
         WRITE(IUN66,*)  'RUN OPTIONS & CHARACTERISTICS'
         WRITE(IUN66,*)  '-----------------------------'
         WRITE(IUN66,*)' '
         WRITE(IUN66,902)'RUN OPTION CHOSEN',IOPRUN
         IF(FSSTART.NE.0)WRITE(IUN66,906)'PREVIOUS MAX. LIKELIHOOD',
     *                                                  -0.5D0*FSSTART
         WRITE(IUN66,*)'STARTING VALUES GIVEN :'
         DO I=1,kPARM
         WRITE(IUN66,2412)I,PARAM(I),SSTART(I)
         END DO
         WRITE(IUN66,*)' '
         WRITE(IUN66,902)'MAX. NUMBER OF ITERATES ALLOWED',MROUND
         WRITE(IUN66,*)' '
         WRITE(IUN66,902)'NO. OF LIKELIHOODS EVALUATED',NCALL
         WRITE(IUN66,902)'NO. OF ITERATES CARRIED OUT',NROUND
         WRITE(IUN66,922)'STORAGE SCHEME USED','COMPRESSED'
         WRITE(IUN66,922)'RE-ORDERING STRATEGY','MINIMUM DEGREE'
         WRITE(IUN66,922)'COMPUTING PROCEDURE','FACTORISATION'
         WRITE(IUN66,902)'NO. OF ROWS IN P.D. SUBMATRIX',NSROW
         WRITE(IUN66,902)'NO. OF SUBSCRIPTS USED IN STORAGE ',MAXSUB
         WRITE(IUN66,902)'NO. OF NON-ZERO ELEMENTS IN FACT. ', MAXLNZ
      END IF

      WRITE(IUN66,908)'---------------------------------'
      WRITE(IUN66,*)  'LOG LIKELIHOOD & ITS CONSTITUENTS'
      WRITE(IUN66,*)  '---------------------------------'
      WRITE(IUN66,*)' '
      IF(IOPRUN.LE.3 .AND. NCALL.GT.0)THEN
         WRITE(IUN66,906)'WEIGHTED SUMS OF SQUARES'
         WRITE(IUN66,906)'   ... TOTAL (Y''R**(-1)Y)',YRY
         WRITE(IUN66,906)'   ... ABSORBING ALL EFFECTS (Y''PY)',YPY
         WRITE(IUN66,906)'LOG DETERMINANT OF COEFFICIENT MATRIX',DET
         WRITE(IUN66,906)'LOG DETERMINANT OF RELATIONSHIP MATRIX',
     *                                                      DETL+DETL
         WRITE(IUN66,*)' '
      END IF
      WRITE(IUN66,907)'LOG LIKELIHOOD INCORPORATING LOG |NRM|',XLIKE
      WRITE(IUN66,*)' '
      WRITE(IUN66,906)'OPERATIONAL ZERO - overall ',ZERO
      WRITE(IUN66,906)'OPERATIONAL ZERO - eigenvalues ',eigzer
      RETURN
C...........................................................................
902   FORMAT(1X,A,T50,'=',I20)
922   FORMAT(1X,A,T50,':',4X,A)
906   FORMAT(1X,A,T50,'=',G20.12)
907   FORMAT(1X,A,T50,'=',G23.15)
908   FORMAT(/1X,A)
911   FORMAT(I8,4G16.7)
912   FORMAT(I8,4F16.4)
919   FORMAT(G18.10,(1X,T20,7G12.5))
2412  FORMAT(I4,4X,A,2G20.10)
9452  FORMAT(/I8,4x,'MAX. NO. OF REPEATED RECORDS     =',I12/
     *          12X,'TOTAL RESIDUAL VARIANCE          =',G17.7)
9453  FORMAT(   12X,'PERMANENT ENVIRONMENTAL VARIANCE =',G17.7/
     *          12X,'TEMPORARY ENVIRONMENTAL VARIANCE =',G17.7)
9541  FORMAT(I8,I6,2F10.4)

      END subroutine dfwrt3





