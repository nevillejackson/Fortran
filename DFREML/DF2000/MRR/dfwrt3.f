!===========================================================================
      SUBROUTINE  DFWRT3 (kparm,ioprun,xlike,fsstart)
!===========================================================================

      use parameters
      use sparse
      use names
      use units
      use likelihoods
      use numbers
      use like_components
      use eigen_decomp

!     arguments
      integer, intent(in)                    :: kparm,ioprun
      real(8), intent(in)                    :: xlike,fsstart

      real(8)                                :: xlstart,aic,bic,bicfac
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      IF(IOPRUN.LE.3)THEN
         WRITE(IUN66,908)'-----------------------------'
         WRITE(IUN66,*)  'RUN OPTIONS & CHARACTERISTICS'
         WRITE(IUN66,*)  '-----------------------------'
         WRITE(IUN66,*)' '
         WRITE(IUN66,902)'RUN OPTION CHOSEN',IOPRUN
         IF(FSSTART.NE.0)then
            xlstart=-0.5D0*FSSTART -DETL*KsFIT(1)
            IF(IOPCOV.EQ.1.OR.IOPCOV.EQ.2)xlstart=xlstart-DETL*KsFIT(2)
            WRITE(IUN66,906)'PREVIOUS MAX. LIKELIHOOD',xlstart
         end if
         WRITE(IUN66,*)'STARTING VALUES GIVEN :'
         DO I=1,kPARM
         WRITE(IUN66,'(I4,4X,A,2G20.10)')I,PARAM(I),START(I)
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
      if(ncall.eq.0.or.ioprun.gt.7)then
         REWIND(IUN44)
         READ(IUN44)DETL
      end if
      WRITE(IUN66,906)'WEIGHTED SUMS OF SQUARES'
      if(ioprun.eq.0)then
         WRITE(IUN66,906)'   ... TOTAL (Y''R**(-1)Y)',YRY
         WRITE(IUN66,906)'   ... ABSORBING ALL EFFECTS (Y''PY)',YPY
         WRITE(IUN66,906)'LOG DETERMINANT OF COEFFICIENT MATRIX',DET
      end if
      WRITE(IUN66,906)'LOG DETERMINANT OF RELATIONSHIP MATRIX',
     *                                                      DETL+DETL
      WRITE(IUN66,*)' '
      WRITE(IUN66,907)'LOG LIKELIHOOD (INCORPORATING LOG |NRM|)',xLIKE
      WRITE(IUN66,*)' '
      bicfac=nrec-nrnkx
      bicfac=dlog(bicfac)
      aic=-2.d0*xlike + 2.d0*kparm
      bic=-2.d0*xlike + bicfac * kparm
      write(iun66,907)'Akaike Information Criterion (AIC)',aic
      write(iun66,907)'Bayesian Information Criterion (BIC)',bic
      write(iun66,907)' ... penalty factor for no. parameters',bicfac
      write(iun66,*)'nrnkx=',nrnkx,' kparm =',kparm
      WRITE(IUN66,*)' '
      WRITE(IUN66,906)'OPERATIONAL ZERO - overall ',ZERO
      WRITE(IUN66,906)'OPERATIONAL ZERO - eigenvalues ',eigzer
      WRITE(IUN66,*)' '
      WRITE(IUN66,*)'Parameterisation used for maximisation :'
      if(kopt.eq.0)then
          write(iun66,*)kopt,' : Original scale '
      else if(kopt.eq.1 .or. kopt.eq.3)then
          write(iun66,*)kopt,' : Cholesky decomposition of matrices'
          if(kopt.eq.3)then
             write(iun66,*)'    setting diagonals to zero'
             write(iun66,*)'    order of fit for "SIG_A" =',kfteig(1)
             if(ioprn2.eq.1)
     &       write(iun66,*)'    order of fit for "SIG_M" =',kfteig(2)
             write(iun66,*)'    order of fit for "SIG_C" =',kfteig(4)
             if(ioprn3.eq.1)
     &       write(iun66,*)'    order of fit for "SIG_Q" =',kfteig(5)
             write(iun66,*)'    reduced no. of parameters =',kkparm
          end if
      else if(kopt.eq.2)then
          write(iun66,*)kopt,' : Cholesky decomp. + log (diagonals) '
      end if
      WRITE(IUN66,*)' '
      RETURN

902   FORMAT(1X,A,T50,'=',I20)
922   FORMAT(1X,A,T50,':',4X,A)
906   FORMAT(1X,A,T50,'=',G20.12)
907   FORMAT(1X,A,T50,'=',G23.15)
908   FORMAT(/1X,A)

      END subroutine dfwrt3











