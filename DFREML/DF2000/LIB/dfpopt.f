!===========================================================================
      SUBROUTINE DFPOPT(N,COVAR)
!===========================================================================

!     read in option for order of fit of covariables
      USE platform

      INTEGER, INTENT(OUT)          :: n
      CHARACTER(LEN=12), INTENT(IN) :: COVAR

      WRITE(*,*)'NO. OF REGRESSION COEFFICIENTS FOR  ',COVAR,' ?'
      WRITE(*,FMT9(1))'1   ...  LINEAR'
      WRITE(*,FMT9(1))'2   ...  LINEAR & QUADRATIC'
      WRITE(*,FMT9(1))'3   ...  LINEAR & QUADRATIC & CUBIC'
      WRITE(*,fmt9(1))'     .          ....                          '
      WRITE(*,FMT9(ipltf))'N   ...  LINEAR TO POWER N'
      CALL OPTION(N,1,19)

      RETURN
      END subroutine dfpopt
