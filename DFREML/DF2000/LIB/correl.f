C============================================================================
      double precision function correl (var1,var2,cov,zero,errval)
C============================================================================

c     purpose : to calculate correlation, checking for positive variances

c     parameters :
c             var1   : variance of trait 1; double precision, input
c             var2   : variance of trait 2; double precision, input
c             cov    : covariance; double precision, input
c             zero   : operational zero; double precision, input
c             errval : correl is set to this value if one of the variances
c                      is less than the operational zero
c-------------------------------------------------------------------km--------

      real(8), intent(in) :: var1,var2,zero,errval,cov

      if(var1.lt.zero.or.var2.lt.zero)then
         correl=errval
      else
         print *,var1,var2,cov
         correl=cov/dsqrt(var1*var2)
         print *,'corr',correl
      end if
      return
      end function correl

C=======================================================================
      DOUBLE PRECISION FUNCTION CORRSE(X,Y,Z,VX,VY,VZ,VXY,VXZ,VYZ)
C=======================================================================

C     PURPOSE : FUNCCTION TO CALCULATE THE APPROXIMATE SAMPLING ERROR
C               OF AN ESTIMATE OF A CORRELATION,
C                      R = X / SQRT ( Y * Z)
C               USING FORMULA OBTAINED BY STATISTICAL DIFFERENTIATION


      real(8), intent(in) ::  X   ! COVARIANCE COMPONENT ESTIMATE
      real(8), intent(in) ::  Y   ! VARIANCE ESTIMATE FOR TRAIT 1
      real(8), intent(in) ::  Z   ! VARIANCE ESTIMATE FOR TRAIT 2
      real(8), intent(in) ::  VX  ! SAMPLING VARIANCE OF X
      real(8), intent(in) ::  VY  ! ...................  Y
      real(8), intent(in) ::  VZ  ! ...................  Z
      real(8), intent(in) ::  VXY ! ........ COVARIANCE BETWEEN X AND Y
      real(8), intent(in) ::  VXZ ! ........................... X ... Z
      real(8), intent(in) ::  VYZ ! ........................... Y ... Z

C----------------------------------------------KARIN MEYER, OCTOBER 1985------

      real(8) ::  ZERO=1.D-12
      real(8) :: sqx,sqy,sqz,xx,xy,xz,xxy,xxz,xyz,ss,tt

      SQX=X*X
      SQY=Y*Y
      SQZ=Z*Z
      XX=VX*SQY*SQZ
      XY=VY*SQX*SQZ
      XZ=VZ*SQX*SQY
      XXY=VXY*X*Y*SQZ
      XXZ=VXZ*X*Z*SQY
      XYZ=VYZ*Y*Z*SQX
      SS=4.D0*XX+XY+XZ-4.D0*(XXY+XXZ)+2.D0*XYZ
      TT=4.D0*Y*Z*SQY*SQZ

      IF(TT.LE.ZERO.OR.SS.LE.ZERO)THEN
         CORRSE=-99.D0
         WRITE(*,*)'"CORRSE" : CANNOT CALCULATE APPROX. SE !'
         WRITE(*,*)'  ...  X   = ',X
         WRITE(*,*)'  ...  Y   = ',Y
         WRITE(*,*)'  ...  Z   = ',Z
         WRITE(*,*)'  ...  VX  = ',VX
         WRITE(*,*)'  ...  VY  = ',VY
         WRITE(*,*)'  ...  VZ  = ',VZ
         WRITE(*,*)'  ...  VXY = ',VXY
         WRITE(*,*)'  ...  VXZ = ',VXZ
         WRITE(*,*)'  ...  VYZ = ',VYZ
         WRITE(*,*)'  ...  TT  = ',TT
         WRITE(*,*)'  ...  SS  = ',SS
         WRITE(*,*)'S.E. SET TO',CORRSE
         RETURN
      ELSE
         CORRSE=DSQRT(SS/TT)
      END IF
      RETURN
      END function corrse



