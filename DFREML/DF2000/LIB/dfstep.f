C=========================================================================
         SUBROUTINE DFSTEP(FSTEP,X1,X2)
C=========================================================================

         real(8), intent(out) :: fstep
         real(8), intent(in) ::  x1,x2
         real(8) :: xx

         IF(X1.LE.0)WRITE(*,9)'0  ...  SPECIFIY INDIVIDUALLY '
         WRITE(*,9)'1  ...  10 % OF STARTING VALUE '
         WRITE(*,9)'2  ...  20 % '
         WRITE(*,9)'3  ...  30 % '
         WRITE(*,9)'4  ...  40 % '
         CALL RVALUE(XX,X1,X2)
         FSTEP=XX*0.1D0
9        FORMAT(8X,A)
         RETURN
         END subroutine dfstep

