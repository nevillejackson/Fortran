C=======================================================================
      SUBROUTINE KOUNT(NREC,NINT)
C=======================================================================

      integer, intent(in)    :: nint
      integer, intent(inout) :: nrec

      NREC=NREC+1
      NX=NREC/NINT
      MX=NX*NINT
      IF(MX.EQ.NREC)WRITE(*,*)'at',NREC
      RETURN
      END subroutine kount

