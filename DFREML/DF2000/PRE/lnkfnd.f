C====================================================================
      SUBROUTINE LNKFND(IDS,NDS)
C====================================================================

      use parameters
      use list_of_ids

!     arguments
      integer, intent(in)  :: ids
      integer, intent(out) :: nds

      IF(IDS.EQ.0)THEN
         NDS=0
         RETURN
      END IF

      IROW=IDS/IFAC+1
      IF(IROW.GT.MROW2)then
         print *, '"LNKFND" : MAX. ROW NO. EXCEEDED !'
         print *,ids,ifac,irow,idmax,ubound(kfirst,1)
         stop
      end if

      DO I=KFIRST(IROW),KFIRST(IROW+1)-1
      IF(IDS.EQ.IDVEC(I))THEN
         NDS=I
         RETURN
      END IF
      end do

      WRITE(*,*)'ID=',IDS,' IROW',IROW
      STOP 'ERROR "LNKFND" : ID NOT FOUND !!'

      END subroutine lnkfnd
