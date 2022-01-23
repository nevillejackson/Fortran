C======================================================================
      SUBROUTINE CHKLEV(N,MAXN,PNAME,PP,IPP)
C======================================================================

C     PURPOSE : ROUTINE TO CHECK THAT PROGRAM DIMENSIONS ARE NOT
C               EXCEEDED BY CURRENT MODEL OF ANALYSIS

C     PARAMETERS : N      = NUMBER REQUIRED
C                  MAXN   = PARAMETER VALUE
C                  PNAME  = NAME OF PARAMETER
C                  PP     = COMMENT TO BE PRINTED
C                  IPP    = NO. OF CHARACTERS IN PP
C----------------------------------------------------------KM--6/89----

      integer, intent(in)                         :: n,maxn,ipp
      character(len=6), intent(in)                :: pname
      CHARACTER(len=ipp),intent(in)               :: pp

      IF(N.GT.MAXN .OR. N.LT.1)THEN
         WRITE(*,1)pp(1:IPP),' ','!','!'
         WRITE(*,2)'CURRENT MAXIMUM  =',MAXN
         WRITE(*,2)'NUMBER REQUIRED  =',N
         WRITE(*,3)PNAME
1        FORMAT(1X,'DIMENSION EXCEEDED : TOO MANY ',a,3a1)
2        FORMAT(11X,A,I10)
3        FORMAT(11X,'RESET PARAMETER  "',A,'"  ! ! !')
         STOP '"CHKLEV" '
      END IF
      RETURN
      END subroutine chklev

C======================================================================
      SUBROUTINE CHKLEW(ind,N,MAXN,PNAME,PP,IPP)
C======================================================================

C     PURPOSE : ROUTINE TO CHECK THAT PROGRAM DIMENSIONS ARE NOT
C               EXCEEDED BY CURRENT MODEL OF ANALYSIS

C     PARAMETERS : N      = NUMBER REQUIRED
C                  MAXN   = PARAMETER VALUE
C                  PNAME  = NAME OF PARAMETER
C                  PP     = COMMENT TO BE PRINTED
C                  IPP    = NO. OF CHARACTERS IN PP
C----------------------------------------------------------KM--6/89----

      CHARACTER PNAME*6,PP*40

      IF(N.GT.MAXN .OR. N.LT.1)THEN
         WRITE(*,1)pp(1:IPP),' ','!','!'
         WRITE(*,2)'CURRENT MAXIMUM  =',MAXN
         WRITE(*,2)'NUMBER REQUIRED  =',N
         WRITE(*,3)PNAME
1        FORMAT(1X,'DIMENSION EXCEEDED : TOO MANY ',a,3a1)
2        FORMAT(11X,A,I10)
3        FORMAT(11X,'RESET PARAMETER  "',A,'"  ! ! !')
         ind=99
      END IF
      RETURN
      END subroutine chklew

