C     Last change:  KM    1 Jul 98    3:06 pm
C=======================================================================
      SUBROUTINE RVALUE(XX,XMIN,XMAX)
C=======================================================================

      REAL(8), INTENT(IN)   :: xmin,xmax
      REAL(8), INTENT(OUT)  :: xx
      CHARACTER A*20

3     READ(*,'(a)')A
      CALL CHKDIG(A,IOPT,XX,20,2)
      IF(XX.LT.XMIN .OR. XX.GT.XMAX)THEN
         WRITE(*,1)'INVALID VALUE GIVEN :',XX
         WRITE(*,1)'PERMISSIBLE RANGE IS',XMIN,'TO',XMAX
         WRITE(*,*)'YOU''VE GOT ANOTHER GO ...'
         WRITE(*,*)'VALUE ?   '
1        FORMAT(1X,A,G15.6,3X,A,G16.6)
         GO TO 3
      END IF
      RETURN
      END subroutine rvalue

C=======================================================================
      SUBROUTINE RVALFL (xx,xmin,xmax,iun)
C=======================================================================

      REAL(8), INTENT(IN)   :: xmin,xmax
      REAL(8), INTENT(OUT)  :: xx
      INTEGER, INTENT(IN)   :: iun
      CHARACTER(LEN=20)     :: A
3     if(iun.eq.0)then
         READ(*,10)A
      else
         READ(iun,10)A
      end if
10    FORMAT(A)
      CALL CHKDJG(A,IOPT,XX,20,2,iun)
      IF(XX.LT.XMIN .OR. XX.GT.XMAX)THEN
         WRITE(*,1)'INVALID VALUE GIVEN :',XX
         WRITE(*,1)'PERMISSIBLE RANGE IS',XMIN,'TO',XMAX
         WRITE(*,*)'YOU''VE GOT ANOTHER GO ...'
         WRITE(*,*)'VALUE ?   '
1        FORMAT(1X,A,G15.6,3X,A,G16.6)
         GO TO 3
      END IF
      RETURN
      END subroutine rvalfl

!=======================================================================
      SUBROUTINE RVLDEF(XX,XMIN,XMAX,XDEFLT)
!=======================================================================

      USE platform
      REAL(8), INTENT(IN)   :: xmin,xmax,xdeflt
      REAL(8), INTENT(OUT)  :: xx
      CHARACTER(LEN=20)     :: A='                    '
      INTEGER               :: iout, iopt

      WRITE(*,FMT5(ipltf))'<RETURN> FOR VALUE =',XDEFLT
      IOUT=0
3     READ(*,'(a)')A
      IF(a(1:2) .EQ. '  ')THEN
         XX=XDEFLT
      ELSE
         CALL CHKDIG(A,IOPT,XX,20,2)
      END IF
      IF(XX.LT.XMIN .OR. XX.GT.XMAX)THEN
         WRITE(*,1)'INVALID VALUE GIVEN :',XX
         WRITE(*,1)'PERMISSIBLE RANGE IS',XMIN,'TO',XMAX
         IOUT=IOUT+1
         IF(IOUT.LT.3)THEN
            WRITE(*,*)'YOU''VE GOT ANOTHER GO ...'
            WRITE(*,*)'VALUE ?   '
            GO TO 3
         ELSE
            STOP 'YOU''VE HAD IT, MATE !'
         END IF
      END IF 
      RETURN
1     FORMAT(1X,A,G15.6,3X,A,G16.6)
      END subroutine rvldef
