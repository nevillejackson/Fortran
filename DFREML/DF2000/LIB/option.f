!======================================================================
      SUBROUTINE OPTION(IOPT,IOMIN,IOMAX)
!======================================================================

!     PURPOSE : ROUTINE TO READ IN OPTION (NUMERICAL VALUE) AND
!               CHECK THAT IT IS IN THE CORRECT RANGE
!------------------------------------------------------------KM--6/89--

      INTEGER, INTENT(OUT) :: iopt
      INTEGER, INTENT(IN)  :: iomin,iomax
      CHARACTER(LEN=20)    :: A
      REAL(8)              :: XOPT

2     READ(*,'(a)')A
      CALL CHKDIG(A,IOPT,XOPT,20,1)
      IF( (IOPT.LT.IOMIN) .OR. (IOPT.GT.IOMAX) )THEN
         WRITE(*,1)'INVALID OPTION GIVEN - PERMISSIBLE RANGE IS',
     *             IOMIN,'TO',IOMAX
         WRITE(*,1)'VALUE SPECIFIED WAS',IOPT
         WRITE(*,*)'TRY AGAIN ...'
         WRITE(*,*)'OPTION ?'
         GO TO 2
1        FORMAT(1X,A,I9,3X,A,I12,' !!')
      END IF
      RETURN
      END subroutine option

!======================================================================
      SUBROUTINE OPTDEF(IOPT,IOMIN,IOMAX,IDEFLT)
!======================================================================

      use platform

      INTEGER, INTENT(OUT) :: iopt
      INTEGER, INTENT(IN)  :: iomin,iomax,ideflt
      CHARACTER(LEN=20)    :: A, BLNK='                    '
      REAL(8)              :: XOPT

      A=blnk
      if(ipltf.eq.1)then
         WRITE(*,'(1x,a,i12)')'<RETURN> FOR VALUE =',IDEFLT
      else
         WRITE(*,'(1x,a,i12/)')'<RETURN> FOR VALUE =',IDEFLT
      end if
11    READ(*,'(a)')A
      IF(A.EQ.BLNK .OR. (A(1:1).EQ.' '.AND.A(2:2).EQ.' ') )THEN
         IOPT=IDEFLT
      ELSE
         CALL CHKDIG(A,IOPT,XOPT,20,1)
      END IF
      IF( (IOPT.LT.IOMIN) .OR. (IOPT.GT.IOMAX) )THEN
         WRITE(*,1)'INVALID OPTION GIVEN - PERMISSIBLE RANGE IS',
     *             IOMIN,'TO',IOMAX
         WRITE(*,1)'VALUE SPECIFIED WAS',IOPT
         WRITE(*,*)'TRY AGAIN ...'
         WRITE(*,*)'OPTION ?'
         GO TO 11
1        FORMAT(1X,A,I5,3X,A,I6,' !!')
      END IF
      RETURN
      END subroutine optdef

C======================================================================
      SUBROUTINE OPTjON(IOPT,IOMIN,IOMAX,iun)
C======================================================================

      INTEGER, INTENT(OUT) :: iopt
      INTEGER, INTENT(IN)  :: iomin,iomax,iun
      CHARACTER(LEN=20)    :: A
      REAL(8)              :: XOPT

2     if(iun.eq.0)then
         READ(*,'(a)')A
      else 
         READ(iun,'(a)')A
      end if
      CALL CHKDIG(A,IOPT,XOPT,20,1)
      IF( (IOPT.LT.IOMIN) .OR. (IOPT.GT.IOMAX) )THEN
         WRITE(*,1)'INVALID OPTION GIVEN - PERMISSIBLE RANGE IS',
     *             IOMIN,'TO',IOMAX
         WRITE(*,1)'VALUE SPECIFIED WAS',IOPT
         WRITE(*,*)'TRY AGAIN ...'
         WRITE(*,*)'OPTION ?'
         GO TO 2
1        FORMAT(1X,A,I5,3X,A,I6,' !!')
      END IF
      RETURN
      END subroutine optjon

!======================================================================
      SUBROUTINE YESNO(IOPT)
!======================================================================

C     PURPOSE : ROUTINE TO INTERACTIVELY ACQUIRE OPTION FOR YES OR
C               NO AND CHECK FOR VALIDITY
C-------------------------------------------------------------KM-11/88-
      use platform

      integer, intent(out) :: iopt
      real(8)              :: xopt
      CHARACTER(len=10)    :: A

      WRITE(*,*)'        1  ...  YES       '
      if(ipltf.eq.1)then
         WRITE(*,*)'        0  ...  NO        '
      else
         WRITE(*,'(a/)')'        0  ...  NO        '
      end if
10    READ(*,'(a)')A
      CALL CHKDIG(A,IOPT,XOPT,5,1)
      IF(IOPT.NE.0.AND.IOPT.NE.1)THEN
         WRITE(*,*)'INVALID OPTION GIVEN - MUST BE 0 OR 1 !'
         WRITE(*,*)'HERE WE GO AGAIN ...'
         WRITE(*,*)'OPTION (0/1) ?'
         GO TO 10
      END IF
      RETURN
      END subroutine yesno

C======================================================================
      SUBROUTINE YNDEF(IOPT,IDEFLT)
C======================================================================

      use platform
      integer, intent(out) :: iopt
      integer, intent(in)  :: ideflt
      real(8)              :: xopt
      CHARACTER(len=10)    :: A

      WRITE(*,*)'        1  ...  YES       '
      WRITE(*,*)'        0  ...  NO        '
      if(ipltf.eq.1)then
         WRITE(*,*)'<RETURN> FOR OPTION = ',IDEFLT
      else
         WRITE(*,'(1x,a,i2/)')'<RETURN> FOR OPTION = ',IDEFLT
      end if
1     READ(*,'(a)')A
      IF(A(1:1).EQ.' ')THEN
         IOPT=IDEFLT
      ELSE
         CALL CHKDIG(A,IOPT,XOPT,5,1) 
      END IF
      IF(IOPT.NE.0.AND.IOPT.NE.1)THEN
         WRITE(*,*)'INVALID OPTION GIVEN - MUST BE 0 OR 1 !'
         WRITE(*,*)'HERE WE GO AGAIN ...'
         WRITE(*,*)'OPTION (0/1) ?'
         GO TO 1
      END IF
      RETURN
      END subroutine yndef











