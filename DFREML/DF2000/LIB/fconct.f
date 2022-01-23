C=====================================================================
      SUBROUTINE FCONCT(IUN,FSTAND,FFORM,FSTAT)
C=====================================================================

C     PURPOSE : ROUTINE TO CHECK WHETHER A LOGICAL UNIT IS PRE-
C               CONNECTED TO A FILE;
C               IF THE FILE IS NOT CONNECTED, IT WILL ATTEMPT TO
C               OPEN A FILE OF STANDARD NAME, FAILING THAT IT
C               WILL PROMPT FOR THE NAME OF THE FILE & OPEN IT.

C     KARIN MEYER, FEBRUARY 1988
C----------------------------------------------------------------------

      INTEGER, intent(in)            :: iun
      CHARACTER(len=25), intent(in)  :: FSTAND,FSTAT,FFORM
      LOGICAL                        :: LOPEN,LEXIST,LNAME
      CHARACTER(len=25)              :: FNAME

      INQUIRE(UNIT=IUN,OPENED=LOPEN,NAME=FNAME,NAMED=LNAME,
     *        EXIST=LEXIST)

      WRITE(*,'(a,i4,2a)')'LOGICAL UNIT NO.  =',IUN,'  ',FSTAND

      IF(FSTAT(1:3).EQ.'OLD'.OR.FSTAT(1:3).EQ.'old')THEN !INPUT FILE MUST EXIST
         IF(LOPEN.AND.LEXIST.AND.LNAME)THEN
C           FILE IS OPENED, EXISTS AND IS NAMED
            WRITE(*,1)'  FILE DEFINITION HAS BEEN SET UP BY USER'
            WRITE(*,1)'  FILE =  ',FNAME
1           FORMAT(1X,A,A,A)
         ELSE
            INQUIRE(FILE=FSTAND,EXIST=LEXIST)
            IF(LEXIST)THEN
               OPEN(IUN,FILE=FSTAND,FORM=FFORM,STATUS=FSTAT)
!               WRITE(*,1)'  "STANDARD" FILE EXISTS & HAS BEEN OPENED'
!               WRITE(*,1)'  FILE =  ',FSTAND
            ELSE
               WRITE(*,1)'  NO FILE DEFINED & "STANDARD" FILE',
     *                      ' DOES NOT EXIST !'
               WRITE(*,*)'GIVE NAME OF FILE TO BE OPENED FOR',IUN
               READ 2,FNAME
2              FORMAT(A)
               INQUIRE(FILE=FNAME,EXIST=LEXIST)
               IF(LEXIST)THEN
                  OPEN(IUN,FILE=FNAME,FORM=FFORM,STATUS=FSTAT)
                  WRITE(*,1)'  FILE SPECIFIED EXISTS & HAS BEEN OPENED'
                  WRITE(*,1)'  FILE =  ',FNAME
               ELSE
                  WRITE(*,1)'  FILE SPECIFIED DOES NOT EXIST EITHER !'
                  WRITE(*,1)'  FILE =  ',FNAME
                  STOP 'CHECK FILES & TRY AGAIN'
               END IF
            END IF
         END IF

      ELSE
C        OUTPUT FILE, ASSUME STATUS IS UNKNOWN (I.E. NO CHECKS FOR
C        STATUS NEW; IF ENCOUNTERED FOR EXISTING FILE PROGRAM WILL CRASH
         IF(LOPEN)THEN
C           FILE IS OPENED
            WRITE(*,1)'  FILE DEFINITION HAS BEEN SET UP BY USER'
            IF(LNAME)WRITE(*,1)'  FILE =  ',FNAME
         ELSE
            OPEN(IUN,FILE=FSTAND,FORM=FFORM,STATUS=FSTAT)
!            WRITE(*,1)'  "STANDARD" FILE HAS BEEN OPENED'
!            WRITE(*,1)'  FILE =  ',FSTAND
         END IF
      END IF

      RETURN
      END subroutine fconct















