C======================================================================
      SUBROUTINE CHKDIG(A,IOPT,XOPT,N,JJ)
C======================================================================

      integer, intent(in)          :: n, jj
      character(len=n), intent(inout) :: a
      real(8), intent(out)         :: xopt
      integer, intent(out)         :: iopt

      character(len=1), dimension(18) :: DIGIT =(/' ','0','1','2','3',
     &                                    '4','5','6','7','8','9','-',
     *                                       '.','+','E','e','D','d'/)
      character(len=15)               :: fmt

      IF(JJ.EQ.1)THEN
         LL=12
         WRITE(FMT,21)N
21       FORMAT('(BN,I',I2,')')
      ELSE
         LL=18
         WRITE(FMT,22)N
22       FORMAT('(BN,G',I2,'.0)')
      END IF
      II=0
      MM=0

11    IBLNK=0
      DO 9 I=1,N
      DO  J=1,LL
      IF(A(I:I).EQ.DIGIT(J))THEN
         IF(J.EQ.1)THEN
            IBLNK=IBLNK+1
         ELSE IF(J.GE.12)THEN
            IF( (MM.EQ.J) .OR. (J.GT.14.AND.MM.GT.14) )exit
            MM=J
         END IF
         GO TO 9
      END IF
      end do

      WRITE(*,*)'NON-DIGIT ENCOUNTERED : ',A,'   TRY AGAIN ...'
      IF(II.LT.2)THEN
         WRITE(*,*)'OPTION ?'
         READ(*,'(a)')A
         II=II+1
         GO TO 11
      ELSE
         STOP 'tried too often'
      END IF
9     CONTINUE

      IF(IBLNK.NE.N)THEN
         IF(JJ.EQ.1)THEN
            READ(A,FMT)IOPT      
         ELSE
            READ(A,FMT)XOPT      
         END IF
         RETURN
      ELSE
         WRITE(*,*)'OPTION MUST BE NON-BLANK - try again ...'
         READ(*,'(a)')A
         GO TO 11
      END IF
      END subroutine chkdig

C======================================================================
      SUBROUTINE CHKDJG(A,IOPT,XOPT,N,JJ,iun)
C======================================================================

      CHARACTER A*20,B*20,AA(20)*1,DIGIT(18)*1,FMT*15
      DOUBLE PRECISION XOPT
      EQUIVALENCE (B,AA(1))
      DATA DIGIT/' ','0','1','2','3','4','5','6','7','8','9','-',
     *           '.','+','E','e','D','d'/

      IF(N.GT.20)THEN
         WRITE(*,*)'SUBROUTINE "CHKDIG" : OPTION TOO LONG !'
         STOP 'RESET CHARACTER VARIABLES'
      END IF
      IF(JJ.EQ.1)THEN
         LL=12
         WRITE(FMT,21)N
21       FORMAT('(BN,I',I2,')')
      ELSE
         LL=18
         WRITE(FMT,22)N
22       FORMAT('(BN,G',I2,'.0)')
      END IF
      II=0
      MM=0
11    B=A
      IBLNK=0
      DO 9 I=1,N
      DO 8 J=1,LL
      IF(AA(I).EQ.DIGIT(J))THEN
         IF(J.EQ.1)THEN
            IBLNK=IBLNK+1
         ELSE IF(J.GE.12)THEN
            IF( (MM.EQ.J) .OR. (J.GT.14.AND.MM.GT.14) )GO TO 13
            MM=J
         END IF
         GO TO 9
      END IF
8     CONTINUE
13    WRITE(*,*)'NON-DIGIT ENCOUNTERED : ',A,'   TRY AGAIN ...'
      IF(II.LT.2)THEN
         if(iun.eq.0)then
            WRITE(*,*)'OPTION ?'
            READ(*,10)A
         else
            READ(iun,10)A
         end if
         II=II+1
         GO TO 11
      ELSE
         STOP '... THIS IS ENOUGH ... !!!'
      END IF
9     CONTINUE
      IF(IBLNK.NE.N)THEN
         IF(JJ.EQ.1)THEN
            READ(A,FMT)IOPT      
         ELSE
            READ(A,FMT)XOPT      
         END IF
         RETURN
      ELSE
         if(iun.eq.0)then
            WRITE(*,*)'OPTION MUST BE NON-BLANK - AGAIN ...'
            READ(*,10)A
         else
            READ(iun,10)A
         end if
         GO TO 11
      END IF
10    FORMAT(A10)
      END

