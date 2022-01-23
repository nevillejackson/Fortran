C     Last change:  KM   29 Jun 98    1:36 pm
C===========================================================================
      SUBROUTINE DFANIM(NANIM,MAXAN,IUN44)
C===========================================================================

      REAL(8) :: DETL

      READ(IUN44)DETL,IOWRT,KANIM,NAFIX,NARND,NAMOD
899   WRITE(*,*)'NO. OF LEVELS OF MAIN RANDOM EFFECT (ANIMALS) ?    '
      CALL OPTDEF(NANIM,1,MAXAN,KANIM)
      CALL CHKLEV(NANIM,MAXAN,'MAXAN ','ANIMALS',7)
      IF(KANIM.NE.NANIM)THEN
         WRITE(*,*)'DISCREPANCY WITH NO.S FROM NRM INVERSE FOUND !'
         WRITE(*,*)'TOTAL NO. OF ANIMALS THERE WAS =',KANIM
         WRITE(*,*)'NO. GIVEN NOW WAS              =',NANIM
         WRITE(*,*)'=> 2 POSSIBILITIES :                '
         WRITE(*,9)' *  WRONG UNIT "44" CONNECTED          -> STOP RUN'
         WRITE(*,9)' *  INCORRECT NO. OF ANIMALS SPECIFIED -> TRY AGAIN'
         WRITE(*,*)'  '
         GO TO 899
      END IF
      WRITE(*,*)' '
      RETURN
9     FORMAT(8X,A,I10)
      END


