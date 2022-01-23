C=============================================================DFREML/LIB======
      SUBROUTINE DFWARN(II)
C=============================================================================

c     purpose : to write out warning message for little tested options
c---------------------------------------------------------------------km-----

      IF(II.EQ.1)THEN
      WRITE(*,*)' '
      WRITE(*,*)'BEWARE : THIS OPTION HAS NOT BEEN TESTED VERY WELL !'
      WRITE(*,*)'          ... ANYTHING COULD HAPPEN ...'
      WRITE(*,*)' '
      ELSE IF(II.EQ.2)THEN
         WRITE(*,*)' '
      WRITE(*,*)'DFREML FOR THIS MODEL/OPTION HAS NOT BEEN TESTED ...'
         WRITE(*,*)' '
         WRITE(*,*)'DO YOU REALLY WANT TO CARRY ON ?'
         CALL YNDEF(JJ,0)
         IF(JJ.EQ.0)STOP
         WRITE(*,*)' '
      END IF
      RETURN
      END

C=============================================================DFREML/LIB======
      SUBROUTINE DFWORN (Iorder)
C=============================================================================

c     alert to missing re-ordering step

      if(iorder.eq.0)then
         write(*,*)'A re-ordering step has not been performed '
         write(*,*)'Carry on on any-way ?'
         call yndef(ii,0)
         if(ii.eq.0)stop 're-order & try again'
      end if
      return
      end


