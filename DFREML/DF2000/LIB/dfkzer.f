C=============================================================================
      SUBROUTINE DFKZER(NZERO,IRZERO)
C=============================================================================

      DIMENSION KRZERO(1000),IRZERO(NZERO)
     
      IF(NZERO.LE.1)RETURN   ! no sorting required
      IF(NZERO.GT.1000)STOP 'ROUTINE "DFKZER"'

C     SORT ROW NO.S IN ASCENDING ORDER, ELIMINATING DOUBLES
      NR=0
      DO 513 J=1,NZERO
      II= IRZERO(J)
      DO 514 K=1,NR
      IF(II.EQ.KRZERO(K))THEN
         GO TO 513
      ELSE IF(KRZERO(K).GT.II)THEN
         NR=NR+1
         DO L=NR,K+1,-1
         KRZERO(L)=KRZERO(L-1)
         end do
         KRZERO(K)=II
         GO TO 513
      END IF
514   CONTINUE
      NR=NR+1
      KRZERO(NR)=II
513   CONTINUE

      IF(NR.NE.NZERO)THEN
         WRITE(*,*)'ROWS TO BE ZEROED OUT HAVE BEEN DUPLICATED !'
         WRITE(*,*)'NEW NO. OF DEPENDENCIES IS :',NR
         NZERO=NR
      END IF
      WRITE(*,*)' '
      WRITE(*,*)'NO. OF EFFECTS SET TO ZERO IN TOTAL =',NZERO
      DO 25 I=1,NZERO
      IRZERO(I)=KRZERO(I)
25    WRITE(*,1)I,' ... EQUATION NO. :',KRZERO(I)
      RETURN
1     FORMAT(I5,2X,A,I7)
      END

