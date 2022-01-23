!===========================================================================
      SUBROUTINE  DFMMP3 (detl)
!===========================================================================

      use params
      use units
      use parmap
      use sigmas
      use numbers

!     arguments
      real(8), intent(out) :: detl

!     local variables
      integer              :: mm,nq2,jj0,ianim,janim,ii,ndom,il
      real(8)              :: aa
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if(iopmod.ne.5 .and. iopmod. ne.6 )stop

      if(nanim.eq.0)go to 599
      REWIND(IUN44)
      READ(IUN44)DETL
      MM=ksfit(1)+ksfit(2)
      NQ2=ksfit(1)
      IF(IOPCOV.eq.1.or.iopcov.eq.2)NQ2=NQ2+ksfit(2)
      nq20=mfitmx+mfitmx

50    READ(IUN44,END=99)JANIM,IANIM,AA
      II=(IANIM-1)*MM+LIM3
      IF(IANIM.EQ.JANIM)THEN
         CALL DxPDIA (II,JJ0,NQ2,NQ20,0,AA,SIGAM)
         IF(IOPCOV.EQ.3)then
C        ... V(M) = SIGM * I FOR MODEL 5 OR 9
             CALL DxPDII (II,JJ0,ksfit(2),mfitmx,0,AA,SIGM)
         END IF
      ELSE
         JJ0=(JANIM-1)*MM+LIM3
         CALL DxPBLK (II,JJ0,NQ2,NQ20,0,AA,SIGAM)
      END IF
      GO TO 50

C     ---------------------------------------------------------
C     BLOCKS FOR SECOND ANIMAL EFFECT WITH ARBITRARY COVARIANCE
C     ----------------------------------------------------------

99    IF (IOPCOV.EQ.4)THEN
          REWIND(IUN45)
          NDOM=0
500       READ(IUN45,END=599)JANIM,IANIM,AA
          NDOM=NDOM+1
          II=(IANIM-1)*MM+LIM3+ksfit(1)
          IF(IANIM.EQ.JANIM)THEN
             CALL DxPDIA (II,JJ0,ksfit(2),mfitmx,0,aa,SIGM)
          ELSE
             JJ0=(JANIM-1)*MM+LIM3+ksfit(1)
             CALL DxPBLK (II,JJ0,ksfit(2),mfitmx,0,aa,SIGM)
          END IF
          GO TO 500
      END IF

C     ----------------------------------------------
C     COVARIANCE BLOCKS FOR ADDITIONAL RANDOM EFFECT
C     ----------------------------------------------

599   II=LIM2
      IF(IOPRN1.EQ.1 .and. ieqmod.eq.0)THEN
         DO IL=1,NRAND1
         CALL DxPDII (II,JJ0,ksfit(4),mfitmx,0,aa,SIGC)
         end do
      END IF

      IF(IOPRN3.EQ.1)THEN
         DO IL=1,NRAND3
         CALL DxPDII (II,JJ0,ksfit(5),mfitmx,0,aa,SIGQ)
         end do
      END IF

      RETURN
      END subroutine dfmmp3

