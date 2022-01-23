C===========================================================================
      SUBROUTINE      DFMMP3 (detl)
C===========================================================================

      use params
      use units
      use parmap
      use sigmas
      use numbers
      
!     arguments
      real(8), intent(out) :: detl

!     local variables
      integer :: nq2,
     &           jj0,ianim,janim,ii,il
      real(8) :: aa
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      REWIND(IUN44)
      READ(IUN44)DETL

      MM=NQ+NQ222
      NQ2=NQ
      IF(IOPCOV.eq.1.or.iopcov.eq.2)NQ2=NQ2+NQ
      nq20=nq+nq

50    READ(IUN44,END=99)JANIM,IANIM,AA
      II=(IANIM-1)*MM+LIM3
      IF(IANIM.EQ.JANIM)THEN
         CALL DxPDIA (II,JJ0,NQ2,NQ20,0,AA,SIGAM)
         IF(IOPCOV.EQ.3)then
C        ... V(M) = SIGM * I FOR MODEL 5 OR 9
             CALL DxPDII (II,JJ0,NQ,NQ,0,AA,SIGM)
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
500       READ(IUN45,END=599)JANIM,IANIM,AA
          II=(IANIM-1)*MM+LIM3+NQ
          IF(IANIM.EQ.JANIM)THEN
             CALL DxPDIA (II,JJ0,NQ,NQ,0,aa,SIGM)
          ELSE
             JJ0=(JANIM-1)*MM+LIM3+NQ
             CALL DxPBLK (II,JJ0,NQ,NQ,0,aa,SIGM)
          END IF
          GO TO 500
      END IF

C     ----------------------------------------------
C     COVARIANCE BLOCKS FOR ADDITIONAL RANDOM EFFECT
C     ----------------------------------------------

599   IF(IOPRN1.EQ.1)THEN
         II=LIM2
         DO IL=1,NRAND1
         CALL DxPDII (II,JJ0,NQ,NQ,0,aa,SIGC)
         end do
         if(ioprn3.eq.1)then
            ii=lim3a
            do il=1,nrand3
            CALL DxPDII (II,JJ0,NQ,NQ,0,aa,SIGQ)
            end do
         end if
      END IF
      RETURN
      END subroutine dfmmp3

















