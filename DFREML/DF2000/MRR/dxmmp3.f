!===========================================================================
      SUBROUTINE DxMMP3 (ipar,iopt)
!===========================================================================

      use params
      use units
      use sparse
      use xsprse
      use sigmas
      use numbers

      integer, intent(in) :: ipar,iopt

      integer             :: mm,nq2,ianim,janim,ii,jj0,il,nq20
      real(8)             :: aa,detl
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


      dia=0.d0
      xspars=0.d0

      if(iopt.eq.1)then

         MM=ksfit(1)+ksfit(2)
         NQ2=ksfit(1)
         IF(IOPCOV.eq.1.or.iopcov.eq.2)NQ2=NQ2+ksfit(2)
         nq20=mfitmx+mfitmx

         REWIND(IUN44)
         READ(IUN44)DETL

150      READ(IUN44,END=199)JANIM,IANIM,AA
         II=(IANIM-1)*MM+LIM3
         IF(IANIM.EQ.JANIM)THEN
            CALL DxPDIA (II,JJ0,NQ2,NQ20,ipar,AA,SIGAM)
         ELSE
            JJ0=(JANIM-1)*MM+LIM3
            CALL DxPBLK (II,JJ0,NQ2,NQ20,ipar,aa,SIGAM)
         END IF
         GO TO 150

      else if(iopt.eq.2 .and. iopcov.eq.3)then
C     ... V(M) = SIGM * I FOR MODEL 5 OR 9
         do ianim=1,nanim
         II=(IANIM-1)*MM+LIM3+ksfit(1)
         CALL DxPDII (II,JJ0,ksfit(2),mfitmx,ipar,AA,SIGM)
         end do

      else if(iopt.eq.2 .and. iopcov.eq.4)then
          REWIND(IUN45)
250       READ(IUN45,END=199)JANIM,IANIM,AA
          II=(IANIM-1)*MM+LIM3+ksfit(1)
          IF(IANIM.EQ.JANIM)THEN
             CALL DxPDIA (II,JJ0,ksfit(2),mfitmx,ipar,aa,SIGM)
          ELSE
             JJ0=(JANIM-1)*MM+LIM3+ksfit(1)
             CALL DxPBLK (II,JJ0,ksfit(2),mfitmx,ipar,aa,SIGM)
          END IF
          GO TO 250
    
      else if(iopt.eq.3)then
         II=LIM2
         DO  IL=1,NRAND1
         CALL DxPDII (II,JJ0,ksfit(4),mfitmx,ipar,aa,SIGC)
         end do

      else if(iopt.eq.4)then
         II=LIM3a
         DO  IL=1,NRAND3
         CALL DxPDII (II,JJ0,ksfit(5),mfitmx,ipar,aa,SIGQ)
         end do
      END IF
 199  return
      END subroutine dxmmp3












