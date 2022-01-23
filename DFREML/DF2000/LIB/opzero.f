C======================================================================
      SUBROUTINE OPZERO(ZERO,IOPT)
C======================================================================

C     PURPOSE : ROUTINE TO OBTAIN VALUE FOR OPERATIONAL ZERO
C               FROM STANDARD INPUT (TERMINAL)
C-------------------------------------------------------------KM--6/89--

      real(8), intent(inout) :: zero
      integer, intent(in)    :: iopt

      IF(IOPT.EQ.0)THEN
!         WRITE(*,*)' '
         WRITE(*,*)'SMALLEST VALUE TO BE TREATED DIFFERENT FROM ZERO ?'
         WRITE(*,*)'       5  ...  0.00001     '
         WRITE(*,*)'       6  ...  0.000001    '
         WRITE(*,*)'          ...          '
         WRITE(*,*)'       N  ...  10**(-N)'
         CALL OPTDEF(IIZERO,1,15,8)
         IF(IIZERO.GT.15)THEN
            WRITE(*,*)'VALUE OF N > 15 -  SET N=15 !'
            IIZERO=15
         END IF
         ZERO=10.D0**(-IIZERO)
         WRITE(*,1)'OPERATIONAL ZERO FOR THIS RUN SET TO',ZERO
         WRITE(*,*)' '

      ELSE if(iopt.eq.1)then
 !        WRITE(*,*)' '
         WRITE(*,1)'OPERATIONAL ZERO IN PREVIOUS RUN WAS',ZERO
         WRITE(*,*)'CHANGE IN OPERATIONAL ZERO REQUIRED ?'
         WRITE(*,*)'       0  ...  NO - USE SAME VALUE   '
         WRITE(*,*)'       N  ...  YES - NEW VALUE IS 10**(-N)   '
         CALL OPTDEF(IIZERO,0,15,0)
         IF(IIZERO.NE.0)THEN
            IF(IIZERO.GT.15)THEN
               WRITE(*,*)'VALUE OF N > 15 -  SET N=15 !'
               IIZERO=15
            END IF
            ZERO=10.D0**(-IIZERO)
            WRITE(*,1)'NEW OPERATIONAL ZERO IS',ZERO
         WRITE(*,*)' '
         END IF
1        FORMAT(1X,A,G13.4)

      else if(iopt.eq.2)then
!         write(*,*)' '
         write(*,*)'Operational zero for eigenvalues ?'
         WRITE(*,*)'       3  ...  0.001      '
         WRITE(*,*)'       4  ...  0.0001     '
         WRITE(*,*)'          ...             '
         WRITE(*,*)'       N  ...  10**(-N)   '
         CALL OPTDEF(JJZERO,1,10,6)
         ZERO=10.D0**(-JJZERO)
      end if
         return
      END subroutine opzero



