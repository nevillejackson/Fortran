C==========================================================================   
      SUBROUTINE DFCONV(XCONV,IIC)
C==========================================================================   

      DOUBLE PRECISION XCONV

      WRITE(*,*)'     1  ...  0.1      '
      WRITE(*,*)'     2  ...  0.01     '
      WRITE(*,*)'        ...           '
      WRITE(*,*)'     N  ...  10**(-N) '
      CALL OPTDEF(ICONV,1,16,IIC)
      XCONV=10.D0**(-ICONV)
      RETURN

C==========================================================================   
      ENTRY DFCONW(XCONV,IIC)
C==========================================================================   

      WRITE(*,*)'  '
      WRITE(*,*)'* * QUADRATIC APPROXIMATION OF LIKELIHOOD * *'
      WRITE(*,*)'CONVERGENCE CRIT. : MAX. CHANGE IN PARAMETER ?'
      WRITE(*,*)'GIVE EXPONENT "N" FOR 10**(-N), FOR EXAMPLE '
      WRITE(*,*)'        3  ...  0.001         '
      WRITE(*,*)'        4  ...  0.0001        '
      WRITE(*,*)'        5  ...  0.00001       '
      WRITE(*,*)'        6  ...  0.000001      '
      WRITE(*,*)'        7  ...  0.0000001     '
      CALL OPTDEF(IEXP,0,16,IIC)
      XCONV=10.D0**(-IEXP)
      RETURN
      END

