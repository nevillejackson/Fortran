      FUNCTION COSH(X)                                                  COSH0002          
      IF(ABS(X)-.66E-5) 30,30,50                                        COSH0003          
30    COSH=1.                                                           COSH0004          
40    GOTO 80                                                           COSH0005          
50    COSH=EXP(ABS(X))/2.                                               COSH0006          
      IF(ABS(X)-12.5) 70,40,40                                          COSH0007          
70    COSH=COSH+.25/COSH                                                COSH0008          
80    RETURN                                                            COSH0009          
      END                                                               COSH0010          
