      FUNCTION SINH(X)                                                  SINH0002          
      IF(ABS(X)-.068) 30,30,60                                          SINH0003          
30    XSQ=X*X                                                           SINH0004          
      SINH=X*(1.+XSQ*(.16666666667+XSQ*.8333333333E-2))                 SINH0005          
      GOTO 100                                                          SINH0006          
60    XSQ=EXP(ABS(X))/2E0                                               SINH0007          
      IF(ABS(X)-12.5) 80,80,90                                          SINH0008          
80    XSQ=XSQ-.25/XSQ                                                   SINH0009          
90    SINH=SIGN(XSQ,X)                                                  SINH0010          
100   RETURN                                                            SINH0011          
      END                                                               SINH0012          
