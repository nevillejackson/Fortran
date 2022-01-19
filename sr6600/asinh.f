      FUNCTION ASINH(X)                                                 ASINH002          
      XSQ=X*X                                                           ASINH003          
      IF(ABS(X)-.0637) 40,60,60                                         ASINH004          
40    ASINH=X*(1.-XSQ*(.16666666667-XSQ*(.075-XSQ*4.4642857143E-2)))    ASINH005          
      GOTO 100                                                          ASINH006          
60    IF(ABS(X)-.227E6) 70,90,90                                        ASINH007          
70    ASINH=SIGN(ALOG(ABS(X)+SQRT(1.+XSQ)),X)                           ASINH008          
      GOTO 100                                                          ASINH009          
90    ASINH=SIGN(ALOG(2.*ABS(X)),X)                                     ASINH010          
100   RETURN                                                            ASINH011          
      END                                                               ASINH012          
