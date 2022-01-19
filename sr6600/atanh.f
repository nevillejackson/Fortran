      FUNCTION ATANH(X)                                                 ATANH002          
20    FORMAT(8H ARTANH ,8HARG GT 1)                                     ATANH003          
      IF(ABS(X)-1.) 60,40,40                                            ATANH004          
40    WRITE(61,20)                                                      ATANH005          
      GOTO 110                                                          ATANH006          
60    IF(ABS(X)-.0289) 70,100,100                                       ATANH007          
70    XSQ=X*X                                                           ATANH008          
      ATANH=X*(1.+XSQ*(.33333333333+XSQ*.2))                            ATANH009          
      GOTO 110                                                          ATANH010          
100   ATANH=.5*ALOG((1.+X)/(1.-X))                                      ATANH011          
110   RETURN                                                            ATANH012          
      END                                                               ATANH013          
