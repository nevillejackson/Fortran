      FUNCTION ACOSH(X)                                                 ACOSH002          
20    FORMAT(8H ARCOSH ,8HARG LESS,7H THAN 1)                           ACOSH003          
      IF(X-1.) 40,60,60                                                 ACOSH004          
40    WRITE(61,20)                                                      ACOSH005          
      GOTO 150                                                          ACOSH006          
60    IF(X-1.001) 70,110,110                                            ACOSH007          
70    W=SQRT((X-1.)/2.)                                                 ACOSH008          
      WSQ=W*W                                                           ACOSH009          
      ACOSH=W*(2.-WSQ*(.33333333333-.15*WSQ))                           ACOSH010          
      GOTO 150                                                          ACOSH011          
110   IF(X-.227E6) 120,140,140                                          ACOSH012          
120   ACOSH=ALOG(X+SQRT((X-1.)*(X+1.)))                                 ACOSH013          
      GOTO 150                                                          ACOSH014          
140   ACOSH=ALOG(2.*X)                                                  ACOSH015          
150   RETURN                                                            ACOSH016          
      END                                                               ACOSH017          
