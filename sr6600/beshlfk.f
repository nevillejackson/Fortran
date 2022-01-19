      SUBROUTINE BESHLFK(NU,Z,RK)                                       BESHL002          
      REAL NU                                                           BESHL003          
      R=ABS(NU)                                                         BESHL004          
      IF(R) 70,40,70                                                    BESHL005          
40    WRITE(61,50)                                                      BESHL006          
50    FORMAT(8H NU=0 IS,8H UNDEFIN,3HED.)                               BESHL007          
      GOTO 180                                                          BESHL008          
70    IF(.NOT.(R.EQ..5)) GOTO 100                                       BESHL009          
      RK=SQRT(1.570796327/Z)*EXP(-Z)                                    BESHL010          
      GOTO 180                                                          BESHL011          
100   D=SQRT(1.570796327/Z)*EXP(-Z)                                     BESHL012          
      B=D                                                               BESHL013          
      C=.5                                                              BESHL014          
120   RK=B+(2E0*C/Z)*D                                                  BESHL015          
      B=D                                                               BESHL016          
      D=RK                                                              BESHL017          
      C=C+1E0                                                           BESHL018          
      IF(.NOT.(C.EQ.R)) GOTO 120                                        BESHL019          
180   RETURN                                                            BESHL020          
      END                                                               BESHL021          
