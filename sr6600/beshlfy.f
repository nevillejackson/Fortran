      SUBROUTINE BESHLFY(NU,Z,Y)                                        BESHL002          
      REAL NU                                                           BESHL003          
C     USE WITH SUBROUTINE BESHLFJ.                                      BESHL004          
      R=NU                                                              BESHL005          
      IF(NU) 70,40,80                                                   BESHL006          
40    WRITE(61,50)                                                      BESHL007          
50    FORMAT(8H ZERO VA,8HLUES OF ,8HNU NOT D,7HEFINED.)                BESHL008          
      GOTO 160                                                          BESHL009          
70    R=ABS(R)                                                          BESHL010          
80    CALL BESHLFJ(R,Z,RJ)                                              BESHL011          
      X=RJ                                                              BESHL012          
      CALL BESHLFJ(-R,Z,RJ)                                             BESHL013          
      S=R*3.1415926536                                                  BESHL014          
      IF(NU) 150,40,130                                                 BESHL015          
130   Y=(X*COS(S)-RJ)/SIN(S)                                            BESHL016          
      GOTO 160                                                          BESHL017          
150   Y=(X-RJ*COS(S))/SIN(S)                                            BESHL018          
160   CONTINUE                                                          BESHL019          
      RETURN                                                            BESHL020          
      END                                                               BESHL021          
