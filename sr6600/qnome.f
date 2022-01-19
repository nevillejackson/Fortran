      FUNCTION QNOME(R)                                                 QNOME002          
      DIMENSION A(13)                                                   QNOME003          
      A(1)=2.                                                           QNOME004          
      A(2)=15.                                                          QNOME005          
      A(3)=150.                                                         QNOME006          
      A(4)=1707.                                                        QNOME007          
      A(5)=20910.                                                       QNOME008          
      A(6)=268616.                                                      QNOME009          
      A(7)=3567400.                                                     QNOME010          
      A(8)=48555069.                                                    QNOME011          
      A(9)=673458874.                                                   QNOME012          
      A(10)=9481557398.                                                 QNOME013          
      A(11)=13511952997.E+1                                             QNOME014          
      A(12)=19449975396.E+2                                             QNOME015          
      A(13)=28235172754.E+3                                             QNOME016          
      ENTRY QNOMEF                                                      QNOME017          
      IF(ABS(R).GT.1.) GOTO 300                                         QNOME018          
      IF(.NOT.(R.EQ.1E0)) GOTO 200                                      QNOME019          
      QNOME=1.                                                          QNOME020          
      GOTO 340                                                          QNOME021          
200   S=SQRT(SQRT(1.-R*R))                                              QNOME022          
      E=.5*(1.-S)/(1.+S)                                                QNOME023          
      QNOME=E                                                           QNOME024          
      U=E*E                                                             QNOME025          
      U=U*U                                                             QNOME026          
      I=0                                                               QNOME027          
250   I=I+1                                                             QNOME028          
      E=U*E                                                             QNOME029          
      S=E*A(I)                                                          QNOME030          
      QNOME=QNOME+S                                                     QNOME031          
      IF(S.LT.1.E-9.OR.I.EQ.13) GOTO 330                                QNOME032          
      GOTO 250                                                          QNOME033          
300   WRITE(61,310) R                                                   QNOME034          
310   FORMAT(8H ERROR I,8HN QNOME,,2HR=,E18.10)                         QNOME035          
      QNOME=1.E307                                                      QNOME036          
330   CONTINUE                                                          QNOME037          
340   RETURN                                                            QNOME038          
      END                                                               QNOME039          
