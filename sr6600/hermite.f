      FUNCTION HERMITE(X,N)                                             HERMI002          
      IF(N-1) 30,50,70                                                  HERMI003          
30    HERMITE=1.                                                        HERMI004          
      GOTO 140                                                          HERMI005          
50    HERMITE=2.*X                                                      HERMI006          
      GOTO 140                                                          HERMI007          
70    H0=1.                                                             HERMI008          
      H1=2.*X                                                           HERMI009          
      NN=N-1                                                            HERMI010          
      NNN=1                                                             HERMI011          
101   IF(NNN.GT.NN) GOTO 109                                            HERMI012          
      HERMITE=2.*(X*H1-FLOAT(NNN)*H0)                                   HERMI013          
      H0=H1                                                             HERMI014          
      H1=HERMITE                                                        HERMI015          
      NNN=NNN+1                                                         HERMI016          
      GOTO 101                                                          HERMI017          
109   CONTINUE                                                          HERMI018          
140   RETURN                                                            HERMI019          
      END                                                               HERMI020          
