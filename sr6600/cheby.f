      FUNCTION CHEBY(X,N)                                               CHEBY002          
      IF(N-1) 30,50,70                                                  CHEBY003          
30    CHEBY=1.                                                          CHEBY004          
      GOTO 150                                                          CHEBY005          
50    CHEBY=X                                                           CHEBY006          
      GOTO 150                                                          CHEBY007          
70    T0=1.                                                             CHEBY008          
      T1=X                                                              CHEBY009          
      XDUBL=2.*X                                                        CHEBY010          
      NN=N-1                                                            CHEBY011          
      NNN=1                                                             CHEBY012          
111   IF(NNN.GT.NN) GOTO 119                                            CHEBY013          
      CHEBY=XDUBL*T1-T0                                                 CHEBY014          
      T0=T1                                                             CHEBY015          
      T1=CHEBY                                                          CHEBY016          
      NNN=NNN+1                                                         CHEBY017          
      GOTO 111                                                          CHEBY018          
119   CONTINUE                                                          CHEBY019          
150   RETURN                                                            CHEBY020          
      END                                                               CHEBY021          
