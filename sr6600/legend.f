      REAL FUNCTION LEGEND(X,N)                                         LEGEN002          
C C1 CSIR LEGEND - LEGENDRE PLOYNOMIAL ORDER N, ARGUMENT X              LEGEN003          
C AUTHOR - J.J.RUSSELL, CSIRO DIVISION OF COMPUTING RESEARCH            LEGEN004          
C REVISED APRIL, 1970                                                   LEGEN005          
      IF(N-1) 30,50,70                                                  LEGEN006          
30    LEGEND=1.                                                         LEGEN007          
      GOTO 150                                                          LEGEN008          
50    LEGEND=X                                                          LEGEN009          
      GOTO 150                                                          LEGEN010          
70    P0=1.                                                             LEGEN011          
      P1=X                                                              LEGEN012          
      NN=N-1                                                            LEGEN013          
      NNN=1                                                             LEGEN014          
101   IF(NNN.GT.NN) GOTO 109                                            LEGEN015          
      PP1=X*P1                                                          LEGEN016          
      LEGEND=(FLOAT(NNN)*(PP1+PP1-P0)+PP1)/(FLOAT(NNN)+1.)              LEGEN017          
      P0=P1                                                             LEGEN018          
      P1=LEGEND                                                         LEGEN019          
      NNN=NNN+1                                                         LEGEN020          
      GOTO 101                                                          LEGEN021          
109   CONTINUE                                                          LEGEN022          
150   RETURN                                                            LEGEN023          
      END                                                               LEGEN024          
