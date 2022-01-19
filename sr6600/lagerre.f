      REAL FUNCTION LAGERRE(X,N)                                        LAGUE002          
C C1 CSIR LAGUERRE - LAGUERRE POLYNOMIAL ORDER N, ARGUMENT X            LAGUE003          
C AUTHOR - J.J.RUSSELL, CSIRO DIVISION OF COMPUTING RESEARCH            LAGUE004          
C REVISED APRIL, 1970                                                   LAGUE005          
      IF(N-1) 30,50,70                                                  LAGUE006          
30    LAGERRE=1.                                                        LAGUE007          
      GOTO 140                                                          LAGUE008          
50    LAGERRE=1.-X                                                      LAGUE009          
      GOTO 140                                                          LAGUE010          
70    RL0=1.                                                            LAGUE011          
      COMPX=1.-X                                                        LAGUE012          
      RL1=COMPX                                                         LAGUE013          
      NN=N-1                                                            LAGUE014          
      NNN=1                                                             LAGUE015          
101   IF(NNN.GT.NN) GOTO 109                                            LAGUE016          
      LAGERRE=(COMPX+FLOAT(NNN)+FLOAT(NNN))*RL1-FLOAT(NNN)*FLOAT(NNN)*  LAGUE017          
     .RL0                                                               LAGUE018          
      RL0=RL1                                                           LAGUE019          
      RL1=LAGERRE                                                       LAGUE020          
      NNN=NNN+1                                                         LAGUE021          
      GOTO 101                                                          LAGUE022          
109   CONTINUE                                                          LAGUE023          
140   RETURN                                                            LAGUE024          
      END                                                               LAGUE025          
