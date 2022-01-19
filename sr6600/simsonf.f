      FUNCTION SIMSONF(A,B,DELTA,FN)                                    SIMPS002          
      REAL K                                                            SIMPS003          
C     ADAPTIVE SIMPSON"S INTEGRATION.                                   SIMPS004          
C. H. J. JOHNSON,DIVISION OF APPLIED CHEM.,MELB. JULY , 1969            SIMPS005          
      X1=A $ X2=B $ K=X2-X1                                             SIMPS006          
      S1=FN(X1) $ S1=S1+FN(X2)                                          SIMPS007          
      S0=S1                                                             SIMPS008          
70    S2=0. $ H=0.5*K $ X=X1+H                                          SIMPS009          
100   S2=S2+FN(X)                                                       SIMPS010          
      X=X+K $ IF(.NOT.(X.GT.X2)) GOTO 100                               SIMPS011          
      S1=S1+4.*S2                                                       SIMPS012          
      IF(H*ABS((S1-S0-S0)/S1).LT.DELTA) GOTO 190                        SIMPS013          
      S0=S1 $ S1=S1-S2-S2 $ K=H $ GOTO 70                               SIMPS014          
190   SIMSONF=0.333333333*H*S1                                          SIMPS015          
      RETURN                                                            SIMPS016          
      END                                                               SIMPS017          
