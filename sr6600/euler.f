      SUBROUTINE EULER(SUM,N,K,E,Z,SETTERM)                             EULER002          
      DIMENSION A(100)                                                  EULER003          
      J=-1 $ N=0 $ K=1                                                  EULER004          
      CALL SETTERM(0,T,Z)                                               EULER005          
      A(1)=T $ SUM=.5*T                                                 EULER006          
80    N=N+1                                                             EULER007          
      CALL SETTERM(N,T,Z)                                               EULER008          
      X=T                                                               EULER009          
      I=1                                                               EULER010          
111   IF(I.GT.K) GOTO 119                                               EULER011          
      Y=A(I) $ A(I)=X                                                   EULER012          
      X=.5*(X+Y)                                                        EULER013          
      I=I+1                                                             EULER014          
      GOTO 111                                                          EULER015          
119   CONTINUE                                                          EULER016          
      IF(ABS(X).GT.ABS(A(I))) GOTO 190                                  EULER017          
      K=K+1                                                             EULER018          
      A(K)=X $ X=.5*X                                                   EULER019          
190   SUM=SUM+X                                                         EULER020          
      IF(ABS(X).GT.E) GOTO 80                                           EULER021          
      IF(J.EQ.N-1) GOTO 240                                             EULER022          
      J=N $ GOTO 80                                                     EULER023          
240   CONTINUE                                                          EULER024          
      RETURN                                                            EULER025          
      END                                                               EULER026          
