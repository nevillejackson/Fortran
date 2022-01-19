      SUBROUTINE PMULT(A,N,B,M,P,L)                                     PMULT002          
      DIMENSION A(1),B(1),P(1)                                          PMULT003          
      L=M+N-1                                                           PMULT004          
      I=1                                                               PMULT005          
31    IF(I.GT.L) GOTO 39                                                PMULT006          
      P(I)=0.0                                                          PMULT007          
      I=I+1                                                             PMULT008          
      GOTO 31                                                           PMULT009          
39    CONTINUE                                                          PMULT010          
      I=1                                                               PMULT011          
51    IF(I.GT.N) GOTO 59                                                PMULT012          
      J=1                                                               PMULT013          
61    IF(J.GT.M) GOTO 69                                                PMULT014          
      K=I+J-1                                                           PMULT015          
      P(K)=P(K)+A(I)*B(J)                                               PMULT016          
      J=J+1                                                             PMULT017          
      GOTO 61                                                           PMULT018          
69    CONTINUE                                                          PMULT019          
      I=I+1                                                             PMULT020          
      GOTO 51                                                           PMULT021          
59    CONTINUE                                                          PMULT022          
      RETURN                                                            PMULT023          
      END                                                               PMULT024          
