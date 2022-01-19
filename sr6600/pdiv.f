      SUBROUTINE PDIV(A,N,B,M,Q,K,R,L)                                  PDIV0002          
      DIMENSION A(1),B(1),Q(1),R(1)                                     PDIV0003          
      I=1                                                               PDIV0004          
21    IF(I.GT.N) GOTO 29                                                PDIV0005          
      R(I)=A(I)                                                         PDIV0006          
      I=I+1                                                             PDIV0007          
      GOTO 21                                                           PDIV0008          
29    CONTINUE                                                          PDIV0009          
      L=N                                                               PDIV0010          
      IF(N-M) 60,80,80                                                  PDIV0011          
60    K=0                                                               PDIV0012          
      GOTO 170                                                          PDIV0013          
80    MM=M-1                                                            PDIV0014          
90    K=L-MM                                                            PDIV0015          
      Q(K)=R(L)/B(M)                                                    PDIV0016          
      I=1                                                               PDIV0017          
111   IF(I.GT.MM) GOTO 119                                              PDIV0018          
      KK=I+L-M                                                          PDIV0019          
      R(KK)=R(KK)-B(I)*Q(K)                                             PDIV0020          
      I=I+1                                                             PDIV0021          
      GOTO 111                                                          PDIV0022          
119   CONTINUE                                                          PDIV0023          
      L=L-1                                                             PDIV0024          
      IF(L-M) 160,90,90                                                 PDIV0025          
160   K=N-M+1                                                           PDIV0026          
170   RETURN                                                            PDIV0027          
      END                                                               PDIV0028          
