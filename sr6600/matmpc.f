      SUBROUTINE MATMPC(A,B,C,M,N,L)                                    MATMP002          
      DIMENSION A(30,30),B(30,30),C(30,30)                              MATMP003          
C     C MUST BE A DIFFERENT ARRAY TO A,B.  FORMS  C=A*B                 MATMP004          
      I=1                                                               MATMP005          
21    IF(I.GT.L) GOTO 29                                                MATMP006          
      J=1                                                               MATMP007          
31    IF(J.GT.M) GOTO 39                                                MATMP008          
      C(J,I)=0.0                                                        MATMP009          
      K=1                                                               MATMP010          
51    IF(K.GT.N) GOTO 59                                                MATMP011          
      C(J,I)=C(J,I)+A(J,K)*B(K,I)                                       MATMP012          
      K=K+1                                                             MATMP013          
      GOTO 51                                                           MATMP014          
59    CONTINUE                                                          MATMP015          
      J=J+1                                                             MATMP016          
      GOTO 31                                                           MATMP017          
39    CONTINUE                                                          MATMP018          
      I=I+1                                                             MATMP019          
      GOTO 21                                                           MATMP020          
29    CONTINUE                                                          MATMP021          
      RETURN                                                            MATMP022          
      END                                                               MATMP023          
