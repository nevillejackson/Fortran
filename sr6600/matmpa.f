      SUBROUTINE MATMPA(A,B,C,M,N,L)                                    MATMP002          
      DIMENSION A(30,30),B(30,30),C(30,30),D(30)                        MATMP003          
C     A AND C MAY BE THE SAME ARRAY.     FORMS  C=A*B                   MATMP004          
      J=1                                                               MATMP005          
21    IF(J.GT.M) GOTO 29                                                MATMP006          
      I=1                                                               MATMP007          
31    IF(I.GT.L) GOTO 39                                                MATMP008          
      D(I)=0.0                                                          MATMP009          
      K=1                                                               MATMP010          
51    IF(K.GT.N) GOTO 59                                                MATMP011          
      D(I)=D(I)+A(J,K)*B(K,I)                                           MATMP012          
      K=K+1                                                             MATMP013          
      GOTO 51                                                           MATMP014          
59    CONTINUE                                                          MATMP015          
      I=I+1                                                             MATMP016          
      GOTO 31                                                           MATMP017          
39    CONTINUE                                                          MATMP018          
      I=1                                                               MATMP019          
71    IF(I.GT.L) GOTO 79                                                MATMP020          
      C(J,I)=D(I)                                                       MATMP021          
      I=I+1                                                             MATMP022          
      GOTO 71                                                           MATMP023          
79    CONTINUE                                                          MATMP024          
      J=J+1                                                             MATMP025          
      GOTO 21                                                           MATMP026          
29    CONTINUE                                                          MATMP027          
      RETURN                                                            MATMP028          
      END                                                               MATMP029          
