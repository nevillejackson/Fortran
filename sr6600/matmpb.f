      SUBROUTINE MATMPB(A,B,C,M,N,L)                                    MATMP002          
      DIMENSION A(30,30),B(30,30),C(30,30),D(30)                        MATMP003          
C     B AND C MAY BE THE SAME ARRAY.   FORMS  C=A*B                     MATMP004          
      I=1                                                               MATMP005          
21    IF(I.GT.L) GOTO 29                                                MATMP006          
      J=1                                                               MATMP007          
31    IF(J.GT.M) GOTO 39                                                MATMP008          
      D(J)=0.0                                                          MATMP009          
      K=1                                                               MATMP010          
51    IF(K.GT.N) GOTO 59                                                MATMP011          
      D(J)=D(J)+A(J,K)*B(K,I)                                           MATMP012          
      K=K+1                                                             MATMP013          
      GOTO 51                                                           MATMP014          
59    CONTINUE                                                          MATMP015          
      J=J+1                                                             MATMP016          
      GOTO 31                                                           MATMP017          
39    CONTINUE                                                          MATMP018          
      J=1                                                               MATMP019          
71    IF(J.GT.M) GOTO 79                                                MATMP020          
      C(J,I)=D(J)                                                       MATMP021          
      J=J+1                                                             MATMP022          
      GOTO 71                                                           MATMP023          
79    CONTINUE                                                          MATMP024          
      I=I+1                                                             MATMP025          
      GOTO 21                                                           MATMP026          
29    CONTINUE                                                          MATMP027          
      RETURN                                                            MATMP028          
      END                                                               MATMP029          
