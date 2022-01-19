      SUBROUTINE CHOESKI(N,A,C,IZ)                                      CHOLE002          
      DIMENSION A(32,32),C(32,32)                                       CHOLE003          
C  DECOMPOSE  SQUARE  MATRIX  A  INTO PRODUCT OF LOWER- AND             CHOLE004          
C  UPPER-TRIANGULAR MATRICES, RETURNED TOGETHER IN ARRAY  C.            CHOLE005          
C  UNIT DIAGONAL IS IMPLIED FOR UPPER HALF (SECOND FACTOR).             CHOLE006          
C  IZ  RETURNS ERROR INDEX.                                             CHOLE007          
      C(1,1)=A(1,1)                                                     CHOLE008          
      IF(N.LE.1) GOTO 370                                               CHOLE009          
      I=2                                                               CHOLE010          
51    IF(I.GT.N) GOTO 59                                                CHOLE011          
      C(I,1)=A(I,1)                                                     CHOLE012          
      C(1,I)=A(1,I)/A(1,1)                                              CHOLE013          
      IZ=2-IABS(LEGVAR(C(1,I)))                                         CHOLE014          
      GOTO(100,120),IZ                                                  CHOLE015          
100   IZ=1                                                              CHOLE016          
      GOTO 380                                                          CHOLE017          
120   CONTINUE                                                          CHOLE018          
      I=I+1                                                             CHOLE019          
      GOTO 51                                                           CHOLE020          
59    CONTINUE                                                          CHOLE021          
      K=2                                                               CHOLE022          
131   IF(K.GT.N) GOTO 139                                               CHOLE023          
      SKK=A(K,K)                                                        CHOLE024          
      IK=K-1                                                            CHOLE025          
      I=1                                                               CHOLE026          
161   IF(I.GT.IK) GOTO 169                                              CHOLE027          
      SKK=SKK-C(K,I)*C(I,K)                                             CHOLE028          
      I=I+1                                                             CHOLE029          
      GOTO 161                                                          CHOLE030          
169   CONTINUE                                                          CHOLE031          
      C(K,K)=SKK                                                        CHOLE032          
      IF(K-N) 210,360,360                                               CHOLE033          
210   JK=K+1                                                            CHOLE034          
      J=JK                                                              CHOLE035          
221   IF(J.GT.N) GOTO 229                                               CHOLE036          
      SJK=A(J,K)                                                        CHOLE037          
      SKJ=A(K,J)                                                        CHOLE038          
      I=1                                                               CHOLE039          
251   IF(I.GT.IK) GOTO 259                                              CHOLE040          
      SJK=SJK-C(J,I)*C(I,K)                                             CHOLE041          
      SKJ=SKJ-C(K,I)*C(I,J)                                             CHOLE042          
      I=I+1                                                             CHOLE043          
      GOTO 251                                                          CHOLE044          
259   CONTINUE                                                          CHOLE045          
      C(J,K)=SJK                                                        CHOLE046          
      C(K,J)=SKJ/SKK                                                    CHOLE047          
      IZ=2-IABS(LEGVAR(C(K,J)))                                         CHOLE048          
      GOTO(330,350),IZ                                                  CHOLE049          
330   IZ=K                                                              CHOLE050          
      GOTO 380                                                          CHOLE051          
350   CONTINUE                                                          CHOLE052          
      J=J+1                                                             CHOLE053          
      GOTO 221                                                          CHOLE054          
229   CONTINUE                                                          CHOLE055          
360   CONTINUE                                                          CHOLE056          
      K=K+1                                                             CHOLE057          
      GOTO 131                                                          CHOLE058          
139   CONTINUE                                                          CHOLE059          
370   IZ=0                                                              CHOLE060          
380   CONTINUE                                                          CHOLE061          
      RETURN                                                            CHOLE062          
      END                                                               CHOLE063          
