      SUBROUTINE JACOBI(N,A,B,KNTRL,ACC)                                JACOB002          
      DIMENSION A(20,20),B(20,20)                                       JACOB003          
C    EIGEN SOLUTION OF REAL SYMMETRIC MATRIX, A,                        JACOB004          
C      BY CYCLIC-THRESHOLD JACOBI 2-BY-2 ROTATIONS.DESCENDING           JACOB005          
C      EIGENVALUES IN A(J,J) AND EIGENVECTORS IN B(*,J),J=1,N.          JACOB006          
      IF(N-1) 650,30,50                                                 JACOB007          
30    B(1,1)=1.                                                         JACOB008          
      GOTO 650                                                          JACOB009          
50    IF(KNTRL) 120,60,120                                              JACOB010          
C      IDENTITY MATRIX IN B                                             JACOB011          
60    I=1                                                               JACOB012          
61    IF(I.GT.N) GOTO 69                                                JACOB013          
      J=1                                                               JACOB014          
71    IF(J.GT.N) GOTO 79                                                JACOB015          
      B(I,J)=0.                                                         JACOB016          
      IF(.NOT.(I.EQ.J)) GOTO 110                                        JACOB017          
      B(I,J)=1.                                                         JACOB018          
110   CONTINUE                                                          JACOB019          
C      CALCULATE NORMS AND THRESHOLD                                    JACOB020          
      J=J+1                                                             JACOB021          
      GOTO 71                                                           JACOB022          
79    CONTINUE                                                          JACOB023          
      I=I+1                                                             JACOB024          
      GOTO 61                                                           JACOB025          
69    CONTINUE                                                          JACOB026          
120   RN=N                                                              JACOB027          
      ANORM=0.                                                          JACOB028          
      I=2                                                               JACOB029          
141   IF(I.GT.N) GOTO 149                                               JACOB030          
      IM=I-1                                                            JACOB031          
      J=1                                                               JACOB032          
161   IF(J.GT.IM) GOTO 169                                              JACOB033          
      AIJ=A(I,J)                                                        JACOB034          
      ANORM=ANORM+AIJ*AIJ                                               JACOB035          
      J=J+1                                                             JACOB036          
      GOTO 161                                                          JACOB037          
169   CONTINUE                                                          JACOB038          
      I=I+1                                                             JACOB039          
      GOTO 141                                                          JACOB040          
149   CONTINUE                                                          JACOB041          
      THR=N*(N-1)                                                       JACOB042          
      THR=SQRT(2.*ANORM/THR)                                            JACOB043          
      INDEX=0                                                           JACOB044          
230   THR=THR/RN                                                        JACOB045          
C      SWEEP THROUGH OFF-DIAGONAL ELEMENTS                              JACOB046          
240   J=2                                                               JACOB047          
241   IF(J.GT.N) GOTO 249                                               JACOB048          
      JM=J-1                                                            JACOB049          
      I=1                                                               JACOB050          
261   IF(I.GT.JM) GOTO 269                                              JACOB051          
      IF(ABS(A(I,J)).LE.THR) GOTO 600                                   JACOB052          
      INDEX=1                                                           JACOB053          
      V=A(I,I)                                                          JACOB054          
      VV=A(I,J)                                                         JACOB055          
      VVV=A(J,J)                                                        JACOB056          
      RMU=0.5*(V-VVV)                                                   JACOB057          
      SIGN=-1.                                                          JACOB058          
      IF(VV) 350,600,360                                                JACOB059          
350   SIGN=1.                                                           JACOB060          
360   IF(RMU/VV) 400,370,400                                            JACOB061          
370   HYP2=ABS(VV)                                                      JACOB062          
      COS2=0.                                                           JACOB063          
      GOTO 440                                                          JACOB064          
C      COMPUTE ROTATIONAL COSX,SINX                                     JACOB065          
400   HYP2=SQRT(VV*VV+RMU*RMU)                                          JACOB066          
      COS2=RMU/HYP2                                                     JACOB067          
      K=COS2                                                            JACOB068          
      IF(K.EQ.0) GOTO 440                                               JACOB069          
      COS2=K                                                            JACOB070          
440   SINX=SIGN*SQRT(0.5*(1.-COS2))                                     JACOB071          
      COSX=SQRT(0.5*(1.+COS2))                                          JACOB072          
      K=1                                                               JACOB073          
461   IF(K.GT.N) GOTO 469                                               JACOB074          
      IF(K.EQ.I.OR.K.EQ.J) GOTO 520                                     JACOB075          
      XX=A(K,I)                                                         JACOB076          
      RMU=A(K,J)                                                        JACOB077          
      A(K,J)=XX*SINX+RMU*COSX                                           JACOB078          
      A(J,K)=A(K,J)                                                     JACOB079          
      A(K,I)=XX*COSX-RMU*SINX                                           JACOB080          
      A(I,K)=A(K,I)                                                     JACOB081          
520   XX=B(K,I)                                                         JACOB082          
      RMU=B(K,J)                                                        JACOB083          
      B(K,J)=XX*SINX+RMU*COSX                                           JACOB084          
      B(K,I)=XX*COSX-RMU*SINX                                           JACOB085          
      K=K+1                                                             JACOB086          
      GOTO 461                                                          JACOB087          
469   CONTINUE                                                          JACOB088          
      XX=0.5*(V+VVV)                                                    JACOB089          
      A(I,I)=XX+HYP2                                                    JACOB090          
      A(J,J)=XX-HYP2                                                    JACOB091          
      A(I,J)=0.                                                         JACOB092          
      A(J,I)=A(I,J)                                                     JACOB093          
600   CONTINUE                                                          JACOB094          
C      TESTS WITH CURRENT AND FINAL TOLERANCES                          JACOB095          
      I=I+1                                                             JACOB096          
      GOTO 261                                                          JACOB097          
269   CONTINUE                                                          JACOB098          
      J=J+1                                                             JACOB099          
      GOTO 241                                                          JACOB100          
249   CONTINUE                                                          JACOB101          
      IF(INDEX.EQ.0) GOTO 640                                           JACOB102          
      INDEX=0                                                           JACOB103          
      GOTO 240                                                          JACOB104          
640   IF(THR.GE.ACC) GOTO 230                                           JACOB105          
650   CONTINUE                                                          JACOB106          
      RETURN                                                            JACOB107          
      END                                                               JACOB108          
