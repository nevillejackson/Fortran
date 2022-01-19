      SUBROUTINE SINSORT(A,N)                                           SINGS002          
      DIMENSION A(N),IU(16),IL(16)                                      SINGS003          
C     CACM ALGORITHM 343  R.C. SINGLETON   17.9.68                      SINGS004          
C     TO SORT INTEGERS, USE    INTEGER A,T,TT                           SINGS005          
      M=1                                                               SINGS006          
      II=1                                                              SINGS007          
      I=II $ J=N                                                        SINGS008          
50    IF(.NOT.(I.LT.J)) GOTO 420                                        SINGS009          
60    K=I                                                               SINGS010          
      IJ=(J+I)/2                                                        SINGS011          
      T=A(IJ)                                                           SINGS012          
      IF(.NOT.(A(I).GT.T)) GOTO 130                                     SINGS013          
      A(IJ)=A(I)                                                        SINGS014          
      A(I)=T                                                            SINGS015          
      T=A(IJ)                                                           SINGS016          
130   L=J                                                               SINGS017          
      IF(.NOT.(A(J).LT.T)) GOTO 250                                     SINGS018          
      A(IJ)=A(J)                                                        SINGS019          
      A(J)=T                                                            SINGS020          
      T=A(IJ)                                                           SINGS021          
      IF(.NOT.(A(I).GT.T)) GOTO 250                                     SINGS022          
      A(IJ)=A(I)                                                        SINGS023          
      A(I)=T                                                            SINGS024          
      T=A(IJ)                                                           SINGS025          
      GOTO 250                                                          SINGS026          
230   A(L)=A(K)                                                         SINGS027          
      A(K)=TT                                                           SINGS028          
250   L=L-1                                                             SINGS029          
      IF(.NOT.(A(L).LE.T)) GOTO 250                                     SINGS030          
      TT=A(L)                                                           SINGS031          
280   K=K+1                                                             SINGS032          
      IF(.NOT.(A(K).GE.T)) GOTO 280                                     SINGS033          
      IF(.NOT.(K.GT.L)) GOTO 230                                        SINGS034          
      IF(.NOT.(L-I.GT.J-K)) GOTO 370                                    SINGS035          
      IL(M)=I                                                           SINGS036          
      IU(M)=L                                                           SINGS037          
      I=K                                                               SINGS038          
      M=M+1                                                             SINGS039          
      GOTO 470                                                          SINGS040          
370   IL(M)=K                                                           SINGS041          
      IU(M)=J                                                           SINGS042          
      J=L                                                               SINGS043          
      M=M+1                                                             SINGS044          
      GOTO 470                                                          SINGS045          
420   M=M-1                                                             SINGS046          
      IF(M.EQ.0) GOTO 600                                               SINGS047          
      I=IL(M)                                                           SINGS048          
      J=IU(M)                                                           SINGS049          
470   IF(.NOT.(J-I.LT.11)) GOTO 60                                      SINGS050          
      IF(.NOT.(I.NE.II)) GOTO 50                                        SINGS051          
      I=I-1                                                             SINGS052          
500   I=I+1                                                             SINGS053          
      IF(.NOT.(I.NE.J)) GOTO 420                                        SINGS054          
      T=A(I+1)                                                          SINGS055          
      IF(.NOT.(A(I).GT.T)) GOTO 500                                     SINGS056          
      K=I                                                               SINGS057          
550   A(K+1)=A(K)                                                       SINGS058          
      K=K-1                                                             SINGS059          
      IF(.NOT.(T.GE.A(K))) GOTO 550                                     SINGS060          
      A(K+1)=T                                                          SINGS061          
      GOTO 500                                                          SINGS062          
600   RETURN                                                            SINGS063          
      END                                                               SINGS064          
