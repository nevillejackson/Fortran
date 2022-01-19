      SUBROUTINE PADD(A,N,B,M,C,L)                                      PADD0002          
      DIMENSION A(1),B(1),C(1)                                          PADD0003          
      MARK=1                                                            PADD0004          
30    K=N                                                               PADD0005          
      J=1                                                               PADD0006          
      IF(M-N) 60,80,80                                                  PADD0007          
60    K=M                                                               PADD0008          
      J=2                                                               PADD0009          
80    I=1                                                               PADD0010          
90    C(I)=A(I)+B(I)                                                    PADD0011          
      I=I+1                                                             PADD0012          
      IF(K-I) 120,90,90                                                 PADD0013          
120   KQZ001=J                                                          PADD0014          
      IF(KQZ001.LT.1) KQZ001=1                                          PADD0015          
      IF(KQZ001.GT.2) KQZ001=2                                          PADD0016          
      GOTO(130,170),KQZ001                                              PADD0017          
130   J=I                                                               PADD0018          
131   IF(J.GT.M) GOTO 139                                               PADD0019          
      C(J)=B(J)                                                         PADD0020          
      J=J+1                                                             PADD0021          
      GOTO 131                                                          PADD0022          
139   CONTINUE                                                          PADD0023          
      L=M                                                               PADD0024          
      KQZ001=MARK                                                       PADD0025          
      IF(KQZ001.LT.1) KQZ001=1                                          PADD0026          
      IF(KQZ001.GT.2) KQZ001=2                                          PADD0027          
      GOTO(270,230),KQZ001                                              PADD0028          
170   J=I                                                               PADD0029          
171   IF(J.GT.N) GOTO 179                                               PADD0030          
      C(J)=A(J)                                                         PADD0031          
      J=J+1                                                             PADD0032          
      GOTO 171                                                          PADD0033          
179   CONTINUE                                                          PADD0034          
      L=N                                                               PADD0035          
      KQZ001=MARK                                                       PADD0036          
      IF(KQZ001.LT.1) KQZ001=1                                          PADD0037          
      IF(KQZ001.GT.2) KQZ001=2                                          PADD0038          
      GOTO(270,230),KQZ001                                              PADD0039          
      ENTRY PSUB                                                        PADD0040          
      MARK=1                                                            PADD0041          
230   MARK=MARK+1                                                       PADD0042          
      I=1                                                               PADD0043          
241   IF(I.GT.M) GOTO 249                                               PADD0044          
      B(I)=-B(I)                                                        PADD0045          
      I=I+1                                                             PADD0046          
      GOTO 241                                                          PADD0047          
249   CONTINUE                                                          PADD0048          
      KQZ001=MARK                                                       PADD0049          
      IF(KQZ001.LT.1) KQZ001=1                                          PADD0050          
      IF(KQZ001.GT.3) KQZ001=3                                          PADD0051          
      GOTO(30,30,270),KQZ001                                            PADD0052          
270   CONTINUE                                                          PADD0053          
      RETURN                                                            PADD0054          
      END                                                               PADD0055          
