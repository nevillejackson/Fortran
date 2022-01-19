      SUBROUTINE CMTINV(A,B,C,D,N,IRR)                                  CMTIN002          
      DIMENSION A(30,30),B(30,30),C(30,30),D(30,30)                     CMTIN003          
C     A AND B ARE NXN MATRICES CONTAINING THE REAL AND IMAGINARY PARTS  CMTIN004          
C     C AND D WILL CONTAIN INVERSE.                                     CMTIN005          
C     IRR WILL BE NON ZERO IF MATRIX IS SINGULAR                        CMTIN006          
      L=1                                                               CMTIN007          
30    I=1                                                               CMTIN008          
31    IF(I.GT.N) GOTO 39                                                CMTIN009          
      J=1                                                               CMTIN010          
41    IF(J.GT.N) GOTO 49                                                CMTIN011          
      D(I,J)=-A(I,J)                                                    CMTIN012          
C     INVERT -A                                                         CMTIN013          
      J=J+1                                                             CMTIN014          
      GOTO 41                                                           CMTIN015          
49    CONTINUE                                                          CMTIN016          
      I=I+1                                                             CMTIN017          
      GOTO 31                                                           CMTIN018          
39    CONTINUE                                                          CMTIN019          
      CALL MATINV(D,N,C,0,DUM,IRR)                                      CMTIN020          
C     CHECK A NON -SINGULAR                                             CMTIN021          
      IF(IRR) 200,80,200                                                CMTIN022          
C     COMPUTE  C=(A+B*A(-1)*B)(-1)                                      CMTIN023          
80    CALL MATMPB(B,D,D,N,N,N)                                          CMTIN024          
      CALL MATMPB(D,B,C,N,N,N)                                          CMTIN025          
      I=1                                                               CMTIN026          
101   IF(I.GT.N) GOTO 109                                               CMTIN027          
      J=1                                                               CMTIN028          
111   IF(J.GT.N) GOTO 119                                               CMTIN029          
      C(I,J)=A(I,J)-C(I,J)                                              CMTIN030          
      J=J+1                                                             CMTIN031          
      GOTO 111                                                          CMTIN032          
119   CONTINUE                                                          CMTIN033          
      I=I+1                                                             CMTIN034          
      GOTO 101                                                          CMTIN035          
109   CONTINUE                                                          CMTIN036          
      CALL MATINV(C,N,D,0,DUM,IRR)                                      CMTIN037          
C     CHECK THAT C EXISTS                                               CMTIN038          
      IF(IRR) 150,170,150                                               CMTIN039          
150   IRR=2                                                             CMTIN040          
      GOTO 370                                                          CMTIN041          
C     COMPUTE D=-C*B*A(-1)                                              CMTIN042          
170   CALL MATMPB(C,D,D,N,N,N)                                          CMTIN043          
      KQZ001=L                                                          CMTIN044          
      IF(KQZ001.LT.1) KQZ001=1                                          CMTIN045          
      IF(KQZ001.GT.2) KQZ001=2                                          CMTIN046          
      GOTO(190,280),KQZ001                                              CMTIN047          
C     SUCCESSFUL INVERSION                                              CMTIN048          
190   GOTO 370                                                          CMTIN049          
C     A IS SINGULAR, INTERCHANGE A AND B AND TRY AGAIN.                 CMTIN050          
200   I=1                                                               CMTIN051          
201   IF(I.GT.N) GOTO 209                                               CMTIN052          
      J=1                                                               CMTIN053          
211   IF(J.GT.N) GOTO 219                                               CMTIN054          
      DUM=A(I,J)                                                        CMTIN055          
      A(I,J)=B(I,J)                                                     CMTIN056          
      B(I,J)=DUM                                                        CMTIN057          
C     IF L=2, A AND B ARE BOTH SINGULAR. IRR=1                          CMTIN058          
      J=J+1                                                             CMTIN059          
      GOTO 211                                                          CMTIN060          
219   CONTINUE                                                          CMTIN061          
      I=I+1                                                             CMTIN062          
      GOTO 201                                                          CMTIN063          
209   CONTINUE                                                          CMTIN064          
      IF(L-2) 260,190,190                                               CMTIN065          
260   L=2                                                               CMTIN066          
      GOTO 30                                                           CMTIN067          
C     INTERCHANGE A AND B, C AND D WITH CHANGED SIGNS.                  CMTIN068          
280   I=1                                                               CMTIN069          
281   IF(I.GT.N) GOTO 289                                               CMTIN070          
      J=1                                                               CMTIN071          
291   IF(J.GT.N) GOTO 299                                               CMTIN072          
      DUM=A(I,J)                                                        CMTIN073          
      A(I,J)=B(I,J)                                                     CMTIN074          
      B(I,J)=DUM                                                        CMTIN075          
      DUM=-C(I,J)                                                       CMTIN076          
      C(I,J)=-D(I,J)                                                    CMTIN077          
      D(I,J)=DUM                                                        CMTIN078          
      J=J+1                                                             CMTIN079          
      GOTO 291                                                          CMTIN080          
299   CONTINUE                                                          CMTIN081          
      I=I+1                                                             CMTIN082          
      GOTO 281                                                          CMTIN083          
289   CONTINUE                                                          CMTIN084          
      GOTO 190                                                          CMTIN085          
370   RETURN                                                            CMTIN086          
      END                                                               CMTIN087          
