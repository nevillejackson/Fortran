      SUBROUTINE BANDIT(ORDER,WIDTH,A,B,IERR)                           BANDI002          
      INTEGER ORDER,WIDTH                                               BANDI003          
      DIMENSION A(1),B(1)                                               BANDI004          
C                                                                       BANDI005          
C     DECK AND WRITE-UP MODIFIED BY A.COOK D.C.R. CLAYTON MAY 1968      BANDI006          
C     GIVEN A COMPUTE B SUCH THAT A IS THE PRODUCT OF B TRANSPOSE AND B BANDI007          
C     A IS A POSITIVE DEFINITE SYMMETRIC BAND MATRIX                    BANDI008          
C                                                                       BANDI009          
C                                                                       BANDI010          
      N=ORDER $ M=WIDTH-1 $ MMINUSN=M-N $ IERR=-1                       BANDI011          
      IF(N.LE.0.OR.M.GT.(N-1).OR.M.LT.0) GOTO 530                       BANDI012          
      CALL INDXSET(M,N,MMINUSN)                                         BANDI013          
      I=1                                                               BANDI014          
81    IF(I.GT.N) GOTO 89                                                BANDI015          
      IPLUS1=I+1                                                        BANDI016          
      IMINUS1=I-1                                                       BANDI017          
      JMAX=MIN0(I+M,N) $ IMAX=MAX0(I-M,1)                               BANDI018          
C                                                                       BANDI019          
C     REPLACE DIAGONAL ELEMENT OF A BY DIAGONAL ELEMENT OF B            BANDI020          
C                                                                       BANDI021          
      TEMP=0.0                                                          BANDI022          
      L=IMAX                                                            BANDI023          
141   IF(L.GT.IMINUS1) GOTO 149                                         BANDI024          
      CALL INDEX(L,I,K)                                                 BANDI025          
      TEMP=TEMP+A(K)**2                                                 BANDI026          
      L=L+1                                                             BANDI027          
      GOTO 141                                                          BANDI028          
149   CONTINUE                                                          BANDI029          
      CALL INDEX(I,I,K)                                                 BANDI030          
      DIAG=A(K)-TEMP $ IERR=1 $ IF(DIAG.LE.0E0) GOTO 530                BANDI031          
      A(K)=SQRT(DIAG)                                                   BANDI032          
      DIAG=A(K)                                                         BANDI033          
C                                                                       BANDI034          
C     REPLACE I,TH ROW OF A BY I,TH ROW OF B                            BANDI035          
C                                                                       BANDI036          
      J=IPLUS1                                                          BANDI037          
221   IF(J.GT.JMAX) GOTO 229                                            BANDI038          
      TEMP=0.0                                                          BANDI039          
      IMAX=MAX0(J-M,1)                                                  BANDI040          
      L=IMAX                                                            BANDI041          
251   IF(L.GT.IMINUS1) GOTO 259                                         BANDI042          
      CALL INDEX(L,I,K1)                                                BANDI043          
      CALL INDEX(L,J,K2)                                                BANDI044          
      TEMP=TEMP+A(K1)*A(K2)                                             BANDI045          
      L=L+1                                                             BANDI046          
      GOTO 251                                                          BANDI047          
259   CONTINUE                                                          BANDI048          
      CALL INDEX(I,J,K)                                                 BANDI049          
      A(K)=(A(K)-TEMP)/DIAG                                             BANDI050          
      J=J+1                                                             BANDI051          
      GOTO 221                                                          BANDI052          
229   CONTINUE                                                          BANDI053          
C                                                                       BANDI054          
C     NOW DO FIRST BACK SUBSTITUTION                                    BANDI055          
C                                                                       BANDI056          
      I=I+1                                                             BANDI057          
      GOTO 81                                                           BANDI058          
89    CONTINUE                                                          BANDI059          
      J=1                                                               BANDI060          
321   IF(J.GT.N) GOTO 329                                               BANDI061          
      TEMP=0.0                                                          BANDI062          
      IMAX=MAX0(J-M,1)                                                  BANDI063          
      IMINUS1=J-1                                                       BANDI064          
      L=IMAX                                                            BANDI065          
361   IF(L.GT.IMINUS1) GOTO 369                                         BANDI066          
      CALL INDEX(L,J,K)                                                 BANDI067          
      TEMP=TEMP+A(K)*B(L)                                               BANDI068          
      L=L+1                                                             BANDI069          
      GOTO 361                                                          BANDI070          
369   CONTINUE                                                          BANDI071          
      CALL INDEX(J,J,K)                                                 BANDI072          
      B(J)=(B(J)-TEMP)/A(K)                                             BANDI073          
C                                                                       BANDI074          
C     FINISH SOLUTION WITH SECOND BACK SUBSTITUTION                     BANDI075          
C                                                                       BANDI076          
      J=J+1                                                             BANDI077          
      GOTO 321                                                          BANDI078          
329   CONTINUE                                                          BANDI079          
      J=N                                                               BANDI080          
420   TEMP=0.0                                                          BANDI081          
      IMAX=MIN0(J+M,N)                                                  BANDI082          
      IPLUS1=J+1                                                        BANDI083          
      L=IPLUS1                                                          BANDI084          
451   IF(L.GT.IMAX) GOTO 459                                            BANDI085          
      CALL INDEX(J,L,K)                                                 BANDI086          
      TEMP=TEMP+A(K)*B(L)                                               BANDI087          
      L=L+1                                                             BANDI088          
      GOTO 451                                                          BANDI089          
459   CONTINUE                                                          BANDI090          
      CALL INDEX(J,J,K)                                                 BANDI091          
      B(J)=(B(J)-TEMP)/A(K)                                             BANDI092          
      J=J-1                                                             BANDI093          
      IF(J.GT.0) GOTO 420                                               BANDI094          
      IERR=0                                                            BANDI095          
530   CONTINUE                                                          BANDI096          
      RETURN                                                            BANDI097          
      END                                                               BANDI098          
      SUBROUTINE INDEX(I,J,K)                                           BANDI099          
      IMINUS1=I-1 $ ITEST=MMINUSN+IMINUS1 $ K=M*IMINUS1+J               BANDI100          
      IF(.NOT.(ITEST.GT.0)) GOTO 70                                     BANDI101          
      K=K-(ITEST*(ITEST+1))/2                                           BANDI102          
70    GOTO 120                                                          BANDI103          
      ENTRY INDXSET                                                     BANDI104          
      M=I $ N=J $ MMINUSN=K                                             BANDI105          
120   RETURN                                                            BANDI106          
      END                                                               BANDI107          
