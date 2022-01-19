      SUBROUTINE DYNA                                                   DYNA0002          
      INTEGER Y                                                         DYNA0003          
      DIMENSION G1(99),G2(99),YDUM(20),Y(20,99),F(99)                   DYNA0004          
      COMMON/DATA/G1DUM,G1,G2DUM,G2,YDUM,Y,FDUM,F,M,N,LL                DYNA0005          
C   USES SUBSCRIPTS WITH 0,HENCE DUMMIES                                DYNA0006          
C     DYNAMIC PROGRAMMING                                               DYNA0007          
C  MAXIMISES SUM(K=1,N) F(K,X(K))                                       DYNA0008          
C   CONSTRAINED BY  SUM(K=1,N) X(K) .LE. M    AND X(K) NON NEG INTEGER  DYNA0009          
C   CALCULATES ONLY ONE OPTIMAL SOLUTION                                DYNA0010          
C***  MODIFY  TO STORE YOUR Y(N,M-1),G1(M-1),G2(M-1),F(M-1)             DYNA0011          
C                                                                       DYNA0012          
      MM=M+1                                                            DYNA0013          
      J=1                                                               DYNA0014          
31    IF(J.GT.MM) GOTO 39                                               DYNA0015          
      K=J-1                                                             DYNA0016          
      G1(K)=0                                                           DYNA0017          
      J=J+1                                                             DYNA0018          
      GOTO 31                                                           DYNA0019          
39    CONTINUE                                                          DYNA0020          
      L=1                                                               DYNA0021          
61    IF(L.GT.N) GOTO 69                                                DYNA0022          
C LL IS INDEX LL FOR F(LL,J)    IN EVAL                                 DYNA0023          
      LL=L                                                              DYNA0024          
C CALLS SUBROUTINE TO EVALUATE  F(I,J)   PASSES VALUES OVER  BY COMMON  DYNA0025          
C MUST DECLARE F TYPE INTEGER IN EVAL                                   DYNA0026          
      CALL EVAL                                                         DYNA0027          
C                                                                       DYNA0028          
      IF(L.EQ.N) GOTO 240                                               DYNA0029          
C     CALCULATE G2 AND Y(L,K)                                           DYNA0030          
      II=1                                                              DYNA0031          
101   IF(II.GT.MM) GOTO 109                                             DYNA0032          
      JJ=1                                                              DYNA0033          
111   IF(JJ.GT.II) GOTO 119                                             DYNA0034          
      I=II-1                                                            DYNA0035          
      J=JJ-1                                                            DYNA0036          
      IMJ=I-J                                                           DYNA0037          
      G=F(J)+G1(IMJ)                                                    DYNA0038          
C   CALCULATE G2(I)=MAX G OVER J                                        DYNA0039          
      IF(JJ.EQ.1) GOTO 180                                              DYNA0040          
      IF(G.LE.G2(I)) GOTO 200                                           DYNA0041          
180   G2(I)=G                                                           DYNA0042          
      Y(L,I)=J                                                          DYNA0043          
200   CONTINUE                                                          DYNA0044          
C   REPLACE G1 WITH G2                                                  DYNA0045          
      JJ=JJ+1                                                           DYNA0046          
      GOTO 111                                                          DYNA0047          
119   CONTINUE                                                          DYNA0048          
      II=II+1                                                           DYNA0049          
      GOTO 101                                                          DYNA0050          
109   CONTINUE                                                          DYNA0051          
      JJ=1                                                              DYNA0052          
211   IF(JJ.GT.MM) GOTO 219                                             DYNA0053          
      J=JJ-1                                                            DYNA0054          
C  CALCULATES FINAL G                                                   DYNA0055          
      G1(J)=G2(J)                                                       DYNA0056          
      JJ=JJ+1                                                           DYNA0057          
      GOTO 211                                                          DYNA0058          
219   CONTINUE                                                          DYNA0059          
      L=L+1                                                             DYNA0060          
      GOTO 61                                                           DYNA0061          
69    CONTINUE                                                          DYNA0062          
240   JJ=1                                                              DYNA0063          
241   IF(JJ.GT.MM) GOTO 249                                             DYNA0064          
      J=JJ-1                                                            DYNA0065          
      MMJ=M-J                                                           DYNA0066          
      GN=F(J)+G1(MMJ)                                                   DYNA0067          
      IF(JJ.EQ.1) GOTO 300                                              DYNA0068          
      IF(GN.LE.U) GOTO 320                                              DYNA0069          
C OPTIMAL VALUE                                                         DYNA0070          
300   U=GN                                                              DYNA0071          
C OPTIMAL X(N)                                                          DYNA0072          
      MAXX=J                                                            DYNA0073          
320   CONTINUE                                                          DYNA0074          
      JJ=JJ+1                                                           DYNA0075          
      GOTO 241                                                          DYNA0076          
249   CONTINUE                                                          DYNA0077          
      WRITE(61,340) U,MAXX                                              DYNA0078          
340   FORMAT(8H OPTIMAL,8H SOLUTIO,5HN U =,F12.9,4X,8HX(N)MAX ,I8)      DYNA0079          
C   CALCULATES OTHER OPTIMAL  X(N-1) TO X(1)                            DYNA0080          
      NL=N-1                                                            DYNA0081          
      I=1                                                               DYNA0082          
361   IF(I.GT.NL) GOTO 369                                              DYNA0083          
      KK=M-MAXX                                                         DYNA0084          
      L=N-I                                                             DYNA0085          
      WRITE(61,400) L,Y(L,KK)                                           DYNA0086          
400   FORMAT(3H X(,I4,2H)=,I5)                                          DYNA0087          
      MAXX=MAXX+Y(L,KK)                                                 DYNA0088          
      I=I+1                                                             DYNA0089          
      GOTO 361                                                          DYNA0090          
369   CONTINUE                                                          DYNA0091          
      RETURN                                                            DYNA0092          
      END                                                               DYNA0093          
