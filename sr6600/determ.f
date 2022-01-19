      SUBROUTINE DETERM(A,N,DET)                                        DETER002          
      DIMENSION IR(50),IC(50),A(50,50)                                  DETER003          
C     A IS AN NXN MATRIX WHOSE DETERMINANT DET IS REQUIRED.             DETER004          
C     THE METHOD OF GAUSS PIVOTAL CONDENSATION IS USED WITH TOTAL PIVOT DETER005          
C     SEARCHING                                                         DETER006          
      DET=1.0                                                           DETER007          
      I=1                                                               DETER008          
31    IF(I.GT.N) GOTO 39                                                DETER009          
      IC(I)=0                                                           DETER010          
      IR(I)=0                                                           DETER011          
      I=I+1                                                             DETER012          
      GOTO 31                                                           DETER013          
39    CONTINUE                                                          DETER014          
      I=1                                                               DETER015          
61    IF(I.GT.N) GOTO 69                                                DETER016          
      AMAX=0.0                                                          DETER017          
C  KEEPING COUNT OF THE SIGN OF THE PIVOT ELEMENT                       DETER018          
      KEYJ=1                                                            DETER019          
      KEYK=1                                                            DETER020          
C     SEARCH SUB-MATRIX FOR LARGEST ELEMENT AS PIVOT                    DETER021          
      J=1                                                               DETER022          
101   IF(J.GT.N) GOTO 109                                               DETER023          
      IF(IR(J)) 230,120,230                                             DETER024          
120   KEYK=-KEYJ                                                        DETER025          
      K=1                                                               DETER026          
131   IF(K.GT.N) GOTO 139                                               DETER027          
      IF(IC(K)) 210,150,210                                             DETER028          
C     THIS ROW/COLUMN HAS NOT BEEN A PIVOT                              DETER029          
150   IF(ABS(A(J,K))-AMAX) 200,200,160                                  DETER030          
160   IROW=J                                                            DETER031          
      ICOL=K                                                            DETER032          
      L=-KEYK                                                           DETER033          
      AMAX=ABS(A(J,K))                                                  DETER034          
200   KEYK=-KEYK                                                        DETER035          
210   CONTINUE                                                          DETER036          
      K=K+1                                                             DETER037          
      GOTO 131                                                          DETER038          
139   CONTINUE                                                          DETER039          
      KEYJ=-KEYJ                                                        DETER040          
230   CONTINUE                                                          DETER041          
      J=J+1                                                             DETER042          
      GOTO 101                                                          DETER043          
109   CONTINUE                                                          DETER044          
      IF(AMAX.EQ.0E0) GOTO 390                                          DETER045          
      IR(IROW)=1                                                        DETER046          
C     PIVOT FOUND                                                       DETER047          
      IC(ICOL)=1                                                        DETER048          
      DET=DET*A(IROW,ICOL)*FLOAT(L)                                     DETER049          
C     ELIMINATE PIVOT COLUMN IN NON PIVOT ROWS                          DETER050          
C     UNTIL LAST TIME ROUND.                                            DETER051          
      IF(I-N) 290,400,400                                               DETER052          
290   J=1                                                               DETER053          
291   IF(J.GT.N) GOTO 299                                               DETER054          
      IF(J-IROW) 310,380,310                                            DETER055          
310   AMAX=A(J,ICOL)/A(IROW,ICOL)                                       DETER056          
      K=1                                                               DETER057          
321   IF(K.GT.N) GOTO 329                                               DETER058          
      IF(IC(K)) 340,360,340                                             DETER059          
340   A(J,K)=0.0                                                        DETER060          
      GOTO 370                                                          DETER061          
360   A(J,K)=A(J,K)-A(IROW,K)*AMAX                                      DETER062          
370   CONTINUE                                                          DETER063          
      K=K+1                                                             DETER064          
      GOTO 321                                                          DETER065          
329   CONTINUE                                                          DETER066          
380   CONTINUE                                                          DETER067          
C     DETERMINANT FORMED                                                DETER068          
      J=J+1                                                             DETER069          
      GOTO 291                                                          DETER070          
299   CONTINUE                                                          DETER071          
      I=I+1                                                             DETER072          
      GOTO 61                                                           DETER073          
69    CONTINUE                                                          DETER074          
390   DET=0                                                             DETER075          
400   CONTINUE                                                          DETER076          
      RETURN                                                            DETER077          
      END                                                               DETER078          
