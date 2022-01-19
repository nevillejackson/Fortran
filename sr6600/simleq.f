      SUBROUTINE SIMLEQ(A,N,M,IRR)                                      SIMLE002          
      DIMENSION A(30,60)                                                SIMLE003          
C     A IS AN NXN EQUATION MATRIX, PLUS M-N RHS VECTORS.                SIMLE004          
C     THE X SOLUTIONS REPLACE THE RHS"S.                                SIMLE005          
C     METHOD OF GAUSSIAN ELIMINATION WITH ROW PIVOTING AND BACK         SIMLE006          
C     SUBSTITUTION                                                      SIMLE007          
      EPS=1.0E-90                                                       SIMLE008          
      NN=N-1                                                            SIMLE009          
      IF(NN) 170,250,50                                                 SIMLE010          
C     FOR EACH COLUMN FIND LARGEST ELEMENT IN LOWER TRIANGLE FOR PIVOT  SIMLE011          
50    L=1                                                               SIMLE012          
51    IF(L.GT.NN) GOTO 59                                               SIMLE013          
      KK=L                                                              SIMLE014          
      K=L                                                               SIMLE015          
71    IF(K.GT.N) GOTO 79                                                SIMLE016          
      IF(ABS(A(K,L))-ABS(A(KK,L))) 100,100,90                           SIMLE017          
90    KK=K                                                              SIMLE018          
100   CONTINUE                                                          SIMLE019          
C     MAKE PIVOT A DIAGONAL ELEMENT BY ROW INTERCHANGE                  SIMLE020          
      K=K+1                                                             SIMLE021          
      GOTO 71                                                           SIMLE022          
79    CONTINUE                                                          SIMLE023          
      IF(KK-L) 160,160,120                                              SIMLE024          
120   J=L                                                               SIMLE025          
121   IF(J.GT.M) GOTO 129                                               SIMLE026          
      B=A(L,J)                                                          SIMLE027          
      A(L,J)=A(KK,J)                                                    SIMLE028          
      A(KK,J)=B                                                         SIMLE029          
C     TEST FOR MATRIX SINGULAR (PIVOT TOO SMALL)                        SIMLE030          
      J=J+1                                                             SIMLE031          
      GOTO 121                                                          SIMLE032          
129   CONTINUE                                                          SIMLE033          
160   IF(ABS(A(L,L))-EPS) 170,170,190                                   SIMLE034          
170   IRR=1                                                             SIMLE035          
      GOTO 380                                                          SIMLE036          
C     ELIMINATE COLUMN BELOW DIAGONAL BY ROW SUBTRACTION                SIMLE037          
190   KK=L+1                                                            SIMLE038          
      K=KK                                                              SIMLE039          
201   IF(K.GT.N) GOTO 209                                               SIMLE040          
      IF(A(K,L)) 220,240,220                                            SIMLE041          
220   J=KK                                                              SIMLE042          
221   IF(J.GT.M) GOTO 229                                               SIMLE043          
      A(K,J)=A(K,J)-A(K,L)/A(L,L)*A(L,J)                                SIMLE044          
      J=J+1                                                             SIMLE045          
      GOTO 221                                                          SIMLE046          
229   CONTINUE                                                          SIMLE047          
240   CONTINUE                                                          SIMLE048          
C     SOLVE FOR X BY BACK SUBSTITUTION                                  SIMLE049          
      K=K+1                                                             SIMLE050          
      GOTO 201                                                          SIMLE051          
209   CONTINUE                                                          SIMLE052          
      L=L+1                                                             SIMLE053          
      GOTO 51                                                           SIMLE054          
59    CONTINUE                                                          SIMLE055          
250   IF(ABS(A(N,N))-EPS) 170,170,260                                   SIMLE056          
260   NN=N+1                                                            SIMLE057          
      II=1                                                              SIMLE058          
271   IF(II.GT.N) GOTO 279                                              SIMLE059          
      I=N-II+1                                                          SIMLE060          
      KK=I+1                                                            SIMLE061          
      J=NN                                                              SIMLE062          
301   IF(J.GT.M) GOTO 309                                               SIMLE063          
      B=0.0                                                             SIMLE064          
      IF(I-N) 330,350,330                                               SIMLE065          
330   K=KK                                                              SIMLE066          
331   IF(K.GT.N) GOTO 339                                               SIMLE067          
      B=B+A(I,K)*A(K,J)                                                 SIMLE068          
      K=K+1                                                             SIMLE069          
      GOTO 331                                                          SIMLE070          
339   CONTINUE                                                          SIMLE071          
350   A(I,J)=(A(I,J)-B)/A(I,I)                                          SIMLE072          
      J=J+1                                                             SIMLE073          
      GOTO 301                                                          SIMLE074          
309   CONTINUE                                                          SIMLE075          
      II=II+1                                                           SIMLE076          
      GOTO 271                                                          SIMLE077          
279   CONTINUE                                                          SIMLE078          
      IRR=0                                                             SIMLE079          
380   RETURN                                                            SIMLE080          
      END                                                               SIMLE081          
