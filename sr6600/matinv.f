      SUBROUTINE MATINV(A,N,B,L,D,IRROR)                                MATIN002          
      DIMENSION A(6,6),B(6,1),IPIV(6),IND(6,2)                          MATIN003          
C     A IS AN NXN MATRIX TO BE INVERTED,OR CONTAINING EQUATION COEFFS   MATIN004          
C     B IS AN NXM RHS MATRIX FOR EQUATIONS                              MATIN005          
C     IF L=0,INVERSE ONLY GIVEN.L POSITIVE,SOLUTIONS ONLY.L NEGATIVE    MATIN006          
C      BOTH.   M=ABS(L).                                                MATIN007          
C     D CONTAINS THE DETERMINANT OF THE A MATRIX ON EXIT                MATIN008          
C     A IS REPLACED BY THE INVERSE ,B BY THE SOLUTIONS.                 MATIN009          
C     METHOD OF GAUSS-JORDON PIVOTAL ELIMINATION                        MATIN010          
      M=IABS(L)                                                         MATIN011          
      D=1.0                                                             MATIN012          
      I=1                                                               MATIN013          
41    IF(I.GT.N) GOTO 49                                                MATIN014          
      IPIV(I)=0                                                         MATIN015          
      I=I+1                                                             MATIN016          
      GOTO 41                                                           MATIN017          
49    CONTINUE                                                          MATIN018          
      I=1                                                               MATIN019          
61    IF(I.GT.N) GOTO 69                                                MATIN020          
      AMAX=0.0                                                          MATIN021          
C     SEARCH SUB-MATRIX FOR LARGEST ELEMENT AS PIVOT                    MATIN022          
      J=1                                                               MATIN023          
81    IF(J.GT.N) GOTO 89                                                MATIN024          
      IF(IPIV(J)) 200,100,170                                           MATIN025          
100   K=1                                                               MATIN026          
101   IF(K.GT.N) GOTO 109                                               MATIN027          
      IF(IPIV(K)-1) 120,160,200                                         MATIN028          
C     THIS ROW COLUMN HAS NOT BEEN A PIVOT                              MATIN029          
120   IF(ABS(A(J,K))-AMAX) 160,160,130                                  MATIN030          
130   IROW=J                                                            MATIN031          
      ICOL=K                                                            MATIN032          
      AMAX=ABS(A(J,K))                                                  MATIN033          
160   CONTINUE                                                          MATIN034          
      K=K+1                                                             MATIN035          
      GOTO 101                                                          MATIN036          
109   CONTINUE                                                          MATIN037          
170   CONTINUE                                                          MATIN038          
C     PIVOT FOUND                                                       MATIN039          
      J=J+1                                                             MATIN040          
      GOTO 81                                                           MATIN041          
89    CONTINUE                                                          MATIN042          
      IPIV(ICOL)=IPIV(ICOL)+1                                           MATIN043          
      IF(AMAX-1.0E-90) 200,200,220                                      MATIN044          
C     MATRIX SINGULAR,ERROR RETURN                                      MATIN045          
200   IRROR=1                                                           MATIN046          
      GOTO 660                                                          MATIN047          
220   IF(IROW-ICOL) 230,330,230                                         MATIN048          
C     MAKE PIVOT A DIAGONAL ELEMENT BY ROW INTERCHANGE.                 MATIN049          
230   D=-D                                                              MATIN050          
      K=1                                                               MATIN051          
241   IF(K.GT.N) GOTO 249                                               MATIN052          
      AMAX=A(IROW,K)                                                    MATIN053          
      A(IROW,K)=A(ICOL,K)                                               MATIN054          
      A(ICOL,K)=AMAX                                                    MATIN055          
      K=K+1                                                             MATIN056          
      GOTO 241                                                          MATIN057          
249   CONTINUE                                                          MATIN058          
      IF(M) 330,330,290                                                 MATIN059          
290   K=1                                                               MATIN060          
291   IF(K.GT.M) GOTO 299                                               MATIN061          
      AMAX=B(IROW,K)                                                    MATIN062          
      B(IROW,K)=B(ICOL,K)                                               MATIN063          
      B(ICOL,K)=AMAX                                                    MATIN064          
      K=K+1                                                             MATIN065          
      GOTO 291                                                          MATIN066          
299   CONTINUE                                                          MATIN067          
330   IND(I,1)=IROW                                                     MATIN068          
      IND(I,2)=ICOL                                                     MATIN069          
      AMAX=A(ICOL,ICOL)                                                 MATIN070          
      D=D*AMAX                                                          MATIN071          
      A(ICOL,ICOL)=1.0                                                  MATIN072          
C     DIVIDE PIVOT ROW BY PIVOT                                         MATIN073          
      K=1                                                               MATIN074          
381   IF(K.GT.N) GOTO 389                                               MATIN075          
      A(ICOL,K)=A(ICOL,K)/AMAX                                          MATIN076          
      K=K+1                                                             MATIN077          
      GOTO 381                                                          MATIN078          
389   CONTINUE                                                          MATIN079          
      IF(M) 430,430,410                                                 MATIN080          
410   K=1                                                               MATIN081          
411   IF(K.GT.M) GOTO 419                                               MATIN082          
      B(ICOL,K)=B(ICOL,K)/AMAX                                          MATIN083          
C     REDUCE NON-PIVOT ROWS                                             MATIN084          
      K=K+1                                                             MATIN085          
      GOTO 411                                                          MATIN086          
419   CONTINUE                                                          MATIN087          
430   J=1                                                               MATIN088          
431   IF(J.GT.N) GOTO 439                                               MATIN089          
      IF(J-ICOL) 450,520,450                                            MATIN090          
450   AMAX=A(J,ICOL)                                                    MATIN091          
      A(J,ICOL)=0.0                                                     MATIN092          
      K=1                                                               MATIN093          
471   IF(K.GT.N) GOTO 479                                               MATIN094          
      A(J,K)=A(J,K)-A(ICOL,K)*AMAX                                      MATIN095          
      K=K+1                                                             MATIN096          
      GOTO 471                                                          MATIN097          
479   CONTINUE                                                          MATIN098          
      IF(M) 520,520,500                                                 MATIN099          
500   K=1                                                               MATIN100          
501   IF(K.GT.M) GOTO 509                                               MATIN101          
      B(J,K)=B(J,K)-B(ICOL,K)*AMAX                                      MATIN102          
      K=K+1                                                             MATIN103          
      GOTO 501                                                          MATIN104          
509   CONTINUE                                                          MATIN105          
520   CONTINUE                                                          MATIN106          
C     AFTER N PIVOTAL CONDENSATIONS,SOLUTIONS LIE IN B MATRIX           MATIN107          
      J=J+1                                                             MATIN108          
      GOTO 431                                                          MATIN109          
439   CONTINUE                                                          MATIN110          
      I=I+1                                                             MATIN111          
      GOTO 61                                                           MATIN112          
69    CONTINUE                                                          MATIN113          
      IF(L) 540,540,640                                                 MATIN114          
C     FOR INVERSE OF A, INTERCHANGE COLUMNS                             MATIN115          
540   I=1                                                               MATIN116          
541   IF(I.GT.N) GOTO 549                                               MATIN117          
      J=N+1-I                                                           MATIN118          
      IF(IND(J,1)-IND(J,2)) 570,630,570                                 MATIN119          
570   IROW=IND(J,1)                                                     MATIN120          
      ICOL=IND(J,2)                                                     MATIN121          
      K=1                                                               MATIN122          
591   IF(K.GT.N) GOTO 599                                               MATIN123          
      AMAX=A(K,IROW)                                                    MATIN124          
      A(K,IROW)=A(K,ICOL)                                               MATIN125          
      A(K,ICOL)=AMAX                                                    MATIN126          
      K=K+1                                                             MATIN127          
      GOTO 591                                                          MATIN128          
599   CONTINUE                                                          MATIN129          
630   CONTINUE                                                          MATIN130          
      I=I+1                                                             MATIN131          
      GOTO 541                                                          MATIN132          
549   CONTINUE                                                          MATIN133          
640   IRROR=0                                                           MATIN134          
660   RETURN                                                            MATIN135          
      END                                                               MATIN136          
