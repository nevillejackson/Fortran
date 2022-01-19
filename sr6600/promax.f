      SUBROUTINE PROMAX(F,W,M,N,KVALUE,P,C)                             PROMA002          
      DIMENSION F(M,N),P(M,N),C(N,N),W(M,N),X(800),Y(800),LDUM(10),KDUM PROMA003          
     .(10)                                                              PROMA004          
C     J.C. MUDGE, SEP.1967                                              PROMA005          
C     METHOD OF HENDRICKSON AND WHITE                                   PROMA006          
C     BRIT. J. STAT. PSYCH. 57, 65, (1964)                              PROMA007          
C                                                                       PROMA008          
C     INITIALIZE                                                        PROMA009          
C     PUT F IN P AND F TRANSPOSE IN X                                   PROMA010          
      I=1                                                               PROMA011          
21    IF(I.GT.M) GOTO 29                                                PROMA012          
      J=1                                                               PROMA013          
31    IF(J.GT.N) GOTO 39                                                PROMA014          
      P(I,J)=F(I,J)                                                     PROMA015          
      KQZ001=J+N*(I-1)                                                  PROMA016          
      X(KQZ001)=F(I,J)                                                  PROMA017          
C     ROW-COLUMN NORMALIZE P                                            PROMA018          
      J=J+1                                                             PROMA019          
      GOTO 31                                                           PROMA020          
39    CONTINUE                                                          PROMA021          
      I=I+1                                                             PROMA022          
      GOTO 21                                                           PROMA023          
29    CONTINUE                                                          PROMA024          
      CALL NORM(P,M,N,0)                                                PROMA025          
C     FORM IDEAL PATTERN MATRIX IN P                                    PROMA026          
      I=1                                                               PROMA027          
71    IF(I.GT.M) GOTO 79                                                PROMA028          
      J=1                                                               PROMA029          
81    IF(J.GT.N) GOTO 89                                                PROMA030          
      IF(P(I,J)) 100,110,100                                            PROMA031          
100   P(I,J)=ABS(P(I,J)**(KVALUE+1))/P(I,J)                             PROMA032          
110   CONTINUE                                                          PROMA033          
C     COMPUTE UNNORMALIZED TRANSFORMATION MATRIX OF REFERENCE VECTOR    PROMA034          
C     STRUCTURE BY PROCRUSTES EQUATION                                  PROMA035          
      J=J+1                                                             PROMA036          
      GOTO 81                                                           PROMA037          
89    CONTINUE                                                          PROMA038          
      I=I+1                                                             PROMA039          
      GOTO 71                                                           PROMA040          
79    CONTINUE                                                          PROMA041          
      CALL MULT(X,P,W,N,M,N)                                            PROMA042          
      CALL MULT(X,F,C,N,M,N)                                            PROMA043          
      CALL BMDX28(C,N,N,LDUM,KDUM)                                      PROMA044          
      CALL MULT(C,W,X,N,N,N)                                            PROMA045          
C                                                                       PROMA046          
C     NORMALIZE COLUMNS OF TRANSFORMATION MATRIX                        PROMA047          
      CALL NORM(X,N,N,1)                                                PROMA048          
      CALL MULT(F,X,P,M,N,N)                                            PROMA049          
C     STORE P, THE OBLIQUE REFERENCE VECTORS, IN F FOR EXIT             PROMA050          
      I=1                                                               PROMA051          
181   IF(I.GT.M) GOTO 189                                               PROMA052          
      J=1                                                               PROMA053          
191   IF(J.GT.N) GOTO 199                                               PROMA054          
      F(I,J)=P(I,J)                                                     PROMA055          
C     COMMENCE TRANSLATION TO PRIMARY FACTOR PATTERN                    PROMA056          
      J=J+1                                                             PROMA057          
      GOTO 191                                                          PROMA058          
199   CONTINUE                                                          PROMA059          
      I=I+1                                                             PROMA060          
      GOTO 181                                                          PROMA061          
189   CONTINUE                                                          PROMA062          
      CALL BMDX28(X,N,N,LDUM,KDUM)                                      PROMA063          
      CALL MULT(P,X,W,M,N,N)                                            PROMA064          
C     NORMALIZE BY ROWS                                                 PROMA065          
      CALL NORM(X,N,N,-1)                                               PROMA066          
      I=1                                                               PROMA067          
241   IF(I.GT.N) GOTO 249                                               PROMA068          
      J=1                                                               PROMA069          
251   IF(J.GT.N) GOTO 259                                               PROMA070          
      KQZ001=I+N*(J-1)                                                  PROMA071          
      KQZ002=J+N*(I-1)                                                  PROMA072          
      Y(KQZ001)=X(KQZ002)                                               PROMA073          
C     FORM MATRIX OF PRIMARY FACTOR CORRELATIONS IN C FOR EXIT          PROMA074          
      J=J+1                                                             PROMA075          
      GOTO 251                                                          PROMA076          
259   CONTINUE                                                          PROMA077          
      I=I+1                                                             PROMA078          
      GOTO 241                                                          PROMA079          
249   CONTINUE                                                          PROMA080          
      CALL MULT(X,Y,C,N,N,N)                                            PROMA081          
      CALL BMDX28(X,N,N,LDUM,KDUM)                                      PROMA082          
C     FORM PRIMARY FACTOR PATTERN MATRIX IN P FOR EXIT                  PROMA083          
      CALL MULT(W,X,P,M,N,N)                                            PROMA084          
      ERR=0                                                             PROMA085          
      RETURN                                                            PROMA086          
      END                                                               PROMA087          
      SUBROUTINE NORM(A,M,N,IND)                                        PROMA088          
      DIMENSION RNORM(100),CNORM(10),A(M,N)                             PROMA089          
C     MATRIX A (MXN) IS NORMALIZED BY ROWS(IF IND IS NEGATIVE),BY COLS  PROMA090          
C     (IND POSITIVE) OR BOTH(IND ZERO).                                 PROMA091          
      IF(IND) 90,30,30                                                  PROMA092          
C     FIND COLUMN NORMS                                                 PROMA093          
30    J=1                                                               PROMA094          
31    IF(J.GT.N) GOTO 39                                                PROMA095          
      W=0.                                                              PROMA096          
      I=1                                                               PROMA097          
51    IF(I.GT.M) GOTO 59                                                PROMA098          
      W=W+A(I,J)*A(I,J)                                                 PROMA099          
      I=I+1                                                             PROMA100          
      GOTO 51                                                           PROMA101          
59    CONTINUE                                                          PROMA102          
      CNORM(J)=SQRT(W)                                                  PROMA103          
      J=J+1                                                             PROMA104          
      GOTO 31                                                           PROMA105          
39    CONTINUE                                                          PROMA106          
      IF(IND) 90,90,200                                                 PROMA107          
C     FIND ROW NORMS                                                    PROMA108          
90    I=1                                                               PROMA109          
91    IF(I.GT.M) GOTO 99                                                PROMA110          
      W=0.                                                              PROMA111          
      J=1                                                               PROMA112          
111   IF(J.GT.N) GOTO 119                                               PROMA113          
      W=W+A(I,J)*A(I,J)                                                 PROMA114          
      J=J+1                                                             PROMA115          
      GOTO 111                                                          PROMA116          
119   CONTINUE                                                          PROMA117          
      RNORM(I)=SQRT(W)                                                  PROMA118          
C     NORMALIZE MATRIX                                                  PROMA119          
      I=I+1                                                             PROMA120          
      GOTO 91                                                           PROMA121          
99    CONTINUE                                                          PROMA122          
      I=1                                                               PROMA123          
141   IF(I.GT.M) GOTO 149                                               PROMA124          
      IF(RNORM(I)) 170,160,170                                          PROMA125          
160   RNORM(I)=1.                                                       PROMA126          
170   J=1                                                               PROMA127          
171   IF(J.GT.N) GOTO 179                                               PROMA128          
      A(I,J)=A(I,J)/RNORM(I)                                            PROMA129          
      J=J+1                                                             PROMA130          
      GOTO 171                                                          PROMA131          
179   CONTINUE                                                          PROMA132          
      I=I+1                                                             PROMA133          
      GOTO 141                                                          PROMA134          
149   CONTINUE                                                          PROMA135          
      IF(IND) 250,200,200                                               PROMA136          
200   I=1                                                               PROMA137          
201   IF(I.GT.M) GOTO 209                                               PROMA138          
      IF(CNORM(J)) 230,220,230                                          PROMA139          
220   CNORM(J)=1.                                                       PROMA140          
230   J=1                                                               PROMA141          
231   IF(J.GT.N) GOTO 239                                               PROMA142          
      A(I,J)=A(I,J)/CNORM(J)                                            PROMA143          
      J=J+1                                                             PROMA144          
      GOTO 231                                                          PROMA145          
239   CONTINUE                                                          PROMA146          
      I=I+1                                                             PROMA147          
      GOTO 201                                                          PROMA148          
209   CONTINUE                                                          PROMA149          
250   CONTINUE                                                          PROMA150          
      RETURN                                                            PROMA151          
      END                                                               PROMA152          
      SUBROUTINE MULT(A,B,C,M,N,I)                                      PROMA153          
      DIMENSION A(M,N),B(N,I),C(M,I)                                    PROMA154          
C     THE PRODUCT OF MATRICES A AND B IS PUT IN C                       PROMA155          
      K=1                                                               PROMA156          
21    IF(K.GT.M) GOTO 29                                                PROMA157          
      L=1                                                               PROMA158          
31    IF(L.GT.I) GOTO 39                                                PROMA159          
      C(K,L)=0.                                                         PROMA160          
      J=1                                                               PROMA161          
51    IF(J.GT.N) GOTO 59                                                PROMA162          
      C(K,L)=C(K,L)+A(K,J)*B(J,L)                                       PROMA163          
      J=J+1                                                             PROMA164          
      GOTO 51                                                           PROMA165          
59    CONTINUE                                                          PROMA166          
      L=L+1                                                             PROMA167          
      GOTO 31                                                           PROMA168          
39    CONTINUE                                                          PROMA169          
      K=K+1                                                             PROMA170          
      GOTO 21                                                           PROMA171          
29    CONTINUE                                                          PROMA172          
      RETURN                                                            PROMA173          
      END                                                               PROMA174          
      SUBROUTINE BMDX28(A,N,M,L,K)                                      PROMA175          
      DIMENSION A(M,N),L(N),K(N)                                        PROMA176          
      EQUIVALENCE(ICHANG,JCHANG,KCHANG)                                 PROMA177          
CBMDX28      SUBROUTINE BMDX28 FOR BMD03M                         9-3-63PROMA178          
C       *** 3600 SCOPE VERSION ***                                      PROMA179          
C                                      BMDX28                           PROMA180          
C               HEALTH SCIENCES COMPUTING FACILITY                      PROMA181          
C                      UCLA MEDICAL SCHOOL                              PROMA182          
C     SUBROUTINE BMDX28(A,N,M,L,K). A FORTRAN PROGRAM FOR INVERTING AN  PROMA183          
C     NXN MATRIX STORED IN AN MXJ STORAGE BLOCK, WHERE M AND J, NOT     PROMA184          
C     NECESSARILY EQUAL, ARE GREATER THAN OR EQUAL TO N.                PROMA185          
      KAS=1                                                             PROMA186          
21    IF(KAS.GT.N) GOTO 29                                              PROMA187          
      MODUAL=(KAS-1)*M                                                  PROMA188          
      NUINDX=KAS+MODUAL                                                 PROMA189          
      L(KAS)=KAS                                                        PROMA190          
      K(KAS)=KAS                                                        PROMA191          
      BIGA=A(NUINDX,1)                                                  PROMA192          
      I=KAS                                                             PROMA193          
81    IF(I.GT.N) GOTO 89                                                PROMA194          
      J=KAS                                                             PROMA195          
91    IF(J.GT.N) GOTO 99                                                PROMA196          
      NOTHRX=I+(J-1)*M                                                  PROMA197          
      IF(ABS(BIGA)-ABS(A(NOTHRX,1))) 120,150,150                        PROMA198          
120   BIGA=A(NOTHRX,1)                                                  PROMA199          
      L(KAS)=I                                                          PROMA200          
      K(KAS)=J                                                          PROMA201          
150   CONTINUE                                                          PROMA202          
C     INTERCHANGE ROWS                                                  PROMA203          
      J=J+1                                                             PROMA204          
      GOTO 91                                                           PROMA205          
99    CONTINUE                                                          PROMA206          
      I=I+1                                                             PROMA207          
      GOTO 81                                                           PROMA208          
89    CONTINUE                                                          PROMA209          
      J=L(KAS)                                                          PROMA210          
      IF(L(KAS)-KAS) 250,250,180                                        PROMA211          
180   I=1                                                               PROMA212          
181   IF(I.GT.N) GOTO 189                                               PROMA213          
      ICHANG=(I-1)*M                                                    PROMA214          
      NOTHRX=KAS+ICHANG                                                 PROMA215          
      NEXTIN=J+ICHANG                                                   PROMA216          
      HOLD=-A(NOTHRX,1)                                                 PROMA217          
      A(NOTHRX,1)=A(NEXTIN,1)                                           PROMA218          
      A(NEXTIN,1)=HOLD                                                  PROMA219          
C     INTERCHANGE COLUMNS                                               PROMA220          
      I=I+1                                                             PROMA221          
      GOTO 181                                                          PROMA222          
189   CONTINUE                                                          PROMA223          
250   I=K(KAS)-1                                                        PROMA224          
      IF(K(KAS)-KAS) 340,340,270                                        PROMA225          
270   ICHANG=I*M                                                        PROMA226          
      J=1                                                               PROMA227          
281   IF(J.GT.N) GOTO 289                                               PROMA228          
      NOTHRX=J+ICHANG                                                   PROMA229          
      NEXTIN=J+MODUAL                                                   PROMA230          
      HOLD=-A(NEXTIN,1)                                                 PROMA231          
      A(NEXTIN,1)=A(NOTHRX,1)                                           PROMA232          
      A(NOTHRX,1)=HOLD                                                  PROMA233          
C     DIVIDE COLUMN BY MINUS PIVOT                                      PROMA234          
      J=J+1                                                             PROMA235          
      GOTO 281                                                          PROMA236          
289   CONTINUE                                                          PROMA237          
340   I=1                                                               PROMA238          
341   IF(I.GT.N) GOTO 349                                               PROMA239          
      IF(I-KAS) 360,380,360                                             PROMA240          
360   NOTHRX=I+MODUAL                                                   PROMA241          
      A(NOTHRX,1)=A(NOTHRX,1)/(-A(NUINDX,1))                            PROMA242          
380   CONTINUE                                                          PROMA243          
C     REDUCE MATRIX                                                     PROMA244          
      I=I+1                                                             PROMA245          
      GOTO 341                                                          PROMA246          
349   CONTINUE                                                          PROMA247          
      I=1                                                               PROMA248          
391   IF(I.GT.N) GOTO 399                                               PROMA249          
      J=1                                                               PROMA250          
401   IF(J.GT.N) GOTO 409                                               PROMA251          
      IF(I-KAS) 420,480,420                                             PROMA252          
420   IF(J-KAS) 430,480,430                                             PROMA253          
430   NOTHRX=I+MODUAL                                                   PROMA254          
      JCHANG=(J-1)*M                                                    PROMA255          
      NEXTIN=I+JCHANG                                                   PROMA256          
      KCHANG=KAS+JCHANG                                                 PROMA257          
      A(NEXTIN,1)=A(NOTHRX,1)*A(KCHANG,1)+A(NEXTIN,1)                   PROMA258          
480   CONTINUE                                                          PROMA259          
C     DIVIDE ROW BY PIVOT                                               PROMA260          
      J=J+1                                                             PROMA261          
      GOTO 401                                                          PROMA262          
409   CONTINUE                                                          PROMA263          
      I=I+1                                                             PROMA264          
      GOTO 391                                                          PROMA265          
399   CONTINUE                                                          PROMA266          
      J=1                                                               PROMA267          
491   IF(J.GT.N) GOTO 499                                               PROMA268          
      IF(J-KAS) 510,530,510                                             PROMA269          
510   NOTHRX=KAS+(J-1)*M                                                PROMA270          
      A(NOTHRX,1)=A(NOTHRX,1)/A(NUINDX,1)                               PROMA271          
530   CONTINUE                                                          PROMA272          
C     REPLACE PIVOT BY RECIPROCAL                                       PROMA273          
      J=J+1                                                             PROMA274          
      GOTO 491                                                          PROMA275          
499   CONTINUE                                                          PROMA276          
      A(NUINDX,1)=1.0/A(NUINDX,1)                                       PROMA277          
C     FINAL ROW AND COLUMN INTERCHANGE                                  PROMA278          
      KAS=KAS+1                                                         PROMA279          
      GOTO 21                                                           PROMA280          
29    CONTINUE                                                          PROMA281          
      KAS=N                                                             PROMA282          
570   KAS=(KAS-1)                                                       PROMA283          
      IF(KAS) 790,790,590                                               PROMA284          
590   I=L(KAS)                                                          PROMA285          
      IF(I-KAS) 690,690,610                                             PROMA286          
610   ICHANG=(I-1)*M                                                    PROMA287          
      NUINDX=(KAS-1)*M                                                  PROMA288          
      J=1                                                               PROMA289          
631   IF(J.GT.N) GOTO 639                                               PROMA290          
      NOTHRX=J+NUINDX                                                   PROMA291          
      NEXTIN=J+ICHANG                                                   PROMA292          
      HOLD=A(NOTHRX,1)                                                  PROMA293          
      A(NOTHRX,1)=-A(NEXTIN,1)                                          PROMA294          
      A(NEXTIN,1)=HOLD                                                  PROMA295          
      J=J+1                                                             PROMA296          
      GOTO 631                                                          PROMA297          
639   CONTINUE                                                          PROMA298          
690   J=K(KAS)                                                          PROMA299          
      IF(J-KAS) 570,570,710                                             PROMA300          
710   I=1                                                               PROMA301          
711   IF(I.GT.N) GOTO 719                                               PROMA302          
      ICHANG=(I-1)*M                                                    PROMA303          
      NUINDX=KAS+ICHANG                                                 PROMA304          
      NOTHRX=J+ICHANG                                                   PROMA305          
      HOLD=A(NUINDX,1)                                                  PROMA306          
      A(NUINDX,1)=-A(NOTHRX,1)                                          PROMA307          
      A(NOTHRX,1)=HOLD                                                  PROMA308          
      I=I+1                                                             PROMA309          
      GOTO 711                                                          PROMA310          
719   CONTINUE                                                          PROMA311          
      GOTO 570                                                          PROMA312          
790   CONTINUE                                                          PROMA313          
      RETURN                                                            PROMA314          
      END                                                               PROMA315          
