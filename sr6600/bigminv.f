      SUBROUTINE BIGMINV(N,M,ITAPE,JTAPE,IRR,DETERM)                    BIGMI002          
      DIMENSION B(60,60),C(60,60),D(60,60),F(60,60)                     BIGMI003          
      COMMON/ /B,C,D,F                                                  BIGMI004          
C     CONTAINS SUBROUTINES TAPE,MATINV                                  BIGMI005          
C     TO INVERT A VERY LARGE MATRIX BY THE METHOD OF PARTITIONS         BIGMI006          
C     N IS ORDER OF MATRIX,M IS THE PARTITION SIZE.                     BIGMI007          
C     ITAPE CONTAINS MATRIX,ONE BINARY RECORD PER PARTITION(COLUMNWISE) BIGMI008          
C     JTAPE CONTAINS INVERSE ON RETURN (CANNOT BE SAME AS ITAPE)        BIGMI009          
C     IRR=0 ON RETURN IF OK, =-1 IF DIAGONAL PARTITION GOES SINGULAR,   BIGMI010          
C     =-2 IF ITAPE OR JTAPE DEFINED AS LUN 50,51 , OR POSITIVE IF       BIGMI011          
C     A PARITY ERROR FOUND WHEN READING TAPE IRR.                       BIGMI012          
      JT=JTAPE                                                          BIGMI013          
      KT=50                                                             BIGMI014          
      LT=51                                                             BIGMI015          
      IF((KT.EQ.JT).OR.(KT.EQ.ITAPE).OR.(LT.EQ.JT).OR.(LT.EQ.ITAPE))    BIGMI016          
     .GOTO 1190                                                         BIGMI017          
      REWIND ITAPE                                                      BIGMI018          
      INO=0                                                             BIGMI019          
      DETERM=1.0                                                        BIGMI020          
C     NP**2 IS THE NUMBER OF PARTITIONS,EACH A M*M MATRIX EXCEPT LAST INBIGMI021          
C     EACH ROW OR COLUMN WHICH MAY BE LESS.                             BIGMI022          
      NP=(N+M-1)/M                                                      BIGMI023          
      MR=N-(NP-1)*M                                                     BIGMI024          
C     MAIN LOOP OF NP ITERATIONS                                        BIGMI025          
      NN=1                                                              BIGMI026          
111   IF(NN.GT.NP) GOTO 119                                             BIGMI027          
      REWIND JT                                                         BIGMI028          
C     K=M  OR LESS AT EDGES                                             BIGMI029          
      IF(NN-NP) 140,160,160                                             BIGMI030          
140   K=M                                                               BIGMI031          
      GOTO 170                                                          BIGMI032          
160   K=MR                                                              BIGMI033          
C     READ A0(NN,NN)  THE NEXT DIAGONAL PARTITION                       BIGMI034          
170   CALL TAPE(B,K,K,NP,NN,NN,ITAPE,INO,IRR)                           BIGMI035          
      IF(IRR) 1220,190,1220                                             BIGMI036          
190   NS=NN-1                                                           BIGMI037          
      IF(NS) 460,460,210                                                BIGMI038          
C     THIS LOOP WILL BE SKIPPED FOR NN=1                                BIGMI039          
210   REWIND LT                                                         BIGMI040          
      LNO=0                                                             BIGMI041          
      REWIND KT                                                         BIGMI042          
      IR=1                                                              BIGMI043          
241   IF(IR.GT.NS) GOTO 249                                             BIGMI044          
      I=1                                                               BIGMI045          
251   IF(I.GT.M) GOTO 259                                               BIGMI046          
      J=1                                                               BIGMI047          
261   IF(J.GT.K) GOTO 269                                               BIGMI048          
      C(I,J)=0.0                                                        BIGMI049          
      J=J+1                                                             BIGMI050          
      GOTO 261                                                          BIGMI051          
269   CONTINUE                                                          BIGMI052          
      I=I+1                                                             BIGMI053          
      GOTO 251                                                          BIGMI054          
259   CONTINUE                                                          BIGMI055          
      JS=1                                                              BIGMI056          
281   IF(JS.GT.NS) GOTO 289                                             BIGMI057          
C     READ ANN-1(IR,JS) AND A0(JS,NN)                                   BIGMI058          
      CALL TAPE(D,M,M,NS,IR,JS,LT,LNO,IRR)                              BIGMI059          
      IF(IRR) 1220,310,1220                                             BIGMI060          
310   CALL TAPE(F,M,K,NP,JS,NN,ITAPE,INO,IRR)                           BIGMI061          
      IF(IRR) 1220,330,1220                                             BIGMI062          
C     FORM SUM(JS)ANN-1(IR,JS)*A0(JS,NN)    IR=1,NN-1                   BIGMI063          
330   I=1                                                               BIGMI064          
331   IF(I.GT.M) GOTO 339                                               BIGMI065          
      J=1                                                               BIGMI066          
341   IF(J.GT.K) GOTO 349                                               BIGMI067          
      L=1                                                               BIGMI068          
351   IF(L.GT.M) GOTO 359                                               BIGMI069          
      C(I,J)=C(I,J)-D(I,L)*F(L,J)                                       BIGMI070          
      L=L+1                                                             BIGMI071          
      GOTO 351                                                          BIGMI072          
359   CONTINUE                                                          BIGMI073          
      J=J+1                                                             BIGMI074          
      GOTO 341                                                          BIGMI075          
349   CONTINUE                                                          BIGMI076          
      I=I+1                                                             BIGMI077          
      GOTO 331                                                          BIGMI078          
339   CONTINUE                                                          BIGMI079          
C     WRITE SUMS ON KT                                                  BIGMI080          
      JS=JS+1                                                           BIGMI081          
      GOTO 281                                                          BIGMI082          
289   CONTINUE                                                          BIGMI083          
      WRITE(KT)((C(I,J),I=1,M),J=1,K)                                   BIGMI084          
C     FORM A0(NN,NN)-SUM(IR)SUM(JS)(A0(NN,IR)*ANN-1(IR,JS)*A0(JS,NN))   BIGMI085          
      CALL TAPE(D,K,M,NP,NN,IR,ITAPE,INO,IRR)                           BIGMI086          
      IF(IRR) 1220,410,1220                                             BIGMI087          
410   I=1                                                               BIGMI088          
411   IF(I.GT.K) GOTO 419                                               BIGMI089          
      J=1                                                               BIGMI090          
421   IF(J.GT.K) GOTO 429                                               BIGMI091          
      L=1                                                               BIGMI092          
431   IF(L.GT.M) GOTO 439                                               BIGMI093          
      B(I,J)=B(I,J)+D(I,L)*C(L,J)                                       BIGMI094          
      L=L+1                                                             BIGMI095          
      GOTO 431                                                          BIGMI096          
439   CONTINUE                                                          BIGMI097          
      J=J+1                                                             BIGMI098          
      GOTO 421                                                          BIGMI099          
429   CONTINUE                                                          BIGMI100          
      I=I+1                                                             BIGMI101          
      GOTO 411                                                          BIGMI102          
419   CONTINUE                                                          BIGMI103          
C     INVERT TO FORM ANN(NN,NN)  AND DETERM                             BIGMI104          
      IR=IR+1                                                           BIGMI105          
      GOTO 241                                                          BIGMI106          
249   CONTINUE                                                          BIGMI107          
460   CALL MATINV(B,K,D,0,DET,IRR)                                      BIGMI108          
      DETERM=DETERM*DET                                                 BIGMI109          
      IF(IRR) 1210,490,1210                                             BIGMI110          
490   IF(NS) 940,940,500                                                BIGMI111          
C     THIS LOOP  WILL BE SKIPPED FOR NN=1                               BIGMI112          
500   REWIND LT                                                         BIGMI113          
      LNO=0                                                             BIGMI114          
      REWIND KT                                                         BIGMI115          
      JS=1                                                              BIGMI116          
531   IF(JS.GT.NS) GOTO 539                                             BIGMI117          
      I=1                                                               BIGMI118          
541   IF(I.GT.K) GOTO 549                                               BIGMI119          
      J=1                                                               BIGMI120          
551   IF(J.GT.M) GOTO 559                                               BIGMI121          
      C(I,J)=0.0                                                        BIGMI122          
      J=J+1                                                             BIGMI123          
      GOTO 551                                                          BIGMI124          
559   CONTINUE                                                          BIGMI125          
      I=I+1                                                             BIGMI126          
      GOTO 541                                                          BIGMI127          
549   CONTINUE                                                          BIGMI128          
      IR=1                                                              BIGMI129          
571   IF(IR.GT.NS) GOTO 579                                             BIGMI130          
C     READ A0(NN,IR) AND ANN-1(IR,JS)    IR=1,NN-1,JS=1,NN-1            BIGMI131          
      CALL TAPE(D,K,M,NP,NN,IR,ITAPE,INO,IRR)                           BIGMI132          
      IF(IRR) 1220,600,1220                                             BIGMI133          
600   CALL TAPE(F,M,M,NS,IR,JS,LT,LNO,IRR)                              BIGMI134          
      IF(IRR) 1220,620,1220                                             BIGMI135          
620   I=1                                                               BIGMI136          
621   IF(I.GT.K) GOTO 629                                               BIGMI137          
      J=1                                                               BIGMI138          
631   IF(J.GT.M) GOTO 639                                               BIGMI139          
      L=1                                                               BIGMI140          
641   IF(L.GT.M) GOTO 649                                               BIGMI141          
      C(I,J)=C(I,J)-D(I,L)*F(L,J)                                       BIGMI142          
      L=L+1                                                             BIGMI143          
      GOTO 641                                                          BIGMI144          
649   CONTINUE                                                          BIGMI145          
      J=J+1                                                             BIGMI146          
      GOTO 631                                                          BIGMI147          
639   CONTINUE                                                          BIGMI148          
      I=I+1                                                             BIGMI149          
      GOTO 621                                                          BIGMI150          
629   CONTINUE                                                          BIGMI151          
C     FORM  ANN(NN,JS)=-ANN(NN,NN)*SUM(IR)((A0(NN,IR)*ANN-1(IR,JS))     BIGMI152          
      IR=IR+1                                                           BIGMI153          
      GOTO 571                                                          BIGMI154          
579   CONTINUE                                                          BIGMI155          
      I=1                                                               BIGMI156          
671   IF(I.GT.K) GOTO 679                                               BIGMI157          
      J=1                                                               BIGMI158          
681   IF(J.GT.M) GOTO 689                                               BIGMI159          
      D(I,J)=0.0                                                        BIGMI160          
      L=1                                                               BIGMI161          
701   IF(L.GT.K) GOTO 709                                               BIGMI162          
      D(I,J)=D(I,J)+B(I,L)*C(L,J)                                       BIGMI163          
      L=L+1                                                             BIGMI164          
      GOTO 701                                                          BIGMI165          
709   CONTINUE                                                          BIGMI166          
      J=J+1                                                             BIGMI167          
      GOTO 681                                                          BIGMI168          
689   CONTINUE                                                          BIGMI169          
      I=I+1                                                             BIGMI170          
      GOTO 671                                                          BIGMI171          
679   CONTINUE                                                          BIGMI172          
      IR=1                                                              BIGMI173          
721   IF(IR.GT.NS) GOTO 729                                             BIGMI174          
C     FORM ANN(IR,JS)=ANN-1(IR,JS)-(SUM(S)(ANN-1(IR,S)*A0(S,NN)))*      BIGMI175          
C     ANN(NN,JS)     FOR IR=1,NN-1 , JS=1,NN-1  , AND WRITE ON JT.      BIGMI176          
      CALL TAPE(C,M,M,NS,IR,JS,LT,LNO,IRR)                              BIGMI177          
      IF(IRR) 1220,750,1220                                             BIGMI178          
750   READ(KT)((F(I,J),I=1,M),J=1,K)                                    BIGMI179          
      I=1                                                               BIGMI180          
761   IF(I.GT.M) GOTO 769                                               BIGMI181          
      J=1                                                               BIGMI182          
771   IF(J.GT.M) GOTO 779                                               BIGMI183          
      L=1                                                               BIGMI184          
781   IF(L.GT.K) GOTO 789                                               BIGMI185          
      C(I,J)=C(I,J)+F(I,L)*D(L,J)                                       BIGMI186          
      L=L+1                                                             BIGMI187          
      GOTO 781                                                          BIGMI188          
789   CONTINUE                                                          BIGMI189          
      J=J+1                                                             BIGMI190          
      GOTO 771                                                          BIGMI191          
779   CONTINUE                                                          BIGMI192          
      I=I+1                                                             BIGMI193          
      GOTO 761                                                          BIGMI194          
769   CONTINUE                                                          BIGMI195          
      WRITE(JT)((C(I,J),I=1,M),J=1,M)                                   BIGMI196          
C     WRITE  ANN(NN,JS)   JS=1,NN-1   ON JT.                            BIGMI197          
      IR=IR+1                                                           BIGMI198          
      GOTO 721                                                          BIGMI199          
729   CONTINUE                                                          BIGMI200          
      WRITE(JT)((D(I,J),I=1,K),J=1,M)                                   BIGMI201          
      REWIND KT                                                         BIGMI202          
      JS=JS+1                                                           BIGMI203          
      GOTO 531                                                          BIGMI204          
539   CONTINUE                                                          BIGMI205          
      IR=1                                                              BIGMI206          
851   IF(IR.GT.NS) GOTO 859                                             BIGMI207          
C     FORM  ANN(IR,NN)=-(SUM(JS)(ANN-1(IR,JS)*A0(JS,NN)))*ANN(NN,NN)    BIGMI208          
      READ(KT)((C(I,J),I=1,M),J=1,K)                                    BIGMI209          
      I=1                                                               BIGMI210          
871   IF(I.GT.M) GOTO 879                                               BIGMI211          
      J=1                                                               BIGMI212          
881   IF(J.GT.K) GOTO 889                                               BIGMI213          
      D(I,J)=0.0                                                        BIGMI214          
      L=1                                                               BIGMI215          
901   IF(L.GT.K) GOTO 909                                               BIGMI216          
      D(I,J)=D(I,J)+C(I,L)*B(L,J)                                       BIGMI217          
C     WRITE ON JT.                                                      BIGMI218          
      L=L+1                                                             BIGMI219          
      GOTO 901                                                          BIGMI220          
909   CONTINUE                                                          BIGMI221          
      J=J+1                                                             BIGMI222          
      GOTO 881                                                          BIGMI223          
889   CONTINUE                                                          BIGMI224          
      I=I+1                                                             BIGMI225          
      GOTO 871                                                          BIGMI226          
879   CONTINUE                                                          BIGMI227          
      WRITE(JT)((D(I,J),I=1,M),J=1,K)                                   BIGMI228          
C     WRITE ANN(NN,NN)  ON JT                                           BIGMI229          
      IR=IR+1                                                           BIGMI230          
      GOTO 851                                                          BIGMI231          
859   CONTINUE                                                          BIGMI232          
940   WRITE(JT)((B(I,J),I=1,K),J=1,K)                                   BIGMI233          
C     SWAP LT AND JT SO THAT OLD INPUT BECOMES OUTPUT AND VICE-VERSA.   BIGMI234          
      JL=JT                                                             BIGMI235          
      JT=LT                                                             BIGMI236          
      LT=JL                                                             BIGMI237          
C     INVERSION COMPLETE, CHECK FOR INVERSE ON JTAPE                    BIGMI238          
      NN=NN+1                                                           BIGMI239          
      GOTO 111                                                          BIGMI240          
119   CONTINUE                                                          BIGMI241          
      REWIND LT                                                         BIGMI242          
      IF(LT-JTAPE) 1010,1170,1010                                       BIGMI243          
C     NO , COPY LT ONTO JTAPE.                                          BIGMI244          
1010  REWIND JTAPE                                                      BIGMI245          
      IS=1                                                              BIGMI246          
1021  IF(IS.GT.NP) GOTO 1029                                            BIGMI247          
      IF(IS-NP) 1040,1060,1060                                          BIGMI248          
1040  K=M                                                               BIGMI249          
      GOTO 1070                                                         BIGMI250          
1060  K=MR                                                              BIGMI251          
1070  IR=1                                                              BIGMI252          
1071  IF(IR.GT.NP) GOTO 1079                                            BIGMI253          
      IF(IR-NP) 1090,1110,1110                                          BIGMI254          
1090  L=M                                                               BIGMI255          
      GOTO 1120                                                         BIGMI256          
1110  L=MR                                                              BIGMI257          
1120  CONTINUE                                                          BIGMI258          
      READ(LT)((B(I,J),I=1,L),J=1,K)                                    BIGMI259          
      WRITE(JTAPE)((B(I,J),I=1,L),J=1,K)                                BIGMI260          
      IR=IR+1                                                           BIGMI261          
      GOTO 1071                                                         BIGMI262          
1079  CONTINUE                                                          BIGMI263          
      IS=IS+1                                                           BIGMI264          
      GOTO 1021                                                         BIGMI265          
1029  CONTINUE                                                          BIGMI266          
      REWIND JTAPE                                                      BIGMI267          
C     NORMAL EXIT                                                       BIGMI268          
1170  IRR=0                                                             BIGMI269          
      GOTO 1230                                                         BIGMI270          
C     DOUBLY DEFINED TAPE NUMBERS                                       BIGMI271          
1190  IRR=-2                                                            BIGMI272          
      GOTO 1230                                                         BIGMI273          
C     MATRIX PARTITION SINGULAR                                         BIGMI274          
1210  IRR=-1                                                            BIGMI275          
C     IRR +VE FOR ERROR ON TAPE IRR                                     BIGMI276          
1220  CONTINUE                                                          BIGMI277          
1230  RETURN                                                            BIGMI278          
      END                                                               BIGMI279          
      SUBROUTINE TAPE(B,K,L,NP,NI,NJ,ITAPE,INO,IRR)                     BIGMI280          
      DIMENSION B(60,60)                                                BIGMI281          
      COMMON/KQZCOM/KQZSBX,KQZB00,KQZB06                                BIGMI282          
C     TO READ A SPECIFIED RECORD FROM A PARTITIONED MATRIX TAPE         BIGMI283          
C     NP**2 PERTITIONS ON ITAPE,(NI,NJ)TH ONE REQUIRED(A K*L MATRIX),   BIGMI284          
C     IJ IS NEW RECORD REQUIRED,INO IS LAST ONE FOUND.                  BIGMI285          
      IJ=NP*(NJ-1)+NI                                                   BIGMI286          
      II=IJ-INO-1                                                       BIGMI287          
      IF(II) 120,150,50                                                 BIGMI288          
C     SPACE FORWARD II RECORDS                                          BIGMI289          
50    I=1                                                               BIGMI290          
51    IF(I.GT.II) GOTO 59                                               BIGMI291          
      READ(ITAPE)                                                       BIGMI292          
      CALL KQZCHK(ITAPE)                                                BIGMI293          
      GOTO(100,80),KQZSBX                                               BIGMI294          
80    CONTINUE                                                          BIGMI295          
      I=I+1                                                             BIGMI296          
      GOTO 51                                                           BIGMI297          
59    CONTINUE                                                          BIGMI298          
      GOTO 150                                                          BIGMI299          
C     ERROR ON ITAPE                                                    BIGMI300          
100   IRR=ITAPE                                                         BIGMI301          
      GOTO 200                                                          BIGMI302          
120   II=-II                                                            BIGMI303          
C     BACKSPACE II RECORDS                                              BIGMI304          
      I=1                                                               BIGMI305          
131   IF(I.GT.II) GOTO 139                                              BIGMI306          
      BACKSPACE ITAPE                                                   BIGMI307          
      I=I+1                                                             BIGMI308          
      GOTO 131                                                          BIGMI309          
139   CONTINUE                                                          BIGMI310          
150   READ(ITAPE)((B(I,J),I=1,K),J=1,L)                                 BIGMI311          
      CALL KQZCHK(ITAPE)                                                BIGMI312          
      GOTO(100,170),KQZSBX                                              BIGMI313          
170   INO=IJ                                                            BIGMI314          
      IRR=0                                                             BIGMI315          
200   RETURN                                                            BIGMI316          
      END                                                               BIGMI317          
