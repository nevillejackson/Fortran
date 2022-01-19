      SUBROUTINE SYMINV(A,N,B,MM,DET,IRR)                               SYMIN002          
      DIMENSION A(1000),B(50,1),IP(50)                                  SYMIN003          
C     A CONTAINS THE LOWER HALF OF A NXN SYMMETRIC MATRIX STORED COLUMN-SYMIN004          
C     WISE,I.E HAS DIMENSION N(N+1)/2                                   SYMIN005          
C     B CONTAINS M RHS VECTORS, TO BE REPLACED BY M SOLUTIONS(M MAY=0)  SYMIN006          
C     IF MM=0,INVERSE ONLY IS GIVEN.MM POSITIVE SOLUTIONS ONLY.MM NEGA- SYMIN007          
C     TIVE,BOTH.   M=ABS(MM).                                           SYMIN008          
C     THE LOWER HALF OF THE INVERSE MATRIX REPLACES A                   SYMIN009          
C     DET WILL CONTAIN THE DETERMINANT OF A.                            SYMIN010          
      M=IABS(MM)                                                        SYMIN011          
      DET=1.0                                                           SYMIN012          
      I=1                                                               SYMIN013          
41    IF(I.GT.N) GOTO 49                                                SYMIN014          
C     CHOOSE LARGEST DIAGONAL ELEMENT AS PIVOT                          SYMIN015          
      AMAX=0.                                                           SYMIN016          
      II=N*(I-1)-I*(I-3)/2                                              SYMIN017          
      K=II                                                              SYMIN018          
      J=I                                                               SYMIN019          
81    IF(J.GT.N) GOTO 89                                                SYMIN020          
      IF(ABS(A(K))-AMAX) 130,130,100                                    SYMIN021          
100   JJ=K                                                              SYMIN022          
      IP(I)=J                                                           SYMIN023          
      AMAX=ABS(A(K))                                                    SYMIN024          
130   K=K+N-J+1                                                         SYMIN025          
C     TEST FOR PIVOT TOO SMALL                                          SYMIN026          
      J=J+1                                                             SYMIN027          
      GOTO 81                                                           SYMIN028          
89    CONTINUE                                                          SYMIN029          
      IF(AMAX-1.0E-90) 150,150,170                                      SYMIN030          
150   IRR=1                                                             SYMIN031          
      GOTO 1480                                                         SYMIN032          
170   DET=DET*A(JJ)                                                     SYMIN033          
C     SWAP ROWS AND COLUMNS SO THAT A(I,I) IS PIVOT,KEEPING SYMMETRY    SYMIN034          
      JI=IP(I)-I                                                        SYMIN035          
      IF(JI) 520,520,200                                                SYMIN036          
200   K=II                                                              SYMIN037          
      KK=K+N-I+JI                                                       SYMIN038          
      L=1                                                               SYMIN039          
221   IF(L.GT.JI) GOTO 229                                              SYMIN040          
      K=K+1                                                             SYMIN041          
      AMAX=A(K)                                                         SYMIN042          
      A(K)=A(KK)                                                        SYMIN043          
      A(KK)=AMAX                                                        SYMIN044          
      KK=KK+N-I-L                                                       SYMIN045          
      L=L+1                                                             SYMIN046          
      GOTO 221                                                          SYMIN047          
229   CONTINUE                                                          SYMIN048          
      JU=I                                                              SYMIN049          
      K=I                                                               SYMIN050          
      IF(JU) 370,370,310                                                SYMIN051          
310   J=1                                                               SYMIN052          
311   IF(J.GT.JU) GOTO 319                                              SYMIN053          
      KK=K+JI                                                           SYMIN054          
      AMAX=A(K)                                                         SYMIN055          
      A(K)=A(KK)                                                        SYMIN056          
      K=K+N-J                                                           SYMIN057          
      A(KK)=AMAX                                                        SYMIN058          
      J=J+1                                                             SYMIN059          
      GOTO 311                                                          SYMIN060          
319   CONTINUE                                                          SYMIN061          
370   K=II+JI                                                           SYMIN062          
      KK=JJ                                                             SYMIN063          
      JU=II+N-I                                                         SYMIN064          
      IF(JU-K) 460,410,410                                              SYMIN065          
410   L=K                                                               SYMIN066          
411   IF(L.GT.JU) GOTO 419                                              SYMIN067          
      AMAX=A(L)                                                         SYMIN068          
      A(L)=A(KK)                                                        SYMIN069          
      A(KK)=AMAX                                                        SYMIN070          
      KK=KK+1                                                           SYMIN071          
      L=L+1                                                             SYMIN072          
      GOTO 411                                                          SYMIN073          
419   CONTINUE                                                          SYMIN074          
460   IF(M) 520,520,470                                                 SYMIN075          
470   K=IP(I)                                                           SYMIN076          
      J=1                                                               SYMIN077          
481   IF(J.GT.M) GOTO 489                                               SYMIN078          
      AMAX=B(I,J)                                                       SYMIN079          
      B(I,J)=B(K,J)                                                     SYMIN080          
      B(K,J)=AMAX                                                       SYMIN081          
C     DIVIDE BY PIVOT AND ELIMINATE ONE COLUMN                          SYMIN082          
      J=J+1                                                             SYMIN083          
      GOTO 481                                                          SYMIN084          
489   CONTINUE                                                          SYMIN085          
520   A(II)=1.0/A(II)                                                   SYMIN086          
      JU=I-1                                                            SYMIN087          
      K=I                                                               SYMIN088          
      IF(JU) 780,780,560                                                SYMIN089          
560   J=1                                                               SYMIN090          
561   IF(J.GT.JU) GOTO 569                                              SYMIN091          
      AMAX=A(K)*A(II)                                                   SYMIN092          
      LL=K-I+J                                                          SYMIN093          
      LU=K-1                                                            SYMIN094          
      KK=J-1                                                            SYMIN095          
      JI=K                                                              SYMIN096          
      L=LL                                                              SYMIN097          
621   IF(L.GT.LU) GOTO 629                                              SYMIN098          
      KK=KK+1                                                           SYMIN099          
      A(L)=A(L)+A(JI)*AMAX                                              SYMIN100          
      JI=JI+N-KK                                                        SYMIN101          
      L=L+1                                                             SYMIN102          
      GOTO 621                                                          SYMIN103          
629   CONTINUE                                                          SYMIN104          
      A(K)=AMAX                                                         SYMIN105          
      LL=K+1                                                            SYMIN106          
      LU=K+N-I                                                          SYMIN107          
      KK=II                                                             SYMIN108          
      IF(LU-LL) 740,710,710                                             SYMIN109          
710   L=LL                                                              SYMIN110          
711   IF(L.GT.LU) GOTO 719                                              SYMIN111          
      KK=KK+1                                                           SYMIN112          
      A(L)=A(L)-A(KK)*AMAX                                              SYMIN113          
      L=L+1                                                             SYMIN114          
      GOTO 711                                                          SYMIN115          
719   CONTINUE                                                          SYMIN116          
740   IF(M) 770,770,750                                                 SYMIN117          
750   L=1                                                               SYMIN118          
751   IF(L.GT.M) GOTO 759                                               SYMIN119          
      B(J,L)=B(J,L)+B(I,L)*AMAX                                         SYMIN120          
      L=L+1                                                             SYMIN121          
      GOTO 751                                                          SYMIN122          
759   CONTINUE                                                          SYMIN123          
770   K=K+N-J                                                           SYMIN124          
      J=J+1                                                             SYMIN125          
      GOTO 561                                                          SYMIN126          
569   CONTINUE                                                          SYMIN127          
780   JU=N-I                                                            SYMIN128          
      KK=II+N-I                                                         SYMIN129          
      LL=N*(N+1)/2                                                      SYMIN130          
      IF(JU) 990,990,820                                                SYMIN131          
820   JJ=1                                                              SYMIN132          
821   IF(JJ.GT.JU) GOTO 829                                             SYMIN133          
      J=N+1-JJ                                                          SYMIN134          
      AMAX=A(KK)*A(II)                                                  SYMIN135          
      K=KK                                                              SYMIN136          
      KK=KK-1                                                           SYMIN137          
      LU=LL+N-J                                                         SYMIN138          
      L=LL                                                              SYMIN139          
881   IF(L.GT.LU) GOTO 889                                              SYMIN140          
      A(L)=A(L)-A(K)*AMAX                                               SYMIN141          
      K=K+1                                                             SYMIN142          
      L=L+1                                                             SYMIN143          
      GOTO 881                                                          SYMIN144          
889   CONTINUE                                                          SYMIN145          
      IF(M) 940,940,920                                                 SYMIN146          
920   L=1                                                               SYMIN147          
921   IF(L.GT.M) GOTO 929                                               SYMIN148          
      B(J,L)=B(J,L)-B(I,L)*AMAX                                         SYMIN149          
      L=L+1                                                             SYMIN150          
      GOTO 921                                                          SYMIN151          
929   CONTINUE                                                          SYMIN152          
940   LL=LL-N+J-2                                                       SYMIN153          
      JJ=JJ+1                                                           SYMIN154          
      GOTO 821                                                          SYMIN155          
829   CONTINUE                                                          SYMIN156          
      LL=II+1                                                           SYMIN157          
      LU=II+N-I                                                         SYMIN158          
      L=LL                                                              SYMIN159          
971   IF(L.GT.LU) GOTO 979                                              SYMIN160          
      A(L)=-A(L)*A(II)                                                  SYMIN161          
      L=L+1                                                             SYMIN162          
      GOTO 971                                                          SYMIN163          
979   CONTINUE                                                          SYMIN164          
990   IF(M) 1020,1020,1000                                              SYMIN165          
1000  K=1                                                               SYMIN166          
1001  IF(K.GT.M) GOTO 1009                                              SYMIN167          
      B(I,K)=B(I,K)*A(II)                                               SYMIN168          
      K=K+1                                                             SYMIN169          
      GOTO 1001                                                         SYMIN170          
1009  CONTINUE                                                          SYMIN171          
1020  CONTINUE                                                          SYMIN172          
C     A CONTAINS INVERSE WITH ROWS AND COLUMNS SWAPPED OR               SYMIN173          
C     B CONTAINS SOLUTIONS WITH ROWS SWAPPED                            SYMIN174          
      I=I+1                                                             SYMIN175          
      GOTO 41                                                           SYMIN176          
49    CONTINUE                                                          SYMIN177          
      IF(M) 1120,1120,1040                                              SYMIN178          
1040  I=1                                                               SYMIN179          
1041  IF(I.GT.N) GOTO 1049                                              SYMIN180          
      L=N-I+1                                                           SYMIN181          
      K=IP(L)                                                           SYMIN182          
      J=1                                                               SYMIN183          
1071  IF(J.GT.M) GOTO 1079                                              SYMIN184          
      AMAX=B(L,J)                                                       SYMIN185          
      B(L,J)=B(K,J)                                                     SYMIN186          
      B(K,J)=AMAX                                                       SYMIN187          
      J=J+1                                                             SYMIN188          
      GOTO 1071                                                         SYMIN189          
1079  CONTINUE                                                          SYMIN190          
      I=I+1                                                             SYMIN191          
      GOTO 1041                                                         SYMIN192          
1049  CONTINUE                                                          SYMIN193          
      IF(MM) 1120,1120,1460                                             SYMIN194          
1120  II=N*(N+1)/2                                                      SYMIN195          
      LL=1                                                              SYMIN196          
1131  IF(LL.GT.N) GOTO 1139                                             SYMIN197          
      I=N-LL+1                                                          SYMIN198          
      JI=IP(I)-I                                                        SYMIN199          
      IF(JI) 1450,1450,1170                                             SYMIN200          
1170  K=II                                                              SYMIN201          
      KK=K+N-I+JI                                                       SYMIN202          
      L=1                                                               SYMIN203          
1191  IF(L.GT.JI) GOTO 1199                                             SYMIN204          
      K=K+1                                                             SYMIN205          
      AMAX=A(K)                                                         SYMIN206          
      A(K)=A(KK)                                                        SYMIN207          
      A(KK)=AMAX                                                        SYMIN208          
      KK=KK+N-I-L                                                       SYMIN209          
      L=L+1                                                             SYMIN210          
      GOTO 1191                                                         SYMIN211          
1199  CONTINUE                                                          SYMIN212          
      K=I                                                               SYMIN213          
      JJ=N*(IP(I)-1)-IP(I)*(IP(I)-3)/2                                  SYMIN214          
      JU=I                                                              SYMIN215          
      IF(JU) 1350,1350,1290                                             SYMIN216          
1290  J=1                                                               SYMIN217          
1291  IF(J.GT.JU) GOTO 1299                                             SYMIN218          
      KK=K+JI                                                           SYMIN219          
      AMAX=A(K)                                                         SYMIN220          
      A(K)=A(KK)                                                        SYMIN221          
      K=K+N-J                                                           SYMIN222          
      A(KK)=AMAX                                                        SYMIN223          
      J=J+1                                                             SYMIN224          
      GOTO 1291                                                         SYMIN225          
1299  CONTINUE                                                          SYMIN226          
1350  K=II+JI                                                           SYMIN227          
      KK=JJ                                                             SYMIN228          
      JU=II+N-I                                                         SYMIN229          
      IF(JU-K) 1440,1390,1390                                           SYMIN230          
1390  L=K                                                               SYMIN231          
1391  IF(L.GT.JU) GOTO 1399                                             SYMIN232          
      AMAX=A(L)                                                         SYMIN233          
      A(L)=A(KK)                                                        SYMIN234          
      A(KK)=AMAX                                                        SYMIN235          
      KK=KK+1                                                           SYMIN236          
      L=L+1                                                             SYMIN237          
      GOTO 1391                                                         SYMIN238          
1399  CONTINUE                                                          SYMIN239          
1440  CONTINUE                                                          SYMIN240          
1450  II=II-N+I-2                                                       SYMIN241          
      LL=LL+1                                                           SYMIN242          
      GOTO 1131                                                         SYMIN243          
1139  CONTINUE                                                          SYMIN244          
1460  IRR=0                                                             SYMIN245          
1480  RETURN                                                            SYMIN246          
      END                                                               SYMIN247          
