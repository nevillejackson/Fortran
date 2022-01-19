      SUBROUTINE CHEBFIT(M,X,Y,N,A,IN,T,TMAX,IMP)                       CHEBF002          
      DIMENSION X(100),Y(100),T(100),A(11),AX(12),AY(12),AH(12),BY(12), CHEBF003          
     .BH(12),IN(12)                                                     CHEBF004          
C                                                                       CHEBF005          
C      FITS THE FUNCTION Y(X) GIVEN AS  M  PAIRS X,Y BY A POLYNOMIAL    CHEBF006          
C             P(X)=A(1)+A(2)*X+.....+A(N+1)*X**N                        CHEBF007          
C      SO THAT MAXIMUM ERROR IS MINIMISED AND EQUAL TO  TMAX.REFERENCES CHEBF008          
C      COMMUNICATIONS OF THE ACM 5,1962,281/6,1963,167/7,1964,296.      CHEBF009          
C      E.STIEFEL,  NUMERICAL METHODS OF TCHEBYCHEFF APPROXIMATION,      CHEBF010          
C      UNIVERSITY OF WISCONSIN PRESS (1959) 217.                        CHEBF011          
C      MAXIMUM VALUES OF M=100,N=10. VALUES OF X(I) ARE MONOTONIC.      CHEBF012          
C                                                                       CHEBF013          
C         INITIALISE                                                    CHEBF014          
      NN=N+1                                                            CHEBF015          
      NNN=N+2                                                           CHEBF016          
      IF(IMP-1) 430,360,50                                              CHEBF017          
C         GIVEN POLYNOMIAL COEFFICIENTS. FIND REFERENCE SET             CHEBF018          
50    L=1                                                               CHEBF019          
      IN(1)=1                                                           CHEBF020          
      ISIGN=1                                                           CHEBF021          
      I=1                                                               CHEBF022          
81    IF(I.GT.M) GOTO 89                                                CHEBF023          
      U=A(N+1)                                                          CHEBF024          
      J=1                                                               CHEBF025          
101   IF(J.GT.N) GOTO 109                                               CHEBF026          
      NJ=N-J+1                                                          CHEBF027          
      U=U*X(I)+A(NJ)                                                    CHEBF028          
      J=J+1                                                             CHEBF029          
      GOTO 101                                                          CHEBF030          
109   CONTINUE                                                          CHEBF031          
      T(I)=(U-Y(I))*FLOAT(ISIGN)                                        CHEBF032          
      IF(.NOT.(I.EQ.1)) GOTO 210                                        CHEBF033          
      TMAX=T(1)                                                         CHEBF034          
      IF(.NOT.(TMAX.LT.0E0)) GOTO 300                                   CHEBF035          
      TMAX=-TMAX                                                        CHEBF036          
      ISIGN=-ISIGN                                                      CHEBF037          
      GOTO 300                                                          CHEBF038          
210   IF(.NOT.(T(I).GE.TMAX)) GOTO 250                                  CHEBF039          
      IN(L)=I                                                           CHEBF040          
      TMAX=T(I)                                                         CHEBF041          
      GOTO 300                                                          CHEBF042          
250   IF(T(I).GE.0E0) GOTO 300                                          CHEBF043          
      L=L+1                                                             CHEBF044          
      IN(L)=I                                                           CHEBF045          
      TMAX=-T(I)                                                        CHEBF046          
      ISIGN=-ISIGN                                                      CHEBF047          
300   CONTINUE                                                          CHEBF048          
      I=I+1                                                             CHEBF049          
      GOTO 81                                                           CHEBF050          
89    CONTINUE                                                          CHEBF051          
      LL=L-2                                                            CHEBF052          
      IF(LL.EQ.N) GOTO 480                                              CHEBF053          
      WRITE(61,340)                                                     CHEBF054          
340   FORMAT(8H POLYNOM,8HIAL AN U,8HNSUITABL,8HE REFERE,8HNCE FUNC,    CHEBF055          
     .4HTION)                                                           CHEBF056          
      GOTO 430                                                          CHEBF057          
C         GIVEN REFERENCE SET IN(J) CHECK FOR SUITABILITY               CHEBF058          
360   IF(.NOT.(IN(1).GT.0.AND.IN(N+2).LE.M)) GOTO 410                   CHEBF059          
      I=1                                                               CHEBF060          
371   IF(I.GT.NN) GOTO 379                                              CHEBF061          
      IF(.NOT.(IN(I).LT.IN(I+1))) GOTO 410                              CHEBF062          
390   CONTINUE                                                          CHEBF063          
      I=I+1                                                             CHEBF064          
      GOTO 371                                                          CHEBF065          
379   CONTINUE                                                          CHEBF066          
      GOTO 480                                                          CHEBF067          
410   WRITE(61,420)                                                     CHEBF068          
420   FORMAT(8H REFEREN,8HCE SET U,8HNSUITABL,1HE)                      CHEBF069          
C         INITIALIZE IN(J) WITH NO PRIOR INFORMATION                    CHEBF070          
430   K=(M-1)/(N+1)                                                     CHEBF071          
      I=1                                                               CHEBF072          
441   IF(I.GT.NN) GOTO 449                                              CHEBF073          
      IN(I)=(I-1)*K+1                                                   CHEBF074          
      I=I+1                                                             CHEBF075          
      GOTO 441                                                          CHEBF076          
449   CONTINUE                                                          CHEBF077          
      IN(NNN)=M                                                         CHEBF078          
C         ITERATION BEGINS                                              CHEBF079          
480   AH(1)=1                                                           CHEBF080          
      I=1                                                               CHEBF081          
491   IF(I.GT.NNN) GOTO 499                                             CHEBF082          
      INI=IN(I)                                                         CHEBF083          
      AX(I)=X(INI)                                                      CHEBF084          
      AY(I)=Y(INI)                                                      CHEBF085          
      IF(I.EQ.1) GOTO 550                                               CHEBF086          
      AH(I)=-AH(I-1)                                                    CHEBF087          
550   CONTINUE                                                          CHEBF088          
C         DIVIDED DIFFERENCES                                           CHEBF089          
      I=I+1                                                             CHEBF090          
      GOTO 491                                                          CHEBF091          
499   CONTINUE                                                          CHEBF092          
      I=2                                                               CHEBF093          
561   IF(I.GT.NNN) GOTO 569                                             CHEBF094          
      IL=I-1                                                            CHEBF095          
      J=IL                                                              CHEBF096          
581   IF(J.GT.NNN) GOTO 589                                             CHEBF097          
      BY(J)=AY(J)                                                       CHEBF098          
      BH(J)=AH(J)                                                       CHEBF099          
      J=J+1                                                             CHEBF100          
      GOTO 581                                                          CHEBF101          
589   CONTINUE                                                          CHEBF102          
      J=I                                                               CHEBF103          
621   IF(J.GT.NNN) GOTO 629                                             CHEBF104          
      JI=J-I+1                                                          CHEBF105          
      AY(J)=(BY(J)-BY(J-1))/(AX(J)-AX(JI))                              CHEBF106          
      AH(J)=(BH(J)-BH(J-1))/(AX(J)-AX(JI))                              CHEBF107          
      J=J+1                                                             CHEBF108          
      GOTO 621                                                          CHEBF109          
629   CONTINUE                                                          CHEBF110          
      I=I+1                                                             CHEBF111          
      GOTO 561                                                          CHEBF112          
569   CONTINUE                                                          CHEBF113          
      H=-AY(N+2)/AH(N+2)                                                CHEBF114          
C         POLYNOMIAL COEFFICIENTS                                       CHEBF115          
      I=1                                                               CHEBF116          
691   IF(I.GT.NN) GOTO 699                                              CHEBF117          
      A(I)=AY(I)+H*AH(I)                                                CHEBF118          
      BY(I)=0                                                           CHEBF119          
      I=I+1                                                             CHEBF120          
      GOTO 691                                                          CHEBF121          
699   CONTINUE                                                          CHEBF122          
      BY(1)=1                                                           CHEBF123          
      TMAX=H                                                            CHEBF124          
      TMAX=ABS(TMAX)                                                    CHEBF125          
      IMAX=IN(1)                                                        CHEBF126          
      I=1                                                               CHEBF127          
771   IF(I.GT.N) GOTO 779                                               CHEBF128          
      J=1                                                               CHEBF129          
781   IF(J.GT.I) GOTO 789                                               CHEBF130          
      IJ=I-J+1                                                          CHEBF131          
      BY(IJ+1)=BY(IJ+1)-AX(I)*BY(IJ)                                    CHEBF132          
      A(J)=A(J)+A(I+1)*BY(IJ+1)                                         CHEBF133          
      J=J+1                                                             CHEBF134          
      GOTO 781                                                          CHEBF135          
789   CONTINUE                                                          CHEBF136          
C         COMPUTE DEVIATIONS AND THEIR MAXIMUM VALUE                    CHEBF137          
      I=I+1                                                             CHEBF138          
      GOTO 771                                                          CHEBF139          
779   CONTINUE                                                          CHEBF140          
      I=1                                                               CHEBF141          
841   IF(I.GT.M) GOTO 849                                               CHEBF142          
      U=A(N+1)                                                          CHEBF143          
      J=1                                                               CHEBF144          
861   IF(J.GT.N) GOTO 869                                               CHEBF145          
      NJ=N-J+1                                                          CHEBF146          
      U=U*X(I)+A(NJ)                                                    CHEBF147          
      J=J+1                                                             CHEBF148          
      GOTO 861                                                          CHEBF149          
869   CONTINUE                                                          CHEBF150          
      T(I)=U-Y(I)                                                       CHEBF151          
      IF(ABS(T(I)).LE.TMAX) GOTO 940                                    CHEBF152          
      TMAX=ABS(T(I))                                                    CHEBF153          
      IMAX=I                                                            CHEBF154          
940   CONTINUE                                                          CHEBF155          
C         TEST REFERENCE DEVIATIONS FOR ALTERNATION OF SIGN             CHEBF156          
      I=I+1                                                             CHEBF157          
      GOTO 841                                                          CHEBF158          
849   CONTINUE                                                          CHEBF159          
      ISIGN1=1                                                          CHEBF160          
      INI=IN(1)                                                         CHEBF161          
      IF(T(INI)) 990,980,1000                                           CHEBF162          
980   ISIGN1=0                                                          CHEBF163          
990   ISIGN1=-ISIGN1                                                    CHEBF164          
1000  I=2                                                               CHEBF165          
1001  IF(I.GT.NNN) GOTO 1009                                            CHEBF166          
      ISIGN2=1                                                          CHEBF167          
      INI=IN(I)                                                         CHEBF168          
      IF(T(INI)) 1050,1040,1060                                         CHEBF169          
1040  ISIGN2=0                                                          CHEBF170          
1050  ISIGN2=-ISIGN2                                                    CHEBF171          
1060  IF((ISIGN1+ISIGN2).EQ.0) GOTO 1100                                CHEBF172          
      WRITE(61,1080)                                                    CHEBF173          
1080  FORMAT(8H REFEREN,8HCE DEVIA,8HTIONS DO,8H NOT ALT,8HERNATE I,    CHEBF174          
     .6HN SIGN)                                                         CHEBF175          
      GOTO 1350                                                         CHEBF176          
1100  ISIGN1=ISIGN2                                                     CHEBF177          
C         EXCHANGE OF REFERENCE VALUES                                  CHEBF178          
      I=I+1                                                             CHEBF179          
      GOTO 1001                                                         CHEBF180          
1009  CONTINUE                                                          CHEBF181          
      I=1                                                               CHEBF182          
1121  IF(I.GT.NNN) GOTO 1129                                            CHEBF183          
      IF(IMAX-IN(I)) 1160,1350,1140                                     CHEBF184          
1140  CONTINUE                                                          CHEBF185          
      I=I+1                                                             CHEBF186          
      GOTO 1121                                                         CHEBF187          
1129  CONTINUE                                                          CHEBF188          
      I=N+2                                                             CHEBF189          
1160  INI=IN(I)                                                         CHEBF190          
      IF(T(IMAX)*T(INI)) 1200,1180,1180                                 CHEBF191          
1180  IN(I)=IMAX                                                        CHEBF192          
      GOTO 480                                                          CHEBF193          
1200  IF(IN(1).LT.IMAX) GOTO 1270                                       CHEBF194          
      I=1                                                               CHEBF195          
1211  IF(I.GT.NN) GOTO 1219                                             CHEBF196          
      NI=N-I+1                                                          CHEBF197          
      IN(NI+2)=IN(NI+1)                                                 CHEBF198          
      I=I+1                                                             CHEBF199          
      GOTO 1211                                                         CHEBF200          
1219  CONTINUE                                                          CHEBF201          
      IN(1)=IMAX                                                        CHEBF202          
      GOTO 480                                                          CHEBF203          
1270  IF(IN(N+2).LE.IMAX) GOTO 1300                                     CHEBF204          
      IN(I-1)=IMAX                                                      CHEBF205          
      GOTO 480                                                          CHEBF206          
1300  I=1                                                               CHEBF207          
1301  IF(I.GT.NN) GOTO 1309                                             CHEBF208          
      IN(I)=IN(I+1)                                                     CHEBF209          
      I=I+1                                                             CHEBF210          
      GOTO 1301                                                         CHEBF211          
1309  CONTINUE                                                          CHEBF212          
      IN(N+2)=IMAX                                                      CHEBF213          
      GOTO 480                                                          CHEBF214          
1350  RETURN                                                            CHEBF215          
      END                                                               CHEBF216          
