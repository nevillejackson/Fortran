      PROGRAM FACORAN(INPUT,OUTPUT,TAPE60=INPUT,TAPE61=OUTPUT)          FACTO002          
      INTEGER EOFCKF                                                    FACTO003          
      DIMENSION NAME(10),FMT(10),R(80,80),S(80,80),X(80),C(80),AVX(80), FACTO004          
     .SDX(80),VALUE(2)                                                  FACTO005          
C ROUTINE      G7 CSIR FACTORAN          DATE JUL 65              010501FACTO006          
C TITLE        PROGRAM FACTORAN                                   010502FACTO007          
C DESCRIPTION  THE PROGRAM CARRIES OUT A PRINCIPAL COMPONENT      010503FACTO008          
C              ANALYSIS OR A PRINCIPAL FACTOR ANALYSIS OF DATA    010504FACTO009          
C              WHICH MAY BE IN THE FORM OF RAW SCORES OR          010505FACTO010          
C              COEFFICIENTS OF CORRELATION. OPTION IS PROVIDED    010506FACTO011          
C              FOR A VARIMAX ROTATION OF THE MATRIX OF FACTOR     010507FACTO012          
C              COEFFICIENTS. VARIOUS CHECKS AND TESTS ARE INCLUDED010508FACTO013          
C              SUBROUTINES REQUIRED ARE MATIN, EIGENVAL, FACTORS. 010509FACTO014          
C LANGUAGE     36FTN                     AVAILABLE FROM SRLIST    010510FACTO015          
C AVAILABLE AS (SOURCE DECK, 36BIN)                               010511FACTO016          
C AUTHOR       T. BROWN, DEPARTMENT OF DENTAL SCIENCE, C.S.I.R.O.,010512FACTO017          
C              UNIVERSITY OF ADELAIDE, SOUTH AUSTRALIA.           010513FACTO018          
C KEYWORDS     FACTOR,ANALYSIS,ORTHOGONAL                         010514FACTO019          
C SEE ALSO     -                                                  010515FACTO020          
C     CONTAINS SUBROUTINES MATIN,EIGENVAL,FACTORS,VARIMAX,FACTESTS      FACTO021          
C     DENTGRO PROJECT PROGRAM CDC003 FORTRAN 36                         FACTO022          
C                                                                       FACTO023          
C     THIS PROGRAM PERFORMS A PRINCIPAL COMPONENT ANALYSIS OR A         FACTO024          
C     COMPLETE FACTOR ANALYSIS INCLUDING ORTHOGONAL ROTATION BY THE     FACTO025          
C     VARIMAX METHOD FOLLOWED BY TESTS ON THE FACTOR SOLUTION           FACTO026          
C     SUBROUTINES USED ARE - MATIN, EIGENVAL, FACTORS, VARIMAX AND      FACTO027          
C     FACTESTS                                                          FACTO028          
C     DATA MAY BE IN THE FORM OF RAW SCORES, MATRIX OF CORRELATION      FACTO029          
C     COEFFICIENTS OR PRINCIPAL FACTOR COEFFICIENTS                     FACTO030          
C                                                                       FACTO031          
C     INPUT                                                             FACTO032          
C     THE FIRST CONTROL CARD FOR EACH GROUP HAS AN IDENTIFYING TITLE    FACTO033          
C        PUNCHED ANYWHERE WITHIN 80 COLUMNS  (ALPHANUMERIC)             FACTO034          
C     THE SECOND CONTROL CARD FOR EACH GROUP HAS THE FOLLOWING VALUES   FACTO035          
C      COL. 1-2    NUMBER OF VARIABLES (N) IN FORMAT(I2)                FACTO036          
C      COL. 3-6    NUMBER OF SUBJECTS (M) IN FORMAT(I4) NEVER BLANK     FACTO037          
C      COL. 7      1= RAW SCORES ENTERED                                FACTO038          
C                  2= CORRELATION COEFFICIENTS ENTERED                  FACTO039          
C                  3= PRINCIPAL FACTOR COEFFICIENTS ENTERED             FACTO040          
C      COL. 8      1= NO COMMUNALITY ESTIMATES REQUIRED (THAT IS PLACE  FACTO041          
C                     UNITIES IN THE MAIN DIAGONALS)                    FACTO042          
C                  2= COMMUNALITY ESTIMATES REQUIRED.  SQUARE MULTIPLE  FACTO043          
C                     CORRELATIONS ARE INSERTED FOLLOWING INVERSION     FACTO044          
C                  3= COMMUNALITY ESTIMATES ENTERED WITH DATA - MUST    FACTO045          
C                     HAVE 2 IN COL. 7                                  FACTO046          
C                  0= PRINCIPAL FACTOR COEFFICIENTS ENTERED             FACTO047          
C      COL. 9-12   CRITERION FOR MINIMUM VALUE OF EIGENVALUE TO BE      FACTO048          
C                  RETAINED FOR FURTHER ANALYSIS.   SUGGEST 0.0 FOR     FACTO049          
C                  PRINCIPAL COMPONENT ANALYSIS AND FACTOR ANALYSIS     FACTO050          
C                  USING COMMUNALITIES AND 1.0 FOR FACTOR ANALYSIS      FACTO051          
C                  USING UNITIES IN DIAGONALS                           FACTO052          
C                  CRITERION TO BE PUNCHED IN FORMAT(4.1)               FACTO053          
C      COL.13-16   CRITERION FOR MINIMUM PER CENT OF EIGENVALUE/TRACE   FACTO054          
C                  TO BE RETAINED FOR FURTHER ANALYSIS.   SUGGEST 0.0   FACTO055          
C                  FOR PRINCIPAL COMPONENTS AND 5.0 FOR FACTOR ANALYSIS FACTO056          
C                  CRITERION TO BE PUNCHED IN FORMAT(4.1)               FACTO057          
C      COL.17-18   NUMBER OF FACTORS (MAXF) IF DATA ARE PRINCIPAL       FACTO058          
C                  COMPONENT COEFFICIENTS.  OTHERWISE BLANK             FACTO059          
C      COL.19      1= PRINCIPAL COMPONENT ANALYSIS ONLY REQUIRED        FACTO060          
C                  0= PRINCIPAL COMPONENT ANALYSIS NOT REQUIRED         FACTO061          
C                                                                       FACTO062          
C     THE THIRD CONTROL CARD FOR EACH  GROUP GIVES THE FORMAT FOR DATA  FACTO063          
C     INPUT                                                             FACTO064          
C     THE FOURTH AND SUBSEQUENT CARDS FOR EACH GROUP ARE THE DATA       FACTO065          
C     CARDS.   NOTE THAT CORRELATION COEFFICIENTS ARE PUNCHED IN        FACTO066          
C     UPPER TRINAGULAR MATRIX FORM INCLUDING DIAGONAL ELEMENTS          FACTO067          
C     TERMINATION CARD TO BE PLACED BEHIND FINAL GROUP OF DATA CARDS    FACTO068          
C                                                                       FACTO069          
C     OUTPUT                                                            FACTO070          
C      TOTAL LINES OUTPUT FOR ALL OPTIONS IS APPROXIMATELY              FACTO071          
C             117+N*(11+I)+(N*N)/10+MAXF*(6+2*I)                        FACTO072          
C      WHERE MAXF= NO. FACTORS RETAINED, N= NO. VARIABLES AND I= N/10   FACTO073          
C      ROUNDED TO THE NEXT HIGHEST INTEGER VALUE                        FACTO074          
C                                                                       FACTO075          
C     SCRATCH TAPE UNIT 50 IS REQUIRED                                  FACTO076          
C                                                                       FACTO077          
C                                                                       FACTO078          
C                                                                       FACTO079          
C     FORMAT STATEMENTS                                                 FACTO080          
20    FORMAT(10A8)                                                      FACTO081          
30    FORMAT(I2,I4,2I1,2F4.1,I2,I1)                                     FACTO082          
40    FORMAT(1H1,43X,8HFACTOR A,8HNALYSIS ,8H- PROGRA,8HM FACTOR,2HAN// FACTO083          
     .20X,10A8//)                                                       FACTO084          
50    FORMAT(48X,8HNUMBER O,8HF VARIAB,5HLES -,I4/)                     FACTO085          
60    FORMAT(35X,8HDATA ENT,8HERED AS ,8HRAW SCOR,7HES FOR ,I8,         FACTO086          
     .8H  SUBJEC,2HTS/)                                                 FACTO087          
70    FORMAT(40X,8HDATA ENT,8HERED AS ,8HCORRELAT,8HION COEF,8HFICIENTS FACTO088          
     ./)                                                                FACTO089          
80    FORMAT(28X,8HDATA ENT,8HERED AS ,8HPRINCIPA,8HL FACTOR,8H    COEF FACTO090          
     .,8HFICIENTS,4H FOR,I3,8H  FACTOR,1HS/)                            FACTO091          
90    FORMAT(43X,8HNO COMMU,8HNALITY E,8HSTIMATES,8H REQUIRE,1HD/)      FACTO092          
100   FORMAT(45X,8HCOMMUNAL,8HITY ESTI,8HMATES RE,6HQUIRED/)            FACTO093          
110   FORMAT(40X,8HCOMMUNAL,8HITY ESTI,8HMATES EN,8HTERED WI,7HTH DATA/ FACTO094          
     .)                                                                 FACTO095          
120   FORMAT(30X,8HCRITERIA,8H FOR SEL,8HECTION O,8HF EIGENV,5HALUES,   FACTO096          
     .F4.1,4H AND,F4.1,8H PER CEN,1HT/)                                 FACTO097          
130   FORMAT(45X,8HVARIABLE,3X,4HMEAN,6X,8HSTAND.DE,1HV/)               FACTO098          
140   FORMAT(46X,I4,2F12.4)                                             FACTO099          
150   FORMAT(/8X,8HVARIABLE,I4)                                         FACTO100          
160   FORMAT(10X,10F10.6)                                               FACTO101          
170   FORMAT(//48X,8HFACTOR A,8HNALYSIS ,8HCOMPLETE)                    FACTO102          
180   FORMAT(//27X,8HCORRELAT,8HION MATR,8HIX, UPPE,8HR TRIANG,         FACTO103          
     .8HULAR ELE,8HMENTS IN,8HCLUDING ,8HDIAGONAL,1HS/)                 FACTO104          
190   FORMAT(30X,8HSMC VALU,8HES AS FO,8HLLOWS  U,8HSE MAXIM,8HUM ROW R FACTO105          
     .,8H VALUES ,8HIN DIAGO,4HNALS/)                                   FACTO106          
200   FORMAT(/48X,8HTRACE OF,8H MATRIX ,1H ,F12.6)                      FACTO107          
      KQZB00=32                                                         FACTO108          
      KQZB06=8                                                          FACTO109          
210   READ(60,20) NAME                                                  FACTO110          
      K=EOFCKF(60)                                                      FACTO111          
      KQZ001=K                                                          FACTO112          
      IF(KQZ001.LT.1) KQZ001=1                                          FACTO113          
      IF(KQZ001.GT.2) KQZ001=2                                          FACTO114          
      GOTO(240,250),KQZ001                                              FACTO115          
240   STOP                                                              FACTO116          
250   READ(60,30) N,M,INPUT,INDEX,VALUE(1),VALUE(2),MAXF,NPC            FACTO117          
      READ(60,20) FMT                                                   FACTO118          
      WRITE(61,40) NAME                                                 FACTO119          
      IF(N) 240,240,290                                                 FACTO120          
290   WRITE(61,50) N                                                    FACTO121          
      KQZ001=INPUT                                                      FACTO122          
      IF(KQZ001.LT.1) KQZ001=1                                          FACTO123          
      IF(KQZ001.GT.3) KQZ001=3                                          FACTO124          
      GOTO(310,330,350),KQZ001                                          FACTO125          
310   WRITE(61,60) M                                                    FACTO126          
      GOTO 370                                                          FACTO127          
330   WRITE(61,70)                                                      FACTO128          
      GOTO 370                                                          FACTO129          
350   WRITE(61,80) MAXF                                                 FACTO130          
      GOTO 1140                                                         FACTO131          
370   KQZ001=INDEX                                                      FACTO132          
      IF(KQZ001.LT.1) KQZ001=1                                          FACTO133          
      IF(KQZ001.GT.3) KQZ001=3                                          FACTO134          
      GOTO(380,400,420),KQZ001                                          FACTO135          
380   WRITE(61,90)                                                      FACTO136          
      GOTO 430                                                          FACTO137          
400   WRITE(61,100)                                                     FACTO138          
      GOTO 430                                                          FACTO139          
420   WRITE(61,110)                                                     FACTO140          
430   WRITE(61,120) VALUE(1),VALUE(2)                                   FACTO141          
C                                                                       FACTO142          
C     READ IN RAW SCORES TO COMPUTE MEANS STAND. DEVIATIONS AND R MATRIXFACTO143          
      KQZ001=INPUT                                                      FACTO144          
      IF(KQZ001.LT.1) KQZ001=1                                          FACTO145          
      IF(KQZ001.GT.3) KQZ001=3                                          FACTO146          
      GOTO(450,740,1140),KQZ001                                         FACTO147          
450   I=1                                                               FACTO148          
451   IF(I.GT.N) GOTO 459                                               FACTO149          
      C(I)=0.0                                                          FACTO150          
      J=I                                                               FACTO151          
471   IF(J.GT.N) GOTO 479                                               FACTO152          
      S(I,J)=0.0                                                        FACTO153          
      J=J+1                                                             FACTO154          
      GOTO 471                                                          FACTO155          
479   CONTINUE                                                          FACTO156          
      I=I+1                                                             FACTO157          
      GOTO 451                                                          FACTO158          
459   CONTINUE                                                          FACTO159          
      NUMCARD=M                                                         FACTO160          
      EN=NUMCARD                                                        FACTO161          
500   READ(60,FMT)(X(I),I=1,N)                                          FACTO162          
      I=1                                                               FACTO163          
511   IF(I.GT.N) GOTO 519                                               FACTO164          
      C(I)=C(I)+X(I)                                                    FACTO165          
      J=I                                                               FACTO166          
531   IF(J.GT.N) GOTO 539                                               FACTO167          
      S(I,J)=S(I,J)+X(I)*X(J)                                           FACTO168          
      J=J+1                                                             FACTO169          
      GOTO 531                                                          FACTO170          
539   CONTINUE                                                          FACTO171          
      I=I+1                                                             FACTO172          
      GOTO 511                                                          FACTO173          
519   CONTINUE                                                          FACTO174          
      NUMCARD=NUMCARD-1                                                 FACTO175          
      IF(NUMCARD) 570,570,500                                           FACTO176          
570   I=1                                                               FACTO177          
571   IF(I.GT.N) GOTO 579                                               FACTO178          
      J=I                                                               FACTO179          
581   IF(J.GT.N) GOTO 589                                               FACTO180          
      R(I,J)=S(I,J)-C(I)*C(J)/EN                                        FACTO181          
      J=J+1                                                             FACTO182          
      GOTO 581                                                          FACTO183          
589   CONTINUE                                                          FACTO184          
      I=I+1                                                             FACTO185          
      GOTO 571                                                          FACTO186          
579   CONTINUE                                                          FACTO187          
      I=1                                                               FACTO188          
601   IF(I.GT.N) GOTO 609                                               FACTO189          
      AVX(I)=C(I)/EN                                                    FACTO190          
      SDX(I)=SQRT(R(I,I)/(EN-1.0))                                      FACTO191          
      I=I+1                                                             FACTO192          
      GOTO 601                                                          FACTO193          
609   CONTINUE                                                          FACTO194          
      WRITE(61,130)                                                     FACTO195          
      I=1                                                               FACTO196          
641   IF(I.GT.N) GOTO 649                                               FACTO197          
      WRITE(61,140) I,AVX(I),SDX(I)                                     FACTO198          
C                                                                       FACTO199          
C     COMPUTE COEFFICIENTS OF CORRELATION                               FACTO200          
      I=I+1                                                             FACTO201          
      GOTO 641                                                          FACTO202          
649   CONTINUE                                                          FACTO203          
      I=1                                                               FACTO204          
661   IF(I.GT.N) GOTO 669                                               FACTO205          
      J=I                                                               FACTO206          
671   IF(J.GT.N) GOTO 679                                               FACTO207          
      R(I,J)=R(I,J)/(EN-1.0)                                            FACTO208          
      J=J+1                                                             FACTO209          
      GOTO 671                                                          FACTO210          
679   CONTINUE                                                          FACTO211          
      I=I+1                                                             FACTO212          
      GOTO 661                                                          FACTO213          
669   CONTINUE                                                          FACTO214          
      I=1                                                               FACTO215          
691   IF(I.GT.N) GOTO 699                                               FACTO216          
      J=I                                                               FACTO217          
701   IF(J.GT.N) GOTO 709                                               FACTO218          
      R(I,J)=R(I,J)/(SDX(I)*SDX(J))                                     FACTO219          
      R(J,I)=R(I,J)                                                     FACTO220          
      J=J+1                                                             FACTO221          
      GOTO 701                                                          FACTO222          
709   CONTINUE                                                          FACTO223          
      I=I+1                                                             FACTO224          
      GOTO 691                                                          FACTO225          
699   CONTINUE                                                          FACTO226          
      GOTO 780                                                          FACTO227          
C     READ IN CORRELATION MATRIX FROM CARDS                             FACTO228          
740   I=1                                                               FACTO229          
741   IF(I.GT.N) GOTO 749                                               FACTO230          
      READ(60,FMT)(R(I,J),J=I,N)                                        FACTO231          
      K=I                                                               FACTO232          
761   IF(K.GT.N) GOTO 769                                               FACTO233          
      R(K,I)=R(I,K)                                                     FACTO234          
C                                                                       FACTO235          
C     COMPUTE COMMUNALITY ESTIMATES IF REQUIRED                         FACTO236          
      K=K+1                                                             FACTO237          
      GOTO 761                                                          FACTO238          
769   CONTINUE                                                          FACTO239          
      I=I+1                                                             FACTO240          
      GOTO 741                                                          FACTO241          
749   CONTINUE                                                          FACTO242          
780   KQZ001=INDEX                                                      FACTO243          
      IF(KQZ001.LT.1) KQZ001=1                                          FACTO244          
      IF(KQZ001.GT.3) KQZ001=3                                          FACTO245          
      GOTO(990,790,990),KQZ001                                          FACTO246          
790   I=1                                                               FACTO247          
791   IF(I.GT.N) GOTO 799                                               FACTO248          
      R(I,I)=1.0                                                        FACTO249          
      J=1                                                               FACTO250          
811   IF(J.GT.N) GOTO 819                                               FACTO251          
      S(I,J)=R(I,J)                                                     FACTO252          
      J=J+1                                                             FACTO253          
      GOTO 811                                                          FACTO254          
819   CONTINUE                                                          FACTO255          
      I=I+1                                                             FACTO256          
      GOTO 791                                                          FACTO257          
799   CONTINUE                                                          FACTO258          
      CALL MATIN(N,S)                                                   FACTO259          
      I=1                                                               FACTO260          
841   IF(I.GT.N) GOTO 849                                               FACTO261          
      R(I,I)=1.0-(1.0/S(I,I))                                           FACTO262          
C                                                                       FACTO263          
      I=I+1                                                             FACTO264          
      GOTO 841                                                          FACTO265          
849   CONTINUE                                                          FACTO266          
      I=1                                                               FACTO267          
861   IF(I.GT.N) GOTO 869                                               FACTO268          
      IF(R(I,I).LT.0..OR.R(I,I).GT.1.) GOTO 900                         FACTO269          
880   CONTINUE                                                          FACTO270          
      I=I+1                                                             FACTO271          
      GOTO 861                                                          FACTO272          
869   CONTINUE                                                          FACTO273          
      GOTO 990                                                          FACTO274          
C     PLACE MAXIMUM CORRELATIONS IN DIAGONALS OF R MATRIX AS            FACTO275          
C     INITIAL COMMUNALITY APPROXIMATIONS                                FACTO276          
900   WRITE(61,190)                                                     FACTO277          
      WRITE(61,160)(R(I,I),I=1,N)                                       FACTO278          
      I=1                                                               FACTO279          
921   IF(I.GT.N) GOTO 929                                               FACTO280          
      R(I,I)=0.0                                                        FACTO281          
      RMAX=R(I,I)                                                       FACTO282          
      J=1                                                               FACTO283          
941   IF(J.GT.N) GOTO 949                                               FACTO284          
      IF(.NOT.(ABS(R(I,J)).GT.RMAX)) GOTO 970                           FACTO285          
      RMAX=ABS(R(I,J))                                                  FACTO286          
970   CONTINUE                                                          FACTO287          
      J=J+1                                                             FACTO288          
      GOTO 941                                                          FACTO289          
949   CONTINUE                                                          FACTO290          
      R(I,I)=RMAX                                                       FACTO291          
C                                                                       FACTO292          
C     WRITE OUT R MATRIX AND PLACE ON SCRATCH TAPE UNIT 50              FACTO293          
      I=I+1                                                             FACTO294          
      GOTO 921                                                          FACTO295          
929   CONTINUE                                                          FACTO296          
990   WRITE(61,180)                                                     FACTO297          
      I=1                                                               FACTO298          
1001  IF(I.GT.N) GOTO 1009                                              FACTO299          
      WRITE(61,150) I                                                   FACTO300          
      WRITE(61,160)(R(I,J),J=I,N)                                       FACTO301          
      I=I+1                                                             FACTO302          
      GOTO 1001                                                         FACTO303          
1009  CONTINUE                                                          FACTO304          
      REWIND 50                                                         FACTO305          
      I=1                                                               FACTO306          
1041  IF(I.GT.N) GOTO 1049                                              FACTO307          
      WRITE(50,160)(R(I,J),J=I,N)                                       FACTO308          
      I=I+1                                                             FACTO309          
      GOTO 1041                                                         FACTO310          
1049  CONTINUE                                                          FACTO311          
      ENDFILE 50                                                        FACTO312          
C                                                                       FACTO313          
C     COMPUTE AND PRINT TRACE R                                         FACTO314          
      TRACE=0.0                                                         FACTO315          
      I=1                                                               FACTO316          
1081  IF(I.GT.N) GOTO 1089                                              FACTO317          
      TRACE=TRACE+R(I,I)                                                FACTO318          
      I=I+1                                                             FACTO319          
      GOTO 1081                                                         FACTO320          
1089  CONTINUE                                                          FACTO321          
      WRITE(61,200) TRACE                                               FACTO322          
C                                                                       FACTO323          
C     COMPUTE PRINCIPAL FACTOR INITIAL SOLUTION                         FACTO324          
      CALL EIGNVAL(NAME,N,R,S,C)                                        FACTO325          
      CALL FACTORS(NAME,N,R,C,MAXF,VALUE,TRACE)                         FACTO326          
C                                                                       FACTO327          
C     PRINCIPAL FACTOR SOLUTION NOW STORED IN R(MAXF,N)                 FACTO328          
      IF(NPC.NE.0) GOTO 1190                                            FACTO329          
      GOTO 1160                                                         FACTO330          
C                                                                       FACTO331          
C     ENTER PRINCIPAL FACTOR SOLUTION FROM CARDS                        FACTO332          
1140  I=1                                                               FACTO333          
1141  IF(I.GT.MAXF) GOTO 1149                                           FACTO334          
      READ(60,FMT)(R(I,J),J=1,N)                                        FACTO335          
C                                                                       FACTO336          
C     COMPUTE VARIMAX SOLUTION AND TESTS FOR FINAL SOLUTION             FACTO337          
      I=I+1                                                             FACTO338          
      GOTO 1141                                                         FACTO339          
1149  CONTINUE                                                          FACTO340          
1160  CALL VARIMAX(NAME,N,R,C,MAXF)                                     FACTO341          
      IF(INPUT.EQ.3) GOTO 1190                                          FACTO342          
      CALL FACESTS(NAME,N,R,S,C,MAXF,M)                                 FACTO343          
1190  WRITE(61,170)                                                     FACTO344          
      GOTO 210                                                          FACTO345          
      END                                                               FACTO346          
      SUBROUTINE MATIN(N,A)                                             FACTO347          
      DIMENSION IPIVOT(80),A(80,80),INDEX(80,2),PIVOT(80)               FACTO348          
      EQUIVALENCE(IROW,JROW),(ICOLUM,JCOLUM),(AMAX,T,SWAP)              FACTO349          
C                                                                       FACTO350          
C     DENTGRO PROJECT PROGRAM CDC003 FORTRAN 36 (SUBROUTINE MATIN)      FACTO351          
C     THIS SUBROUTINE COMPUTES THE INVERSE OF MATRIX A, OF ORDER N, BY  FACTO352          
C     THE GAUSS-JORDAN METHOD.                                          FACTO353          
C     A-INVERSE REPLACES A, AND THE DETERMINANT OF A IS PRINTED         FACTO354          
C     MATIN IS USED TO PROVIDE INITIAL COMMUNALITY ESTIMATES            FACTO355          
C     PRIOR TO FACTOR ANALYSIS.                                         FACTO356          
C     LINES OUTPUT FROM MATIN  3                                        FACTO357          
C     REFERENCE COOLEY AND LOHNES, MULTIVARIATE PROCEDURES FOR THE      FACTO358          
C     BEHAVIOURAL SCIENCES. WILEY (1962), PAGE 198                      FACTO359          
C                                                                       FACTO360          
20    FORMAT(/27X,8HDETERMIN,8HANT OF C,8HORRELATI,8HON MATRI,1HX,F14.8 FACTO361          
     ./)                                                                FACTO362          
C     INITIALIZE VALUES                                                 FACTO363          
      DET=1.0                                                           FACTO364          
      J=1                                                               FACTO365          
41    IF(J.GT.N) GOTO 49                                                FACTO366          
      IPIVOT(J)=0                                                       FACTO367          
      J=J+1                                                             FACTO368          
      GOTO 41                                                           FACTO369          
49    CONTINUE                                                          FACTO370          
      I=1                                                               FACTO371          
61    IF(I.GT.N) GOTO 69                                                FACTO372          
C     SEARCH FOR PIVOT ELEMENT                                          FACTO373          
      AMAX=0.0                                                          FACTO374          
      J=1                                                               FACTO375          
81    IF(J.GT.N) GOTO 89                                                FACTO376          
      IF(IPIVOT(J)-1) 100,170,100                                       FACTO377          
100   K=1                                                               FACTO378          
101   IF(K.GT.N) GOTO 109                                               FACTO379          
      IF(IPIVOT(K)-1) 120,160,500                                       FACTO380          
120   IF(ABS(AMAX)-ABS(A(J,K))) 130,160,160                             FACTO381          
130   IROW=J                                                            FACTO382          
      ICOLUM=K                                                          FACTO383          
      AMAX=A(J,K)                                                       FACTO384          
160   CONTINUE                                                          FACTO385          
      K=K+1                                                             FACTO386          
      GOTO 101                                                          FACTO387          
109   CONTINUE                                                          FACTO388          
170   CONTINUE                                                          FACTO389          
      J=J+1                                                             FACTO390          
      GOTO 81                                                           FACTO391          
89    CONTINUE                                                          FACTO392          
      IPIVOT(ICOLUM)=IPIVOT(ICOLUM)+1                                   FACTO393          
C                                                                       FACTO394          
C     INTERCHANGE ROWS TO PUT PIVOT ELEMENT ON DIAGONAL                 FACTO395          
      IF(IROW-ICOLUM) 200,250,200                                       FACTO396          
200   DET=-DET                                                          FACTO397          
      L=1                                                               FACTO398          
211   IF(L.GT.N) GOTO 219                                               FACTO399          
      SWAP=A(IROW,L)                                                    FACTO400          
      A(IROW,L)=A(ICOLUM,L)                                             FACTO401          
      A(ICOLUM,L)=SWAP                                                  FACTO402          
      L=L+1                                                             FACTO403          
      GOTO 211                                                          FACTO404          
219   CONTINUE                                                          FACTO405          
250   INDEX(I,1)=IROW                                                   FACTO406          
      INDEX(I,2)=ICOLUM                                                 FACTO407          
      PIVOT(I)=A(ICOLUM,ICOLUM)                                         FACTO408          
      DET=DET*PIVOT(I)                                                  FACTO409          
C                                                                       FACTO410          
C     DIVIDE PIVOT ROW BY PIVOT ELEMENT                                 FACTO411          
      A(ICOLUM,ICOLUM)=1.0                                              FACTO412          
      L=1                                                               FACTO413          
301   IF(L.GT.N) GOTO 309                                               FACTO414          
      A(ICOLUM,L)=A(ICOLUM,L)/PIVOT(I)                                  FACTO415          
C                                                                       FACTO416          
C     REDUCE NON-PIVOT ROWS                                             FACTO417          
      L=L+1                                                             FACTO418          
      GOTO 301                                                          FACTO419          
309   CONTINUE                                                          FACTO420          
      L1=1                                                              FACTO421          
321   IF(L1.GT.N) GOTO 329                                              FACTO422          
      IF(L1-ICOLUM) 340,380,340                                         FACTO423          
340   T=A(L1,ICOLUM)                                                    FACTO424          
      A(L1,ICOLUM)=0.0                                                  FACTO425          
      L=1                                                               FACTO426          
361   IF(L.GT.N) GOTO 369                                               FACTO427          
      A(L1,L)=A(L1,L)-A(ICOLUM,L)*T                                     FACTO428          
      L=L+1                                                             FACTO429          
      GOTO 361                                                          FACTO430          
369   CONTINUE                                                          FACTO431          
380   CONTINUE                                                          FACTO432          
C                                                                       FACTO433          
C     INTERCHANGE COLUMNS                                               FACTO434          
      L1=L1+1                                                           FACTO435          
      GOTO 321                                                          FACTO436          
329   CONTINUE                                                          FACTO437          
      I=I+1                                                             FACTO438          
      GOTO 61                                                           FACTO439          
69    CONTINUE                                                          FACTO440          
      I=1                                                               FACTO441          
391   IF(I.GT.N) GOTO 399                                               FACTO442          
      L=N+1-I                                                           FACTO443          
      IF(INDEX(L,1)-INDEX(L,2)) 420,490,420                             FACTO444          
420   JROW=INDEX(L,1)                                                   FACTO445          
      JCOLUM=INDEX(L,2)                                                 FACTO446          
      K=1                                                               FACTO447          
441   IF(K.GT.N) GOTO 449                                               FACTO448          
      SWAP=A(K,JROW)                                                    FACTO449          
      A(K,JROW)=A(K,JCOLUM)                                             FACTO450          
      A(K,JCOLUM)=SWAP                                                  FACTO451          
      K=K+1                                                             FACTO452          
      GOTO 441                                                          FACTO453          
449   CONTINUE                                                          FACTO454          
490   CONTINUE                                                          FACTO455          
      I=I+1                                                             FACTO456          
      GOTO 391                                                          FACTO457          
399   CONTINUE                                                          FACTO458          
500   WRITE(61,20) DET                                                  FACTO459          
      RETURN                                                            FACTO460          
      END                                                               FACTO461          
      SUBROUTINE EIGNVAL(NAME,N,R,S,C)                                  FACTO462          
      DIMENSION R(80,80),S(80,80),C(80),NAME(10)                        FACTO463          
C     DENTGRO PROJECT PROGRAM CDC003 FORTRAN 36 (SUBROUTINE EIGENVAL)   FACTO464          
C     COMPUTES THE EIGENVALUES AND EIGENVECTORS FOR A REAL SYMMETRIC    FACTO465          
C     MATRIX OF ORDER UP TO 80 BY THE JACOBI METHOD                     FACTO466          
C     ADAPTED FROM PROGRAMS 5.0.016,UA-010 AND DENTGRO PROGRAM 42       FACTO467          
C                                                                       FACTO468          
C     OFF IS THRESHOLD VALUE FOR OFF-DIAGONAL ELEMENTS OF R AND =1.0E-10FACTO469          
C     N IS NUMBER OF VARIABLES                                          FACTO470          
C     ITER IS NUMBER OF ITERATIONS PERFORMED                            FACTO471          
C     EIGENVALUES ARE RETURNED AS THE VECTOR C(N)                       FACTO472          
C     EIGENVECTORS ARE RETURNED AS ELEMENTS OF MATRIX R(N,N)            FACTO473          
C     ORIGINAL CORRELATION MATRIX R(N,N) IS DESTROYED                   FACTO474          
C     NUMBER OF LINES OUTPUT = 9+2N+(N*I) WHERE N = NUMBER OF           FACTO475          
C     VARIABLES AND I = N/10, ROUNDED TO NEXT HIGHEST INTEGER           FACTO476          
C                                                                       FACTO477          
C     FORMAT STATEMENTS                                                 FACTO478          
20    FORMAT(1H1,19X,10A8//)                                            FACTO479          
30    FORMAT(43X,8HEIGENVAL,8HUES AND ,8HEIGENVEC,8HTORS OUT,3HPUT/)    FACTO480          
40    FORMAT(43X,8HNUMBER O,8HF ITERAT,6HIONS =,I12/)                   FACTO481          
50    FORMAT(/10X,8HEIGENVAL,2HUE,I4,F12.8)                             FACTO482          
60    FORMAT(10X,10F10.6)                                               FACTO483          
70    FORMAT(/46X,8HSUBROUTI,8HNE EIGEN,8HVAL COMP,4HLETE)              FACTO484          
C                                                                       FACTO485          
C     BEGIN JACOBI DIAGONALIZATION                                      FACTO486          
      OFF=1.E-10                                                        FACTO487          
      ITER=0                                                            FACTO488          
      INDIC=ITER                                                        FACTO489          
C                                                                       FACTO490          
C     IDENTITY MATRIX ENTERED INTO LOCATIONS S(N,N) AS FIRST            FACTO491          
C     APPROXIMATION TO FINAL MATRIX S                                   FACTO492          
      I=1                                                               FACTO493          
101   IF(I.GT.N) GOTO 109                                               FACTO494          
      J=1                                                               FACTO495          
111   IF(J.GT.N) GOTO 119                                               FACTO496          
      S(I,J)=0.0                                                        FACTO497          
      J=J+1                                                             FACTO498          
      GOTO 111                                                          FACTO499          
119   CONTINUE                                                          FACTO500          
      I=I+1                                                             FACTO501          
      GOTO 101                                                          FACTO502          
109   CONTINUE                                                          FACTO503          
      I=1                                                               FACTO504          
131   IF(I.GT.N) GOTO 139                                               FACTO505          
      S(I,I)=1.0                                                        FACTO506          
      I=I+1                                                             FACTO507          
      GOTO 131                                                          FACTO508          
139   CONTINUE                                                          FACTO509          
      SUM=0.0                                                           FACTO510          
      L=N-1                                                             FACTO511          
      I=1                                                               FACTO512          
171   IF(I.GT.L) GOTO 179                                               FACTO513          
      K=I+1                                                             FACTO514          
      J=K                                                               FACTO515          
191   IF(J.GT.N) GOTO 199                                               FACTO516          
      SUM=SUM+R(I,J)*R(I,J)                                             FACTO517          
      J=J+1                                                             FACTO518          
      GOTO 191                                                          FACTO519          
199   CONTINUE                                                          FACTO520          
      I=I+1                                                             FACTO521          
      GOTO 171                                                          FACTO522          
179   CONTINUE                                                          FACTO523          
      VF=SQRT(SUM*2.0)                                                  FACTO524          
      NB=1                                                              FACTO525          
230   KQ=2                                                              FACTO526          
240   JP=1                                                              FACTO527          
250   IF(VF-ABS(R(JP,KQ))) 260,580,580                                  FACTO528          
260   INDIC=1                                                           FACTO529          
      Y=-R(JP,KQ)                                                       FACTO530          
      ZI=0.5*(R(JP,JP)-R(KQ,KQ))                                        FACTO531          
      W=Y/SQRT(Y*Y+ZI*ZI)                                               FACTO532          
      IF(ZI) 310,320,320                                                FACTO533          
310   W=-W                                                              FACTO534          
320   SN=W/(SQRT(2.0*(1.0+SQRT(1.0-W*W))))                              FACTO535          
      CS=SQRT(1.0-SN*SN)                                                FACTO536          
      IF(NB) 390,390,350                                                FACTO537          
350   S(JP,JP)=CS                                                       FACTO538          
      S(KQ,JP)=-SN                                                      FACTO539          
      S(JP,KQ)=SN                                                       FACTO540          
      S(KQ,KQ)=CS                                                       FACTO541          
390   HOLD1=R(JP,JP)*CS*CS+R(KQ,KQ)*SN*SN-2.0*R(JP,KQ)*SN*CS            FACTO542          
      HOLD2=R(JP,JP)*SN*SN+R(KQ,KQ)*CS*CS+2.0*R(JP,KQ)*SN*CS            FACTO543          
      I=1                                                               FACTO544          
411   IF(I.GT.N) GOTO 419                                               FACTO545          
      D=R(I,JP)*CS-R(I,KQ)*SN                                           FACTO546          
      R(I,KQ)=R(I,JP)*SN+R(I,KQ)*CS                                     FACTO547          
      R(I,JP)=D                                                         FACTO548          
      I=I+1                                                             FACTO549          
      GOTO 411                                                          FACTO550          
419   CONTINUE                                                          FACTO551          
      IF(NB) 480,480,460                                                FACTO552          
460   NB=0                                                              FACTO553          
      GOTO 520                                                          FACTO554          
480   I=1                                                               FACTO555          
481   IF(I.GT.N) GOTO 489                                               FACTO556          
      D=S(I,JP)*CS-S(I,KQ)*SN                                           FACTO557          
      S(I,KQ)=S(I,JP)*SN+S(I,KQ)*CS                                     FACTO558          
      S(I,JP)=D                                                         FACTO559          
      I=I+1                                                             FACTO560          
      GOTO 481                                                          FACTO561          
489   CONTINUE                                                          FACTO562          
520   R(JP,JP)=HOLD1                                                    FACTO563          
      R(KQ,KQ)=HOLD2                                                    FACTO564          
      R(JP,KQ)=0.0                                                      FACTO565          
      I=1                                                               FACTO566          
551   IF(I.GT.N) GOTO 559                                               FACTO567          
      R(JP,I)=R(I,JP)                                                   FACTO568          
      R(KQ,I)=R(I,KQ)                                                   FACTO569          
      I=I+1                                                             FACTO570          
      GOTO 551                                                          FACTO571          
559   CONTINUE                                                          FACTO572          
580   IF(JP-KQ+1) 590,610,610                                           FACTO573          
590   JP=JP+1                                                           FACTO574          
      GOTO 250                                                          FACTO575          
610   IF(KQ-N) 620,640,670                                              FACTO576          
620   KQ=KQ+1                                                           FACTO577          
      GOTO 240                                                          FACTO578          
640   IF(INDIC) 670,670,650                                             FACTO579          
650   INDIC=0                                                           FACTO580          
      GOTO 230                                                          FACTO581          
670   IF(OFF-VF) 680,710,710                                            FACTO582          
680   VF=VF/10.0                                                        FACTO583          
      ITER=ITER+1                                                       FACTO584          
      GOTO 230                                                          FACTO585          
C                                                                       FACTO586          
C     END OF JACOBI DIAGONALIZATION                                     FACTO587          
C     STORE EIGENVALUES IN VECTOR C AND STORE TRANSPOSE OF S IN R       FACTO588          
C     COLUMNS OF MATRIX S BECOME ROWS OF MATRIX R                       FACTO589          
C                                                                       FACTO590          
710   I=1                                                               FACTO591          
711   IF(I.GT.N) GOTO 719                                               FACTO592          
      C(I)=R(I,I)                                                       FACTO593          
      J=1                                                               FACTO594          
731   IF(J.GT.N) GOTO 739                                               FACTO595          
      R(I,J)=S(J,I)                                                     FACTO596          
C                                                                       FACTO597          
C     WRITE EIGENVALUES IN ORDER FOLLOWED BY EIGENVECTORS               FACTO598          
      J=J+1                                                             FACTO599          
      GOTO 731                                                          FACTO600          
739   CONTINUE                                                          FACTO601          
      I=I+1                                                             FACTO602          
      GOTO 711                                                          FACTO603          
719   CONTINUE                                                          FACTO604          
      WRITE(61,20) NAME                                                 FACTO605          
      WRITE(61,30)                                                      FACTO606          
      WRITE(61,40) ITER                                                 FACTO607          
      I=1                                                               FACTO608          
781   IF(I.GT.N) GOTO 789                                               FACTO609          
      WRITE(61,50) I,C(I)                                               FACTO610          
      WRITE(61,60)(R(I,J),J=1,N)                                        FACTO611          
      I=I+1                                                             FACTO612          
      GOTO 781                                                          FACTO613          
789   CONTINUE                                                          FACTO614          
      WRITE(61,70)                                                      FACTO615          
C                                                                       FACTO616          
C     EIGENVALUES STORED IN C(N), EIGENVECTORS STORED IN R(N,N)         FACTO617          
      RETURN                                                            FACTO618          
      END                                                               FACTO619          
      SUBROUTINE FACTORS(NAME,N,R,C,MAXF,VALUE,TRACE)                   FACTO620          
      DIMENSION R(80,80),C(80),CENT(80),NAME(10),VALUE(2)               FACTO621          
C     DENTGRO PROJECT PROGRAM CDC003 FORTRAN 36 (SUBROUTINE FACTORS)    FACTO622          
C     CONVERTS EIGENVALUES AND EIGENVECTORS FROM SUBROUTINE EIGENVAL    FACTO623          
C     INTO PRINCIPAL FACTOR COEFFICIENTS, USING EIGENVALUES GREATER     FACTO624          
C     THAN VALUE(1) AND CONTRIBUTING MORE THAN VALUE(2) PER CENT TO     FACTO625          
C     THE TRACE OF THE CORRELATION MATRIX                               FACTO626          
C     COMPUTES PERCENTAGE CONTRIBUTION OF FACTORS SELECTED              FACTO627          
C     FOR FURTHER ANALYSIS AND THE COMMUNALITIES OF EACH VARIABLE       FACTO628          
C     CHECKS ORTHOGONALITY OF SOLUTION                                  FACTO629          
C     OUTPUT OF PRINCIPAL FACTOR COEFFICIENTS IS IN ORDER OF FACTORS    FACTO630          
C     EACH WITH N VARIABLE LOADINGS                                     FACTO631          
C     PRINTED OUTPUT IN LINES                                           FACTO632          
C        SECTION 1   8 + N  (N=NUMBER OF VARIABLES)                     FACTO633          
C        SECTION 2  15 + MAXF   (MAXF=NO. OF SELECTED FACTORS)          FACTO634          
C        SECTION 3   9+MAXF*(2+I)   I=N/10 ROUNDED TO NEXT HIGHEST      FACTO635          
C            INTEGER                                                    FACTO636          
C        TOTAL      32+N+MAXF*(3+I)                                     FACTO637          
C                                                                       FACTO638          
C                                                                       FACTO639          
C     FORMAT STATEMENTS                                                 FACTO640          
20    FORMAT(1H1,19X,10A8//)                                            FACTO641          
30    FORMAT(20X,8HEIGENVAL,3HUES,2X,8HPER CENT,6X,8HCUMULATI,          FACTO642          
     .8HVE PER C,3HENT/)                                                FACTO643          
40    FORMAT(20X,F11.6,F9.3,10X,F9.3)                                   FACTO644          
50    FORMAT(/46X,8HSUM OF E,8HIGENVALU,2HES,F11.6/)                    FACTO645          
60    FORMAT(45X,8HNUMBER O,8HF SELECT,8HED EIGEN,6HVALUES,I4/)         FACTO646          
70    FORMAT(50X,8HSELECTED,8H EIGENVA,4HLUES/)                         FACTO647          
80    FORMAT(32X,8HPERCENTA,8HGE OF OR,8HIGINAL T,8HOTAL VAR,8HIANCE PR FACTO648          
     .,7HESERVED,F10.4/)                                                FACTO649          
90    FORMAT(45X,8HPRINCIPA,8HL COMPON,8HENTS SOL,5HUTION/)             FACTO650          
100   FORMAT(/10X,6HFACTOR,I4,8H  LOADIN,6HGS FOR,I4,8H  VARIAB,3HLES)  FACTO651          
110   FORMAT(10X,10F10.6)                                               FACTO652          
120   FORMAT(/43X,8HCHECK ON,8H ORTHOGO,6HNALITY,F12.8)                 FACTO653          
130   FORMAT(/46X,8HSUBROUTI,8HNE FACTO,8HRS COMPL,3HETE)               FACTO654          
140   FORMAT(32X,8HPERCENTA,8HGE OF TR,8HACE R PR,7HESERVED,F10.4)      FACTO655          
C                                                                       FACTO656          
C     WRITE INITIAL EIGENVALUES, THEIR PERCENTAGE OF TRACE AND          FACTO657          
C     CUMULATIVE PER CENT                                               FACTO658          
      SUM=0.0                                                           FACTO659          
      CUMUL=SUM                                                         FACTO660          
      WRITE(61,20) NAME                                                 FACTO661          
      WRITE(61,30)                                                      FACTO662          
      I=1                                                               FACTO663          
181   IF(I.GT.N) GOTO 189                                               FACTO664          
      SUM=SUM+C(I)                                                      FACTO665          
      CENT(I)=(C(I)/TRACE)*100.0                                        FACTO666          
      CUMUL=CUMUL+CENT(I)                                               FACTO667          
      WRITE(61,40) C(I),CENT(I),CUMUL                                   FACTO668          
      I=I+1                                                             FACTO669          
      GOTO 181                                                          FACTO670          
189   CONTINUE                                                          FACTO671          
      WRITE(61,50) SUM                                                  FACTO672          
C                                                                       FACTO673          
C     SELECT REQUIRED EIGENVALUES FOR FURTHER ANALYSIS AND PLACE IN     FACTO674          
C     ELEMENTS OF VECTOR C(80)                                          FACTO675          
C     USE MAXF AS NUMBER OF FACTORS RETAINED                            FACTO676          
C     PLACE CORRESPONDING EIGENVECTORS IN ELEMENTS OF R(80,80)          FACTO677          
      MAXF=0                                                            FACTO678          
      I=1                                                               FACTO679          
251   IF(I.GT.N) GOTO 259                                               FACTO680          
      IF(.NOT.(C(I).GE.VALUE(1).AND.CENT(I).GE.VALUE(2))) GOTO 310      FACTO681          
      MAXF=MAXF+1                                                       FACTO682          
      C(MAXF)=C(I)                                                      FACTO683          
      J=1                                                               FACTO684          
291   IF(J.GT.N) GOTO 299                                               FACTO685          
      R(MAXF,J)=R(I,J)                                                  FACTO686          
      J=J+1                                                             FACTO687          
      GOTO 291                                                          FACTO688          
299   CONTINUE                                                          FACTO689          
310   CONTINUE                                                          FACTO690          
C                                                                       FACTO691          
C     NORMALIZE FACTOR MATRIX                                           FACTO692          
      I=I+1                                                             FACTO693          
      GOTO 251                                                          FACTO694          
259   CONTINUE                                                          FACTO695          
      I=1                                                               FACTO696          
321   IF(I.GT.MAXF) GOTO 329                                            FACTO697          
      SRT=SQRT(C(I))                                                    FACTO698          
      J=1                                                               FACTO699          
341   IF(J.GT.N) GOTO 349                                               FACTO700          
      R(I,J)=R(I,J)*SRT                                                 FACTO701          
C                                                                       FACTO702          
C     NORMALIZED LOADINGS NOW STORED IN MATRIX R(MAXF,N)                FACTO703          
C     WRITE SELECTED EIGENVALUES, THEIR PERCENTAGE OF PRESERVED VARIANCEFACTO704          
C     AND THE CUMULATIVE PERCENTAGES OF THE COMMUNALITY                 FACTO705          
      J=J+1                                                             FACTO706          
      GOTO 341                                                          FACTO707          
349   CONTINUE                                                          FACTO708          
      I=I+1                                                             FACTO709          
      GOTO 321                                                          FACTO710          
329   CONTINUE                                                          FACTO711          
      WRITE(61,20) NAME                                                 FACTO712          
      WRITE(61,60) MAXF                                                 FACTO713          
      CUMUL=0.0                                                         FACTO714          
      SUM=CUMUL                                                         FACTO715          
      I=1                                                               FACTO716          
391   IF(I.GT.MAXF) GOTO 399                                            FACTO717          
      SUM=SUM+C(I)                                                      FACTO718          
      I=I+1                                                             FACTO719          
      GOTO 391                                                          FACTO720          
399   CONTINUE                                                          FACTO721          
      WRITE(61,70)                                                      FACTO722          
      WRITE(61,30)                                                      FACTO723          
      I=1                                                               FACTO724          
431   IF(I.GT.MAXF) GOTO 439                                            FACTO725          
      CENT(I)=(C(I)/SUM)*100.0                                          FACTO726          
      CUMUL=CUMUL+CENT(I)                                               FACTO727          
      WRITE(61,40) C(I),CENT(I),CUMUL                                   FACTO728          
      I=I+1                                                             FACTO729          
      GOTO 431                                                          FACTO730          
439   CONTINUE                                                          FACTO731          
      WRITE(61,50) SUM                                                  FACTO732          
      CUMUL=(SUM/FLOAT(N))*100.0                                        FACTO733          
      WRITE(61,80) CUMUL                                                FACTO734          
      CUMUL=(SUM/TRACE)*100.0                                           FACTO735          
      WRITE(61,140) CUMUL                                               FACTO736          
C                                                                       FACTO737          
C     WRITE VARIABLE LOADINGS IN ORDER OF FACTORS TO FORM THE INITIAL   FACTO738          
C     SOLUTION (PRINCIPAL FACTOR SOLUTION)                              FACTO739          
      WRITE(61,20) NAME                                                 FACTO740          
      WRITE(61,90)                                                      FACTO741          
      I=1                                                               FACTO742          
541   IF(I.GT.MAXF) GOTO 549                                            FACTO743          
      WRITE(61,100) I,N                                                 FACTO744          
      WRITE(61,110)(R(I,J),J=1,N)                                       FACTO745          
C                                                                       FACTO746          
C     CHECK ON ORTHOGONALITY                                            FACTO747          
      I=I+1                                                             FACTO748          
      GOTO 541                                                          FACTO749          
549   CONTINUE                                                          FACTO750          
      ORTH=0.0                                                          FACTO751          
      L=MAXF-1                                                          FACTO752          
      I=1                                                               FACTO753          
591   IF(I.GT.N) GOTO 599                                               FACTO754          
      J=1                                                               FACTO755          
601   IF(J.GT.L) GOTO 609                                               FACTO756          
      ORTH=ORTH+R(J,I)*R(J+1,I)                                         FACTO757          
      J=J+1                                                             FACTO758          
      GOTO 601                                                          FACTO759          
609   CONTINUE                                                          FACTO760          
      I=I+1                                                             FACTO761          
      GOTO 591                                                          FACTO762          
599   CONTINUE                                                          FACTO763          
      WRITE(61,120) ORTH                                                FACTO764          
      WRITE(61,130)                                                     FACTO765          
C                                                                       FACTO766          
C     PRINCIPAL FACTOR COEFFICIENTS NOW STORED IN MATRIX R(MAXF,N)      FACTO767          
      RETURN                                                            FACTO768          
      END                                                               FACTO769          
      SUBROUTINE VARIMAX(NAME,N,R,C,MAXF)                               FACTO770          
      DIMENSION R(80,80),C(80),NAME(10)                                 FACTO771          
C     SUBROUTINES VARIMAX AND FACTESTS BELONG TO PROGRAM FACTORAN       FACTO772          
C     DENTGRO PROJECT PROGRAM CDC003 FORTRAN 36 (SUBROUTINE VARIMAX)    FACTO773          
C     PERFORMS VARIMAX ROTATION ON INITIAL PRINCIPAL FACTOR SOLUTION    FACTO774          
C     LISTS COMMUNALITIES OF VARIABLES, COMPUTED FROM INITIAL SOLUTION  FACTO775          
C     EPS IS VALUE TO DETERMINE ROTATION AND IS SET AS FOLLOWS          FACTO776          
C         0.06993 (NO ROTATION IF ANGLE LESS THAN 1 DEGREE)             FACTO777          
C         0.00116 (NO ROTATION IF ANGLE LESS THAN 1 MINUTE)             FACTO778          
C     OUTPUT OF VARIMAX COEFFICIENTS IS IN ORDER OF FACTORS EACH WITH N FACTO779          
C     VARIABLE LOADINGS                                                 FACTO780          
C     REFERENCE -PROGRAM IBM 6.0.094 AND DENTGRO PROGRAM 45             FACTO781          
C     PRINTED OUTPUT IN LINES                                           FACTO782          
C        SECTION 1  10+N  (N=NUMBER OF VARIABLES)                       FACTO783          
C        SECTION 2  10+MAXF*(2+I)    (MAXF=NO. OF FACTORS AND I         FACTO784          
C          =N/10 ROUNDED TO NEXT HIGHEST INTEGER                        FACTO785          
C        TOTAL      20+N+MAXF*(2+I)                                     FACTO786          
C                                                                       FACTO787          
C                                                                       FACTO788          
C     FORMAT STATEMENTS                                                 FACTO789          
20    FORMAT(1H1,19X,10A8//)                                            FACTO790          
30    FORMAT(53X,8HVARIMAX ,8HSOLUTION/)                                FACTO791          
40    FORMAT(42X,8HSQ.ROOT ,8HCOMMUNAL,8HITIES  C,8HOMMUNALI,4HTIES/)   FACTO792          
50    FORMAT(41X,I4,F12.6,10X,F10.6)                                    FACTO793          
60    FORMAT(/39X,8HTOTAL CO,8HMMUNALIT,8HY OF VAR,6HIABLES,F12.6/)     FACTO794          
70    FORMAT(/46X,8HNUMBER O,8HF ROTATI,3HONS,I8/)                      FACTO795          
80    FORMAT(/10X,6HFACTOR,I4)                                          FACTO796          
90    FORMAT(10X,10F10.6)                                               FACTO797          
100   FORMAT(/46X,8HSUBROUTI,8HNE VARIM,8HAX COMPL,3HETE)               FACTO798          
C                                                                       FACTO799          
C     COMPUTE AND LIST COMMUNALITIES OF VARIABLES AND NORMALIZE MATRIX  FACTO800          
      EPS=0.06993                                                       FACTO801          
      WRITE(61,20) NAME                                                 FACTO802          
      WRITE(61,30)                                                      FACTO803          
      WRITE(61,40)                                                      FACTO804          
      SUM=0.0                                                           FACTO805          
      I=1                                                               FACTO806          
161   IF(I.GT.N) GOTO 169                                               FACTO807          
      C(I)=0.0                                                          FACTO808          
      J=1                                                               FACTO809          
181   IF(J.GT.MAXF) GOTO 189                                            FACTO810          
      C(I)=C(I)+R(J,I)*R(J,I)                                           FACTO811          
      J=J+1                                                             FACTO812          
      GOTO 181                                                          FACTO813          
189   CONTINUE                                                          FACTO814          
      SUM=SUM+C(I)                                                      FACTO815          
      COM=C(I)                                                          FACTO816          
      C(I)=SQRT(C(I))                                                   FACTO817          
      WRITE(61,50) I,C(I),COM                                           FACTO818          
      J=1                                                               FACTO819          
241   IF(J.GT.MAXF) GOTO 249                                            FACTO820          
      R(J,I)=R(J,I)/C(I)                                                FACTO821          
      J=J+1                                                             FACTO822          
      GOTO 241                                                          FACTO823          
249   CONTINUE                                                          FACTO824          
      I=I+1                                                             FACTO825          
      GOTO 161                                                          FACTO826          
169   CONTINUE                                                          FACTO827          
      WRITE(61,60) SUM                                                  FACTO828          
C                                                                       FACTO829          
C     COMMENCE FACTOR ROTATION                                          FACTO830          
      L=MAXF-1                                                          FACTO831          
      ITER=0                                                            FACTO832          
290   NOROT=0                                                           FACTO833          
      I=1                                                               FACTO834          
301   IF(I.GT.L) GOTO 309                                               FACTO835          
      K=I+1                                                             FACTO836          
      MONE=K                                                            FACTO837          
321   IF(MONE.GT.MAXF) GOTO 329                                         FACTO838          
      E=0.0                                                             FACTO839          
      D=E                                                               FACTO840          
      B=D                                                               FACTO841          
      A=B                                                               FACTO842          
      J=1                                                               FACTO843          
341   IF(J.GT.N) GOTO 349                                               FACTO844          
      U=R(I,J)*R(I,J)-R(MONE,J)*R(MONE,J)                               FACTO845          
      V=R(I,J)*R(MONE,J)*2.0                                            FACTO846          
      A=A+U                                                             FACTO847          
      B=B+V                                                             FACTO848          
      E=E+U*U-V*V                                                       FACTO849          
      D=D+U*V*2.0                                                       FACTO850          
      J=J+1                                                             FACTO851          
      GOTO 341                                                          FACTO852          
349   CONTINUE                                                          FACTO853          
      T=N                                                               FACTO854          
      QNUM=D-2.0*A*B/T                                                  FACTO855          
      QDEN=E-(A*A-B*B)/T                                                FACTO856          
      IF(ABS(QNUM)+ABS(QDEN)) 770,770,450                               FACTO857          
450   IF(ABS(QNUM)-ABS(QDEN)) 460,630,550                               FACTO858          
460   T=ABS(QNUM/QDEN)                                                  FACTO859          
      IF(T-EPS) 510,480,480                                             FACTO860          
480   CS4TH=COS(ATAN(T))                                                FACTO861          
      SN4TH=SIN(ATAN(T))                                                FACTO862          
      GOTO 650                                                          FACTO863          
510   IF(QDEN) 520,770,770                                              FACTO864          
520   SNPHI=0.70710678                                                  FACTO865          
      CSPHI=SNPHI                                                       FACTO866          
      GOTO 790                                                          FACTO867          
550   T=ABS(QDEN/QNUM)                                                  FACTO868          
      IF(T-EPS) 600,570,570                                             FACTO869          
570   SN4TH=1.0/SQRT(1.0+T*T)                                           FACTO870          
      CS4TH=SN4TH*T                                                     FACTO871          
      GOTO 650                                                          FACTO872          
600   CS4TH=0.0                                                         FACTO873          
      SN4TH=1.0                                                         FACTO874          
      GOTO 650                                                          FACTO875          
630   CS4TH=0.70710678                                                  FACTO876          
      SN4TH=CS4TH                                                       FACTO877          
650   T=SQRT((1.0+CS4TH)*0.5)                                           FACTO878          
      CSTH=SQRT((1.0+T)*0.5)                                            FACTO879          
      SNTH=SN4TH/(4.0*CSTH*T)                                           FACTO880          
      IF(QDEN) 690,720,720                                              FACTO881          
690   CSPHI=0.70710678*(CSTH+SNTH)                                      FACTO882          
      SNPHI=0.70710678*(CSTH-SNTH)                                      FACTO883          
      GOTO 740                                                          FACTO884          
720   CSPHI=CSTH                                                        FACTO885          
      SNPHI=SNTH                                                        FACTO886          
740   IF(QNUM) 750,790,790                                              FACTO887          
750   SNPHI=-SNPHI                                                      FACTO888          
      GOTO 790                                                          FACTO889          
770   NOROT=NOROT+1                                                     FACTO890          
      GOTO 830                                                          FACTO891          
790   J=1                                                               FACTO892          
791   IF(J.GT.N) GOTO 799                                               FACTO893          
      T=R(I,J)*CSPHI+R(MONE,J)*SNPHI                                    FACTO894          
      R(MONE,J)=R(MONE,J)*CSPHI-R(I,J)*SNPHI                            FACTO895          
      R(I,J)=T                                                          FACTO896          
830   CONTINUE                                                          FACTO897          
      J=J+1                                                             FACTO898          
      GOTO 791                                                          FACTO899          
799   CONTINUE                                                          FACTO900          
      MONE=MONE+1                                                       FACTO901          
      GOTO 321                                                          FACTO902          
329   CONTINUE                                                          FACTO903          
      I=I+1                                                             FACTO904          
      GOTO 301                                                          FACTO905          
309   CONTINUE                                                          FACTO906          
      ITER=ITER+1                                                       FACTO907          
      IF(NOROT-(MAXF*L)/2) 290,860,290                                  FACTO908          
860   J=1                                                               FACTO909          
861   IF(J.GT.N) GOTO 869                                               FACTO910          
      I=1                                                               FACTO911          
871   IF(I.GT.MAXF) GOTO 879                                            FACTO912          
      R(I,J)=R(I,J)*C(J)                                                FACTO913          
C                                                                       FACTO914          
C     END MATRIX ROTATION                                               FACTO915          
C                                                                       FACTO916          
C     LIST VARIMAX FACTOR LOADINGS                                      FACTO917          
      I=I+1                                                             FACTO918          
      GOTO 871                                                          FACTO919          
879   CONTINUE                                                          FACTO920          
      J=J+1                                                             FACTO921          
      GOTO 861                                                          FACTO922          
869   CONTINUE                                                          FACTO923          
      WRITE(61,20) NAME                                                 FACTO924          
      WRITE(61,30)                                                      FACTO925          
      WRITE(61,70) ITER                                                 FACTO926          
      I=1                                                               FACTO927          
921   IF(I.GT.MAXF) GOTO 929                                            FACTO928          
      WRITE(61,80) I                                                    FACTO929          
      WRITE(61,90)(R(I,J),J=1,N)                                        FACTO930          
C                                                                       FACTO931          
C     END FACTOR OUTPUT                                                 FACTO932          
      I=I+1                                                             FACTO933          
      GOTO 921                                                          FACTO934          
929   CONTINUE                                                          FACTO935          
      WRITE(61,100)                                                     FACTO936          
C     VARIMAX FACTOR COEFFICIENTS STORED IN ELEMENTS OF R(MAXF,N)       FACTO937          
      RETURN                                                            FACTO938          
      END                                                               FACTO939          
      SUBROUTINE FACESTS(NAME,N,R,S,C,MAXF,M)                           FACTO940          
      INTEGER EOFCKF                                                    FACTO941          
      DIMENSION R(80,80),S(80,80),C(80),NAME(10),UNI(80)                FACTO942          
C     DENTGRO PROJECT PROGRAM CDC003 FORTRAN 36 (SUBROUTINE FACTESTS)   FACTO943          
C     CARRIES OUT PROCEDURES ON VARIMAX FACTOR MATRIX                   FACTO944          
C     OUTPUT FROM FACTESTS INCLUDES                                     FACTO945          
C       COMMUNALITY AND UNIQUENESS OF EACH VARIABLE                     FACTO946          
C       TOTAL COMMUNALITY OF VARIABLES                                  FACTO947          
C       CONTRIBUTION AND PERCENTAGE CONTRIBUTION OF FACTORS             FACTO948          
C       AVERAGE COEFFICIENT IN FACTOR MATRIX                            FACTO949          
C       RESTORED CORRELATION MATRIX                                     FACTO950          
C       RESIDUAL CORRELATION MATRIX                                     FACTO951          
C     ORIGINAL CORRELATION MATRIX IS ENTERED IN TRIANGULAR FORM FROM SCRFACTO952          
C     TAPE 50                                                           FACTO953          
C     LINES OUTPUT PER PRINTER                                          FACTO954          
C        SECTION 1  16+N+MAXF  (N=NO.VARIABLES, MAXF=NO.FACTORS)        FACTO955          
C        SECTION 2  4+(N*5)/2+(N*N)/20   (APPROX.)                      FACTO956          
C        SECTION 3  9+(N*5)/2+(N*N)/20   (APPROX.)                      FACTO957          
C        TOTAL     29+MAXF+(N*6)+(N*N)/10  (APPROX.)                    FACTO958          
C     EACH SECTION ON NEW PAGE                                          FACTO959          
C                                                                       FACTO960          
C     FORMAT STATEMENTS                                                 FACTO961          
20    FORMAT(1H1,19X,10A8//)                                            FACTO962          
30    FORMAT(40X,8HFACTOR A,8HNALYSIS ,8H  TESTS ,8HON FACTO,8HR MATRIX FACTO963          
     ./)                                                                FACTO964          
40    FORMAT(40X,8HVARIABLE,4X,8HCOMMUNAL,3HITY,6X,8HUNIQUENE,2HSS/)    FACTO965          
50    FORMAT(43X,I3,2(6X,F10.6))                                        FACTO966          
60    FORMAT(/40X,8HTOTAL CO,8HMMUNALIT,1HY,F12.6/)                     FACTO967          
70    FORMAT(48X,8HCONTRIBU,8HTIONS OF,8H FACTORS/)                     FACTO968          
80    FORMAT(43X,I3,2F16.6,2X,8HPER CENT)                               FACTO969          
90    FORMAT(/25X,8HTOTAL FA,8HCTOR CON,8HTRIBUTIO,1HN,F12.6,4H FOR,    FACTO970          
     .F12.6,8H  PER CE,2HNT)                                            FACTO971          
100   FORMAT(/40X,8HAVERAGE ,8HCOEFFICI,3HENT,F10.6,4H FOR,I5,          FACTO972          
     .8H LOADING,1HS)                                                   FACTO973          
110   FORMAT(46X,8HRESTORED,8H CORRELA,8HTION MAT,3HRIX)                FACTO974          
120   FORMAT(/10X,8HVARIABLE,I4)                                        FACTO975          
130   FORMAT(10X,10F10.6)                                               FACTO976          
140   FORMAT(40X,8HPARITY E,8HRROR DUR,8HING READ,8H ON TAPE,3H 50)     FACTO977          
150   FORMAT(/33X,I4,8H  ROWS O,8HF COEFFI,8HCIENTS T,8HRANSMITT,       FACTO978          
     .8HED FROM ,7HTAPE 50/)                                            FACTO979          
160   FORMAT(40X,8HEND OF F,8HILE ERRO,8HR DURING,8H READ ON,8H TAPE 50 FACTO980          
     ./)                                                                FACTO981          
170   FORMAT(46X,8HRESIDUAL,8H CORRELA,8HTION MAT,3HRIX)                FACTO982          
180   FORMAT(/46X,8HSUBROUTI,8HNE FACTE,8HSTS COMP,4HLETE)              FACTO983          
190   FORMAT(/27X,8HCHI-SQUA,8HRE TEST ,8HON RESID,8HUALS   C,          FACTO984          
     .8HRITERION,F12.4,4H FOR,I6,4H DOF/)                               FACTO985          
C                                                                       FACTO986          
C     COMPUTE AND PRINT COMMUNALITY AND UNIQUENESS OF EACH VARIABLE,    FACTO987          
C     AND COMMUNALITY AND CONTRIBUTIONS OF FACTORS                      FACTO988          
      WRITE(61,20) NAME                                                 FACTO989          
      WRITE(61,30)                                                      FACTO990          
      WRITE(61,40)                                                      FACTO991          
      TOTC=0.0                                                          FACTO992          
      I=1                                                               FACTO993          
241   IF(I.GT.N) GOTO 249                                               FACTO994          
      C(I)=0.0                                                          FACTO995          
      J=1                                                               FACTO996          
261   IF(J.GT.MAXF) GOTO 269                                            FACTO997          
      C(I)=C(I)+R(J,I)*R(J,I)                                           FACTO998          
      J=J+1                                                             FACTO999          
      GOTO 261                                                          FACTP000          
269   CONTINUE                                                          FACTP001          
      TOTC=TOTC+C(I)                                                    FACTP002          
      UNI(I)=1.0-C(I)                                                   FACTP003          
      WRITE(61,50) I,C(I),UNI(I)                                        FACTP004          
      I=I+1                                                             FACTP005          
      GOTO 241                                                          FACTP006          
249   CONTINUE                                                          FACTP007          
      WRITE(61,60) TOTC                                                 FACTP008          
      WRITE(61,70)                                                      FACTP009          
      TCENT=0.0                                                         FACTP010          
      TCONT=TCENT                                                       FACTP011          
      I=1                                                               FACTP012          
341   IF(I.GT.MAXF) GOTO 349                                            FACTP013          
      CONT=0.0                                                          FACTP014          
      J=1                                                               FACTP015          
361   IF(J.GT.N) GOTO 369                                               FACTP016          
      CONT=CONT+R(I,J)*R(I,J)                                           FACTP017          
      J=J+1                                                             FACTP018          
      GOTO 361                                                          FACTP019          
369   CONTINUE                                                          FACTP020          
      CENT=CONT*100.0/TOTC                                              FACTP021          
      TCONT=TCONT+CONT                                                  FACTP022          
      TCENT=TCENT+CENT                                                  FACTP023          
      WRITE(61,80) I,CONT,CENT                                          FACTP024          
      I=I+1                                                             FACTP025          
      GOTO 341                                                          FACTP026          
349   CONTINUE                                                          FACTP027          
      WRITE(61,90) TCONT,TCENT                                          FACTP028          
C                                                                       FACTP029          
C     PRINT AVERAGE COEFFICIENT                                         FACTP030          
      EN=N*MAXF                                                         FACTP031          
      AVR=0.0                                                           FACTP032          
      I=1                                                               FACTP033          
451   IF(I.GT.MAXF) GOTO 459                                            FACTP034          
      J=1                                                               FACTP035          
461   IF(J.GT.N) GOTO 469                                               FACTP036          
      AVR=AVR+ABS(R(I,J))                                               FACTP037          
      J=J+1                                                             FACTP038          
      GOTO 461                                                          FACTP039          
469   CONTINUE                                                          FACTP040          
      I=I+1                                                             FACTP041          
      GOTO 451                                                          FACTP042          
459   CONTINUE                                                          FACTP043          
      AVR=AVR/EN                                                        FACTP044          
      NUMBER=EN                                                         FACTP045          
      WRITE(61,100) AVR,NUMBER                                          FACTP046          
C                                                                       FACTP047          
C     RESTORE ORIGINAL MATRIX AND PLACE IN S (TRIANGULAR ELEMENTS)      FACTP048          
      I=1                                                               FACTP049          
511   IF(I.GT.N) GOTO 519                                               FACTP050          
      J=I                                                               FACTP051          
521   IF(J.GT.N) GOTO 529                                               FACTP052          
      S(I,J)=0.0                                                        FACTP053          
      K=1                                                               FACTP054          
541   IF(K.GT.MAXF) GOTO 549                                            FACTP055          
      S(I,J)=S(I,J)+R(K,I)*R(K,J)                                       FACTP056          
      K=K+1                                                             FACTP057          
      GOTO 541                                                          FACTP058          
549   CONTINUE                                                          FACTP059          
      J=J+1                                                             FACTP060          
      GOTO 521                                                          FACTP061          
529   CONTINUE                                                          FACTP062          
      I=I+1                                                             FACTP063          
      GOTO 511                                                          FACTP064          
519   CONTINUE                                                          FACTP065          
      WRITE(61,20) NAME                                                 FACTP066          
      WRITE(61,110)                                                     FACTP067          
      I=1                                                               FACTP068          
581   IF(I.GT.N) GOTO 589                                               FACTP069          
      WRITE(61,120) I                                                   FACTP070          
      WRITE(61,130)(S(I,J),J=I,N)                                       FACTP071          
C                                                                       FACTP072          
C     READ IN ORIGINAL CORRELATION MATRIX FROM SCRATCH TAPE 50 AND PLACEFACTP073          
C        INTO STORAGE R(80,80)    CHECK READ OPERATION                  FACTP074          
      I=I+1                                                             FACTP075          
      GOTO 581                                                          FACTP076          
589   CONTINUE                                                          FACTP077          
      REWIND 50                                                         FACTP078          
      I=1                                                               FACTP079          
621   IF(I.GT.N) GOTO 629                                               FACTP080          
      READ(50,130)(R(I,J),J=I,N)                                        FACTP081          
      K=IOCHKF(50,J)                                                    FACTP082          
      KQZ001=K                                                          FACTP083          
      IF(KQZ001.LT.1) KQZ001=1                                          FACTP084          
      IF(KQZ001.GT.2) KQZ001=2                                          FACTP085          
      GOTO(660,680),KQZ001                                              FACTP086          
660   WRITE(61,140)                                                     FACTP087          
      GOTO 940                                                          FACTP088          
680   K=EOFCKF(50,J)                                                    FACTP089          
      KQZ001=K                                                          FACTP090          
      IF(KQZ001.LT.1) KQZ001=1                                          FACTP091          
      IF(KQZ001.GT.2) KQZ001=2                                          FACTP092          
      GOTO(720,700),KQZ001                                              FACTP093          
700   CONTINUE                                                          FACTP094          
      I=I+1                                                             FACTP095          
      GOTO 621                                                          FACTP096          
629   CONTINUE                                                          FACTP097          
      GOTO 760                                                          FACTP098          
720   WRITE(61,150) I                                                   FACTP099          
      IF(I-N) 740,760,740                                               FACTP100          
740   WRITE(61,160)                                                     FACTP101          
      GOTO 940                                                          FACTP102          
C                                                                       FACTP103          
C     COMPUTE AND PRINT RESIDUAL CORRELATION MATRIX AND STORE IN R      FACTP104          
760   WRITE(61,20) NAME                                                 FACTP105          
      WRITE(61,170)                                                     FACTP106          
      I=1                                                               FACTP107          
781   IF(I.GT.N) GOTO 789                                               FACTP108          
      J=I                                                               FACTP109          
791   IF(J.GT.N) GOTO 799                                               FACTP110          
      R(I,J)=R(I,J)-S(I,J)                                              FACTP111          
      J=J+1                                                             FACTP112          
      GOTO 791                                                          FACTP113          
799   CONTINUE                                                          FACTP114          
      I=I+1                                                             FACTP115          
      GOTO 781                                                          FACTP116          
789   CONTINUE                                                          FACTP117          
      I=1                                                               FACTP118          
811   IF(I.GT.N) GOTO 819                                               FACTP119          
      WRITE(61,120) I                                                   FACTP120          
      WRITE(61,130)(R(I,J),J=I,N)                                       FACTP121          
C                                                                       FACTP122          
C     COMPUTE CHI-SQUARE TEST ON RESIDUAL MATRIX                        FACTP123          
      I=I+1                                                             FACTP124          
      GOTO 811                                                          FACTP125          
819   CONTINUE                                                          FACTP126          
      NDOF=((N-MAXF)*(N-MAXF)-N-MAXF)/2                                 FACTP127          
      CRIT=0.0                                                          FACTP128          
      NI=N-1                                                            FACTP129          
      I=1                                                               FACTP130          
871   IF(I.GT.NI) GOTO 879                                              FACTP131          
      JO=I+1                                                            FACTP132          
      J=JO                                                              FACTP133          
891   IF(J.GT.N) GOTO 899                                               FACTP134          
      CRIT=CRIT+(R(I,J)*R(I,J))/(UNI(I)*UNI(J))                         FACTP135          
      J=J+1                                                             FACTP136          
      GOTO 891                                                          FACTP137          
899   CONTINUE                                                          FACTP138          
      I=I+1                                                             FACTP139          
      GOTO 871                                                          FACTP140          
879   CONTINUE                                                          FACTP141          
      EM=M-1                                                            FACTP142          
      CRIT=CRIT*EM                                                      FACTP143          
      WRITE(61,190) CRIT,NDOF                                           FACTP144          
940   WRITE(61,180)                                                     FACTP145          
      REWIND 50                                                         FACTP146          
      RETURN                                                            FACTP147          
      END                                                               FACTP148          
