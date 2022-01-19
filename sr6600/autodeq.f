      SUBROUTINE NORDINT(K,T,H,N,Y,F,DELTAY,B,NTL,TL,NPL,PL)            AUTOD002          
      INTEGER DOUBLE,FIND,HALVE,STEP                                    AUTOD003          
      DOUBLEPRECISION DPTEMA                                            AUTOD004          
      DIMENSION Y(1),F(1),B(10,1),TL(1),PL(1),DPTA(2),TEST(2),FIND(10), AUTOD005          
     .PLEFT(10),PRITE(10)                                               AUTOD006          
      COMMON/DATA/IDER,IEOS,ITL,IPL,STEP,HMAX,HMIN,HBIG,HL              AUTOD007          
      COMMON/KQZCOM/KQZSBX,KQZB00,KQZB06                                AUTOD008          
      EQUIVALENCE(DPTA,DPTEMA)                                          AUTOD009          
C                                                                       AUTOD010          
C     K            CONTROL INTEGER FOR USER STATEMENTS     INTEGER      AUTOD011          
C     T            INDEPENDENT VARIABLE                    REAL         AUTOD012          
C     H            INTEGRATION STEP SIZE                   REAL         AUTOD013          
C     N            NUMBER OF FIRST ORDER EQUATIONS         INTEGER      AUTOD014          
C     Y            DEPENDENT VARIABLES                     REAL         AUTOD015          
C     F            DERIVATIVES                             REAL         AUTOD016          
C     DELTAY       ERROR CONTROL VECTOR                    REAL         AUTOD017          
C     B            TEMPORARY STORAGE, DIMENSION 10*N       REAL         AUTOD018          
C     NTL          NUMBER OF ENTRIES IN TL                 INTEGER      AUTOD019          
C     TL           LIST OF INTERRUPT TIMES                 REAL         AUTOD020          
C     NPL          NUMBER OF ENTRIES IN PL                 INTEGER      AUTOD021          
C     PL           LIST OF INTERRUPT FUNCTIONS             REAL         AUTOD022          
C                                                                       AUTOD023          
C     B(1,I)       EQUIVALENT                                           AUTOD024          
C     B(2,I)           OF                                               AUTOD025          
C     B(3,I)               ADAMS                                        AUTOD026          
C     B(4,I)                  DIFFERENCES                               AUTOD027          
C     B(5,I)       PREDICTED DERIVATIVES                                AUTOD028          
C     B(6,I)       Y AT START OF INTEGRATION STEP                       AUTOD029          
C     B(7,I)       SECOND PRECISION PART OF Y ABOVE                     AUTOD030          
C     B(8,I)       F AT START OF INTEGRATION STEP                       AUTOD031          
C     B(9,I)       HOLE FOR INITIAL Y WHILE STARTING                    AUTOD032          
C     B(10,I)      SECOND PRECISION PART OF Y                           AUTOD033          
C                                                                       AUTOD034          
      DATA(HBIG=0.),(HL=0.)                                             AUTOD035          
CL    TYPE DOUBLE (4) DPTEMA                                            AUTOD036          
C                                                                       AUTOD037          
      GOTO 2                                                            AUTOD038          
1     GOTO(130,190,270,540,980,450,380,530,810,1970,630,2000,1550,910,  AUTOD039          
     .950,1130,1690,1030),KQZSBX                                        AUTOD040          
2     CONTINUE                                                          AUTOD041          
      IF(.NOT.(K.NE.0)) GOTO 60                                         AUTOD042          
      KQZSBX=KFLIP                                                      AUTOD043          
      GOTO 1                                                            AUTOD044          
C                                                                       AUTOD045          
C     TEST FOR CALLING SEQUENCE ERROR                                   AUTOD046          
C                                                                       AUTOD047          
60    CONTINUE                                                          AUTOD048          
      IF(H.LE.0..OR.N.LE.0.OR.N.GT.2000.OR.NTL.LT.0.OR.NTL.GT.500.OR.   AUTOD049          
     .NPL.LT.0.OR.NPL.GT.500.OR.DELTAY.LE.0..OR.T.LT.0.) GOTO 2390      AUTOD050          
C                                                                       AUTOD051          
C     SET SUBROUTINE COUNTERS AND STEP SIZE DATA                        AUTOD052          
C                                                                       AUTOD053          
      HMIN=0.                                                           AUTOD054          
      HMAX=HMIN                                                         AUTOD055          
      STEP=0                                                            AUTOD056          
      IPL=STEP                                                          AUTOD057          
      ITL=IPL                                                           AUTOD058          
      IEOS=ITL                                                          AUTOD059          
      IDER=IEOS                                                         AUTOD060          
C                                                                       AUTOD061          
C     CONTROL SECTION FOR STARTING INTEGRATION                          AUTOD062          
C                                                                       AUTOD063          
      KFLIP=1                                                           AUTOD064          
      GOTO 1970                                                         AUTOD065          
130   H=H.AND.3777 4000 0000 0000 0000B                                 AUTOD066          
      HTEST=.5*H                                                        AUTOD067          
      IF(1.GT.NTL) GOTO 159                                             AUTOD068          
      DO 158 J=1,NTL                                                    AUTOD069          
      IF(.NOT.(T.EQ.TL(J))) GOTO 190                                    AUTOD070          
      KFLIP=2                                                           AUTOD071          
      GOTO 2000                                                         AUTOD072          
190   CONTINUE                                                          AUTOD073          
158   CONTINUE                                                          AUTOD074          
159   CONTINUE                                                          AUTOD075          
      TLEFT=T                                                           AUTOD076          
      IF(1.GT.NPL) GOTO 219                                             AUTOD077          
      DO 218 J=1,NPL                                                    AUTOD078          
      PLEFT(J)=PL(J)                                                    AUTOD079          
      IF(PL(J).NE.0) GOTO 270                                           AUTOD080          
      KFLIP=3                                                           AUTOD081          
      GOTO 2030                                                         AUTOD082          
270   CONTINUE                                                          AUTOD083          
218   CONTINUE                                                          AUTOD084          
219   CONTINUE                                                          AUTOD085          
      IF(1.GT.N) GOTO 289                                               AUTOD086          
      DO 288 I=1,N                                                      AUTOD087          
      B(9,I)=Y(I)                                                       AUTOD088          
288   CONTINUE                                                          AUTOD089          
289   CONTINUE                                                          AUTOD090          
      D1=-1.                                                            AUTOD091          
      ISFOUR=4                                                          AUTOD092          
      GOTO 2060                                                         AUTOD093          
330   I=STEP.AND.3                                                      AUTOD094          
      IF(I.NE.0) GOTO 980                                               AUTOD095          
      I=STEP/4                                                          AUTOD096          
      KQZ001=I                                                          AUTOD097          
      IF(KQZ001.LT.1) KQZ001=1                                          AUTOD098          
      IF(KQZ001.GT.6) KQZ001=6                                          AUTOD099          
      GOTO(380,450,380,500,380,410),KQZ001                              AUTOD100          
380   D1=-1.                                                            AUTOD101          
      ISFOUR=5                                                          AUTOD102          
      GOTO 2060                                                         AUTOD103          
410   D1=2.                                                             AUTOD104          
      HMIN=-H                                                           AUTOD105          
      HMAX=HMIN                                                         AUTOD106          
      ISFOUR=6                                                          AUTOD107          
      GOTO 2060                                                         AUTOD108          
450   IF(1.GT.N) GOTO 459                                               AUTOD109          
      DO 458 I=1,N                                                      AUTOD110          
      Y(I)=B(9,I)                                                       AUTOD111          
      B(10,I)=0.0                                                       AUTOD112          
458   CONTINUE                                                          AUTOD113          
459   CONTINUE                                                          AUTOD114          
      KFLIP=7                                                           AUTOD115          
      GOTO 1940                                                         AUTOD116          
500   D1=.5                                                             AUTOD117          
      ISFOUR=8                                                          AUTOD118          
      GOTO 2060                                                         AUTOD119          
530   IF(.NOT.(HALVE.NE.0.AND.HTEST.GE.HL)) GOTO 450                    AUTOD120          
540   STEP=0                                                            AUTOD121          
      IF(1.GT.N) GOTO 559                                               AUTOD122          
      DO 558 I=1,N                                                      AUTOD123          
      B(4,I)=0.0                                                        AUTOD124          
      B(3,I)=B(4,I)                                                     AUTOD125          
      B(2,I)=B(3,I)                                                     AUTOD126          
      B(1,I)=B(2,I)                                                     AUTOD127          
558   CONTINUE                                                          AUTOD128          
559   CONTINUE                                                          AUTOD129          
      GOTO 450                                                          AUTOD130          
C                                                                       AUTOD131          
C     CONTROL SECTION FOR TIME INTERUPTS DURING NORMAL INTEGRATION      AUTOD132          
C                   STATEMENT 1700 INTEGRATES FORWARD,RETURNING TO 1701 AUTOD133          
C                                                                       AUTOD134          
580   GOTO 1890                                                         AUTOD135          
590   IF(1.GT.N) GOTO 599                                               AUTOD136          
      DO 598 I=1,N                                                      AUTOD137          
      B(6,I)=Y(I)                                                       AUTOD138          
      B(8,I)=F(I)                                                       AUTOD139          
598   CONTINUE                                                          AUTOD140          
599   CONTINUE                                                          AUTOD141          
      TSAVE=T                                                           AUTOD142          
630   Z=2.*TSAVE                                                        AUTOD143          
      IF(1.GT.NTL) GOTO 649                                             AUTOD144          
      DO 648 I=1,NTL                                                    AUTOD145          
      IF(.NOT.(TL(I).LT.Z)) GOTO 680                                    AUTOD146          
      Z=TL(I)                                                           AUTOD147          
      J=I                                                               AUTOD148          
680   CONTINUE                                                          AUTOD149          
648   CONTINUE                                                          AUTOD150          
649   CONTINUE                                                          AUTOD151          
      IF(Z.GE.TSAVE) GOTO 840                                           AUTOD152          
      KFLIP=9                                                           AUTOD153          
      TSAVEZ=TSAVE/Z                                                    AUTOD154          
      TSAVEZ=TSAVEZ.AND..NOT.3                                          AUTOD155          
      IF (TSAVEZ.NE.1.) GOTO 780                                        AUTOD156          
      IF(1.GT.N) GOTO 749                                               AUTOD157          
      DO 748 I=1,N                                                      AUTOD158          
      Y(I)=B(6,I)                                                       AUTOD159          
748   CONTINUE                                                          AUTOD160          
749   CONTINUE                                                          AUTOD161          
      T=TSAVE                                                           AUTOD162          
      GOTO 1970                                                         AUTOD163          
780   HP=Z-TSAVE                                                        AUTOD164          
      ISTWO=10                                                          AUTOD165          
      GOTO 2170                                                         AUTOD166          
810   KFLIP=11                                                          AUTOD167          
      ISTHREE=12                                                        AUTOD168          
      GOTO 1550                                                         AUTOD169          
840   IF(1.GT.N) GOTO 849                                               AUTOD170          
      DO 848 I=1,N                                                      AUTOD171          
      F(I)=B(8,I)                                                       AUTOD172          
      Y(I)=B(6,I)                                                       AUTOD173          
848   CONTINUE                                                          AUTOD174          
849   CONTINUE                                                          AUTOD175          
      T=TSAVE                                                           AUTOD176          
      KFLIP=13                                                          AUTOD177          
      ISTHREE=14                                                        AUTOD178          
      GOTO 1970                                                         AUTOD179          
910   TSAVEZ=Z/T                                                        AUTOD180          
      TSAVEZ=TSAVEZ.AND..NOT.3                                          AUTOD181          
      IF (TSAVEZ.NE.1.) GOTO 950                                        AUTOD182          
      KFLIP=15                                                          AUTOD183          
      GOTO 2000                                                         AUTOD184          
950   IF(1.GT.NPL) GOTO 959                                             AUTOD185          
      DO 958 I=1,NPL                                                    AUTOD186          
      FIND(I)=0                                                         AUTOD187          
958   CONTINUE                                                          AUTOD188          
959   CONTINUE                                                          AUTOD189          
      GOTO 580                                                          AUTOD190          
C                                                                       AUTOD191          
C     INTEGRATE ONE STEP                                                AUTOD192          
C                                                                       AUTOD193          
C                   SAVE CONDITIONS AT START OF STEP                    AUTOD194          
C                                                                       AUTOD195          
980   IF(1.GT.N) GOTO 989                                               AUTOD196          
      DO 988 I=1,N                                                      AUTOD197          
      B(6,I)=Y(I)                                                       AUTOD198          
      B(7,I)=B(10,I)                                                    AUTOD199          
      B(8,I)=F(I)                                                       AUTOD200          
988   CONTINUE                                                          AUTOD201          
989   CONTINUE                                                          AUTOD202          
      TSTART=T                                                          AUTOD203          
C                                                                       AUTOD204          
C                   ENTRY FOR HALVED STEP                               AUTOD205          
C                                                                       AUTOD206          
1030  T=T+H                                                             AUTOD207          
      IF(1.GT.N) GOTO 1049                                              AUTOD208          
      DO 1048 I=1,N                                                     AUTOD209          
      Y(I)=B(6,I)+H*(B(8,I)+(B(1,I)+(B(2,I)+(B(3,I)+(B(4,I))))))        AUTOD210          
      B(5,I)=F(I)+(2.*B(1,I)+(3.*B(2,I)+(4.*B(3,I)+(5.*B(4,I)))))       AUTOD211          
C                                                                       AUTOD212          
C                   ITERATE TWICE,DEVELOP TEST PARAMETERS               AUTOD213          
C                                                                       AUTOD214          
1048  CONTINUE                                                          AUTOD215          
1049  CONTINUE                                                          AUTOD216          
      HALVE=0                                                           AUTOD217          
      DOUBLE=1                                                          AUTOD218          
      TEST(2)=0.                                                        AUTOD219          
      TEST(1)=TEST(2)                                                   AUTOD220          
      DO 1108 J=1,2                                                     AUTOD221          
      KFLIP=16                                                          AUTOD222          
      GOTO 1940                                                         AUTOD223          
1130  IF(1.GT.N) GOTO 1139                                              AUTOD224          
      DO 1138 I=1,N                                                     AUTOD225          
      Z=F(I)-B(5,I)                                                     AUTOD226          
      IF(.NOT.(J.EQ.2)) GOTO 1220                                       AUTOD227          
      ZZ=ABS(Z*H)                                                       AUTOD228          
      RTEST=DELTAY*(ABS(Y(I))+1.E-10)                                   AUTOD229          
      IF(.NOT.(ZZ.GT.RTEST)) GOTO 1190                                  AUTOD230          
      HALVE=1                                                           AUTOD231          
1190  CONTINUE                                                          AUTOD232          
      IF(.NOT.(ZZ.GT.RTEST*.015625)) GOTO 1210                          AUTOD233          
      DOUBLE=0                                                          AUTOD234          
1210  CONTINUE                                                          AUTOD235          
1220  DPTA(1)=B(6,I)                                                    AUTOD236          
      DPTA(2)=B(7,I)                                                    AUTOD237          
      Z=Z*.3298611111                                                   AUTOD238          
      DPTEMA=DPTEMA+DBLE(H)*DBLE(B(8,I)+(B(1,I)+(B(2,I)+(B(3,I)+(B(4,I) AUTOD239          
     .+Z)))))                                                           AUTOD240          
      ZZ=ABS(DPTA(1)-Y(I))                                              AUTOD241          
      IF(.NOT.(ZZ.GT.TEST(J))) GOTO 1280                                AUTOD242          
      TEST(J)=ZZ                                                        AUTOD243          
1280  CONTINUE                                                          AUTOD244          
      Y(I)=DPTA(1)                                                      AUTOD245          
      B(10,I)=DPTA(2)                                                   AUTOD246          
C                                                                       AUTOD247          
C     CHECK TEST PARAMETERS,BUMP COUNT OF INTEGRATION STEPS             AUTOD248          
C                                                                       AUTOD249          
1138  CONTINUE                                                          AUTOD250          
1139  CONTINUE                                                          AUTOD251          
1108  CONTINUE                                                          AUTOD252          
      STEP=STEP+1                                                       AUTOD253          
      IF(STEP.GT.1.AND.STEP.LT.25) GOTO 1420                            AUTOD254          
      IF(8.*TEST(2).GT.TEST(1).AND..NOT.DOUBLE.NE.0.AND.HTEST.GE.HL)    AUTOD255          
     .GOTO 2250                                                         AUTOD256          
      IF(.NOT.(8.*TEST(2).GT.TEST(1))) GOTO 1370                        AUTOD257          
      DOUBLE=0                                                          AUTOD258          
1370  CONTINUE                                                          AUTOD259          
      IF(STEP.EQ.1) GOTO 1420                                           AUTOD260          
      IF(HALVE.NE.0.AND.HTEST.GE.HL) GOTO 2250                          AUTOD261          
C                                                                       AUTOD262          
C     UPDATE ROUTINE,RETURNS TO 3020 IF STARTING - 1701 OTHERWISE       AUTOD263          
C                                                                       AUTOD264          
1420  IF(1.GT.N) GOTO 1429                                              AUTOD265          
      DO 1428 I=1,N                                                     AUTOD266          
      Z=F(I)-B(5,I)                                                     AUTOD267          
      B(1,I)=B(1,I)+(3.*B(2,I)+(6.*B(3,I)+(10.*B(4,I)+Z*1.041666667)))  AUTOD268          
      B(2,I)=B(2,I)+(4.*B(3,I)+(10.*B(4,I)+Z*.4861111111))              AUTOD269          
      B(3,I)=B(3,I)+(5.*B(4,I)+Z*.1041666667)                           AUTOD270          
      B(4,I)=B(4,I)+Z*.0083333333                                       AUTOD271          
1428  CONTINUE                                                          AUTOD272          
1429  CONTINUE                                                          AUTOD273          
      IF(STEP.LE.24) GOTO 330                                           AUTOD274          
      IF(.NOT.(H.GT.HMAX)) GOTO 1510                                    AUTOD275          
      HMAX=H                                                            AUTOD276          
1510  CONTINUE                                                          AUTOD277          
      IF(.NOT.(H.LT.HMIN)) GOTO 1530                                    AUTOD278          
      HMIN=H                                                            AUTOD279          
1530  CONTINUE                                                          AUTOD280          
      GOTO 590                                                          AUTOD281          
C                                                                       AUTOD282          
C     ROUTINE TESTPHI,FALSE EXIT IS S1300,TRUE EXIT IS 1800             AUTOD283          
C                                                                       AUTOD284          
1550  IF(1.GT.NPL) GOTO 1559                                            AUTOD285          
      DO 1558 I=1,NPL                                                   AUTOD286          
      IF(FIND(I).NE.0) GOTO 1600                                        AUTOD287          
      IF(PL(I)*PLEFT(I).LT.0.) GOTO 1650                                AUTOD288          
1600  CONTINUE                                                          AUTOD289          
1558  CONTINUE                                                          AUTOD290          
1559  CONTINUE                                                          AUTOD291          
      IF(1.GT.NPL) GOTO 1619                                            AUTOD292          
      DO 1618 I=1,NPL                                                   AUTOD293          
      PLEFT(I)=PL(I)                                                    AUTOD294          
1618  CONTINUE                                                          AUTOD295          
1619  CONTINUE                                                          AUTOD296          
      TLEFT=T                                                           AUTOD297          
      KQZSBX=ISTHREE                                                    AUTOD298          
      GOTO 1                                                            AUTOD299          
1650  IF(1.GT.NPL) GOTO 1659                                            AUTOD300          
      DO 1658 I=1,NPL                                                   AUTOD301          
      PRITE(I)=PL(I)                                                    AUTOD302          
1658  CONTINUE                                                          AUTOD303          
1659  CONTINUE                                                          AUTOD304          
      TRITE=T                                                           AUTOD305          
      GOTO 1690                                                         AUTOD306          
C                                                                       AUTOD307          
C     DEPENDENT VARIABLE SEARCH PROCEDURE,ENTERED IF PL(I) CHANGES SIGN AUTOD308          
C                                                                       AUTOD309          
1690  Z=0.0                                                             AUTOD310          
      IF(1.GT.NPL) GOTO 1709                                            AUTOD311          
      DO 1708 I=1,NPL                                                   AUTOD312          
      IF(FIND(I).NE.0) GOTO 1790                                        AUTOD313          
      IF(.NOT.(.NOT.PRITE(I).NE.0)) GOTO 1740                           AUTOD314          
      PLEFT(I)=0.                                                       AUTOD315          
1740  CONTINUE                                                          AUTOD316          
      ZZ=PLEFT(I)/PRITE(I)                                              AUTOD317          
      IF(.NOT.(ZZ.LE.Z)) GOTO 1790                                      AUTOD318          
      Z=ZZ                                                              AUTOD319          
      J=I                                                               AUTOD320          
1790  CONTINUE                                                          AUTOD321          
1708  CONTINUE                                                          AUTOD322          
1709  CONTINUE                                                          AUTOD323          
      HP=(TRITE-TSAVE)-(TRITE-TLEFT)/(1.-Z)                             AUTOD324          
      IF(.NOT.((TSAVE+HP).EQ.T.OR.Z.EQ.0.)) GOTO 1850                   AUTOD325          
      KFLIP=11                                                          AUTOD326          
      FIND(J)=1                                                         AUTOD327          
      GOTO 2030                                                         AUTOD328          
1850  ISTWO=10                                                          AUTOD329          
      KFLIP=13                                                          AUTOD330          
      ISTHREE=17                                                        AUTOD331          
      GOTO 2170                                                         AUTOD332          
C                                                                       AUTOD333          
C     CHECK FOR DOUBLE OF STEP SIZE                                     AUTOD334          
C                                                                       AUTOD335          
1890  IF(.NOT.DOUBLE.NE.0.OR.HBIG.NE.0.AND.ABS(H+H).GT.HBIG) GOTO 980   AUTOD336          
      D1=2.                                                             AUTOD337          
      ISFOUR=5                                                          AUTOD338          
      GOTO 2060                                                         AUTOD339          
C                                                                       AUTOD340          
C     SUBROUTINE CALLS, ASSUMES KFLIP SET PRIOR TO ENTRY                AUTOD341          
C                                                                       AUTOD342          
1940  K=1                                                               AUTOD343          
      IDER=IDER+1                                                       AUTOD344          
      GOTO 2450                                                         AUTOD345          
1970  K=2                                                               AUTOD346          
      IEOS=IEOS+1                                                       AUTOD347          
      GOTO 2450                                                         AUTOD348          
2000  K=J+2                                                             AUTOD349          
      ITL=ITL+1                                                         AUTOD350          
      GOTO 2450                                                         AUTOD351          
2030  K=J+NTL+2                                                         AUTOD352          
      IPL=IPL+1                                                         AUTOD353          
      GOTO 2450                                                         AUTOD354          
C                                                                       AUTOD355          
C     SUBROUTINE TO CHANGE STEP SIZE                                    AUTOD356          
C                                                                       AUTOD357          
2060  H=H*D1                                                            AUTOD358          
      HTEST=ABS(.5*H)                                                   AUTOD359          
      D2=D1*D1                                                          AUTOD360          
      D3=D2*D1                                                          AUTOD361          
      D4=D3*D1                                                          AUTOD362          
      IF(1.GT.N) GOTO 2119                                              AUTOD363          
      DO 2118 I=1,N                                                     AUTOD364          
      B(1,I)=B(1,I)*D1                                                  AUTOD365          
      B(2,I)=B(2,I)*D2                                                  AUTOD366          
      B(3,I)=B(3,I)*D3                                                  AUTOD367          
      B(4,I)=B(4,I)*D4                                                  AUTOD368          
2118  CONTINUE                                                          AUTOD369          
2119  CONTINUE                                                          AUTOD370          
      KQZSBX=ISFOUR                                                     AUTOD371          
      GOTO 1                                                            AUTOD372          
C                                                                       AUTOD373          
C     ROUTINE TO PREDICT INTERMEDIATE VALUES OF Y(I)                    AUTOD374          
C                                                                       AUTOD375          
2170  T=TSAVE+HP                                                        AUTOD376          
      D1=HP/H                                                           AUTOD377          
      D2=D1*D1                                                          AUTOD378          
      D3=D2*D1                                                          AUTOD379          
      D4=D3*D1                                                          AUTOD380          
      IF(1.GT.N) GOTO 2229                                              AUTOD381          
      DO 2228 I=1,N                                                     AUTOD382          
      Y(I)=B(6,I)+HP*(B(8,I)+(D1*B(1,I)+(D2*B(2,I)+(D3*B(3,I)+D4*B(4,I) AUTOD383          
     .))))                                                              AUTOD384          
2228  CONTINUE                                                          AUTOD385          
2229  CONTINUE                                                          AUTOD386          
      KQZSBX=ISTWO                                                      AUTOD387          
      GOTO 1                                                            AUTOD388          
C                                                                       AUTOD389          
C     RESTORE T,Y,F, HALVE STEP SIZE, TRY STEP AGAIN                    AUTOD390          
C                                                                       AUTOD391          
2250  RTEST=.5*H                                                        AUTOD392          
      IF(HTEST.LT.HL) GOTO 1030                                         AUTOD393          
      IF(T.EQ.T+RTEST) GOTO 2420                                        AUTOD394          
      STEP=STEP-1                                                       AUTOD395          
      T=TSTART                                                          AUTOD396          
      IF(1.GT.N) GOTO 2329                                              AUTOD397          
      DO 2328 I=1,N                                                     AUTOD398          
      Y(I)=B(6,I)                                                       AUTOD399          
      B(10,I)=B(7,I)                                                    AUTOD400          
      F(I)=B(8,I)                                                       AUTOD401          
2328  CONTINUE                                                          AUTOD402          
2329  CONTINUE                                                          AUTOD403          
      D1=.5                                                             AUTOD404          
      ISFOUR=18                                                         AUTOD405          
      GOTO 2060                                                         AUTOD406          
C                                                                       AUTOD407          
C     ERROR PRINTOUT AND STOPS                                          AUTOD408          
C                                                                       AUTOD409          
2390  WRITE(61,2400)                                                    AUTOD410          
2400  FORMAT(8H ERROR I,8HN NORDSE,8HT CALLIN,8HG SEQUEN,3HCE.)         AUTOD411          
      CALL Q8QRROR(0,6HERROR.)                                          AUTOD412          
2420  WRITE(61,2430)                                                    AUTOD413          
2430  FORMAT(8H H LESS ,8HTHAN 2**,8H(-36)*T.)                          AUTOD414          
      CALL Q8QRROR(0,6HERROR.)                                          AUTOD415          
2450  RETURN                                                            AUTOD416          
      END                                                               AUTOD417          
