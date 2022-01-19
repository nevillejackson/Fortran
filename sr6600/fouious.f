      SUBROUTINE FOUIOUS(A,N2P,N3,N5,COSIN,IORIG)                       FOURI002          
      INTEGER RP                                                        FOURI003          
      DIMENSION A(1),COSIN(1),XR(5),XI(5),IRP(20)                       FOURI004          
C                                                                       FOURI005          
C     REVISED AUGUST, 1969                                              FOURI006          
C AUTHOR - D.MC.LEAN,DIVISION OF RADIOPHYSICS,SYDNEY                    FOURI007          
C                                                                       FOURI008          
      INV=0                                                             FOURI009          
      GOTO 60                                                           FOURI010          
      ENTRY FOURINV                                                     FOURI011          
      INV=1                                                             FOURI012          
60    CONTINUE                                                          FOURI013          
      NF=1                                                              FOURI014          
      IREAL=0                                                           FOURI015          
      N2=N2P                                                            FOURI016          
100   CONTINUE                                                          FOURI017          
      MAX2=30 $ MAX3=20 $ MAX5=15                                       FOURI018          
      IF(N2.LT.2.OR.N2.GT.MAX2) GOTO 170                                FOURI019          
      IF(N3.GT.MAX3.OR.N3.LT.0) GOTO 170                                FOURI020          
      IF(.NOT.(N5.GT.MAX5.OR.N5.LT.0)) GOTO 200                         FOURI021          
170   WRITE(61,180) N2,N3,N5,IORIG                                      FOURI022          
180   FORMAT(8H1ERROR I,8HN CALL T,8HO FOURIO,8HUS(FUNCT,4HION,,I3,1H,, FOURI023          
     .I3,1H,,I3,7H,COSIN,,I5,1H))                                       FOURI024          
      STOP                                                              FOURI025          
200   CONTINUE                                                          FOURI026          
      PI=3.1415926536                                                   FOURI027          
      N=2**N2*3**N3*5**N5                                               FOURI028          
      NQ1=N/4                                                           FOURI029          
      NQ2=NQ1+NQ1                                                       FOURI030          
      NQ3=NQ2+NQ1                                                       FOURI031          
      NQ4=NQ3+NQ1                                                       FOURI032          
      NT2=N+N                                                           FOURI033          
      X=0                                                               FOURI034          
      DX=PI/FLOAT(NQ2)                                                  FOURI035          
      I=1                                                               FOURI036          
301   IF(I.GT.NQ1) GOTO 309                                             FOURI037          
      COSIN(I)=COS(X)                                                   FOURI038          
      X=X+DX                                                            FOURI039          
      I=I+1                                                             FOURI040          
      GOTO 301                                                          FOURI041          
309   CONTINUE                                                          FOURI042          
      IF(.NOT.(IREAL.NE.0.AND.INV.NE.0)) GOTO 360                       FOURI043          
      IF(IORIG.NE.1) GOTO 910                                           FOURI044          
      GOTO 2170                                                         FOURI045          
360   IF(.NOT.(IORIG.EQ.1.OR.INV.EQ.0)) GOTO 910                        FOURI046          
370   CONTINUE                                                          FOURI047          
      JSWITCH=1                                                         FOURI048          
      N1P=N                                                             FOURI049          
      NP=N1P                                                            FOURI050          
      RP=5 $ ISWITCH=1                                                  FOURI051          
      I=1                                                               FOURI052          
421   IF(I.GT.N5) GOTO 429                                              FOURI053          
      GOTO 1350                                                         FOURI054          
440   CONTINUE                                                          FOURI055          
      I=I+1                                                             FOURI056          
      GOTO 421                                                          FOURI057          
429   CONTINUE                                                          FOURI058          
      RP=3 $ ISWITCH=2                                                  FOURI059          
      I=1                                                               FOURI060          
481   IF(I.GT.N3) GOTO 489                                              FOURI061          
      GOTO 1350                                                         FOURI062          
500   CONTINUE                                                          FOURI063          
      I=I+1                                                             FOURI064          
      GOTO 481                                                          FOURI065          
489   CONTINUE                                                          FOURI066          
      RP=2 $ ISWITCH=3                                                  FOURI067          
      I=1                                                               FOURI068          
541   IF(I.GT.N2) GOTO 549                                              FOURI069          
      GOTO 1350                                                         FOURI070          
560   CONTINUE                                                          FOURI071          
C-----------------------------------------------------------------------FOURI072          
C                                                                       FOURI073          
C              THIS SECTION SORTS THE RESULTS INTO THEIR CORRECT        FOURI074          
C                              ORDER                                    FOURI075          
C                                                                       FOURI076          
      I=I+1                                                             FOURI077          
      GOTO 541                                                          FOURI078          
549   CONTINUE                                                          FOURI079          
      I=1                                                               FOURI080          
581   IF(I.GT.N5) GOTO 589                                              FOURI081          
      IRP(I)=5                                                          FOURI082          
      I=I+1                                                             FOURI083          
      GOTO 581                                                          FOURI084          
589   CONTINUE                                                          FOURI085          
      M3=N3+N5 $ M5=N5+1                                                FOURI086          
      I=M5                                                              FOURI087          
621   IF(I.GT.M3) GOTO 629                                              FOURI088          
      IRP(I)=3                                                          FOURI089          
      I=I+1                                                             FOURI090          
      GOTO 621                                                          FOURI091          
629   CONTINUE                                                          FOURI092          
      M2=M3+N2 $ M3=M3+1                                                FOURI093          
      I=M3                                                              FOURI094          
661   IF(I.GT.M2) GOTO 669                                              FOURI095          
      IRP(I)=2                                                          FOURI096          
      I=I+1                                                             FOURI097          
      GOTO 661                                                          FOURI098          
669   CONTINUE                                                          FOURI099          
      NM2=N-2                                                           FOURI100          
      I=2                                                               FOURI101          
691   IF(I.GT.NM2) GOTO 699                                             FOURI102          
      J=I                                                               FOURI103          
710   IND=0                                                             FOURI104          
      JAM=J-1                                                           FOURI105          
      II=1                                                              FOURI106          
731   IF(II.GT.M2) GOTO 739                                             FOURI107          
      IXL=JAM/IRP(II)                                                   FOURI108          
      IND=(IND-IXL)*IRP(II)+JAM                                         FOURI109          
      JAM=IXL                                                           FOURI110          
      II=II+1                                                           FOURI111          
      GOTO 731                                                          FOURI112          
739   CONTINUE                                                          FOURI113          
      J=IND+1                                                           FOURI114          
      IF(I-J) 800,860,710                                               FOURI115          
800   TR=A(2*I-1) $ TI=A(2*I) $ A(2*I-1)=A(2*J-1) $ A(2*I)=A(2*J)       FOURI116          
      A(2*J-1)=TR $ A(2*J)=TI                                           FOURI117          
860   CONTINUE                                                          FOURI118          
      I=I+1                                                             FOURI119          
      GOTO 691                                                          FOURI120          
699   CONTINUE                                                          FOURI121          
      IF(IREAL.NE.0.AND..NOT.INV.NE.0) GOTO 2170                        FOURI122          
880   CONTINUE                                                          FOURI123          
C                                                                       FOURI124          
C-----------------------------------------------------------------------FOURI125          
C                                                                       FOURI126          
      IF(INV.NE.0.OR.IORIG.EQ.1) GOTO 1260                              FOURI127          
C                                                                       FOURI128          
C-----------------------------------------------------------------------FOURI129          
C                                                                       FOURI130          
C              THIS SECTION TAKES INTO ACCOUNT THE EFFECTS OF A         FOURI131          
C                SHIFT OF ORIGIN                                        FOURI132          
C                                                                       FOURI133          
910   CONTINUE                                                          FOURI134          
      JSWITCH=2                                                         FOURI135          
      IEXP=0                                                            FOURI136          
      IEXPDEL=1-IORIG                                                   FOURI137          
      NOP=N                                                             FOURI138          
      IF(.NOT.(IREAL.NE.0)) GOTO 1090                                   FOURI139          
      JREAL=IEXPDEL/2                                                   FOURI140          
      IEXDELD=IEXPDEL-2*JREAL                                           FOURI141          
      IEXPDEL=JREAL                                                     FOURI142          
      NOP=N+1                                                           FOURI143          
      IF(.NOT.(IEXDELD.NE.0)) GOTO 1080                                 FOURI144          
      JREAL=1                                                           FOURI145          
      XPHR=COS(DX*0.5) $ XPHI=SIN(DX*0.5)                               FOURI146          
      IF(.NOT.(INV.NE.0.AND.IEXDELD.GT.0.OR..NOT.INV.NE.0.AND.IEXDELD   FOURI147          
     ..LT.0)) GOTO 1060                                                 FOURI148          
      XPHI=-XPHI                                                        FOURI149          
1060  CONTINUE                                                          FOURI150          
      GOTO 1100                                                         FOURI151          
1080  CONTINUE                                                          FOURI152          
1090  JREAL=0                                                           FOURI153          
1100  CONTINUE                                                          FOURI154          
      I=1                                                               FOURI155          
1111  IF(I.GT.NOP) GOTO 1119                                            FOURI156          
      GOTO 1490                                                         FOURI157          
C     EVALUATE EXPN=EXP(IEXP*(2*PI*I/N))                                FOURI158          
1130  IF(JREAL) 1140,1190,1180                                          FOURI159          
1140  TR=XPR $ XPR=XPR*XPHR-XPI*XPHI $ XPI=XPI*XPHR+TR*XPHI             FOURI160          
      IEXP=IEXP+IEXDELD                                                 FOURI161          
1180  JREAL=-JREAL                                                      FOURI162          
1190  TR=A(2*I-1) $ A(2*I-1)=A(2*I-1)*XPR-A(2*I)*XPI                    FOURI163          
      A(2*I)=A(2*I)*XPR+TR*XPI                                          FOURI164          
      IEXP=IEXP+IEXPDEL                                                 FOURI165          
      I=I+1                                                             FOURI166          
      GOTO 1111                                                         FOURI167          
1119  CONTINUE                                                          FOURI168          
      IF(.NOT.(INV.NE.0)) GOTO 1260                                     FOURI169          
      IF(IREAL.NE.0) GOTO 2170                                          FOURI170          
      GOTO 370                                                          FOURI171          
C                                                                       FOURI172          
C-----------------------------------------------------------------------FOURI173          
1260  CONTINUE                                                          FOURI174          
      FACT=1./SQRT(FLOAT(N*NF))                                         FOURI175          
      NOP=N                                                             FOURI176          
      IF(.NOT.(IREAL.NE.0.AND..NOT.INV.NE.0)) GOTO 1300                 FOURI177          
      NOP=1+NOP                                                         FOURI178          
1300  CONTINUE                                                          FOURI179          
      NOP=NOP+NOP                                                       FOURI180          
      I=1                                                               FOURI181          
1321  IF(I.GT.NOP) GOTO 1329                                            FOURI182          
      A(I)=A(I)*FACT                                                    FOURI183          
      I=I+1                                                             FOURI184          
      GOTO 1321                                                         FOURI185          
1329  CONTINUE                                                          FOURI186          
      GOTO 2620                                                         FOURI187          
C-----------------------------------------------------------------------FOURI188          
C           THE REST OF THE PROGRAM  RUNS THRO" THE FUNCTION N2+N3+N5   FOURI189          
C            TIMES.   AFTER THE LAST TIME THE FOURIER TRANSFORM IS      FOURI190          
C                         COMPLETE                                      FOURI191          
1350  N1P=NP $ NP=NP/RP $ NSN1P=N/N1P                                   FOURI192          
      NSRP=N/RP                                                         FOURI193          
      NKPSN1P=0                                                         FOURI194          
      KP=1                                                              FOURI195          
1401  IF(KP.GT.NP) GOTO 1409                                            FOURI196          
      KA1=KP                                                            FOURI197          
      LLP=1                                                             FOURI198          
1421  IF(LLP.GT.NSN1P) GOTO 1429                                        FOURI199          
      IEXPDEL=0                                                         FOURI200          
      IEXP=IEXPDEL                                                      FOURI201          
      IEXP1=0                                                           FOURI202          
      J1P=1                                                             FOURI203          
1451  IF(J1P.GT.RP) GOTO 1459                                           FOURI204          
      XI(J1P)=0.                                                        FOURI205          
      XR(J1P)=XI(J1P)                                                   FOURI206          
      KA=KA1                                                            FOURI207          
      K1P=1                                                             FOURI208          
1481  IF(K1P.GT.RP) GOTO 1489                                           FOURI209          
1490  IF(IEXP-NQ4) 1520,1500,1500                                       FOURI210          
1500  IEXP=IEXP-NQ4                                                     FOURI211          
      GOTO 1490                                                         FOURI212          
1520  IF(IEXP-NQ2) 1530,1810,1540                                       FOURI213          
1530  IF(IEXP-NQ1) 1550,1870,1610                                       FOURI214          
1540  IF(IEXP-NQ3) 1660,1840,1710                                       FOURI215          
C     FIRST QUADRANT                                                    FOURI216          
1550  IF(IEXP) 1760,1780,1560                                           FOURI217          
1560  JCOS=IEXP $ JSIN=NQ1-JCOS                                         FOURI218          
      XPR=COSIN(JCOS+1) $ XPI=COSIN(JSIN+1)                             FOURI219          
      GOTO 1890                                                         FOURI220          
C     SECOND QUADRANT                                                   FOURI221          
1610  JCOS=NQ2-IEXP $ JSIN=NQ1-JCOS                                     FOURI222          
      XPR=-COSIN(JCOS+1) $ XPI=COSIN(JSIN+1)                            FOURI223          
      GOTO 1890                                                         FOURI224          
C     THIRD QUADRANT                                                    FOURI225          
1660  JCOS=IEXP-NQ2 $ JSIN=NQ1-JCOS                                     FOURI226          
      XPR=-COSIN(JCOS+1) $ XPI=-COSIN(JSIN+1)                           FOURI227          
      GOTO 1890                                                         FOURI228          
C     FOURTH QUADRANT                                                   FOURI229          
1710  JCOS=NQ4-IEXP $ JSIN=NQ1-JCOS                                     FOURI230          
      XPR=COSIN(JCOS+1) $ XPI=-COSIN(JSIN+1)                            FOURI231          
      GOTO 1890                                                         FOURI232          
1760  IEXP=IEXP+NQ4                                                     FOURI233          
      GOTO 1490                                                         FOURI234          
1780  XPR=1. $ XPI=0.                                                   FOURI235          
      GOTO 1890                                                         FOURI236          
1810  XPR=-1. $ XPI=0.                                                  FOURI237          
      GOTO 1890                                                         FOURI238          
1840  XPR=0. $ XPI=-1.                                                  FOURI239          
      GOTO 1890                                                         FOURI240          
1870  XPR=0. $ XPI=1.                                                   FOURI241          
1890  IF(.NOT.(INV.NE.0)) GOTO 1900                                     FOURI242          
      XPI=-XPI                                                          FOURI243          
1900  CONTINUE                                                          FOURI244          
      KQZ001=JSWITCH                                                    FOURI245          
      IF(KQZ001.LT.1) KQZ001=1                                          FOURI246          
      IF(KQZ001.GT.3) KQZ001=3                                          FOURI247          
      GOTO(1920,1130,2370),KQZ001                                       FOURI248          
1920  XR(J1P)=XR(J1P)+A(2*KA-1)*XPR-A(2*KA)*XPI                         FOURI249          
      XI(J1P)=XI(J1P)+A(2*KA-1)*XPI+A(2*KA)*XPR                         FOURI250          
      IEXP=IEXP+IEXPDEL $ KA=KA+NP                                      FOURI251          
      K1P=K1P+1                                                         FOURI252          
      GOTO 1481                                                         FOURI253          
1489  CONTINUE                                                          FOURI254          
      IEXP1=IEXP1+NKPSN1P                                               FOURI255          
      IEXP=IEXP1                                                        FOURI256          
      IEXPDEL=IEXPDEL+NSRP                                              FOURI257          
      J1P=J1P+1                                                         FOURI258          
      GOTO 1451                                                         FOURI259          
1459  CONTINUE                                                          FOURI260          
      KA=KA1                                                            FOURI261          
      J1P=1                                                             FOURI262          
2011  IF(J1P.GT.RP) GOTO 2019                                           FOURI263          
      A(2*KA-1)=XR(J1P) $ A(2*KA)=XI(J1P)                               FOURI264          
      KA=KA+NP                                                          FOURI265          
      J1P=J1P+1                                                         FOURI266          
      GOTO 2011                                                         FOURI267          
2019  CONTINUE                                                          FOURI268          
      KA1=KA1+N1P                                                       FOURI269          
      LLP=LLP+1                                                         FOURI270          
      GOTO 1421                                                         FOURI271          
1429  CONTINUE                                                          FOURI272          
      NKPSN1P=NKPSN1P+NSN1P                                             FOURI273          
      KP=KP+1                                                           FOURI274          
      GOTO 1401                                                         FOURI275          
1409  CONTINUE                                                          FOURI276          
      KQZ001=ISWITCH                                                    FOURI277          
      IF(KQZ001.LT.1) KQZ001=1                                          FOURI278          
      IF(KQZ001.GT.3) KQZ001=3                                          FOURI279          
      GOTO(440,500,560),KQZ001                                          FOURI280          
C                                                                       FOURI281          
C-----------------------------------------------------------------------FOURI282          
C                                                                       FOURI283          
C    FOUREALT IS THE ENTRY POINT FOR THE F.T. OF A REAL FUNCT.          FOURI284          
C    STORED IN SUCCESSIVE PAIRS OF MEMORIES                             FOURI285          
C      I.E.   A(J)=R(2*J-1)+I*R(2*J)            I**2=-1                 FOURI286          
C                                                                       FOURI287          
      ENTRY FOUEALT                                                     FOURI288          
      N2=N2P-1                                                          FOURI289          
      IREAL=1                                                           FOURI290          
      INV=0                                                             FOURI291          
      NF=8                                                              FOURI292          
      GOTO 100                                                          FOURI293          
C                                                                       FOURI294          
C-----------------------------------------------------------------------FOURI295          
C                                                                       FOURI296          
CC       THIS SECTION DISENTANGLES THE F.T. OF R FROM THE F.T. OF A     FOURI297          
C     OR VICE VERSA FOR AN INVERSE TRANSFORM                            FOURI298          
C                                                                       FOURI299          
2170  IEXP=0                                                            FOURI300          
      JEXP=0                                                            FOURI301          
      XPHR=COS(DX*0.5) $ XPHI=SIN(DX*0.5)                               FOURI302          
      NP1=N+1                                                           FOURI303          
      IF(.NOT.(INV.NE.0)) GOTO 2250                                     FOURI304          
      XPHI=-XPHI                                                        FOURI305          
      GOTO 2270                                                         FOURI306          
2250  A(2*NP1-1)=A(1) $ A(2*NP1)=A(2)                                   FOURI307          
2270  CONTINUE                                                          FOURI308          
      JSWITCH=3                                                         FOURI309          
      NS2=N/2+1                                                         FOURI310          
      JR=NP1                                                            FOURI311          
      IR=1                                                              FOURI312          
2311  IF(IR.GT.NS2) GOTO 2319                                           FOURI313          
      IF(JEXP) 1490,1490,2330                                           FOURI314          
2330  TR=XPR $ XPR=XPR*XPHR-XPI*XPHI $ XPI=TR*XPHI+XPI*XPHR             FOURI315          
      IEXP=IEXP+1                                                       FOURI316          
C                                                                       FOURI317          
C     JUMP TO 100 RETURNS TO 301 WITH EXPN=EXP((2*PI*I/N)*((IR-1)/2))   FOURI318          
C                                                                       FOURI319          
2370  JEXP=1-JEXP                                                       FOURI320          
      TR=A(2*IR-1)-A(2*JR-1) $ TI=A(2*IR)+A(2*JR)                       FOURI321          
      XR(2)=TR*XPI+TI*XPR $ XI(2)=TI*XPI-TR*XPR                         FOURI322          
      IF(INV.NE.0) GOTO 2460                                            FOURI323          
      XR(2)=-XR(2) $ XI(2)=-XI(2)                                       FOURI324          
2460  CONTINUE                                                          FOURI325          
      XR(1)=A(2*IR-1)+A(2*JR-1) $ XI(1)=A(2*IR)-A(2*JR)                 FOURI326          
      A(2*IR-1)=XR(1)-XR(2) $ A(2*IR)=XI(1)-XI(2)                       FOURI327          
      A(2*JR-1)=XR(1)+XR(2) $ A(2*JR)=-XI(1)-XI(2)                      FOURI328          
      JR=NP1-IR                                                         FOURI329          
      IR=IR+1                                                           FOURI330          
      GOTO 2311                                                         FOURI331          
2319  CONTINUE                                                          FOURI332          
      IF(INV.NE.0) GOTO 370                                             FOURI333          
      GOTO 880                                                          FOURI334          
C                                                                       FOURI335          
C-----------------------------------------------------------------------FOURI336          
C                                                                       FOURI337          
C       FOUREALI   IS THE IN  ENTRY PT. FOR AN INVERSE TRANSFORM        FOURI338          
C        OF A COMPLEX F.T. TO YIELD A REAL FUNCTION                     FOURI339          
C        I.E. A CALL TO FOUREALT FOLLOWED BY A CALL TO FOUREALI         FOURI340          
C    RESTORES THE STATUS QUO ANTE                                       FOURI341          
C                                                                       FOURI342          
      ENTRY FOUEALI                                                     FOURI343          
      INV=1 $ IREAL=1                                                   FOURI344          
      N2=N2P-1                                                          FOURI345          
      NF=2                                                              FOURI346          
      GOTO 100                                                          FOURI347          
2620  RETURN                                                            FOURI348          
      END                                                               FOURI349          
