*TEXT                                                                                     
      SUBROUTINE LSMNS                                                                    
C     SUBROUTINE FOR COMPUTING AND LISTING LEAST-SQUARES MEANS AND      DEC15000          
C     STANDARD ERRORS WHEN NAB=0 OR NAB=3 AND BOTH NMEA AND NNEA        DEC15001          
C     EQUAL ZERO.                                                       DEC15002          
C     -------------------------------------------------------           DEC15003          
      DIMENSION ARRAY(2000),SSCPR(630),SSS(630),RHM(0250),TOT(106),TOT2(DEC15005          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100)            DEC15006          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),NEQ(6),IPL(13),NSDEC15007          
     1P(13),NND(13),XP(81),YP(41),      NDC(10),NMI(10),MEN(20),NCL(20),DEC15008          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(40),NCLN(40),LNE(40),NDEN(1DEC15009          
     300),NLIT(40),NMA(30),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(40),DEC15010          
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDEC15011          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),IC(480),DEC15012          
     6EFF1(50),EFF2(50),NOS(200),X(106),NMAC(40)                        DEC15013          
      DIMENSION LSKF(20),LBEG(20),IREJ(20)                              M0215013          
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADEC15014          
     1B4,LITY,TRED,YM,IM,MS,NEQ,IPL,NSP,NND,XP,YP                       DEC15015          
      COMMON /CMBLK2/NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NCLN,NEN,LNE,NBEGDEC15016          
     1,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQC,NREGP,LGTX,JBEG,NDDEC15017          
     2ECX,LITR,NEGY,LNY,LHY,KBEG,NDECY,XM,IC,EFF1,EFF2,NOS,X,NMAC       DEC15018          
      COMMON /CMBLK3/IJOB,NAB,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F,NPR,IRAN,MDEC15019          
     1POP,LIOP,IAD,REP,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,NOM,NON,ML,MLB,NSDEC15020          
     2,NOT,DF,KPUT,SDF,EDF,WK,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,RR       DEC15021          
     3,IN,NSKF,LSKF,LBEG,IREJ                                           M0215021          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DEC15022          
     1L,L7                                                              DEC15023          
      DATA (MU=6HMU    )                                                M0115024          
      WRITE (6,1001) IJOB                                               DEC15025          
 1001 FORMAT (1H0,7X,77HLISTING OF CONSTANTS, LEAST-SQUARES MEANS AND STDEC15026          
     1ANDARD ERRORS FOR PROBLEM NO.,I3)                                 DEC15027          
      WRITE (6,1002)                                                    DEC15028          
 1002 FORMAT (1H0,10H RHM   ROW,30X,3HNO.,7X,8HCONSTANT,6X,13HLEAST-SQUADEC15029          
     1RES,7X,8HSTANDARD)                                                DEC15030          
      WRITE (6,1003)                                                    DEC15031          
 1003 FORMAT (1H ,33H NAME  CODE  INDEPENDENT VARIABLE,7X,4HOBS.,6X,8HESDEC15032          
     1TIMATE,10X,4HMEAN,13X,5HERROR)                                    DEC15033          
      I=0                                                               DEC15034          
    1 I=I+1                                                             DEC15035          
      NCT=1                                                             DEC15036          
      WRITE (6,1004)                                                    DEC15037          
 1004 FORMAT (1H )                                                      DEC15038          
      IF (NCPR.EQ.1) GO TO 2                                            DEC15039          
      J=I                                                               DEC15040          
      GO TO 3                                                           DEC15041          
    2 J=NRHM*(I-1)-I*(I-3)/2                                            DEC15042          
    3 WK=(SSCPR(J)-TRED(I))/EDF                                         DEC15043          
      IF (NAB.EQ.3) WK=WK/(RR+1.0)                                      DEC15044          
      IF (WK.LT.0.) GO TO 151                                           DEC15045          
      REP=SQRT(WK*ARRAY(1))                                             DEC15046          
      J=MATX+(I-1)*NLHM+1                                               DEC15047          
      SDF=ARRAY(J)+YM(I)                                                DEC15048          
      WRITE (6,1005) LITY(I),MU,NCDS,SDF,SDF,REP                        DEC15049          
 1005 FORMAT (1H ,A6,3X,1H1,A8,20X,I5,3F17.8)                           DEC15050          
      L=1                                                               DEC15051          
      L2=1                                                              DEC15052          
      K=ML+1                                                            DEC15053          
      IF (NOM.EQ.0) GO TO 4                                             DEC15054          
      DO 5 J=1,NOM                                                      DEC15055          
      K2=NCL(J)                                                         DEC15056          
      DO 5 K1=1,K2                                                      DEC15057          
      IF (K1.EQ.K2) GO TO 7                                             DEC15058          
      K3=IM(J)+K1                                                       DEC15059          
      K4=NLHM*(K3-1)-K3*(K3-3)/2                                        DEC15060          
      REP=SQRT(WK*(ARRAY(1)+ARRAY(K4)+2.0*ARRAY(K3)))                   DEC15061          
      K4=MATX+(I-1)*NLHM+K3                                             DEC15062          
      AC=ARRAY(K4)                                                      DEC15063          
      ALS=AC+SDF                                                        DEC15064          
      L2=L2+1                                                           DEC15065          
      L1=L2                                                             DEC15066          
      GO TO 8                                                           DEC15067          
    7 K3=IM(J)+1                                                        DEC15068          
      K4=MATX+(I-1)*NLHM+K3                                             DEC15069          
      AC=0.                                                             DEC15070          
      US=0.                                                             DEC15071          
      I5=K2-1                                                           DEC15072          
      DO 9 M=1,I5                                                       DEC15073          
      AC=AC-ARRAY(K4)                                                   DEC15074          
      US=US-ARRAY(K3)                                                   DEC15075          
      K4=K4+1                                                           DEC15076          
    9 K3=K3+1                                                           DEC15077          
      K5=IM(J)+K2-1                                                     DEC15078          
      SS=0.                                                             DEC15079          
      K3=IM(J)+1                                                        DEC15080          
      DO 81 M=K3,K5                                                     DEC15081          
      K4=NLHM*(M-1)-M*(M-3)/2                                           DEC15082          
      DO 10 N=M,K5                                                      DEC15083          
      IF (M.EQ.N) GO TO 80                                              DEC15084          
      K6=K4+N-M                                                         DEC15085          
      SS=SS+2.0*ARRAY(K6)                                               DEC15086          
      GO TO 10                                                          DEC15087          
   80 SS=SS+ARRAY(K4)                                                   DEC15088          
   10 CONTINUE                                                          DEC15089          
   81 CONTINUE                                                          DEC15090          
      REP=SQRT(WK*(ARRAY(1)+SS+2.0*US))                                 DEC15091          
      ALS=AC+SDF                                                        DEC15092          
      L1=0                                                              DEC15093          
    8 WRITE (6,1006) LITY(I),L1,LIT(J),IDEN(K),NOS(L),AC,ALS,REP        DEC15094          
 1006 FORMAT (1H ,A6,I4,A8,I5,15X,I5,3F17.8)                            DEC15095          
      K=K+1                                                             DEC15096          
    5 L=L+1                                                             DEC15097          
C     -------------------------------------------------------           DEC15098          
C     CONSTRUCTION OF X AND TOT2 VECTORS FOR NESTED MAIN EFFECTS        DEC15099          
C     -------------------------------------------------------           DEC15100          
    4 K=MLB+1                                                           DEC15101          
      IF (NON.EQ.0) GO TO 6                                             DEC15102          
      DO 11 J=1,NON                                                     DEC15103          
      K4=NMA(J)-1                                                       DEC15104          
      NSUM=1                                                            DEC15105          
      IF (K4.EQ.0) GO TO 13                                             DEC15106          
      DO 12 K3=1,K4                                                     DEC15107          
   12 NSUM=NSUM+NCL(K3)-1                                               DEC15108          
   13 K2=NCLN(J)                                                        DEC15109          
      DO 11 K1=1,K2                                                     DEC15110          
      DO 14 K3=1,NLHM                                                   DEC15111          
      TOT2(K3)=0.                                                       DEC15112          
   14 X(K3)=0.                                                          DEC15113          
      X(1)=1.                                                           DEC15114          
      I2=NOM+J                                                          DEC15115          
      K3=IM(I2)+K1                                                      DEC15116          
      IF (NMAC(J).NE.NCL(K4+1)) GO TO 15                                DEC15117          
      I3=NCL(K4+1)-1                                                    DEC15118          
      DO 16 K5=1,I3                                                     DEC15119          
      I4=NSUM+K5                                                        DEC15120          
   16 X(I4)=-1.0                                                        DEC15121          
      IF (K1.NE.K2) GO TO 17                                            DEC15122          
   20 I5=K2-1                                                           DEC15123          
      DO 18 K5=1,I5                                                     DEC15124          
      I4=IM(I2)+K5                                                      DEC15125          
      X(I4)=-1.0                                                        DEC15126          
   18 TOT2(I4)=-1.0                                                     DEC15127          
      L1=0                                                              DEC15128          
      GO TO 19                                                          DEC15129          
   15 I4=NSUM+NMAC(J)                                                   DEC15130          
      X(I4)=1.                                                          DEC15131          
      IF (K1.NE.K2) GO TO 17                                            DEC15132          
      GO TO 20                                                          DEC15133          
   17 TOT2(K3)=1.                                                       DEC15134          
      L2=L2+1                                                           DEC15135          
      L1=L2                                                             DEC15136          
      X(K3)=1.                                                          DEC15137          
   19 YT=YM(I)                                                          DEC15138          
      NR=MATX+(I-1)*NLHM+1                                              DEC15139          
C     ------------------------------------------------------            DEC15140          
C     CALL SUBROUTINE WHICH COMPUTES CONSTANT, LS MEAN AND SE, AND LISTSDEC15141          
C     ------------------------------------------------------            DEC15142          
      CALL CANDSE (AC,ALS,REP,TOT2,X,ARRAY,NR,NLHM,WK,YT,I,SSS,NCT)     DEC15143          
      WRITE (6,1006) LITY(I),L1,NLIT(J),NDEN(K),NOS(L),AC,ALS,REP       DEC15144          
      K=K+1                                                             DEC15145          
   11 L=L+1                                                             DEC15146          
C     ------------------------------------------------------            DEC15147          
C     CONSTRUCTION OF X AND TOT2 VECTORS FOR TWO-WAY SUBCLASSES         DEC15148          
C     ------------------------------------------------------            DEC15149          
    6 NOT=NOM+NON+1                                                     DEC15150          
      K8=IM(NOT)                                                        DEC15151          
      K6=0                                                              DEC15152          
      IF (N2F.EQ.0) GO TO 21                                            DEC15153          
      DO 22 J=1,N2F                                                     DEC15154          
      MJ1=0                                                             DEC15155          
      MJ2=0                                                             DEC15156          
      MJ3=0                                                             DEC15157          
      MJ4=0                                                             DEC15158          
C     ------------------------------------------------------            DEC15159          
C     SETS X AND TOT2 ARRAYS TO ZERO AND SETS X(1) TO 1 FOR MU          DEC15160          
C     ------------------------------------------------------            DEC15161          
      DO 23 K1=1,NLHM                                                   DEC15162          
      TOT2(K1)=0.                                                       DEC15163          
   23 X(K1)=0.                                                          DEC15164          
      X(1)=1.                                                           DEC15165          
      K6=K6+NMC(J)                                                      DEC15166          
      NSUM=0                                                            DEC15167          
      K4=INT2(J)-1                                                      DEC15168          
      MSUM=0                                                            DEC15169          
      K5=INT1(J)-1                                                      DEC15170          
      IF (INT1(J).GT.NOM) GO TO 24                                      DEC15171          
      MJ1=INT1(J)                                                       DEC15172          
      IF (K5.EQ.0) GO TO 25                                             DEC15173          
      DO 26 K3=1,K5                                                     DEC15174          
   26 NSUM=NSUM+NCL(K3)                                                 DEC15175          
   25 K2=INT1(J)                                                        DEC15176          
      I1=IM(K2)+1                                                       DEC15177          
      I2=IM(K2+1)+1                                                     DEC15178          
      ID1=LIT(K2)                                                       DEC15179          
      GO TO 27                                                          DEC15180          
C     ------------------------------------------------------            DEC15181          
C     SETS UP +1 AND -1 FOR MAJOR CLASS WHEN FIRST MAIN EFFECT IS       DEC15182          
C     NESTED                                                            DEC15183          
C     ------------------------------------------------------            DEC15184          
   24 K2=NOM+1                                                          DEC15185          
      IF (K2.GT.K5) GO TO 28                                            DEC15186          
      DO 29 K3=K2,K5                                                    DEC15187          
      K=K3-NOM                                                          DEC15188          
   29 NSUM=NSUM+NCLN(K)                                                 DEC15189          
   28 K2=INT1(J)-NOM                                                    DEC15190          
      K3=INT1(J)                                                        DEC15191          
      I1=IM(K3)+1                                                       DEC15192          
      I2=IM(K3+1)+1                                                     DEC15193          
      ID1=NLIT(K2)                                                      DEC15194          
      K=NMA(K2)                                                         DEC15195          
      MJ2=NMA(K2)                                                       DEC15196          
      NI=NMAC(K2)                                                       DEC15197          
      IF (NMAC(K2).EQ.NCL(K)) GO TO 60                                  DEC15198          
      K7=IM(K)+NMAC(K2)                                                 DEC15199          
      X(K7)=1.                                                          DEC15200          
      GO TO 27                                                          DEC15201          
   60 K1=NCL(K)-1                                                       DEC15202          
      DO 61 K3=1,K1                                                     DEC15203          
      K2=IM(K)+K3                                                       DEC15204          
   61 X(K2)=-1.                                                         DEC15205          
   27 IF (INT2(J).GT.NOM) GO TO 30                                      DEC15206          
      MJ3=INT2(J)                                                       DEC15207          
      IF (K4.EQ.0) GO TO 31                                             DEC15208          
      DO 32 K3=1,K4                                                     DEC15209          
   32 MSUM=MSUM+NCL(K3)                                                 DEC15210          
   31 K2=INT2(J)                                                        DEC15211          
      I3=IM(K2)+1                                                       DEC15212          
      I4=IM(K2+1)+1                                                     DEC15213          
      ID2=LIT(K2)                                                       DEC15214          
      GO TO 33                                                          DEC15215          
C     ------------------------------------------------------            DEC15216          
C     SETS UP +1 AND -1 FOR MAJOR CLASS WHEN SECOND MAIN EFFECT IS      DEC15217          
C     NESTED                                                            DEC15218          
C     ------------------------------------------------------            DEC15219          
   30 K2=NOM+1                                                          DEC15220          
      IF (K2.GT.K4) GO TO 34                                            DEC15221          
      DO 35 K3=K2,K4                                                    DEC15222          
      K=K3-NOM                                                          DEC15223          
   35 MSUM=MSUM+NCLN(K)                                                 DEC15224          
   34 K2=INT2(J)-NOM                                                    DEC15225          
      K3=INT2(J)                                                        DEC15226          
      I3=IM(K3)+1                                                       DEC15227          
      I4=IM(K3+1)+1                                                     DEC15228          
      ID2=NLIT(K2)                                                      DEC15229          
      K=NMA(K2)                                                         DEC15230          
      MJ4=NMA(K2)                                                       DEC15231          
      NJ=NMAC(K2)                                                       DEC15232          
      IF (NMAC(K2).EQ.NCL(K)) GO TO 62                                  DEC15233          
      K7=IM(K)+NMAC(K2)                                                 DEC15234          
      X(K7)=1.                                                          DEC15235          
      GO TO 111                                                         DEC15236          
   62 K1=NCL(K)-1                                                       DEC15237          
      DO 63 K3=1,K1                                                     DEC15238          
      K2=IM(K)+K3                                                       DEC15239          
   63 X(K2)=-1.                                                         DEC15240          
C     ------------------------------------------------------            DEC15241          
C     SETS UP X MATRIX FOR MAIN EFFECTS AND BOTH X AND TOT2 ARRAYS FOR  DEC15242          
C     INTERACTION CONSTANTS                                             DEC15243          
C     ------------------------------------------------------            DEC15244          
  111 IF (MJ2.NE.MJ4.AND.MJ1.EQ.MJ3) GO TO 100                          DEC15245          
      GO TO 33                                                          DEC15246          
  100 ME1=MJ2                                                           DEC15247          
      ME2=MJ4                                                           DEC15248          
      CALL INTACT (ME1,ME2,NI,NJ,N2F,NOT,INT1,INT2,X,NMC,MSCL,NCL,EFF1,EDEC15249          
     1FF2,IM,NCLN,NOM)                                                  DEC15250          
   33 DO 36 K=I1,I2                                                     DEC15251          
      J2=K-I1+1                                                         DEC15252          
C     ------------------------------------------------------            DEC15253          
C     SETS UP X ARRAY FOR FIRST MAIN EFFECT                             DEC15254          
C     ------------------------------------------------------            DEC15255          
      L3=I2-1                                                           DEC15256          
      IF (K.EQ.I2) GO TO 42                                             DEC15257          
      DO 64 K1=I1,L3                                                    DEC15258          
   64 X(K1)=0.                                                          DEC15259          
      X(K)=1.                                                           DEC15260          
      GO TO 41                                                          DEC15261          
   42 DO 46 J4=I1,L3                                                    DEC15262          
   46 X(J4)=-1.                                                         DEC15263          
C     ------------------------------------------------------            DEC15264          
C     GETS IDENTIFICATION OF CLASS FOR FIRST MAIN EFFECT                DEC15265          
C     ------------------------------------------------------            DEC15266          
   41 K2=NSUM+J2                                                        DEC15267          
      IF (INT1(J).GT.NOM) GO TO 37                                      DEC15268          
      ID3=IDEN(K2)                                                      DEC15269          
      GO TO 38                                                          DEC15270          
   37 ID3=NDEN(K2)                                                      DEC15271          
   38 DO 36 J1=I3,I4                                                    DEC15272          
      J3=J1-I3+1                                                        DEC15273          
C     ------------------------------------------------------            DEC15274          
C     CHECKS FOR MISSING SUBCLASS                                       DEC15275          
C     ------------------------------------------------------            DEC15276          
      IF (NMC(J).EQ.0) GO TO 45                                         DEC15277          
      K7=J2*100+J3                                                      DEC15278          
      K1=K6-NMC(J)                                                      DEC15279          
   43 K1=K1+1                                                           DEC15280          
      IF (K1.GT.K6) GO TO 45                                            DEC15281          
      IF (K7-MSCL(K1)) 43,44,43                                         DEC15282          
C     ------------------------------------------------------            DEC15283          
C     SETS UP X ARRAY FOR SECOND MAIN EFFECT                            DEC15284          
C     ------------------------------------------------------            DEC15285          
   45 I5=I4-1                                                           DEC15286          
      IF (J1.EQ.I4) GO TO 47                                            DEC15287          
      DO 40 K1=I3,I5                                                    DEC15288          
   40 X(K1)=0.                                                          DEC15289          
      X(J1)=1.                                                          DEC15290          
      GO TO 48                                                          DEC15291          
   47 DO 49 J4=I3,I5                                                    DEC15292          
   49 X(J4)=-1.                                                         DEC15293          
C     ------------------------------------------------------            DEC15294          
C     GETS IDENTIFICATION OF CLASS FOR SECOND MAIN EFFECT               DEC15295          
C     ------------------------------------------------------            DEC15296          
   48 K2=MSUM+J3                                                        DEC15297          
      IF (INT2(J).GT.NOM) GO TO 39                                      DEC15298          
      ID4=IDEN(K2)                                                      DEC15299          
      GO TO 110                                                         DEC15300          
   39 ID4=NDEN(K2)                                                      DEC15301          
C     ------------------------------------------------------            DEC15302          
C     SETS UP X AND TOT2 ARRAYS FOR INTERACTION EFFECT                  DEC15303          
C     ------------------------------------------------------            DEC15304          
  110 IF (MJ1.NE.0.AND.MJ3.EQ.0) GO TO 101                              DEC15305          
      GO TO 102                                                         DEC15306          
  101 ME1=MJ1                                                           DEC15307          
      ME2=MJ4                                                           DEC15308          
      NI=J2                                                             DEC15309          
      CALL INTACT (ME1,ME2,NI,NJ,N2F,NOT,INT1,INT2,X,NMC,MSCL,NCL,EFF1,EDEC15310          
     1FF2,IM,NCLN,NOM)                                                  DEC15311          
  102 IF (MJ3.NE.0.AND.MJ1.EQ.0) GO TO 103                              DEC15312          
      GO TO 104                                                         DEC15313          
  103 ME1=MJ2                                                           DEC15314          
      ME2=MJ3                                                           DEC15315          
      NJ=J3                                                             DEC15316          
      CALL INTACT (ME1,ME2,NI,NJ,N2F,NOT,INT1,INT2,X,NMC,MSCL,NCL,EFF1,EDEC15317          
     1FF2,IM,NCLN,NOM)                                                  DEC15318          
  104 IF (MJ2.NE.MJ4.AND.MJ1.EQ.MJ3) GO TO 105                          DEC15319          
      GO TO 74                                                          DEC15320          
  105 ME1=INT1(J)                                                       DEC15321          
      ME2=MJ4                                                           DEC15322          
      NI=J2                                                             DEC15323          
      K2=INT2(J)-NOM                                                    DEC15324          
      NJ=NMAC(K2)                                                       DEC15325          
      CALL INTACT (ME1,ME2,NI,NJ,N2F,NOT,INT1,INT2,X,NMC,MSCL,NCL,EFF1,EDEC15326          
     1FF2,IM,NCLN,NOM)                                                  DEC15327          
      ME1=MJ2                                                           DEC15328          
      ME2=INT2(J)                                                       DEC15329          
      K2=INT1(J)-NOM                                                    DEC15330          
      NI=NMAC(K2)                                                       DEC15331          
      NJ=J3                                                             DEC15332          
      CALL INTACT (ME1,ME2,NI,NJ,N2F,NOT,INT1,INT2,X,NMC,MSCL,NCL,EFF1,EDEC15333          
     1FF2,IM,NCLN,NOM)                                                  DEC15334          
   74 L3=I2-I1                                                          DEC15335          
      I5=I4-I3                                                          DEC15336          
      N=NOT+J-1                                                         DEC15337          
      K2=IM(N)+1                                                        DEC15338          
      N=L3*I5-NMC(J)                                                    DEC15339          
      M=K2+N-1                                                          DEC15340          
      DO 59 J4=K2,M                                                     DEC15341          
      TOT2(J4)=0.                                                       DEC15342          
   59 X(J4)=0.                                                          DEC15343          
      DO 50 J4=1,L3                                                     DEC15344          
   50 EFF1(J4)=0.                                                       DEC15345          
      DO 51 J4=1,I5                                                     DEC15346          
   51 EFF2(J4)=0.                                                       DEC15347          
      IF  (K.EQ.I2) GO TO 52                                            DEC15348          
      EFF1(J2)=1.                                                       DEC15349          
      GO TO 53                                                          DEC15350          
   52 DO 54 J4=1,L3                                                     DEC15351          
   54 EFF1(J4)=-1.                                                      DEC15352          
   53 IF (J1.EQ.I4) GO TO 55                                            DEC15353          
      EFF2(J3)=1.                                                       DEC15354          
      GO TO 56                                                          DEC15355          
   55 DO 76 J4=1,I5                                                     DEC15356          
   76 EFF2(J4)=-1.                                                      DEC15357          
   56 K5=K8+1                                                           DEC15358          
      DO 82 K1=1,L3                                                     DEC15359          
      DO 58 K2=1,I5                                                     DEC15360          
      K7=K1*100+K2                                                      DEC15361          
      IF (NMC(J).EQ.0) GO TO 71                                         DEC15362          
      J4=K6-NMC(J)                                                      DEC15363          
   70 J4=J4+1                                                           DEC15364          
      IF (J4.GT.K6) GO TO 71                                            DEC15365          
      IF (K7-MSCL(J4)) 70,58,70                                         DEC15366          
   71 X(K5)=EFF1(K1)*EFF2(K2)                                           DEC15367          
      TOT2(K5)=X(K5)                                                    DEC15368          
      K5=K5+1                                                           DEC15369          
   58 CONTINUE                                                          DEC15370          
   82 CONTINUE                                                          DEC15371          
      IF  (K.EQ.I2.OR.J1.EQ.I4) GO TO 72                                DEC15372          
      L2=L2+1                                                           DEC15373          
      L1=L2                                                             DEC15374          
      GO TO 73                                                          DEC15375          
   72 L1=0                                                              DEC15376          
   73 NR=MATX+(I-1)*NLHM+1                                              DEC15377          
      YT=YM(I)                                                          DEC15378          
      CALL CANDSE (AC,ALS,REP,TOT2,X,ARRAY,NR,NLHM,WK,YT,I,SSS,NCT)     DEC15379          
      WRITE (6,1007) LITY(I),L1,ID1,ID2,ID3,ID4,NOS(L),AC,ALS,REP       DEC15380          
 1007 FORMAT (1H ,A6,I4,A8,3H X ,A6,2I5,I6,3F17.8)                      DEC15381          
   44 L=L+1                                                             DEC15382          
   36 CONTINUE                                                          DEC15383          
      K8=K8+L3*I5-NMC(J)                                                DEC15384          
   22 CONTINUE                                                          DEC15385          
C     ------------------------------------------------------            DEC15386          
C     LISTING OF PARTIAL REGRESSIONS AND STANDARD ERRORS FOR            DEC15387          
C     CONTINUOUS INDEPENDENT VARIABLES                                  DEC15388          
C     ------------------------------------------------------            DEC15389          
   21 IF (NPR.EQ.0) GO TO 75                                            DEC15390          
      L1=L2+1                                                           DEC15391          
      DO 57 J=IE,NLHM                                                   DEC15392          
      L=NLHM*(J-1)-J*(J-3)/2                                            DEC15393          
      K=MATX+(I-1)*NLHM+J                                               DEC15394          
      REP=SQRT(ARRAY(L)*WK)                                             DEC15395          
      WRITE (6,1008) LITY(I),L1,LAB1(J),LAB2(J),LAB3(J),LAB4(J),ARRAY(K)DEC15396          
     1,REP                                                              DEC15397          
 1008 FORMAT (1H ,A6,I4,4A7,5X,F17.8,17X,F17.8)                         DEC15398          
   57 L1=L1+1                                                           DEC15399          
   75 IF (I.NE.NRHM) GO TO 1                                            DEC15400          
      RETURN                                                            DEC15401          
  151 KPUT=1                                                            DEC15402          
      RETURN                                                            DEC15403          
      END                                                               DEC15404          
*ENDTEXT                                                                                  
