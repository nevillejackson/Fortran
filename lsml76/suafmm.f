      SUBROUTINE SUAFMM                                                 DK230010          
C     ------------------------------------------------------            DK230020          
C     SUBROUTINE WHICH CORRECTS ARRAYS FOR COMPUTATION OF LS MEANS      DK230030          
C     AND STANDARD ERRORS FOR MIXED MODELS                              DK230040          
C     ------------------------------------------------------            DK230050          
      DIMENSION ARRAY(5000),SSCPR(630),SSS(630),RHM(2450),TOT(106),TOT2(DK230080          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100),NS2(15)    DK230090          
      DIMENSION FAB(5000),FY(630),TOT4(106),SAB(630),FSQ(525),TOT5(106) DK230100          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),MZ(10),IPL(13),NSDK230110          
     1P(13),NND(13),XP(81),YP(41),      NDC(10),NMI(10),MEN(20),NCL(20),DK230120          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(50),NCLN(50),LNE(50),NDEN(1                  
     300),NLIT(50),NMA(50),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(50),                  
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDK230150          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),                          
     6EFF1(50),EFF2(50),NOS(300),X(106),NMAC(50),I309(15),NR1(15),NW(15)                  
     7,R1I(100),R2I(100),WW(200),ICLR(90),IRM(40),LAD(90)               DK230180          
      DIMENSION IC(1000)                                                                  
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADK230190          
     1B4,LITY,TRED,YM,XP,YP,IM,MS,NEG,IPL,NSP,NND,FAB,FY,TOT4,SAB,FSQ,TODK230200          
     2T5,MZ,R1I,R2I,WW,ICLR,IRM,LAD                                     DK230210          
      COMMON /CMBLK2/LIT,LITR,NLIT,XM,EFF1,EFF2,X,NDC,NMI,MEN,NCL,LME,IBDK230220          
     1EG,IDEN,NCLN,NEN,LNE,NBEG,NDEN,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQDK230230          
     2C,NREGP,LGTX,JBEG,NDECX,NEGY,LNY,LHY,KBEG,NDECY,IC,NOS,NMAC       DK230240          
      COMMON /CMBLK3/RR,REP,DF,SDF,EDF,WK,IJOB,NAB,NLHM,NRHM,NMEA,NME,NNDK230250          
     1EA,NNE,N2F,NPR,IRAN,MPOP,LIOP,IAD,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,DK230260          
     2NOM,NON,ML,MLB,NS,NOT,KPUT,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,IN,I30DK230270          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DK230280          
     1L,L7,KA,KB,KC,SOF,NX,KD,MTY,MAN,NRUN,NRN,EDFF,NAME,EDFS,NAM5,IFP,NDK230290          
     2DFA,IFPC,NDFC,NDFAC,NINT,IBET,LBEG,LTHN,NNDC,WTT,SWT1             DK230300          
      INTEGER RGRSN                                                                       
      DATA RGRSN/6HRGRSN /                                              DK230310          
      NAB=0                                                             DK230320          
      KB=0                                                              DK230330          
      GO TO (1,2,3,2,3,3,7),MTY                                         DK230340          
C     ------------------------------------------------------            DK230350          
C     SETS UP B AND C-1 FOR ALL FIXED EFFECTS WHEN MTY=2 OR 4           DK230360          
C     ------------------------------------------------------            DK230370          
    2 L=NLHM+1                                                          DK230380          
      K=MATX+NLHM*NRHM                                                  DK230390          
      K1=L*(L+1)/2+L*NRHM                                               DK230400          
      DO 10 I=1,NRHM                                                    DK230410          
      K2=L+NRHM+1-I                                                     DK230420          
      SUM=TOT5(K2)                                                      DK230430          
      DO 30 J=1,L                                                       DK230440          
      IF (J.EQ.L) GO TO 11                                              DK230450          
      SUM=SUM-TOT5(L+1-J)*ARRAY(K)                                      DK230460          
      ARRAY(K1)=ARRAY(K)                                                DK230470          
      K=K-1                                                             DK230480          
      GO TO 30                                                          DK230490          
   11 ARRAY(K1)=SUM/TOT5(1)                                             DK230500          
   30 K1=K1-1                                                           DK230510          
   10 CONTINUE                                                          DK230520          
      L=MATX+NLHM+1                                                     DK230530          
      J=4999                                                            DK230540          
      DO 12 I=1,L                                                       DK230550          
      IF (I.GT.MATX) GO TO 13                                           DK230560          
      ARRAY(K1)=FAB(K)                                                  DK230570          
      K=K-1                                                             DK230580          
      GO TO 12                                                          DK230590          
   13 ARRAY(K1)=FAB(J)                                                  DK230600          
      J=J-1                                                             DK230610          
   12 K1=K1-1                                                           DK230620          
C     ------------------------------------------------------            DK230630          
C     CORRECTS IM ARRAY, NLHM, NS, MATX AND IE WHEN MTY=2 OR 4          DK230640          
C     ------------------------------------------------------            DK230650          
      K=NS+1                                                            DK230660          
      L=NS                                                              DK230670          
      K1=NS+1                                                           DK230680          
      DO 14 I=1,K1                                                      DK230690          
      IF (I.EQ.K1) GO TO 15                                             DK230700          
      IM(K)=IM(L)+1                                                     DK230710          
      L=L-1                                                             DK230720          
      GO TO 14                                                          DK230730          
   15 IM(K)=1                                                           DK230740          
   14 K=K-1                                                             DK230750          
      IF (IE.GT.NLHM) GO TO 31                                          DK230760          
      K=NLHM+1                                                          DK230770          
      DO 32 I=IE,NLHM                                                   DK230780          
      LAB1(K)=RGRSN                                                     DK230790          
      LAB2(K)=LAB2(K-1)                                                 DK230800          
      LAB3(K)=LAB3(K-1)                                                 DK230810          
      LAB4(K)=LAB4(K-1)                                                 DK230820          
   32 K=K-1                                                             DK230830          
   31 NLHM=NLHM+1                                                       DK230840          
      NS=NS+1                                                           DK230850          
      KB=1                                                              DK230860          
      MATX=MATX+NLHM                                                    DK230870          
      IE=IE+1                                                           DK230880          
      GO TO 1                                                           DK230890          
C     ------------------------------------------------------            DK230900          
C     SETS UP B AND C-1 ARRAYS FOR ALL FIXED EFFECTS WHEN MTY=3, 5 OR 6 DK230910          
C     ------------------------------------------------------            DK230920          
    3 K2=NDFA+1                                                         DK230930          
      L=NLHM+K2                                                         DK230940          
      K4=5000-NDFA*NRHM                                                 DK230950          
      J=1                                                               DK230960          
      K=NDFA*NRHM+K4-1                                                  DK230970          
      DO 20 I=K4,K                                                      DK230980          
      RHM(J)=ARRAY(I)                                                   DK230990          
   20 J=J+1                                                             DK231000          
      K=MATX+NLHM*NRHM                                                  DK231010          
      K1=L*(L+1)/2+L*NRHM                                               DK231020          
      K4=NDFA*NRHM                                                      DK231030          
      DO 35 I=1,NRHM                                                    DK231040          
      K3=L+NRHM+1-I                                                     DK231050          
      SUM=TOT5(K3)                                                      DK231060          
      DO 16 J=1,L                                                       DK231070          
      I2=L+1-J                                                          DK231080          
      IF (J.GT.NLHM) GO TO 17                                           DK231090          
      SUM=SUM-TOT5(I2)*ARRAY(K)                                         DK231100          
      ARRAY(K1)=ARRAY(K)                                                DK231110          
      K=K-1                                                             DK231120          
      GO TO 16                                                          DK231130          
   17 IF (J.EQ.L) GO TO 18                                              DK231140          
      SUM=SUM-TOT5(I2)*RHM(K4)                                          DK231150          
      ARRAY(K1)=RHM(K4)                                                 DK231160          
      K4=K4-1                                                           DK231170          
      GO TO 16                                                          DK231180          
   18 ARRAY(K1)=SUM/TOT5(1)                                             DK231190          
   16 K1=K1-1                                                           DK231200          
   35 CONTINUE                                                          DK231210          
      L=L*(L+1)/2                                                       DK231220          
      J=4999                                                            DK231230          
      DO 19 I=1,L                                                       DK231240          
      IF (I.GT.MATX) GO TO 21                                           DK231250          
      ARRAY(K1)=FAB(K)                                                  DK231260          
      K=K-1                                                             DK231270          
      GO TO 19                                                          DK231280          
   21 ARRAY(K1)=FAB(J)                                                  DK231290          
      J=J-1                                                             DK231300          
   19 K1=K1-1                                                           DK231310          
C     ------------------------------------------------------            DK231320          
C     CORRECTS IM AND LAB ARRAYS MATX, NLHM, NS AND IE WHEN MTY=3,5 OR 6DK231330          
C     ------------------------------------------------------            DK231340          
      MATX=L                                                            DK231350          
      K=NS+2                                                            DK231360          
      L=NS                                                              DK231370          
      K1=NS+2                                                           DK231380          
      DO 22 I=1,K1                                                      DK231390          
      IF (I.GT.NS) GO TO 99                                             DK231400          
      IM(K)=IM(L)+NDFA+1                                                DK231410          
      L=L-1                                                             DK231420          
      GO TO 22                                                          DK231430          
   99 IF (K.EQ.2) IM(K)=NDFA+1                                          DK231440          
      IF (K.EQ.1) IM(K)=1                                               DK231450          
   22 K=K-1                                                             DK231460          
      IF (IE.GT.NLHM) GO TO 25                                          DK231470          
      K=NLHM+NDFA+1                                                     DK231480          
      I=NLHM                                                            DK231490          
      DO 26 J=IE,NLHM                                                   DK231500          
      LAB1(K)=RGRSN                                                     DK231510          
      LAB2(K)=LAB2(I)                                                   DK231520          
      LAB3(K)=LAB3(I)                                                   DK231530          
      LAB4(K)=LAB4(I)                                                   DK231540          
      I=I-1                                                             DK231550          
   26 K=K-1                                                             DK231560          
   25 IE=IE+NDFA+1                                                      DK231570          
      NLHM=NLHM+NDFA+1                                                  DK231580          
      KB=NDFA+1                                                         DK231590          
      NS=NS+2                                                           DK231600          
C     ------------------------------------------------------            DK231610          
C     CORRECTS LIT, NCL, IDEN AND NOS ARRAYS AND NOM AND ML             DK231620          
C     WHEN MTY=3, 5 OR 6                                                DK231630          
C     ------------------------------------------------------            DK231640          
      J=301-K2                                                          DK231650          
      K4=101-K2                                                         DK231660          
      DO 37 I=1,K2                                                      DK231670          
      IC(I)=NOS(J)                                                      DK231680          
      NREGP(I)=IDEN(K4)                                                 DK231690          
      J=J+1                                                             DK231700          
   37 K4=K4+1                                                           DK231710          
      IF (NMEA.EQ.1) GO TO 23                                           DK231720          
      K=NOM                                                             DK231730          
      K4=0                                                              DK231740          
      IF (NOM.EQ.0) GO TO 36                                            DK231750          
      DO 24 I=1,NOM                                                     DK231760          
      K4=K4+NCL(I)                                                      DK231770          
      LIT(K+1)=LIT(K)                                                   DK231780          
      NCL(K+1)=NCL(K)                                                   DK231790          
   24 K=K-1                                                             DK231800          
   36 K3=K4                                                             DK231810          
      IF (NON.EQ.0) GO TO 70                                            DK231820          
      DO 71 I=1,NON                                                     DK231830          
   71 NMA(I)=NMA(I)+1                                                   DK231840          
   70 K=NDFA+1                                                          DK231850          
      J=K4+K                                                            DK231860          
      L=J                                                               DK231870          
      DO 27 I=1,L                                                       DK231880          
      IF (I.GT.K3) GO TO 28                                             DK231890          
      IDEN(J)=IDEN(K4)                                                  DK231900          
      K4=K4-1                                                           DK231910          
      GO TO 29                                                          DK231920          
   28 IDEN(J)=NREGP(K)                                                  DK231930          
      K=K-1                                                             DK231940          
   29 J=J-1                                                             DK231950          
   27 CONTINUE                                                          DK231960          
      IF (N2F.EQ.0) GO TO 23                                            DK231970          
      DO 98 I=1,N2F                                                     DK231980          
      INT1(I)=INT1(I)+1                                                 DK231990          
   98 INT2(I)=INT2(I)+1                                                 DK232000          
   23 LIT(1)=LAB1(100)                                                  DK232010          
      NCL(1)=NDFA+1                                                     DK232020          
      NOM=NOM+1-NMEA                                                    DK232030          
      ML=0                                                              DK232040          
      J=NCC+NCL(1)                                                      DK232050          
      L=J                                                               DK232060          
      K4=NCC                                                            DK232070          
      K1=NCL(1)                                                         DK232080          
      DO 33 I=1,L                                                       DK232090          
      IF (I.GT.NCC) GO TO 34                                            DK232100          
      NOS(J)=NOS(K4)                                                    DK232110          
      K4=K4-1                                                           DK232120          
      GO TO 33                                                          DK232130          
   34 NOS(J)=IC(K1)                                                     DK232140          
      K1=K1-1                                                           DK232150          
   33 J=J-1                                                             DK232160          
      GO TO 1                                                           DK232170          
C     ------------------------------------------------------            DK232180          
C     SETS UP B AND C-1 ARRAYS FOR FIXED EFFECTS WHEN MTY=7             DK232190          
C     ------------------------------------------------------            DK232200          
    7 K2=NDFA+NDFC+NDFAC+1                                              DK232210          
      L=NLHM+K2                                                         DK232220          
      I1=NDFA+NDFC                                                      DK232230          
      K=K2-1                                                            DK232240          
      I2=1                                                              DK232250          
      DO 38 I=1,NRHM                                                    DK232260          
      I3=5000-NDFA*NRHM-(NDFC+NDFAC)*(NRHM-I+1)                         DK232270          
      DO 38 J=1,K                                                       DK232280          
      K4=I3+J-1-NDFA                                                    DK232290          
      IF (J.LE.NDFA) K4=5000-NDFA*(NRHM-I+1)+J-1                        DK232300          
      RHM(I2)=ARRAY(K4)                                                 DK232310          
   38 I2=I2+1                                                           DK232320          
      K=MATX+NLHM*NRHM                                                  DK232330          
      K1=L*(L+1)/2+L*NRHM                                               DK232340          
      I1=NLHM-IEI+1                                                     DK232350          
      I2=I1+NDFAC                                                       DK232360          
      I3=NLHM+NDFAC                                                     DK232370          
      DO 39 I=1,NRHM                                                    DK232380          
      K3=L+NRHM+1-I                                                     DK232390          
      I4=(K2-1)*(1+NRHM-I)                                              DK232400          
      SUM=TOT5(K3)                                                      DK232410          
      DO 40 J=1,L                                                       DK232420          
      K4=L+1-J                                                          DK232430          
      IF (J.GT.I1) GO TO 41                                             DK232440          
      SUM=SUM-TOT5(K4)*ARRAY(K)                                         DK232450          
      ARRAY(K1)=ARRAY(K)                                                DK232460          
      K=K-1                                                             DK232470          
      GO TO 40                                                          DK232480          
   41 IF (J.GT.I2) GO TO 42                                             DK232490          
      SUM=SUM-TOT5(K4)*RHM(I4)                                          DK232500          
      ARRAY(K1)=RHM(I4)                                                 DK232510          
      I4=I4-1                                                           DK232520          
      GO TO 40                                                          DK232530          
   42 IF (J.GT.I3) GO TO 43                                             DK232540          
      SUM=SUM-TOT5(K4)*ARRAY(K)                                         DK232550          
      ARRAY(K1)=ARRAY(K)                                                DK232560          
      K=K-1                                                             DK232570          
      GO TO 40                                                          DK232580          
   43 IF (J.EQ.L) GO TO 44                                              DK232590          
      SUM=SUM-TOT5(K4)*RHM(I4)                                          DK232600          
      ARRAY(K1)=RHM(I4)                                                 DK232610          
      I4=I4-1                                                           DK232620          
      GO TO 40                                                          DK232630          
   44 ARRAY(K1)=SUM/TOT5(1)                                             DK232640          
   40 K1=K1-1                                                           DK232650          
   39 CONTINUE                                                          DK232660          
      L=L*(L+1)/2                                                       DK232670          
      I1=I1*(I1+1)/2                                                    DK232680          
      I2=I2*(I2+1)/2                                                    DK232690          
      I3=I3*(I3+1)/2                                                    DK232700          
      K3=NLHM-IEI+1                                                     DK232710          
      K4=0                                                              DK232720          
      I4=0                                                              DK232730          
      J=4999                                                            DK232740          
      DO 45 I=1,L                                                       DK232750          
      IF (I.GT.I1) GO TO 46                                             DK232760          
      ARRAY(K1)=FAB(K)                                                  DK232770          
      K=K-1                                                             DK232780          
      GO TO 45                                                          DK232790          
   46 IF (I.GT.I2) GO TO 47                                             DK232800          
      ARRAY(K1)=FAB(J)                                                  DK232810          
      J=J-1                                                             DK232820          
      GO TO 45                                                          DK232830          
   47 IF (I.GT.I3) GO TO 48                                             DK232840          
      IF (K4.EQ.K3) GO TO 4                                             DK232850          
      K4=K4+1                                                           DK232860          
      ARRAY(K1)=FAB(K)                                                  DK232870          
      K=K-1                                                             DK232880          
      GO TO 45                                                          DK232890          
    4 I4=I4+1                                                           DK232900          
      IF (I4.EQ.NDFAC) GO TO 5                                          DK232910          
      GO TO 45                                                          DK232920          
    5 I4=0                                                              DK232930          
      K4=0                                                              DK232940          
      K3=K3+1                                                           DK232950          
      GO TO 45                                                          DK232960          
   48 ARRAY(K1)=FAB(J)                                                  DK232970          
      J=J-1                                                             DK232980          
   45 K1=K1-1                                                           DK232990          
C     ------------------------------------------------------            DK233000          
C     CORRECTS IM AND LAB ARRAYS AND MATX, NLHM, NS AND IE WHEN MTY=7   DK233010          
C     ------------------------------------------------------            DK233020          
      MATX=L                                                            DK233030          
      K=NS+4                                                            DK233040          
      I1=NS+1                                                           DK233050          
      L=NS                                                              DK233060          
      K1=NS-NME-NNE                                                     DK233070          
      DO 49 I=1,I1                                                      DK233080          
      IF (I.GT.K1) GO TO 50                                             DK233090          
      IM(K)=IM(L)+K2                                                    DK233100          
      L=L-1                                                             DK233110          
      GO TO 49                                                          DK233120          
   50 IF (I.EQ.K1+1) GO TO 51                                           DK233130          
      IM(K)=IM(L)+K2-NDFAC                                              DK233140          
      L=L-1                                                             DK233150          
      GO TO 49                                                          DK233160          
   51 IM(K)=IM(L)+K2                                                    DK233170          
      IF (L.EQ.0) IM(K)=K2                                              DK233180          
   49 K=K-1                                                             DK233190          
      IM(3)=NDFA+NDFC+1                                                 DK233200          
      IM(2)=NDFA+1                                                      DK233210          
      IM(1)=1                                                           DK233220          
      IF (IE.GT.NLHM) GO TO 52                                          DK233230          
      K=NLHM+K2                                                         DK233240          
      I=NLHM                                                            DK233250          
      DO 53 J=IE,NLHM                                                   DK233260          
      LAB1(K)=RGRSN                                                     DK233270          
      LAB2(K)=LAB2(I)                                                   DK233280          
      LAB3(K)=LAB3(I)                                                   DK233290          
      LAB4(K)=LAB4(I)                                                   DK233300          
      I=I-1                                                             DK233310          
   53 K=K-1                                                             DK233320          
   52 IE=IE+K2                                                          DK233330          
      NLHM=NLHM+K2                                                      DK233340          
      KB=K2                                                             DK233350          
      NS=NS+4                                                           DK233360          
C     ------------------------------------------------------            DK233370          
C     CORRECTS LIT,NCL,IDEN AND NOS ARRAYS AND NOM AND ML WHEN MTY=7    DK233380          
C     ------------------------------------------------------            DK233390          
      I3=NDFA+NDFC+2                                                    DK233400          
      K3=I3+(NDFA+1)*(NDFC+1)                                           DK233410          
      J=301-K3                                                          DK233420          
      K4=101-I3                                                         DK233430          
      DO 54 I=1,K3                                                      DK233440          
      IC(I)=NOS(J)                                                      DK233450          
      IF (I.GT.I3) GO TO 54                                             DK233460          
      NREGP(I)=IDEN(K4)                                                 DK233470          
      K4=K4+1                                                           DK233480          
   54 J=J+1                                                             DK233490          
      I3=NCC                                                            DK233500          
      IF (NOM.EQ.0) GO TO 64                                            DK233510          
      DO 65 I=1,NOM                                                     DK233520          
   65 I3=I3-NCL(I)                                                      DK233530          
   64 IF (NON.EQ.0) GO TO 66                                            DK233540          
      DO 67 I=1,NON                                                     DK233550          
      IF (NMEA.EQ.0)NMA(I)=NMA(I)+2                                     DK233560          
      IF(NMEA.EQ.1)NMA(I)=NMA(I)+1                                      DK233570          
   67 I3=I3-NCLN(I)                                                     DK233580          
   66 I3=I3+ML+MLB                                                      DK233590          
      IF (NMEA.EQ.2) GO TO 55                                           DK233600          
      J=2                                                               DK233610          
      IF (NMEA.EQ.1) J=1                                                DK233620          
      K=NOM                                                             DK233630          
      K4=0                                                              DK233640          
      IF (NOM.EQ.0) GO TO 57                                            DK233650          
      DO 58 I=1,NOM                                                     DK233660          
      K4=K4+NCL(I)                                                      DK233670          
      LIT(K+J)=LIT(K)                                                   DK233680          
      NCL(K+J)=NCL(K)                                                   DK233690          
   58 K=K-1                                                             DK233700          
      J=K4+NDFA+NDFC+2                                                  DK233710          
      IF (NMEA.EQ.1.AND.LIT(1).EQ.LAB1(98)) J=K4+NDFA+1                 DK233720          
      IF (NMEA.EQ.1.AND.LIT(1).EQ.LAB1(100)) J=K4+NDFC+1                DK233730          
      L=K4                                                              DK233740          
      DO 59 I=1,L                                                       DK233750          
      IDEN(J)=IDEN(K4)                                                  DK233760          
      K4=K4-1                                                           DK233770          
   59 J=J-1                                                             DK233780          
   57 K=NDFA+NDFC+2                                                     DK233790          
      DO 56 I=1,K                                                       DK233800          
   56 IDEN(I)=NREGP(I)                                                  DK233810          
   55 LIT(1)=LAB1(100)                                                  DK233820          
      LIT(2)=LAB1(98)                                                   DK233830          
      NCL(1)=NDFA+1                                                     DK233840          
      NCL(2)=NDFC+1                                                     DK233850          
      NOM=NOM+2-NMEA                                                    DK233860          
      IF (N2F.EQ.0) GO TO 69                                            DK233870          
      J=N2F                                                             DK233880          
      K=2                                                               DK233890          
      IF (NMEA.EQ.1) K=1                                                DK233900          
      IF (NMEA.EQ.2) K=0                                                DK233910          
      DO 68 I=1,N2F                                                     DK233920          
      INT1(J+1)=INT1(J)+K                                               DK233930          
      INT2(J+1)=INT2(J)+K                                               DK233940          
      NMC(J+1)=NMC(J)                                                   DK233950          
   68 J=J-1                                                             DK233960          
   69 INT1(1)=1                                                         DK233970          
      INT2(1)=2                                                         DK233980          
      NMC(1)=0                                                          DK233990          
      ML=0                                                              DK234000          
      N2F=N2F+1                                                         DK234010          
      J=NCC+K3                                                          DK234020          
      L=J                                                               DK234030          
      K4=NCC                                                            DK234040          
      I1=NCC+(NDFA+1)*(NDFC+1)                                          DK234050          
      I2=I3+(NDFA+1)*(NDFC+1)                                           DK234060          
      DO 60 I=1,L                                                       DK234070          
      IF (I.GT.I3) GO TO 61                                             DK234080          
      NOS(J)=NOS(K4)                                                    DK234090          
      K4=K4-1                                                           DK234100          
      GO TO 60                                                          DK234110          
   61 IF (I.GT.I2) GO TO 62                                             DK234120          
      NOS(J)=IC(K3)                                                     DK234130          
      K3=K3-1                                                           DK234140          
      GO TO 60                                                          DK234150          
   62 IF (I.GT.I1) GO TO 63                                             DK234160          
      NOS(J)=NOS(K4)                                                    DK234170          
      K4=K4-1                                                           DK234180          
      GO TO 60                                                          DK234190          
   63 NOS(J)=IC(K3)                                                     DK234200          
      K3=K3-1                                                           DK234210          
   60 J=J-1                                                             DK234220          
    1 MN2=1                                                             DK234230          
      IF (MTY.GT.1) NMEA=0                                              DK234240          
      IF (MTY.GT.1) NNEA=0                                              DK234250          
      CALL LSMNS                                                        DK234260          
      WRITE (6,1001) IJOB                                               DK234270          
 1001 FORMAT (1H0,29X,43HLISTING OF INVERSE ELEMENTS FOR PROBLEM NO.,I3)DK234280          
      WRITE (6,1004)                                                    DK234290          
 1004 FORMAT (1H )                                                      DK234300          
      WRITE (6,1025) (ARRAY(I),I=1,MATX)                                DK234310          
 1025 FORMAT (7(2X,E15.8))                                              DK234320          
      WRITE (6,1022)                                                    DK234330          
 1022 FORMAT (1H0,29X,36HLISTING OF DIAGONAL INVERSE ELEMENTS)          DK234340          
      WRITE (6,1004)                                                    DK234350          
      K=1                                                               DK234360          
      DO 75 I=1,NLHM                                                    DK234370          
      X(I)=ARRAY(K)                                                     DK234380          
   75 K=K+NLHM-I+1                                                      DK234390          
      WRITE (6,1025) (X(K),K=1,NLHM)                                    DK234400          
      RETURN                                                            DK234410          
  300 WRITE (6,1021)                                                    DK234420          
 1021 FORMAT (1H0,10X,29HNEGATIVE ERROR SUM OF SQUARES)                 DK234430          
      RETURN                                                            DK234440          
      END                                                               DK234450          
