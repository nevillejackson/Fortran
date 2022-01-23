      SUBROUTINE SUAFMM                                                 DK230010          
C     ------------------------------------------------------            DK230020          
C     SUBROUTINE WHICH CORRECTS ARRAYS FOR COMPUTATION OF LS MEANS      DK230030          
C     AND STANDARD ERRORS FOR MIXED MODELS                              DK230040          
C     ------------------------------------------------------            DK230050          
      IMPLICIT REAL*8(A-H,O-Z)                                          DK230060          
      REAL*8 LAB1,LAB2,LAB3,LAB4,LITY,NLIT,LIT,LITR,NAME,NAM5           DK230070          
      DIMENSION ARRAY(5000),SSCPR(630),SSS(630),RHM(2450),TOT(106),TOT2(DK230080          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100),NS2(15)    DK230090          
      DIMENSION FAB(5000),FY(630),TOT4(106),SAB(630),FSQ(525),TOT5(106) DK230100          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),MZ(10),IPL(13),NSDK230110          
     1P(13),NND(13),XP(81),YP(41),      NDC(10),NMI(10),MEN(20),NCL(20),DK230120          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(40),NCLN(40),LNE(40),NDEN(1DK230130          
     300),NLIT(40),NMA(30),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(40),DK230140          
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDK230150          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),IC(480),DK230160          
     6EFF1(50),EFF2(50),NOS(300),X(106),NMAC(40),I309(15),NR1(15),NW(15)DK230170          
     7,R1I(50),R2I(50)                                                  DK230180          
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADK230190          
     1B4,LITY,TRED,YM,XP,YP,IM,MS,NEG,IPL,NSP,NND,FAB,FY,TOT4,SAB,FSQ,TODK230200          
     2T5,MZ,R1I,R2I                                                     DK230210          
      COMMON /CMBLK2/LIT,LITR,NLIT,XM,EFF1,EFF2,X,NDC,NMI,MEN,NCL,LME,IBDK230220          
     1EG,IDEN,NCLN,NEN,LNE,NBEG,NDEN,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQDK230230          
     2C,NREGP,LGTX,JBEG,NDECX,NEGY,LNY,LHY,KBEG,NDECY,IC,NOS,NMAC       DK230240          
      COMMON /CMBLK3/RR,REP,DF,SDF,EDF,WK,IJOB,NAB,NLHM,NRHM,NMEA,NME,NNDK230250          
     1EA,NNE,N2F,NPR,IRAN,MPOP,LIOP,IAD,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,DK230260          
     2NOM,NON,ML,MLB,NS,NOT,KPUT,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,IN,I30DK230270          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DK230280          
     1L,L7,KA,KB,KC,SOF,NX,KD,MTY,MAN,NRUN,NRN,EDFF,NAME,EDFS,NAM5,IFP,NDK230290          
     2DFA,IFPC,NDFC,NDFAC,NINT                                          DK230300          
      DATA RGRSN/6HRGRSN /                                              DK230310          
      IF (KD.EQ.1) GO TO 76                                             DK230320          
      NAB=0                                                             DK230330          
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
      DO 32 I=IE,NLHM                                                   DK230770          
   32 LAB1(I)=RGRSN                                                     DK230780          
   31 NLHM=NLHM+1                                                       DK230790          
      NS=NS+1                                                           DK230800          
      MATX=MATX+NLHM                                                    DK230810          
      IE=IE+1                                                           DK230820          
      GO TO 1                                                           DK230830          
C     ------------------------------------------------------            DK230840          
C     SETS UP B AND C-1 ARRAYS FOR ALL FIXED EFFECTS WHEN MTY=3, 5 OR 6 DK230850          
C     ------------------------------------------------------            DK230860          
    3 K2=NDFA+1                                                         DK230870          
      L=NLHM+K2                                                         DK230880          
      K4=5000-K2*NRHM                                                   DK230890          
      J=1                                                               DK230900          
      K=NDFA*NRHM+K4-1                                                  DK230910          
      DO 20 I=K4,K                                                      DK230920          
      RHM(J)=ARRAY(I)                                                   DK230930          
   20 J=J+1                                                             DK230940          
      K=MATX+NLHM*NRHM                                                  DK230950          
      K1=L*(L+1)/2+L*NRHM                                               DK230960          
      K4=NDFA*NRHM                                                      DK230970          
      DO 35 I=1,NRHM                                                    DK230980          
      K3=L+NRHM+1-I                                                     DK230990          
      SUM=TOT5(K3)                                                      DK231000          
      DO 16 J=1,L                                                       DK231010          
      I2=L+1-J                                                          DK231020          
      IF (J.GT.NLHM) GO TO 17                                           DK231030          
      SUM=SUM-TOT5(I2)*ARRAY(K)                                         DK231040          
      ARRAY(K1)=ARRAY(K)                                                DK231050          
      K=K-1                                                             DK231060          
      GO TO 16                                                          DK231070          
   17 IF (J.EQ.L) GO TO 18                                              DK231080          
      SUM=SUM-TOT5(I2)*RHM(K4)                                          DK231090          
      ARRAY(K1)=RHM(K4)                                                 DK231100          
      K4=K4-1                                                           DK231110          
      GO TO 16                                                          DK231120          
   18 ARRAY(K1)=SUM/TOT5(1)                                             DK231130          
   16 K1=K1-1                                                           DK231140          
   35 CONTINUE                                                          DK231150          
      L=L*(L+1)/2                                                       DK231160          
      J=4999                                                            DK231170          
      DO 19 I=1,L                                                       DK231180          
      IF (I.GT.MATX) GO TO 21                                           DK231190          
      ARRAY(K1)=FAB(K)                                                  DK231200          
      K=K-1                                                             DK231210          
      GO TO 19                                                          DK231220          
   21 ARRAY(K1)=FAB(J)                                                  DK231230          
      J=J-1                                                             DK231240          
   19 K1=K1-1                                                           DK231250          
C     ------------------------------------------------------            DK231260          
C     CORRECTS IM AND LAB ARRAYS MATX, NLHM, NS AND IE WHEN MTY=3,5 OR 6DK231270          
C     ------------------------------------------------------            DK231280          
      MATX=L                                                            DK231290          
      K=NS+2                                                            DK231300          
      L=NS                                                              DK231310          
      K1=NS+2                                                           DK231320          
      DO 22 I=1,K1                                                      DK231330          
      IF (I.GT.NS) GO TO 99                                             DK231340          
      IM(K)=IM(L)+NDFA+1                                                DK231350          
      L=L-1                                                             DK231360          
      GO TO 22                                                          DK231370          
   99 IF (K.EQ.2) IM(K)=NDFA+1                                          DK231380          
      IF (K.EQ.1) IM(K)=1                                               DK231390          
   22 K=K-1                                                             DK231400          
      IF (IE.GT.NLHM) GO TO 25                                          DK231410          
      K=NLHM+NDFA+1                                                     DK231420          
      I=NLHM                                                            DK231430          
      DO 26 J=IE,NLHM                                                   DK231440          
      LAB1(K)=RGRSN                                                     DK231450          
      LAB2(K)=LAB2(I)                                                   DK231460          
      LAB3(K)=LAB3(I)                                                   DK231470          
      LAB4(K)=LAB4(I)                                                   DK231480          
      I=I-1                                                             DK231490          
   26 K=K-1                                                             DK231500          
   25 IE=IE+NDFA+1                                                      DK231510          
      NLHM=NLHM+NDFA+1                                                  DK231520          
      NS=NS+2                                                           DK231530          
C     ------------------------------------------------------            DK231540          
C     CORRECTS LIT, NCL, IDEN AND NOS ARRAYS AND NOM AND ML             DK231550          
C     WHEN MTY=3, 5 OR 6                                                DK231560          
C     ------------------------------------------------------            DK231570          
      J=301-K2                                                          DK231580          
      K4=101-K2                                                         DK231590          
      DO 37 I=1,K2                                                      DK231600          
      IC(I)=NOS(J)                                                      DK231610          
      NREGP(I)=IDEN(K4)                                                 DK231620          
      J=J+1                                                             DK231630          
   37 K4=K4+1                                                           DK231640          
      IF (NMEA.EQ.1) GO TO 23                                           DK231650          
      K=NOM                                                             DK231660          
      K4=0                                                              DK231670          
      IF (NOM.EQ.0) GO TO 36                                            DK231680          
      DO 24 I=1,NOM                                                     DK231690          
      K4=K4+NCL(I)                                                      DK231700          
      LIT(K+1)=LIT(K)                                                   DK231710          
      NCL(K+1)=NCL(K)                                                   DK231720          
   24 K=K-1                                                             DK231730          
   36 K3=K4                                                             DK231740          
      K=NDFA+1                                                          DK231750          
      J=K4+K                                                            DK231760          
      L=J                                                               DK231770          
      DO 27 I=1,L                                                       DK231780          
      IF (I.GT.K3) GO TO 28                                             DK231790          
      IDEN(J)=IDEN(K4)                                                  DK231800          
      K4=K4-1                                                           DK231810          
      GO TO 29                                                          DK231820          
   28 IDEN(J)=NREGP(K)                                                  DK231830          
      K=K-1                                                             DK231840          
   29 J=J-1                                                             DK231850          
   27 CONTINUE                                                          DK231860          
   23 LIT(1)=LAB1(100)                                                  DK231870          
      NCL(1)=NDFA+1                                                     DK231880          
      NOM=NOM+1-NMEA                                                    DK231890          
      ML=0                                                              DK231900          
      J=NCC+NCL(1)                                                      DK231910          
      L=J                                                               DK231920          
      K4=NCC                                                            DK231930          
      K1=NCL(1)                                                         DK231940          
      DO 33 I=1,L                                                       DK231950          
      IF (I.GT.NCC) GO TO 34                                            DK231960          
      NOS(J)=NOS(K4)                                                    DK231970          
      K4=K4-1                                                           DK231980          
      GO TO 33                                                          DK231990          
   34 NOS(J)=IC(K1)                                                     DK232000          
      K1=K1-1                                                           DK232010          
   33 J=J-1                                                             DK232020          
      GO TO 1                                                           DK232030          
C     ------------------------------------------------------            DK232040          
C     SETS UP B AND C-1 ARRAYS FOR FIXED EFFECTS WHEN MTY=7             DK232050          
C     ------------------------------------------------------            DK232060          
    7 K2=NDFA+NDFC+NDFAC+1                                              DK232070          
      L=NLHM+K2                                                         DK232080          
      K4=5000-L*NRHM+1                                                  DK232090          
      J=1                                                               DK232100          
      K=(K2-1)*NRHM+K4                                                  DK232110          
      DO 38 I=K4,K                                                      DK232120          
      RHM(J)=ARRAY(I)                                                   DK232130          
   38 J=J+1                                                             DK232140          
      K=MATX+NLHM*NRHM                                                  DK232150          
      K1=L*(L+1)/2+L*NRHM                                               DK232160          
      I1=NLHM-IEI+1                                                     DK232170          
      I2=I1+NDFAC                                                       DK232180          
      I3=NLHM+NDFAC                                                     DK232190          
      DO 39 I=1,NRHM                                                    DK232200          
      K3=L+NRHM+1-I                                                     DK232210          
      I4=(K2-1)*(1+NRHM-I)                                              DK232220          
      SUM=TOT5(K3)                                                      DK232230          
      DO 40 J=1,L                                                       DK232240          
      K4=L+1-J                                                          DK232250          
      IF (J.GT.I1) GO TO 41                                             DK232260          
      SUM=SUM-TOT5(K4)*ARRAY(K)                                         DK232270          
      ARRAY(K1)=ARRAY(K)                                                DK232280          
      K=K-1                                                             DK232290          
      GO TO 40                                                          DK232300          
   41 IF (J.GT.I2) GO TO 42                                             DK232310          
      SUM=SUM-TOT5(K4)*RHM(I4)                                          DK232320          
      ARRAY(K1)=RHM(I4)                                                 DK232330          
      I4=I4-1                                                           DK232340          
      GO TO 40                                                          DK232350          
   42 IF (J.GT.I3) GO TO 43                                             DK232360          
      SUM=SUM-TOT5(K4)*ARRAY(K)                                         DK232370          
      ARRAY(K1)=ARRAY(K)                                                DK232380          
      K=K-1                                                             DK232390          
      GO TO 40                                                          DK232400          
   43 IF (J.EQ.L) GO TO 44                                              DK232410          
      SUM=SUM-TOT5(K4)*RHM(I4)                                          DK232420          
      ARRAY(K1)=RHM(I4)                                                 DK232430          
      I4=I4-1                                                           DK232440          
      GO TO 40                                                          DK232450          
   44 ARRAY(K1)=SUM/TOT5(1)                                             DK232460          
   40 K1=K1-1                                                           DK232470          
   39 CONTINUE                                                          DK232480          
      L=L*(L+1)/2                                                       DK232490          
      I1=I1*(I1+1)/2                                                    DK232500          
      I2=I2*(I2+1)/2                                                    DK232510          
      I3=I3*(I3+1)/2                                                    DK232520          
      J=4999                                                            DK232530          
      DO 45 I=1,L                                                       DK232540          
      IF (I.GT.I1) GO TO 46                                             DK232550          
      ARRAY(K1)=FAB(K)                                                  DK232560          
      K=K-1                                                             DK232570          
      GO TO 45                                                          DK232580          
   46 IF (I.GT.I2) GO TO 47                                             DK232590          
      ARRAY(K1)=FAB(J)                                                  DK232600          
      J=J-1                                                             DK232610          
      GO TO 45                                                          DK232620          
   47 IF (I.GT.I3) GO TO 48                                             DK232630          
      ARRAY(K1)=FAB(K)                                                  DK232640          
      K=K-1                                                             DK232650          
      GO TO 45                                                          DK232660          
   48 ARRAY(K1)=FAB(J)                                                  DK232670          
      J=J-1                                                             DK232680          
   45 K1=K1-1                                                           DK232690          
C     ------------------------------------------------------            DK232700          
C     CORRECTS IM AND LAB ARRAYS AND MATX, NLHM, NS AND IE WHEN MTY=7   DK232710          
C     ------------------------------------------------------            DK232720          
      MATX=L                                                            DK232730          
      K=NS+4                                                            DK232740          
      I1=NS+1                                                           DK232750          
      L=NS                                                              DK232760          
      K1=NS-NME-NNE                                                     DK232770          
      DO 49 I=1,I1                                                      DK232780          
      IF (I.GT.K1) GO TO 50                                             DK232790          
      IM(K)=IM(L)+K2                                                    DK232800          
      L=L-1                                                             DK232810          
      GO TO 49                                                          DK232820          
   50 IF (I.EQ.K1+1) GO TO 51                                           DK232830          
      IM(K)=IM(L)+K2-NDFAC                                              DK232840          
      L=L-1                                                             DK232850          
      GO TO 49                                                          DK232860          
   51 IM(K)=IM(L)+K2                                                    DK232870          
      IF (L.EQ.0) IM(K)=K2                                              DK232880          
   49 K=K-1                                                             DK232890          
      IM(3)=NDFA+NDFC+1                                                 DK232900          
      IM(2)=NDFA+1                                                      DK232910          
      IM(1)=1                                                           DK232920          
      IF (IE.GT.NLHM) GO TO 52                                          DK232930          
      K=NLHM+K2                                                         DK232940          
      I=NLHM                                                            DK232950          
      DO 53 J=IE,NLHM                                                   DK232960          
      LAB1(K)=RGRSN                                                     DK232970          
      LAB2(K)=LAB2(I)                                                   DK232980          
      LAB3(K)=LAB3(I)                                                   DK232990          
      LAB4(K)=LAB4(I)                                                   DK233000          
      I=I-1                                                             DK233010          
   53 K=K-1                                                             DK233020          
   52 IE=IE+K2                                                          DK233030          
      NLHM=NLHM+K2                                                      DK233040          
      NS=NS+4                                                           DK233050          
C     ------------------------------------------------------            DK233060          
C     CORRECTS LIT,NCL,IDEN AND NOS ARRAYS AND NOM AND ML WHEN MTY=7    DK233070          
C     ------------------------------------------------------            DK233080          
      K3=NDFA+NDFC+2+(NDFA+1)*(NDFC+1)                                  DK233090          
      J=301-K3                                                          DK233100          
      K4=101-K3                                                         DK233110          
      DO 54 I=1,K3                                                      DK233120          
      IC(I)=NOS(J)                                                      DK233130          
      NREGP(I)=IDEN(K4)                                                 DK233140          
      J=J+1                                                             DK233150          
   54 K4=K4+1                                                           DK233160          
      I3=NCC                                                            DK233170          
      IF (NOM.EQ.0) GO TO 64                                            DK233180          
      DO 65 I=1,NOM                                                     DK233190          
   65 I3=I3-NCL(I)                                                      DK233200          
   64 IF (NON.EQ.0) GO TO 66                                            DK233210          
      DO 67 I=1,NON                                                     DK233220          
   67 I3=I3-NCLN(I)                                                     DK233230          
   66 I3=I3+ML+MLB                                                      DK233240          
      IF (NMEA.EQ.2) GO TO 55                                           DK233250          
      J=2                                                               DK233260          
      IF (NMEA.EQ.1) J=1                                                DK233270          
      K=NOM                                                             DK233280          
      K4=0                                                              DK233290          
      IF (NOM.EQ.0) GO TO 57                                            DK233300          
      DO 58 I=1,NOM                                                     DK233310          
      K4=K4+NCL(I)                                                      DK233320          
      LIT(K+J)=LIT(K)                                                   DK233330          
      NCL(K+J)=NCL(K)                                                   DK233340          
   58 K=K-1                                                             DK233350          
      J=K4+NDFA+NDFC+2                                                  DK233360          
      IF (NMEA.EQ.1.AND.LIT(1).EQ.LAB1(98)) J=K4+NDFA+1                 DK233370          
      IF (NMEA.EQ.1.AND.LIT(1).EQ.LAB1(100)) J=K4+NDFC+1                DK233380          
      L=K4                                                              DK233390          
      DO 59 I=1,L                                                       DK233400          
      IDEN(J)=IDEN(K4)                                                  DK233410          
      K4=K4-1                                                           DK233420          
   59 J=J-1                                                             DK233430          
   57 K=NDFA+NDFC+2                                                     DK233440          
      DO 56 I=1,K                                                       DK233450          
   56 IDEN(I)=NREGP(I)                                                  DK233460          
   55 LIT(1)=LAB1(100)                                                  DK233470          
      LIT(2)=LAB1(98)                                                   DK233480          
      NCL(1)=NDFA+1                                                     DK233490          
      NCL(2)=NDFC+1                                                     DK233500          
      NOM=NOM+2-NMEA                                                    DK233510          
      IF (N2F.EQ.0) GO TO 69                                            DK233520          
      J=N2F                                                             DK233530          
      K=2                                                               DK233540          
      IF (NMEA.EQ.1) K=1                                                DK233550          
      IF (NMEA.EQ.2) K=0                                                DK233560          
      DO 68 I=1,N2F                                                     DK233570          
      INT1(J+1)=INT1(J)+K                                               DK233580          
      INT2(J+1)=INT2(J)+K                                               DK233590          
      NMC(J+1)=NMC(J)                                                   DK233600          
   68 J=J-1                                                             DK233610          
   69 INT1(1)=1                                                         DK233620          
      INT2(1)=2                                                         DK233630          
      NMC(1)=0                                                          DK233640          
      ML=0                                                              DK233650          
      N2F=N2F+1                                                         DK233660          
      J=NCC+K3                                                          DK233670          
      L=J                                                               DK233680          
      K4=NCC                                                            DK233690          
      I1=NCC+(NDFA+1)*(NDFC+1)                                          DK233700          
      I2=I3+(NDFA+1)*(NDFC+1)                                           DK233710          
      DO 60 I=1,L                                                       DK233720          
      IF (I.GT.I3) GO TO 61                                             DK233730          
      NOS(J)=NOS(K4)                                                    DK233740          
      K4=K4-1                                                           DK233750          
      GO TO 60                                                          DK233760          
   61 IF (I.GT.I2) GO TO 62                                             DK233770          
      NOS(J)=IC(K3)                                                     DK233780          
      K3=K3-1                                                           DK233790          
      GO TO 60                                                          DK233800          
   62 IF (I.GT.I1) GO TO 63                                             DK233810          
      NOS(J)=NOS(K4)                                                    DK233820          
      K4=K4-1                                                           DK233830          
      GO TO 60                                                          DK233840          
   63 NOS(J)=IC(K3)                                                     DK233850          
      K3=K3-1                                                           DK233860          
   60 J=J-1                                                             DK233870          
    1 MN2=1                                                             DK233880          
      CALL LSMNS                                                        DK233890          
      IF (NLC.EQ.0) GO TO 77                                            DK233900          
   76 WRITE (6,1051)                                                    DK233910          
 1051 FORMAT (1H0,23X,66HLISTING OF SELECTED LINEAR FUNCTIONS, STANDARD DK233920          
     1ERRORS AND T VALUES)                                              DK233930          
      WRITE (6,1052)                                                    DK233940          
 1052 FORMAT (1H0,2X,11HDESCRIPTION,2X,12HERR T  NO.  ,84HR(1) C(1) R(2)DK233950          
     1 C(2) R(3) C(3) R(4) C(4) R(5) C(5) R(6) C(6) R(7) C(7) R(8) C(8) DK233960          
     2ETC.)                                                             DK233970          
      DO 301 K1=1,NLC                                                   DK233980          
      READ (5,1050)  A1, A2, A3,KB,NERC,(JBEG(J),TOT2(J),J=1,NERC)      DK233990          
 1050 FORMAT (2A6,A1,I1,I2,12(I2,F3.0),4X/(16(I2,F3.0)))                DK234000          
      WRITE (6,1053)  A1, A2, A3,KB,NERC,(JBEG(J),TOT2(J),J=1,NERC)     DK234010          
 1053 FORMAT (1H0,2A6,A1,2X,I2,5X,I2,5X,9(I2,2X,F4.0,2X)/(17X,9(I2,2X,F4DK234020          
     1.0,2X)))                                                          DK234030          
      WRITE (6,1004)                                                    DK234040          
      DO 302 J=1,NLHM                                                   DK234050          
  302 X(J)=0.                                                           DK234060          
      DO 303 J=1,NERC                                                   DK234070          
      K=JBEG(J)                                                         DK234080          
  303 X(K)=TOT2(J)                                                      DK234090          
      DO 309 J=1,NLHM                                                   DK234100          
  309 TOT2(J)=0.                                                        DK234110          
      YT=0.0                                                            DK234120          
      DO 304 I=1,NRHM                                                   DK234130          
      NCT=1                                                             DK234140          
      NR=MATX+(I-1)*NLHM+1                                              DK234150          
      IF (NCPR.EQ.1) GO TO 305                                          DK234160          
      K4=I                                                              DK234170          
      GO TO 306                                                         DK234180          
  305 K4=NRHM*(I-1)-I*(I-3)/2                                           DK234190          
  306 IF (KB.EQ.1) WK=(SSCPR(K4)-TRED(I))/EDF                           DK234200          
      IF (KB.EQ.1.AND.MN2.EQ.1) WK=SSCPR(K4)                            DK234210          
      IF (KB.EQ.2) WK=XP(I)                                             DK234220          
      IF (KB.EQ.3) WK=YP(I)                                             DK234230          
      IF (WK.LT.0.) GO TO 300                                           DK234240          
  308 CALL CANDSE (AC,ALS,REP,TOT2,X,ARRAY,NR,NLHM,WK,YT,I,SSS,NCT)     DK234250          
      IF (NAB.EQ.0.OR.NAB.EQ.3) ALS=ALS+X(1)*YM(I)                      DK234260          
      TEMP=ALS/REP                                                      DK234270          
  304 WRITE (6,1054) LITY(I),ALS,REP,TEMP                               DK234280          
  301 CONTINUE                                                          DK234290          
 1054 FORMAT (1H ,A6,5X,17HLINEAR FUNCTION =,F17.8,4X,16HSTANDARD ERROR DK234300          
     1=,F17.8,2X,2HT=,F12.3)                                            DK234310          
      IF (KD.EQ.1) RETURN                                               DK234320          
   77 WRITE (6,1001) IJOB                                               DK234330          
 1001 FORMAT (1H0,29X,43HLISTING OF INVERSE ELEMENTS FOR PROBLEM NO.,I3)DK234340          
      WRITE (6,1004)                                                    DK234350          
 1004 FORMAT (1H )                                                      DK234360          
      WRITE (6,1025) (ARRAY(I),I=1,MATX)                                DK234370          
 1025 FORMAT (7(2X,E15.8))                                              DK234380          
      WRITE (6,1022)                                                    DK234390          
 1022 FORMAT (1H0,29X,36HLISTING OF DIAGONAL INVERSE ELEMENTS)          DK234400          
      WRITE (6,1004)                                                    DK234410          
      K=1                                                               DK234420          
      DO 75 I=1,NLHM                                                    DK234430          
      X(I)=ARRAY(K)                                                     DK234440          
   75 K=K+NLHM-I+1                                                      DK234450          
      WRITE (6,1025) (X(K),K=1,NLHM)                                    DK234460          
      RETURN                                                            DK234470          
  300 WRITE (6,1021)                                                    DK234480          
 1021 FORMAT (1H0,10X,29HNEGATIVE ERROR SUM OF SQUARES)                 DK234490          
      RETURN                                                            DK234500          
      END                                                               DK234510          
