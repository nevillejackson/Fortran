*TEXT                                                                                     
      SUBROUTINE PARAM                                                  DK030010          
C     -------------------------------------------                       DK030020          
C     SUBROUTINE WHICH READS AND LISTS PARAMETER CARDS, AND SETS UP     DK030030          
C     ARRAYS FOR LABELING OUTPUT                                        DK030040          
C     ----------------------------------------------------              DK030050          
      IMPLICIT REAL*8(A-H,O-Z)                                          DK030060          
      REAL*8 LAB1,LAB2,LAB3,LAB4,LITY,NLIT,LIT,LITR,MU,LINEAR,NAME,NAM5 DK030070          
      DIMENSION ARRAY(5000),SSCPR(630),SSS(630),RHM(2450),TOT(106),TOT2(DK030080          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100),NS2(15)    DK030090          
      DIMENSION FAB(5000),FY(630),TOT4(106),SAB(630),FSQ(525),TOT5(106) DK030100          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),MZ(10),IPL(13),NSDK030110          
     1P(13),NND(13),XP(81),YP(41),      NDC(10),NMI(10),MEN(20),NCL(20),DK030120          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(40),NCLN(40),LNE(40),NDEN(1DK030130          
     300),NLIT(40),NMA(30),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(40),DK030140          
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDK030150          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),IC(480),DK030160          
     6EFF1(50),EFF2(50),NOS(300),X(106),NMAC(40),I309(15),NR1(15),NW(15)DK030170          
     7,R1I(50),R2I(50)                                                  DK030180          
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADK030190          
     1B4,LITY,TRED,YM,XP,YP,IM,MS,NEG,IPL,NSP,NND,FAB,FY,TOT4,SAB,FSQ,TODK030200          
     2T5,MZ,R1I,R2I                                                     DK030210          
      COMMON /CMBLK2/LIT,LITR,NLIT,XM,EFF1,EFF2,X,NDC,NMI,MEN,NCL,LME,IBDK030220          
     1EG,IDEN,NCLN,NEN,LNE,NBEG,NDEN,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQDK030230          
     2C,NREGP,LGTX,JBEG,NDECX,NEGY,LNY,LHY,KBEG,NDECY,IC,NOS,NMAC       DK030240          
      COMMON /CMBLK3/RR,REP,DF,SDF,EDF,WK,IJOB,NAB,NLHM,NRHM,NMEA,NME,NNDK030250          
     1EA,NNE,N2F,NPR,IRAN,MPOP,LIOP,IAD,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,DK030260          
     2NOM,NON,ML,MLB,NS,NOT,KPUT,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,IN,I30DK030270          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DK030280          
     1L,L7,KA,KB,KC,SOF,NX,KD,MTY,MAN,NRUN,NRN,EDFF,NAME,EDFS,NAM5,IFP,NDK030290          
     2DFA,IFPC,NDFC,NDFAC,NINT                                          DK030300          
      DATA BLANK,CUBIC,MU,QUAD,RGRSN,LINEAR/6H      ,6HCUBIC ,6HMU    , DK030310          
     16HQUAD  ,6HRGRSN ,6HLINEAR/                                       DK030320          
C     ----------------------------------------------------              DK030330          
C     READS AND LISTS PARAMETER CARD 1.                                 DK030340          
C     ----------------------------------------------------              DK030350          
    1 READ  (5,1000) IJOB,NAB,NCD,ICN1,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F, DK030360          
     1NPR,NLC,NCPR,IRAN,MPOP,LIOP,IN,MTY,MAN,NRUN,NRN                   DK030370          
 1000 FORMAT (36I2)                                                     DK030380          
      IPL(1)=0                                                          DK030390          
      IF (IN.EQ.0) IN=5                                                 DK030400          
      WRITE (6,1001) IJOB                                               DK030410          
 1001 FORMAT (1H1,20X,42HLISTING OF PARAMETER CARDS FOR PROBLEM NO.,I3) DK030420          
      WRITE (6,1002) IJOB,NAB,NCD,ICN1,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F, DK030430          
     1NPR,NLC,NCPR,IRAN,MPOP,LIOP,IN,MTY,MAN,NRUN,NRN                   DK030440          
 1002 FORMAT (1H0,108HIJOB NAB  NCD  ICN1 NLHM NRHM NMEA NME NNEA  NNE  DK030450          
     1N2F  NPR  NLC NCPR IRAN MPOP LIOP   IN  MTY  MAN NRUN  NRN/" ",I3,DK030460          
     2I4,20I5)                                                          DK030470          
C     ----------------------------------------------------              DK030480          
C     READS CONTROL IDENTIFICATION FROM PARAMETER CARDS IF ABSORPTION   DK030490          
C                   IS TO OCCUR                                         DK030500          
C     ----------------------------------------------------              DK030510          
      IF (MTY.GT.1.OR.IRAN.GT.0) NCPR=1                                 DK030520          
      K8=1                                                              DK030530          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4.OR.NAB.EQ.5) GO TO 4         DK030540          
      IM(1)=1                                                           DK030550          
      LAB1(1)=MU                                                        DK030560          
      LAB2(1)=BLANK                                                     DK030570          
      LAB3(1)=BLANK                                                     DK030580          
      LAB4(1)=BLANK                                                     DK030590          
      K8=2                                                              DK030600          
    4 IF (NAB.EQ.0) GO TO 13                                            DK030610          
      GO TO (13,16,5,5,3,2),NAB                                         DK030620          
    2 READ  (5,999) NAME,IAD,NR1(14),NW(14),NCC,(NDC(I),I=1,NCC)        DK030630          
  999 FORMAT (A6,36I2)                                                  DK030640          
      WRITE (6,1003) NAME,IAD,NR1(14),NW(14),NCC,(NDC(I),I=1,NCC)       DK030650          
 1003 FORMAT (1H0,54H NAME   IAD  NR1   NW  NCC  NDC(I) WHERE I=1,2,...NDK030660          
     1CC./1H ,A6,24(3X,I2))                                             DK030670          
      GO TO 13                                                          DK030680          
    5 READ  (5,1004) IAD,REP,NMJC,(NDC(I),I=1,NMJC),NMIC,(NMI(I),I=1,NMIDK030690          
     1C)                                                                DK030700          
      WRITE (6,1005) IAD,REP,NMJC,(NDC(I),I=1,NMJC)                     DK030710          
 1004 FORMAT (I2,F8.6,31I2)                                             DK030720          
 1005 FORMAT (1H0,43HIAD    REP   NCC  NDC(I) WHERE I=1,2,...NCC/1H ,   DK030730          
     1I2,2X,F8.6,I4,20I5)                                               DK030740          
      IF (NAB.EQ.3) NCC=NMJC                                            DK030750          
      RR=REP                                                            DK030760          
      REP=(1.-REP)/REP                                                  DK030770          
      IF (NAB.EQ.4) GO TO 6                                             DK030780          
      GO TO 13                                                          DK030790          
   16 READ (5,999) NAME,IAD,NR1(14),NW(14),NCC,(NDC(I),I=1,NCC),NINT,   DK030800          
     1(MZ(I),I=1,NINT),NMIC,(NMI(I),I=1,NMIC)                           DK030810          
      WRITE (6,1024) NAME,IAD,NR1(14),NW(14),NCC,(NDC(I),I=1,NCC)       DK030820          
 1024 FORMAT (1H0,57H NAME   IAD  NR1   NW  NMJC  NDC(I) WHERE I=1,2,...DK030830          
     1NMJC. /1H ,A6,24(3X,I2))                                          DK030840          
      WRITE (6,1025) NINT,(MZ(I),I=1,NINT)                              DK030850          
 1025 FORMAT (1H0,23X,33HNINT  MZ(I)  WHERE I=1,2,...NINT./1H ,21X,12(3XDK030860          
     1,I2))                                                             DK030870          
      WRITE (6,1026) NMIC,(NMI(I),I=1,NMIC)                             DK030880          
 1026 FORMAT (1H0,23X,33HNMIC  NMI(I) WHERE I=1,2,...NMIC./1H ,21X,12(3XDK030890          
     1,I2))                                                             DK030900          
      GO TO 13                                                          DK030910          
    3 READ (5,999)  NAM5,IAD,NR1(15),NW(15),NMJC,(NDC(I),I=1,NMJC),NMIC,DK030920          
     1(NMI(I),I=1,NMIC)                                                 DK030930          
      WRITE (6,1006) NAM5,IAD,NR1(15),NW(15),NMJC,(NDC(I),I=1,NMJC)     DK030940          
 1006 FORMAT (1H0,57H NAM5   IAD  NR1   NW  NMJC  NDC(I) WHERE I=1,2,...DK030950          
     1NMJC. /1H ,A6,24(3X,I2))                                          DK030960          
    6 WRITE (6,1007) NMIC,(NMI(I),I=1,NMIC)                             DK030970          
 1007 FORMAT (1H0,23X,33HNMIC  NMI(I) WHERE I=1,2,...NMIC. /1H ,24X,15(IDK030980          
     12,3X))                                                            DK030990          
      TOT4(102)=IAD                                                     DK031000          
      LGT=10**NMIC                                                      DK031010          
C     ----------------------------------------------------              DK031020          
C     READS AND LISTS PARAMETER CARDS FOR MAIN EFFECTS                  DK031030          
C     ----------------------------------------------------              DK031040          
   13 K1=1                                                              DK031050          
      K2=0                                                              DK031060          
      IF (NMEA.EQ.0) GO TO 10                                           DK031070          
      DO 8 I=1,NMEA                                                     DK031080          
      READ  (5,1008)   MEN(I),LIT(I),K7,MPOL,LME(I),IBEG(I),(NOS(J),J=1,DK031090          
     1K7)                                                               DK031100          
 1008 FORMAT (I2,A6,I2,2I1,I2,16I4,2X/(20I4))                           DK031110          
      WRITE (6,1009) I,MEN(I),LIT(I),K7,MPOL,LME(I),IBEG(I)             DK031120          
 1009 FORMAT (1H0,6HFOR I=,I4,47H  MEN(I)  LIT(I)  NCL(I)  MPOL  LME(I) DK031130          
     1 IBEG(I)/1H ,12X,I2,6X,A6,2X,I2,6X,I2,4X,I2,7X,I2)                DK031140          
      NCL(I)=K7                                                         DK031150          
      WRITE (6,1010) (NOS(J),J=1,K7)                                    DK031160          
 1010 FORMAT (1H0,72HIDEN(J) WHERE J=K2 BY STEPS OF ONE AS LONG AS J IS DK031170          
     1LESS THAN K2+NCL(I)+1/(1H ,I5,5X,I4,3X,20I5))                     DK031180          
      DO 7 J=1,K7                                                       DK031190          
      K3=K1+J-1                                                         DK031200          
      IDEN(K3)=NOS(J)                                                   DK031210          
    7 NOS(J)=0                                                          DK031220          
      K1=K1+K7                                                          DK031230          
    8 K2=K2+K7                                                          DK031240          
   10 ML=K2                                                             DK031250          
      NOM=NME+NMEA                                                      DK031260          
      IF (NME.EQ.0) GO TO 12                                            DK031270          
      K3=NMEA+1                                                         DK031280          
      DO 11 I=K3,NOM                                                    DK031290          
      READ  (5,1008)   MEN(I),LIT(I),K7,MPOL,LME(I),IBEG(I),(NOS(J),J=1,DK031300          
     1K7)                                                               DK031310          
      WRITE (6,1009) I,MEN(I),LIT(I),K7,MPOL,LME(I),IBEG(I)             DK031320          
      WRITE (6,1010) (NOS(J),J=1,K7)                                    DK031330          
      NCL(I)=K7                                                         DK031340          
      DO 9 J=1,K7                                                       DK031350          
      K3=K1+J-1                                                         DK031360          
      IDEN(K3)=NOS(J)                                                   DK031370          
    9 NOS(J)=0                                                          DK031380          
      IF (K8.NE.1) GO TO 17                                             DK031390          
      IM(K8)=NCL(I)-1                                                   DK031400          
      K5=1                                                              DK031410          
      GO TO 18                                                          DK031420          
   17 IM(K8)=NCL(I)-1+IM(K8-1)                                          DK031430          
      K5=IM(K8-1)+1                                                     DK031440          
C     ----------------------------------------------------              DK031450          
C     SETS UP BEGINNING AND ENDING ROW NUMBERS FOR MAIN EFFECTS FOR     DK031460          
C     WHICH POLYNOMIALS ARE TO BE FITTED                                DK031470          
C     ----------------------------------------------------              DK031480          
   18 IF (MPOL.EQ.0) GO TO 99                                           DK031490          
      NSME=NSME+1                                                       DK031500          
      IPL(NSME)=MEN(I)                                                  DK031510          
      NSP(NSME)=K5                                                      DK031520          
      NND(NSME)=IM(K8)                                                  DK031530          
      K6=NCL(I)                                                         DK031540          
      DO 100 J=1,K6                                                     DK031550          
      K=NCAS+J                                                          DK031560          
      L=K1+J-1                                                          DK031570          
  100 XP(K)=IDEN(L)                                                     DK031580          
      NCAS=NCAS+NCL(I)                                                  DK031590          
C     ----------------------------------------------------              DK031600          
C     SETS UP LAB ARRAYS FOR LISTING LATER                              DK031610          
C     ----------------------------------------------------              DK031620          
   99 K=K1                                                              DK031630          
      K6=IM(K8)                                                         DK031640          
      DO 15 J=K5,K6                                                     DK031650          
      LAB1(J)=LIT(I)                                                    DK031660          
      CALL ITOA (IDEN(K),LAB2(J),4)                                     DK031670          
      LAB3(J)=BLANK                                                     DK031680          
      LAB4(J)=BLANK                                                     DK031690          
   15 K=K+1                                                             DK031700          
      K1=K1+NCL(I)                                                      DK031710          
      K2=K2+NCL(I)                                                      DK031720          
   11 K8=K8+1                                                           DK031730          
C     ----------------------------------------------------              DK031740          
C     READS AND LISTS PARAMETER CARDS FOR NESTED MAIN EFFECTS           DK031750          
C     ----------------------------------------------------              DK031760          
   12 K1=1                                                              DK031770          
      K2=0                                                              DK031780          
      IF (NNEA.EQ.0) GO TO 20                                           DK031790          
      DO 19 I=1,NNEA                                                    DK031800          
      READ  (5,1011)   NMA(I),NMAC(I),NEN(I),NLIT(I),K7,MPOL,LNE(I),NBEGDK031810          
     1(I),(NOS(J),J=1,K7)                                               DK031820          
 1011 FORMAT (3I2,A6,I2,2I1,I2,15I4,2X/(20I4))                          DK031830          
      WRITE (6,1012) I,NMA(I),NMAC(I),NEN(I),NLIT(I),K7,MPOL,LNE(I),NBEGDK031840          
     1(I)                                                               DK031850          
 1012 FORMAT (1H0,6HFOR I=,I4,66H  NMA(I)  NMAC(I)  NEN(I)  NLIT(I)  NCLDK031860          
     1N(I)  MPOL  LNE(I)  NBEG(I)/ 1H ,13X,I2,I9,I9,5X,A6,I5,I9,I6,I8)  DK031870          
      WRITE (6,1013) K1,K2,K7,(NOS(J),J=1,K7)                           DK031880          
 1013 FORMAT (1H0,16HNDEN(J) WHERE J=,I4,45H  INCREMENTED BY ONE UNTIL JDK031890          
     1 IS GREATER THAN ,I4,1H+,I4/(1H ,19(I4,2X)))                      DK031900          
      NCLN(I)=K7                                                        DK031910          
      DO 14 J=1,K7                                                      DK031920          
      K3=K1+J-1                                                         DK031930          
      NDEN(K3)=NOS(J)                                                   DK031940          
   14 NOS(J)=0                                                          DK031950          
      K1=K1+K7                                                          DK031960          
   19 K2=K2+K7                                                          DK031970          
   20 MLB=K2                                                            DK031980          
      NON=NNEA+NNE                                                      DK031990          
      IF (NNE.EQ.0) GO TO 25                                            DK032000          
      K3=NNEA+1                                                         DK032010          
      DO 24 I=K3,NON                                                    DK032020          
      READ  (5,1011)   NMA(I),NMAC(I),NEN(I),NLIT(I),K7,MPOL,LNE(I),NBEGDK032030          
     1(I),(NOS(J),J=1,K7)                                               DK032040          
      WRITE (6,1012) I,NMA(I),NMAC(I),NEN(I),NLIT(I),K7,MPOL,LNE(I),NBEGDK032050          
     1(I)                                                               DK032060          
      WRITE (6,1013) K1,K2,K7,(NOS(J),J=1,K7)                           DK032070          
      NCLN(I)=K7                                                        DK032080          
      DO 22 J=1,K7                                                      DK032090          
      K3=K1+J-1                                                         DK032100          
      NDEN(K3)=NOS(J)                                                   DK032110          
   22 NOS(J)=0                                                          DK032120          
      IF (K8.NE.1) GO TO 31                                             DK032130          
      IM(K8)=NCLN(I)-1                                                  DK032140          
      K5=1                                                              DK032150          
      GO TO 32                                                          DK032160          
   31 IM(K8)=NCLN(I)-1+IM(K8-1)                                         DK032170          
      K5=IM(K8-1)+1                                                     DK032180          
C     ----------------------------------------------------              DK032190          
C     SETS UP ARRAYS NEEDED WHEN POLYNOMIALS ARE TO BE FITTED           DK032200          
C     ----------------------------------------------------              DK032210          
   32 IF (MPOL.EQ.0) GO TO 98                                           DK032220          
      NSME=NSME+1                                                       DK032230          
      IPL(NSME)=NEN(I)                                                  DK032240          
      NSP(NSME)=K5                                                      DK032250          
      NND(NSME)=IM(K8)                                                  DK032260          
      K6=NCLN(I)                                                        DK032270          
      DO 101 J=1,K6                                                     DK032280          
      K=NCAS+J                                                          DK032290          
      L=K1+J-1                                                          DK032300          
  101 XP(K)=NDEN(L)                                                     DK032310          
      NCAS=NCAS+NCLN(I)                                                 DK032320          
C     ----------------------------------------------------              DK032330          
C     SETS UP LAB ARRAYS TO IDENTIFY LISTINGS                           DK032340          
C     ----------------------------------------------------              DK032350          
   98 K=K1                                                              DK032360          
      K6=IM(K8)                                                         DK032370          
      DO 23 J=K5,K6                                                     DK032380          
      LAB1(J)=NLIT(I)                                                   DK032390          
      CALL ITOA (NDEN(K),LAB2(J),4)                                     DK032400          
      LAB3(J)=BLANK                                                     DK032410          
      LAB4(J)=BLANK                                                     DK032420          
   23 K=K+1                                                             DK032430          
      K8=K8+1                                                           DK032440          
      K1=K1+K7                                                          DK032450          
   24 K2=K2+K7                                                          DK032460          
C     ----------------------------------------------------              DK032470          
C     READS AND LISTS PARAMETER CARDS FOR TWO-FACTOR INTERACTION EFFECTSDK032480          
C     ----------------------------------------------------              DK032490          
   25 K1=1                                                              DK032500          
      K2=0                                                              DK032510          
      IEI=IM(K8-1)+1                                                    DK032520          
      IF (K8.EQ.1) IEI=1                                                DK032530          
      IF (N2F.EQ.0) GO TO 50                                            DK032540          
      DO 49 I=1,N2F                                                     DK032550          
      READ  (5,1014)   INT1(I),INT2(I),K7,(NOS(J),J=1,K7)               DK032560          
 1014 FORMAT (3I2,16I4,2X/(18I4))                                       DK032570          
      WRITE (6,1015) I,INT1(I),INT2(I),K7                               DK032580          
 1015 FORMAT (1H0,6HFOR I=,I4,25H  INT1(I) INT2(I)  NMC(I)/ 1H ,12X,3(I2DK032590          
     1,6X))                                                             DK032600          
      IF (K7.NE.0) WRITE (6,1016) K1,K2,K7,(NOS(J),J=1,K7)              DK032610          
 1016 FORMAT (1H0,16HMSCL(J) WHERE J=,I4,45H  INCREMENTED BY ONE UNTIL JDK032620          
     1 IS GREATER THAN ,I4,1H+,I4/(1H ,I5,I9,12I8))                     DK032630          
      NMC(I)=K7                                                         DK032640          
      DO 26 J=1,K7                                                      DK032650          
      K3=K1+J-1                                                         DK032660          
      MSCL(K3)=NOS(J)                                                   DK032670          
   26 NOS(J)=0                                                          DK032680          
      NSUM=0                                                            DK032690          
      MSUM=0                                                            DK032700          
      IF (INT1(I).GT.NOM) GO TO 29                                      DK032710          
      K6=INT1(I)-1                                                      DK032720          
      IF (K6.EQ.0) GO TO 28                                             DK032730          
      DO 27 J=1,K6                                                      DK032740          
   27 NSUM=NSUM+NCL(J)                                                  DK032750          
   28 K6=INT1(I)                                                        DK032760          
      NDF1=NCL(K6)-1                                                    DK032770          
      GO TO 34                                                          DK032780          
   29 K6=INT1(I)-NOM                                                    DK032790          
      NDF1=NCLN(K6)-1                                                   DK032800          
      K3=NOM+1                                                          DK032810          
      K4=INT1(I)-1                                                      DK032820          
      IF (K4.LT.K3) GO TO 34                                            DK032830          
      DO 30 J=K3,K4                                                     DK032840          
      K=J-NOM                                                           DK032850          
   30 NSUM=NSUM+NCLN(K)                                                 DK032860          
   34 IF (INT2(I).GT.NOM) GO TO 38                                      DK032870          
      K4=INT2(I)-1                                                      DK032880          
      IF (K4.EQ.0) GO TO 36                                             DK032890          
      DO 35 J=1,K4                                                      DK032900          
   35 MSUM=MSUM+NCL(J)                                                  DK032910          
   36 K6=INT2(I)                                                        DK032920          
      NDF2=NCL(K6)-1                                                    DK032930          
      GO TO 41                                                          DK032940          
   38 K6=INT2(I)-NOM                                                    DK032950          
      NDF2=NCLN(K6)-1                                                   DK032960          
      K3=NOM+1                                                          DK032970          
      K4=INT2(I)-1                                                      DK032980          
      IF (K4.LT.K3) GO TO 41                                            DK032990          
      DO 40 J=K3,K4                                                     DK033000          
      K=J-NOM                                                           DK033010          
   40 MSUM=MSUM+NCLN(K)                                                 DK033020          
   41 IF (K8.NE.1) GO TO 52                                             DK033030          
      IM(K8)=NDF1*NDF2-NMC(I)                                           DK033040          
      J=1                                                               DK033050          
      GO TO 53                                                          DK033060          
   52 IM(K8)=IM(K8-1)+NDF1*NDF2-NMC(I)                                  DK033070          
      J=IM(K8-1)+1                                                      DK033080          
C     ----------------------------------------------------              DK033090          
C     SETS UP LAB ARRAYS TO IDENTIFY LISTINGS                           DK033100          
C     ----------------------------------------------------              DK033110          
   53 K8=K8+1                                                           DK033120          
      DO 48 K=1,NDF1                                                    DK033130          
      DO 48 L=1,NDF2                                                    DK033140          
      K6=NSUM+K                                                         DK033150          
      K7=MSUM+L                                                         DK033160          
      IF (NMC(I).EQ.0) GO TO 43                                         DK033170          
      K5=K*100+L                                                        DK033180          
      K3=0                                                              DK033190          
      K4=K1-1                                                           DK033200          
   42 K3=K3+1                                                           DK033210          
      K4=K4+1                                                           DK033220          
      IF (K3.GT.NMC(I)) GO TO 43                                        DK033230          
      IF (K5.NE.MSCL(K4)) GO TO 42                                      DK033240          
      GO TO 48                                                          DK033250          
   43 IF (INT1(I).GT.NOM) GO TO 44                                      DK033260          
      N=INT1(I)                                                         DK033270          
      LAB1(J)=LIT(N)                                                    DK033280          
      CALL ITOA (IDEN(K6),LAB3(J),4)                                    DK033290          
      GO TO 45                                                          DK033300          
   44 N=INT1(I)-NOM                                                     DK033310          
      LAB1(J)=NLIT(N)                                                   DK033320          
      CALL ITOA (NDEN(K6),LAB3(J),4)                                    DK033330          
   45 IF (INT2(I).GT.NOM) GO TO 46                                      DK033340          
      N=INT2(I)                                                         DK033350          
      LAB2(J)=LIT(N)                                                    DK033360          
      CALL ITOA (IDEN(K7),LAB4(J),4)                                    DK033370          
      GO TO 47                                                          DK033380          
   46 N=INT2(I)-NOM                                                     DK033390          
      LAB2(J)=NLIT(N)                                                   DK033400          
      CALL ITOA (NDEN(K7),LAB4(J),4)                                    DK033410          
   47 J=J+1                                                             DK033420          
   48 CONTINUE                                                          DK033430          
      K1=K1+NMC(I)                                                      DK033440          
   49 K2=K2+NMC(I)                                                      DK033450          
C     ----------------------------------------------------              DK033460          
C     READS AND LISTS PARAMETER CARDS FOR POOLED REGRESSIONS            DK033470          
C     ----------------------------------------------------              DK033480          
   50 IE=IM(K8-1)+1                                                     DK033490          
      IF (K8.EQ.1) IE=1                                                 DK033500          
      IF (NPR.EQ.0) GO TO 65                                            DK033510          
      DO 64 K=1,NPR                                                     DK033520          
      READ  (5,1017)   NEGX(K),LOGE(K),LQC(K),NREGP(K),LGTX(K),JBEG(K), DK033530          
     1NDECX(K),XM(K),LITR(K)                                            DK033540          
 1017 FORMAT (2I1,3I2,I3,I2,F10.5,A6)                                   DK033550          
      WRITE (6,1018) K,NEGX(K),LOGE(K),LQC(K),NREGP(K),LGTX(K),JBEG(K), DK033560          
     1NDECX(K),XM(K),LITR(K)                                            DK033570          
 1018 FORMAT (1H0,6HFOR K=,I2,72H  NEGX(K) LOGE(K) LQC(K) NREGP(K) LGTX(DK033580          
     1K) JBEG(K) NDECX(K) XM(K) LITR(K) /1H ,12X,I2,3I8,I7,2I9,2X,F10.5,DK033590          
     22X,A6)                                                            DK033600          
C     ----------------------------------------------------              DK033610          
C     SETS UP LAB ARRAYS TO IDENTIFY LISTINGS                           DK033620          
C     ----------------------------------------------------              DK033630          
      K3=LQC(K)                                                         DK033640          
      DO 63 J=1,K3                                                      DK033650          
      IM(K8)=IM(K8-1)+1                                                 DK033660          
      IF (K8.EQ.1) IM(K8)=1                                             DK033670          
      I=IM(K8)                                                          DK033680          
      K8=K8+1                                                           DK033690          
      LAB1(I)=RGRSN                                                     DK033700          
      LAB2(I)=LITR(K)                                                   DK033710          
      LAB4(I)=BLANK                                                     DK033720          
      IF (J-2) 60,61,62                                                 DK033730          
   60 LAB3(I)=LINEAR                                                    DK033740          
      GO TO 63                                                          DK033750          
   61 LAB3(I)=QUAD                                                      DK033760          
      GO TO 63                                                          DK033770          
   62 LAB3(I)=CUBIC                                                     DK033780          
   63 I=I+1                                                             DK033790          
   64 IF (LOGE(K).EQ.1) XM(K)=DLOG10(XM(K))                             DK033800          
C     ----------------------------------------------------              DK033810          
C     READS AND LISTS PARAMETER CARDS FOR RHM                           DK033820          
C     ----------------------------------------------------              DK033830          
   65 IF (NRHM.EQ.0) GO TO 67                                           DK033840          
      DO 66 I=1,NRHM                                                    DK033850          
      READ  (5,1019)   NEGY(I),LNY(I),LHY(I),KBEG(I),NDECY(I),YM(I),LITYDK033860          
     1(I)                                                               DK033870          
 1019 FORMAT (2I1,I2,I3,I2,F10.5,A6)                                    DK033880          
      WRITE (6,1020) I,NEGY(I),LNY(I),LHY(I),KBEG(I),NDECY(I),YM(I),LITYDK033890          
     1(I)                                                               DK033900          
 1020 FORMAT (1H0,6HFOR I=,I2,54H  NEGY(I) LNY(I) LHY(I) KBEG(I) NDECY(IDK033910          
     1) YM(I) LITY(I)/ 1H ,12X,I2,2I7,2I8,2X,F10.5,1X,A6)               DK033920          
   66 IF (LNY(I).EQ.1) YM(I)=DLOG10(YM(I))                              DK033930          
   67 IF (IRAN.EQ.0) GO TO 68                                           DK033940          
      K=1                                                               DK033950          
      DO 76 J=1,IRAN                                                    DK033960          
      K3=101-J                                                          DK033970          
      READ (5,1023)  LAB4(K3),I309(J),NR1(J),NW(J),L,(NOS(I),I=1,L)     DK033980          
 1023 FORMAT (A6,6I2)                                                   DK033990          
      WRITE (6,1021) LAB4(K3),I309(J),NR1(J),NW(J),L,(NOS(I),I=1,L)     DK034000          
 1021 FORMAT (1H0,"CLASSIFICATION  I309 NR1   NW  NS2  MS(1) MS(2)"/(1H DK034010          
     1,4X,A6,5X,6I5))                                                   DK034020          
      IF (I309(J).EQ.1) NCPR=1                                          DK034030          
      NS2(J)=L                                                          DK034040          
      DO 77 M=1,L                                                       DK034050          
      K1=K+M-1                                                          DK034060          
      MS(K1)=NOS(M)                                                     DK034070          
   77 NOS(M)=0                                                          DK034080          
   76 K=K+L                                                             DK034090          
   68 CONTINUE                                                          DK034100          
      IF (N2F.EQ.0) GO TO 75                                            DK034110          
C     ----------------------------------------------------              DK034120          
C     CHECKS FOR CONSECUTIVE NUMBERING OF ALL MAIN EFFECTS              DK034130          
C     ----------------------------------------------------              DK034140          
      J=1                                                               DK034150          
      IF (NOM.EQ.0) GO TO 70                                            DK034160          
      DO 69 I=1,NOM                                                     DK034170          
      IF (MEN(I).NE.J) GO TO 900                                        DK034180          
   69 J=J+1                                                             DK034190          
   70 IF (NON.EQ.0) GO TO 75                                            DK034200          
      DO 71 I=1,NON                                                     DK034210          
      IF (NEN(I).NE.J) GO TO 900                                        DK034220          
   71 J=J+1                                                             DK034230          
      GO TO 75                                                          DK034240          
  900 WRITE (6,1022) IJOB                                               DK034250          
 1022 FORMAT (1H0,73HCODES FOR EFFECTS NOT CONSECUTIVE---CHECK PARAMETERDK034260          
     1 CARDS FOR PROBLEM NO.,I3)                                        DK034270          
      MULL=1                                                            DK034280          
   75 CONTINUE                                                          DK034290          
      NS=K8-1                                                           DK034300          
      RETURN                                                            DK034310          
      END                                                               DK034320          
*ENDTEXT                                                                                  
