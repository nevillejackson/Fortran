      SUBROUTINE PARAM                                                  DK030010          
C     -------------------------------------------                       DK030020          
C     SUBROUTINE WHICH READS AND LISTS PARAMETER CARDS, AND SETS UP     DK030030          
C     ARRAYS FOR LABELING OUTPUT                                        DK030040          
C     ----------------------------------------------------              DK030050          
      EXTERNAL DSQRT,DLOG10                                                               
      DIMENSION ARRAY(5000),SSCPR(630),SSS(630),RHM(2450),TOT(106),TOT2(DK030080          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100),NS2(15)    DK030090          
      DIMENSION FAB(5000),FY(630),TOT4(106),SAB(630),FSQ(525),TOT5(106) DK030100          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),MZ(10),IPL(13),NSDK030110          
     1P(13),NND(13),XP(81),YP(41),      NDC(10),NMI(10),MEN(20),NCL(20),DK030120          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(50),NCLN(50),LNE(50),NDEN(1                  
     300),NLIT(50),NMA(50),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(50),                  
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDK030150          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),                          
     6EFF1(50),EFF2(50),NOS(300),X(106),NMAC(50),I309(15),NR1(15),NW(15)                  
     7,R1I(100),R2I(100),WW(200),ICLR(90),IRM(40),LAD(90)               DK030180          
      DIMENSION IC(1000)                                                                  
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADK030190          
     1B4,LITY,TRED,YM,XP,YP,IM,MS,NEG,IPL,NSP,NND,FAB,FY,TOT4,SAB,FSQ,TODK030200          
     2T5,MZ,R1I,R2I,WW,ICLR,IRM,LAD                                     DK030210          
      COMMON /CMBLK2/LIT,LITR,NLIT,XM,EFF1,EFF2,X,NDC,NMI,MEN,NCL,LME,IBDK030220          
     1EG,IDEN,NCLN,NEN,LNE,NBEG,NDEN,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQDK030230          
     2C,NREGP,LGTX,JBEG,NDECX,NEGY,LNY,LHY,KBEG,NDECY,IC,NOS,NMAC       DK030240          
      COMMON /CMBLK3/RR,REP,DF,SDF,EDF,WK,IJOB,NAB,NLHM,NRHM,NMEA,NME,NNDK030250          
     1EA,NNE,N2F,NPR,IRAN,MPOP,LIOP,IAD,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,DK030260          
     2NOM,NON,ML,MLB,NS,NOT,KPUT,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,IN,I30DK030270          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DK030280          
     1L,L7,KA,KB,KC,SOF,NX,KD,MTY,MAN,NRUN,NRN,EDFF,NAME,EDFS,NAM5,IFP,NDK030290          
     2DFA,IFPC,NDFC,NDFAC,NINT,IBET,LBEG,LTHN,NNDC,WTT,SWT1             DK030300          
      INTEGER BLANK,CUBIC,MU,QUAD,RGRSN,LINEAR,RGNL,RGNQ,RGNC                             
      DATA BLANK,CUBIC,MU,QUAD,RGRSN,LINEAR/6H      ,6HCUBIC ,6HMU    , DK030310          
     16HQUAD  ,6HRGRSN ,6HLINEAR/,RGNL,RGNQ,RGNC/6H RGN L,6H RGN Q,6H RGDK030320          
     2N C/                                                              DK030330          
C     ----------------------------------------------------              DK030340          
C     READS AND LISTS PARAMETER CARD 1.                                 DK030350          
C     ----------------------------------------------------              DK030360          
      WRITE(6,2000)                                                     DK030370          
 2000 FORMAT(100H1MIXED MODEL LEAST SQUARES AND MAXIMUM LIKELIHOOD COMPU                  
     1TER PROGRAM -- W.R.HARVEY -- OHIO STATE UNIV.)                                      
    1 READ  (5,1000) IJOB,NAB,NCD,ICN1,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F, DK030420          
     1NPR,NLC,NCPR,IRAN,MPOP,LIOP,IN,MTY,MAN,NRUN,NRN,IBET,LBEG,LTHN,NNDDK030430          
     2C                                                                 DK030440          
 1000 FORMAT (23I2,I3,2I2)                                              DK030450          
      IF (NRN.EQ.1) MULL=0                                              DK030460          
      IF (NRUN.NE.NRN) MPOP=1                                           DK030470          
      IPL(1)=0                                                          DK030480          
      IF (IN.EQ.0) IN=5                                                 DK030490          
      WRITE (6,1001) IJOB                                               DK030500          
 1001 FORMAT (1H0,20X,42HLISTING OF PARAMETER CARDS FOR PROBLEM NO.,I3) DK030510          
      WRITE (6,1002) IJOB,NAB,NCD,ICN1,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F, DK030520          
     1NPR,NLC,NCPR,IRAN,MPOP,LIOP,IN,MTY,MAN,NRUN,NRN,IBET,LBEG,LTHN,NNDDK030530          
     2C                                                                 DK030540          
 1002 FORMAT (1H0,128HIJOB NAB  NCD  ICN1 NLHM NRHM NMEA NME NNEA  NNE  DK030550          
     1N2F  NPR  NLC NCPR IRAN MPOP LIOP   IN  MTY  MAN NRUN  NRN IBET IBDK030560          
     2EG LTHN NNDC/1H ,I3,I4,24I5)                                                        
      NTOTE=(NLHM*(NLHM+1))/2+(NLHM*NRHM)                               DK030580          
      IF (NTOTE.GT.5000) GO TO 901                                      DK030590          
      IF ((NAB.EQ.0.OR.NAB.EQ.3.OR.NAB.EQ.4).AND.MTY.GT.1) GO TO 902    DK030600          
      IF (NAB.GT.6) GO TO 902                                           DK030610          
      IF (NME.GT.20) GO TO 903                                          DK030620          
      IF(NNE.GT.50) GO TO 903                                                             
      NTOTE=NME+NMEA                                                    DK030640          
      IF(NTOTE.GT.20) GO TO 903                                                           
      NTOTE=NNE+NNEA                                                    DK030660          
      IF(NTOTE.GT.50) GO TO 903                                                           
      IF (N2F.GT.30) GO TO 903                                          DK030680          
      IF (NPR.GT.90) GO TO 903                                          DK030690          
      LINOUT=NME+NNE+N2F+NPR                                            DK030700          
      IF (LINOUT.GT.90) GO TO 908                                       DK030710          
C     ----------------------------------------------------              DK030720          
C     READS CONTROL IDENTIFICATION FROM PARAMETER CARDS IF ABSORPTION   DK030730          
C                   IS TO OCCUR                                         DK030740          
C     ----------------------------------------------------              DK030750          
      IF (MTY.GT.1.OR.IRAN.GT.0) NCPR=1                                 DK030760          
      K8=1                                                              DK030770          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4.OR.NAB.EQ.5) GO TO 4         DK030780          
      IM(1)=1                                                           DK030790          
      LAB1(1)=MU                                                        DK030800          
      LAB2(1)=BLANK                                                     DK030810          
      LAB3(1)=BLANK                                                     DK030820          
      LAB4(1)=BLANK                                                     DK030830          
      K8=2                                                              DK030840          
    4 IF (NAB.EQ.0) GO TO 13                                            DK030850          
      GO TO (13,16,5,5,3,2),NAB                                         DK030860          
    2 READ  (5,999) NAME,IAD,NR1(14),NW(14),NCC,(NDC(I),I=1,NCC)        DK030870          
  999 FORMAT (A6,36I2)                                                  DK030880          
      WRITE (6,1003) NAME,IAD,NR1(14),NW(14),NCC,(NDC(I),I=1,NCC)       DK030890          
 1003 FORMAT (1H0,54H NAME   IAD  NR1   NW  NCC  NDC(I) WHERE I=1,2,...NDK030900          
     1CC./1H ,A6,24(3X,I2))                                             DK030910          
      IF (NCC.GT.10) GO TO 904                                          DK030920          
      GO TO 13                                                          DK030930          
    5 READ  (5,1004) IAD,REP,NMJC,(NDC(I),I=1,NMJC),NMIC,(NMI(I),I=1,NMIDK030940          
     1C)                                                                DK030950          
      WRITE (6,1005) IAD,REP,NMJC,(NDC(I),I=1,NMJC)                     DK030960          
 1004 FORMAT (I2,F8.6,31I2)                                             DK030970          
 1005 FORMAT (1H0,43HIAD    REP   NCC  NDC(I) WHERE I=1,2,...NCC/1H ,   DK030980          
     1I2,2X,F8.6,I4,20I5)                                               DK030990          
      IF (NMJC.GT.10) GO TO 904                                         DK031000          
      IF (NAB.EQ.3) NCC=NMJC                                            DK031010          
      RR=REP                                                            DK031020          
      REP=(1.-REP)/REP                                                  DK031030          
      IF (NAB.EQ.4) GO TO 6                                             DK031040          
      GO TO 13                                                          DK031050          
   16 READ (5,999) NAME,IAD,NR1(14),NW(14),NCC,(NDC(I),I=1,NCC),NINT,   DK031060          
     1(MZ(I),I=1,NINT),NMIC,(NMI(I),I=1,NMIC)                           DK031070          
      WRITE (6,1024) NAME,IAD,NR1(14),NW(14),NCC,(NDC(I),I=1,NCC)       DK031080          
 1024 FORMAT (1H0,57H NAME   IAD  NR1   NW  NMJC  NDC(I) WHERE I=1,2,...DK031090          
     1NMJC. /1H ,A6,24(3X,I2))                                          DK031100          
      WRITE (6,1025) NINT,(MZ(I),I=1,NINT)                              DK031110          
 1025 FORMAT (1H0,23X,33HNINT  MZ(I)  WHERE I=1,2,...NINT./1H ,21X,12(3XDK031120          
     1,I2))                                                             DK031130          
      WRITE (6,1026) NMIC,(NMI(I),I=1,NMIC)                             DK031140          
 1026 FORMAT (1H0,23X,33HNMIC  NMI(I) WHERE I=1,2,...NMIC./1H ,21X,12(3XDK031150          
     1,I2))                                                             DK031160          
      IF (NCC.GT.10.OR.NINT.GT.10.OR.NMIC.GT.10) GO TO 904              DK031170          
      GO TO 13                                                          DK031180          
    3 READ (5,999)  NAM5,IAD,NR1(15),NW(15),NMJC,(NDC(I),I=1,NMJC),NMIC,DK031190          
     1(NMI(I),I=1,NMIC)                                                 DK031200          
      WRITE (6,1006) NAM5,IAD,NR1(15),NW(15),NMJC,(NDC(I),I=1,NMJC)     DK031210          
 1006 FORMAT (1H0,57H NAM5   IAD  NR1   NW  NMJC  NDC(I) WHERE I=1,2,...DK031220          
     1NMJC. /1H ,A6,24(3X,I2))                                          DK031230          
    6 WRITE (6,1007) NMIC,(NMI(I),I=1,NMIC)                             DK031240          
 1007 FORMAT (1H0,23X,33HNMIC  NMI(I) WHERE I=1,2,...NMIC. /1H ,24X,15(IDK031250          
     12,3X))                                                            DK031260          
      IF (NMJC.GT.10.OR.NMIC.GT.10) GO TO 904                           DK031270          
      TOT4(102)=IAD                                                     DK031280          
      LGT=10**NMIC                                                      DK031290          
C     ----------------------------------------------------              DK031300          
C     READS AND LISTS PARAMETER CARDS FOR MAIN EFFECTS                  DK031310          
C     ----------------------------------------------------              DK031320          
   13 K1=1                                                              DK031330          
      K2=0                                                              DK031340          
      IF (NMEA.EQ.0) GO TO 10                                           DK031350          
      DO 8 I=1,NMEA                                                     DK031360          
      READ  (5,1008)   MEN(I),LIT(I),K7,MPOL,LME(I),IBEG(I),(NOS(J),J=1,DK031370          
     1K7)                                                               DK031380          
 1008 FORMAT (I2,A6,I2,2I1,I2,16I4,2X/(20I4))                           DK031390          
      WRITE (6,1009) I,MEN(I),LIT(I),K7,MPOL,LME(I),IBEG(I)             DK031400          
 1009 FORMAT (1H0,6HFOR I=,I4,47H  MEN(I)  LIT(I)  NCL(I)  MPOL  LME(I) DK031410          
     1 IBEG(I)/1H ,12X,I2,6X,A6,2X,I2,6X,I2,4X,I2,7X,I2)                DK031420          
      NCL(I)=K7                                                         DK031430          
      WRITE (6,1010) (NOS(J),J=1,K7)                                    DK031440          
 1010 FORMAT (1H0,72HIDEN(J) WHERE J=K2 BY STEPS OF ONE AS LONG AS J IS DK031450          
     1LESS THAN K2+NCL(I)+1/(1H ,I5,5X,I4,3X,20I5))                     DK031460          
      DO 7 J=1,K7                                                       DK031470          
      K3=K1+J-1                                                         DK031480          
      IDEN(K3)=NOS(J)                                                   DK031490          
    7 NOS(J)=0                                                          DK031500          
      K1=K1+K7                                                          DK031510          
    8 K2=K2+K7                                                          DK031520          
   10 ML=K2                                                             DK031530          
      NOM=NME+NMEA                                                      DK031540          
      IF (NME.EQ.0) GO TO 12                                            DK031550          
      K3=NMEA+1                                                         DK031560          
      DO 11 I=K3,NOM                                                    DK031570          
      READ  (5,1008)   MEN(I),LIT(I),K7,MPOL,LME(I),IBEG(I),(NOS(J),J=1,DK031580          
     1K7)                                                               DK031590          
      WRITE (6,1009) I,MEN(I),LIT(I),K7,MPOL,LME(I),IBEG(I)             DK031600          
      WRITE (6,1010) (NOS(J),J=1,K7)                                    DK031610          
      NCL(I)=K7                                                         DK031620          
      DO 9 J=1,K7                                                       DK031630          
      K5=K1+J-1                                                         DK031640          
      IDEN(K5)=NOS(J)                                                   DK031650          
    9 NOS(J)=0                                                          DK031660          
      IF (K8.NE.1) GO TO 17                                             DK031670          
      IM(K8)=NCL(I)-1                                                   DK031680          
      K5=1                                                              DK031690          
      GO TO 18                                                          DK031700          
   17 IM(K8)=NCL(I)-1+IM(K8-1)                                          DK031710          
      K5=IM(K8-1)+1                                                     DK031720          
C     ----------------------------------------------------              DK031730          
C     SETS UP BEGINNING AND ENDING ROW NUMBERS FOR MAIN EFFECTS FOR     DK031740          
C     WHICH POLYNOMIALS ARE TO BE FITTED                                DK031750          
C     ----------------------------------------------------              DK031760          
   18 IF (MPOL.EQ.0) GO TO 99                                           DK031770          
      IF (NSME.GT.12) GO TO 905                                         DK031780          
      NSME=NSME+1                                                       DK031790          
      IPL(NSME)=MEN(I)                                                  DK031800          
      NSP(NSME)=K5                                                      DK031810          
      NND(NSME)=IM(K8)                                                  DK031820          
      K6=NCL(I)                                                         DK031830          
      DO 100 J=1,K6                                                     DK031840          
      K=NCAS+J                                                          DK031850          
      L=K1+J-1                                                          DK031860          
  100 XP(K)=IDEN(L)                                                     DK031870          
      NCAS=NCAS+NCL(I)                                                  DK031880          
      IF (NCAS.GT.80) GO TO 906                                         DK031890          
C     ----------------------------------------------------              DK031900          
C     SETS UP LAB ARRAYS FOR LISTING LATER                              DK031910          
C     ----------------------------------------------------              DK031920          
   99 K=K1                                                              DK031930          
      K6=IM(K8)                                                         DK031940          
      DO 15 J=K5,K6                                                     DK031950          
      LAB1(J)=LIT(I)                                                    DK031960          
      CALL ITOA (IDEN(K),LAB2(J))                                       DK031970          
      LAB3(J)=BLANK                                                     DK031980          
      LAB4(J)=BLANK                                                     DK031990          
   15 K=K+1                                                             DK032000          
      K1=K1+NCL(I)                                                      DK032010          
      K2=K2+NCL(I)                                                      DK032020          
   11 K8=K8+1                                                           DK032030          
C     ----------------------------------------------------              DK032040          
C     READS AND LISTS PARAMETER CARDS FOR NESTED MAIN EFFECTS           DK032050          
C     ----------------------------------------------------              DK032060          
   12 NSC=K2-ML                                                         DK032070          
      MN2=NSC                                                           DK032080          
      K1=1                                                              DK032090          
      K2=0                                                              DK032100          
      IF (NNEA.EQ.0) GO TO 20                                           DK032110          
      DO 19 I=1,NNEA                                                    DK032120          
      READ  (5,1011)   NMA(I),NMAC(I),NEN(I),NLIT(I),K7,MPOL,LNE(I),NBEGDK032130          
     1(I),(NOS(J),J=1,K7)                                               DK032140          
 1011 FORMAT (3I2,A6,I2,2I1,I2,15I4,2X/(20I4))                          DK032150          
      WRITE (6,1012) I,NMA(I),NMAC(I),NEN(I),NLIT(I),K7,MPOL,LNE(I),NBEGDK032160          
     1(I)                                                               DK032170          
 1012 FORMAT (1H0,6HFOR I=,I4,66H  NMA(I)  NMAC(I)  NEN(I)  NLIT(I)  NCLDK032180          
     1N(I)  MPOL  LNE(I)  NBEG(I)/ 1H ,13X,I2,I9,I9,5X,A6,I5,I9,I6,I8)  DK032190          
      WRITE (6,1013) K1,K2,K7,(NOS(J),J=1,K7)                           DK032200          
 1013 FORMAT (1H0,16HNDEN(J) WHERE J=,I4,45H  INCREMENTED BY ONE UNTIL JDK032210          
     1 IS GREATER THAN ,I4,1H+,I4/(1H ,19(I4,2X)))                      DK032220          
      NCLN(I)=K7                                                        DK032230          
      DO 14 J=1,K7                                                      DK032240          
      K3=K1+J-1                                                         DK032250          
      NDEN(K3)=NOS(J)                                                   DK032260          
   14 NOS(J)=0                                                          DK032270          
      K1=K1+K7                                                          DK032280          
   19 K2=K2+K7                                                          DK032290          
   20 MLB=K2                                                            DK032300          
      NON=NNEA+NNE                                                      DK032310          
      IF (NNE.EQ.0) GO TO 25                                            DK032320          
      K3=NNEA+1                                                         DK032330          
      DO 24 I=K3,NON                                                    DK032340          
      READ  (5,1011)   NMA(I),NMAC(I),NEN(I),NLIT(I),K7,MPOL,LNE(I),NBEGDK032350          
     1(I),(NOS(J),J=1,K7)                                               DK032360          
      WRITE (6,1012) I,NMA(I),NMAC(I),NEN(I),NLIT(I),K7,MPOL,LNE(I),NBEGDK032370          
     1(I)                                                               DK032380          
      WRITE (6,1013) K1,K2,K7,(NOS(J),J=1,K7)                           DK032390          
      NCLN(I)=K7                                                        DK032400          
      DO 22 J=1,K7                                                      DK032410          
      K5=K1+J-1                                                         DK032420          
      NDEN(K5)=NOS(J)                                                   DK032430          
   22 NOS(J)=0                                                          DK032440          
      IF (K8.NE.1) GO TO 31                                             DK032450          
      IM(K8)=NCLN(I)-1                                                  DK032460          
      K5=1                                                              DK032470          
      GO TO 32                                                          DK032480          
   31 IM(K8)=NCLN(I)-1+IM(K8-1)                                         DK032490          
      K5=IM(K8-1)+1                                                     DK032500          
C     ----------------------------------------------------              DK032510          
C     SETS UP ARRAYS NEEDED WHEN POLYNOMIALS ARE TO BE FITTED           DK032520          
C     ----------------------------------------------------              DK032530          
   32 IF (MPOL.EQ.0) GO TO 98                                           DK032540          
      NSME=NSME+1                                                       DK032550          
      IF (NSME.GT.12) GO TO 905                                         DK032560          
      IPL(NSME)=NEN(I)                                                  DK032570          
      NSP(NSME)=K5                                                      DK032580          
      NND(NSME)=IM(K8)                                                  DK032590          
      K6=NCLN(I)                                                        DK032600          
      DO 101 J=1,K6                                                     DK032610          
      K=NCAS+J                                                          DK032620          
      L=K1+J-1                                                          DK032630          
  101 XP(K)=NDEN(L)                                                     DK032640          
      NCAS=NCAS+NCLN(I)                                                 DK032650          
      IF (NCAS.GT.80) GO TO 906                                         DK032660          
C     ----------------------------------------------------              DK032670          
C     SETS UP LAB ARRAYS TO IDENTIFY LISTINGS                           DK032680          
C     ----------------------------------------------------              DK032690          
   98 K=K1                                                              DK032700          
      K6=IM(K8)                                                         DK032710          
      DO 23 J=K5,K6                                                     DK032720          
      LAB1(J)=NLIT(I)                                                   DK032730          
      CALL ITOA (NDEN(K),LAB2(J))                                       DK032740          
      LAB3(J)=BLANK                                                     DK032750          
      LAB4(J)=BLANK                                                     DK032760          
   23 K=K+1                                                             DK032770          
      K8=K8+1                                                           DK032780          
      K1=K1+K7                                                          DK032790          
   24 K2=K2+K7                                                          DK032800          
C     ----------------------------------------------------              DK032810          
C     READS AND LISTS PARAMETER CARDS FOR TWO-FACTOR INTERACTION EFFECTSDK032820          
C     ----------------------------------------------------              DK032830          
   25 NSC=NSC+K2-MLB                                                    DK032840          
      K1=1                                                              DK032850          
      K2=0                                                              DK032860          
      IEI=IM(K8-1)+1                                                    DK032870          
      IF (K8.EQ.1) IEI=1                                                DK032880          
      IF (N2F.EQ.0) GO TO 50                                            DK032890          
      DO 49 I=1,N2F                                                     DK032900          
      READ  (5,1014)   INT1(I),INT2(I),K7,(NOS(J),J=1,K7)               DK032910          
 1014 FORMAT (3I2,16I4,2X/(18I4))                                       DK032920          
      WRITE (6,1015) I,INT1(I),INT2(I),K7                               DK032930          
 1015 FORMAT (1H0,6HFOR I=,I4,25H  INT1(I) INT2(I)  NMC(I)/ 1H ,12X,3(I2DK032940          
     1,6X))                                                             DK032950          
      IF (K7.NE.0) WRITE (6,1016) K1,K2,K7,(NOS(J),J=1,K7)              DK032960          
 1016 FORMAT (1H0,16HMSCL(J) WHERE J=,I4,45H  INCREMENTED BY ONE UNTIL JDK032970          
     1 IS GREATER THAN ,I4,1H+,I4/(1H ,I5,I9,12I8))                     DK032980          
      NMC(I)=K7                                                         DK032990          
      DO 26 J=1,K7                                                      DK033000          
      K3=K1+J-1                                                         DK033010          
      MSCL(K3)=NOS(J)                                                   DK033020          
   26 NOS(J)=0                                                          DK033030          
      NSUM=0                                                            DK033040          
      MSUM=0                                                            DK033050          
      IF (INT1(I).GT.NOM) GO TO 29                                      DK033060          
      K6=INT1(I)-1                                                      DK033070          
      IF (K6.EQ.0) GO TO 28                                             DK033080          
      DO 27 J=1,K6                                                      DK033090          
   27 NSUM=NSUM+NCL(J)                                                  DK033100          
   28 K6=INT1(I)                                                        DK033110          
      NDF1=NCL(K6)-1                                                    DK033120          
      GO TO 34                                                          DK033130          
   29 K6=INT1(I)-NOM                                                    DK033140          
      NDF1=NCLN(K6)-1                                                   DK033150          
      K3=NOM+1                                                          DK033160          
      K4=INT1(I)-1                                                      DK033170          
      IF (K4.LT.K3) GO TO 34                                            DK033180          
      DO 30 J=K3,K4                                                     DK033190          
      K=J-NOM                                                           DK033200          
   30 NSUM=NSUM+NCLN(K)                                                 DK033210          
   34 IF (INT2(I).GT.NOM) GO TO 38                                      DK033220          
      K4=INT2(I)-1                                                      DK033230          
      IF (K4.EQ.0) GO TO 36                                             DK033240          
      DO 35 J=1,K4                                                      DK033250          
   35 MSUM=MSUM+NCL(J)                                                  DK033260          
   36 K6=INT2(I)                                                        DK033270          
      NDF2=NCL(K6)-1                                                    DK033280          
      GO TO 41                                                          DK033290          
   38 K6=INT2(I)-NOM                                                    DK033300          
      NDF2=NCLN(K6)-1                                                   DK033310          
      K3=NOM+1                                                          DK033320          
      K4=INT2(I)-1                                                      DK033330          
      IF (K4.LT.K3) GO TO 41                                            DK033340          
      DO 40 J=K3,K4                                                     DK033350          
      K=J-NOM                                                           DK033360          
   40 MSUM=MSUM+NCLN(K)                                                 DK033370          
   41 IF (K8.NE.1) GO TO 52                                             DK033380          
      IM(K8)=NDF1*NDF2-NMC(I)                                           DK033390          
      J=1                                                               DK033400          
      GO TO 53                                                          DK033410          
   52 IM(K8)=IM(K8-1)+NDF1*NDF2-NMC(I)                                  DK033420          
      J=IM(K8-1)+1                                                      DK033430          
C     ----------------------------------------------------              DK033440          
C     SETS UP LAB ARRAYS TO IDENTIFY LISTINGS                           DK033450          
C     ----------------------------------------------------              DK033460          
   53 K8=K8+1                                                           DK033470          
      DO 48 K=1,NDF1                                                    DK033480          
      DO 48 L=1,NDF2                                                    DK033490          
      K6=NSUM+K                                                         DK033500          
      K7=MSUM+L                                                         DK033510          
      IF (NMC(I).EQ.0) GO TO 43                                         DK033520          
      K5=K*100+L                                                        DK033530          
      K3=0                                                              DK033540          
      K4=K1-1                                                           DK033550          
   42 K3=K3+1                                                           DK033560          
      K4=K4+1                                                           DK033570          
      IF (K3.GT.NMC(I)) GO TO 43                                        DK033580          
      IF (K5.NE.MSCL(K4)) GO TO 42                                      DK033590          
      GO TO 48                                                          DK033600          
   43 IF (INT1(I).GT.NOM) GO TO 44                                      DK033610          
      N=INT1(I)                                                         DK033620          
      LAB1(J)=LIT(N)                                                    DK033630          
      CALL ITOA (IDEN(K6),LAB3(J))                                      DK033640          
      GO TO 45                                                          DK033650          
   44 N=INT1(I)-NOM                                                     DK033660          
      LAB1(J)=NLIT(N)                                                   DK033670          
      CALL ITOA (NDEN(K6),LAB3(J))                                      DK033680          
   45 IF (INT2(I).GT.NOM) GO TO 46                                      DK033690          
      N=INT2(I)                                                         DK033700          
      LAB2(J)=LIT(N)                                                    DK033710          
      CALL ITOA (IDEN(K7),LAB4(J))                                      DK033720          
      GO TO 47                                                          DK033730          
   46 N=INT2(I)-NOM                                                     DK033740          
      LAB2(J)=NLIT(N)                                                   DK033750          
      CALL ITOA (NDEN(K7),LAB4(J))                                      DK033760          
   47 J=J+1                                                             DK033770          
   48 CONTINUE                                                          DK033780          
      K1=K1+NMC(I)                                                      DK033790          
      NSC=NSC+(NDF1+1)*(NDF2+1)                                         DK033800          
   49 K2=K2+NMC(I)                                                      DK033810          
C     ----------------------------------------------------              DK033820          
C     READS AND LISTS PARAMETER CARDS FOR REGRESSIONS                   DK033830          
C     ----------------------------------------------------              DK033840          
   50 DO 51 I=1,NSC                                                     DK033850          
      WW(I)=0.0                                                         DK033860          
   51 NOS(I)=0                                                          DK033870          
      IE=IM(K8-1)+1                                                     DK033880          
      IF (K8.EQ.1) IE=1                                                 DK033890          
      IF (NPR.EQ.0) GO TO 65                                            DK033900          
      NTOTE=0                                                           DK033910          
      K9=1                                                              DK033920          
      DO 64 K=1,NPR                                                     DK033930          
      READ (5,1017) NEGX(K),LOGE(K),LQC(K),K4,LGTX(K),JBEG(K),NDECX(K),XDK033940          
     1M(K),LITR(K),K2,LAD(K),(MS(J),J=1,K2)                             DK033950          
 1017 FORMAT (2I1,3I2,I3,I2,F10.5,A6,25I2/17I2)                         DK033960          
      IF(LQC(K).GT.1.AND.LAD(K).EQ.0) GO TO 909                                           
      IF(NAB.EQ.3.AND.LAD(K).EQ.0) GO TO 910                                              
   54 IF(NRN.GT.1.AND.LAD(K).EQ.3.AND.IN.NE.5) XM(K)=X(K)                                 
      WRITE (6,1018) K,NEGX(K),LOGE(K),LQC(K),K4,LGTX(K),JBEG(K),NDECX(KDK033980          
     1),XM(K),LITR(K),K2,LAD(K)                                         DK033990          
 1018 FORMAT (1H0,6HFOR K=,I2,90H  NEGX(K) LOGE(K) LQC(K) NREGP(K) LGTX(DK034000          
     1K) JBEG(K) NDECX(K) XM(K)    LITR(K) ICLR(K) LAD(K)/1H ,12X,      DK034010          
     2I2,3I8,I7,2I9,2X,F10.5,2X,A6,2X,I3,5X,I2)                         DK034020          
      IF (K2.GT.0) WRITE (6,2018) (MS(J),J=1,K2)                        DK034030          
 2018 FORMAT (1H0,6HIRM(I)/1H ,40I3)                                    DK034040          
      ICLR(K)=K2                                                        DK034050          
      IF (K2.EQ.0) GO TO 108                                            DK034060          
      NTOTE=NTOTE+K2                                                    DK034070          
      IF (NTOTE.GT.40) GO TO 907                                        DK034080          
      LINOUT=LINOUT+K2                                                  DK034090          
      IF (LINOUT.GT.90) GO TO 908                                       DK034100          
      DO 110 I=1,K2                                                     DK034110          
      IRM(K9)=MS(I)                                                     DK034120          
      LAD(50+K9)=0                                                      DK034130          
      ICLR(50+K9)=0                                                     DK034140          
      IF (MS(I).LE.NOM) GO TO 110                                       DK034150          
      J=MS(I)-NOM                                                       DK034160          
      DO 119 L=1,K2                                                     DK034170          
      IF (MS(L).NE.NMA(J)) GO TO 119                                    DK034180          
      LAD(50+K9)=NMA(J)                                                 DK034190          
      ICLR(50+K9)=NMAC(J)                                               DK034200          
      N=MS(L)                                                           DK034210          
      IF (NCL(N).EQ.NMAC(J)) ICLR(50+K9)=99                             DK034220          
  119 CONTINUE                                                          DK034230          
  110 K9=K9+1                                                           DK034240          
C     ----------------------------------------------------              DK034250          
C     SETS UP LAB ARRAYS TO IDENTIFY LISTINGS                           DK034260          
C     ----------------------------------------------------              DK034270          
      DO 102 J=1,K2                                                     DK034280          
      NW(J)=1                                                           DK034290          
      N=MS(J)                                                           DK034300          
      IF (N.GT.NOM) GO TO 103                                           DK034310          
      NR1(J)=N-1                                                        DK034320          
      IF (NR1(J).EQ.0) GO TO 104                                        DK034330          
      NS=NR1(J)                                                         DK034340          
      DO 105 L=1,NS                                                     DK034350          
  105 NW(J)=NW(J)+NCL(L)                                                DK034360          
  104 NR1(J)=NCL(N)                                                     DK034370          
      GO TO 102                                                         DK034380          
  103 NS=N-NOM-1                                                        DK034390          
      IF (NS.EQ.0) GO TO 107                                            DK034400          
      DO 106 L=1,NS                                                     DK034410          
  106 NW(J)=NW(J)+NCLN(L)                                               DK034420          
  107 NR1(J)=NCLN(NS+1)                                                 DK034430          
  102 CONTINUE                                                          DK034440          
  108 K3=LQC(K)                                                         DK034450          
      DO 63 J=1,K3                                                      DK034460          
      IM(K8)=IM(K8-1)+1                                                 DK034470          
      IF (K8.EQ.1) IM(K8)=1                                             DK034480          
      I=IM(K8)                                                          DK034490          
      K8=K8+1                                                           DK034500          
      LAB1(I)=RGRSN                                                     DK034510          
      LAB2(I)=LITR(K)                                                   DK034520          
      LAB4(I)=BLANK                                                     DK034530          
      IF (J-2) 60,61,62                                                 DK034540          
   60 LAB3(I)=LINEAR                                                    DK034550          
      GO TO 109                                                         DK034560          
   61 LAB3(I)=QUAD                                                      DK034570          
      GO TO 109                                                         DK034580          
   62 LAB3(I)=CUBIC                                                     DK034590          
  109 IF (ICLR(K).EQ.0) GO TO 63                                        DK034600          
      DO 112 L=1,K2                                                     DK034610          
      IM(K8)=IM(K8-1)+NR1(L)-1                                          DK034620          
      M=K9-K2                                                           DK034630          
      DO 118 N=1,K2                                                     DK034640          
      IF (LAD(50+M).EQ.MS(L)) LAD(50+M)=K8                              DK034650          
      M=M+1                                                             DK034660          
  118 CONTINUE                                                          DK034670          
      K5=IM(K8-1)+1                                                     DK034680          
      K4=IM(K8)                                                         DK034690          
      K8=K8+1                                                           DK034700          
      K7=NW(L)                                                          DK034710          
      DO 111 I=K5,K4                                                    DK034720          
      LAB3(I)=LITR(K)                                                   DK034730          
      N=MS(L)                                                           DK034740          
      IF (N.GT.NOM) GO TO 113                                           DK034750          
      LAB2(I)=LIT(N)                                                    DK034760          
      CALL ITOA (IDEN(K7),LAB1(I))                                      DK034770          
      GO TO 114                                                         DK034780          
  113 K6=N-NOM                                                          DK034790          
      LAB2(I)=NLIT(K6)                                                  DK034800          
      CALL ITOA (NDEN(K7),LAB1(I))                                      DK034810          
  114 IF (J-2) 115,116,117                                              DK034820          
  115 LAB4(I)=RGNL                                                      DK034830          
      GO TO 111                                                         DK034840          
  116 LAB4(I)=RGNQ                                                      DK034850          
      GO TO 111                                                         DK034860          
  117 LAB4(I)=RGNC                                                      DK034870          
  111 K7=K7+1                                                           DK034880          
  112 CONTINUE                                                          DK034890          
   63 CONTINUE                                                          DK034900          
      IF(XM(K).EQ.0.0) GO TO 64                                         DK034901          
      IF (LOGE(K).EQ.1) XM(K)=DLOG10(XM(K))                             DK034910          
   64 IF (LOGE(K).EQ.2) XM(K)=DSQRT(XM(K))                              DK034920          
C     ----------------------------------------------------              DK034930          
C     READS AND LISTS PARAMETER CARDS FOR RHM                           DK034940          
C     ----------------------------------------------------              DK034950          
   65 IF (NRHM.EQ.0) GO TO 67                                           DK034960          
      DO 66 I=1,NRHM                                                    DK034970          
      READ  (5,1019)   NEGY(I),LNY(I),LHY(I),KBEG(I),NDECY(I),YM(I),LITYDK034980          
     1(I)                                                               DK034990          
 1019 FORMAT (2I1,I2,I3,I2,F10.5,A6)                                    DK035000          
      WRITE (6,1020) I,NEGY(I),LNY(I),LHY(I),KBEG(I),NDECY(I),YM(I),LITYDK035010          
     1(I)                                                               DK035020          
 1020 FORMAT (1H0,6HFOR I=,I2,54H  NEGY(I) LNY(I) LHY(I) KBEG(I) NDECY(IDK035030          
     1) YM(I) LITY(I)/ 1H ,12X,I2,2I7,2I8,2X,F10.5,1X,A6)               DK035040          
      IF(YM(I).EQ.0.0) GO TO 66                                         DK035041          
      IF (LNY(I).EQ.1) YM(I)=DLOG10(YM(I))                              DK035050          
   66 IF (LNY(I).EQ.2) YM(I)=DSQRT(YM(I))                               DK035060          
   67 IF (IRAN.EQ.0) GO TO 68                                           DK035070          
      K=1                                                               DK035080          
      DO 76 J=1,IRAN                                                    DK035090          
      K3=101-J                                                          DK035100          
      READ (5,1023)  LAB4(K3),I309(J),NR1(J),NW(J),L,(NOS(I),I=1,L)     DK035110          
 1023 FORMAT (A6,6I2)                                                   DK035120          
      WRITE (6,1021) LAB4(K3),I309(J),NR1(J),NW(J),L,(NOS(I),I=1,L)     DK035130          
 1021 FORMAT(48H0CLASSIFICATION  I309 NR1   NW  NS2  MS(1) MS(2)/                         
     1 (1H ,4X,A6,5X,6I5))                                                                
      NS2(J)=L                                                          DK035160          
      DO 77 M=1,L                                                       DK035170          
      K1=K+M-1                                                          DK035180          
      MS(K1)=NOS(M)                                                     DK035190          
   77 NOS(M)=0                                                          DK035200          
   76 K=K+L                                                             DK035210          
   68 IF (MULL.EQ.0) GO TO 72                                           DK035220          
      WRITE (6,1027)                                                    DK035230          
 1027 FORMAT(33H0JOB BOMBED OUT ON A PREVIOUS RUN)                                        
      RETURN                                                            DK035250          
   72 IF (N2F.EQ.0) GO TO 75                                            DK035260          
C     ----------------------------------------------------              DK035270          
C     CHECKS FOR CONSECUTIVE NUMBERING OF ALL MAIN EFFECTS              DK035280          
C     ----------------------------------------------------              DK035290          
      J=1                                                               DK035300          
      IF (NOM.EQ.0) GO TO 70                                            DK035310          
      DO 69 I=1,NOM                                                     DK035320          
      IF (MEN(I).NE.J) GO TO 900                                        DK035330          
   69 J=J+1                                                             DK035340          
   70 IF (NON.EQ.0) GO TO 75                                            DK035350          
      DO 71 I=1,NON                                                     DK035360          
      IF (NEN(I).NE.J) GO TO 900                                        DK035370          
   71 J=J+1                                                             DK035380          
      GO TO 75                                                          DK035390          
  900 WRITE (6,1022) IJOB                                               DK035400          
 1022 FORMAT (1H0,73HCODES FOR EFFECTS NOT CONSECUTIVE---CHECK PARAMETERDK035410          
     1 CARDS FOR PROBLEM NO.,I3)                                        DK035420          
      MULL=1                                                            DK035430          
   75 CONTINUE                                                          DK035440          
      NS=K8-1                                                           DK035450          
      RETURN                                                            DK035460          
  901 WRITE (6,1901)                                                    DK035470          
 1901 FORMAT (1H0,42HPROGRAM SIZE EXCEEDED.  CHECK LIMITATIONS.)        DK035480          
      GO TO 950                                                         DK035490          
  902 WRITE (6,1902)                                                    DK035500          
 1902 FORMAT (1H0,60HINCOMPATIBLE ABSORPTION OPTION AND MODEL TYPE SPECIDK035510          
     *FICATION.)                                                        DK035520          
      GO TO 950                                                         DK035530          
  903 WRITE (6,1903)                                                    DK035540          
 1903 FORMAT (1H0,54HPROGRAM LIMITATIONS EXCEEDED ON PARAMETER CARD TYPEDK035550          
     * 1.)                                                              DK035560          
      GO TO 950                                                         DK035570          
  904 WRITE (6,1904)                                                    DK035580          
 1904 FORMAT (1H0,97HMAXIMUM NUMBER OF CONTROL COLUMNS EXCEEDED FOR NCC,DK035590          
     * NMJC, NINT, OR NMIC ON PARAMETER CARD TYPE 2.)                   DK035600          
      GO TO 950                                                         DK035610          
  905 WRITE (6,1905)                                                    DK035620          
 1905 FORMAT (1H0,92HMAXIMUM NUMBER OF SETS OF MAIN AND NESTED EFFECTS FDK035630          
     *OR WHICH POLYNOMIALS ARE FITTED EXCEEDED.)                        DK035640          
      GO TO 950                                                         DK035650          
  906 WRITE (6,1906)                                                    DK035660          
 1906 FORMAT (1H0,96HMAXIMUM NUMBER OF CLASSES FOR MAIN AND NESTED EFFECDK035670          
     *TS FOR WHICH POLYNOMIALS ARE FITTED EXCEEDED.)                    DK035680          
      GO TO 950                                                         DK035690          
  907 WRITE (6,1907)                                                    DK035700          
 1907 FORMAT (1H0,50HMAXIMUM NUMBER OF INDIVIDUAL REGRESSIONS EXCEEDED.)DK035710          
      GO TO 950                                                         DK035720          
  909 LAD(K)=1                                                                            
      WRITE(6,1909)                                                                       
 1909 FORMAT(90H0     ** WARNING **     LQC>1 AND LAD=0.  LAD HAS BEEN S                  
     1ET TO 1. ADJUSTMENT IS MADE TO XM./)                                                
      GO TO 54                                                                            
  910 LAD(K)=1                                                                            
      WRITE(6,1910)                                                                       
 1910 FORMAT(68H0NAB=3 AND LAD=0.  LAD HAS BEEN SET TO 1.  ADJUSTMENT IS                  
     1 MADE TO XM.)                                                                       
      GO TO 54                                                                            
  908 WRITE (6,1908)                                                    DK035730          
 1908 FORMAT(1H0,51HMAXIMUM NUMBER OF LINES ALLOWED FOR ANOVA EXCEEDED.)DK035740          
  950 MULL=1                                                            DK035750          
      RETURN                                                            DK035760          
      END                                                               DK035770          
