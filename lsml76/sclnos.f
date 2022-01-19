      SUBROUTINE SCLNOS                                                 DK130010          
C     -------------------------------------------                       DK130020          
C     SUBROUTINE TO LIST DISTRIBUTION OF CLASS AND SUBCLASS NUMBERS     DK130030          
C     ------------------------------------------------------            DK130040          
      DIMENSION ARRAY(5000),SSCPR(630),SSS(630),RHM(2450),TOT(106),TOT2(DK130080          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100),NS2(15)    DK130090          
      DIMENSION FAB(5000),FY(630),TOT4(106),SAB(630),FSQ(525),TOT5(106) DK130100          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),MZ(10),IPL(13),NSDK130110          
     1P(13),NND(13),XP(81),YP(41),      NDC(10),NMI(10),MEN(20),NCL(20),DK130120          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(50),NCLN(50),LNE(50),NDEN(1                  
     300),NLIT(50),NMA(50),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(50),                  
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDK130150          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),                          
     6EFF1(50),EFF2(50),NOS(300),X(106),NMAC(50),I309(15),NR1(15),NW(15)                  
     7,R1I(100),R2I(100),WW(200),ICLR(90),IRM(40),LAD(90)               DK130180          
      DIMENSION IC(1000)                                                                  
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADK130190          
     1B4,LITY,TRED,YM,XP,YP,IM,MS,NEG,IPL,NSP,NND,FAB,FY,TOT4,SAB,FSQ,TODK130200          
     2T5,MZ,R1I,R2I,WW,ICLR,IRM,LAD                                     DK130210          
      COMMON /CMBLK2/LIT,LITR,NLIT,XM,EFF1,EFF2,X,NDC,NMI,MEN,NCL,LME,IBDK130220          
     1EG,IDEN,NCLN,NEN,LNE,NBEG,NDEN,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQDK130230          
     2C,NREGP,LGTX,JBEG,NDECX,NEGY,LNY,LHY,KBEG,NDECY,IC,NOS,NMAC       DK130240          
      COMMON /CMBLK3/RR,REP,DF,SDF,EDF,WK,IJOB,NAB,NLHM,NRHM,NMEA,NME,NNDK130250          
     1EA,NNE,N2F,NPR,IRAN,MPOP,LIOP,IAD,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,DK130260          
     2NOM,NON,ML,MLB,NS,NOT,KPUT,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,IN,I30DK130270          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DK130280          
     1L,L7,KA,KB,KC,SOF,NX,KD,MTY,MAN,NRUN,NRN,EDFF,NAME,EDFS,NAM5,IFP,NDK130290          
     2DFA,IFPC,NDFC,NDFAC,NINT,IBET,LBEG,LTHN,NNDC,WTT,SWT1             DK130300          
      DATA IBLK,MIS,MISN,MISI/6H      ,6HMIS NI,6HID MIS,6HMIS ID/      DK130310          
      WRITE (6,1001) IJOB                                               DK130320          
 1001 FORMAT (1H0,60H  DISTRIBUTION OF CLASS AND SUBCLASS NUMBERS FOR PRDK130330          
     1OBLEM NO.,I4)                                                     DK130340          
      IF (IBET.EQ.0) GO TO 1                                            DK130350          
      WRITE (6,1005)                                                    DK130360          
 1005 FORMAT (1H0,19X,14HIDENTIFICATION,13X,3HNO.,8X,14HSUM OF WEIGHTS) DK130370          
      WRITE (6,1007) NCDS,SWT1                                          DK130380          
 1007 FORMAT (1H0,17X,5HTOTAL,21X,I6,10X,F10.2)                         DK130390          
      GO TO 2                                                           DK130400          
    1 WRITE (6,1002)                                                    DK130410          
 1002 FORMAT (1H0,19X,14HIDENTIFICATION,13X,3HNO.)                      DK130420          
      WRITE (6,1007) NCDS                                               DK130430          
    2 L=1                                                               DK130440          
      K=ML+1                                                            DK130450          
      MISS=IBLK                                                         DK130460          
      K1=NMEA+1                                                         DK130470          
      IF (K1.GT.NOM) GO TO 17                                           DK130480          
      DO 16 I=K1,NOM                                                    DK130490          
      WRITE (6,1000)                                                    DK130500          
 1000 FORMAT (1H )                                                      DK130510          
      K2=NCL(I)                                                         DK130520          
      DO 16 J=1,K2                                                      DK130530          
      IF (NOS(L).NE.0) GO TO 10                                         DK130540          
      MISS=MIS                                                          DK130550          
      KPUT=2                                                            DK130560          
   10 IF (IBET.EQ.0) GO TO 3                                            DK130570          
      WRITE (6,1003) LIT(I),IDEN(K),NOS(L),MISS,WW(L)                   DK130580          
      GO TO 4                                                           DK130590          
    3 WRITE (6,1003) LIT(I),IDEN(K),NOS(L),MISS                         DK130600          
 1003 FORMAT (1H ,17X,A6,I5,15X,I6,2X,A6,2X,F10.2)                      DK130610          
    4 K=K+1                                                             DK130620          
      MISS=IBLK                                                         DK130630          
   16 L=L+1                                                             DK130640          
   17 K=MLB+1                                                           DK130650          
      K1=NNEA+1                                                         DK130660          
      IF (K1.GT.NON) GO TO 23                                           DK130670          
      DO 24 I=K1,NON                                                    DK130680          
      WRITE (6,1000)                                                    DK130690          
      NSUM=0                                                            DK130700          
      K2=NCLN(I)                                                        DK130710          
      DO 22 J=1,K2                                                      DK130720          
      NSUM=NSUM+NOS(L)                                                  DK130730          
      IF (NOS(L).NE.0) GO TO 20                                         DK130740          
      MISS=MIS                                                          DK130750          
      KPUT=2                                                            DK130760          
   20 IF (IBET.EQ.0) GO TO 5                                            DK130770          
      WRITE (6,1003)NLIT(I),NDEN(K),NOS(L),MISS,WW(L)                   DK130780          
      GO TO 6                                                           DK130790          
    5 WRITE (6,1003) NLIT(I),NDEN(K),NOS(L),MISS                        DK130800          
    6 MISS=IBLK                                                         DK130810          
      K=K+1                                                             DK130820          
   22 L=L+1                                                             DK130830          
      IF (NMA(I).LE.NMEA) GO TO 24                                      DK130840          
      K6=NMA(I)-1                                                       DK130850          
      K4=0                                                              DK130860          
      K5=NMEA+1                                                         DK130870          
      IF (K6.LT.K5) GO TO 9                                             DK130880          
      DO 11 K3=K5,K6                                                    DK130890          
   11 K4=K4+NCL(K3)                                                     DK130900          
    9 K4=K4+NMAC(I)                                                     DK130910          
      IF (NSUM.EQ.NOS(K4)) GO TO 24                                     DK130920          
      WRITE (6,1006) NLIT(I),NSUM,NOS(K4)                               DK130930          
 1006 FORMAT(30H SUM OF SUBCLASS NUMBERS FOR  ,A6,5H  IS ,I5,                             
     122H  AND IS NOT EQUAL TO ,I5,24H  THE MAIN EFFECT NUMBER)                           
   24 CONTINUE                                                          DK130960          
   23 K6=0                                                              DK130970          
      IF (N2F.EQ.0) GO TO 81                                            DK130980          
      DO 80 I=1,N2F                                                     DK130990          
      WRITE (6,1000)                                                    DK131000          
      K6=K6+NMC(I)                                                      DK131010          
      NSUM=0                                                            DK131020          
      MSUM=0                                                            DK131030          
      IF (INT1(I).GT.NOM) GO TO 34                                      DK131040          
      K1=INT1(I)-1                                                      DK131050          
      IF (K1.EQ.0) GO TO 31                                             DK131060          
      DO 30 J=1,K1                                                      DK131070          
   30 NSUM=NSUM+NCL(J)                                                  DK131080          
   31 K1=INT1(I)                                                        DK131090          
      K3=NCL(K1)                                                        DK131100          
      ID1=LIT(K1)                                                       DK131110          
      GO TO 40                                                          DK131120          
   34 K1=NOM+1                                                          DK131130          
      K2=INT1(I)-1                                                      DK131140          
      IF (K1.GT.K2) GO TO 37                                            DK131150          
      DO 36 J=K1,K2                                                     DK131160          
      K=J-NOM                                                           DK131170          
   36 NSUM=NSUM+NCLN(K)                                                 DK131180          
   37 K1=INT1(I)-NOM                                                    DK131190          
      K3=NCLN(K1)                                                       DK131200          
      ID1=NLIT(K1)                                                      DK131210          
   40 IF (INT2(I).GT.NOM) GO TO 46                                      DK131220          
      K2=INT2(I)-1                                                      DK131230          
      IF (K2.EQ.0) GO TO 43                                             DK131240          
      DO 42 J=1,K2                                                      DK131250          
   42 MSUM=MSUM+NCL(J)                                                  DK131260          
   43 K2=INT2(I)                                                        DK131270          
      K4=NCL(K2)                                                        DK131280          
      ID2=LIT(K2)                                                       DK131290          
      GO TO 50                                                          DK131300          
   46 K1=NOM+1                                                          DK131310          
      K2=INT2(I)-1                                                      DK131320          
      IF (K1.GT.K2) GO TO 49                                            DK131330          
      DO 48 J=K1,K2                                                     DK131340          
      K=J-NOM                                                           DK131350          
   48 MSUM=MSUM+NCLN(K)                                                 DK131360          
   49 K1=INT2(I)-NOM                                                    DK131370          
      K4=NCLN(K1)                                                       DK131380          
      ID2=NLIT(K1)                                                      DK131390          
   50 DO 80 J=1,K3                                                      DK131400          
      K2=NSUM+J                                                         DK131410          
      IF (INT1(I).GT.NOM) GO TO 54                                      DK131420          
      ID3=IDEN(K2)                                                      DK131430          
      GO TO 56                                                          DK131440          
   54 ID3=NDEN(K2)                                                      DK131450          
   56 DO 80 K=1,K4                                                      DK131460          
      K5=MSUM+K                                                         DK131470          
      IF (NOS(L).NE.0.AND.NMC(I).EQ.0) GO TO 70                         DK131480          
      MISS=MIS                                                          DK131490          
      IF (NMC(I).EQ.0.AND.NOS(L).EQ.0) KPUT=2                           DK131500          
      IF (NMC(I).EQ.0) GO TO 70                                         DK131510          
      K7=J*100+K                                                        DK131520          
      K1=K6-NMC(I)                                                      DK131530          
   60 K1=K1+1                                                           DK131540          
      IF (K1.GT.K6) GO TO 68                                            DK131550          
      IF (K7-MSCL(K1)) 60,64,60                                         DK131560          
   64 IF (NOS(L).EQ.0) MISS=MISI                                        DK131570          
      IF (NOS(L).NE.0) MISS=MISN                                        DK131580          
      IF (J.EQ.K3.OR.K.EQ.K4) KPUT=2                                    DK131590          
      GO TO 70                                                          DK131600          
   68 IF (NOS(L).NE.0) MISS=IBLK                                        DK131610          
      IF (NOS(L).EQ.0) KPUT=2                                           DK131620          
   70 IF (INT2(I).GT.NOM) GO TO 74                                      DK131630          
      ID4=IDEN(K5)                                                      DK131640          
      GO TO 76                                                          DK131650          
   74 ID4=NDEN(K5)                                                      DK131660          
   76 IF (IBET.EQ.0) GO TO 7                                            DK131670          
      WRITE (6,1004) ID1,ID2,ID3,ID4,NOS(L),MISS,WW(L)                  DK131680          
      GO TO 8                                                           DK131690          
    7 WRITE (6,1004) ID1,ID2,ID3,ID4,NOS(L),MISS                        DK131700          
 1004 FORMAT (1H ,17X,A6,3H X ,A6,2I5,I7,2X,A6,2X,F10.2)                DK131710          
    8 MISS=IBLK                                                         DK131720          
   80 L=L+1                                                             DK131730          
   81 NCC=L-1                                                           DK131740          
      RETURN                                                            DK131750          
      END                                                               DK131760          
