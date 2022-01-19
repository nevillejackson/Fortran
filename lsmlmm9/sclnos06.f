*TEXT                                                                                     
      SUBROUTINE SCLNOS                                                 DK130010          
C     -------------------------------------------                       DK130020          
C     SUBROUTINE TO LIST DISTRIBUTION OF CLASS AND SUBCLASS NUMBERS     DK130030          
C     ------------------------------------------------------            DK130040          
      DIMENSION ARRAY(5000),SSCPR(630),SSS(630),RHM(2450),TOT(106),TOT2(DK130080          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100),NS2(15)    DK130090          
      DIMENSION FAB(5000),FY(630),TOT4(106),SAB(630),FSQ(525),TOT5(106) DK130100          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),MZ(10),IPL(13),NSDK130110          
     1P(13),NND(13),XP(81),YP(41),      NDC(10),NMI(10),MEN(20),NCL(20),DK130120          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(40),NCLN(40),LNE(40),NDEN(1DK130130          
     300),NLIT(40),NMA(30),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(40),DK130140          
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDK130150          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),IC(480),DK130160          
     6EFF1(50),EFF2(50),NOS(300),X(106),NMAC(40),I309(15),NR1(15),NW(15)DK130170          
     7,R1I(100),R2I(100)                                                DK130180          
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADK130190          
     1B4,LITY,TRED,YM,XP,YP,IM,MS,NEG,IPL,NSP,NND,FAB,FY,TOT4,SAB,FSQ,TODK130200          
     2T5,MZ,R1I,R2I                                                     DK130210          
      COMMON /CMBLK2/LIT,LITR,NLIT,XM,EFF1,EFF2,X,NDC,NMI,MEN,NCL,LME,IBDK130220          
     1EG,IDEN,NCLN,NEN,LNE,NBEG,NDEN,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQDK130230          
     2C,NREGP,LGTX,JBEG,NDECX,NEGY,LNY,LHY,KBEG,NDECY,IC,NOS,NMAC       DK130240          
      COMMON /CMBLK3/RR,REP,DF,SDF,EDF,WK,IJOB,NAB,NLHM,NRHM,NMEA,NME,NNDK130250          
     1EA,NNE,N2F,NPR,IRAN,MPOP,LIOP,IAD,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,DK130260          
     2NOM,NON,ML,MLB,NS,NOT,KPUT,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,IN,I30DK130270          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DK130280          
     1L,L7,KA,KB,KC,SOF,NX,KD,MTY,MAN,NRUN,NRN,EDFF,NAME,EDFS,NAM5,IFP,NDK130290          
     2DFA,IFPC,NDFC,NDFAC,NINT                                          DK130300          
      DATA IBLK,MIS,MISN,MISI/6H      ,6HMIS NI,6HID MIS,6HMIS ID/      DK130310          
      WRITE (6,1001) IJOB                                               DK130320          
 1001 FORMAT (1H0,60H  DISTRIBUTION OF CLASS AND SUBCLASS NUMBERS FOR PRDK130330          
     1OBLEM NO.,I4)                                                     DK130340          
      WRITE (6,1002)                                                    DK130350          
 1002 FORMAT (1H0,19X,14HIDENTIFICATION,13X,3HNO.)                      DK130360          
      L=1                                                               DK130370          
      K=ML+1                                                            DK130380          
      MISS=IBLK                                                         DK130390          
      K1=NMEA+1                                                         DK130400          
      DO 16 I=K1,NOM                                                    DK130410          
      IF (K1.GT.NOM) GO TO 17                                           DK130420          
      WRITE (6,1000)                                                    DK130430          
 1000 FORMAT (1H )                                                      DK130440          
      K2=NCL(I)                                                         DK130450          
      DO 16 J=1,K2                                                      DK130460          
      IF (NOS(L).NE.0) GO TO 10                                         DK130470          
      MISS=MIS                                                          DK130480          
      KPUT=1                                                            DK130490          
   10 WRITE (6,1003) LIT(I),IDEN(K),NOS(L),MISS                         DK130500          
 1003 FORMAT (1H ,17X,A6,I5,16X,I5,2X,A6)                               DK130510          
      K=K+1                                                             DK130520          
      MISS=IBLK                                                         DK130530          
   16 L=L+1                                                             DK130540          
   17 K=MLB+1                                                           DK130550          
      K1=NNEA+1                                                         DK130560          
      IF (K1.GT.NON) GO TO 23                                           DK130570          
      DO 22 I=K1,NON                                                    DK130580          
      WRITE (6,1000)                                                    DK130590          
      K2=NCLN(I)                                                        DK130600          
      DO 22 J=1,K2                                                      DK130610          
      IF (NOS(L).NE.0) GO TO 20                                         DK130620          
      MISS=MIS                                                          DK130630          
      KPUT=1                                                            DK130640          
   20 WRITE (6,1003) NLIT(I),NDEN(K),NOS(L),MISS                        DK130650          
      MISS=IBLK                                                         DK130660          
      K=K+1                                                             DK130670          
   22 L=L+1                                                             DK130680          
   23 K6=0                                                              DK130690          
      IF (N2F.EQ.0) GO TO 81                                            DK130700          
      DO 80 I=1,N2F                                                     DK130710          
      WRITE (6,1000)                                                    DK130720          
      K6=K6+NMC(I)                                                      DK130730          
      NSUM=0                                                            DK130740          
      MSUM=0                                                            DK130750          
      IF (INT1(I).GT.NOM) GO TO 34                                      DK130760          
      K1=INT1(I)-1                                                      DK130770          
      IF (K1.EQ.0) GO TO 31                                             DK130780          
      DO 30 J=1,K1                                                      DK130790          
   30 NSUM=NSUM+NCL(J)                                                  DK130800          
   31 K1=INT1(I)                                                        DK130810          
      K3=NCL(K1)                                                        DK130820          
      ID1=LIT(K1)                                                       DK130830          
      GO TO 40                                                          DK130840          
   34 K1=NOM+1                                                          DK130850          
      K2=INT1(I)-1                                                      DK130860          
      IF (K1.GT.K2) GO TO 37                                            DK130870          
      DO 36 J=K1,K2                                                     DK130880          
      K=J-NOM                                                           DK130890          
   36 NSUM=NSUM+NCLN(K)                                                 DK130900          
   37 K1=INT1(I)-NOM                                                    DK130910          
      K3=NCLN(K1)                                                       DK130920          
      ID1=NLIT(K1)                                                      DK130930          
   40 IF (INT2(I).GT.NOM) GO TO 46                                      DK130940          
      K2=INT2(I)-1                                                      DK130950          
      IF (K2.EQ.0) GO TO 43                                             DK130960          
      DO 42 J=1,K2                                                      DK130970          
   42 MSUM=MSUM+NCL(J)                                                  DK130980          
   43 K2=INT2(I)                                                        DK130990          
      K4=NCL(K2)                                                        DK131000          
      ID2=LIT(K2)                                                       DK131010          
      GO TO 50                                                          DK131020          
   46 K1=NOM+1                                                          DK131030          
      K2=INT2(I)-1                                                      DK131040          
      IF (K1.GT.K2) GO TO 49                                            DK131050          
      DO 48 J=K1,K2                                                     DK131060          
      K=J-NOM                                                           DK131070          
   48 MSUM=MSUM+NCLN(K)                                                 DK131080          
   49 K1=INT2(I)-NOM                                                    DK131090          
      K4=NCLN(K1)                                                       DK131100          
      ID2=NLIT(K1)                                                      DK131110          
   50 DO 80 J=1,K3                                                      DK131120          
      K2=NSUM+J                                                         DK131130          
      IF (INT1(I).GT.NOM) GO TO 54                                      DK131140          
      ID3=IDEN(K2)                                                      DK131150          
      GO TO 56                                                          DK131160          
   54 ID3=NDEN(K2)                                                      DK131170          
   56 DO 80 K=1,K4                                                      DK131180          
      K5=MSUM+K                                                         DK131190          
      IF (NOS(L).NE.0.AND.NMC(I).EQ.0) GO TO 70                         DK131200          
      MISS=MIS                                                          DK131210          
      IF (NMC(I).EQ.0.AND.NOS(L).EQ.0) KPUT=1                           DK131220          
      IF (NMC(I).EQ.0) GO TO 70                                         DK131230          
      K7=J*100+K                                                        DK131240          
      K1=K6-NMC(I)                                                      DK131250          
   60 K1=K1+1                                                           DK131260          
      IF (K1.GT.K6) GO TO 68                                            DK131270          
      IF (K7-MSCL(K1)) 60,64,60                                         DK131280          
   64 IF (NOS(L).EQ.0) MISS=MISI                                        DK131290          
      IF (NOS(L).NE.0) MISS=MISN                                        DK131300          
      IF (J.EQ.K3.OR.K.EQ.K4) KPUT=1                                    DK131310          
      GO TO 70                                                          DK131320          
   68 IF(NOS(L).NE.0) MISS=IBLK                                         DK131330          
      IF(NOS(L).EQ.0) KPUT=1                                            DK131340          
   70 IF (INT2(I).GT.NOM) GO TO 74                                      DK131350          
      ID4=IDEN(K5)                                                      DK131360          
      GO TO 76                                                          DK131370          
   74 ID4=NDEN(K5)                                                      DK131380          
   76 WRITE (6,1004) ID1,ID2,ID3,ID4,NOS(L),MISS                        DK131390          
 1004 FORMAT (1H ,17X,A6,3H X ,A6,2I5,I7,2X,A6)                         DK131400          
      MISS=IBLK                                                         DK131410          
   80 L=L+1                                                             DK131420          
   81 NCC=L-1                                                           DK131430          
      RETURN                                                            DK131440          
      END                                                               DK131450          
*ENDTEXT                                                                                  
