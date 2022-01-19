      SUBROUTINE CODE                                                   DK100010          
C     -------------------------------------------                       DK100020          
C     SUBROUTINE USED TO GET DATA FROM CARD                             DK100030          
C     CODE PARAMETERS (ML3,INTV,NOM,NCL,IBEG,LME,NCLN,NBEG,LNE,IC,IDEN, DK100040          
C     NDEN,ML,K1,ICOD,ISW2,ISW3,L,L7,IJOB)                              DK100050          
C     ----------------------------------------------------              DK100060          
      DIMENSION NS2(15),NDC(10),    NMI(10),    MEN(20),   NCL(20),     DK100090          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(50),NCLN(50),LNE(50),NDEN(1                  
     300),NLIT(50),NMA(50),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(50),                  
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDK100120          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),                          
     6EFF1(50),EFF2(50),NOS(300),X(106),NMAC(50),I309(15),NR1(15),NW(15)                  
      DIMENSION IC(1000)                                                                  
      COMMON /CMBLK2/LIT,LITR,NLIT,XM,EFF1,EFF2,X,NDC,NMI,MEN,NCL,LME,IBDK100150          
     1EG,IDEN,NCLN,NEN,LNE,NBEG,NDEN,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQDK100160          
     2C,NREGP,LGTX,JBEG,NDECX,NEGY,LNY,LHY,KBEG,NDECY,IC,NOS,NMAC       DK100170          
      COMMON /CMBLK3/RR,REP,DF,SDF,EDF,WK,IJOB,NAB,NLHM,NRHM,NMEA,NME,NNDK100180          
     1EA,NNE,N2F,NPR,IRAN,MPOP,LIOP,IAD,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,DK100190          
     2NOM,NON,ML,MLB,NS,NOT,KPUT,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,IN,I30DK100200          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DK100210          
     1L,L7,KA,KB,KC,SOF,NX,KD,MTY,MAN,NRUN,NRN,EDFF,NAME,EDFS,NAM5,IFP,NDK100220          
     2DFA,IFPC,NDFC,NDFAC,NINT,IBET,LBEG,LTHN,NNDC,WTT,SWT1             DK100230          
      ISW2=0                                                            DK100240          
      ISW3=0                                                            DK100250          
      ML3=0                                                             DK100260          
      INTVM1=INTV-1                                                     DK100270          
      IF (INTV.GT.NOM) GO TO 6                                          DK100280          
      K1=NCL(INTV)                                                      DK100290          
      L7=IBEG(INTV)                                                     DK100300          
      ISW=0                                                             DK100310          
      L=LME(INTV)+L7-1                                                  DK100320          
      IF (INTVM1.EQ.0) GO TO 12                                         DK100330          
      DO 4 J=1,INTVM1                                                   DK100340          
    4 ML3=NCL(J)+ML3                                                    DK100350          
      GO TO 12                                                          DK100360          
    6 K2=NOM+1                                                          DK100370          
      IF (K2.GT.INTVM1) GO TO 9                                         DK100380          
      DO 8 J=K2,INTVM1                                                  DK100390          
      K3=J-NOM                                                          DK100400          
    8 ML3=NCLN(K3)+ML3                                                  DK100410          
    9 KIX=INTV-NOM                                                      DK100420          
      L7=NBEG(KIX)                                                      DK100430          
      K1=NCLN(KIX)                                                      DK100440          
      L=LNE(KIX)+L7-1                                                   DK100450          
      ISW=1                                                             DK100460          
   12 K=K1+ML3                                                          DK100470          
      J=NCDS                                                            DK100480          
      CALL FIELD (ICOD,L,IC,L7,J)                                       DK100490          
      L=0                                                               DK100500          
      IF (J.EQ.1) GO TO 50                                              DK100510          
      ML1=ML3                                                           DK100520          
   16 ML3=ML3+1                                                         DK100530          
      IF (ISW.EQ.1) GO TO 20                                            DK100540          
      IF (ML3.GT.K) GO TO 900                                           DK100550          
      IF (ICOD.NE.IDEN(ML3)) GO TO 16                                   DK100560          
      GO TO 26                                                          DK100570          
   20 IF (ML3.LE.K) GO TO 24                                            DK100580          
      ISW2=1                                                            DK100590          
      GO TO 49                                                          DK100600          
   24 IF (ICOD.NE.NDEN(ML3)) GO TO 16                                   DK100610          
   26 ML3=ML3-ML1                                                       DK100620          
      GO TO 49                                                          DK100630          
  900 WRITE (6,1000) NCDS,INTV,ICOD,IJOB                                DK100640          
 1000 FORMAT(31H0CLASS CODE MISSING ON CARD NO.,I8,5X,6HEFFECT,I3,                        
     1 17HCANNOT FIND CODE ,I5,39H. CHECK PARAMETER CARDS FOR PROBLEM NO                  
     2.,I3)                                                                               
      ISW3=1                                                            DK100670          
   49 RETURN                                                            DK100680          
   50 L=1                                                               DK100690          
      RETURN                                                            DK100700          
      END                                                               DK100710          
