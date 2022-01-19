*TEXT                                                                                     
      PROGRAM LSMLMM(INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT,TAPE2)                         
C     MAIN PROGRAM--------------------------------------                DK010010          
C     LEAST SQUARES AND MAXIMUM LIKELIHOOD GENERAL PURPOSE PROGRAM      DK010020          
C     MIXED MODEL VERSION-----LSMLMM ----WALTER R. HARVEY               DK010030          
C     LSMLMM DRIVER DECK                                                DK010040          
C     --------------------------------------------------                DK010050          
      DIMENSION ARRAY(5000),SSCPR(630),SSS(630),RHM(2450),TOT(106),TOT2(DK010080          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100),NS2(15)    DK010090          
      DIMENSION FAB(5000),FY(630),TOT4(106),SAB(630),FSQ(525),TOT5(106) DK010100          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),MZ(10),IPL(13),NSDK010110          
     1P(13),NND(13),XP(81),YP(41),      NDC(10),NMI(10),MEN(20),NCL(20),DK010120          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(40),NCLN(40),LNE(40),NDEN(1DK010130          
     300),NLIT(40),NMA(30),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(40),DK010140          
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDK010150          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),IC(480),DK010160          
     6EFF1(50),EFF2(50),NOS(300),X(106),NMAC(40),I309(15),NR1(15),NW(15)DK010170          
     7,R1I(100),R2I(100)                                                DK010180          
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADK010190          
     1B4,LITY,TRED,YM,XP,YP,IM,MS,NEG,IPL,NSP,NND,FAB,FY,TOT4,SAB,FSQ,TODK010200          
     2T5,MZ,R1I,R2I                                                     DK010210          
      COMMON /CMBLK2/LIT,LITR,NLIT,XM,EFF1,EFF2,X,NDC,NMI,MEN,NCL,LME,IBDK010220          
     1EG,IDEN,NCLN,NEN,LNE,NBEG,NDEN,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQDK010230          
     2C,NREGP,LGTX,JBEG,NDECX,NEGY,LNY,LHY,KBEG,NDECY,IC,NOS,NMAC       DK010240          
      COMMON /CMBLK3/RR,REP,DF,SDF,EDF,WK,IJOB,NAB,NLHM,NRHM,NMEA,NME,NNDK010250          
     1EA,NNE,N2F,NPR,IRAN,MPOP,LIOP,IAD,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,DK010260          
     2NOM,NON,ML,MLB,NS,NOT,KPUT,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,IN,I30DK010270          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DK010280          
     1L,L7,KA,KB,KC,SOF,NX,KD,MTY,MAN,NRUN,NRN,EDFF,NAME,EDFS,NAM5,IFP,NDK010290          
     2DFA,IFPC,NDFC,NDFAC,NINT                                          DK010300          
      DATA IBLK/10H          /                                                            
    1 MULL=0                                                            DK010330          
      KPUT=0                                                            DK010340          
      NSME=0                                                            DK010350          
      NCAS=0                                                            DK010360          
      I30=0                                                             DK010370          
      KB=0                                                              DK010380          
      KC=0                                                              DK010390          
      KD=0                                                              DK010400          
      CALL PARAM                                                        DK010410          
      IF (MULL.EQ.1) GO TO 900                                          DK010420          
      CALL RCBM                                                         DK010430          
      IF (MULL.EQ.1) GO TO 900                                          DK010440          
      IF (LIOP.EQ.6.OR.KPUT.EQ.1) GO TO 900                             DK010450          
      CALL LSCOMB                                                       DK010460          
      KD=0                                                              DK010470          
      IF (MTY.GT.1.AND.NRUN.EQ.NRN) CALL SUAFMM                         DK010480          
  900 CONTINUE                                                          DK010490          
      IF (MULL.EQ.0) GO TO 918                                          DK010530          
  910 READ(IN,1000)(IC(I),I=1,8)                                                          
      IF(EOF(IN)) 912,920                                                                 
  920 CONTINUE                                                                            
      NTRA=0                                                            DK010550          
 1000 FORMAT(8A10)                                                                        
      DO 911 I=1,20                                                     DK010570          
  911 IF (IC(I).NE.IBLK) NTRA=1                                         DK010580          
      GO TO 915                                                         DK010590          
  912 NTRA=0                                                            DK010600          
  915 IF (NTRA.EQ.0) GO TO 913                                          DK010610          
      GO TO 910                                                         DK010620          
  913 IF (NLC.EQ.0) GO TO 918                                           DK010630          
      DO 914 I=1,NLC                                                    DK010640          
  914 READ (5,1002) NCD,(TOT2(J), J=1,NCD)                              DK010650          
 1002 FORMAT (14X,I2,12F5.0,4X/(16F5.0))                                DK010660          
  918 IF (MPOP.EQ.1) GO TO 1                                            DK010670          
      STOP                                                              DK010680          
      END                                                               DK010690          
*ENDTEXT                                                                                  
