      PROGRAM LSML76(TAPE10=/1000,INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)                  
      EXTERNAL DSQRT                                                                      
      EXTERNAL DFLOAT                                                                     
      EXTERNAL DLOG10                                                                     
C     MAIN PROGRAM--------------------------------------                DK010010          
C     LEAST SQUARES AND MAXIMUM LIKELIHOOD GENERAL PURPOSE PROGRAM      DK010020          
C     MIXED MODEL VERSION-----LSML76 --- WALTER R. HARVEY               DK010030          
C     LSML76 DRIVER DECK                                                DK010040          
C     --------------------------------------------------                DK010050          
      DIMENSION ARRAY(5000),SSCPR(630),SSS(630),RHM(2450),TOT(106),TOT2(DK010080          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100),NS2(15)    DK010090          
      DIMENSION FAB(5000),FY(630),TOT4(106),SAB(630),FSQ(525),TOT5(106) DK010100          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),MZ(10),IPL(13),NSDK010110          
     1P(13),NND(13),XP(81),YP(41),      NDC(10),NMI(10),MEN(20),NCL(20),DK010120          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(50),NCLN(50),LNE(50),NDEN(1                  
     300),NLIT(50),NMA(50),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(50),                  
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDK010150          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),                          
     6EFF1(50),EFF2(50),NOS(300),X(106),NMAC(50),I309(15),NR1(15),NW(15)                  
     7,R1I(100),R2I(100),WW(200),ICLR(90),IRM(40),LAD(90)               DK010180          
      DIMENSION IC(1000)                                                                  
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADK010190          
     1B4,LITY,TRED,YM,XP,YP,IM,MS,NEG,IPL,NSP,NND,FAB,FY,TOT4,SAB,FSQ,TODK010200          
     2T5,MZ,R1I,R2I,WW,ICLR,IRM,LAD                                     DK010210          
      COMMON /CMBLK2/LIT,LITR,NLIT,XM,EFF1,EFF2,X,NDC,NMI,MEN,NCL,LME,IBDK010220          
     1EG,IDEN,NCLN,NEN,LNE,NBEG,NDEN,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQDK010230          
     2C,NREGP,LGTX,JBEG,NDECX,NEGY,LNY,LHY,KBEG,NDECY,IC,NOS,NMAC       DK010240          
      COMMON /CMBLK3/RR,REP,DF,SDF,EDF,WK,IJOB,NAB,NLHM,NRHM,NMEA,NME,NNDK010250          
     1EA,NNE,N2F,NPR,IRAN,MPOP,LIOP,IAD,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,DK010260          
     2NOM,NON,ML,MLB,NS,NOT,KPUT,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,IN,I30DK010270          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DK010280          
     1L,L7,KA,KB,KC,SOF,NX,KD,MTY,MAN,NRUN,NRN,EDFF,NAME,EDFS,NAM5,IFP,NDK010290          
     2DFA,IFPC,NDFC,NDFAC,NINT,IBET,LBEG,LTHN,NNDC,WTT,SWT1             DK010300          
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
      COMMON /CMBLK5/ LREC,LRECW,LAST                                                     
      CALL DEFINE                                                                         
    1 KPUT=0                                                            DK010320          
      NSME=0                                                            DK010330          
      NCAS=0                                                            DK010340          
      I30=0                                                             DK010350          
      KB=0                                                              DK010360          
      KC=0                                                              DK010370          
      WTT=0.                                                            DK010380          
      KD=0                                                              DK010390          
      NCC=0                                                                               
      NMJC=0                                                                              
      NMIC=0                                                                              
      NINT=0                                                                              
      CALL PARAM                                                        DK010400          
      IF (MULL.EQ.1) GO TO 900                                          DK010410          
      CALL RCBM                                                         DK010420          
      IF (MULL.EQ.1) GO TO 900                                          DK010430          
      IF(LIOP.EQ.6.OR.KPUT.GT.0.OR.LIOP.EQ.7) GO TO 900                 DK010440          
      CALL LSCOMB                                                       DK010450          
      IF (MULL.EQ.1) GO TO 900                                          DK010460          
      KD=0                                                              DK010470          
      IF (MTY.GT.1.AND.NRUN.EQ.NRN) CALL SUAFMM                         DK010480          
  900 IF (KPUT.EQ.2) WRITE (6,1003)                                     DK010490          
 1003 FORMAT(114H0CLASS OR SUBCLASS FOUND WITH NO OBSERVATIONS (MIS NI)                   
     1OR SUBCLASS MISSING IN LAST ROW OR COLUMN. ANALYSIS STOPPED)                        
      IF (MULL.EQ.0) GO TO 918                                          DK010520          
      IF (IN.NE.5) GO TO 913                                            DK010530          
  910 CALL READUN(IN,IC,IFLAG,LAST)                                                       
      IF(IFLAG) 912,916,919                                                               
  916 NTRA=0                                                                              
      DO 911 I=1,LREC                                                                     
      IF(LETTER(INST(IC,I))) 911,909,909                                                  
  909 NTRA=1                                                                              
      GO TO 915                                                                           
  911 CONTINUE                                                                            
      GO TO 915                                                         DK010590          
  912 NTRA=0                                                            DK010600          
  915 IF (NTRA.EQ.0) GO TO 913                                          DK010610          
      GO TO 910                                                         DK010620          
  913 IF (NLC.EQ.0) GO TO 918                                           DK010630          
      DO 914 I=1,NLC                                                    DK010640          
  914 READ (5,1002) NCD,(TOT2(J), J=1,NCD)                              DK010650          
 1002 FORMAT (14X,I2,12F5.0,4X/(16F5.0))                                DK010660          
  918 IF (MPOP.EQ.1) GO TO 1                                            DK010670          
  919 STOP                                                                                
      END                                                               DK010690          
