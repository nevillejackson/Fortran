*TEXT                                                                                     
      SUBROUTINE CODE                                                                     
C     SUBROUTINE USED TO GET DATA FROM CARD                             DEC07000          
C     CODE PARAMETERS (ML3,INTV,NOM,NCL,IBEG,LME,NCLN,NBEG,LNE,IC,IDEN, DEC07001          
C     NDEN,ML,K1,ICOD,ISW2,ISW3,L,L7,IJOB)                              DEC07002          
C     ----------------------------------------------------              DEC07003          
      DIMENSION         NDC(10),    NMI(10),    MEN(20),   NCL(20),     DEC07005          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(40),NCLN(40),LNE(40),NDEN(1DEC07006          
     300),NLIT(40),NMA(30),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(40),DEC07007          
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDEC07008          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),IC(480),DEC07009          
     6EFF1(50),EFF2(50),NOS(200),X(106),NMAC(40)                        DEC07010          
      DIMENSION LSKF(20),LBEG(20),IREJ(20)                              M0207010          
      COMMON /CMBLK2/NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NCLN,NEN,LNE,NBEGDEC07011          
     1,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQC,NREGP,LGTX,JBEG,NDDEC07012          
     2ECX,LITR,NEGY,LNY,LHY,KBEG,NDECY,XM,IC,EFF1,EFF2,NOS,X,NMAC       DEC07013          
      COMMON /CMBLK3/IJOB,NAB,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F,NPR,IRAN,MDEC07014          
     1POP,LIOP,IAD,REP,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,NOM,NON,ML,MLB,NSDEC07015          
     2,NOT,DF,KPUT,SDF,EDF,WK,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,RR       DEC07016          
     3,IN,NSKF,LSKF,LBEG,IREJ                                           M0207016          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DEC07017          
     1L,L7                                                              DEC07018          
      ISW2=0                                                            DEC07019          
      ISW3=0                                                            DEC07020          
      ML3=0                                                             DEC07021          
      INTVM1=INTV-1                                                     DEC07022          
      IF (INTV.GT.NOM) GO TO 6                                          DEC07023          
      K1=NCL(INTV)                                                      DEC07024          
      L7=IBEG(INTV)                                                     DEC07025          
      ISW=0                                                             DEC07026          
      L=LME(INTV)+L7-1                                                  DEC07027          
      IF (INTVM1.EQ.0) GO TO 12                                         DEC07028          
      DO 4 J=1,INTVM1                                                   DEC07029          
    4 ML3=NCL(J)+ML3                                                    DEC07030          
      GO TO 12                                                          DEC07031          
    6 K2=NOM+1                                                          DEC07032          
      IF (K2.GT.INTVM1) GO TO 9                                         DEC07033          
      DO 8 J=K2,INTVM1                                                  DEC07034          
      K3=J-NOM                                                          DEC07035          
    8 ML3=NCLN(K3)+ML3                                                  DEC07036          
    9 KIX=INTV-NOM                                                      DEC07037          
      L7=NBEG(KIX)                                                      DEC07038          
      K1=NCLN(KIX)                                                      DEC07039          
      L=LNE(KIX)+L7-1                                                   DEC07040          
      ISW=1                                                             DEC07041          
   12 K=K1+ML3                                                          DEC07042          
      CALL FIELD (ICOD,L,IC,L7,J)                                       DEC07043          
      L=0                                                               DEC07044          
      IF (J.EQ.1) GO TO 50                                              DEC07045          
      ML1=ML3                                                           DEC07046          
   16 ML3=ML3+1                                                         DEC07047          
      IF (ISW.EQ.1) GO TO 20                                            DEC07048          
      IF (ML3.GT.K) GO TO 900                                           DEC07049          
      IF (ICOD.NE.IDEN(ML3)) GO TO 16                                   DEC07050          
      GO TO 26                                                          DEC07051          
   20 IF (ML3.LE.K) GO TO 24                                            DEC07052          
      ISW2=1                                                            DEC07053          
      GO TO 49                                                          DEC07054          
   24 IF (ICOD.NE.NDEN(ML3)) GO TO 16                                   DEC07055          
   26 ML3=ML3-ML1                                                       DEC07056          
      GO TO 49                                                          DEC07057          
  900 WRITE (6,1000) IJOB                                               DEC07058          
 1000 FORMAT (1H0,57HCLASS CODE MISSING. CHECK PARAMETER CARDS FOR PROBLDEC07059          
     1EM NO.,I3)                                                        DEC07060          
      ISW3=1                                                            DEC07061          
   49 RETURN                                                            DEC07062          
   50 L=1                                                               DEC07063          
      RETURN                                                            DEC07064          
      END                                                               DEC07065          
*ENDTEXT                                                                                  
