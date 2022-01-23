*TEXT                                                                                     
      PROGRAM LSMLGP                                                                      
C     LEAST SQUARES AND MAXIMUM LIKELIHOOD GENERAL PURPOSE PROGRAM      DEC01001          
C     FORTRAN IV VERSION, CONVERTED TO FORTRAN IV FROM SCATRAN BY       DEC01002          
C                       CHARLES GASKINS AND WALTER HARVEY               DEC01003          
C     LSMLGP DRIVER DECK                                                DEC01004          
C     --------------------------------------------------                DEC01005          
      DIMENSION ARRAY(2000),SSCPR(630),SSS(630),RHM(0250),TOT(106),TOT2(DEC01006          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100)            DEC01007          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),NEQ(6),IPL(13),NSDEC01008          
     1P(13),NND(13),XP(81),YP(41),      NDC(10),NMI(10),MEN(20),NCL(20),DEC01009          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(40),NCLN(40),LNE(40),NDEN(1DEC01010          
     300),NLIT(40),NMA(30),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(40),DEC01011          
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDEC01012          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),IC(480),DEC01013          
     6EFF1(50),EFF2(50),NOS(200),X(106),NMAC(40)                        DEC01014          
      DIMENSION LSKF(20),LBEG(20),IREJ(20)                              M0201014          
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADEC01015          
     1B4,LITY,TRED,YM,IM,MS,NEQ,IPL,NSP,NND,XP,YP                       DEC01016          
      COMMON /CMBLK2/NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NCLN,NEN,LNE,NBEGDEC01017          
     1,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQC,NREGP,LGTX,JBEG,NDDEC01018          
     2ECX,LITR,NEGY,LNY,LHY,KBEG,NDECY,XM,IC,EFF1,EFF2,NOS,X,NMAC       DEC01019          
      COMMON /CMBLK3/IJOB,NAB,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F,NPR,IRAN,MDEC01020          
     1POP,LIOP,IAD,REP,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,NOM,NON,ML,MLB,NSDEC01021          
     2,NOT,DF,KPUT,SDF,EDF,WK,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,RR       DEC01022          
     3,IN,NSKF,LSKF,LBEG,IREJ                                           M0201022          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DEC01023          
     1L,L7                                                              DEC01024          
      DATA (IBLK=6H      )                                              M0101025          
    1 CONTINUE                                                          M0101026          
      KPUT=0                                                            DEC01028          
      MULL=0                                                            M0101028          
      NSME=0                                                            DEC01029          
      NCAS=0                                                            DEC01030          
      I309=0                                                            DEC01031          
      DO 2 I=1,5                                                        DEC01032          
    2 NEQ(I)=0                                                          DEC01033          
      CALL PARAM                                                        DEC01034          
      IF (MULL.EQ.1) GO TO 900                                          DEC01035          
      CALL MAIN1                                                        M0101036          
      IF (MULL.EQ.1) GO TO 900                                          DEC01037          
      IF (LIOP.EQ.6.OR.KPUT.EQ.1) GO TO 900                             DEC01038          
      CALL LSCOMB                                                       DEC01039          
  900 CONTINUE                                                          DEC01040          
      IF (MULL.EQ.0) GO TO 913                                          DEC01044          
  910 READ(IN,1000) (IC(I),I=1,20)                                      M0201045          
      NTRA=0                                                            DEC01046          
 1000 FORMAT (80A1)                                                     DEC01047          
      DO 911 I=1,20                                                     DEC01048          
  911 IF (IC(I).NE.IBLK) NTRA=1                                         DEC01049          
      IF (NTRA.EQ.1) GO TO 910                                          DEC01050          
  913 IF (MPOP.EQ.1) GO TO 1                                            DEC01051          
      STOP                                                              DEC01052          
      END                                                               DEC01053          
*ENDTEXT                                                                                  
