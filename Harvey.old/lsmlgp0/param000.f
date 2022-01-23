*TEXT                                                                                     
      SUBROUTINE PARAM                                                                    
C     SUBROUTINE WHICH READS AND LISTS PARAMETER CARDS, AND SETS UP     DEC02000          
C     ARRAYS FOR LABELING OUTPUT                                        DEC02001          
C     ----------------------------------------------------              DEC02002          
      DIMENSION ARRAY(2000),SSCPR(630),SSS(630),RHM(0250),TOT(106),TOT2(DEC02004          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100)            DEC02005          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),NEQ(6),IPL(13),NSDEC02006          
     1P(13),NND(13),XP(81),YP(41),      NDC(10),NMI(10),MEN(20),NCL(20),DEC02007          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(40),NCLN(40),LNE(40),NDEN(1DEC02008          
     300),NLIT(40),NMA(30),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(40),DEC02009          
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDEC02010          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),IC(480),DEC02011          
     6EFF1(50),EFF2(50),NOS(200),X(106),NMAC(40)                        DEC02012          
      DIMENSION LSKF(20),LBEG(20),IREJ(20)                              M0202012          
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADEC02013          
     1B4,LITY,TRED,YM,IM,MS,NEQ,IPL,NSP,NND,XP,YP                       DEC02014          
      COMMON /CMBLK2/NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NCLN,NEN,LNE,NBEGDEC02015          
     1,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQC,NREGP,LGTX,JBEG,NDDEC02016          
     2ECX,LITR,NEGY,LNY,LHY,KBEG,NDECY,XM,IC,EFF1,EFF2,NOS,X,NMAC       DEC02017          
      COMMON /CMBLK3/IJOB,NAB,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F,NPR,IRAN,MDEC02018          
     1POP,LIOP,IAD,REP,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,NOM,NON,ML,MLB,NSDEC02019          
     2,NOT,DF,KPUT,SDF,EDF,WK,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,RR       DEC02020          
     3,IN,NSKF,LSKF,LBEG,IREJ                                           M0202020          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DEC02021          
     1L,L7                                                              DEC02022          
      INTEGER BLANK,CUBIC,QUAD,RGRSN                                    M0102023          
      DATA (BLANK=6H      )                                             M0102024          
      DATA (CUBIC=6HCUBIC )                                             M0102024          
      DATA (MU=6HMU    )                                                M0102024          
      DATA (QUAD=6HQUAD  )                                              M0102024          
      DATA (RGRSN=6HRGRSN )                                             M0102024          
      DATA (LINEAR=6HLINEAR)                                            M0102024          
C     ----------------------------------------------------              DEC02026          
C     READS AND LISTS PARAMETER CARD 1.  NOTE THAT N3F, NSIR, N2FR, N3FRDEC02027          
C     AND NSPIR ARE NOT USED IN THE PRESENT PROGRAM                     DEC02028          
C     ----------------------------------------------------              DEC02029          
    1 READ  (5,1000) IJOB,NAB,NCD,ICN1,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F, DEC02030          
     1NPR,NLC,NCPR,IRAN,MPOP,LIOP,IN                                    M0202031          
 1000 FORMAT (36I2)                                                     DEC02032          
      WRITE (6,1001) IJOB                                               DEC02033          
 1001 FORMAT (1H1,20X,42HLISTING OF PARAMETER CARDS FOR PROBLEM NO.,I3) DEC02034          
      WRITE (6,1002) IJOB,NAB,NCD,ICN1,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F, DEC02035          
     1NPR,NLC,NCPR,IRAN,MPOP,LIOP,IN                                    M0202036          
 1002 FORMAT (1H0, 88HIJOB NAB  NCD  ICN1 NLHM NRHM NMEA NME NNEA  NNE  M0202037          
     1N2F  NPR  NLC NCPR IRAN MPOP LIOP   IN/1H  I3,I4,16I5)            M0202038          
C     ----------------------------------------------------              M0202038          
C     READS AND LISTS PARAMETER CARD(S) 1A.                             M0202038          
C     ----------------------------------------------------              M0202038          
      IF(IN) 200,200,201                                                M0202038          
  200 IN=05                                                             M0202038          
      NSKF=0                                                            M0202038          
      GO TO 202                                                         M0202038          
  201 READ(5,1100) NSKF,(LSKF(I),LBEG(I),IREJ(I),I=1,NSKF)              M0202038          
 1100 FORMAT(I2,(I2,I3,I10,I2,I3,I10,I2,I3,I10,I2,I3,I10,I2,I3,I10))    M0202038          
      DO 203 I=1,NSKF                                                   M0202038          
  203 WRITE(6,1101) I,LSKF(I),LBEG(I),IREJ(I)                           M0202038          
 1101 FORMAT(1H0,8HFOR I = ,I2,29H  LSKF(I)  LBEG(I)    IREJ(I)/1H ,10X,M0202038          
     12I9,I11)                                                          M0202038          
  202 CONTINUE                                                          M0202038          
C     ----------------------------------------------------              DEC02039          
C     READS CONTROL IDENTIFICATION FROM PARAMETER CARDS IF ABSORPTION   DEC02040          
C                   IS TO OCCUR                                         DEC02041          
C     ----------------------------------------------------              DEC02042          
      K8=1                                                              DEC02043          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 4                     DEC02044          
      IM(1)=1                                                           DEC02045          
      LAB1(1)=MU                                                        DEC02046          
      LAB2(1)=BLANK                                                     DEC02047          
      LAB3(1)=BLANK                                                     DEC02048          
      LAB4(1)=BLANK                                                     DEC02049          
      K8=2                                                              DEC02050          
    4 IF (NAB.NE.2) GO TO 5                                             DEC02051          
      READ  (5,1000) IAD,NCC,(NDC(I),I=1,NCC)                           DEC02052          
      WRITE (6,1003) IAD,NCC,(NDC(I),I=1,NCC)                           DEC02053          
 1003 FORMAT (1H0,36HIAD  NCC  NDC(I) WHERE I=1,2,...NCC./1H ,23(I2,3X))DEC02054          
    5 IF (NAB.NE.3) GO TO 6                                             DEC02055          
      READ  (5,1004) IAD,REP,NCC,(NDC(I),I=1,NCC)                       DEC02056          
      WRITE (6,1005) IAD,REP,NCC,(NDC(I),I=1,NCC)                       DEC02057          
 1004 FORMAT (I2,F8.6,31I2)                                             DEC02058          
 1005 FORMAT (1H0,43HIAD    REP   NCC  NDC(I) WHERE I=1,2,...NCC/1H ,   DEC02059          
     1I2,2X,F8.6,I4,20I5)                                               DEC02060          
      RR=REP                                                            DEC02061          
      REP=(1.-REP)/REP                                                  DEC02062          
    6 IF (NAB.NE.4) GO TO 13                                            DEC02063          
      READ  (5,1004) IAD,REP,NMJC,(NDC(I),I=1,NMJC),NMIC,(NMI(I),I=1,NMIDEC02064          
     1C)                                                                DEC02065          
      WRITE (6,1006) IAD,REP,NMJC,(NDC(I),I=1,NMJC)                     DEC02066          
 1006 FORMAT (1H0,46HIAD    REP   NMJC  NDC(I) WHERE I=1,2,...NMJC. /   DEC02068          
     11H ,I2,2X,F8.6,I4,20I5)                                           DEC02069          
      WRITE (6,1007) NMIC,(NMI(I),I=1,NMIC)                             DEC02070          
 1007 FORMAT (1H0,33HNMIC  NMI(I) WHERE I=1,2,...NMIC. /1H ,1X,15(I2,3X)DEC02071          
     1)                                                                 DEC02072          
      RR=REP                                                            DEC02073          
      REP=(1.-REP)/REP                                                  DEC02074          
      LGT=10**NMIC                                                      DEC02075          
C     ----------------------------------------------------              DEC02076          
C     READS AND LISTS PARAMETER CARDS FOR MAIN EFFECTS                  DEC02077          
C     ----------------------------------------------------              DEC02078          
   13 K1=1                                                              DEC02079          
      K2=0                                                              DEC02080          
      IF (NMEA.EQ.0) GO TO 10                                           DEC02081          
      DO 8 I=1,NMEA                                                     DEC02082          
      READ  (5,1008)   MEN(I),LIT(I),K7,MPOL,LME(I),IBEG(I),(NOS(J),J=1,DEC02083          
     1K7)                                                               DEC02084          
 1008 FORMAT (I2,A6,I2,2I1,I2,16I4,2X/(20I4))                           DEC02085          
      WRITE (6,1009) I,MEN(I),LIT(I),K7,MPOL,LME(I),IBEG(I)             DEC02086          
 1009 FORMAT (1H0,6HFOR I=,I4,47H  MEN(I)  LIT(I)  NCL(I)  MPOL  LME(I) DEC02087          
     1 IBEG(I)/1H ,12X,I2,6X,A6,2X,I2,6X,I2,4X,I2,7X,I2)                DEC02088          
      NCL(I)=K7                                                         DEC02089          
      WRITE (6,1010) (NOS(J),J=1,K7)                                    DEC02090          
 1010 FORMAT (1H0,72HIDEN(J) WHERE J=K2 BY STEPS OF ONE AS LONG AS J IS DEC02091          
     1LESS THAN K2+NCL(I)+1/(1H ,I5,5X,I4,3X,20I5))                     DEC02092          
      DO 7 J=1,K7                                                       DEC02093          
      K3=K1+J-1                                                         DEC02094          
      IDEN(K3)=NOS(J)                                                   DEC02095          
    7 NOS(J)=0                                                          DEC02096          
      K1=K1+K7                                                          DEC02097          
    8 K2=K2+K7                                                          DEC02098          
   10 ML=K2                                                             DEC02099          
      NOM=NME+NMEA                                                      DEC02100          
      IF (NME.EQ.0) GO TO 12                                            DEC02101          
      K3=NMEA+1                                                         DEC02102          
      DO 11 I=K3,NOM                                                    DEC02103          
      READ  (5,1008)   MEN(I),LIT(I),K7,MPOL,LME(I),IBEG(I),(NOS(J),J=1,DEC02104          
     1K7)                                                               DEC02105          
      WRITE (6,1009) I,MEN(I),LIT(I),K7,MPOL,LME(I),IBEG(I)             DEC02106          
      WRITE (6,1010) (NOS(J),J=1,K7)                                    DEC02107          
      NCL(I)=K7                                                         DEC02108          
      DO 9 J=1,K7                                                       DEC02109          
      K3=K1+J-1                                                         DEC02110          
      IDEN(K3)=NOS(J)                                                   DEC02111          
    9 NOS(J)=0                                                          DEC02112          
      IF (K8.NE.1) GO TO 17                                             DEC02113          
      IM(K8)=NCL(I)-1                                                   DEC02114          
      K5=1                                                              DEC02115          
      GO TO 18                                                          DEC02116          
   17 IM(K8)=NCL(I)-1+IM(K8-1)                                          DEC02117          
      K5=IM(K8-1)+1                                                     DEC02118          
C     ----------------------------------------------------              DEC02119          
C     SETS UP BEGINNING AND ENDING ROW NUMBERS FOR MAIN EFFECTS FOR     DEC02120          
C     WHICH POLYNOMIALS ARE TO BE FITTED                                DEC02121          
C     ----------------------------------------------------              DEC02122          
   18 IF (MPOL.EQ.0) GO TO 99                                           DEC02123          
      NSME=NSME+1                                                       DEC02124          
      IPL(NSME)=MEN(I)                                                  DEC02125          
      NSP(NSME)=K5                                                      DEC02126          
      NND(NSME)=IM(K8)                                                  DEC02127          
      K6=NCL(I)                                                         DEC02128          
      DO 100 J=1,K6                                                     DEC02129          
      K=NCAS+J                                                          DEC02130          
      L=K1+J-1                                                          DEC02131          
  100 XP(K)=IDEN(L)                                                     DEC02132          
      NCAS=NCAS+NCL(I)                                                  DEC02133          
C     ----------------------------------------------------              DEC02134          
C     SETS UP LAB ARRAYS FOR LISTING LATER                              DEC02135          
C     ----------------------------------------------------              DEC02136          
   99 K=K1                                                              DEC02137          
      K6=IM(K8)                                                         DEC02138          
      DO 15 J=K5,K6                                                     DEC02139          
      LAB1(J)=LIT(I)                                                    DEC02140          
      CALL ITOA (IDEN(K),LAB2(J),4)                                     DEC02141          
      LAB3(J)=BLANK                                                     DEC02142          
      LAB4(J)=BLANK                                                     DEC02143          
   15 K=K+1                                                             DEC02144          
      K1=K1+NCL(I)                                                      DEC02145          
      K2=K2+NCL(I)                                                      DEC02146          
   11 K8=K8+1                                                           DEC02147          
C     ----------------------------------------------------              DEC02148          
C     READS AND LISTS PARAMETER CARDS FOR NESTED MAIN EFFECTS           DEC02149          
C     ----------------------------------------------------              DEC02150          
   12 K1=1                                                              DEC02151          
      K2=0                                                              DEC02152          
      IF (NNEA.EQ.0) GO TO 20                                           DEC02153          
      DO 19 I=1,NNEA                                                    DEC02154          
      READ  (5,1011)   NMA(I),NMAC(I),NEN(I),NLIT(I),K7,MPOL,LNE(I),NBEGDEC02155          
     1(I),(NOS(J),J=1,K7)                                               DEC02156          
 1011 FORMAT (3I2,A6,I2,2I1,I2,15I4,2X/(20I4))                          DEC02157          
      WRITE (6,1012) I,NMA(I),NMAC(I),NEN(I),NLIT(I),K7,MPOL,LNE(I),NBEGDEC02158          
     1(I)                                                               DEC02159          
 1012 FORMAT (1H0,6HFOR I=,I4,66H  NMA(I)  NMAC(I)  NEN(I)  NLIT(I)  NCLDEC02160          
     1N(I)  MPOL  LNE(I)  NBEG(I)/ 1H ,13X,I2,I9,I9,5X,A6,I5,I9,I6,I8)  DEC02161          
      WRITE (6,1013) K1,K2,K7,(NOS(J),J=1,K7)                           DEC02162          
 1013 FORMAT (1H0,16HNDEN(J) WHERE J=,I4,45H  INCREMENTED BY ONE UNTIL JDEC02163          
     1 IS GREATER THAN ,I4,1H+,I4/(1H ,19(I4,2X)))                      DEC02164          
      NCLN(I)=K7                                                        DEC02165          
      DO 14 J=1,K7                                                      DEC02166          
      K3=K1+J-1                                                         DEC02167          
      NDEN(K3)=NOS(J)                                                   DEC02168          
   14 NOS(J)=0                                                          DEC02169          
      K1=K1+K7                                                          DEC02170          
   19 K2=K2+K7                                                          DEC02171          
   20 MLB=K2                                                            DEC02172          
      NON=NNEA+NNE                                                      DEC02173          
      IF (NNE.EQ.0) GO TO 25                                            DEC02174          
      K3=NNEA+1                                                         DEC02175          
      DO 24 I=K3,NON                                                    DEC02176          
      READ  (5,1011)   NMA(I),NMAC(I),NEN(I),NLIT(I),K7,MPOL,LNE(I),NBEGDEC02177          
     1(I),(NOS(J),J=1,K7)                                               DEC02178          
      WRITE (6,1012) I,NMA(I),NMAC(I),NEN(I),NLIT(I),K7,MPOL,LNE(I),NBEGDEC02179          
     1(I)                                                               DEC02180          
      WRITE (6,1013) K1,K2,K7,(NOS(J),J=1,K7)                           DEC02181          
      NCLN(I)=K7                                                        DEC02182          
      DO 22 J=1,K7                                                      DEC02183          
      K3=K1+J-1                                                         DEC02184          
      NDEN(K3)=NOS(J)                                                   DEC02185          
   22 NOS(J)=0                                                          DEC02186          
      IF (K8.NE.1) GO TO 31                                             DEC02187          
      IM(K8)=NCLN(I)-1                                                  DEC02188          
      K5=1                                                              DEC02189          
      GO TO 32                                                          DEC02190          
   31 IM(K8)=NCLN(I)-1+IM(K8-1)                                         DEC02191          
      K5=IM(K8-1)+1                                                     DEC02192          
C     ----------------------------------------------------              DEC02193          
C     SETS UP ARRAYS NEEDED WHEN POLYNOMIALS ARE TO BE FITTED           DEC02194          
C     ----------------------------------------------------              DEC02195          
   32 IF (MPOL.EQ.0) GO TO 98                                           DEC02196          
      NSME=NSME+1                                                       DEC02197          
      IPL(NSME)=NEN(I)                                                  DEC02198          
      NSP(NSME)=K5                                                      DEC02199          
      NND(NSME)=IM(K8)                                                  DEC02200          
      K6=NCLN(I)                                                        DEC02201          
      DO 101 J=1,K6                                                     DEC02202          
      K=NCAS+J                                                          DEC02203          
      L=K1+J-1                                                          DEC02204          
  101 XP(K)=NDEN(L)                                                     DEC02205          
      NCAS=NCAS+NCLN(I)                                                 DEC02206          
C     ----------------------------------------------------              DEC02207          
C     SETS UP LAB ARRAYS TO IDENTIFY LISTINGS                           DEC02208          
C     ----------------------------------------------------              DEC02209          
   98 K=K1                                                              DEC02210          
      K6=IM(K8)                                                         DEC02211          
      DO 23 J=K5,K6                                                     DEC02212          
      LAB1(J)=NLIT(I)                                                   DEC02213          
      CALL ITOA (NDEN(K),LAB2(J),4)                                     DEC02214          
      LAB3(J)=BLANK                                                     DEC02215          
      LAB4(J)=BLANK                                                     DEC02216          
   23 K=K+1                                                             DEC02217          
      K8=K8+1                                                           DEC02218          
      K1=K1+K7                                                          DEC02219          
   24 K2=K2+K7                                                          DEC02220          
C     ----------------------------------------------------              DEC02221          
C     READS AND LISTS PARAMETER CARDS FOR TWO-FACTOR INTERACTION EFFECTSDEC02222          
C     ----------------------------------------------------              DEC02223          
   25 K1=1                                                              DEC02224          
      K2=0                                                              DEC02225          
      IEI=IM(K8-1)+1                                                    DEC02226          
      IF (K8.EQ.1) IEI=1                                                DEC02227          
      IF (N2F.EQ.0) GO TO 50                                            DEC02228          
      DO 49 I=1,N2F                                                     DEC02229          
      READ  (5,1014)   INT1(I),INT2(I),K7,(NOS(J),J=1,K7)               DEC02230          
 1014 FORMAT (3I2,16I4,2X/(18I4))                                       DEC02231          
      WRITE (6,1015) I,INT1(I),INT2(I),K7                               DEC02232          
 1015 FORMAT (1H0,6HFOR I=,I4,25H  INT1(I) INT2(I)  NMC(I)/ 1H ,12X,3(I2DEC02233          
     1,6X))                                                             DEC02234          
      IF (K7.NE.0) WRITE (6,1016) K1,K2,K7,(NOS(J),J=1,K7)              DEC02235          
 1016 FORMAT (1H0,16HMSCL(J) WHERE J=,I4,45H  INCREMENTED BY ONE UNTIL JDEC02236          
     1 IS GREATER THAN ,I4,1H+,I4/(1H ,I5,I9,12I8))                     DEC02237          
      NMC(I)=K7                                                         DEC02238          
      DO 26 J=1,K7                                                      DEC02239          
      K3=K1+J-1                                                         DEC02240          
      MSCL(K3)=NOS(J)                                                   DEC02241          
   26 NOS(J)=0                                                          DEC02242          
      NSUM=0                                                            DEC02243          
      MSUM=0                                                            DEC02244          
      IF (INT1(I).GT.NOM) GO TO 29                                      DEC02245          
      K6=INT1(I)-1                                                      DEC02246          
      IF (K6.EQ.0) GO TO 28                                             DEC02247          
      DO 27 J=1,K6                                                      DEC02248          
   27 NSUM=NSUM+NCL(J)                                                  DEC02249          
   28 K6=INT1(I)                                                        DEC02250          
      NDF1=NCL(K6)-1                                                    DEC02251          
      GO TO 34                                                          DEC02252          
   29 K6=INT1(I)-NOM                                                    DEC02253          
      NDF1=NCLN(K6)-1                                                   DEC02254          
      K3=NOM+1                                                          DEC02255          
      K4=INT1(I)-1                                                      DEC02256          
      IF (K4.LT.K3) GO TO 34                                            DEC02257          
      DO 30 J=K3,K4                                                     DEC02258          
      K=J-NOM                                                           DEC02259          
   30 NSUM=NSUM+NCLN(K)                                                 DEC02260          
   34 IF (INT2(I).GT.NOM) GO TO 38                                      DEC02261          
      K4=INT2(I)-1                                                      DEC02262          
      IF (K4.EQ.0) GO TO 36                                             DEC02263          
      DO 35 J=1,K4                                                      DEC02264          
   35 MSUM=MSUM+NCL(J)                                                  DEC02265          
   36 K6=INT2(I)                                                        DEC02266          
      NDF2=NCL(K6)-1                                                    DEC02267          
      GO TO 41                                                          DEC02268          
   38 K6=INT2(I)-NOM                                                    DEC02269          
      NDF2=NCLN(K6)-1                                                   DEC02270          
      K3=NOM+1                                                          DEC02271          
      K4=INT2(I)-1                                                      DEC02272          
      IF (K4.LT.K3) GO TO 41                                            DEC02273          
      DO 40 J=K3,K4                                                     DEC02274          
      K=J-NOM                                                           DEC02275          
   40 MSUM=MSUM+NCLN(K)                                                 DEC02276          
   41 IF (K8.NE.1) GO TO 52                                             DEC02277          
      IM(K8)=NDF1*NDF2-NMC(I)                                           DEC02278          
      J=1                                                               DEC02279          
      GO TO 53                                                          DEC02280          
   52 IM(K8)=IM(K8-1)+NDF1*NDF2-NMC(I)                                  DEC02281          
      J=IM(K8-1)+1                                                      DEC02282          
C     ----------------------------------------------------              DEC02283          
C     SETS UP LAB ARRAYS TO IDENTIFY LISTINGS                           DEC02284          
C     ----------------------------------------------------              DEC02285          
   53 K8=K8+1                                                           DEC02286          
      DO 48 K=1,NDF1                                                    DEC02287          
      DO 48 L=1,NDF2                                                    DEC02288          
      K6=NSUM+K                                                         DEC02289          
      K7=MSUM+L                                                         DEC02290          
      IF (NMC(I).EQ.0) GO TO 43                                         DEC02291          
      K5=K*100+L                                                        DEC02292          
      K3=0                                                              DEC02293          
      K4=K1-1                                                           DEC02294          
   42 K3=K3+1                                                           DEC02295          
      K4=K4+1                                                           DEC02296          
      IF (K3.GT.NMC(I)) GO TO 43                                        DEC02297          
      IF (K5.NE.MSCL(K4)) GO TO 42                                      DEC02298          
      GO TO 48                                                          DEC02299          
   43 IF (INT1(I).GT.NOM) GO TO 44                                      DEC02300          
      N=INT1(I)                                                         DEC02301          
      LAB1(J)=LIT(N)                                                    DEC02302          
      CALL ITOA (IDEN(K6),LAB3(J),4)                                    DEC02303          
      GO TO 45                                                          DEC02304          
   44 N=INT1(I)-NOM                                                     DEC02305          
      LAB1(J)=NLIT(N)                                                   DEC02306          
      CALL ITOA (NDEN(K6),LAB3(J),4)                                    DEC02307          
   45 IF (INT2(I).GT.NOM) GO TO 46                                      DEC02308          
      N=INT2(I)                                                         DEC02309          
      LAB2(J)=LIT(N)                                                    DEC02310          
      CALL ITOA (IDEN(K7),LAB4(J),4)                                    DEC02311          
      GO TO 47                                                          DEC02312          
   46 N=INT2(I)-NOM                                                     DEC02313          
      LAB2(J)=NLIT(N)                                                   DEC02314          
      CALL ITOA (NDEN(K7),LAB4(J),4)                                    DEC02315          
   47 J=J+1                                                             DEC02316          
   48 CONTINUE                                                          DEC02317          
      K1=K1+NMC(I)                                                      DEC02318          
   49 K2=K2+NMC(I)                                                      DEC02319          
C     ----------------------------------------------------              DEC02320          
C     READS AND LISTS PARAMETER CARDS FOR POOLED REGRESSIONS            DEC02321          
C     ----------------------------------------------------              DEC02322          
   50 IE=IM(K8-1)+1                                                     DEC02323          
      IF (K8.EQ.1) IE=1                                                 DEC02324          
      IF (NPR.EQ.0) GO TO 65                                            DEC02325          
      DO 64 K=1,NPR                                                     DEC02326          
      READ  (5,1017)   NEGX(K),LOGE(K),LQC(K),NREGP(K),LGTX(K),JBEG(K), DEC02327          
     1NDECX(K),XM(K),LITR(K)                                            DEC02328          
 1017 FORMAT (2I1,3I2,I3,I2,F10.5,A6)                                   DEC02329          
      WRITE (6,1018) K,NEGX(K),LOGE(K),LQC(K),NREGP(K),LGTX(K),JBEG(K), DEC02330          
     1NDECX(K),XM(K),LITR(K)                                            DEC02331          
 1018 FORMAT (1H0,6HFOR K=,I2,72H  NEGX(K) LOGE(K) LQC(K) NREGP(K) LGTX(DEC02332          
     1K) JBEG(K) NDECX(K) XM(K) LITR(K) /1H ,12X,I2,3I8,I7,2I9,2X,F10.5,DEC02333          
     22X,A6)                                                            DEC02334          
C     ----------------------------------------------------              DEC02335          
C     SETS UP LAB ARRAYS TO IDENTIFY LISTINGS                           DEC02336          
C     ----------------------------------------------------              DEC02337          
      K3=LQC(K)                                                         DEC02338          
      DO 63 J=1,K3                                                      DEC02339          
      IM(K8)=IM(K8-1)+1                                                 DEC02340          
      IF (K8.EQ.1) IM(K8)=1                                             DEC02341          
      I=IM(K8)                                                          DEC02342          
      K8=K8+1                                                           DEC02343          
      LAB1(I)=RGRSN                                                     DEC02344          
      LAB2(I)=LITR(K)                                                   DEC02345          
      LAB4(I)=BLANK                                                     DEC02346          
      IF (J-2) 60,61,62                                                 DEC02347          
   60 LAB3(I)=LINEAR                                                    DEC02348          
      GO TO 63                                                          DEC02349          
   61 LAB3(I)=QUAD                                                      DEC02350          
      GO TO 63                                                          DEC02351          
   62 LAB3(I)=CUBIC                                                     DEC02352          
   63 I=I+1                                                             DEC02353          
   64 IF (LOGE(K).EQ.1) XM(K)=ALOG10(XM(K))                             DEC02354          
C     ----------------------------------------------------              DEC02355          
C     READS AND LISTS PARAMETER CARDS FOR RHM                           DEC02356          
C     ----------------------------------------------------              DEC02357          
   65 IF (NRHM.EQ.0) GO TO 67                                           DEC02358          
      DO 66 I=1,NRHM                                                    DEC02359          
      READ  (5,1019)   NEGY(I),LNY(I),LHY(I),KBEG(I),NDECY(I),YM(I),LITYDEC02360          
     1(I)                                                               DEC02361          
 1019 FORMAT (2I1,I2,I3,I2,F10.5,A6)                                    DEC02362          
      WRITE (6,1020) I,NEGY(I),LNY(I),LHY(I),KBEG(I),NDECY(I),YM(I),LITYDEC02363          
     1(I)                                                               DEC02364          
 1020 FORMAT (1H0,6HFOR I=,I2,54H  NEGY(I) LNY(I) LHY(I) KBEG(I) NDECY(IDEC02365          
     1) YM(I) LITY(I)/ 1H ,12X,I2,2I7,2I8,2X,F10.5,1X,A6)               DEC02366          
   66 IF (LNY(I).EQ.1) YM(I)=ALOG10(YM(I))                              DEC02367          
   67 IF (IRAN.EQ.0) GO TO 68                                           DEC02368          
      READ  (5,1000) I309,NR1,NW,NS2,(MS(I),I=1,NS2)                    DEC02369          
      WRITE (6,1021) I309,NR1,NW,NS2,(MS(I),I=1,NS2)                    DEC02370          
 1021 FORMAT (1H0,56H  I309 NR1   NW  NS2  MS(1) MS(2) MS(3) MS(4) MS(5)DEC02371          
     1 ETC./(1H ,23I5))                                                 DEC02372          
   68 CONTINUE                                                          DEC02373          
      IF (N2F.EQ.0) GO TO 75                                            DEC02374          
C     ----------------------------------------------------              DEC02375          
C     CHECKS FOR CONSECUTIVE NUMBERING OF ALL MAIN EFFECTS              DEC02376          
C     ----------------------------------------------------              DEC02377          
      J=1                                                               DEC02378          
      IF (NOM.EQ.0) GO TO 70                                            DEC02379          
      DO 69 I=1,NOM                                                     DEC02380          
      IF (MEN(I).NE.J) GO TO 900                                        DEC02381          
   69 J=J+1                                                             DEC02382          
   70 IF (NON.EQ.0) GO TO 75                                            DEC02383          
      DO 71 I=1,NON                                                     DEC02384          
      IF (NEN(I).NE.J) GO TO 900                                        DEC02385          
   71 J=J+1                                                             DEC02386          
      GO TO 75                                                          DEC02387          
  900 WRITE (6,1022) IJOB                                               DEC02388          
 1022 FORMAT (1H0,73HCODES FOR EFFECTS NOT CONSECUTIVE---CHECK PARAMETERDEC02389          
     1 CARDS FOR PROBLEM NO.,I3)                                        DEC02390          
      MULL=1                                                            DEC02391          
   75 CONTINUE                                                          DEC02392          
      NS=K8-1                                                           DEC02393          
      RETURN                                                            DEC02394          
      END                                                               DEC02395          
*ENDTEXT                                                                                  
