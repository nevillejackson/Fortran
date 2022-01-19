      SUBROUTINE POLYNO                                                 DK200010          
C     -------------------------------------------                       DK200020          
C     SUBROUTINE FOR COMPUTING POLYNOMIALS FOR CROSS CLASSIFIED OR      DK200030          
C     NESTED MAIN EFFECTS                                               DK200040          
C     ----------------------------------------------------              DK200050          
      EXTERNAL DSQRT                                                                      
      DIMENSION ARRAY(5000),SSCPR(630),SSS(630),RHM(2450),TOT(106),TOT2(DK200080          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100),NS2(15)    DK200090          
      DIMENSION FAB(5000),FY(630),TOT4(106),SAB(630),FSQ(525),TOT5(106) DK200100          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),MZ(10),IPL(13),NSDK200110          
     1P(13),NND(13),XP(81),YP(41),LP(5),NDC(10),NMI(10),MEN(20),NCL(20),DK200120          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(50),NCLN(50),LNE(50),NDEN(1                  
     300),NLIT(50),NMA(50),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(50),                  
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDK200150          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),                          
     6EFF1(50),EFF2(50),NOS(300),X(106),NMAC(50),I309(15),NR1(15),NW(15)                  
     7,R1I(100),R2I(100),WW(200),ICLR(90),IRM(40),LAD(90)               DK200180          
      DIMENSION PM(5)                                                   DK200190          
      DIMENSION IC(1000)                                                                  
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADK200200          
     1B4,LITY,TRED,YM,XP,YP,IM,MS,NEG,IPL,NSP,NND,FAB,FY,TOT4,SAB,FSQ,TODK200210          
     2T5,MZ,R1I,R2I,WW,ICLR,IRM,LAD                                     DK200220          
      COMMON /CMBLK2/LIT,LITR,NLIT,XM,EFF1,EFF2,X,NDC,NMI,MEN,NCL,LME,IBDK200230          
     1EG,IDEN,NCLN,NEN,LNE,NBEG,NDEN,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQDK200240          
     2C,NREGP,LGTX,JBEG,NDECX,NEGY,LNY,LHY,KBEG,NDECY,IC,NOS,NMAC       DK200250          
      COMMON /CMBLK3/RR,REP,DF,SDF,EDF,WK,IJOB,NAB,NLHM,NRHM,NMEA,NME,NNDK200260          
     1EA,NNE,N2F,NPR,IRAN,MPOP,LIOP,IAD,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,DK200270          
     2NOM,NON,ML,MLB,NS,NOT,KPUT,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,IN,I30DK200280          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DK200290          
     1L,L7,KA,KB,KC,SOF,NX,KD,MTY,MAN,NRUN,NRN,EDFF,NAME,EDFS,NAM5,IFP,NDK200300          
     2DFA,IFPC,NDFC,NDFAC,NINT,IBET,LBEG,LTHN,NNDC,WTT,SWT1             DK200310          
      DATA LP(1),LP(2),LP(3),LP(4),LP(5)/6HLINEAR,6HQUAD  ,6HCUBIC ,6HQUDK200320          
     1ARD ,6HQUIN  /                                                    DK200330          
      NCAS=0                                                            DK200340          
      WRITE (6,1000)                                                    DK200350          
 1000 FORMAT (1H0,20X,53HLISTING OF POLYNOMIAL REGRESSIONS AND STANDARD DK200360          
     1ERRORS)                                                           DK200370          
      WRITE (6,1001)                                                    DK200380          
 1001 FORMAT (1H0,29H RHM    HIGH ORDER   STANDARD,21X,38HNON-ORTHOGONALDK200390          
     1 REGRESSION COEFFICIENTS)                                         DK200400          
      WRITE (6,1002)                                                    DK200410          
 1002 FORMAT (1H ,27H NAME   REGRESSION    ERROR,9X,2HB0,9X,2HB1,10X,2HBDK200420          
     12,10X,2HB3,10X,2HB4,10X,2HB5)                                     DK200430          
      DO 73 K1=1,NSME                                                   DK200440          
      I2=NND(K1)-NSP(K1)+1                                              DK200450          
      DF=FLOAT(I2)                                                      DK200460          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DK200470          
C     DEFINING CONSTANTS FOR TRANSFORMATION MATRIX                      DK200480          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DK200490          
      SD=DF/(DF+1.)*DF/(DF+1.)                                          DK200500          
      F=DF/(DF+1.)*1.0/(DF+1.)*(-1.0)                                   DK200510          
      R=1.0/(DF+1.)*1.0/(DF+1.)                                         DK200520          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DK200530          
C     TRANSFORMATION OF Z-1 MATRIX TO OBTAIN MATRIX OF WEIGHTS          DK200540          
C     WHICH IS STORED IN RHM ARRAY                                      DK200550          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DK200560          
      DO 75 K3=1,I2                                                     DK200570          
      DO 75 K4=K3,I2                                                    DK200580          
      N2=NSP(K1)+K3-1                                                   DK200590          
      N3=NSP(K1)+K4-1                                                   DK200600          
      SUM=0.                                                            DK200610          
      K7=NSP(K1)                                                        DK200620          
      K6=NND(K1)                                                        DK200630          
      DO 76 I=K7,K6                                                     DK200640          
      DO 76 J=K7,K6                                                     DK200650          
      IF (I-J.LE.0) GO TO 79                                            DK200660          
      L=NLHM*(J-1)-J*(J-3)/2+I-J                                        DK200670          
      GO TO 82                                                          DK200680          
   79 L=NLHM*(I-1)-I*(I-3)/2+J-I                                        DK200690          
   82 IF (N2.EQ.J.AND.N3.EQ.I) GO TO 77                                 DK200700          
      IF (N3.EQ.I.AND.N2.NE.J.OR.N2.EQ.J.AND.N3.NE.I) GO TO 78          DK200710          
      SUM=SUM+ARRAY(L)*R                                                DK200720          
      GO TO 76                                                          DK200730          
   78 SUM=SUM+ARRAY(L)*F                                                DK200740          
      GO TO 76                                                          DK200750          
   77 SUM=SUM+ARRAY(L)*SD                                               DK200760          
   76 CONTINUE                                                          DK200770          
      L=I2*(K3-1)-K3*(K3-3)/2+K4-K3                                     DK200780          
   75 RHM(L)=SUM                                                        DK200790          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DK200800          
C     COMPUTATION OF MEAN OF P AND DEVIATIONS FROM THIS MEAN            DK200810          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DK200820          
      K3=NCAS+I2+1                                                      DK200830          
      SUM=0.                                                            DK200840          
      DO 83 I=1,I2                                                      DK200850          
      K2=NCAS+I                                                         DK200860          
   83 SUM=SUM+XP(K2)                                                    DK200870          
      SUM=(SUM+XP(K3))/(DF+1.0)                                         DK200880          
      AMNX=SUM                                                          DK200890          
      F=FLOAT(I2+1)                                                     DK200900          
      XP(K3)=XP(K3)-SUM                                                 DK200910          
      DO 80 I=2,5                                                       DK200920          
   80 PM(I)=XP(K3)**I/F                                                 DK200930          
      DO 130 I=1,I2                                                     DK200940          
      K2=NCAS+I                                                         DK200950          
      XP(K2)=XP(K2)-SUM                                                 DK200960          
      DO 130 II=2,5                                                     DK200970          
  130 PM(II)=PM(II)+(XP(K2)**II)/F                                      DK200980          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DK200990          
C     CALCULATION OF CONSTANT FOR THE LAST LEVEL FOR EACH RHM           DK201000          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DK201010          
      DO 87 I=1,NRHM                                                    DK201020          
      SUM=0.                                                            DK201030          
      DO 86 J=1,I2                                                      DK201040          
      K2=MATX+(I-1)*NLHM+NSP(K1)+J-1                                    DK201050          
   86 SUM=SUM+ARRAY(K2)*(-1.0)                                          DK201060          
   87 YP(I)=SUM                                                         DK201070          
      I3=I2*(I2+1)/2                                                    DK201080          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DK201090          
C     BEGINNING OF LOOP FOR STEPWISE POLYNOMIAL FITTING                 DK201100          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DK201110          
      DO 88 I=1,I2                                                      DK201120          
      IF (I.GT.5) GO TO 88                                              DK201130          
      WRITE (6,1004) LAB1(K7),LP(I),AMNX                                DK201140          
 1004 FORMAT (1H0,29X,A6,2X,A6,4X,11HMEAN OF X =,F10.4)                 DK201150          
      WRITE (6,1003)                                                    DK201160          
 1003 FORMAT (1H )                                                      DK201170          
      N2=I*(I+1)/2                                                      DK201180          
      N3=N2+I*NRHM                                                      DK201190          
      J=I3+1                                                            DK201200          
      K2=N3+I3                                                          DK201210          
      DO 90 K=J,K2                                                      DK201220          
   90 RHM(K)=0.                                                         DK201230          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DK201240          
C     CALCULATION OF WEIGHTED SS AND CP FOR POLYNOMIALS AND RHM         DK201250          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DK201260          
      DO 91 L=1,I                                                       DK201270          
      DO 91 K=L,I                                                       DK201280          
      DO 89 K6=1,I2                                                     DK201290          
      JJ=NCAS+K6                                                        DK201300          
      SMS=XP(K3)                                                        DK201310          
      SUM=XP(JJ)                                                        DK201320          
      IF (L.EQ.1) GO TO 140                                             DK201330          
      DO 93 L2=2,L                                                      DK201340          
      SUM=SUM*XP(JJ)                                                    DK201350          
   93 SMS=SMS*XP(K3)                                                    DK201360          
  140 SUM=SUM-SMS                                                       DK201370          
      DO 89 K2=1,I2                                                     DK201380          
      JJ=NCAS+K2                                                        DK201390          
      SMS=XP(K3)                                                        DK201400          
      TEMP=XP(JJ)                                                       DK201410          
      IF (K.EQ.1) GO TO 141                                             DK201420          
      DO 94 L2=2,K                                                      DK201430          
      TEMP=TEMP*XP(JJ)                                                  DK201440          
   94 SMS=SMS*XP(K3)                                                    DK201450          
  141 TEMP=TEMP-SMS                                                     DK201460          
      IF (K6-K2.LE.0) GO TO 95                                          DK201470          
      K11=I2*(K2-1)-K2*(K2-3)/2+K6-K2                                   DK201480          
      GO TO 96                                                          DK201490          
   95 K11=I2*(K6-1)-K6*(K6-3)/2+K2-K6                                   DK201500          
   96 RHM(J)=RHM(J)+SUM*RHM(K11)*TEMP                                   DK201510          
      IF (L-K.NE.0) GO TO 89                                            DK201520          
      DO 97 K4=1,NRHM                                                   DK201530          
      K5=MATX+(K4-1)*NLHM+NSP(K1)+K2-1                                  DK201540          
      JJ=N2+I*(K4-1)+L+I3                                               DK201550          
   97 RHM(JJ)=RHM(JJ)+SUM*RHM(K11)*(ARRAY(K5)-YP(K4))                   DK201560          
   89 CONTINUE                                                          DK201570          
   91 J=J+1                                                             DK201580          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DK201590          
C     INVERSION OF SS AND CP MATRIX                                     DK201600          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DK201610          
      DO 110 K=1,I                                                      DK201620          
      K2=I*(K-1)-K*(K-3)/2+I3                                           DK201630          
      RECIP=1./RHM(K2)                                                  DK201640          
      RHM(K2)=-RECIP                                                    DK201650          
      DO 110 L=1,I                                                      DK201660          
      K11=I*(L-1)-L*(L-3)/2+I3                                          DK201670          
      IF (L-K) 111,110,112                                              DK201680          
  111 K4=K11+K-L                                                        DK201690          
      GO TO 113                                                         DK201700          
  112 K4=K2+L-K                                                         DK201710          
  113 R=RHM(K4)*RECIP                                                   DK201720          
      DO 114 J=L,I                                                      DK201730          
      K5=K11+J-L                                                        DK201740          
      IF (J-K) 115,114,116                                              DK201750          
  115 K6=I*(J-1)-J*(J-3)/2+K-J+I3                                       DK201760          
      GO TO 117                                                         DK201770          
  116 K6=K2+J-K                                                         DK201780          
  117 RHM(K5)=RHM(K5)-R*RHM(K6)                                         DK201790          
  114 CONTINUE                                                          DK201800          
      RHM(K4)=R                                                         DK201810          
  110 CONTINUE                                                          DK201820          
      DO 118 J=1,I                                                      DK201830          
      DO 118 K=J,I                                                      DK201840          
      K2=I*(J-1)-J*(J-3)/2+K-J+I3                                       DK201850          
  118 RHM(K2)=-RHM(K2)                                                  DK201860          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DK201870          
C     COMPUTATION OF B VALUES, STANDARD ERRORS AND PRINTING OF RESULTS  DK201880          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DK201890          
      DO 120 J=1,NRHM                                                   DK201900          
      DO 122 K=1,I                                                      DK201910          
      TEMP=0.                                                           DK201920          
      DO 121 L=1,I                                                      DK201930          
      IF (K-L.LT.0) GO TO 123                                           DK201940          
      K2=I*(L-1)-L*(L-3)/2+K-L+I3                                       DK201950          
      GO TO 124                                                         DK201960          
  123 K2=I*(K-1)-K*(K-3)/2+L-K+I3                                       DK201970          
  124 K4=I3+N2+I*(J-1)+L                                                DK201980          
  121 TEMP=TEMP+RHM(K4)*RHM(K2)                                         DK201990          
  122 SSS(K)=TEMP                                                       DK202000          
      K=J*5+I+(K1-1)*NRHM*5                                             DK202010          
      K5=I3+N2                                                          DK202020          
      SSS(K)=(SSS(I)*SSS(I))/RHM(K5)                                    DK202030          
      IF (NCPR.EQ.0) GO TO 125                                          DK202040          
      K2=NRHM*(J-1)-J*(J-3)/2                                           DK202050          
      TEMP=SSCPR(K2)                                                    DK202060          
      GO TO 126                                                         DK202070          
  125 IF (I30.EQ.1) GO TO 127                                           DK202080          
      TEMP=(SSCPR(J)-TRED(J))/EDF                                       DK202090          
      GO TO 126                                                         DK202100          
  127 TEMP=SSCPR(J)                                                     DK202110          
  126 SD=0.0                                                            DK202120          
      IF (TEMP*RHM(K5).LE.0.0) GO TO 128                                DK202130          
      SD=DSQRT(DIV(TEMP*RHM(K5)))                                       DK202140          
  128 IF(NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 132                    DK202150          
      K=MATX+(J-1)*NLHM+1                                               DK202160          
      RECIP=ARRAY(K)+YM(J)                                              DK202170          
      GO TO 133                                                         DK202180          
  132 K=NLHM+J                                                          DK202190          
      RECIP=TOT(K)/FLOAT(NCDS)+YM(J)                                    DK202200          
      IF (IBET.EQ.1) RECIP=TOT(K)/SWT1+YM(J)                            DK202210          
  133 IF (I.EQ.1) GO TO 120                                             DK202220          
      DO 134 II=2,I                                                     DK202230          
  134 RECIP=RECIP-SSS(II)*PM(II)                                        DK202240          
  120 WRITE (6,1005) LITY(J),SSS(I),SD,RECIP,(SSS(K),K=1,I)             DK202250          
 1005 FORMAT (1H ,A6,8(1X,E11.4))                                       DK202260          
   88 CONTINUE                                                          DK202270          
   73 NCAS=NCAS+I2+1                                                    DK202280          
      DF=EDF+FLOAT(NLHM)                                                DK202290          
      RETURN                                                            DK202300          
      END                                                               DK202310          
