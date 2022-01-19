*TEXT                                                                                     
      SUBROUTINE LSCOMB                                                 DK170010          
C     -------------------------------------------                       DK170020          
C     SUBROUTINE WHICH CALLS MATINV, COMPUTES AND LISTS CONSTANTS.      DK170030          
C     ALSO CALLS POLYNO AND SVCVC IF NEEDED                             DK170040          
C     ----------------------------------------------------              DK170050          
      IMPLICIT REAL*8(A-H,O-Z)                                          DK170060          
      REAL*8 LAB1,LAB2,LAB3,LAB4,LITY,NLIT,LIT,LITR,NAME,NAM5           DK170070          
      DIMENSION ARRAY(5000),SSCPR(630),SSS(630),RHM(2450),TOT(106),TOT2(DK170080          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100),NS2(15)    DK170090          
      DIMENSION FAB(5000),FY(630),TOT4(106),SAB(630),FSQ(525),TOT5(106) DK170100          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),MZ(10),IPL(13),NSDK170110          
     1P(13),NND(13),XP(81),YP(41),      NDC(10),NMI(10),MEN(20),NCL(20),DK170120          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(40),NCLN(40),LNE(40),NDEN(1DK170130          
     300),NLIT(40),NMA(30),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(40),DK170140          
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDK170150          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),IC(480),DK170160          
     6EFF1(50),EFF2(50),NOS(300),X(106),NMAC(40),I309(15),NR1(15),NW(15)DK170170          
     7,R1I(50),R2I(50)                                                  DK170180          
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADK170190          
     1B4,LITY,TRED,YM,XP,YP,IM,MS,NEG,IPL,NSP,NND,FAB,FY,TOT4,SAB,FSQ,TODK170200          
     2T5,MZ,R1I,R2I                                                     DK170210          
      COMMON /CMBLK2/LIT,LITR,NLIT,XM,EFF1,EFF2,X,NDC,NMI,MEN,NCL,LME,IBDK170220          
     1EG,IDEN,NCLN,NEN,LNE,NBEG,NDEN,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQDK170230          
     2C,NREGP,LGTX,JBEG,NDECX,NEGY,LNY,LHY,KBEG,NDECY,IC,NOS,NMAC       DK170240          
      COMMON /CMBLK3/RR,REP,DF,SDF,EDF,WK,IJOB,NAB,NLHM,NRHM,NMEA,NME,NNDK170250          
     1EA,NNE,N2F,NPR,IRAN,MPOP,LIOP,IAD,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,DK170260          
     2NOM,NON,ML,MLB,NS,NOT,KPUT,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,IN,I30DK170270          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DK170280          
     1L,L7,KA,KB,KC,SOF,NX,KD,MTY,MAN,NRUN,NRN,EDFF,NAME,EDFS,NAM5,IFP,NDK170290          
     2DFA,IFPC,NDFC,NDFAC,NINT                                          DK170300          
C     ----------------------------------------------------              DK170310          
C     INVERSION AND LISTING OF INVERSE MATRIX                           DK170320          
C     ----------------------------------------------------              DK170330          
      NBRC=1                                                            DK170340          
      NERC=NLHM                                                         DK170350          
      K=1                                                               DK170360          
      IF (NLHM.EQ.0) GO TO 9                                            DK170370          
      DO 18 I=1,NLHM                                                    DK170380          
      X(I)=ARRAY(K)                                                     DK170390          
   18 K=K+NLHM-I+1                                                      DK170400          
      DET=1.0                                                           DK170410          
      CALL MATINV (ARRAY,NBRC,NERC,NLHM,X,DET)                          DK170420          
   24 IF (LIOP.EQ.20) GO TO 19                                          DK170430          
      WRITE (6,1001) IJOB                                               DK170440          
      WRITE (6,1004)                                                    DK170450          
      IF (LIOP.NE.10) GO TO 26                                          DK170460          
      WRITE (6,1025) (ARRAY(I), I=1,MATX)                               DK170470          
      WRITE (6,1022)                                                    DK170480          
 1022 FORMAT (1H0,29X,36HLISTING OF DIAGONAL INVERSE ELEMENTS)          DK170490          
      WRITE (6,1004)                                                    DK170500          
      K=1                                                               DK170510          
      DO 25 I=1,NLHM                                                    DK170520          
      X(I)=ARRAY(K)                                                     DK170530          
   25 K=K+NLHM-I+1                                                      DK170540          
      WRITE (6,1025) (X(K),K=1,NLHM)                                    DK170550          
      GO TO 19                                                          DK170560          
 1025 FORMAT (7(2X,E15.8))                                              DK170570          
 1001 FORMAT (1H0,29X,43HLISTING OF INVERSE ELEMENTS FOR PROBLEM NO.,I3)DK170580          
   26 WRITE (6,1002)                                                    DK170590          
 1002 FORMAT (1H0,8HROW  COL,15X,21HINDEPENDENT VARIABLES,34X,15HINVERSEDK170600          
     1 ELEMENT)                                                         DK170610          
      WRITE (6,1003)                                                    DK170620          
 1003 FORMAT (1H ,9HCODE CODE,9X,3HROW,22X,6HCOLUMN,16X,41HFIXED POINT FDK170630          
     1ORMAT  FLOATING POINT FORMAT)                                     DK170640          
   28 DO 32 I=NBRC,NERC                                                 DK170650          
      WRITE (6,1004)                                                    DK170660          
 1004 FORMAT (1H )                                                      DK170670          
   30 DO 32 J=I,NERC                                                    DK170680          
      K1=NLHM*(I-1)-I*(I-3)/2+J-I                                       DK170690          
      WRITE (6,1005) I,J,LAB1(I),LAB2(I),LAB3(I),LAB4(I),LAB1(J),LAB2(J)DK170700          
     1,LAB3(J),LAB4(J),ARRAY(K1),ARRAY(K1)                              DK170710          
 1005 FORMAT (1H ,2(I3,2X),2(A6,1X),2A6,2(A6,1X),2A6,2X,F17.8,8X,E15.8) DK170720          
   32 CONTINUE                                                          DK170730          
C     ----------------------------------------------------              DK170740          
C     COMPUTATION AND LISTING OF CONSTANTS                              DK170750          
C     ----------------------------------------------------              DK170760          
   19 WRITE (6,1024) DET,DET                                            DK170770          
 1024 FORMAT(1H0,44HTHE DETERMINANT OF THE CORRELATION MATRIX IS,F30.16,DK170780          
     1D28.16)                                                           DK170790          
    9 EDF=DF-FLOAT(NLHM)                                                DK170800          
      IF (EDF.GT.0) GO TO 57                                            DK170810          
      WRITE (6,1060)                                                    DK170820          
 1060 FORMAT (85H0THE DEGREES OF FREEDOM FOR THE REMAINDER(RDF) ARE ZERODK170830          
     1. HOWEVER, THE PROGRAM HAS SET/84H THIS VALUE TO 1 SO THAT THE PRODK170840          
     2BLEM CAN BE FINISHED WITHOUT ENCOUNTERING INTERRUPTS/78H CAUSED BYDK170850          
     3 DIVISION BY ZERO. ALL RESULTS WHICH ARE A FUNCTION OF RDF SHOULD DK170860          
     4BE/13H DISREGARDED.)                                              DK170870          
      EDF=1.0                                                           DK170880          
   57 IF (KB.EQ.0.AND.KD.EQ.0) GO TO 50                                 DK170890          
      CALL MIXEDF(NAB,MATX,KA,KB,KC,NOT,TOT,NMIG,NLHM,NCPR,NRHM,TOT3,ARRDK170900          
     1AY,NCDG,FAB,TOT4,SOF,NX,KD,LIOP)                                  DK170910          
   50 IF (NLHM.EQ.0.OR.NRHM.EQ.0) GO TO 8                               DK170920          
      IF (LIOP.EQ.20) GO TO 55                                          DK170930          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 56                    DK170940          
      IF (NMEA.EQ.0.AND.NNEA.EQ.0) GO TO 55                             DK170950          
   56 WRITE (6,1006) IJOB                                               DK170960          
 1006 FORMAT (1H0,18X,45HLISTING OF CONSTANT ESTIMATES FOR PROBLEM NO.,IDK170970          
     13)                                                                DK170980          
      WRITE (6,1007)                                                    DK170990          
 1007 FORMAT (1H0,10H RHM   ROW,43X,18HCONSTANT ESTIMATES/ 1H ,33H NAME DK171000          
     1 CODE  INDEPENDENT VARIABLE,9X,41HFIXED POINT FORMAT  FLOATING POIDK171010          
     2NT FORMAT)                                                        DK171020          
   55 DO 54 K=1,NRHM                                                    DK171030          
      WRITE (6,1004)                                                    DK171040          
   34 TREDS=0.0                                                         DK171050          
      DO 52 I=NBRC,NERC                                                 DK171060          
      TEMP=0.0                                                          DK171070          
      DO 44 J=NBRC,NERC                                                 DK171080          
      IF (I-J.LT.0) GO TO 38                                            DK171090          
   36 K1=NLHM*(J-1)-J*(J-3)/2+I-J                                       DK171100          
      GO TO 40                                                          DK171110          
   38 K1=NLHM*(I-1)-I*(I-3)/2+J-I                                       DK171120          
   40 K4=(K-1)*NLHM+J                                                   DK171130          
   44 TEMP=TEMP+RHM(K4)*ARRAY(K1)                                       DK171140          
      K4=NLHM*(K-1)+I                                                   DK171150          
   48 TREDS=TREDS+RHM(K4)*TEMP                                          DK171160          
      TRED(K)=TREDS                                                     DK171170          
      K1=MATX+(K-1)*NLHM+I                                              DK171180          
      ARRAY(K1)=TEMP                                                    DK171190          
      IF (LIOP.EQ.20) GO TO 52                                          DK171200          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 51                    DK171210          
      IF (NMEA.EQ.0.AND.NNEA.EQ.0) GO TO 52                             DK171220          
      IF ((NAB.EQ.0.OR.NAB.EQ.3).AND.I.EQ.1) TEMP=TEMP+YM(K)            DK171230          
   51 WRITE (6,1008) LITY(K),I,LAB1(I),LAB2(I),LAB3(I),LAB4(I),TEMP,TEMPDK171240          
 1008 FORMAT (1H ,A6,I4,2X,3(A6,1X),A5,2X,F17.8,8X,E15.8)               DK171250          
   52 CONTINUE                                                          DK171260          
   54 CONTINUE                                                          DK171270          
      IF (MTY.EQ.1.OR.NRN.GT.1) GO TO 10                                DK171280          
      DO 20 I=1,NOT                                                     DK171290          
      TOT5(I)=TOT(I)                                                    DK171300          
      IF (I.GE.IE.AND.I.LT.NLHM+1) TOT5(I)=0.0                          DK171310          
   20 CONTINUE                                                          DK171320          
      GO TO (11,12,13,12,13,13,17),MTY                                  DK171330          
C     ------------------------------------------------------            DK171340          
C     STORE INVERSE ELEMENTS FOR MU                                     DK171350          
C     ------------------------------------------------------            DK171360          
   12 J=5000-NLHM                                                       DK171370          
      K1=1                                                              DK171380          
      DO 21 I=J,4999                                                    DK171390          
      FAB(I)=ARRAY(K1)                                                  DK171400          
   21 K1=K1+1                                                           DK171410          
      GO TO 11                                                          DK171420          
C     ------------------------------------------------------            DK171430          
C     STORE IDEN, NOS AND INVERSE ELEMENTS FOR MU AND A CONSTANTS       DK171440          
C     ------------------------------------------------------            DK171450          
   13 K=NCL(1)-1                                                        DK171460          
      K2=NCL(1)                                                         DK171470          
      J=301-K2                                                          DK171480          
      K4=101-K2                                                         DK171490          
      DO 84 I=1,K2                                                      DK171500          
      IDEN(K4)=IDEN(I)                                                  DK171510          
      NOS(J)=NOS(I)                                                     DK171520          
      K4=K4+1                                                           DK171530          
   84 J=J+1                                                             DK171540          
      J=5000-NLHM*K2+(K2*K)/2                                           DK171550          
      K1=1                                                              DK171560          
      DO 22 I=J,4999                                                    DK171570          
      FAB(I)=ARRAY(K1)                                                  DK171580          
   22 K1=K1+1                                                           DK171590          
C     ------------------------------------------------------            DK171600          
C     STORE CONSTANTS FOR A                                             DK171610          
C     ------------------------------------------------------            DK171620          
   29 K4=5000-K2*NRHM                                                   DK171630          
      DO 23 I=1,NRHM                                                    DK171640          
      DO 23 J=2,K2                                                      DK171650          
      K1=MATX+(I-1)*NLHM+J                                              DK171660          
      ARRAY(K4)=ARRAY(K1)                                               DK171670          
   23 K4=K4+1                                                           DK171680          
      GO TO 11                                                          DK171690          
C     ------------------------------------------------------            DK171700          
C     STORE INVERSE ELEMENTS FOR MU AND A FOR MODEL 7                   DK171710          
C     ------------------------------------------------------            DK171720          
   17 K=NCL(1)-1                                                        DK171730          
      J=NCL(2)-1                                                        DK171740          
      K2=NCL(1)                                                         DK171750          
      K3=K2+NCL(2)                                                      DK171760          
      K1=K3+K2*NCL(2)                                                   DK171770          
      I2=301-K1                                                         DK171780          
      K4=101-K1                                                         DK171790          
      DO 87 I=1,K3                                                      DK171800          
      IDEN(K4)=IDEN(I)                                                  DK171810          
      NOS(I2)=NOS(I)                                                    DK171820          
      K4=K4+1                                                           DK171830          
   87 I2=I2+1                                                           DK171840          
      K1=0                                                              DK171850          
      DO 88 I=1,NME                                                     DK171860          
   88 K1=K1+NCL(I)                                                      DK171870          
      IF (NNE.EQ.0) GO TO 91                                            DK171880          
      DO 90 I=1,NNE                                                     DK171890          
   90 K1=K1+NCLN(I)                                                     DK171900          
   91 K3=K1+K2*NCL(2)                                                   DK171910          
      K1=K1+1                                                           DK171920          
      DO 89 I=K1,K3                                                     DK171930          
      NOS(I2)=NOS(I)                                                    DK171940          
   89 I2=I2+1                                                           DK171950          
      K=K+J+K*J-NMC(1)                                                  DK171960          
      K1=NLHM-K-1                                                       DK171970          
      J=5000-MATX+(K1*(K1+1))/2                                         DK171980          
      K1=J+NLHM*K2-(K2*(K2-1))/2-1                                      DK171990          
      K4=1                                                              DK172000          
      DO 27 I=J,K1                                                      DK172010          
      FAB(I)=ARRAY(K4)                                                  DK172020          
   27 K4=K4+1                                                           DK172030          
      K3=5000-NLHM*NRHM                                                 DK172040          
      DO 92 I=1,NRHM                                                    DK172050          
      DO 92 J=2,K2                                                      DK172060          
      K4=K3+K*(I-1)+J-1                                                 DK172070          
      K1=MATX+(I-1)*NLHM+J                                              DK172080          
   92 ARRAY(K4)=ARRAY(K1)                                               DK172090          
      GO TO 11                                                          DK172100          
   10 IF (MTY.EQ.7.AND.NRN.EQ.2) GO TO 14                               DK172110          
      GO TO 80                                                          DK172120          
C     ------------------------------------------------------            DK172130          
C     STORE INVERSE ELEMENTS FOR C AND AC CONSTANTS                     DK172140          
C     ------------------------------------------------------            DK172150          
   14 J=NCL(2)-1                                                        DK172160          
      K3=NLHM*J-(J*(J-1))/2                                             DK172170          
      K1=NLHM-J-NDFA*J+NMC(1)                                           DK172180          
      J=5000-MATX+(K1*(K1+1))/2                                         DK172190          
      K3=J+K3-1                                                         DK172200          
      K4=1                                                              DK172210          
      DO 15 I=J,K3                                                      DK172220          
      FAB(I)=ARRAY(K4)                                                  DK172230          
   15 K4=K4+1                                                           DK172240          
      K2=IEI-1                                                          DK172250          
C     ------------------------------------------------------            DK172260          
C     STORE C AND AC CONSTANTS                                          DK172270          
C     ------------------------------------------------------            DK172280          
      J=K2*NLHM-(K2*(K2-1))/2+1                                         DK172290          
      K3=K3+1                                                           DK172300          
      DO 75 I=K3,4999                                                   DK172310          
      FAB(I)=ARRAY(J)                                                   DK172320          
   75 J=J+1                                                             DK172330          
      K3=NCL(2)-1                                                       DK172340          
      K1=K3+NDFA*K3-NMC(1)                                              DK172350          
      I1=K1+NDFA                                                        DK172360          
      I2=5000-(NDFA+1+NLHM)*NRHM+NDFA                                   DK172370          
      DO 16 I=1,NRHM                                                    DK172380          
      DO 16 J=1,K1                                                      DK172390          
      K=MATX+(I-1)*NLHM+J                                               DK172400          
      IF (J.GT.K3) K=K+K2-K3                                            DK172410          
      K4=I2+I1*(I-1)+J                                                  DK172420          
   16 ARRAY(K4)=ARRAY(K)                                                DK172430          
   80 IF (MTY.EQ.6.AND.NRN.EQ.2) GO TO 81                               DK172440          
      GO TO 11                                                          DK172450          
   81 K=NCL(1)-1                                                        DK172460          
      J=5000-(NLHM+1)*NCL(1)+(NCL(1)*K)/2+NLHM+1                        DK172470          
      K1=1                                                              DK172480          
      DO 82 I=J,4999                                                    DK172490          
      FAB(I)=ARRAY(K1)                                                  DK172500          
   82 K1=K1+1                                                           DK172510          
      K4=5000-(NLHM+1)*NRHM                                             DK172520          
      DO 83 I=1,NRHM                                                    DK172530          
      DO 83 J=1,K                                                       DK172540          
      K1=MATX+(I-1)*NLHM+J                                              DK172550          
      ARRAY(K4)=ARRAY(K1)                                               DK172560          
   83 K4=K4+1                                                           DK172570          
   11 IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4.OR.LIOP.EQ.20) GO TO 299     DK172580          
      IF (NMEA.NE.0.OR.NNEA.NE.0) GO TO 299                             DK172590          
      MN2=0                                                             DK172600          
      CALL LSMNS                                                        DK172610          
  299 IF (NLC.EQ.0.OR.MTY.GT.1) GO TO 53                                DK172620          
      KD=1                                                              DK172630          
      CALL SUAFMM                                                       DK172640          
C     ----------------------------------------------------              DK172650          
C     COMPUTATION AND LISTING, IF DESIRED, OF INVERSE ELEMENTS          DK172660          
C             OF SEGMENTS                                               DK172670          
C     ----------------------------------------------------              DK172680          
   53 IF (MTY.EQ.1.OR.NRN.NE.NRUN) GO TO 85                             DK172690          
      DO 86 I=1,MATX                                                    DK172700          
   86 FAB(I)=ARRAY(I)                                                   DK172710          
   85 NERC=0                                                            DK172720          
      IF (LIOP.LT.3.OR.LIOP.GT.9)  GO TO 221                            DK172730          
      WRITE (6,1000) IJOB                                               DK172740          
 1000 FORMAT (1H0,23X,55HLISTING OF INVERSE ELEMENTS OF SEGMENTS FOR PRODK172750          
     1BLEM NO.,I3)                                                      DK172760          
      WRITE (6,1002)                                                    DK172770          
      WRITE (6,1003)                                                    DK172780          
  221 DO 58 I2=1,NS                                                     DK172790          
      NBRC=NERC+1                                                       DK172800          
      NERC=IM(I2)                                                       DK172810          
      DET=0.0                                                           DK172820          
      CALL MATINV (ARRAY,NBRC,NERC,NLHM,X,DET)                          DK172830          
      IF (LIOP.LT.3.OR.LIOP.GT.9)  GO TO 58                             DK172840          
      DO 220 I=NBRC,NERC                                                DK172850          
      WRITE (6,1004)                                                    DK172860          
      DO 220 J=I,NERC                                                   DK172870          
      K1=NLHM*(I-1)-I*(I-3)/2+J-I                                       DK172880          
      WRITE (6,1005) I,J,LAB1(I),LAB2(I),LAB3(I),LAB4(I),LAB1(J),LAB2(J)DK172890          
     1,LAB3(J),LAB4(J),ARRAY(K1),ARRAY(K1)                              DK172900          
  220 CONTINUE                                                          DK172910          
   58 CONTINUE                                                          DK172920          
C     ----------------------------------------------------              DK172930          
C     COMPUTATION AND LISTING OF RESIDUAL MATRIX FOR RHM                DK172940          
C     ----------------------------------------------------              DK172950          
    8 IF (NCPR.EQ.0.) GO TO 77                                          DK172960          
      WRITE (6,1009)                                                    DK172970          
 1009 FORMAT (1H0,22X,40HRESIDUAL MATRICES FOR RIGHT HAND MEMBERS// 1H ,DK172980          
     127H JOB ROW COL   RHM      RHM,11X,14HERROR SS OR CP,7X,15HERROR MDK172990          
     2S OR COV,5X,11HCORRELATION)                                       DK173000          
      DO 74 K=1,NRHM                                                    DK173010          
      WRITE (6,1004)                                                    DK173020          
      K3=NRHM*(K-1)-K*(K-3)/2                                           DK173030          
      K1=(K-1)*NLHM                                                     DK173040          
      DO 74 L=K,NRHM                                                    DK173050          
      K2=(L-1)*NLHM                                                     DK173060          
      TR=0.0                                                            DK173070          
      IF (NLHM.EQ.0) GO TO 7                                            DK173080          
      DO 64 I=1,NLHM                                                    DK173090          
      K4=K1+I                                                           DK173100          
      K5=MATX+K2+I                                                      DK173110          
   64 TR=TR+ARRAY(K5)*RHM(K4)                                           DK173120          
    7 J=K3+L-K                                                          DK173130          
      ESS=SSCPR(J)-TR                                                   DK173140          
C     ------------------------------------------------------            DK173150          
C     RETAINS SS AND DF FOR RESIDUAL FOR INDIRECT ANALYSIS WHEN RUN     DK173160          
C     NO. IS LESS THAN NO. RUNS                                         DK173170          
C     ------------------------------------------------------            DK173180          
      IF (KB.EQ.0.AND.KD.EQ.0) GO TO 66                                 DK173190          
      IF (KD.EQ.0) GO TO 65                                             DK173200          
      FY(J)=ESS                                                         DK173210          
      EDFF=EDF                                                          DK173220          
      GO TO 66                                                          DK173230          
   65 SAB(J)=ESS                                                        DK173240          
      EDFS=EDF                                                          DK173250          
   66 EMS=ESS/EDF                                                       DK173260          
      SSCPR(J)=EMS                                                      DK173270          
      IF (K.EQ.L) GO TO 72                                              DK173280          
      AK=0.0                                                            DK173290          
      IF (NLHM.EQ.0) GO TO 6                                            DK173300          
      DO 68 I=1,NLHM                                                    DK173310          
      K4=K2+I                                                           DK173320          
      K5=MATX+K4                                                        DK173330          
   68 AK=AK+ARRAY(K5)*RHM(K4)                                           DK173340          
    6 JJ=NRHM*(L-1)-L*(L-3)/2                                           DK173350          
      SM=(SSCPR(JJ)-AK)/EDF                                             DK173360          
      F=EMS/DSQRT(SSCPR(K3)*SM)                                         DK173370          
      GO TO 74                                                          DK173380          
   72 F=1.                                                              DK173390          
   74 WRITE (6,1010) IJOB,K,L,LITY(K),LITY(L),ESS,EMS,F                 DK173400          
 1010 FORMAT (1H ,I4,I3,I4,2X,2(2X,A6),F22.6,F23.8,F14.4)               DK173410          
C     ----------------------------------------------------              DK173420          
C     CALLS POLYNO SUBROUTINE IF POLYNOMIALS ARE TO BE FITTED           DK173430          
C     ----------------------------------------------------              DK173440          
   77 IF (NSME.EQ.0) GO TO 76                                           DK173450          
      CALL POLYNO                                                       DK173460          
C     ----------------------------------------------------              DK173470          
C     COMPUTES AND LISTS SS, MS AND F FOR ANOVA FOR EACH RHM            DK173480          
C     ----------------------------------------------------              DK173490          
   76 IF (NRHM.EQ.0) GO TO 5                                            DK173500          
      CALL LSANOV                                                       DK173510          
C     ----------------------------------------------------              DK173520          
C     COMPUTATION AND LISTING OF K, SS, CP, MS, MCP AND VARIANCE AND    DK173530          
C     COVARIANCE COMPONENTS FOR ONE OR MORE SETS OF MAIN OR NESTED      DK173540          
C     EFFECTS, IF IRAN IS NOT EQUAL TO ZERO                             DK173550          
C     ----------------------------------------------------              DK173560          
    5 MN2=NRHM*(NRHM+1)/2                                               DK173570          
      IF (IRAN.EQ.0) GO TO 151                                          DK173580          
      WRITE (6,1017)                                                    DK173590          
 1017 FORMAT (1H1,15X,"VARIANCE AND COVARIANCE COMPONENT ESTIMATES FROM DK173600          
     1DIRECT ANALYSIS")                                                 DK173610          
      KK=0                                                              DK173620          
      LK=NS+1                                                           DK173630          
      DO 150 M=1,IRAN                                                   DK173640          
      IF (NS2(M).EQ.0.OR.NS2(M).GT.2) GO TO 147                         DK173650          
      KK=KK+NS2(M)                                                      DK173660          
      SDF=0.                                                            DK173670          
      AK=0.                                                             DK173680          
      DO 114 J=1,KA                                                     DK173690          
  114 SSS(J)=0.                                                         DK173700          
      IF (NS2(M).EQ.1) GO TO 115                                        DK173710          
      I3=0                                                              DK173720          
  116 I3=I3+1                                                           DK173730          
      IF (IM(I3).EQ.MS(KK)) GO TO 120                                   DK173740          
      IF (I3.EQ.LK) GO TO 147                                           DK173750          
      GO TO 116                                                         DK173760          
  120 L=KK-1                                                            DK173770          
      J=0                                                               DK173780          
  122 J=J+1                                                             DK173790          
      IF (MS(L).LE.IM(J)) GO TO 123                                     DK173800          
      IF (J.EQ.LK) GO TO 147                                            DK173810          
      GO TO 122                                                         DK173820          
  123 IF (J.EQ.1) GO TO 139                                             DK173830          
      IF (IM(J-1).NE.MS(L)-1) GO TO 147                                 DK173840          
  139 DO 126 I2=J,I3                                                    DK173850          
      IF (I2.EQ.1) GO TO 124                                            DK173860          
      NBRC=IM(I2-1)+1                                                   DK173870          
      GO TO 125                                                         DK173880          
  124 NBRC=1                                                            DK173890          
      IF (MS(L).NE.1) GO TO 147                                         DK173900          
  125 NERC=IM(I2)                                                       DK173910          
      DET=0.                                                            DK173920          
      CALL MATINV (ARRAY,NBRC,NERC,NLHM,X,DET)                          DK173930          
  126 CONTINUE                                                          DK173940          
      NBRC=MS(L)                                                        DK173950          
      NERC=MS(KK)                                                       DK173960          
      DET=0.                                                            DK173970          
      CALL MATINV (ARRAY,NBRC,NERC,NLHM,X,DET)                          DK173980          
  135 DO 127 I2=J,I3                                                    DK173990          
      IF (I2.EQ.1) GO TO 128                                            DK174000          
      NBRC1=IM(I2-1)+1                                                  DK174010          
      MRANK=IM(I2)-IM(I2-1)                                             DK174020          
      GO TO 129                                                         DK174030          
  128 MRANK=IM(I2)                                                      DK174040          
      NBRC1=1                                                           DK174050          
  129 NERC1=IM(I2)                                                      DK174060          
      RANK=MRANK                                                        DK174070          
      SOD=0.                                                            DK174080          
      SD=0.                                                             DK174090          
      DO 130 I=NBRC1,NERC1                                              DK174100          
      DO 130 K=I,NERC1                                                  DK174110          
      IF (I-K.LT.0) GO TO 131                                           DK174120          
      L=NLHM*(I-1)-I*(I-3)/2                                            DK174130          
      SD=SD+ARRAY(L)                                                    DK174140          
      GO TO 130                                                         DK174150          
  131 L=NLHM*(I-1)-I*(I-3)/2+K-I                                        DK174160          
      SOD=SOD+ARRAY(L)*2.                                               DK174170          
  130 CONTINUE                                                          DK174180          
      AK=AK+((SD-(SOD/RANK))/(RANK+1.))*RANK                            DK174190          
      SDF=SDF+RANK                                                      DK174200          
  127 CONTINUE                                                          DK174210          
      GO TO 132                                                         DK174220          
  115 I3=0                                                              DK174230          
  133 I3=I3+1                                                           DK174240          
      IF (IM(I3).EQ.MS(KK)) GO TO 134                                   DK174250          
      IF (I3.EQ.LK) GO TO 147                                           DK174260          
      GO TO 133                                                         DK174270          
  134 J=I3                                                              DK174280          
      GO TO 135                                                         DK174290          
  132 IF (NS2(M).EQ.2) GO TO 136                                        DK174300          
      NBRC=NBRC1                                                        DK174310          
      NERC=NERC1                                                        DK174320          
  136 DO 142 K=1,NRHM                                                   DK174330          
      DO 142 I2=K,NRHM                                                  DK174340          
      SUM=0.0                                                           DK174350          
      DO 140 I=NBRC,NERC                                                DK174360          
      N3=MATX+(K-1)*NLHM+I                                              DK174370          
      DO 140 J=NBRC,NERC                                                DK174380          
      IF (I-J.LE.0) GO TO 137                                           DK174390          
      L=NLHM*(J-1)-J*(J-3)/2+I-J                                        DK174400          
      GO TO 138                                                         DK174410          
  137 L=NLHM*(I-1)-I*(I-3)/2+J-I                                        DK174420          
  138 N2=MATX+(I2-1)*NLHM+J                                             DK174430          
  140 SUM=SUM+ARRAY(N3)*ARRAY(L)*ARRAY(N2)                              DK174440          
      L2=NRHM*(K-1)-K*(K-3)/2+I2-K                                      DK174450          
  142 SSS(L2)=SSS(L2)+SUM                                               DK174460          
      WK=AK/SDF                                                         DK174470          
      K=101-M                                                           DK174480          
      WRITE (6,1004)                                                    DK174490          
      WRITE (6,1018) LAB4(K),WK,SDF                                     DK174500          
 1018 FORMAT (1H0,10X,"K FOR RANDOM EFFECTS COMPONENT (",A6,")=",F8.4,3XDK174510          
     1,"DEGREES OF FREEDOM =",F5.0)                                     DK174520          
      WRITE (6,1019)                                                    DK174530          
 1019 FORMAT (1H0/25X,51HSS, CP, MS, MCP, VARIANCE AND COVARIANCE COMPONDK174540          
     1ENTS//1H ,25HJOB ROW COL   RHM     RHM,20X,8HSS OR CP,19X,9HMS OR DK174550          
     2COV,18X,10HCOMPONENTS)                                            DK174560          
      DO 146 I=1,NRHM                                                   DK174570          
      WRITE (6,1004)                                                    DK174580          
      DO 146 J=I,NRHM                                                   DK174590          
      K=NRHM*(I-1)-I*(I-3)/2+J-I                                        DK174600          
      SMS=SSS(K)/SDF                                                    DK174610          
      SOD=(SMS-SSCPR(K))/WK                                             DK174620          
      WRITE (6,1020) IJOB,I,J,LITY(I),LITY(J),SSS(K),SMS,SOD            DK174630          
 1020 FORMAT (1H ,I3,2I4,2(2X,A6),3F27.8)                               DK174640          
  146 SSS(K)=SOD                                                        DK174650          
C     ----------------------------------------------------              DK174660          
C     CALLS SVCVC SUBROUTINE TO COMPUTE HERITABILITIES, GENETIC         DK174670          
C     CORRELATIONS, ETC., IF I309 IS NOT EQUAL TO ZERO                  DK174680          
C     ----------------------------------------------------              DK174690          
      IF (I309(M).EQ.0) GO TO 150                                       DK174700          
      L=M                                                               DK174710          
      CALL SVCVC                                                        DK174720          
      GO TO 150                                                         DK174730          
  147 K=101-M                                                           DK174740          
      WRITE (6,1023) M,LAB4(K)                                          DK174750          
 1023 FORMAT (1H0,"ERROR IN IRAN PARAMETER CARD NO.",I5,4X,A6)          DK174760          
  150 CONTINUE                                                          DK174770          
  151 RETURN                                                            DK174780          
      END                                                               DK174790          
*ENDTEXT                                                                                  
