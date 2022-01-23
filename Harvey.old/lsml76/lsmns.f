      SUBROUTINE LSMNS                                                  DK140010          
C     -------------------------------------------                       DK140020          
C     SUBROUTINE FOR COMPUTING AND LISTING LEAST-SQUARES MEANS AND      DK140030          
C     STANDARD ERRORS WHEN NAB=0 OR NAB=3 AND BOTH NMEA AND NNEA        DK140040          
C     EQUAL ZERO. ALSO USED WITH MIXED MODELS.                          DK140050          
C     -------------------------------------------------------           DK140060          
      EXTERNAL DSQRT,DFLOAT                                                               
      DIMENSION ARRAY(5000),SSCPR(630),SSS(630),RHM(2450),TOT(106),TOT2(DK140090          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100),NS2(15)    DK140100          
      DIMENSION FAB(5000),FY(630),TOT4(106),SAB(630),FSQ(525),TOT5(106) DK140110          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),MZ(10),IPL(13),NSDK140120          
     1P(13),NND(13),XP(81),YP(41),      NDC(10),NMI(10),MEN(20),NCL(20),DK140130          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(50),NCLN(50),LNE(50),NDEN(1                  
     300),NLIT(50),NMA(50),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(50),                  
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDK140160          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),                          
     6EFF1(50),EFF2(50),NOS(300),X(106),NMAC(50),I309(15),NR1(15),NW(15)                  
     7,R1I(100),R2I(100),WW(200),ICLR(90),IRM(40),LAD(90)               DK140190          
      DIMENSION IC(1000)                                                                  
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADK140200          
     1B4,LITY,TRED,YM,XP,YP,IM,MS,NEG,IPL,NSP,NND,FAB,FY,TOT4,SAB,FSQ,TODK140210          
     2T5,MZ,R1I,R2I,WW,ICLR,IRM,LAD                                     DK140220          
      COMMON /CMBLK2/LIT,LITR,NLIT,XM,EFF1,EFF2,X,NDC,NMI,MEN,NCL,LME,IBDK140230          
     1EG,IDEN,NCLN,NEN,LNE,NBEG,NDEN,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQDK140240          
     2C,NREGP,LGTX,JBEG,NDECX,NEGY,LNY,LHY,KBEG,NDECY,IC,NOS,NMAC       DK140250          
      COMMON /CMBLK3/RR,REP,DF,SDF,EDF,WK,IJOB,NAB,NLHM,NRHM,NMEA,NME,NNDK140260          
     1EA,NNE,N2F,NPR,IRAN,MPOP,LIOP,IAD,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,DK140270          
     2NOM,NON,ML,MLB,NS,NOT,KPUT,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,IN,I30DK140280          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DK140290          
     1L,L7,KA,KB,KC,SOF,NX,KD,MTY,MAN,NRUN,NRN,EDFF,NAME,EDFS,NAM5,IFP,NDK140300          
     2DFA,IFPC,NDFC,NDFAC,NINT,IBET,LBEG,LTHN,NNDC,WTT,SWT1             DK140310          
      DATA MU/6HMU    /                                                 DK140320          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 999                   DK140330          
      WRITE (6,1001) IJOB                                               DK140340          
 1001 FORMAT (1H0,7X,77HLISTING OF CONSTANTS, LEAST-SQUARES MEANS AND STDK140350          
     1ANDARD ERRORS FOR PROBLEM NO.,I3)                                 DK140360          
      WRITE (6,2001)                                                    DK140370          
 2001 FORMAT (1H0,76X,8HSTANDARD,26X,8HSTANDARD)                        DK140380          
      WRITE (6,1002)                                                    DK140390          
 1002 FORMAT (1H ,10H RHM   ROW,30X,3HNO.,3X,9HEFFECTIVE,4X,8HCONSTANT, DK140400          
     19X,8HERROR OF,6X,13HLEAST-SQUARES,7X,8HERROR OF)                  DK140410          
      WRITE (6,1003)                                                    DK140420          
 1003 FORMAT (1H ,33H NAME  CODE  INDEPENDENT VARIABLE,7X,4HOBS.,5X,3HNODK140430          
     1.,7X,8HESTIMATE,9X,8HCONSTANT,10X,4HMEAN,12X,7HLS MEAN)           DK140440          
      I=0                                                               DK140450          
      IF (IBET.EQ.0) SWT1=NCDS                                          DK140460          
      IF (MTY.GT.3.AND.MN2.EQ.1) TOT4(105)=TOT4(105)/SWT1               DK140470          
    1 I=I+1                                                             DK140480          
      NCT=1                                                             DK140490          
      WRITE (6,1004)                                                    DK140510          
 1004 FORMAT (1H )                                                      DK140520          
      IF (NCPR.EQ.1) GO TO 2                                            DK140530          
      J=I                                                               DK140540          
      GO TO 3                                                           DK140550          
    2 J=NRHM*(I-1)-I*(I-3)/2                                            DK140560          
      LL=J                                                              DK140570          
    3 WK=(SSCPR(J)-TRED(I))/EDF                                         DK140580          
      IF (MN2.EQ.1) WK=SSCPR(J)                                         DK140590          
      IF (MN2.EQ.0) GO TO 91                                            DK140600          
      IF (WK.LT.0.0) WK=+0.0                                            DK140610          
      GO TO (91,92,92,94,94,94,94),MTY                                  DK140620          
   92 IF (SAB(J).LT.0.0) SAB(J)=+0.0                                    DK140630          
      AD=TOT4(104)*SAB(J)*ARRAY(1)                                      DK140640          
      REP=DSQRT(WK*ARRAY(1)+AD)                                         DK140650          
      GO TO 112                                                         DK140660          
   94 IF (FY(J).LT.0.0) FY(J)=+0.0                                      DK140670          
      IF (SAB(J).LT.0.0) SAB(J)=+0.0                                    DK140680          
      IF (MTY.GT.5) GO TO 96                                            DK140690          
      AD=ARRAY(1)*(TOT4(104)*FY(J)+TOT4(106)*SAB(J))                    DK140700          
      REP=DSQRT(WK*ARRAY(1)+AD)                                         DK140710          
      GO TO 112                                                         DK140720          
   96 AD=ARRAY(1)*(TOT4(104)*FY(J)+TOT4(105)*SAB(J))                    DK140730          
      REP=DSQRT(WK*ARRAY(1)+AD)                                         DK140740          
      GO TO 112                                                         DK140750          
   91 REP=DSQRT(WK*ARRAY(1))                                            DK140760          
  112 J=MATX+(I-1)*NLHM+1                                               DK140770          
      SDF=ARRAY(J)+YM(I)                                                DK140780          
      EFNUM=1.0D0/ARRAY(1)                                              DK140800          
      WRITE (6,1005) LITY(I),MU,NCDS,EFNUM,SDF,REP,SDF,REP              DK140810          
 1005 FORMAT (1H ,A6,3X,1H1,2X,A6,18X,I7,F9.1,4F17.8)                   DK140820          
      L=1                                                               DK140830          
      L2=1                                                              DK140840          
      K=ML+1                                                            DK140850          
      IF (NOM.EQ.NMEA) GO TO 4                                          DK140860          
      K8=NOM-NMEA                                                       DK140870          
      DO 5 J=1,K8                                                       DK140880          
      K2=NCL(J+NMEA)                                                    DK140890          
      DO 5 K1=1,K2                                                      DK140900          
      IF (K1.EQ.K2) GO TO 7                                             DK140910          
      K3=IM(J)+K1                                                       DK140920          
      K4=NLHM*(K3-1)-K3*(K3-3)/2                                        DK140930          
      AC=WK*(ARRAY(1)+ARRAY(K4)+2.0*ARRAY(K3))                          DK140940          
      IF (AC.LT.0.0) AC=0.0                                             DK140950          
      REP=DSQRT(AC)                                                     DK140960          
      REPAC=WK*ARRAY(K4)                                                DK140970          
      IF (REPAC.LT.0.0) REPAC=0.0                                       DK140980          
      REPAC=DSQRT(REPAC)                                                DK140990          
      K4=MATX+(I-1)*NLHM+K3                                             DK141000          
      AC=ARRAY(K4)                                                      DK141010          
      ALS=AC+SDF                                                        DK141020          
      L2=L2+1                                                           DK141030          
      L1=L2                                                             DK141040          
      GO TO 8                                                           DK141050          
    7 K3=IM(J)+1                                                        DK141060          
      K4=MATX+(I-1)*NLHM+K3                                             DK141070          
      AC=0.                                                             DK141080          
      US=0.                                                             DK141090          
      I5=K2-1                                                           DK141100          
      DO 9 M=1,I5                                                       DK141110          
      AC=AC-ARRAY(K4)                                                   DK141120          
      US=US-ARRAY(K3)                                                   DK141130          
      K4=K4+1                                                           DK141140          
    9 K3=K3+1                                                           DK141150          
      K5=IM(J)+K2-1                                                     DK141160          
      SS=0.                                                             DK141170          
      K3=IM(J)+1                                                        DK141180          
      DO 81 M=K3,K5                                                     DK141190          
      K4=NLHM*(M-1)-M*(M-3)/2                                           DK141200          
      DO 10 N=M,K5                                                      DK141210          
      IF (M.EQ.N) GO TO 80                                              DK141220          
      K6=K4+N-M                                                         DK141230          
      SS=SS+2.0*ARRAY(K6)                                               DK141240          
      GO TO 10                                                          DK141250          
   80 SS=SS+ARRAY(K4)                                                   DK141260          
   10 CONTINUE                                                          DK141270          
   81 CONTINUE                                                          DK141280          
      ALS=WK*(ARRAY(1)+SS+2.0*US)                                       DK141290          
      IF (ALS.LT.0.0) ALS=0.0                                           DK141300          
      REP=DSQRT(ALS)                                                    DK141310          
      SSAC=SS*WK                                                        DK141320          
      IF (SSAC.LT.0.0) SSAC=0.0                                         DK141330          
      REPAC=DSQRT(SSAC)                                                 DK141340          
      ALS=AC+SDF                                                        DK141350          
      L1=0                                                              DK141360          
    8 EFNUM=DFLOAT(NOS(L))                                              DK141370          
      IF (REP.NE.0.0) EFNUM=WK/(REP*REP)                                DK141380          
      IF (MTY.EQ.1.OR.MN2.EQ.0) GO TO 93                                DK141390          
      SS=REP*REP                                                        DK141400          
      SSAC=REPAC*REPAC                                                  DK141410          
      YT=SS                                                             DK141420          
      IF (MTY.EQ.2.OR.MTY.EQ.4) SS=SS+AD                                DK141430          
      IF (J.GT.1.AND.(MTY.EQ.3.OR.MTY.EQ.5.OR.MTY.EQ.6)) SS=SS+AD       DK141440          
      IF (MTY.EQ.7.AND.J.GT.2) SS=SS+AD                                 DK141450          
      IF (MTY.EQ.3.AND.J.EQ.1) SS=SS+(SS*R1I(K1)*SAB(LL))/WK            DK141460          
      IF (MTY.EQ.3.AND.J.EQ.1) SSAC=SSAC+(SSAC*R1I(K1)*SAB(LL))/WK      DK141470          
      IF (J.EQ.1.AND.(MTY.EQ.5.OR.MTY.EQ.7)) SS=SS+(SS*(R1I(K1)*FY(LL)+ DK141480          
     1R2I(K1)*SAB(LL)))/WK                                              DK141490          
      IF (J.EQ.1.AND.(MTY.EQ.5.OR.MTY.EQ.7)) SSAC=SSAC+(SSAC*(R1I(K1)*  DK141500          
     *FY(LL)+R2I(K1)*SAB(LL)))/WK                                       DK141510          
      IF (MTY.EQ.6.AND.J.EQ.1) GO TO 201                                DK141520          
      IF (MTY.EQ.7.AND.J.EQ.2) GO TO 201                                DK141530          
      IF (MTY.EQ.7.AND.J.EQ.1) R2I(K1)=YT/WK                            DK141540          
      GO TO 202                                                         DK141550          
  201 SS=SS+(SS*TOT4(K1)*SAB(LL))/WK+ARRAY(1)*TOT4(104)*FY(LL)          DK141560          
      SSAC=SSAC+(SSAC*TOT4(K1)*SAB(LL))/WK                              DK141570          
  202 IF (SS.LT.0.0) SS=+0.0                                            DK141580          
      REP=DSQRT(SS)                                                     DK141590          
      IF (SSAC.LT.0.0) SSAC=+0.0                                        DK141600          
      REPAC=DSQRT(SSAC)                                                 DK141610          
   93 WRITE (6,1006) LITY(I),L1,LIT(J+NMEA),IDEN(K),NOS(L),EFNUM,AC,    DK141620          
     1REPAC,ALS,REP                                                     DK141630          
 1006 FORMAT (1H ,A6,I4,2X,A6,I5,13X,I7,F9.1,4F17.8)                    DK141640          
      K=K+1                                                             DK141650          
    5 L=L+1                                                             DK141660          
C     -------------------------------------------------------           DK141670          
C     CONSTRUCTION OF X AND TOT2 VECTORS FOR NESTED MAIN EFFECTS        DK141680          
C     -------------------------------------------------------           DK141690          
    4 K=MLB+1                                                           DK141700          
      IF (NON.EQ.NNEA) GO TO 6                                          DK141710          
      K8=NNEA+1                                                         DK141720          
      DO 11 J=K8,NON                                                    DK141730          
      K9=0                                                              DK141740          
      K4=NMA(J)-1                                                       DK141750          
      IF (K4.LT.NMEA) K9=1                                              DK141760          
      NSUM=1                                                            DK141770          
      IF (K4.LE.NMEA) GO TO 13                                          DK141780          
      DO 12 K3=1,K4                                                     DK141790          
   12 NSUM=NSUM+NCL(K3+NMEA)-1                                          DK141800          
   13 K2=NCLN(J)                                                        DK141810          
      DO 11 K1=1,K2                                                     DK141820          
      DO 14 K3=1,NLHM                                                   DK141830          
      TOT2(K3)=0.                                                       DK141840          
   14 X(K3)=0.                                                          DK141850          
      X(1)=1.                                                           DK141860          
      I2=NOM+J-NNEA-NMEA                                                DK141870          
      K3=IM(I2)+K1                                                      DK141880          
      IF (K9.EQ.1) GO TO 15                                             DK141890          
      IF (NMAC(J).NE.NCL(K4+1)) GO TO 15                                DK141900          
      I3=NCL(K4+1)-1                                                    DK141910          
      DO 16 K5=1,I3                                                     DK141920          
      I4=NSUM+K5                                                        DK141930          
   16 X(I4)=-1.0                                                        DK141940          
      IF (K1.NE.K2) GO TO 17                                            DK141950          
   20 I5=K2-1                                                           DK141960          
      DO 18 K5=1,I5                                                     DK141970          
      I4=IM(I2)+K5                                                      DK141980          
      X(I4)=-1.0                                                        DK141990          
   18 TOT2(I4)=-1.0                                                     DK142000          
      L1=0                                                              DK142010          
      GO TO 19                                                          DK142020          
   15 I4=NSUM+NMAC(J)                                                   DK142030          
      X(I4)=1.                                                          DK142040          
      IF (K1.NE.K2) GO TO 17                                            DK142050          
      GO TO 20                                                          DK142060          
   17 TOT2(K3)=1.                                                       DK142070          
      L2=L2+1                                                           DK142080          
      L1=L2                                                             DK142090          
      X(K3)=1.                                                          DK142100          
   19 YT=YM(I)                                                          DK142110          
      NR=MATX+(I-1)*NLHM+1                                              DK142120          
C     ------------------------------------------------------            DK142130          
C     CALL SUBROUTINE WHICH COMPUTES CONSTANT, LS MEAN AND SE, AND LISTSDK142140          
C     ------------------------------------------------------            DK142150          
      CALL CANDSE (AC,REPAC,ALS,REP,TOT2,X,ARRAY,NR,NLHM,WK,YT,I,SSS,   DK142160          
     *NCT)                                                              DK142170          
      EFNUM=DFLOAT(NOS(L))                                              DK142180          
      IF (REP.NE.0.0) EFNUM=WK/(REP*REP)                                DK142190          
      IF (MN2.EQ.0) GO TO 206                                           DK142200          
      IF (MTY.GT.1) REP=DSQRT(REP*REP+AD)                               DK142210          
      IF (NMA(J).EQ.1.AND.(MTY.EQ.3.OR.MTY.EQ.5.OR.MTY.EQ.7)) GO TO 205 DK142220          
      GO TO 206                                                         DK142230          
  205 YT=1.0/FLOAT(K2)                                                  DK142240          
      K6=NMAC(J)                                                        DK142250          
      SS=REP*REP-AD                                                     DK142260          
      IF (MTY.EQ.3) REP=DSQRT(SS+(SS*R1I(K6)*YT*SAB(LL))/WK)            DK142270          
      IF (MTY.EQ.5.OR.MTY.EQ.7) REP=DSQRT(SS+(SS*YT*(R1I(K6)*FY(LL)+R2I(DK142280          
     1K6)*SAB(LL)))/WK)                                                 DK142290          
  206 IF (K9.EQ.1) WRITE (6,2006) LITY(I),L1,NLIT(J),NDEN(K),NOS(L),    DK142300          
     1AC,REPAC                                                          DK142310          
 2006 FORMAT (1H ,A6,I4,2X,A6,I5,13X,I7,9X,2F17.8)                      DK142320          
      IF (K9.EQ.1) GO TO 250                                            DK142330          
      WRITE (6,1006) LITY(I),L1,NLIT(J),NDEN(K),NOS(L),EFNUM,AC,REPAC,  DK142340          
     *ALS,REP                                                           DK142350          
  250 K=K+1                                                             DK142360          
   11 L=L+1                                                             DK142370          
C     ------------------------------------------------------            DK142380          
C     CONSTRUCTION OF X AND TOT2 VECTORS FOR TWO-WAY SUBCLASSES         DK142390          
C     ------------------------------------------------------            DK142400          
    6 NOT=NOM+NON+1-NMEA-NNEA                                           DK142410          
      K8=IM(NOT)                                                        DK142420          
      K6=0                                                              DK142430          
      IF (N2F.EQ.0) GO TO 21                                            DK142440          
      K10=NOM+NNEA                                                      DK142450          
      DO 22 J=1,N2F                                                     DK142460          
      MJ1=0                                                             DK142470          
      MJ2=0                                                             DK142480          
      MJ3=0                                                             DK142490          
      MJ4=0                                                             DK142500          
      K9=0                                                              DK142510          
C     ------------------------------------------------------            DK142520          
C     SETS X AND TOT2 ARRAYS TO ZERO AND SETS X(1) TO 1 FOR MU          DK142530          
C     ------------------------------------------------------            DK142540          
      DO 23 K1=1,NLHM                                                   DK142550          
      TOT2(K1)=0.                                                       DK142560          
   23 X(K1)=0.                                                          DK142570          
      X(1)=1.                                                           DK142580          
      K6=K6+NMC(J)                                                      DK142590          
      NSUM=0                                                            DK142600          
      K4=INT2(J)-1                                                      DK142610          
      MSUM=0                                                            DK142620          
      K5=INT1(J)-1                                                      DK142630          
      IF (INT1(J).GT.NOM) GO TO 24                                      DK142640          
      IF (INT1(J).LE.NMEA) K9=1                                         DK142650          
      MJ1=INT1(J)                                                       DK142660          
      IF (K5.EQ.0) GO TO 25                                             DK142670          
      DO 26 K3=1,K5                                                     DK142680          
   26 NSUM=NSUM+NCL(K3)                                                 DK142690          
   25 K2=INT1(J)-NMEA                                                   DK142700          
      I1=IM(K2)+1                                                       DK142710          
      IF (K2.LE.0) I1=1                                                 DK142720          
      I2=IM(K2+1)+1                                                     DK142730          
      IF (K2.LE.0) I2=NCL(MJ1)                                          DK142740          
      ID1=LIT(MJ1)                                                      DK142750          
      GO TO 27                                                          DK142760          
C     ------------------------------------------------------            DK142770          
C     SETS UP +1 AND -1 FOR MAJOR CLASS WHEN FIRST MAIN EFFECT IS       DK142780          
C     NESTED                                                            DK142790          
C     ------------------------------------------------------            DK142800          
   24 K2=NOM+1                                                          DK142810          
      IF (INT1(J).LE.K10) K9=1                                          DK142820          
      IF (K2.GT.K5) GO TO 28                                            DK142830          
      DO 29 K3=K2,K5                                                    DK142840          
      K=K3-NOM                                                          DK142850          
   29 NSUM=NSUM+NCLN(K)                                                 DK142860          
   28 K2=INT1(J)-NOM                                                    DK142870          
      K3=INT1(J)-NNEA-NMEA                                              DK142880          
      I1=IM(K3)+1                                                       DK142890          
      IF (K3.LE.NOM+NNEA) I1=1                                          DK142900          
      I2=IM(K3+1)+1                                                     DK142910          
      IF (K3.LE.NOM+NNEA) I2=NCLN(K2)                                   DK142920          
      ID1=NLIT(K2)                                                      DK142930          
      K=NMA(K2)-NMEA                                                    DK142940          
      MJ2=NMA(K2)                                                       DK142950          
      NI=NMAC(K2)                                                       DK142960          
      IF (NMAC(K2).EQ.NCL(K)) GO TO 60                                  DK142970          
      K7=IM(K)+NMAC(K2)                                                 DK142980          
      X(K7)=1.                                                          DK142990          
      GO TO 27                                                          DK143000          
   60 K1=NCL(K)-1                                                       DK143010          
      DO 61 K3=1,K1                                                     DK143020          
      K2=IM(K)+K3                                                       DK143030          
   61 X(K2)=-1.                                                         DK143040          
   27 IF (INT2(J).GT.NOM) GO TO 30                                      DK143050          
      IF (INT2(J).LE.NMEA) K9=1                                         DK143060          
      MJ3=INT2(J)                                                       DK143070          
      IF (K4.EQ.0) GO TO 31                                             DK143080          
      DO 32 K3=1,K4                                                     DK143090          
   32 MSUM=MSUM+NCL(K3)                                                 DK143100          
   31 K2=INT2(J)-NMEA                                                   DK143110          
      I3=IM(K2)+1                                                       DK143120          
      IF (K2.LE.0) I3=1                                                 DK143130          
      I4=IM(K2+1)+1                                                     DK143140          
      IF (K2.LE.0) I4=NCL(MJ3)                                          DK143150          
      ID2=LIT(MJ3)                                                      DK143160          
      GO TO 33                                                          DK143170          
C     ------------------------------------------------------            DK143180          
C     SETS UP +1 AND -1 FOR MAJOR CLASS WHEN SECOND MAIN EFFECT IS      DK143190          
C     NESTED                                                            DK143200          
C     ------------------------------------------------------            DK143210          
   30 K2=NOM+1                                                          DK143220          
      IF (INT2(J).LE.K10) K9=1                                          DK143230          
      IF (K2.GT.K4) GO TO 34                                            DK143240          
      DO 35 K3=K2,K4                                                    DK143250          
      K=K3-NOM                                                          DK143260          
   35 MSUM=MSUM+NCLN(K)                                                 DK143270          
   34 K2=INT2(J)-NOM                                                    DK143280          
      K3=INT2(J)-NNEA-NMEA                                              DK143290          
      I3=IM(K3)+1                                                       DK143300          
      IF (K3.LE.NOM+NNEA) I3=1                                          DK143310          
      I4=IM(K3+1)+1                                                     DK143320          
      IF (K3.LE.NOM+NNEA) I4=NCLN(K2)                                   DK143330          
      ID2=NLIT(K2)                                                      DK143340          
      K=NMA(K2)-NMEA                                                    DK143350          
      MJ4=NMA(K2)                                                       DK143360          
      NJ=NMAC(K2)                                                       DK143370          
      IF (NMAC(K2).EQ.NCL(K)) GO TO 62                                  DK143380          
      K7=IM(K)+NMAC(K2)                                                 DK143390          
      X(K7)=1.                                                          DK143400          
      GO TO 111                                                         DK143410          
   62 K1=NCL(K)-1                                                       DK143420          
      DO 63 K3=1,K1                                                     DK143430          
      K2=IM(K)+K3                                                       DK143440          
   63 X(K2)=-1.                                                         DK143450          
C     ------------------------------------------------------            DK143460          
C     SETS UP X MATRIX FOR MAIN EFFECTS AND BOTH X AND TOT2 ARRAYS FOR  DK143470          
C     INTERACTION CONSTANTS                                             DK143480          
C     ------------------------------------------------------            DK143490          
  111 IF (MJ2.NE.MJ4.AND.MJ1.EQ.MJ3) GO TO 100                          DK143500          
      GO TO 33                                                          DK143510          
  100 ME1=MJ2                                                           DK143520          
      ME2=MJ4                                                           DK143530          
      CALL INTACT (ME1,ME2,NI,NJ,N2F,NOT,INT1,INT2,X,NMC,MSCL,NCL,EFF1,EDK143540          
     1FF2,IM,NCLN,NOM)                                                  DK143550          
   33 DO 36 K=I1,I2                                                     DK143560          
      J2=K-I1+1                                                         DK143570          
C     ------------------------------------------------------            DK143580          
C     SETS UP X ARRAY FOR FIRST MAIN EFFECT                             DK143590          
C     ------------------------------------------------------            DK143600          
      L3=I2-1                                                           DK143610          
      IF (K.EQ.I2) GO TO 42                                             DK143620          
      DO 64 K1=I1,L3                                                    DK143630          
   64 X(K1)=0.                                                          DK143640          
      X(K)=1.                                                           DK143650          
      GO TO 41                                                          DK143660          
   42 DO 46 J4=I1,L3                                                    DK143670          
   46 X(J4)=-1.                                                         DK143680          
C     ------------------------------------------------------            DK143690          
C     GETS IDENTIFICATION OF CLASS FOR FIRST MAIN EFFECT                DK143700          
C     ------------------------------------------------------            DK143710          
   41 K2=NSUM+J2                                                        DK143720          
      IF (INT1(J).GT.NOM) GO TO 37                                      DK143730          
      ID3=IDEN(K2)                                                      DK143740          
      GO TO 38                                                          DK143750          
   37 ID3=NDEN(K2)                                                      DK143760          
   38 DO 36 J1=I3,I4                                                    DK143770          
      J3=J1-I3+1                                                        DK143780          
C     ------------------------------------------------------            DK143790          
C     CHECKS FOR MISSING SUBCLASS                                       DK143800          
C     ------------------------------------------------------            DK143810          
      IF (NMC(J).EQ.0) GO TO 45                                         DK143820          
      K7=J2*100+J3                                                      DK143830          
      K1=K6-NMC(J)                                                      DK143840          
   43 K1=K1+1                                                           DK143850          
      IF (K1.GT.K6) GO TO 45                                            DK143860          
      IF (K7-MSCL(K1)) 43,44,43                                         DK143870          
C     ------------------------------------------------------            DK143880          
C     SETS UP X ARRAY FOR SECOND MAIN EFFECT                            DK143890          
C     ------------------------------------------------------            DK143900          
   45 I5=I4-1                                                           DK143910          
      IF (J1.EQ.I4) GO TO 47                                            DK143920          
      DO 40 K1=I3,I5                                                    DK143930          
   40 X(K1)=0.                                                          DK143940          
      X(J1)=1.                                                          DK143950          
      GO TO 48                                                          DK143960          
   47 DO 49 J4=I3,I5                                                    DK143970          
   49 X(J4)=-1.                                                         DK143980          
C     ------------------------------------------------------            DK143990          
C     GETS IDENTIFICATION OF CLASS FOR SECOND MAIN EFFECT               DK144000          
C     ------------------------------------------------------            DK144010          
   48 K2=MSUM+J3                                                        DK144020          
      IF (INT2(J).GT.NOM) GO TO 39                                      DK144030          
      ID4=IDEN(K2)                                                      DK144040          
      GO TO 110                                                         DK144050          
   39 ID4=NDEN(K2)                                                      DK144060          
C     ------------------------------------------------------            DK144070          
C     SETS UP X AND TOT2 ARRAYS FOR INTERACTION EFFECT                  DK144080          
C     ------------------------------------------------------            DK144090          
  110 IF (MJ1.NE.0.AND.MJ3.EQ.0) GO TO 101                              DK144100          
      GO TO 102                                                         DK144110          
  101 ME1=MJ1                                                           DK144120          
      ME2=MJ4                                                           DK144130          
      NI=J2                                                             DK144140          
      CALL INTACT (ME1,ME2,NI,NJ,N2F,NOT,INT1,INT2,X,NMC,MSCL,NCL,EFF1,EDK144150          
     1FF2,IM,NCLN,NOM)                                                  DK144160          
  102 IF (MJ3.NE.0.AND.MJ1.EQ.0) GO TO 103                              DK144170          
      GO TO 104                                                         DK144180          
  103 ME1=MJ2                                                           DK144190          
      ME2=MJ3                                                           DK144200          
      NJ=J3                                                             DK144210          
      CALL INTACT (ME1,ME2,NI,NJ,N2F,NOT,INT1,INT2,X,NMC,MSCL,NCL,EFF1,EDK144220          
     1FF2,IM,NCLN,NOM)                                                  DK144230          
  104 IF (MJ2.NE.MJ4.AND.MJ1.EQ.MJ3) GO TO 105                          DK144240          
      GO TO 74                                                          DK144250          
  105 ME1=INT1(J)                                                       DK144260          
      ME2=MJ4                                                           DK144270          
      NI=J2                                                             DK144280          
      K2=INT2(J)-NOM                                                    DK144290          
      NJ=NMAC(K2)                                                       DK144300          
      CALL INTACT (ME1,ME2,NI,NJ,N2F,NOT,INT1,INT2,X,NMC,MSCL,NCL,EFF1,EDK144310          
     1FF2,IM,NCLN,NOM)                                                  DK144320          
      ME1=MJ2                                                           DK144330          
      ME2=INT2(J)                                                       DK144340          
      K2=INT1(J)-NOM                                                    DK144350          
      NI=NMAC(K2)                                                       DK144360          
      NJ=J3                                                             DK144370          
      CALL INTACT (ME1,ME2,NI,NJ,N2F,NOT,INT1,INT2,X,NMC,MSCL,NCL,EFF1,EDK144380          
     1FF2,IM,NCLN,NOM)                                                  DK144390          
   74 L3=I2-I1                                                          DK144400          
      I5=I4-I3                                                          DK144410          
      N=NOT+J-1                                                         DK144420          
      K2=IM(N)+1                                                        DK144430          
      N=L3*I5-NMC(J)                                                    DK144440          
      M=K2+N-1                                                          DK144450          
      DO 59 J4=K2,M                                                     DK144460          
      TOT2(J4)=0.                                                       DK144470          
   59 X(J4)=0.                                                          DK144480          
      DO 50 J4=1,L3                                                     DK144490          
   50 EFF1(J4)=0.                                                       DK144500          
      DO 51 J4=1,I5                                                     DK144510          
   51 EFF2(J4)=0.                                                       DK144520          
      IF  (K.EQ.I2) GO TO 52                                            DK144530          
      EFF1(J2)=1.                                                       DK144540          
      GO TO 53                                                          DK144550          
   52 DO 54 J4=1,L3                                                     DK144560          
   54 EFF1(J4)=-1.                                                      DK144570          
   53 IF (J1.EQ.I4) GO TO 55                                            DK144580          
      EFF2(J3)=1.                                                       DK144590          
      GO TO 56                                                          DK144600          
   55 DO 76 J4=1,I5                                                     DK144610          
   76 EFF2(J4)=-1.                                                      DK144620          
   56 K5=K8+1                                                           DK144630          
      DO 82 K1=1,L3                                                     DK144640          
      DO 58 K2=1,I5                                                     DK144650          
      K7=K1*100+K2                                                      DK144660          
      IF (NMC(J).EQ.0) GO TO 71                                         DK144670          
      J4=K6-NMC(J)                                                      DK144680          
   70 J4=J4+1                                                           DK144690          
      IF (J4.GT.K6) GO TO 71                                            DK144700          
      IF (K7-MSCL(J4)) 70,58,70                                         DK144710          
   71 X(K5)=EFF1(K1)*EFF2(K2)                                           DK144720          
      TOT2(K5)=X(K5)                                                    DK144730          
      K5=K5+1                                                           DK144740          
   58 CONTINUE                                                          DK144750          
   82 CONTINUE                                                          DK144760          
      IF  (K.EQ.I2.OR.J1.EQ.I4) GO TO 72                                DK144770          
      L2=L2+1                                                           DK144780          
      L1=L2                                                             DK144790          
      GO TO 73                                                          DK144800          
   72 L1=0                                                              DK144810          
   73 NR=MATX+(I-1)*NLHM+1                                              DK144820          
      YT=YM(I)                                                          DK144830          
      CALL CANDSE (AC,REPAC,ALS,REP,TOT2,X,ARRAY,NR,NLHM,WK,YT,I,SSS,   DK144840          
     *NCT)                                                              DK144850          
      EFNUM=DFLOAT(NOS(L))                                              DK144860          
      IF (REP.NE.0.0) EFNUM=WK/(REP*REP)                                DK144870          
      IF (MN2.EQ.0) GO TO 204                                           DK144880          
      IF (MTY.GT.1) REP=DSQRT(REP*REP+AD)                               DK144890          
      IF ((MTY.EQ.3.OR.MTY.GT.4).AND.INT1(J).EQ.1) GO TO 203            DK144900          
      GO TO 204                                                         DK144910          
  203 SS=REP*REP-AD                                                     DK144920          
      K1=K-I1+1                                                         DK144930          
      K2=INT2(J)                                                        DK144940          
      IF(K2.GT.NOM) GO TO 204                                           DK144941          
      YT=1.0/FLOAT(NCL(K2))                                             DK144950          
      IF (MTY.EQ.3) REP=DSQRT(SS+(SS*YT*R1I(K1)*SAB(LL))/WK)            DK144960          
      IF (MTY.EQ.5) REP=DSQRT(SS+(SS*YT*(R1I(K1)*FY(LL)+R2I(K1)*SAB(LL))DK144970          
     1)/WK)                                                             DK144980          
      IF (MTY.EQ.6) REP=DSQRT(SS+(SS*YT*TOT4(K1)*(SAB(LL)+FY(LL)))/WK)  DK144990          
      IF (MTY.EQ.7) REP=DSQRT(SS+(SS*YT*R1I(K1)*SAB(LL))/WK+R2I(K1)*YT*RDK145000          
     11I(K1)*FY(LL))                                                    DK145010          
  204 IF (K9.EQ.1) WRITE (6,2007) LITY(I),L1,ID1,ID2,ID3,ID4,NOS(L),    DK145020          
     1AC,REPAC                                                          DK145030          
 2007 FORMAT (1H ,A6,I4,2X,A6,3H X ,A6,2I5,I6,9X,2F17.8)                DK145040          
      IF (K9.EQ.1) GO TO 44                                             DK145050          
      WRITE (6,1007) LITY(I),L1,ID1,ID2,ID3,ID4,NOS(L),EFNUM,AC,REPAC,  DK145060          
     *ALS,REP                                                           DK145070          
 1007 FORMAT (1H ,A6,I4,2X,A6,3H X ,A6,2I5,I6,F9.1,4F17.8)              DK145080          
   44 L=L+1                                                             DK145090          
   36 CONTINUE                                                          DK145100          
      K8=K8+L3*I5-NMC(J)                                                DK145110          
   22 CONTINUE                                                          DK145120          
C     ------------------------------------------------------            DK145130          
C     LISTING OF PARTIAL REGRESSIONS AND STANDARD ERRORS FOR            DK145140          
C     CONTINUOUS INDEPENDENT VARIABLES                                  DK145150          
C     ------------------------------------------------------            DK145160          
   21 IF(NPR.EQ.0) GO TO 83                                                               
      K4=1                                                              DK145180          
      K=IE                                                              DK145190          
      DO 57 J=1,NPR                                                     DK145200          
      IF (ICLR(J).EQ.0) GO TO 150                                       DK145210          
      K3=LQC(J)                                                         DK145220          
      DO 151 K5=1,K3                                                    DK145230          
      K6=ICLR(J)                                                        DK145240          
      IF (K5.GT.1) K4=K4-K6                                             DK145250          
      DO 159 K7=1,K6                                                    DK145260          
      L2=0                                                              DK145270          
      K2=IRM(K4)                                                        DK145280          
      IF (IRM(K4).GT.NOM) GO TO 153                                     DK145290          
      J1=NCL(K2)+1                                                      DK145300          
      L3=K2-1                                                           DK145310          
      IF (L3.EQ.0) GO TO 154                                            DK145320          
      DO 163 L4=1,L3                                                    DK145330          
  163 L2=L2+NCL(L4)                                                     DK145340          
      GO TO 154                                                         DK145350          
  153 L5=IRM(K4)-NOM                                                    DK145360          
      J1=NCLN(L5)+1                                                     DK145370          
      L3=L5-1                                                           DK145380          
      IF (L3.EQ.0) GO TO 154                                            DK145390          
      DO 164 L4=1,L3                                                    DK145400          
  164 L2=L2+NCLN(L4)                                                    DK145410          
  154 K8=1                                                              DK145420          
      IF (K7.GT.1) K8=2                                                 DK145430          
      DO 152 J2=K8,J1                                                   DK145440          
      DO 161 L3=1,NLHM                                                  DK145450          
      TOT2(L3)=0.                                                       DK145460          
  161 X(L3)=0.                                                          DK145470          
      IF (J2.EQ.1) L6=K                                                 DK145480          
      X(L6)=1.                                                          DK145490          
      IF (J2.GT.1) GO TO 156                                            DK145500          
      YT=0.                                                             DK145510          
      TOT2(K)=1.                                                        DK145520          
      GO TO 155                                                         DK145530          
  156 IF (J2.EQ.J1) GO TO 158                                           DK145540          
      L2=L2+1                                                           DK145550          
      TOT2(K)=1.                                                        DK145560          
      X(K)=1.                                                           DK145570          
      IF (LAD(50+K4).EQ.0) GO TO 155                                    DK145580          
      L4=LAD(50+K4)-1                                                   DK145590          
      IF (ICLR(50+K4).EQ.99) GO TO 162                                  DK145600          
  168 L5=IM(L4)+IRM(K4)-NOM                                             DK145610          
      X(L5)=1.                                                          DK145620          
      IF (J2.EQ.J1) GO TO 167                                           DK145630          
      GO TO 155                                                         DK145640          
  162 J3=IM(L4)+1                                                       DK145650          
      L5=IM(L4+1)                                                       DK145660          
      DO 166 M=J3,L5                                                    DK145670          
  166 X(M)=-1.                                                          DK145680          
      IF (J2.EQ.J1) GO TO 167                                           DK145690          
      GO TO 155                                                         DK145700          
  158 I5=K-1                                                            DK145710          
      I4=K-J1+2                                                         DK145720          
      DO 160 M=I4,I5                                                    DK145730          
      X(M)=-1.                                                          DK145740          
  160 TOT2(M)=-1.                                                       DK145750          
      L2=L2+1                                                           DK145760          
      K=K-1                                                             DK145770          
      IF (LAD(50+K4).EQ.0) GO TO 167                                    DK145780          
      L4=LAD(50+K4)-1                                                   DK145790          
      IF (ICLR(50+K4).EQ.99) GO TO 162                                  DK145800          
      GO TO 168                                                         DK145810          
  167 K4=K4+1                                                           DK145820          
  155 NR=MATX+(I-1)*NLHM+1                                              DK145830          
      CALL CANDSE (AC,REPAC,ALS,REP,TOT2,X,ARRAY,NR,NLHM,WK,YT,I,SSS,   DK145840          
     *NCT)                                                              DK145850          
      L1=K                                                              DK145860          
      IF(J2.NE.1) GO TO 157                                                               
      WRITE (6,1008) LITY(I),L1,LAB1(K),LAB2(K),LAB3(K),LAB4(K),AC,REPACDK145920          
     1,ALS,REP                                                          DK145930          
 1008 FORMAT (1H ,A6,I4,4(1X,A6),14X,4F17.8)                            DK145940          
      GO TO 152                                                         DK145950          
  157 IDD=IDEN(L2)                                                      DK145960          
      IF (J2.EQ.J1) L1=0                                                DK145970          
      IF (K2.GT.NOM) IDD=NDEN(L2)                                       DK145980          
      WRITE (6,1009) LITY(I),L1,IDD,LAB2(K),LAB3(K),LAB4(K),AC,REPAC,   DK145990          
     *ALS,REP                                                           DK146000          
 1009 FORMAT (1H ,A6,I4,I5,2X,3(1X,A6),14X,4F17.8)                      DK146010          
  152 K=K+1                                                             DK146020          
  159 CONTINUE                                                          DK146030          
  151 CONTINUE                                                          DK146040          
      GO TO 57                                                          DK146050          
  150 K3=LQC(J)                                                         DK146060          
      AA=SDF                                                                              
      DO 165 K5=1,K3                                                    DK146070          
      L=NLHM*(K-1)-K*(K-3)/2                                            DK146080          
      K9=MATX+(I-1)*NLHM+K                                              DK146090          
      AC=ARRAY(L)*WK                                                    DK146100          
      IF (AC.LT.0.0) AC=0.0                                             DK146110          
      REP=DSQRT(AC)                                                     DK146120          
      ALS=ARRAY(K9)                                                     DK146130          
      AC=ALS                                                            DK146140          
      L1=K                                                                                
      WRITE (6,1008) LITY(I),L1,LAB1(K),LAB2(K),LAB3(K),LAB4(K),AC,REP, DK146200          
     1ALS,REP                                                           DK146210          
      K=K+1                                                             DK146220          
  165 CONTINUE                                                                            
   57 CONTINUE                                                          DK146240          
   83 IF(I.NE.NRHM) GO TO 1                                                               
  999 IF (NLC.EQ.0) RETURN                                              DK146290          
      CALL LINFUN (NLC,NLHM,X,JBEG,TOT2,NRHM,MATX,NCPR,SSCPR,TRED,EDF,XPDK146300          
     1,YP,AC,ALS,REP,ARRAY,SSS,NAB,LITY,MN2,YM,MTY)                     DK146310          
      RETURN                                                            DK146320          
      END                                                               DK146330          
