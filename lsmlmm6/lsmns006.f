*TEXT                                                                                     
      SUBROUTINE LSMNS                                                  DK140010          
C     -------------------------------------------                       DK140020          
C     SUBROUTINE FOR COMPUTING AND LISTING LEAST-SQUARES MEANS AND      DK140030          
C     STANDARD ERRORS WHEN NAB=0 OR NAB=3 AND BOTH NMEA AND NNEA        DK140040          
C     EQUAL ZERO.                                                       DK140050          
C     -------------------------------------------------------           DK140060          
      IMPLICIT REAL*8(A-H,O-Z)                                          DK140070          
      REAL*8 LAB1,LAB2,LAB3,LAB4,LITY,NLIT,LIT,LITR,MU,ID1,ID2,NAME,NAM5DK140080          
      DIMENSION ARRAY(5000),SSCPR(630),SSS(630),RHM(2450),TOT(106),TOT2(DK140090          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100),NS2(15)    DK140100          
      DIMENSION FAB(5000),FY(630),TOT4(106),SAB(630),FSQ(525),TOT5(106) DK140110          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),MZ(10),IPL(13),NSDK140120          
     1P(13),NND(13),XP(81),YP(41),      NDC(10),NMI(10),MEN(20),NCL(20),DK140130          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(40),NCLN(40),LNE(40),NDEN(1DK140140          
     300),NLIT(40),NMA(30),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(40),DK140150          
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDK140160          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),IC(480),DK140170          
     6EFF1(50),EFF2(50),NOS(300),X(106),NMAC(40),I309(15),NR1(15),NW(15)DK140180          
     7,R1I(50),R2I(50)                                                  DK140190          
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADK140200          
     1B4,LITY,TRED,YM,XP,YP,IM,MS,NEG,IPL,NSP,NND,FAB,FY,TOT4,SAB,FSQ,TODK140210          
     2T5,MZ,R1I,R2I                                                     DK140220          
      COMMON /CMBLK2/LIT,LITR,NLIT,XM,EFF1,EFF2,X,NDC,NMI,MEN,NCL,LME,IBDK140230          
     1EG,IDEN,NCLN,NEN,LNE,NBEG,NDEN,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQDK140240          
     2C,NREGP,LGTX,JBEG,NDECX,NEGY,LNY,LHY,KBEG,NDECY,IC,NOS,NMAC       DK140250          
      COMMON /CMBLK3/RR,REP,DF,SDF,EDF,WK,IJOB,NAB,NLHM,NRHM,NMEA,NME,NNDK140260          
     1EA,NNE,N2F,NPR,IRAN,MPOP,LIOP,IAD,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,DK140270          
     2NOM,NON,ML,MLB,NS,NOT,KPUT,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,IN,I30DK140280          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DK140290          
     1L,L7,KA,KB,KC,SOF,NX,KD,MTY,MAN,NRUN,NRN,EDFF,NAME,EDFS,NAM5,IFP,NDK140300          
     2DFA,IFPC,NDFC,NDFAC,NINT                                          DK140310          
      DATA MU/6HMU    /                                                 DK140320          
      WRITE (6,1001) IJOB                                               DK140330          
 1001 FORMAT (1H0,7X,77HLISTING OF CONSTANTS, LEAST-SQUARES MEANS AND STDK140340          
     1ANDARD ERRORS FOR PROBLEM NO.,I3)                                 DK140350          
      WRITE (6,1002)                                                    DK140360          
 1002 FORMAT (1H0,10H RHM   ROW,30X,3HNO.,7X,8HCONSTANT,6X,13HLEAST-SQUADK140370          
     1RES,7X,8HSTANDARD)                                                DK140380          
      WRITE (6,1003)                                                    DK140390          
 1003 FORMAT (1H ,33H NAME  CODE  INDEPENDENT VARIABLE,7X,4HOBS.,6X,8HESDK140400          
     1TIMATE,10X,4HMEAN,13X,5HERROR)                                    DK140410          
      I=0                                                               DK140420          
      IF (MTY.GT.3.AND.MN2.EQ.1) TOT4(105)=TOT4(105)/FLOAT(NCDS)        DK140430          
    1 I=I+1                                                             DK140440          
      NCT=1                                                             DK140450          
      WRITE (6,1004)                                                    DK140460          
 1004 FORMAT (1H )                                                      DK140470          
      IF (NCPR.EQ.1) GO TO 2                                            DK140480          
      J=I                                                               DK140490          
      GO TO 3                                                           DK140500          
    2 J=NRHM*(I-1)-I*(I-3)/2                                            DK140510          
      LL=J                                                              DK140520          
    3 WK=(SSCPR(J)-TRED(I))/EDF                                         DK140530          
      IF (MN2.EQ.1) WK=SSCPR(J)                                         DK140540          
      IF (MN2.EQ.0) GO TO 91                                            DK140541          
      IF (WK.LT.0.0) WK=+0.0                                            DK140550          
      GO TO (91,92,92,94,94,94,94),MTY                                  DK140560          
   92 IF (SAB(J).LT.0.0) SAB(J)=+0.0                                    DK140570          
      AD=TOT4(104)*SAB(J)*ARRAY(1)                                      DK140580          
      REP=DSQRT(WK*ARRAY(1)+AD)                                         DK140590          
      GO TO 112                                                         DK140600          
   94 IF (FY(J).LT.0.0) FY(J)=+0.0                                      DK140610          
      IF (SAB(J).LT.0.0) SAB(J)=+0.0                                    DK140620          
      IF (MTY.GT.5) GO TO 96                                            DK140630          
      AD=ARRAY(1)*(TOT4(104)*FY(J)+TOT4(106)*SAB(J))                    DK140640          
      REP=DSQRT(WK*ARRAY(1)+AD)                                         DK140650          
      GO TO 112                                                         DK140660          
   96 AD=ARRAY(1)*(TOT4(104)*FY(J)+TOT4(105)*SAB(J))                    DK140670          
      REP=DSQRT(WK*ARRAY(1)+AD)                                         DK140680          
      GO TO 112                                                         DK140690          
   91 REP=DSQRT(WK*ARRAY(1))                                            DK140700          
  112 J=MATX+(I-1)*NLHM+1                                               DK140710          
      SDF=ARRAY(J)+YM(I)                                                DK140720          
      WRITE (6,1005) LITY(I),MU,NCDS,SDF,SDF,REP                        DK140730          
 1005 FORMAT (1H ,A6,3X,1H1,2X,A6,20X,I5,3F17.8)                        DK140740          
      L=1                                                               DK140750          
      L2=1                                                              DK140760          
      K=ML+1                                                            DK140770          
      IF (NOM.EQ.0) GO TO 4                                             DK140780          
      DO 5 J=1,NOM                                                      DK140790          
      K2=NCL(J)                                                         DK140800          
      DO 5 K1=1,K2                                                      DK140810          
      IF (K1.EQ.K2) GO TO 7                                             DK140820          
      K3=IM(J)+K1                                                       DK140830          
      K4=NLHM*(K3-1)-K3*(K3-3)/2                                        DK140840          
      AC=WK*(ARRAY(1)+ARRAY(K4)+2.0*ARRAY(K3))                          DK140850          
      IF (AC.LT.0.0) AC=0.0                                             DK140860          
      REP=DSQRT(AC)                                                     DK140870          
      K4=MATX+(I-1)*NLHM+K3                                             DK140880          
      AC=ARRAY(K4)                                                      DK140890          
      ALS=AC+SDF                                                        DK140900          
      L2=L2+1                                                           DK140910          
      L1=L2                                                             DK140920          
      GO TO 8                                                           DK140930          
    7 K3=IM(J)+1                                                        DK140940          
      K4=MATX+(I-1)*NLHM+K3                                             DK140950          
      AC=0.                                                             DK140960          
      US=0.                                                             DK140970          
      I5=K2-1                                                           DK140980          
      DO 9 M=1,I5                                                       DK140990          
      AC=AC-ARRAY(K4)                                                   DK141000          
      US=US-ARRAY(K3)                                                   DK141010          
      K4=K4+1                                                           DK141020          
    9 K3=K3+1                                                           DK141030          
      K5=IM(J)+K2-1                                                     DK141040          
      SS=0.                                                             DK141050          
      K3=IM(J)+1                                                        DK141060          
      DO 81 M=K3,K5                                                     DK141070          
      K4=NLHM*(M-1)-M*(M-3)/2                                           DK141080          
      DO 10 N=M,K5                                                      DK141090          
      IF (M.EQ.N) GO TO 80                                              DK141100          
      K6=K4+N-M                                                         DK141110          
      SS=SS+2.0*ARRAY(K6)                                               DK141120          
      GO TO 10                                                          DK141130          
   80 SS=SS+ARRAY(K4)                                                   DK141140          
   10 CONTINUE                                                          DK141150          
   81 CONTINUE                                                          DK141160          
      ALS=WK*(ARRAY(1)+SS+2.0*US)                                       DK141170          
      IF (ALS.LT.0.0) ALS=0.0                                           DK141180          
      REP=DSQRT(ALS)                                                    DK141190          
      ALS=AC+SDF                                                        DK141200          
      L1=0                                                              DK141210          
    8 IF (MTY.EQ.1.OR.MN2.EQ.0) GO TO 93                                DK141220          
      SS=REP*REP                                                        DK141230          
      YT=SS                                                             DK141240          
      IF (MTY.EQ.2.OR.MTY.EQ.4) SS=SS+AD                                DK141250          
      IF (J.GT.1.AND.(MTY.EQ.3.OR.MTY.EQ.5.OR.MTY.EQ.6)) SS=SS+AD       DK141260          
      IF (MTY.EQ.7.AND.J.GT.2) SS=SS+AD                                 DK141270          
      IF (MTY.EQ.3.AND.J.EQ.1) SS=SS+(SS*R1I(K1)*SAB(LL))/WK            DK141280          
      IF (J.EQ.1.AND.(MTY.EQ.5.OR.MTY.EQ.7)) SS=SS+(SS*(R1I(K1)*FY(LL)+RDK141290          
     12I(K1)*SAB(LL)))/WK                                               DK141300          
      IF (MTY.EQ.6.AND.J.EQ.1) GO TO 201                                DK141310          
      IF (MTY.EQ.7.AND.J.EQ.2) GO TO 201                                DK141320          
      IF (MTY.EQ.7.AND.J.EQ.1) R2I(K1)=YT/WK                            DK141330          
      GO TO 202                                                         DK141340          
  201 SS=SS+(SS*TOT4(K1)*SAB(LL))/WK+ARRAY(1)*TOT4(104)*FY(LL)          DK141350          
  202 IF (SS.LT.0.0) SS=+0.0                                            DK141360          
      REP=DSQRT(SS)                                                     DK141370          
   93 WRITE (6,1006) LITY(I),L1,LIT(J),IDEN(K),NOS(L),AC,ALS,REP        DK141380          
 1006 FORMAT (1H ,A6,I4,2X,A6,I5,15X,I5,3F17.8)                         DK141390          
      K=K+1                                                             DK141400          
    5 L=L+1                                                             DK141410          
C     -------------------------------------------------------           DK141420          
C     CONSTRUCTION OF X AND TOT2 VECTORS FOR NESTED MAIN EFFECTS        DK141430          
C     -------------------------------------------------------           DK141440          
    4 K=MLB+1                                                           DK141450          
      IF (NON.EQ.0) GO TO 6                                             DK141460          
      DO 11 J=1,NON                                                     DK141470          
      K4=NMA(J)-1                                                       DK141480          
      NSUM=1                                                            DK141490          
      IF (K4.EQ.0) GO TO 13                                             DK141500          
      DO 12 K3=1,K4                                                     DK141510          
   12 NSUM=NSUM+NCL(K3)-1                                               DK141520          
   13 K2=NCLN(J)                                                        DK141530          
      DO 11 K1=1,K2                                                     DK141540          
      DO 14 K3=1,NLHM                                                   DK141550          
      TOT2(K3)=0.                                                       DK141560          
   14 X(K3)=0.                                                          DK141570          
      X(1)=1.                                                           DK141580          
      I2=NOM+J                                                          DK141590          
      K3=IM(I2)+K1                                                      DK141600          
      IF (NMAC(J).NE.NCL(K4+1)) GO TO 15                                DK141610          
      I3=NCL(K4+1)-1                                                    DK141620          
      DO 16 K5=1,I3                                                     DK141630          
      I4=NSUM+K5                                                        DK141640          
   16 X(I4)=-1.0                                                        DK141650          
      IF (K1.NE.K2) GO TO 17                                            DK141660          
   20 I5=K2-1                                                           DK141670          
      DO 18 K5=1,I5                                                     DK141680          
      I4=IM(I2)+K5                                                      DK141690          
      X(I4)=-1.0                                                        DK141700          
   18 TOT2(I4)=-1.0                                                     DK141710          
      L1=0                                                              DK141720          
      GO TO 19                                                          DK141730          
   15 I4=NSUM+NMAC(J)                                                   DK141740          
      X(I4)=1.                                                          DK141750          
      IF (K1.NE.K2) GO TO 17                                            DK141760          
      GO TO 20                                                          DK141770          
   17 TOT2(K3)=1.                                                       DK141780          
      L2=L2+1                                                           DK141790          
      L1=L2                                                             DK141800          
      X(K3)=1.                                                          DK141810          
   19 YT=YM(I)                                                          DK141820          
      NR=MATX+(I-1)*NLHM+1                                              DK141830          
C     ------------------------------------------------------            DK141840          
C     CALL SUBROUTINE WHICH COMPUTES CONSTANT, LS MEAN AND SE, AND LISTSDK141850          
C     ------------------------------------------------------            DK141860          
      CALL CANDSE (AC,ALS,REP,TOT2,X,ARRAY,NR,NLHM,WK,YT,I,SSS,NCT)     DK141870          
      IF (MN2.EQ.0) GO TO 206                                           DK141871          
      IF (MTY.GT.1) REP=DSQRT(REP*REP+AD)                               DK141880          
      IF (NMA(J).EQ.1.AND.(MTY.EQ.3.OR.MTY.EQ.5.OR.MTY.EQ.7)) GO TO 205 DK141890          
      GO TO 206                                                         DK141900          
  205 YT=1.0/FLOAT(K2)                                                  DK141910          
      K6=NMAC(J)                                                        DK141920          
      SS=REP*REP-AD                                                     DK141930          
      IF (MTY.EQ.3) REP=DSQRT(SS+(SS*R1I(K6)*YT*SAB(LL))/WK)            DK141940          
      IF (MTY.EQ.5.OR.MTY.EQ.7) REP=DSQRT(SS+(SS*YT*(R1I(K6)*FY(LL)+R2I(DK141950          
     1K6)*SAB(LL)))/WK)                                                 DK141960          
  206 WRITE (6,1006) LITY(I),L1,NLIT(J),NDEN(K),NOS(L),AC,ALS,REP       DK141970          
      K=K+1                                                             DK141980          
   11 L=L+1                                                             DK141990          
C     ------------------------------------------------------            DK142000          
C     CONSTRUCTION OF X AND TOT2 VECTORS FOR TWO-WAY SUBCLASSES         DK142010          
C     ------------------------------------------------------            DK142020          
    6 NOT=NOM+NON+1                                                     DK142030          
      K8=IM(NOT)                                                        DK142040          
      K6=0                                                              DK142050          
      IF (N2F.EQ.0) GO TO 21                                            DK142060          
      DO 22 J=1,N2F                                                     DK142070          
      MJ1=0                                                             DK142080          
      MJ2=0                                                             DK142090          
      MJ3=0                                                             DK142100          
      MJ4=0                                                             DK142110          
C     ------------------------------------------------------            DK142120          
C     SETS X AND TOT2 ARRAYS TO ZERO AND SETS X(1) TO 1 FOR MU          DK142130          
C     ------------------------------------------------------            DK142140          
      DO 23 K1=1,NLHM                                                   DK142150          
      TOT2(K1)=0.                                                       DK142160          
   23 X(K1)=0.                                                          DK142170          
      X(1)=1.                                                           DK142180          
      K6=K6+NMC(J)                                                      DK142190          
      NSUM=0                                                            DK142200          
      K4=INT2(J)-1                                                      DK142210          
      MSUM=0                                                            DK142220          
      K5=INT1(J)-1                                                      DK142230          
      IF (INT1(J).GT.NOM) GO TO 24                                      DK142240          
      MJ1=INT1(J)                                                       DK142250          
      IF (K5.EQ.0) GO TO 25                                             DK142260          
      DO 26 K3=1,K5                                                     DK142270          
   26 NSUM=NSUM+NCL(K3)                                                 DK142280          
   25 K2=INT1(J)                                                        DK142290          
      I1=IM(K2)+1                                                       DK142300          
      I2=IM(K2+1)+1                                                     DK142310          
      ID1=LIT(K2)                                                       DK142320          
      GO TO 27                                                          DK142330          
C     ------------------------------------------------------            DK142340          
C     SETS UP +1 AND -1 FOR MAJOR CLASS WHEN FIRST MAIN EFFECT IS       DK142350          
C     NESTED                                                            DK142360          
C     ------------------------------------------------------            DK142370          
   24 K2=NOM+1                                                          DK142380          
      IF (K2.GT.K5) GO TO 28                                            DK142390          
      DO 29 K3=K2,K5                                                    DK142400          
      K=K3-NOM                                                          DK142410          
   29 NSUM=NSUM+NCLN(K)                                                 DK142420          
   28 K2=INT1(J)-NOM                                                    DK142430          
      K3=INT1(J)                                                        DK142440          
      I1=IM(K3)+1                                                       DK142450          
      I2=IM(K3+1)+1                                                     DK142460          
      ID1=NLIT(K2)                                                      DK142470          
      K=NMA(K2)                                                         DK142480          
      MJ2=NMA(K2)                                                       DK142490          
      NI=NMAC(K2)                                                       DK142500          
      IF (NMAC(K2).EQ.NCL(K)) GO TO 60                                  DK142510          
      K7=IM(K)+NMAC(K2)                                                 DK142520          
      X(K7)=1.                                                          DK142530          
      GO TO 27                                                          DK142540          
   60 K1=NCL(K)-1                                                       DK142550          
      DO 61 K3=1,K1                                                     DK142560          
      K2=IM(K)+K3                                                       DK142570          
   61 X(K2)=-1.                                                         DK142580          
   27 IF (INT2(J).GT.NOM) GO TO 30                                      DK142590          
      MJ3=INT2(J)                                                       DK142600          
      IF (K4.EQ.0) GO TO 31                                             DK142610          
      DO 32 K3=1,K4                                                     DK142620          
   32 MSUM=MSUM+NCL(K3)                                                 DK142630          
   31 K2=INT2(J)                                                        DK142640          
      I3=IM(K2)+1                                                       DK142650          
      I4=IM(K2+1)+1                                                     DK142660          
      ID2=LIT(K2)                                                       DK142670          
      GO TO 33                                                          DK142680          
C     ------------------------------------------------------            DK142690          
C     SETS UP +1 AND -1 FOR MAJOR CLASS WHEN SECOND MAIN EFFECT IS      DK142700          
C     NESTED                                                            DK142710          
C     ------------------------------------------------------            DK142720          
   30 K2=NOM+1                                                          DK142730          
      IF (K2.GT.K4) GO TO 34                                            DK142740          
      DO 35 K3=K2,K4                                                    DK142750          
      K=K3-NOM                                                          DK142760          
   35 MSUM=MSUM+NCLN(K)                                                 DK142770          
   34 K2=INT2(J)-NOM                                                    DK142780          
      K3=INT2(J)                                                        DK142790          
      I3=IM(K3)+1                                                       DK142800          
      I4=IM(K3+1)+1                                                     DK142810          
      ID2=NLIT(K2)                                                      DK142820          
      K=NMA(K2)                                                         DK142830          
      MJ4=NMA(K2)                                                       DK142840          
      NJ=NMAC(K2)                                                       DK142850          
      IF (NMAC(K2).EQ.NCL(K)) GO TO 62                                  DK142860          
      K7=IM(K)+NMAC(K2)                                                 DK142870          
      X(K7)=1.                                                          DK142880          
      GO TO 111                                                         DK142890          
   62 K1=NCL(K)-1                                                       DK142900          
      DO 63 K3=1,K1                                                     DK142910          
      K2=IM(K)+K3                                                       DK142920          
   63 X(K2)=-1.                                                         DK142930          
C     ------------------------------------------------------            DK142940          
C     SETS UP X MATRIX FOR MAIN EFFECTS AND BOTH X AND TOT2 ARRAYS FOR  DK142950          
C     INTERACTION CONSTANTS                                             DK142960          
C     ------------------------------------------------------            DK142970          
  111 IF (MJ2.NE.MJ4.AND.MJ1.EQ.MJ3) GO TO 100                          DK142980          
      GO TO 33                                                          DK142990          
  100 ME1=MJ2                                                           DK143000          
      ME2=MJ4                                                           DK143010          
      CALL INTACT (ME1,ME2,NI,NJ,N2F,NOT,INT1,INT2,X,NMC,MSCL,NCL,EFF1,EDK143020          
     1FF2,IM,NCLN,NOM)                                                  DK143030          
   33 DO 36 K=I1,I2                                                     DK143040          
      J2=K-I1+1                                                         DK143050          
C     ------------------------------------------------------            DK143060          
C     SETS UP X ARRAY FOR FIRST MAIN EFFECT                             DK143070          
C     ------------------------------------------------------            DK143080          
      L3=I2-1                                                           DK143090          
      IF (K.EQ.I2) GO TO 42                                             DK143100          
      DO 64 K1=I1,L3                                                    DK143110          
   64 X(K1)=0.                                                          DK143120          
      X(K)=1.                                                           DK143130          
      GO TO 41                                                          DK143140          
   42 DO 46 J4=I1,L3                                                    DK143150          
   46 X(J4)=-1.                                                         DK143160          
C     ------------------------------------------------------            DK143170          
C     GETS IDENTIFICATION OF CLASS FOR FIRST MAIN EFFECT                DK143180          
C     ------------------------------------------------------            DK143190          
   41 K2=NSUM+J2                                                        DK143200          
      IF (INT1(J).GT.NOM) GO TO 37                                      DK143210          
      ID3=IDEN(K2)                                                      DK143220          
      GO TO 38                                                          DK143230          
   37 ID3=NDEN(K2)                                                      DK143240          
   38 DO 36 J1=I3,I4                                                    DK143250          
      J3=J1-I3+1                                                        DK143260          
C     ------------------------------------------------------            DK143270          
C     CHECKS FOR MISSING SUBCLASS                                       DK143280          
C     ------------------------------------------------------            DK143290          
      IF (NMC(J).EQ.0) GO TO 45                                         DK143300          
      K7=J2*100+J3                                                      DK143310          
      K1=K6-NMC(J)                                                      DK143320          
   43 K1=K1+1                                                           DK143330          
      IF (K1.GT.K6) GO TO 45                                            DK143340          
      IF (K7-MSCL(K1)) 43,44,43                                         DK143350          
C     ------------------------------------------------------            DK143360          
C     SETS UP X ARRAY FOR SECOND MAIN EFFECT                            DK143370          
C     ------------------------------------------------------            DK143380          
   45 I5=I4-1                                                           DK143390          
      IF (J1.EQ.I4) GO TO 47                                            DK143400          
      DO 40 K1=I3,I5                                                    DK143410          
   40 X(K1)=0.                                                          DK143420          
      X(J1)=1.                                                          DK143430          
      GO TO 48                                                          DK143440          
   47 DO 49 J4=I3,I5                                                    DK143450          
   49 X(J4)=-1.                                                         DK143460          
C     ------------------------------------------------------            DK143470          
C     GETS IDENTIFICATION OF CLASS FOR SECOND MAIN EFFECT               DK143480          
C     ------------------------------------------------------            DK143490          
   48 K2=MSUM+J3                                                        DK143500          
      IF (INT2(J).GT.NOM) GO TO 39                                      DK143510          
      ID4=IDEN(K2)                                                      DK143520          
      GO TO 110                                                         DK143530          
   39 ID4=NDEN(K2)                                                      DK143540          
C     ------------------------------------------------------            DK143550          
C     SETS UP X AND TOT2 ARRAYS FOR INTERACTION EFFECT                  DK143560          
C     ------------------------------------------------------            DK143570          
  110 IF (MJ1.NE.0.AND.MJ3.EQ.0) GO TO 101                              DK143580          
      GO TO 102                                                         DK143590          
  101 ME1=MJ1                                                           DK143600          
      ME2=MJ4                                                           DK143610          
      NI=J2                                                             DK143620          
      CALL INTACT (ME1,ME2,NI,NJ,N2F,NOT,INT1,INT2,X,NMC,MSCL,NCL,EFF1,EDK143630          
     1FF2,IM,NCLN,NOM)                                                  DK143640          
  102 IF (MJ3.NE.0.AND.MJ1.EQ.0) GO TO 103                              DK143650          
      GO TO 104                                                         DK143660          
  103 ME1=MJ2                                                           DK143670          
      ME2=MJ3                                                           DK143680          
      NJ=J3                                                             DK143690          
      CALL INTACT (ME1,ME2,NI,NJ,N2F,NOT,INT1,INT2,X,NMC,MSCL,NCL,EFF1,EDK143700          
     1FF2,IM,NCLN,NOM)                                                  DK143710          
  104 IF (MJ2.NE.MJ4.AND.MJ1.EQ.MJ3) GO TO 105                          DK143720          
      GO TO 74                                                          DK143730          
  105 ME1=INT1(J)                                                       DK143740          
      ME2=MJ4                                                           DK143750          
      NI=J2                                                             DK143760          
      K2=INT2(J)-NOM                                                    DK143770          
      NJ=NMAC(K2)                                                       DK143780          
      CALL INTACT (ME1,ME2,NI,NJ,N2F,NOT,INT1,INT2,X,NMC,MSCL,NCL,EFF1,EDK143790          
     1FF2,IM,NCLN,NOM)                                                  DK143800          
      ME1=MJ2                                                           DK143810          
      ME2=INT2(J)                                                       DK143820          
      K2=INT1(J)-NOM                                                    DK143830          
      NI=NMAC(K2)                                                       DK143840          
      NJ=J3                                                             DK143850          
      CALL INTACT (ME1,ME2,NI,NJ,N2F,NOT,INT1,INT2,X,NMC,MSCL,NCL,EFF1,EDK143860          
     1FF2,IM,NCLN,NOM)                                                  DK143870          
   74 L3=I2-I1                                                          DK143880          
      I5=I4-I3                                                          DK143890          
      N=NOT+J-1                                                         DK143900          
      K2=IM(N)+1                                                        DK143910          
      N=L3*I5-NMC(J)                                                    DK143920          
      M=K2+N-1                                                          DK143930          
      DO 59 J4=K2,M                                                     DK143940          
      TOT2(J4)=0.                                                       DK143950          
   59 X(J4)=0.                                                          DK143960          
      DO 50 J4=1,L3                                                     DK143970          
   50 EFF1(J4)=0.                                                       DK143980          
      DO 51 J4=1,I5                                                     DK143990          
   51 EFF2(J4)=0.                                                       DK144000          
      IF  (K.EQ.I2) GO TO 52                                            DK144010          
      EFF1(J2)=1.                                                       DK144020          
      GO TO 53                                                          DK144030          
   52 DO 54 J4=1,L3                                                     DK144040          
   54 EFF1(J4)=-1.                                                      DK144050          
   53 IF (J1.EQ.I4) GO TO 55                                            DK144060          
      EFF2(J3)=1.                                                       DK144070          
      GO TO 56                                                          DK144080          
   55 DO 76 J4=1,I5                                                     DK144090          
   76 EFF2(J4)=-1.                                                      DK144100          
   56 K5=K8+1                                                           DK144110          
      DO 82 K1=1,L3                                                     DK144120          
      DO 58 K2=1,I5                                                     DK144130          
      K7=K1*100+K2                                                      DK144140          
      IF (NMC(J).EQ.0) GO TO 71                                         DK144150          
      J4=K6-NMC(J)                                                      DK144160          
   70 J4=J4+1                                                           DK144170          
      IF (J4.GT.K6) GO TO 71                                            DK144180          
      IF (K7-MSCL(J4)) 70,58,70                                         DK144190          
   71 X(K5)=EFF1(K1)*EFF2(K2)                                           DK144200          
      TOT2(K5)=X(K5)                                                    DK144210          
      K5=K5+1                                                           DK144220          
   58 CONTINUE                                                          DK144230          
   82 CONTINUE                                                          DK144240          
      IF  (K.EQ.I2.OR.J1.EQ.I4) GO TO 72                                DK144250          
      L2=L2+1                                                           DK144260          
      L1=L2                                                             DK144270          
      GO TO 73                                                          DK144280          
   72 L1=0                                                              DK144290          
   73 NR=MATX+(I-1)*NLHM+1                                              DK144300          
      YT=YM(I)                                                          DK144310          
      CALL CANDSE (AC,ALS,REP,TOT2,X,ARRAY,NR,NLHM,WK,YT,I,SSS,NCT)     DK144320          
      IF (MN2.EQ.0) GO TO 204                                           DK144321          
      IF (MTY.GT.1) REP=DSQRT(REP*REP+AD)                               DK144330          
      IF ((MTY.EQ.3.OR.MTY.GT.4).AND.INT1(J).EQ.1) GO TO 203            DK144340          
      GO TO 204                                                         DK144350          
  203 SS=REP*REP-AD                                                     DK144360          
      K1=K-I1+1                                                         DK144370          
      K2=INT2(J)                                                        DK144380          
      YT=1.0/FLOAT(NCL(K2))                                             DK144390          
      IF (MTY.EQ.3) REP=DSQRT(SS+(SS*YT*R1I(K1)*SAB(LL))/WK)            DK144400          
      IF (MTY.EQ.5) REP=DSQRT(SS+(SS*YT*(R1I(K1)*FY(LL)+R2I(K1)*SAB(LL))DK144410          
     1)/WK)                                                             DK144420          
      IF (MTY.EQ.6) REP=DSQRT(SS+(SS*YT*TOT4(K1)*(SAB(LL)+FY(LL)))/WK)  DK144430          
      IF (MTY.EQ.7) REP=DSQRT(SS+(SS*YT*R1I(K1)*SAB(LL))/WK+R2I(K1)*YT*RDK144440          
     11I(K1)*FY(LL))                                                    DK144450          
  204 WRITE (6,1007) LITY(I),L1,ID1,ID2,ID3,ID4,NOS(L),AC,ALS,REP       DK144460          
 1007 FORMAT (1H ,A6,I4,2X,A6,3H X ,A6,2I5,I6,3F17.8)                   DK144470          
   44 L=L+1                                                             DK144480          
   36 CONTINUE                                                          DK144490          
      K8=K8+L3*I5-NMC(J)                                                DK144500          
   22 CONTINUE                                                          DK144510          
C     ------------------------------------------------------            DK144520          
C     LISTING OF PARTIAL REGRESSIONS AND STANDARD ERRORS FOR            DK144530          
C     CONTINUOUS INDEPENDENT VARIABLES                                  DK144540          
C     ------------------------------------------------------            DK144550          
   21 IF (NPR.EQ.0) GO TO 75                                            DK144560          
      L1=L2+1                                                           DK144570          
      DO 57 J=IE,NLHM                                                   DK144580          
      L=NLHM*(J-1)-J*(J-3)/2                                            DK144590          
      K=MATX+(I-1)*NLHM+J                                               DK144600          
      AC=ARRAY(L)*WK                                                    DK144610          
      IF (AC.LT.0.0) AC=0.0                                             DK144620          
      REP=DSQRT(AC)                                                     DK144630          
      WRITE (6,1008) LITY(I),L1,LAB1(J),LAB2(J),LAB3(J),LAB4(J),ARRAY(K)DK144640          
     1,REP                                                              DK144650          
 1008 FORMAT (1H ,A6,I4,4(1X,A6),5X,F17.8,17X,F17.8)                    DK144660          
   57 L1=L1+1                                                           DK144670          
   75 IF (I.NE.NRHM) GO TO 1                                            DK144680          
      RETURN                                                            DK144690          
      END                                                               DK144700          
*ENDTEXT                                                                                  
