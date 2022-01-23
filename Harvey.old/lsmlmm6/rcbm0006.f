*TEXT                                                                                     
      SUBROUTINE RCBM                                                   DK050010          
C     -------------------------------------------                       DK050020          
C     SUBROUTINE WHICH READS DATA CARDS, CALLS CODEX, COMPUTES LS OR ML DK050030          
C     MATRICES, CALLS SCLNOS AND LISTS CERTAIN MATRICES, MEANS, SD AND  DK050040          
C     CORRELATIONS                                                      DK050050          
C     ----------------------------------------------------              DK050060          
      IMPLICIT REAL*8(A-H,O-Z)                                          DK050070          
      REAL*8 LAB1,LAB2,LAB3,LAB4,LITY,NLIT,LIT,LITR,NAME,NAM5           DK050080          
      DIMENSION ARRAY(5000),SSCPR(630),SSS(630),RHM(2450),TOT(106),TOT2(DK050090          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100),NS2(15)    DK050100          
      DIMENSION FAB(5000),FY(630),TOT4(106),SAB(630),FSQ(525),TOT5(106) DK050110          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),MZ(10),IPL(13),NSDK050120          
     1P(13),NND(13),XP(81),YP(41),      NDC(10),NMI(10),MEN(20),NCL(20),DK050130          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(40),NCLN(40),LNE(40),NDEN(1DK050140          
     300),NLIT(40),NMA(30),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(40),DK050150          
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDK050160          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),IC(480),DK050170          
     6EFF1(50),EFF2(50),NOS(300),X(106),NMAC(40),I309(15),NR1(15),NW(15)DK050180          
     7,R1I(50),R2I(50)                                                  DK050190          
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADK050200          
     1B4,LITY,TRED,YM,XP,YP,IM,MS,NEG,IPL,NSP,NND,FAB,FY,TOT4,SAB,FSQ,TODK050210          
     2T5,MZ,R1I,R2I                                                     DK050220          
      COMMON /CMBLK2/LIT,LITR,NLIT,XM,EFF1,EFF2,X,NDC,NMI,MEN,NCL,LME,IBDK050230          
     1EG,IDEN,NCLN,NEN,LNE,NBEG,NDEN,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQDK050240          
     2C,NREGP,LGTX,JBEG,NDECX,NEGY,LNY,LHY,KBEG,NDECY,IC,NOS,NMAC       DK050250          
      COMMON /CMBLK3/RR,REP,DF,SDF,EDF,WK,IJOB,NAB,NLHM,NRHM,NMEA,NME,NNDK050260          
     1EA,NNE,N2F,NPR,IRAN,MPOP,LIOP,IAD,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,DK050270          
     2NOM,NON,ML,MLB,NS,NOT,KPUT,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,IN,I30DK050280          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DK050290          
     1L,L7,KA,KB,KC,SOF,NX,KD,MTY,MAN,NRUN,NRN,EDFF,NAME,EDFS,NAM5,IFP,NDK050300          
     2DFA,IFPC,NDFC,NDFAC,NINT                                          DK050310          
      DATA IBLK,JBLK/Z40404040,Z00000040/                               DK050320          
      REWIND 2                                                          DK050330          
      IF (LIOP.EQ.1.OR.LIOP.EQ.3) GO TO 1                               DK050340          
      GO TO 2                                                           DK050350          
    1 WRITE (6,1001) IJOB                                               DK050360          
 1001 FORMAT (1H1,20X,35HLISTING OF X MATRIX FOR PROBLEM NO.,I3)        DK050370          
    2 MATX=NLHM*(NLHM+1)/2                                              DK050380          
      DO 4 I=1,MATX                                                     DK050390          
    4 ARRAY(I)=0.0                                                      DK050400          
      K1=NLHM*NRHM                                                      DK050410          
      DO 5 I=1,K1                                                       DK050420          
    5 RHM(I)=0.0                                                        DK050430          
      IF (NCPR.EQ.0) GO TO 6                                            DK050440          
      KA=NRHM*(NRHM+1)/2                                                DK050450          
      MIN=0                                                             DK050460          
      IF (NAB.EQ.2) MIN=1                                               DK050470          
      GO TO 7                                                           DK050480          
    6 KA=NRHM                                                           DK050490          
    7 IF (NAB.EQ.5.OR.NAB.EQ.6) GO TO 3                                 DK050500          
      GO TO 11                                                          DK050510          
    3 CALL MIXED (NAB,MATX,KA,KB,KC,NOT,TOT,NMIG,NLHM,NCPR,NRHM,TOT3,ARRDK050520          
     1AY,NCDG,FAB,TOT4,SOF,NX,KD)                                       DK050530          
      IF ((MTY.EQ.4.OR.MTY.EQ.5).AND.NRN.EQ.1) KD=2                     DK050540          
   11 DO 8 I=1,KA                                                       DK050550          
    8 SSCPR(I)=0.0                                                      DK050560          
      NOT=NLHM+NRHM                                                     DK050570          
      DO 9 I=1,NOT                                                      DK050580          
      TOT(I)=0.0                                                        DK050590          
      TOT2(I)=0.0                                                       DK050600          
    9 TOT3(I)=0.0                                                       DK050610          
      DO 10 I=1,200                                                     DK050620          
   10 NOS(I)=0                                                          DK050630          
      K1=0                                                              DK050640          
      NCDS=0                                                            DK050650          
      LL=1                                                              DK050660          
      NSM=0                                                             DK050670          
      NSUM=0                                                            DK050680          
      GNI=0.                                                            DK050690          
      MJN=0                                                             DK050700          
      NCDG=0                                                            DK050710          
      NMIG=1                                                            DK050720          
      IF (MTY.GT.5.AND.NRN.EQ.NRUN) GO TO 227                           DK050730          
      GO TO 228                                                         DK050740          
  227 DO 229 I=1,50                                                     DK050750          
  229 TOT4(I)=R2I(I)                                                    DK050760          
  228 IF (KB.EQ.1.AND.MTY.GT.5) GO TO 225                               DK050770          
      GO TO 20                                                          DK050780          
  225 DO 226 I=1,50                                                     DK050790          
      R1I(I)=0.0                                                        DK050800          
  226 R2I(I)=0.0                                                        DK050810          
   20 K=1                                                               DK050820          
      NAB2=0                                                            DK050830          
      IF (NAB.EQ.2.AND.(MTY.EQ.3.OR.MTY.EQ.5.OR.MTY.EQ.7)) NAB2=1       DK050840          
      L=80                                                              DK050850          
      CALL RCARD(IN,K,L,IC,IDUM,+17)                                    DK050860          
 1000 FORMAT (80A1)                                                     DK050870          
      NTRA=0                                                            DK050880          
      DO 21 I=1,80                                                      DK050890          
   21 IF (IC(I).NE.JBLK) NTRA=1                                         DK050900          
      GO TO 18                                                          DK050910          
   17 NTRA=0                                                            DK050920          
   18 IF (NTRA.EQ.0) GO TO 156                                          DK050930          
      NCDS=NCDS+1                                                       DK050940          
      IF (NCD.LT.2) GO TO 32                                            DK050950          
      DO 30 J=2,NCD                                                     DK050960          
      L=J*80                                                            DK050970          
      K=L-79                                                            DK050980          
      CALL RCARD(IN,K,L,IC,J,+9999)                                     DK050990          
 9999 K1=ICN1+K-1                                                       DK051000          
      K2=K1-80                                                          DK051010          
      IF (IC(K1).LE.IC(K2)) GO TO 901                                   DK051020          
   30 CONTINUE                                                          DK051030          
   32 CONTINUE                                                          DK051040          
      IF (NAB.GT.1.OR.KD.GT.0) GO TO 100                                DK051050          
   36 CONTINUE                                                          DK051060          
      CALL XMAT                                                         DK051070          
      IF (MULL.EQ.1) RETURN                                             DK051080          
      IF (LIOP.EQ.1.OR.LIOP.EQ.3) GO TO 41                              DK051090          
      GO TO 39                                                          DK051100          
   41 WRITE (6,1002) (X(I),I=1,NOT)                                     DK051110          
 1002 FORMAT (1H ,17F7.2)                                               DK051120          
   39 IF (NLHM.EQ.0) GO TO 47                                           DK051130          
   42 DO 46 I=1,NLHM                                                    DK051140          
      IF (X(I).EQ.0.0) GO TO 46                                         DK051150          
      DO 45 J=I,NLHM                                                    DK051160          
      K=NLHM*(I-1)-I*(I-3)/2+J-I                                        DK051170          
   45 ARRAY(K)=ARRAY(K)+X(I)*X(J)                                       DK051180          
   46 CONTINUE                                                          DK051190          
   47 DO 50 I=1,NOT                                                     DK051200          
      IF (KB.EQ.0.AND.KD.EQ.0) GO TO 50                                 DK051210          
      TOT3(I)=TOT3(I)+X(I)                                              DK051220          
   50 TOT(I)=TOT(I)+X(I)                                                DK051230          
      IF (NRHM.EQ.0) GO TO 20                                           DK051240          
      IF (NCPR.EQ.0) GO TO 56                                           DK051250          
      DO 55 I=1,NRHM                                                    DK051260          
      DO 55 J=I,NRHM                                                    DK051270          
      K=NRHM*(I-1)-I*(I-3)/2+J-I                                        DK051280          
      K2=NLHM+I                                                         DK051290          
      K3=NLHM+J                                                         DK051300          
   55 SSCPR(K)=SSCPR(K)+X(K2)*X(K3)                                     DK051310          
      GO TO 58                                                          DK051320          
   56 CONTINUE                                                          DK051330          
      DO 57 I=1,NRHM                                                    DK051340          
      K2=NLHM+I                                                         DK051350          
   57 SSCPR(I)=SSCPR(I)+X(K2)*X(K2)                                     DK051360          
   58 K=0                                                               DK051370          
      DO 59 I=1,NRHM                                                    DK051380          
      J2=NLHM+I                                                         DK051390          
      DO 59 J=1,NLHM                                                    DK051400          
      K=K+1                                                             DK051410          
   59 RHM(K)=RHM(K)+X(J)*X(J2)                                          DK051420          
      GO TO 20                                                          DK051430          
C     ----------------------------------------------------              DK051440          
C     ABSORPTION PROCESS                                                DK051450          
C     ----------------------------------------------------              DK051460          
  100 CONTINUE                                                          DK051470          
      IF (NAB.LT.4.AND.KB.EQ.0) GO TO 110                               DK051480          
      IMJ1=0                                                            DK051490          
      DO 102 I=1,NMJC                                                   DK051500          
      K=NDC(I)                                                          DK051510          
      IF (IC(K).NE.JBLK) GO TO 102                                      DK051520          
      IF (I.EQ.NMJC) GO TO 900                                          DK051530          
      IC(K)=0                                                           DK051540          
  102 IMJ1=IC(K)+IMJ1*10                                                DK051550          
      IMI1=0                                                            DK051560          
      DO 104 I=1,NMIC                                                   DK051570          
      K=NMI(I)                                                          DK051580          
      IF (IC(K).NE.JBLK) GO TO 104                                      DK051590          
      IF (I.EQ.NMIC) GO TO 900                                          DK051600          
      IC(K)=0                                                           DK051610          
  104 IMI1=IC(K)+IMI1*10                                                DK051620          
      IF (NCDS.EQ.1) GO TO 108                                          DK051630          
      IF (IMJ2-IMJ1) 164,107,901                                        DK051640          
  107 IF (IMI2-IMI1) 164,106,901                                        DK051650          
  106 NMIG=NMIG+1                                                       DK051660          
  108 IMI2=IMI1                                                         DK051670          
  109 IMJ2=IMJ1                                                         DK051680          
      GO TO 36                                                          DK051690          
  110 IMJ1=0                                                            DK051700          
      DO 112 I=1,NCC                                                    DK051710          
      K=NDC(I)                                                          DK051720          
      IF (IC(K).NE.JBLK) GO TO 112                                      DK051730          
      IF (I.EQ.NCC) GO TO 900                                           DK051740          
      IC(K)=0                                                           DK051750          
  112 IMJ1=IC(K)+IMJ1*10                                                DK051760          
      IF (NAB.EQ.2) GO TO 201                                           DK051770          
      IF (NCDS.EQ.1) GO TO 114                                          DK051780          
      IF (IMJ2-IMJ1) 116,114,901                                        DK051790          
  114 NCDG=NCDG+1                                                       DK051800          
      GO TO 109                                                         DK051810          
  201 IIN1=0                                                            DK051820          
      K2=0                                                              DK051830          
      IF (NINT.EQ.0) GO TO 203                                          DK051840          
      DO 202 I=1,NINT                                                   DK051850          
      K=MZ(I)                                                           DK051860          
      IF (IC(K).NE.JBLK) GO TO 202                                      DK051870          
      IF (I.EQ.NINT) GO TO 900                                          DK051880          
      IC(K)=0                                                           DK051890          
  202 IIN1=IIN1*10+IC(K)                                                DK051900          
      IF (MTY.EQ.3.AND.NCDS.EQ.1) GO TO 212                             DK051910          
      IMI1=0                                                            DK051920          
      IF (MTY.EQ.3.OR.NMIC.EQ.0) GO TO 203                              DK051930          
      DO 204 I=1,NMIC                                                   DK051940          
      K=NMI(I)                                                          DK051950          
      IF (IC(K).NE.JBLK) GO TO 204                                      DK051960          
      IF (I.EQ.NMIC) GO TO 900                                          DK051970          
      IC(K)=0                                                           DK051980          
  204 IMI1=IMI1*10+IC(K)                                                DK051990          
  203 IF (NCDS.EQ.1) GO TO 213                                          DK052000          
      IF (IMJ2-IMJ1) 206,207,901                                        DK052010          
  207 NMIG=NMIG+1                                                       DK052020          
      IF (NINT.EQ.0) GO TO 212                                          DK052030          
      IF (IIN2-IIN1) 206,209,901                                        DK052040          
  209 MIN=MIN+1                                                         DK052050          
      IF (MTY.EQ.3.OR.NMIC.EQ.0) GO TO 212                              DK052060          
      IF (IMI2-IMI1) 206,213,901                                        DK052070          
  213 IMI2=IMI1                                                         DK052080          
  212 IIN2=IIN1                                                         DK052090          
      IF (K2.EQ.1) GO TO 116                                            DK052100          
      GO TO 114                                                         DK052110          
  206 IF (NAB2.EQ.0) GO TO 217                                          DK052120          
      NSUM=NSUM+NCDG*NCDG                                               DK052130          
      IF (IMJ2.LT.IMJ1.OR.IIN2.LT.IIN1) GO TO 214                       DK052140          
      K2=1                                                              DK052150          
      GO TO 213                                                         DK052160          
  217 K2=1                                                              DK052170          
      IMJ2=IMJ1                                                         DK052180          
      GO TO 213                                                         DK052190          
  214 NSM=NSM+MIN*MIN                                                   DK052200          
      MIN=1                                                             DK052210          
      IF (IMJ2.LT.IMJ1) GO TO 215                                       DK052220          
      K2=1                                                              DK052230          
      GO TO 213                                                         DK052240          
  215 IMJ2=IMJ1                                                         DK052250          
      R1I(LL)=FLOAT(NSM)/FLOAT(NMIG)                                    DK052260          
      R2I(LL)=FLOAT(NSUM)/FLOAT(NMIG)                                   DK052270          
      NMIG=1                                                            DK052280          
      NSM=0                                                             DK052290          
      NSUM=0                                                            DK052300          
      LL=LL+1                                                           DK052310          
      K2=1                                                              DK052320          
      GO TO 213                                                         DK052330          
  116 DM=NCDG                                                           DK052340          
      IF (KD.EQ.0) GO TO 115                                            DK052350          
      KC=1                                                              DK052360          
      NMIG=NCDG                                                         DK052370          
      CALL MIXED (NAB,MATX,KA,KB,KC,NOT,TOT,NMIG,NLHM,NCPR,NRHM,TOT3,ARRDK052380          
     1AY,NCDG,FAB,TOT4,SOF,NX,KD)                                       DK052390          
      GO TO 109                                                         DK052400          
  115 IF (NAB.EQ.2) GO TO 118                                           DK052410          
      ROWN=1./(DM+REP)                                                  DK052420          
      GO TO 120                                                         DK052430          
  118 ROWN=1./DM                                                        DK052440          
  120 IF (NLHM.EQ.0) GO TO 130                                          DK052450          
      DO 124 I=1,NLHM                                                   DK052460          
      IF (TOT(I).EQ.0.0) GO TO 124                                      DK052470          
      DO 122 J=I,NLHM                                                   DK052480          
      K=NLHM*(I-1)-I*(I-3)/2+J-I                                        DK052490          
  122 ARRAY(K)=ARRAY(K)-TOT(I)*TOT(J)*ROWN                              DK052500          
  124 CONTINUE                                                          DK052510          
  130 IF (LIOP.EQ.0.OR.LIOP.EQ.5.OR.LIOP.GT.9)  GO TO 135               DK052520          
      IF (NAB.EQ.4) GO TO 132                                           DK052530          
      WRITE (6,1050) IMJ2,NCDG                                          DK052540          
 1050 FORMAT (1H ,10X,14HMINOR CLASS IS,I12,6H   NI=,I4/1H ,   6HTOTALS)DK052550          
      GO TO 134                                                         DK052560          
  132 WRITE (6,1050) IMI2,NMIG                                          DK052570          
  134 WRITE (6,1052) (TOT(I),I=1,NOT)                                   DK052580          
  135 IF (NRHM.EQ.0) GO TO 146                                          DK052590          
      IF (NCPR.EQ.0) GO TO 138                                          DK052600          
      DO 136 I=1,NRHM                                                   DK052610          
      DO 136 J=I,NRHM                                                   DK052620          
      K=NRHM*(I-1)-I*(I-3)/2+J-I                                        DK052630          
      K2=NLHM+I                                                         DK052640          
      K3=NLHM+J                                                         DK052650          
  136 SSCPR(K)=SSCPR(K)-TOT(K2)*TOT(K3)*ROWN                            DK052660          
      GO TO 142                                                         DK052670          
  138 DO 140 I=1,NRHM                                                   DK052680          
      K2=NLHM+I                                                         DK052690          
  140 SSCPR(I)=SSCPR(I)-TOT(K2)*TOT(K2)*ROWN                            DK052700          
  142 IF (NLHM.EQ.0) GO TO 146                                          DK052710          
      K=0                                                               DK052720          
      DO 144 I=1,NRHM                                                   DK052730          
      J2=NLHM+I                                                         DK052740          
      DO 144 J=1,NLHM                                                   DK052750          
      K=K+1                                                             DK052760          
  144 RHM(K)=RHM(K)-TOT(J)*TOT(J2)*ROWN                                 DK052770          
  146 IF (NAB.EQ.1) GO TO 299                                           DK052780          
      DO 148 I=1,NOT                                                    DK052790          
      IF (KB.EQ.1) GO TO 148                                            DK052800          
      TOT3(I)=TOT3(I)+TOT(I)                                            DK052810          
  148 TOT2(I)=TOT2(I)+TOT(I)                                            DK052820          
      IF (NAB.NE.4) GO TO 152                                           DK052830          
      DO 150 I=1,NOT                                                    DK052840          
      TOT2(I)=TOT2(I)-DM*TOT(I)*ROWN                                    DK052850          
  150 TOT(I)=0.0                                                        DK052860          
      GNI=GNI-DM*DM*ROWN                                                DK052870          
      GO TO 166                                                         DK052880          
  152 MJN=MJN+1                                                         DK052890          
      DO 154 I=1,NOT                                                    DK052900          
  154 TOT(I)=0.0                                                        DK052910          
      NCDG=1                                                            DK052920          
      IF (KB.EQ.1) NCDG=0                                               DK052930          
      IF (NTRA.EQ.0) GO TO 193                                          DK052940          
      IF (KB.EQ.1) GO TO 108                                            DK052950          
      GO TO 109                                                         DK052960          
  156 IF (KB.EQ.1) GO TO 158                                            DK052970          
      IF (NAB.EQ.2.AND.(MTY.EQ.3.OR.MTY.EQ.5.OR.MTY.EQ.7)) GO TO 216    DK052980          
      IF (NAB.NE.0) GO TO 158                                           DK052990          
      DF=NCDS                                                           DK053000          
      GO TO 299                                                         DK053010          
  216 NSUM=NSUM+NCDG*NCDG                                               DK053020          
      NSM=NSM+MIN*MIN                                                   DK053030          
      R1I(LL)=FLOAT(NSM)/FLOAT(NMIG)                                    DK053040          
      R2I(LL)=FLOAT(NSUM)/FLOAT(NMIG)                                   DK053050          
  158 IF (NAB.NE.1) GO TO 160                                           DK053060          
      DF=NCDS-1                                                         DK053070          
      GO TO 162                                                         DK053080          
  160 IF (KB.NE.1) GO TO 161                                            DK053090          
      NCDG=NCDG+NMIG                                                    DK053100          
      IF (MTY.LT.6) GO TO 230                                           DK053110          
      K=NCL(1)                                                          DK053120          
      DO 220 I=1,K                                                      DK053130          
      R2I(I)=R2I(I)+R1I(I)*R1I(I)                                       DK053140          
      R2I(I)=R2I(I)/FLOAT(NOS(I))                                       DK053150          
  220 R1I(I)=0.0                                                        DK053160          
  230 KC=4                                                              DK053170          
      CALL MIXED (NAB,MATX,KA,KB,KC,NOT,TOT,NMIG,NLHM,NCPR,NRHM,TOT3,ARRDK053180          
     1AY,NCDG,FAB,TOT4,SOF,NX,KD)                                       DK053190          
  161 IF (NAB.LT.4) GO TO 116                                           DK053200          
      DF=NCDS-1                                                         DK053210          
      GO TO 164                                                         DK053220          
  162 DM=NCDS                                                           DK053230          
      ROWN=1./DM                                                        DK053240          
      GO TO 120                                                         DK053250          
  164 NCDG=NCDG+NMIG                                                    DK053260          
      IF (KB.NE.1) GO TO 163                                            DK053270          
      IF (MTY.LT.6) GO TO 231                                           DK053280          
      K=NCL(1)                                                          DK053290          
      DO 221 I=1,K                                                      DK053300          
      R2I(I)=R2I(I)+R1I(I)*R1I(I)                                       DK053310          
  221 R1I(I)=0.0                                                        DK053320          
  231 KC=1                                                              DK053330          
      CALL MIXED (NAB,MATX,KA,KB,KC,NOT,TOT,NMIG,NLHM,NCPR,NRHM,TOT3,ARRDK053340          
     1AY,NCDG,FAB,TOT4,SOF,NX,KD)                                       DK053350          
      IF (IMJ2.EQ.IMJ1) GO TO 108                                       DK053360          
      KC=2                                                              DK053370          
      CALL MIXED (NAB,MATX,KA,KB,KC,NOT,TOT,NMIG,NLHM,NCPR,NRHM,TOT3,ARRDK053380          
     1AY,NCDG,FAB,TOT4,SOF,NX,KD)                                       DK053390          
      GO TO 116                                                         DK053400          
  163 DM=NMIG                                                           DK053410          
      NCDG=NCDG+NMIG                                                    DK053420          
      ROWN=1./(DM+REP)                                                  DK053430          
      GNI=GNI+DM                                                        DK053440          
      GO TO 120                                                         DK053450          
  166 MIN=MIN+1                                                         DK053460          
      NMIG=1                                                            DK053470          
      IF (NTRA.EQ.0.OR.IMJ1.NE.IMJ2) GO TO 168                          DK053480          
      GO TO 108                                                         DK053490          
  168 ROWN=1./GNI                                                       DK053500          
      IF (NLHM.EQ.0) GO TO 172                                          DK053510          
      DO 170 I=1,NLHM                                                   DK053520          
      DO 170 J=I,NLHM                                                   DK053530          
      K=NLHM*(I-1)-I*(I-3)/2+J-I                                        DK053540          
  170 ARRAY(K)=ARRAY(K)-TOT2(I)*TOT2(J)*ROWN                            DK053550          
  172 IF (NRHM.EQ.0) GO TO 184                                          DK053560          
      IF (NCPR.EQ.0) GO TO 176                                          DK053570          
      DO 174 I=1,NRHM                                                   DK053580          
      DO 174 J=I,NRHM                                                   DK053590          
      K=NRHM*(I-1)-I*(I-3)/2+J-I                                        DK053600          
      K3=NLHM+J                                                         DK053610          
      K2=NLHM+I                                                         DK053620          
  174 SSCPR(K)=SSCPR(K)-TOT2(K2)*TOT2(K3)*ROWN                          DK053630          
      GO TO 180                                                         DK053640          
  176 DO 178 I=1,NRHM                                                   DK053650          
      K2=NLHM+I                                                         DK053660          
  178 SSCPR(I)=SSCPR(I)-TOT2(K2)*TOT2(K2)*ROWN                          DK053670          
  180 K=0                                                               DK053680          
      DO 182 I=1,NRHM                                                   DK053690          
      J2=NLHM+I                                                         DK053700          
      DO 182 J=1,NLHM                                                   DK053710          
      K=K+1                                                             DK053720          
  182 RHM(K)=RHM(K)-TOT2(J)*TOT2(J2)*ROWN                               DK053730          
  184 IF (LIOP.EQ.0.OR.LIOP.EQ.5.OR.LIOP.GT.9)  GO TO 190               DK053740          
      WRITE (6,1051) IMJ2,GNI                                           DK053750          
 1051 FORMAT (1H0,5X,14HMAJOR CLASS IS,I12,3X,31HREDUCED DIAGONAL ELEMENDK053760          
     1T (GNI)=,F18.8)                                                   DK053770          
      WRITE (6,1052) (TOT2(I),I=1,NOT)                                  DK053780          
 1052 FORMAT (1H ,9F13.3)                                               DK053790          
  190 DO 191 I=1,NOT                                                    DK053800          
  191 TOT2(I)=0.0                                                       DK053810          
      MJN=MJN+1                                                         DK053820          
      GNI=0.0                                                           DK053830          
      NCDG=0                                                            DK053840          
      IF (NTRA.EQ.0) GO TO 196                                          DK053850          
      GO TO 108                                                         DK053860          
  193 DO 194 I=1,NOT                                                    DK053870          
  194 TOT(I)=TOT2(I)                                                    DK053880          
      DF=NCDS-MJN                                                       DK053890          
      IF (NAB.EQ.3) DF=DF+FLOAT(MJN)                                    DK053900          
      GO TO 299                                                         DK053910          
  196 DO 198 I=1,NOT                                                    DK053920          
  198 TOT(I)=TOT3(I)                                                    DK053930          
      GO TO 299                                                         DK053940          
  900 WRITE (6,1053) NCDS                                               DK053950          
 1053 FORMAT (1H0,69HUNITS POSITION OF AN ID FIELD OR A CONTROL FIELD ISDK053960          
     1 BLANK ON CARD NO.,I5)                                            DK053970          
      GO TO 902                                                         DK053980          
  901 WRITE (6,1054) NCDS,IJOB                                          DK053990          
 1054 FORMAT (1H0,38HCARDS OUT OF SEQUENCE---CHECK CARD NO.,I5,5X,15HFORDK054000          
     1 PROBLEM NO.,I3)                                                  DK054010          
  902 MULL=1                                                            DK054020          
      RETURN                                                            DK054030          
  299 CONTINUE                                                          DK054040          
      IF (NAB.EQ.0) GO TO 300                                           DK054050          
      GO TO (301,302,303,304), NAB                                      DK054060          
  300 IF (KD.EQ.0) GO TO 305                                            DK054070          
      KC=4                                                              DK054080          
      NMIG=NCDG                                                         DK054090          
      NCDG=NCDS                                                         DK054100          
      CALL MIXED (NAB,MATX,KA,KB,KC,NOT,TOT,NMIG,NLHM,NCPR,NRHM,TOT3,ARRDK054110          
     1AY,NCDG,FAB,TOT4,SOF,NX,KD)                                       DK054120          
  305 WRITE (6,1060) NCDS                                               DK054130          
 1060 FORMAT (1H1,5X,53HTOTAL LEAST-SQUARES ANALYSIS.  NO EQUATIONS ABSODK054140          
     1RBED.,5X,13HDF=NO. CARDS=,I5)                                     DK054150          
      GO TO 310                                                         DK054160          
  301 WRITE (6,1061) DF                                                 DK054170          
 1061 FORMAT (1H1,21X,38HEQUATION FOR THE OVERALL MEAN ABSORBED,5X,3HDF=DK054180          
     1,F5.0)                                                            DK054190          
      GO TO 310                                                         DK054200          
  302 WRITE (6,1062) MJN,NCDS,DF                                        DK054210          
 1062 FORMAT (1H1,10X,55HNO. OF CLASSES OR SUBCLASSES ABSORBED BY LEAST DK054220          
     1SQUARES=,I4,5X,10HNO. CARDS=,I5,5X,3HDF=,F5.0)                    DK054230          
      GO TO 310                                                         DK054240          
  303 WRITE (6,1063) MJN,REP,NCDS,DF                                    DK054250          
 1063 FORMAT (1H1,60HNO. OF CLASSES OR SUBCLASSES ABSORBED BY MAXIMUM LIDK054260          
     1KELIHOOD=,I4,3X,7H1-R/R =,F7.4,3X,10HNO. CARDS=,I5,3X,3HDF=,F5.0) DK054270          
      GO TO 310                                                         DK054280          
  304 WRITE (6,1064) MJN,MIN,REP,NCDS,DF                                DK054290          
 1064 FORMAT (1H1,37H NO. OF MAJOR CLASSES ABSORBED BY ML=,I4,3X,21HNO. DK054300          
     1MINOR SUBCLASSES=,I4,3X,7H1-R/R =,F7.4,3X,10HNO. CARDS=,I5,3X,3HDFDK054310          
     3=,F5.0)                                                           DK054320          
  310 IF (NOM+NON.EQ.0) GO TO 320                                       DK054330          
      CALL SCLNOS                                                       DK054340          
  320 IF (NLHM.EQ.0.OR.(NPR.EQ.0.AND.LIOP.GT.9))  GO TO 330             DK054350          
      WRITE (6,1100) IJOB                                               DK054360          
 1100 FORMAT (1H0,60HOVERALL MEANS AND STANDARD DEVIATIONS OF LHM FOR PRDK054370          
     1OBLEM NO.,I3)                                                     DK054380          
      WRITE (6,1101)                                                    DK054390          
 1101 FORMAT (1H0,35H CODED LHM    INDEPENDENT VARIABLES,11X,4HMEAN,9X,4DK054400          
     1HS.D.)                                                            DK054410          
      WRITE (6,1102)                                                    DK054420          
 1102 FORMAT (1H )                                                      DK054430          
      L=1                                                               DK054440          
      K3=1                                                              DK054450          
      DO 329 I=1,NLHM                                                   DK054460          
      IF (NAB.EQ.3) TOT(I)=ARRAY(I)                                     DK054470          
      IF (I.LT.IE.AND.LIOP.GT.9)  GO TO 329                             DK054480          
      AMN=TOT(I)/FLOAT(NCDS)                                            DK054490          
      K=NLHM*(I-1)-I*(I-3)/2                                            DK054500          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 324                   DK054510          
      SD=DSQRT((ARRAY(K)-(TOT(I)*TOT(I)/ARRAY(1)))/(DF-1.))             DK054520          
      GO TO 326                                                         DK054530          
  324 SD=DSQRT(ARRAY(K)/DF)                                             DK054540          
  326 IF (I.LT.IE) GO TO 328                                            DK054550          
      L=L-1                                                             DK054560          
      IF (L.NE.0) GO TO 328                                             DK054570          
      IF (LQC(K3)-2) 390,391,392                                        DK054580          
  390 AMN=AMN+XM(K3)                                                    DK054590          
      K3=K3+1                                                           DK054600          
      L=1                                                               DK054610          
      GO TO 328                                                         DK054620          
  391 K3=K3+1                                                           DK054630          
      L=2                                                               DK054640          
      GO TO 328                                                         DK054650          
  392 K3=K3+1                                                           DK054660          
      L=3                                                               DK054670          
  328 WRITE (6,1103) I,LAB1(I),LAB2(I),LAB3(I),LAB4(I),AMN,SD           DK054680          
  329 CONTINUE                                                          DK054690          
 1103 FORMAT (1H ,I6,6X,A6,2(1X,A6),A6,2F13.5)                          DK054700          
  330 IF (NRHM.EQ.0) GO TO 342                                          DK054710          
      WRITE (6,1104)                                                    DK054720          
 1104 FORMAT (1H0,15X,44HOVERALL MEANS AND STANDARD DEVIATIONS OF RHM)  DK054730          
      WRITE (6,1102)                                                    DK054740          
      DO 340 I=1,NRHM                                                   DK054750          
      K2=NLHM+I                                                         DK054760          
      AMN=TOT(K2)/FLOAT(NCDS)                                           DK054770          
      K=NLHM*(I-1)+1                                                    DK054780          
      IF (NAB.EQ.0.OR.NAB.EQ.3) TOT(K2)=RHM(K)                          DK054790          
      IF (NCPR.EQ.0) GO TO 332                                          DK054800          
      J=NRHM*(I-1)-I*(I-3)/2                                            DK054810          
      GO TO 334                                                         DK054820          
  332 J=I                                                               DK054830          
  334 IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 336                   DK054840          
      SD=DSQRT((SSCPR(J)-(TOT(K2)*TOT(K2)/ARRAY(1)))/(DF-1.))           DK054850          
      GO TO 338                                                         DK054860          
  336 SD=DSQRT(SSCPR(J)/DF)                                             DK054870          
  338 AMN=AMN+YM(I)                                                     DK054880          
  340 WRITE (6,1105) LITY(I),AMN,SD                                     DK054890          
 1105 FORMAT (1H ,12X,A6,4X,5HMEAN=,F12.5,10X,5HS.D.=,F12.5)            DK054900          
  342 IF (LIOP.EQ.7) GO TO 500                                          DK054910          
  344 IF (NLHM.EQ.0) GO TO 359                                          DK054920          
      IF (IE.GT.NLHM.AND.LIOP.GT.9)  GO TO 359                          DK054930          
      WRITE (6,1102)                                                    DK054940          
      WRITE (6,1106) IJOB                                               DK054950          
 1106 FORMAT (1H0,15X,64HSUMS OF SQUARES, C.P. AND CORRELATIONS AMONG LHDK054960          
     1M FOR PROBLEM NO.,I3)                                             DK054970          
      WRITE (6,1107)                                                    DK054980          
 1107 FORMAT (1H0,4H ROW,5H  COL,15X,21HINDEPENDENT VARIABLES)          DK054990          
      WRITE (6,1108)                                                    DK055000          
 1108 FORMAT (1H ,9HCODE CODE,9X,3HROW,22X,6HCOLUMN,23X,28HS.SQS. OR C.PDK055010          
     1.   CORRELATION)                                                  DK055020          
      DO 360 I=1,NLHM                                                   DK055030          
      DO 360 J=I,NLHM                                                   DK055040          
      IF (I.LT.IE.AND.J.LT.IE.AND.LIOP.GT.9)  GO TO 360                 DK055050          
      K=NLHM*(I-1)-I*(I-3)/2                                            DK055060          
      K1=NLHM*(J-1)-J*(J-3)/2                                           DK055070          
      K3=K+J-I                                                          DK055080          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 354                   DK055090          
      SCP=ARRAY(K3)-(TOT(I)*TOT(J))/ARRAY(1)                            DK055100          
      IF (I.GE.IE.AND.I.NE.J) GO TO 350                                 DK055110          
      SSY1=ARRAY(K)-(TOT(I)*TOT(I))/ARRAY(1)                            DK055120          
      GO TO 352                                                         DK055130          
  350 SSY1=ARRAY(K)                                                     DK055140          
  352 SSY2=ARRAY(K1)-(TOT(J)*TOT(J))/ARRAY(1)                           DK055150          
      RLHM=SCP/DSQRT(DIV(SSY1*SSY2))                                    DK055160          
      IF (I.LT.IE.AND.J.LT.IE) GO TO 356                                DK055170          
      ARRAY(K3)=SCP                                                     DK055180          
      GO TO 356                                                         DK055190          
  354 RLHM=ARRAY(K3)/DSQRT(ARRAY(K)*ARRAY(K1))                          DK055200          
  356 IF (I.LT.IE.AND.LIOP.GT.9)  GO TO 360                             DK055210          
      IF (I.NE.J) GO TO 358                                             DK055220          
      RLHM=1.                                                           DK055230          
      WRITE (6,1102)                                                    DK055240          
  358 WRITE (6,1109) I,J,LAB1(I),LAB2(I),LAB3(I),LAB4(I),LAB1(J),LAB2(J)DK055250          
     1,LAB3(J),LAB4(J),ARRAY(K3),RLHM                                   DK055260          
  360 CONTINUE                                                          DK055270          
 1109 FORMAT (1H ,I3,I5,2X,A6,2(1X,A6),2A6,2(1X,A6),A6,F23.8,F13.4)     DK055280          
  359 IF (NRHM.EQ.0) GO TO 386                                          DK055290          
      IF (NLHM.EQ.0) GO TO 376                                          DK055300          
      IF (IE.GT.NLHM.AND.LIOP.GT.9)  GO TO 376                          DK055310          
      WRITE (6,1102)                                                    DK055320          
      WRITE (6,1110) IJOB                                               DK055330          
 1110 FORMAT (1H0,71H SUMS OF CROSSPRODUCTS AND CORRELATIONS OF LHM WITHDK055340          
     1 RHM FOR PROBLEM NO.,I3)                                          DK055350          
      WRITE (6,1111)                                                    DK055360          
 1111 FORMAT (1H0,41HRHM  LHM  RHM NAME   INDEPENDENT VARIABLE,16X,4HC.PDK055370          
     1.,7X,11HCORRELATION)                                              DK055380          
      DO 375 I=1,NRHM                                                   DK055390          
      K4=NLHM+I                                                         DK055400          
      WRITE (6,1102)                                                    DK055410          
      DO 375 J=1,NLHM                                                   DK055420          
      IF (J.LT.IE.AND.LIOP.GT.9)  GO TO 375                             DK055430          
      K=NLHM*(J-1)-J*(J-3)/2                                            DK055440          
      IF (NCPR.EQ.0) GO TO 362                                          DK055450          
      K1=NRHM*(I-1)-I*(I-3)/2                                           DK055460          
      GO TO 364                                                         DK055470          
  362 K1=I                                                              DK055480          
  364 K2=NLHM*(I-1)+J                                                   DK055490          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 372                   DK055500          
      SCP=RHM(K2)-(TOT(J)*TOT(K4))/ARRAY(1)                             DK055510          
      IF (J.GE.IE) GO TO 368                                            DK055520          
      SSY1=ARRAY(K)-(TOT(J)*TOT(J))/ARRAY(1)                            DK055530          
      GO TO 370                                                         DK055540          
  368 SSY1=ARRAY(K)                                                     DK055550          
  370 SSY2=SSCPR(K1)-(TOT(K4)*TOT(K4))/ARRAY(1)                         DK055560          
      RLR=SCP/DSQRT(DIV(SSY1*SSY2))                                     DK055570          
      IF (J.LT.IE) GO TO 374                                            DK055580          
      RHM(K2)=SCP                                                       DK055590          
      GO TO 374                                                         DK055600          
  372 RLR=RHM(K2)/DSQRT(ARRAY(K)*SSCPR(K1))                             DK055610          
  374 WRITE (6,1112) I,J,LITY(I),LAB1(J),LAB2(J),LAB3(J),LAB4(J),RHM(K2)DK055620          
     1,RLR                                                              DK055630          
  375 CONTINUE                                                          DK055640          
 1112 FORMAT (1H ,I3,I5,3X,A6,2X,2(A6,1X),2A6,F23.8,F10.4)              DK055650          
  376 IF (NCPR.EQ.0) GO TO 386                                          DK055660          
      IF (NRN.NE.NRUN.AND.LIOP.EQ.20) GO TO 386                         DK055670          
      WRITE (6,1102)                                                    DK055680          
      WRITE (6,1113) IJOB                                               DK055690          
 1113 FORMAT (1H0,15X,64HSUMS OF SQUARES, C.P. AND CORRELATIONS AMONG RHDK055700          
     1M FOR PROBLEM NO.,I3)                                             DK055710          
      WRITE (6,1114)                                                    DK055720          
 1114 FORMAT (1H0,20X,3HROW,6H   COL,4X,3HRHM,4X,3HRHM,15X,14HS.SQS. OR DK055730          
     1C.P.,4X,11HCORRELATION)                                           DK055740          
      DO 384 I=1,NRHM                                                   DK055750          
      DO 384 J=I,NRHM                                                   DK055760          
      K=NRHM*(I-1)-I*(I-3)/2                                            DK055770          
      K1=NRHM*(J-1)-J*(J-3)/2                                           DK055780          
      K3=K+J-I                                                          DK055790          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 380                   DK055800          
      K4=NLHM+I                                                         DK055810          
      K5=NLHM+J                                                         DK055820          
      SCP=SSCPR(K3)-(TOT(K4)*TOT(K5))/ARRAY(1)                          DK055830          
      SSY1=SSCPR(K)-(TOT(K4)*TOT(K4))/ARRAY(1)                          DK055840          
      SSY2=SSCPR(K1)-(TOT(K5)*TOT(K5))/ARRAY(1)                         DK055850          
      RLHM=SCP/DSQRT(SSY1*SSY2)                                         DK055860          
      GO TO 382                                                         DK055870          
  380 RLHM=SSCPR(K3)/DSQRT(SSCPR(K)*SSCPR(K1))                          DK055880          
  382 IF (I.NE.J) GO TO 384                                             DK055890          
      WRITE (6,1102)                                                    DK055900          
  384 WRITE (6,1115) I,J,LITY(I),LITY(J),SSCPR(K3),RLHM                 DK055910          
 1115 FORMAT (1H ,20X,I3,I6,3X,A6,2X,A6,F25.8,F14.4)                    DK055920          
  386 N=NLHM                                                            DK055930          
  500 CONTINUE                                                          DK055940          
      RETURN                                                            DK055950          
      END                                                               DK055960          
*ENDTEXT                                                                                  
