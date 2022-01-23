      SUBROUTINE XMAT                                                   DK080010          
C     -------------------------------------------                       DK080020          
C     CODING SUBROUTINE - - SETS UP X ARRAY FOR EACH OBSERVATION        DK080030          
C     ----------------------------------------------------              DK080040          
      EXTERNAL DLOG10,DSQRT                                                               
      DIMENSION ARRAY(5000),SSCPR(630),SSS(630),RHM(2450),TOT(106),TOT2(DK080070          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100),NS2(15)    DK080080          
      DIMENSION FAB(5000),FY(630),TOT4(106),SAB(630),FSQ(525),TOT5(106) DK080090          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),MZ(10),IPL(13),NSDK080100          
     1P(13),NND(13),XP(81),YP(41),      NDC(10),NMI(10),MEN(20),NCL(20),DK080110          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(50),NCLN(50),LNE(50),NDEN(1                  
     300),NLIT(50),NMA(50),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(50),                  
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDK080140          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),                          
     6EFF1(50),EFF2(50),NOS(300),X(106),NMAC(50),I309(15),NR1(15),NW(15)                  
     7,R1I(100),R2I(100),WW(200),ICLR(90),IRM(40),LAD(90)               DK080170          
      DIMENSION IC(1000)                                                                  
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADK080180          
     1B4,LITY,TRED,YM,XP,YP,IM,MS,NEG,IPL,NSP,NND,FAB,FY,TOT4,SAB,FSQ,TODK080190          
     2T5,MZ,R1I,R2I,WW,ICLR,IRM,LAD                                     DK080200          
      COMMON /CMBLK2/LIT,LITR,NLIT,XM,EFF1,EFF2,X,NDC,NMI,MEN,NCL,LME,IBDK080210          
     1EG,IDEN,NCLN,NEN,LNE,NBEG,NDEN,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQDK080220          
     2C,NREGP,LGTX,JBEG,NDECX,NEGY,LNY,LHY,KBEG,NDECY,IC,NOS,NMAC       DK080230          
      COMMON /CMBLK3/RR,REP,DF,SDF,EDF,WK,IJOB,NAB,NLHM,NRHM,NMEA,NME,NNDK080240          
     1EA,NNE,N2F,NPR,IRAN,MPOP,LIOP,IAD,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,DK080250          
     2NOM,NON,ML,MLB,NS,NOT,KPUT,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,IN,I30DK080260          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DK080270          
     1L,L7,KA,KB,KC,SOF,NX,KD,MTY,MAN,NRUN,NRN,EDFF,NAME,EDFS,NAM5,IFP,NDK080280          
     2DFA,IFPC,NDFC,NDFAC,NINT,IBET,LBEG,LTHN,NNDC,WTT,SWT1             DK080290          
      DO 1 I=1,NOT                                                      DK080300          
    1 X(I)=0.                                                           DK080310          
      N=1                                                               DK080320          
      K=0                                                               DK080330          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 2                     DK080340          
      X(1)=1                                                            DK080350          
      N=N+1                                                             DK080360          
C     ----------------------------------------------------              DK080370          
C     PLACES CODES IN X FOR MAIN EFFECTS                                DK080380          
C     ----------------------------------------------------              DK080390          
    2 IF (NME.EQ.0) GO TO 11                                            DK080400          
      K1=NMEA+1                                                         DK080410          
      DO 10 I=K1,NOM                                                    DK080420          
      K=NCL(I)+K                                                        DK080430          
      L7=IBEG(I)                                                        DK080440          
      L=LME(I)+L7-1                                                     DK080450          
      J=NCDS                                                            DK080460          
      CALL FIELD (ICOD,L,IC,L7,J)                                       DK080470          
      IF (J.EQ.1) GO TO 902                                             DK080480          
      ML1=ML+K                                                          DK080490          
      ML2=ML1-NCL(I)                                                    DK080500          
      ML3=ML2                                                           DK080510          
      NEND=N+NCL(I)-1                                                   DK080520          
    4 ML3=ML3+1                                                         DK080530          
      LSCJM=I                                                           DK080540          
      IF (ML3.GT.ML1) GO TO 901                                         DK080550          
      IF (ICOD.NE.IDEN(ML3)) GO TO 4                                    DK080560          
      MLOC=ML3-ML                                                       DK080570          
      NOS(MLOC)=NOS(MLOC)+1                                             DK080580          
      WW(MLOC)=WW(MLOC)+WTT                                             DK080590          
      IF (I.EQ.1.AND.KB.EQ.1.AND.MTY.EQ.6) R1I(MLOC)=R1I(MLOC)+1.0      DK080600          
      IF (I.EQ.2.AND.KB.EQ.1.AND.MTY.EQ.7) R1I(MLOC)=R1I(MLOC)+1.0      DK080610          
      ML3=ML3-ML2                                                       DK080620          
      MLOC=N+ML3-1                                                      DK080630          
      IF (ML3.EQ.NCL(I)) GO TO 6                                        DK080640          
      X(MLOC)=1.                                                        DK080650          
      GO TO 10                                                          DK080660          
    6 K2=NEND-1                                                         DK080670          
      DO 8 J=N,K2                                                       DK080680          
    8 X(J)=-1.                                                          DK080690          
   10 N=NEND                                                            DK080700          
C     ----------------------------------------------------              DK080710          
C     PLACES CODES IN X FOR NESTED MAIN EFFECTS                         DK080720          
C     ----------------------------------------------------              DK080730          
   11 J1=K                                                              DK080740          
      K=0                                                               DK080750          
      IF (NNE.EQ.0) GO TO 19                                            DK080760          
      K1=NNEA+1                                                         DK080770          
      DO 18 I=K1,NON                                                    DK080780          
      K=NCLN(I)+K                                                       DK080790          
      L7=NBEG(I)                                                        DK080800          
      L=LNE(I)+L7-1                                                     DK080810          
      J=NCDS                                                            DK080820          
      CALL FIELD (ICOD,L,IC,L7,J)                                       DK080830          
      IF (J.EQ.1) GO TO 902                                             DK080840          
      ML1=MLB+K                                                         DK080850          
      ML2=ML1-NCLN(I)                                                   DK080860          
      ML3=ML2                                                           DK080870          
      NEND=N+NCLN(I)-1                                                  DK080880          
   12 ML3=ML3+1                                                         DK080890          
      IF (ML3.GT.ML1) GO TO 18                                          DK080900          
      IF (ICOD.NE.NDEN(ML3)) GO TO 12                                   DK080910          
      MLOC=J1+ML3-MLB                                                   DK080920          
      NOS(MLOC)=NOS(MLOC)+1                                             DK080930          
      WW(MLOC)=WW(MLOC)+WTT                                             DK080940          
      ML3=ML3-ML2                                                       DK080950          
      MLOC=N+ML3-1                                                      DK080960          
      IF (ML3.EQ.NCLN(I)) GO TO 14                                      DK080970          
      X(MLOC)=1.                                                        DK080980          
      GO TO 18                                                          DK080990          
   14 K2=NEND-1                                                         DK081000          
      DO 16 J=N,K2                                                      DK081010          
   16 X(J)=-1.                                                          DK081020          
   18 N=NEND                                                            DK081030          
C     ----------------------------------------------------              DK081040          
C     PLACES CODES IN X FOR TWO-FACTOR INTERACTIONS                     DK081050          
C     ----------------------------------------------------              DK081060          
   19 J1=J1+K                                                           DK081070          
      K5=0                                                              DK081080          
      NSUB=0                                                            DK081090          
      IF (N2F.EQ.0) GO TO 51                                            DK081100          
      DO 50 I=1,N2F                                                     DK081110          
      INTV=INT1(I)                                                      DK081120          
      ISW4=0                                                            DK081130          
      CALL CODE                                                         DK081140          
      IF (L.EQ.1) GO TO 902                                             DK081150          
      IF (ISW2.NE.1) GO TO 20                                           DK081160          
      ISW4=1                                                            DK081170          
   20 IF (ISW3.EQ.1) GO TO 904                                          DK081180          
      ICON=ML3*100                                                      DK081190          
      ME3=ML3                                                           DK081200          
      K3=K1                                                             DK081210          
      INTV=INT2(I)                                                      DK081220          
      CALL CODE                                                         DK081230          
      IF (L.EQ.1) GO TO 902                                             DK081240          
      IF (ISW3.EQ.1) GO TO 904                                          DK081250          
      ICON=ICON+ML3                                                     DK081260          
      ME2=ML3                                                           DK081270          
      K4=K1                                                             DK081280          
      K6=K5+NMC(I)                                                      DK081290          
      IF (ISW2.EQ.1.OR.ISW4.EQ.1) GO TO 48                              DK081300          
      MLOC=J1+NSUB+(ME3-1)*K1+ME2                                       DK081310          
      NOS(MLOC)=NOS(MLOC)+1                                             DK081320          
      WW(MLOC)=WW(MLOC)+WTT                                             DK081330          
      K7=K6-NMC(I)                                                      DK081340          
   22 K7=K7+1                                                           DK081350          
      IF (K7.GT.K6) GO TO 24                                            DK081360          
      IF (ICON-MSCL(K7)) 22,48,22                                       DK081370          
   24 K9=K3-1                                                           DK081380          
      DO 26 J=1,K9                                                      DK081390          
   26 EFF1(J)=0.0                                                       DK081400          
      K9=K4-1                                                           DK081410          
      DO 28 J=1,K9                                                      DK081420          
   28 EFF2(J)=0.0                                                       DK081430          
      IF (ME3.EQ.K3) GO TO 30                                           DK081440          
      EFF1(ME3)=1.                                                      DK081450          
      GO TO 34                                                          DK081460          
   30 K9=K3-1                                                           DK081470          
      DO 32 J=1,K9                                                      DK081480          
   32 EFF1(J)=-1.                                                       DK081490          
   34 IF (ME2.EQ.K4) GO TO 36                                           DK081500          
      EFF2(ME2)=1.                                                      DK081510          
      GO TO 40                                                          DK081520          
   36 K9=K4-1                                                           DK081530          
      DO 38 J=1,K9                                                      DK081540          
   38 EFF2(J)=-1.                                                       DK081550          
   40 L1=0                                                              DK081560          
      K9=K3-1                                                           DK081570          
      K2=K4-1                                                           DK081580          
      DO 46 J=1,K9                                                      DK081590          
      DO 46 J2=1,K2                                                     DK081600          
      ICO=J*100+J2                                                      DK081610          
      IF (NMC(I).EQ.0) GO TO 44                                         DK081620          
      K7=K6-NMC(I)                                                      DK081630          
   42 K7=K7+1                                                           DK081640          
      IF (K7.GT.K6) GO TO 44                                            DK081650          
      IF (ICO-MSCL(K7)) 42,46,42                                        DK081660          
   44 L1=L1+1                                                           DK081670          
      MLOC=N+L1-1                                                       DK081680          
      X(MLOC)=EFF1(J)*EFF2(J2)                                          DK081690          
   46 CONTINUE                                                          DK081700          
   48 K5=K6                                                             DK081710          
      NSUB=NSUB+K3*K4                                                   DK081720          
   50 N=N+(K3-1)*(K4-1)-NMC(I)                                          DK081730          
C     ----------------------------------------------------              DK081740          
C     PLACES CODES (X-XM), ETC, IN X ARRAY FOR REGRESSIONS              DK081750          
C     ----------------------------------------------------              DK081760          
   51 IF (NPR.EQ.0) GO TO 81                                            DK081770          
      K2=1                                                              DK081780          
   52 DO 80 I=1,NPR                                                     DK081790          
      IF (NEGX(I)-1) 58,54,56                                           DK081800          
   54 CALL NEGF (TX,JBEG,LGTX,IC,I)                                     DK081810          
      GO TO 58                                                          DK081820          
   56 CALL NEGZ  (TX,JBEG,LGTX,IC,I)                                    DK081830          
   58 L7=JBEG(I)                                                        DK081840          
      L=LGTX(I)+L7-1                                                    DK081850          
      J=NCDS                                                            DK081860          
      CALL FIELD (ICOD,L,IC,L7,J)                                       DK081870          
      IF (J.EQ.1) GO TO 903                                             DK081880          
      XR=ICOD                                                           DK081890          
      IF (NEGX(I).EQ.0) GO TO 60                                        DK081900          
      XR=XR*TX                                                          DK081910          
   60 XR=XR/(10.**NDECX(I))                                             DK081920          
      IF(XR.EQ.0.0) GO TO 62                                            DK081920          
      IF (LOGE(I).EQ.1) XR=DLOG10(XR)                                   DK081930          
      IF (LOGE(I).EQ.2) XR=DSQRT(XR)                                    DK081940          
   62 XR=XR-XM(I)                                                       DK081950          
   68 YR=1.                                                             DK081960          
      K5=K2                                                             DK081970          
      K3=LQC(I)                                                         DK081980          
      DO 107 J=1,K3                                                     DK081990          
      X(N)=YR*XR                                                        DK082000          
      YR=X(N)                                                           DK082010          
      N=N+1                                                             DK082020          
      IF (ICLR(I).EQ.0) GO TO 107                                       DK082030          
      K6=ICLR(I)                                                        DK082040          
      DO 101 K=1,K6                                                     DK082050          
      INTV=IRM(K2)                                                      DK082060          
      CALL CODE                                                         DK082070          
      K9=K1-1                                                           DK082080          
      DO 102 K7=1,K9                                                    DK082090          
  102 EFF1(K7)=0.0                                                      DK082100          
      IF (ML3.EQ.K1) GO TO 103                                          DK082110          
      EFF1(ML3)=1.                                                      DK082120          
      GO TO 104                                                         DK082130          
  103 DO 105 K7=1,K9                                                    DK082140          
  105 EFF1(K7)=-1.                                                      DK082150          
  104 DO 106 K7=1,K9                                                    DK082160          
      X(N)=EFF1(K7)*YR                                                  DK082170          
  106 N=N+1                                                             DK082180          
  101 K2=K2+1                                                           DK082190          
      K2=K5                                                             DK082200          
  107 CONTINUE                                                          DK082210          
      K2=K5+ICLR(I)                                                     DK082220          
   80 CONTINUE                                                          DK082230          
C     ----------------------------------------------------              DK082240          
C     PLACES (Y-YM) IN X ARRAY FOR RHM                                  DK082250          
C     ----------------------------------------------------              DK082260          
   81 IF (NRHM.EQ.0)  GO TO 96                                          DK082270          
      DO 95 I=1,NRHM                                                    DK082280          
      IF (NEGY(I)-1) 86,82,84                                           DK082290          
   82 CALL NEGF (TX,KBEG,LHY,IC,I)                                      DK082300          
      GO TO 86                                                          DK082310          
   84 CALL NEGZ  (TX,KBEG,LHY,IC,I)                                     DK082320          
   86 L7=KBEG(I)                                                        DK082330          
      L=LHY(I)+L7-1                                                     DK082340          
      J=NCDS                                                            DK082350          
      CALL FIELD (ICOD,L,IC,L7,J)                                       DK082360          
      IF (J.EQ.1) GO TO 903                                             DK082370          
      YR=ICOD                                                           DK082380          
      IF (NEGY(I).EQ.0) GO TO 88                                        DK082390          
      YR=YR*TX                                                          DK082400          
   88 YR=YR/(10.**NDECY(I))                                             DK082410          
      IF(YR.EQ.0.0) GO TO 90                                            DK082411          
      IF (LNY(I).EQ.1) YR=DLOG10(YR)                                    DK082420          
      IF (LNY(I).EQ.2) YR=DSQRT(YR)                                     DK082430          
   90 YR=YR-YM(I)                                                       DK082440          
   92 X(N)=YR                                                           DK082450          
      N=N+1                                                             DK082460          
   95 CONTINUE                                                          DK082470          
   96 IF (IBET.EQ.0) GO TO 97                                           DK082480          
      L=0                                                               DK082490          
      I=N-1                                                             DK082500          
      DO 98 J=1,I                                                       DK082510          
      IF (J.GT.NLHM) L=L+1                                              DK082520          
      IF (J.LE.NLHM.OR.LNY(L).EQ.3) GO TO 99                            DK082530          
      X(J)=X(J)-(WTT-1.)*YM(L)                                          DK082540          
      GO TO 98                                                          DK082550          
   99 X(J)=X(J)*WTT                                                     DK082560          
   98 CONTINUE                                                          DK082570          
   97 IF (N-1.NE.NOT) GO TO 900                                         DK082580          
      GO TO 100                                                         DK082590          
  900 J=N-1                                                             DK082600          
      WRITE (6,1000) J                                                  DK082610          
 1000 FORMAT(23H0NO. LHM PLUS NO. RHM (,I3,                                               
     1 42H) DOES NOT AGREE WITH PARAMETER CARD NO. 1)                                     
      IF (N-1.GT.NOT.OR.MTY.GT.1) GO TO 904                             DK082640          
      NOT=N-1                                                           DK082650          
      NLHM=N-1-NRHM                                                     DK082660          
      MATX=NLHM*(NLHM+1)/2                                              DK082670          
      GO TO 100                                                         DK082680          
  901 WRITE (6,1001) NCDS,LSCJM,ICOD,IJOB                               DK082690          
 1001 FORMAT(31H0CLASS CODE MISSING ON CARD NO.,I8,5X,6HEFFECT,                           
     1,I3,18H CANNOT FIND CODE ,I5,39H. CHECK PARAMETER CARDS FOR PROBLE                  
     2M NO.,I3)                                                                           
      GO TO 904                                                         DK082720          
  902 WRITE (6,1002) NCDS                                               DK082730          
 1002 FORMAT (1H0,69HUNITS POSITION OF AN ID FIELD OR A CONTROL FIELD ISDK082740          
     4 BLANK ON CARD NO.,I6)                                            DK082750          
      GO TO 904                                                         DK082760          
  903 WRITE (6,1003) NCDS                                               DK082770          
 1003 FORMAT (1H0,55HUNITS POSITION OF AN X OR Y FIELD IS BLANK ON CARD DK082780          
     1 NO.,I6)                                                          DK082790          
  904 MULL=1                                                            DK082800          
      CALL WCARD(IN,K,L,IC,IDUM,IFLAG)                                                    
  100 CONTINUE                                                          DK082810          
      RETURN                                                            DK082820          
      END                                                               DK082830          
