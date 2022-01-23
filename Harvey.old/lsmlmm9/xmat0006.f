*TEXT                                                                                     
      SUBROUTINE XMAT                                                   DK080010          
C     -------------------------------------------                       DK080020          
C     CODING SUBROUTINE - - SETS UP X ARRAY FOR EACH OBSERVATION        DK080030          
C     ----------------------------------------------------              DK080040          
      DIMENSION ARRAY(5000),SSCPR(630),SSS(630),RHM(2450),TOT(106),TOT2(DK080070          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100),NS2(15)    DK080080          
      DIMENSION FAB(5000),FY(630),TOT4(106),SAB(630),FSQ(525),TOT5(106) DK080090          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),MZ(10),IPL(13),NSDK080100          
     1P(13),NND(13),XP(81),YP(41),      NDC(10),NMI(10),MEN(20),NCL(20),DK080110          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(40),NCLN(40),LNE(40),NDEN(1DK080120          
     300),NLIT(40),NMA(30),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(40),DK080130          
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDK080140          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),IC(480),DK080150          
     6EFF1(50),EFF2(50),NOS(300),X(106),NMAC(40),I309(15),NR1(15),NW(15)DK080160          
     7,R1I(100),R2I(100)                                                DK080170          
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADK080180          
     1B4,LITY,TRED,YM,XP,YP,IM,MS,NEG,IPL,NSP,NND,FAB,FY,TOT4,SAB,FSQ,TODK080190          
     2T5,MZ,R1I,R2I                                                     DK080200          
      COMMON /CMBLK2/LIT,LITR,NLIT,XM,EFF1,EFF2,X,NDC,NMI,MEN,NCL,LME,IBDK080210          
     1EG,IDEN,NCLN,NEN,LNE,NBEG,NDEN,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQDK080220          
     2C,NREGP,LGTX,JBEG,NDECX,NEGY,LNY,LHY,KBEG,NDECY,IC,NOS,NMAC       DK080230          
      COMMON /CMBLK3/RR,REP,DF,SDF,EDF,WK,IJOB,NAB,NLHM,NRHM,NMEA,NME,NNDK080240          
     1EA,NNE,N2F,NPR,IRAN,MPOP,LIOP,IAD,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,DK080250          
     2NOM,NON,ML,MLB,NS,NOT,KPUT,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,IN,I30DK080260          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DK080270          
     1L,L7,KA,KB,KC,SOF,NX,KD,MTY,MAN,NRUN,NRN,EDFF,NAME,EDFS,NAM5,IFP,NDK080280          
     2DFA,IFPC,NDFC,NDFAC,NINT                                          DK080290          
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
      IF (I.EQ.1.AND.KB.EQ.1.AND.MTY.GT.5) R1I(MLOC)=R1I(MLOC)+1.0      DK080590          
      ML3=ML3-ML2                                                       DK080600          
      MLOC=N+ML3-1                                                      DK080610          
      IF (ML3.EQ.NCL(I)) GO TO 6                                        DK080620          
      X(MLOC)=1.                                                        DK080630          
      GO TO 10                                                          DK080640          
    6 K2=NEND-1                                                         DK080650          
      DO 8 J=N,K2                                                       DK080660          
    8 X(J)=-1.                                                          DK080670          
   10 N=NEND                                                            DK080680          
C     ----------------------------------------------------              DK080690          
C     PLACES CODES IN X FOR NESTED MAIN EFFECTS                         DK080700          
C     ----------------------------------------------------              DK080710          
   11 J1=K                                                              DK080720          
      K=0                                                               DK080730          
      IF (NNE.EQ.0) GO TO 19                                            DK080740          
      K1=NNEA+1                                                         DK080750          
      DO 18 I=K1,NON                                                    DK080760          
      K=NCLN(I)+K                                                       DK080770          
      L7=NBEG(I)                                                        DK080780          
      L=LNE(I)+L7-1                                                     DK080790          
      J=NCDS                                                            DK080800          
      CALL FIELD (ICOD,L,IC,L7,J)                                       DK080810          
      IF (J.EQ.1) GO TO 902                                             DK080820          
      ML1=MLB+K                                                         DK080830          
      ML2=ML1-NCLN(I)                                                   DK080840          
      ML3=ML2                                                           DK080850          
      NEND=N+NCLN(I)-1                                                  DK080860          
   12 ML3=ML3+1                                                         DK080870          
      IF (ML3.GT.ML1) GO TO 18                                          DK080880          
      IF (ICOD.NE.NDEN(ML3)) GO TO 12                                   DK080890          
      MLOC=J1+ML3-MLB                                                   DK080900          
      NOS(MLOC)=NOS(MLOC)+1                                             DK080910          
      ML3=ML3-ML2                                                       DK080920          
      MLOC=N+ML3-1                                                      DK080930          
      IF (ML3.EQ.NCLN(I)) GO TO 14                                      DK080940          
      X(MLOC)=1.                                                        DK080950          
      GO TO 18                                                          DK080960          
   14 K2=NEND-1                                                         DK080970          
      DO 16 J=N,K2                                                      DK080980          
   16 X(J)=-1.                                                          DK080990          
   18 N=NEND                                                            DK081000          
C     ----------------------------------------------------              DK081010          
C     PLACES CODES IN X FOR TWO-FACTOR INTERACTIONS                     DK081020          
C     ----------------------------------------------------              DK081030          
   19 J1=J1+K                                                           DK081040          
      K5=0                                                              DK081050          
      NSUB=0                                                            DK081060          
      IF (N2F.EQ.0) GO TO 51                                            DK081070          
      DO 50 I=1,N2F                                                     DK081080          
      INTV=INT1(I)                                                      DK081090          
      ISW4=0                                                            DK081100          
      CALL CODE                                                         DK081110          
      IF (L.EQ.1) GO TO 902                                             DK081120          
      IF (ISW2.NE.1) GO TO 20                                           DK081130          
      ISW4=1                                                            DK081140          
   20 IF (ISW3.EQ.1) GO TO 904                                          DK081150          
      ICON=ML3*100                                                      DK081160          
      ME3=ML3                                                           DK081170          
      K3=K1                                                             DK081180          
      INTV=INT2(I)                                                      DK081190          
      CALL CODE                                                         DK081200          
      IF (L.EQ.1) GO TO 902                                             DK081210          
      IF (ISW3.EQ.1) GO TO 904                                          DK081220          
      ICON=ICON+ML3                                                     DK081230          
      ME2=ML3                                                           DK081240          
      K4=K1                                                             DK081250          
      K6=K5+NMC(I)                                                      DK081260          
      IF (ISW2.EQ.1.OR.ISW4.EQ.1) GO TO 48                              DK081270          
      MLOC=J1+NSUB+(ME3-1)*K1+ME2                                       DK081280          
      NOS(MLOC)=NOS(MLOC)+1                                             DK081290          
      K7=K6-NMC(I)                                                      DK081300          
   22 K7=K7+1                                                           DK081310          
      IF (K7.GT.K6) GO TO 24                                            DK081320          
      IF (ICON-MSCL(K7)) 22,48,22                                       DK081330          
   24 K9=K3-1                                                           DK081340          
      DO 26 J=1,K9                                                      DK081350          
   26 EFF1(J)=0.0                                                       DK081360          
      K9=K4-1                                                           DK081370          
      DO 28 J=1,K9                                                      DK081380          
   28 EFF2(J)=0.0                                                       DK081390          
      IF (ME3.EQ.K3) GO TO 30                                           DK081400          
      EFF1(ME3)=1.                                                      DK081410          
      GO TO 34                                                          DK081420          
   30 K9=K3-1                                                           DK081430          
      DO 32 J=1,K9                                                      DK081440          
   32 EFF1(J)=-1.                                                       DK081450          
   34 IF (ME2.EQ.K4) GO TO 36                                           DK081460          
      EFF2(ME2)=1.                                                      DK081470          
      GO TO 40                                                          DK081480          
   36 K9=K4-1                                                           DK081490          
      DO 38 J=1,K9                                                      DK081500          
   38 EFF2(J)=-1.                                                       DK081510          
   40 L1=0                                                              DK081520          
      K9=K3-1                                                           DK081530          
      K2=K4-1                                                           DK081540          
      DO 46 J=1,K9                                                      DK081550          
      DO 46 J2=1,K2                                                     DK081560          
      ICO=J*100+J2                                                      DK081570          
      IF (NMC(I).EQ.0) GO TO 44                                         DK081580          
      K7=K6-NMC(I)                                                      DK081590          
   42 K7=K7+1                                                           DK081600          
      IF (K7.GT.K6) GO TO 44                                            DK081610          
      IF (ICO-MSCL(K7)) 42,46,42                                        DK081620          
   44 L1=L1+1                                                           DK081630          
      MLOC=N+L1-1                                                       DK081640          
      X(MLOC)=EFF1(J)*EFF2(J2)                                          DK081650          
   46 CONTINUE                                                          DK081660          
   48 K5=K6                                                             DK081670          
      NSUB=NSUB+K3*K4                                                   DK081680          
   50 N=N+(K3-1)*(K4-1)-NMC(I)                                          DK081690          
C     ----------------------------------------------------              DK081700          
C     PLACES CODES (X-XM), ETC, IN X ARRAY FOR POOLED REGRESSIONS       DK081710          
C     ----------------------------------------------------              DK081720          
   51 IF (NPR.EQ.0) GO TO 81                                            DK081730          
   52 DO 80 I=1,NPR                                                     DK081740          
      IF (NEGX(I)-1) 58,54,56                                           DK081750          
   54 CALL NEGF (TX,JBEG,LGTX,IC,I)                                     DK081760          
      GO TO 58                                                          DK081770          
   56 CALL NEGZ  (TX,JBEG,LGTX,IC,I)                                    DK081780          
   58 L7=JBEG(I)                                                        DK081790          
      L=LGTX(I)+L7-1                                                    DK081800          
      J=NCDS                                                            DK081810          
      CALL FIELD (ICOD,L,IC,L7,J)                                       DK081820          
      IF (J.EQ.1) GO TO 903                                             DK081830          
      XR=ICOD                                                           DK081840          
      IF (NEGX(I).EQ.0) GO TO 60                                        DK081850          
      XR=XR*TX                                                          DK081860          
   60 XR=XR/10.**NDECX(I)                                               DK081870          
      IF (LOGE(I).EQ.0) GO TO 62                                        DK081880          
      XR=ALOG10(XR)-XM(I)                                                                 
      GO TO 68                                                          DK081900          
   62 XR=XR-XM(I)                                                       DK081910          
   68 YR=1.                                                             DK081920          
      K9=LQC(I)                                                         DK081930          
      DO 80 J=1,K9                                                      DK081940          
      X(N)=YR*XR                                                        DK081950          
      YR=X(N)                                                           DK081960          
   80 N=N+1                                                             DK081970          
C     ----------------------------------------------------              DK081980          
C     PLACES (Y-YM) IN X ARRAY FOR RHM                                  DK081990          
C     ----------------------------------------------------              DK082000          
   81 IF (NRHM.EQ.0)  GO TO 96                                          DK082010          
      DO 95 I=1,NRHM                                                    DK082020          
      IF (NEGY(I)-1) 86,82,84                                           DK082030          
   82 CALL NEGF (TX,KBEG,LHY,IC,I)                                      DK082040          
      GO TO 86                                                          DK082050          
   84 CALL NEGZ  (TX,KBEG,LHY,IC,I)                                     DK082060          
   86 L7=KBEG(I)                                                        DK082070          
      L=LHY(I)+L7-1                                                     DK082080          
      J=NCDS                                                            DK082090          
      CALL FIELD (ICOD,L,IC,L7,J)                                       DK082100          
      IF (J.EQ.1) GO TO 903                                             DK082110          
      YR=ICOD                                                           DK082120          
      IF (NEGY(I).EQ.0) GO TO 88                                        DK082130          
      YR=YR*TX                                                          DK082140          
   88 YR=YR/(10.**NDECY(I))                                             DK082150          
      IF (LNY(I).EQ.0) GO TO 90                                         DK082160          
      YR=ALOG10(YR)-YM(I)                                                                 
      GO TO 92                                                          DK082180          
   90 YR=YR-YM(I)                                                       DK082190          
   92 X(N)=YR                                                           DK082200          
      N=N+1                                                             DK082210          
   95 CONTINUE                                                          DK082220          
   96 IF (N-1.NE.NOT) GO TO 900                                         DK082230          
      GO TO 100                                                         DK082240          
  900 WRITE (6,1000)                                                    DK082250          
 1000 FORMAT (1H0,40HNO. LHM PLUS RHM DO NOT AGREE WITH PAR 1)          DK082260          
      GO TO 904                                                         DK082270          
  901 WRITE (6,1001) NCDS,LSCJM,ICOD,IJOB                               DK082280          
 1001 FORMAT ("0CLASS CODE MISSING ON CARD NO.",I8,5X,"EFFECT",I3," CANNDK082290          
     8OT FIND CODE ",I5,". CHECK PARAMETER CARDS FOR PROBLEM NO.",I3)   DK082300          
      GO TO 904                                                         DK082310          
  902 WRITE (6,1002) NCDS                                               DK082320          
 1002 FORMAT (1H0,69HUNITS POSITION OF AN ID FIELD OR A CONTROL FIELD ISDK082330          
     4 BLANK ON CARD NO.,I5)                                            DK082340          
      GO TO 904                                                         DK082350          
  903 WRITE (6,1003) NCDS                                               DK082360          
 1003 FORMAT (1H0,55HUNITS POSITION OF AN X OR Y FIELD IS BLANK ON CARD DK082370          
     1 NO.,I5)                                                          DK082380          
  904 MULL=1                                                            DK082390          
  100 CONTINUE                                                          DK082400          
      RETURN                                                            DK082410          
      END                                                               DK082420          
*ENDTEXT                                                                                  
