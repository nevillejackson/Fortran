*TEXT                                                                                     
      SUBROUTINE CODEX                                                                    
C     CODING SUBROUTINE - - SETS UP X ARRAY FOR EACH OBSERVATION        DEC05000          
C     ----------------------------------------------------              DEC05001          
      DIMENSION ARRAY(2000),SSCPR(630),SSS(630),RHM(0250),TOT(106),TOT2(DEC05003          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100)            DEC05004          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),NEQ(6),IPL(13),NSDEC05005          
     1P(13),NND(13),XP(81),YP(41),      NDC(10),NMI(10),MEN(20),NCL(20),DEC05006          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(40),NCLN(40),LNE(40),NDEN(1DEC05007          
     300),NLIT(40),NMA(30),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(40),DEC05008          
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDEC05009          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),IC(480),DEC05010          
     6EFF1(50),EFF2(50),NOS(200),X(106),NMAC(40)                        DEC05011          
      DIMENSION LSKF(20),LBEG(20),IREJ(20)                              M0205011          
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADEC05012          
     1B4,LITY,TRED,YM,IM,MS,NEQ,IPL,NSP,NND,XP,YP                       DEC05013          
      COMMON /CMBLK2/NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NCLN,NEN,LNE,NBEGDEC05014          
     1,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQC,NREGP,LGTX,JBEG,NDDEC05015          
     2ECX,LITR,NEGY,LNY,LHY,KBEG,NDECY,XM,IC,EFF1,EFF2,NOS,X,NMAC       DEC05016          
      COMMON /CMBLK3/IJOB,NAB,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F,NPR,IRAN,MDEC05017          
     1POP,LIOP,IAD,REP,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,NOM,NON,ML,MLB,NSDEC05018          
     2,NOT,DF,KPUT,SDF,EDF,WK,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,RR       DEC05019          
     3,IN,NSKF,LSKF,LBEG,IREJ                                           M0205019          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DEC05020          
     1L,L7                                                              DEC05021          
      DO 1 I=1,NOT                                                      DEC05022          
    1 X(I)=0.                                                           DEC05023          
      N=1                                                               DEC05024          
      K=0                                                               DEC05025          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 2                     DEC05026          
      X(1)=1                                                            DEC05027          
      N=N+1                                                             DEC05028          
C     ----------------------------------------------------              DEC05029          
C     PLACES CODES IN X FOR MAIN EFFECTS                                DEC05030          
C     ----------------------------------------------------              DEC05031          
    2 IF (NME.EQ.0) GO TO 11                                            DEC05032          
      K1=NMEA+1                                                         DEC05033          
      DO 10 I=K1,NOM                                                    DEC05034          
      K=NCL(I)+K                                                        DEC05035          
      L7=IBEG(I)                                                        DEC05036          
      L=LME(I)+L7-1                                                     DEC05037          
      J=NCDS                                                            01C05037          
      CALL FIELD (ICOD,L,IC,L7,J)                                       DEC05038          
      IF (J.EQ.1) GO TO 902                                             DEC05039          
      ML1=ML+K                                                          DEC05040          
      ML2=ML1-NCL(I)                                                    DEC05041          
      ML3=ML2                                                           DEC05042          
      NEND=N+NCL(I)-1                                                   DEC05043          
    4 ML3=ML3+1                                                         DEC05044          
      IF (ML3.GT.ML1) GO TO 901                                         DEC05045          
      IF (ICOD.NE.IDEN(ML3)) GO TO 4                                    DEC05046          
      MLOC=ML3-ML                                                       DEC05047          
      NOS(MLOC)=NOS(MLOC)+1                                             DEC05048          
      ML3=ML3-ML2                                                       DEC05049          
      MLOC=N+ML3-1                                                      DEC05050          
      IF (ML3.EQ.NCL(I)) GO TO 6                                        DEC05051          
      X(MLOC)=1.                                                        DEC05052          
      GO TO 10                                                          DEC05053          
    6 K2=NEND-1                                                         DEC05054          
      DO 8 J=N,K2                                                       DEC05055          
    8 X(J)=-1.                                                          DEC05056          
   10 N=NEND                                                            DEC05057          
C     ----------------------------------------------------              DEC05058          
C     PLACES CODES IN X FOR NESTED MAIN EFFECTS                         DEC05059          
C     ----------------------------------------------------              DEC05060          
   11 J1=K                                                              DEC05061          
      K=0                                                               DEC05062          
      IF (NNE.EQ.0) GO TO 19                                            DEC05063          
      K1=NNEA+1                                                         DEC05064          
      DO 18 I=K1,NON                                                    DEC05065          
      K=NCLN(I)+K                                                       DEC05066          
      L7=NBEG(I)                                                        DEC05067          
      L=LNE(I)+L7-1                                                     DEC05068          
      J=NCDS                                                            01C05068          
      CALL FIELD (ICOD,L,IC,L7,J)                                       DEC05069          
      IF (J.EQ.1) GO TO 902                                             DEC05070          
      ML1=MLB+K                                                         DEC05071          
      ML2=ML1-NCLN(I)                                                   DEC05072          
      ML3=ML2                                                           DEC05073          
      NEND=N+NCLN(I)-1                                                  DEC05074          
   12 ML3=ML3+1                                                         DEC05075          
      IF (ML3.GT.ML1) GO TO 18                                          DEC05076          
      IF (ICOD.NE.NDEN(ML3)) GO TO 12                                   DEC05077          
      MLOC=J1+ML3-MLB                                                   DEC05078          
      NOS(MLOC)=NOS(MLOC)+1                                             DEC05079          
      ML3=ML3-ML2                                                       DEC05080          
      MLOC=N+ML3-1                                                      DEC05081          
      IF (ML3.EQ.NCLN(I)) GO TO 14                                      DEC05082          
      X(MLOC)=1.                                                        DEC05083          
      GO TO 18                                                          DEC05084          
   14 K2=NEND-1                                                         DEC05085          
      DO 16 J=N,K2                                                      DEC05086          
   16 X(J)=-1.                                                          DEC05087          
   18 N=NEND                                                            DEC05088          
C     ----------------------------------------------------              DEC05089          
C     PLACES CODES IN X FOR TWO-FACTOR INTERACTIONS                     DEC05090          
C     ----------------------------------------------------              DEC05091          
   19 J1=J1+K                                                           DEC05092          
      K5=0                                                              DEC05093          
      NSUB=0                                                            DEC05094          
      IF (N2F.EQ.0) GO TO 51                                            DEC05095          
      DO 50 I=1,N2F                                                     DEC05096          
      INTV=INT1(I)                                                      DEC05097          
      ISW4=0                                                            DEC05098          
      CALL CODE                                                         DEC05099          
      IF (L.EQ.1) GO TO 902                                             DEC05100          
      IF (ISW2.NE.1) GO TO 20                                           DEC05101          
      ISW4=1                                                            DEC05102          
   20 IF (ISW3.EQ.1) GO TO 904                                          DEC05103          
      ICON=ML3*100                                                      DEC05104          
      ME3=ML3                                                           DEC05105          
      K3=K1                                                             DEC05106          
      INTV=INT2(I)                                                      DEC05107          
      CALL CODE                                                         DEC05108          
      IF (L.EQ.1) GO TO 902                                             DEC05109          
      IF (ISW3.EQ.1) GO TO 904                                          DEC05110          
      ICON=ICON+ML3                                                     DEC05111          
      ME2=ML3                                                           DEC05112          
      K4=K1                                                             DEC05113          
      K6=K5+NMC(I)                                                      DEC05114          
      IF (ISW2.EQ.1.OR.ISW4.EQ.1) GO TO 48                              DEC05115          
      MLOC=J1+NSUB+(ME3-1)*K1+ME2                                       DEC05116          
      NOS(MLOC)=NOS(MLOC)+1                                             DEC05117          
      K7=K6-NMC(I)                                                      DEC05118          
   22 K7=K7+1                                                           DEC05119          
      IF (K7.GT.K6) GO TO 24                                            DEC05120          
      IF (ICON-MSCL(K7)) 22,48,22                                       DEC05121          
   24 K9=K3-1                                                           DEC05122          
      DO 26 J=1,K9                                                      DEC05123          
   26 EFF1(J)=0.0                                                       DEC05124          
      K9=K4-1                                                           DEC05125          
      DO 28 J=1,K9                                                      DEC05126          
   28 EFF2(J)=0.0                                                       DEC05127          
      IF (ME3.EQ.K3) GO TO 30                                           DEC05128          
      EFF1(ME3)=1.                                                      DEC05129          
      GO TO 34                                                          DEC05130          
   30 K9=K3-1                                                           DEC05131          
      DO 32 J=1,K9                                                      DEC05132          
   32 EFF1(J)=-1.                                                       DEC05133          
   34 IF (ME2.EQ.K4) GO TO 36                                           DEC05134          
      EFF2(ME2)=1.                                                      DEC05135          
      GO TO 40                                                          DEC05136          
   36 K9=K4-1                                                           DEC05137          
      DO 38 J=1,K9                                                      DEC05138          
   38 EFF2(J)=-1.                                                       DEC05139          
   40 L1=0                                                              DEC05140          
      K9=K3-1                                                           DEC05141          
      K2=K4-1                                                           DEC05142          
      DO 46 J=1,K9                                                      DEC05143          
      DO 46 J2=1,K2                                                     DEC05144          
      ICO=J*100+J2                                                      DEC05145          
      IF (NMC(I).EQ.0) GO TO 44                                         DEC05146          
      K7=K6-NMC(I)                                                      DEC05147          
   42 K7=K7+1                                                           DEC05148          
      IF (K7.GT.K6) GO TO 44                                            DEC05149          
      IF (ICO-MSCL(K7)) 42,46,42                                        DEC05150          
   44 L1=L1+1                                                           DEC05151          
      MLOC=N+L1-1                                                       DEC05152          
      X(MLOC)=EFF1(J)*EFF2(J2)                                          DEC05153          
   46 CONTINUE                                                          DEC05154          
   48 K5=K6                                                             DEC05155          
      NSUB=NSUB+K3*K4                                                   DEC05156          
   50 N=N+(K3-1)*(K4-1)-NMC(I)                                          DEC05157          
C     ----------------------------------------------------              DEC05158          
C     PLACES CODES (X-XM), ETC, IN X ARRAY FOR POOLED REGRESSIONS       DEC05159          
C     ----------------------------------------------------              DEC05160          
   51 IF (NPR.EQ.0) GO TO 81                                            DEC05161          
   52 DO 80 I=1,NPR                                                     DEC05162          
      IF (NEGX(I)-1) 58,54,56                                           DEC05163          
   54 CALL NEGN (TX,JBEG,LGTX,IC,I)                                     DEC05164          
      GO TO 58                                                          DEC05165          
   56 CALL NEGNO (TX,JBEG,LGTX,IC,I)                                    DEC05166          
   58 L7=JBEG(I)                                                        DEC05167          
      L=LGTX(I)+L7-1                                                    DEC05168          
      J=NCDS                                                            01C05168          
      CALL FIELD (ICOD,L,IC,L7,J)                                       DEC05169          
      IF (J.EQ.1) GO TO 903                                             DEC05170          
      XR=ICOD                                                           DEC05171          
      IF (NEGX(I).EQ.0) GO TO 60                                        DEC05172          
      XR=XR*TX                                                          DEC05173          
   60 XR=XR/10.**NDECX(I)                                               DEC05174          
      IF (LOGE(I).EQ.0) GO TO 62                                        DEC05175          
      XR=ALOG10(XR)-XM(I)                                               DEC05176          
      GO TO 68                                                          DEC05177          
   62 XR=XR-XM(I)                                                       DEC05178          
   68 YR=1.                                                             DEC05179          
      K9=LQC(I)                                                         DEC05180          
      DO 80 J=1,K9                                                      DEC05181          
      X(N)=YR*XR                                                        DEC05182          
      YR=X(N)                                                           DEC05183          
   80 N=N+1                                                             DEC05184          
C     ----------------------------------------------------              DEC05185          
C     PLACES (Y-YM) IN X ARRAY FOR RHM                                  DEC05186          
C     ----------------------------------------------------              DEC05187          
   81 IF (NRHM.EQ.0)  GO TO 96                                          DEC05188          
      DO 95 I=1,NRHM                                                    DEC05189          
      IF (NEGY(I)-1) 86,82,84                                           DEC05190          
   82 CALL NEGN (TX,KBEG,LHY,IC,I)                                      DEC05191          
      GO TO 86                                                          DEC05192          
   84 CALL NEGNO (TX,KBEG,LHY,IC,I)                                     DEC05193          
   86 L7=KBEG(I)                                                        DEC05194          
      L=LHY(I)+L7-1                                                     DEC05195          
      J=NCDS                                                            01C05195          
      CALL FIELD (ICOD,L,IC,L7,J)                                       DEC05196          
      IF (J.EQ.1) GO TO 903                                             DEC05197          
      YR=ICOD                                                           DEC05198          
      IF (NEGY(I).EQ.0) GO TO 88                                        DEC05199          
      YR=YR*TX                                                          DEC05200          
   88 YR=YR/(10.**NDECY(I))                                             DEC05201          
      IF (LNY(I).EQ.0) GO TO 90                                         DEC05202          
      YR=ALOG10(YR)-YM(I)                                               DEC05203          
      GO TO 92                                                          DEC05204          
   90 YR=YR-YM(I)                                                       DEC05205          
   92 X(N)=YR                                                           DEC05206          
      N=N+1                                                             DEC05207          
   95 CONTINUE                                                          DEC05208          
   96 IF (N-1.NE.NOT) GO TO 900                                         DEC05209          
      GO TO 100                                                         DEC05210          
  900 WRITE (6,1000)                                                    DEC05211          
 1000 FORMAT (1H0,40HNO. LHM PLUS RHM DO NOT AGREE WITH PAR 1)          DEC05212          
      GO TO 904                                                         DEC05213          
  901 WRITE (6,1001) NCDS,IJOB                                          DEC05214          
 1001 FORMAT (1H0,30HCLASS CODE MISSING ON CARD NO.,I8,5X,37HCHECK PARAMDEC05215          
     1ETER CARDS FOR PROBLEM NO.,I3)                                    DEC05216          
      GO TO 904                                                         DEC05217          
  902 WRITE (6,1002) NCDS                                               DEC05218          
 1002 FORMAT (1H0,69HUNITS POSITION OF AN ID FIELD OR A CONTROL FIELD ISDEC05219          
     4 BLANK ON CARD NO.,I5)                                            DEC05220          
      GO TO 904                                                         DEC05221          
  903 WRITE (6,1003) NCDS                                               DEC05222          
 1003 FORMAT (1H0,55HUNITS POSITION OF AN X OR Y FIELD IS BLANK ON CARD DEC05223          
     1 NO.,I5)                                                          DEC05224          
  904 MULL=1                                                            DEC05225          
  100 CONTINUE                                                          DEC05226          
      RETURN                                                            DEC05227          
      END                                                               DEC05228          
*ENDTEXT                                                                                  
