      SUBROUTINE SVCVC                                                  DK220010          
C     -------------------------------------------                       DK220020          
C     SUBROUTINE USED TO COMPUTE VARIANCE AND COVARIANCE COMPONENTS,    DK220030          
C     HERITABILITIES, GENETIC, PHENOTYPIC AND ENVIRONMENTAL CORRELATIONSDK220040          
C     AND STANDARD ERRORS                                               DK220050          
C     ----------------------------------------------------              DK220060          
      EXTERNAL DSQRT                                                                      
      DIMENSION ARRAY(5000),SSCPR(630),SSS(630),RHM(2450),TOT(106),TOT2(DK220090          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100),NS2(15)    DK220100          
      DIMENSION FAB(5000),FY(630),TOT4(106),SAB(630),FSQ(525),TOT5(106) DK220110          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),MZ(10),IPL(13),NSDK220120          
     1P(13),NND(13),XP(81),YP(41),      NDC(10),NMI(10),MEN(20),NCL(20),DK220130          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(50),NCLN(50),LNE(50),NDEN(1                  
     300),NLIT(50),NMA(50),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(50),                  
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDK220160          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),                          
     6EFF1(50),EFF2(50),NOS(300),X(106),NMAC(50),I309(15),NR1(15),NW(15)                  
     7,R1I(100),R2I(100),WW(200),ICLR(90),IRM(40),LAD(90)               DK220190          
      DIMENSION IC(1000)                                                                  
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADK220200          
     1B4,LITY,TRED,YM,XP,YP,IM,MS,NEG,IPL,NSP,NND,FAB,FY,TOT4,SAB,FSQ,TODK220210          
     2T5,MZ,R1I,R2I,WW,ICLR,IRM,LAD                                     DK220220          
      COMMON /CMBLK2/LIT,LITR,NLIT,XM,EFF1,EFF2,X,NDC,NMI,MEN,NCL,LME,IBDK220230          
     1EG,IDEN,NCLN,NEN,LNE,NBEG,NDEN,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQDK220240          
     2C,NREGP,LGTX,JBEG,NDECX,NEGY,LNY,LHY,KBEG,NDECY,IC,NOS,NMAC       DK220250          
      COMMON /CMBLK3/RR,REP,DF,SDF,EDF,WK,IJOB,NAB,NLHM,NRHM,NMEA,NME,NNDK220260          
     1EA,NNE,N2F,NPR,IRAN,MPOP,LIOP,IAD,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,DK220270          
     2NOM,NON,ML,MLB,NS,NOT,KPUT,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,IN,I30DK220280          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DK220290          
     1L,L7,KA,KB,KC,SOF,NX,KD,MTY,MAN,NRUN,NRN,EDFF,NAME,EDFS,NAM5,IFP,NDK220300          
     2DFA,IFPC,NDFC,NDFAC,NINT,IBET,LBEG,LTHN,NNDC,WTT,SWT1             DK220310          
      W=NW(L)                                                           DK220320          
      R1=NR1(L)                                                         DK220330          
      R1=R1/100.                                                        DK220340          
      W=W/100.                                                          DK220350          
      C=1./R1                                                           DK220360          
      D=(1.-W)/R1                                                       DK220370          
      E=W/R1                                                            DK220380          
      TN=(SDF+1.)*WK                                                    DK220390          
      SK3=WK-1.                                                         DK220400          
      SK4=WK*WK                                                         DK220410          
      C1=(C*C*2.)*(TN-1.)                                               DK220420          
      C2=SK4*SDF                                                        DK220430          
      C3=SK4*EDF                                                        DK220440          
      SK5=2.*SK3                                                        DK220450          
      C4=C3*SDF                                                         DK220460          
      IDENT=0                                                           DK220470          
      WRITE (6,1003)                                                    DK220480          
 1003 FORMAT(1H0,15X,72HNEGATIVE VARIANCE COMPONENT ESTIMATES SET TO ZER                  
     1O FOR THESE COMPUTATIONS)                                                           
      WRITE (6,1000)                                                    DK220510          
 1000 FORMAT (1H0/1H ,76HESTIMATES OF HERITABILITIES, GENETIC, PHENOTYPIDK220520          
     1C, ENVIRONMENTAL CORRELATIONS//1H ,110H JOB ROW COL   RHM    RHM  DK220530          
     2 HERITABILITY  STANDARD  PHENOTYPIC  ENVIRONMENTAL NEG   VARIANCE DK220540          
     3OR COV COMPONENTS/ 1H ,28X, 47HOR GENETIC R   ERRORS   CORRELATIONDK220550          
     4 CORRELATION,11X,5HAMONG,12X,6HWITHIN)                            DK220560          
      DO 60 I=1,NRHM                                                    DK220570          
      WRITE (6,1001)                                                    DK220580          
 1001 FORMAT (1H )                                                      DK220590          
      DO 60 J=I,NRHM                                                    DK220600          
      IF (I-J) 16,12,12                                                 DK220610          
   12 K=NRHM*(I-1)-I*(I-3)/2                                            DK220620          
      H=(SSS(K)*C)/(D*SSS(K)+SSCPR(K))                                  DK220630          
      T=SSS(K)/(SSS(K)+SSCPR(K))                                        DK220640          
      T1=1.-T                                                           DK220650          
      SEH=DSQRT((C1*T1*T1*(1.+(SK3*T))*(1.+(SK3*T)))/C4)                DK220660          
      RP=1.                                                             DK220670          
      RE=1.                                                             DK220680          
      NEG=0                                                             DK220690          
      GO TO 46                                                          DK220700          
   16 K=NRHM*(I-1)-I*(I-3)/2                                            DK220710          
      K1=NRHM*(J-1)-J*(J-3)/2                                           DK220720          
      K2=K+J-I                                                          DK220730          
      SUM=SSS(K)*SSS(K1)                                                DK220740          
      IF (SUM) 18,18,20                                                 DK220750          
   18 H=0.0                                                             DK220760          
      SEH=0.                                                            DK220770          
      GO TO 22                                                          DK220780          
   20 SUM=DSQRT(SUM)                                                    DK220790          
      H=SSS(K2)/SUM                                                     DK220800          
   22 DEM1=(SSCPR(K)+(D*SSS(K)))*(SSCPR(K1)+(D*SSS(K1)))                DK220810          
      IF (DEM1.GT.0.0) GO TO 26                                         DK220820          
   24 RP=0.0                                                            DK220830          
      GO TO 28                                                          DK220840          
   26 RP=(SSCPR(K2)+(D*SSS(K2)))/DSQRT(DEM1)                            DK220850          
   28 RG=H*H                                                            DK220860          
      IF (H) 30,40,30                                                   DK220870          
   30 T2=SSS(K1)/(SSS(K1)+SSCPR(K1))                                    DK220880          
      T3=T*T2                                                           DK220890          
      IF (D-1.) 32,34,32                                                DK220900          
   32 RP1=(SSCPR(K2)+SSS(K2))/DSQRT((SSCPR(K)+SSS(K))*(SSCPR(K1)+SSS(K1)DK220910          
     1))                                                                DK220920          
      GO TO 36                                                          DK220930          
   34 RP1=RP                                                            DK220940          
   36 A1=1.+RG                                                          DK220950          
      B1=DSQRT(T3*RG)                                                   DK220960          
      C5=(T+T2)/T3                                                      DK220970          
      D1=(RG*((T-T2)**2))/(2.*T3)                                       DK220980          
      TK=(1.+(SK3*T))*(1.+(SK3*T2))                                     DK220990          
      RPB=RP1+(SK3*B1)                                                  DK221000          
      R=RP1-B1                                                          DK221010          
      SERG1=((A1*(TK+(RPB*RPB)))-(2.*B1*RPB*(C5+SK5))+D1)/(C2*T3)       DK221020          
      SERG2=((A1*(T1*(1.-T2)+(R**2)))-(2.*B1*R*(C5-2.))+D1)/(C3*T3)     DK221030          
      C5=SERG1+SERG2                                                    DK221040          
      IF (C5.LT.0.0) C5=0.0                                             DK221050          
      SEH=DSQRT(C5)                                                     DK221060          
   40 DEM2=(SSCPR(K)-(E*SSS(K)))*(SSCPR(K1)-(E*SSS(K1)))                DK221070          
      IF (DEM2) 42,44,44                                                DK221080          
   42 DEM2=-DEM2                                                        DK221090          
      RE=(SSCPR(K2)-(E*SSS(K2)))/(DSQRT(DEM2)*(-1.))                    DK221100          
      NEG=1                                                             DK221110          
      GO TO 48                                                          DK221120          
   44 RE=(SSCPR(K2)-(E*SSS(K2)))/DSQRT(DEM2)                            DK221130          
      NEG=0                                                             DK221140          
      GO TO 48                                                          DK221150          
   46 K2=K                                                              DK221160          
   48 IF (H.EQ.0.0) GO TO 49                                            DK221170          
      WRITE (6,1002) IJOB,I,J,LITY(I),LITY(J),H,SEH,RP,RE,NEG,SSS(K2),SSDK221180          
     1CPR(K2)                                                           DK221190          
 1002 FORMAT (1H ,3I4,2X,A6,1X,A6,4F11.3,I9,2F18.8)                     DK221200          
      GO TO 60                                                          DK221210          
   49 WRITE (6,1004) IJOB,I,J,LITY(I),LITY(J),RP,NEG,SSS(K2),SSCPR(K2)  DK221220          
 1004 FORMAT (1H ,3I4,2X,A6,1X,A6,22X,F11.3,11X,I9,2F18.8)              DK221230          
   60 CONTINUE                                                          DK221240          
      RETURN                                                            DK221250          
      END                                                               DK221260          
