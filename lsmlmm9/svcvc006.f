*TEXT                                                                                     
      SUBROUTINE SVCVC                                                  DK220010          
C     -------------------------------------------                       DK220020          
C     SUBROUTINE USED TO COMPUTE VARIANCE AND COVARIANCE COMPONENTS,    DK220030          
C     HERITABILITIES, GENETIC, PHENOTYPIC AND ENVIRONMENTAL CORRELATIONSDK220040          
C     AND STANDARD ERRORS                                               DK220050          
C     ----------------------------------------------------              DK220060          
      DIMENSION ARRAY(5000),SSCPR(630),SSS(630),RHM(2450),TOT(106),TOT2(DK220090          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100),NS2(15)    DK220100          
      DIMENSION FAB(5000),FY(630),TOT4(106),SAB(630),FSQ(525),TOT5(106) DK220110          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),MZ(10),IPL(13),NSDK220120          
     1P(13),NND(13),XP(81),YP(41),      NDC(10),NMI(10),MEN(20),NCL(20),DK220130          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(40),NCLN(40),LNE(40),NDEN(1DK220140          
     300),NLIT(40),NMA(30),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(40),DK220150          
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDK220160          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),IC(480),DK220170          
     6EFF1(50),EFF2(50),NOS(300),X(106),NMAC(40),I309(15),NR1(15),NW(15)DK220180          
     7,R1I(100),R2I(100)                                                DK220190          
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADK220200          
     1B4,LITY,TRED,YM,XP,YP,IM,MS,NEG,IPL,NSP,NND,FAB,FY,TOT4,SAB,FSQ,TODK220210          
     2T5,MZ,R1I,R2I                                                     DK220220          
      COMMON /CMBLK2/LIT,LITR,NLIT,XM,EFF1,EFF2,X,NDC,NMI,MEN,NCL,LME,IBDK220230          
     1EG,IDEN,NCLN,NEN,LNE,NBEG,NDEN,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQDK220240          
     2C,NREGP,LGTX,JBEG,NDECX,NEGY,LNY,LHY,KBEG,NDECY,IC,NOS,NMAC       DK220250          
      COMMON /CMBLK3/RR,REP,DF,SDF,EDF,WK,IJOB,NAB,NLHM,NRHM,NMEA,NME,NNDK220260          
     1EA,NNE,N2F,NPR,IRAN,MPOP,LIOP,IAD,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,DK220270          
     2NOM,NON,ML,MLB,NS,NOT,KPUT,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,IN,I30DK220280          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DK220290          
     1L,L7,KA,KB,KC,SOF,NX,KD,MTY,MAN,NRUN,NRN,EDFF,NAME,EDFS,NAM5,IFP,NDK220300          
     2DFA,IFPC,NDFC,NDFAC,NINT                                          DK220310          
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
      WRITE (6,1000)                                                    DK220480          
 1000 FORMAT (1H0/1H ,76HESTIMATES OF HERITABILITIES, GENETIC, PHENOTYPIDK220490          
     1C, ENVIRONMENTAL CORRELATIONS//1H ,110H JOB ROW COL   RHM    RHM  DK220500          
     2 HERITABILITY  STANDARD  PHENOTYPIC  ENVIRONMENTAL NEG   VARIANCE DK220510          
     3OR COV COMPONENTS/ 1H ,28X, 47HOR GENETIC R   ERRORS   CORRELATIONDK220520          
     4 CORRELATION,11X,5HAMONG,12X,6HWITHIN)                            DK220530          
      DO 60 I=1,NRHM                                                    DK220540          
      WRITE (6,1001)                                                    DK220550          
 1001 FORMAT (1H )                                                      DK220560          
      DO 60 J=I,NRHM                                                    DK220570          
      IF (I-J) 16,12,50                                                 DK220580          
   12 K=NRHM*(I-1)-I*(I-3)/2                                            DK220590          
      H=(SSS(K)*C)/(D*SSS(K)+SSCPR(K))                                  DK220600          
      T=SSS(K)/(SSS(K)+SSCPR(K))                                        DK220610          
      T1=1.-T                                                           DK220620          
      SEH=SQTF((C1*T1*T1*(1.+(SK3*T))*(1.+(SK3*T)))/C4)                                   
      RP=1.                                                             DK220640          
      RE=1.                                                             DK220650          
      NEG=0                                                             DK220660          
      GO TO 46                                                          DK220670          
   16 K=NRHM*(I-1)-I*(I-3)/2                                            DK220680          
      K1=NRHM*(J-1)-J*(J-3)/2                                           DK220690          
      K2=K+J-I                                                          DK220700          
      SUM=SSS(K)*SSS(K1)                                                DK220710          
      IF (SUM) 18,18,20                                                 DK220720          
   18 H=0.0                                                             DK220730          
      SEH=0.                                                            DK220740          
      GO TO 22                                                          DK220750          
   20 SUM=SQTF(SUM)                                                                       
      H=SSS(K2)/SUM                                                     DK220770          
   22 DEM1=(SSCPR(K)+(D*SSS(K)))*(SSCPR(K1)+(D*SSS(K1)))                DK220780          
      IF (DEM1.GT.0.0) GO TO 26                                         DK220790          
   24 RP=0.0                                                            DK220800          
      GO TO 28                                                          DK220810          
   26 RP=(SSCPR(K2)+(D*SSS(K2)))/SQTF(DEM1)                                               
   28 RG=H*H                                                            DK220830          
      IF (H) 30,40,30                                                   DK220840          
   30 T2=SSS(K1)/(SSS(K1)+SSCPR(K1))                                    DK220850          
      T3=T*T2                                                           DK220860          
      IF (D-1.) 32,34,32                                                DK220870          
   32 RP1=(SSCPR(K2)+SSS(K2))/SQTF((SSCPR(K)+SSS(K))*(SSCPR(K1)+SSS(K1)                   
     1))                                                                DK220890          
      GO TO 36                                                          DK220900          
   34 RP1=RP                                                            DK220910          
   36 A1=1.+RG                                                          DK220920          
      B1=SQTF(T3*RG)                                                                      
      C5=(T+T2)/T3                                                      DK220940          
      D1=(RG*((T-T2)**2))/(2.*T3)                                       DK220950          
      TK=(1.+(SK3*T))*(1.+(SK3*T2))                                     DK220960          
      RPB=RP1+(SK3*B1)                                                  DK220970          
      R=RP1-B1                                                          DK220980          
      SERG1=((A1*(TK+(RPB*RPB)))-(2.*B1*RPB*(C5+SK5))+D1)/(C2*T3)       DK220990          
      SERG2=((A1*(T1*(1.-T2)+(R**2)))-(2.*B1*R*(C5-2.))+D1)/(C3*T3)     DK221000          
      C5=SERG1+SERG2                                                    DK221010          
      IF (C5.LT.0.0) C5=0.0                                             DK221020          
      SEH=SQTF(C5)                                                                        
   40 DEM2=(SSCPR(K)-(E*SSS(K)))*(SSCPR(K1)-(E*SSS(K1)))                DK221040          
      IF (DEM2) 42,44,44                                                DK221050          
   42 DEM2=-DEM2                                                        DK221060          
      RE=(SSCPR(K2)-(E*SSS(K2)))/(SQTF(DEM2)*(-1.))                                       
      NEG=1                                                             DK221080          
      GO TO 48                                                          DK221090          
   44 RE=(SSCPR(K2)-(E*SSS(K2)))/SQTF(DEM2)                                               
      NEG=0                                                             DK221110          
      GO TO 48                                                          DK221120          
   46 K2=K                                                              DK221130          
   48 WRITE (6,1002) IJOB,I,J,LITY(I),LITY(J),H,SEH,RP,RE,NEG,SSS(K2),SSDK221140          
     1CPR(K2)                                                           DK221150          
 1002 FORMAT (1H ,3I4,2X,A6,1X,A6,4F11.3,I9,2F18.8)                     DK221160          
      GO TO 60                                                          DK221170          
   50 WRITE (6,1003)                                                    DK221180          
 1003 FORMAT (1H0,5HERROR)                                              DK221190          
   60 CONTINUE                                                          DK221200          
      RETURN                                                            DK221210          
      END                                                               DK221220          
*ENDTEXT                                                                                  
