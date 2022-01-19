      SUBROUTINE LSANOV                                                 DK210010          
C     ------------------------------------------------------            DK210020          
C     SUBROUTINE WHICH COMPUTES AND LISTS THE ANOVA AND VARIANCE AND    DK210030          
C     COVARIANCE COMPONENTS FROM INDIRECT ANALYSES                      DK210040          
C     ------------------------------------------------------            DK210050          
      EXTERNAL DSQRT                                                                      
      DIMENSION ARRAY(5000),SSCPR(630),SSS(630),RHM(2450),TOT(106),TOT2(DK210090          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100),NS2(15)    DK210100          
      DIMENSION FAB(5000),FY(630),TOT4(106),SAB(630),FSQ(525),TOT5(106) DK210110          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),MZ(10),IPL(13),NSDK210120          
     1P(13),NND(13),XP(81),YP(41),LP(5),NDC(10),NMI(10),MEN(20),NCL(20),DK210130          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(50),NCLN(50),LNE(50),NDEN(1                  
     300),NLIT(50),NMA(50),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(50),                  
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDK210160          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),                          
     6EFF1(50),EFF2(50),NOS(300),X(106),NMAC(50),I309(15),NR1(15),NW(15)                  
     7,R1I(100),R2I(100),WW(200),ICLR(90),IRM(40),LAD(90)               DK210190          
      DIMENSION IC(1000)                                                                  
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADK210200          
     1B4,LITY,TRED,YM,XP,YP,IM,MS,NEG,IPL,NSP,NND,FAB,FY,TOT4,SAB,FSQ,TODK210210          
     2T5,MZ,R1I,R2I,WW,ICLR,IRM,LAD                                     DK210220          
      COMMON /CMBLK2/LIT,LITR,NLIT,XM,EFF1,EFF2,X,NDC,NMI,MEN,NCL,LME,IBDK210230          
     1EG,IDEN,NCLN,NEN,LNE,NBEG,NDEN,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQDK210240          
     2C,NREGP,LGTX,JBEG,NDECX,NEGY,LNY,LHY,KBEG,NDECY,IC,NOS,NMAC       DK210250          
      COMMON /CMBLK3/RR,REP,DF,SDF,EDF,WK,IJOB,NAB,NLHM,NRHM,NMEA,NME,NNDK210260          
     1EA,NNE,N2F,NPR,IRAN,MPOP,LIOP,IAD,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,DK210270          
     2NOM,NON,ML,MLB,NS,NOT,KPUT,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,IN,I30DK210280          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DK210290          
     1L,L7,KA,KB,KC,SOF,NX,KD,MTY,MAN,NRUN,NRN,EDFF,NAME,EDFS,NAM5,IFP,NDK210300          
     2DFA,IFPC,NDFC,NDFAC,NINT,IBET,LBEG,LTHN,NNDC,WTT,SWT1             DK210310          
      INTEGER RGNL,RGNQ,RGNC,RL,RQ,RC,REG1,REG2                                           
      INTEGER A2                                                                          
      DATA MUMYM,IBLK,IXLAB,IBLAB/6HMU-YM ,6H      ,6HX     ,6HB     /  DK210320          
      DATA LP(1),LP(2),LP(3),LP(4),LP(5)/6HLINEAR,6HQUAD  ,6HCUBIC ,6HQUDK210330          
     1ARD ,6HQUIN  /,IRES,IRMD/8HRESIDUAL,8HREMAINDR/                   DK210340          
      DATA RGNL,RGNQ,RGNC,RL,RQ,RC,REG1,REG2/6H RGN L,6H RGN Q,6H RGN C,DK210350          
     16HRL    ,6HRQ    ,6HRC    ,6HREGRES,6HSIONS /                     DK210360          
      IF (MTY.GT.1.AND.NRN.EQ.NRUN) GO TO 1                             DK210370          
      IF (LIOP.EQ.20) GO TO 2                                           DK210380          
      LAB1(99)=IBLK                                                     DK210390          
      WRITE (6,1011)                                                    DK210400          
 1011 FORMAT (1H1,28X,34HLEAST-SQUARES ANALYSIS OF VARIANCE)            DK210410          
      GO TO 2                                                           DK210420          
    1 KB=2                                                              DK210430          
      LAB1(99)=IRMD                                                     DK210440          
      WRITE (6,1000)                                                    DK210450          
 1000 FORMAT(1H1,24X,43HCOMBINED LEAST SQUARES ANALYSIS OF VARIANCE)                      
    2 IF (NAB.EQ.0.OR.NAB.EQ.3) LAB1(1)=MUMYM                           DK210470          
      DO 108 K=1,NRHM                                                   DK210480          
      K5=0                                                              DK210490          
      IF (KB.EQ.2.OR.LIOP.NE.20) WRITE (6,1012) LITY(K)                 DK210500          
 1012 FORMAT (1H / 42X,A6)                                              DK210510          
      IF (KB.EQ.2) GO TO 3                                              DK210520          
      IF (LIOP.NE.20) WRITE (6,1013)                                    DK210530          
 1013 FORMAT (1H ,/ 5X,6HSOURCE,8X,4HD.F.,8X,14HSUM OF SQUARES,6X,12HMEADK210540          
     1N SQUARES,10X,1HF,6X,4HPROB)                                                        
      GO TO 4                                                           DK210560          
    3 WRITE (6,1001)                                                    DK210570          
 1001 FORMAT (1H ,/ 5X,6HSOURCE,8X,4HD.F.,8X,14HSUM OF SQUARES,6X,12HMEADK210580          
     1N SQUARES,10X,1HF,6X,4HPROB,5X,10HERROR LINE)                                       
      IF (NLHM.EQ.0) GO TO 16                                           DK210600          
    4 DO 80 I=1,NLHM                                                    DK210610          
      K1=MATX+(K-1)*NLHM+I                                              DK210620          
   80 RHM(I)=ARRAY(K1)                                                  DK210630          
   16 K6=DF                                                             DK210640          
   82 IF (KB.EQ.2.OR.LIOP.NE.20) WRITE (6,1004)                         DK210650          
 1004 FORMAT (1H )                                                      DK210660          
      IF (NLHM.EQ.0) TRED(K)=0.0                                        DK210670          
      K2=1                                                              DK210680          
      IF (NCPR.EQ.0) GO TO 84                                           DK210690          
      K1=NRHM*(K-1)-K*(K-3)/2                                           DK210700          
      SS=SSCPR(K1)*EDF+TRED(K)                                          DK210710          
      GO TO 86                                                          DK210720          
   84 SS=SSCPR(K)                                                       DK210730          
   86 IF (NAB.GT.1) GO TO 22                                            DK210740          
      IF (LIOP.NE.20) WRITE (6,1014) K6,SS                              DK210750          
 1014 FORMAT (1H ,5HTOTAL,13X,I6,F20.6,F18.6,F13.3)                     DK210760          
      GO TO 21                                                          DK210770          
   22 IF (KB.EQ.2) GO TO 21                                             DK210780          
      IF (LIOP.NE.20) WRITE (6,1026) K6,SS                              DK210790          
 1026 FORMAT (1H ,9HWITHIN SS,9X,I6,F20.6,1X,F17.6,2X,F11.3)            DK210800          
   21 K6=K6-NLHM                                                        DK210810          
      IF(K6.EQ.0) K6=1                                                  DK210820          
      EDF=K6                                                            DK210830          
      R=SS-TRED(K)                                                      DK210840          
      SD=R                                                              DK210850          
      SM=R/EDF                                                          DK210860          
      K3=2                                                              DK210870          
      RECIP=NLHM                                                        DK210880          
      IF (NLHM.EQ.0) RECIP=1.0                                          DK210890          
      RECIP=TRED(K)/RECIP                                               DK210900          
      F=RECIP/SM                                                        DK210910          
      IF (KB.EQ.2) GO TO 5                                              DK210920          
      FPRO=PROF(NLHM,EDF,F)                                             DK210930          
      IF (LIOP.NE.20) WRITE (6,1015) NLHM,TRED(K),RECIP,F,FPRO          DK210940          
 1015 FORMAT (1H ,15HTOTAL REDUCTION,I9,F20.6,F18.6,F13.3,F10.4)        DK210950          
      GO TO 6                                                           DK210960          
    5 IF (MTY.EQ.3.OR.MTY.EQ.5.OR.MTY.EQ.7) GO TO 7                     DK210970          
      IF (MTY.NE.2) GO TO 8                                             DK210980          
      TREDS=FY(K1)-SD                                                   DK210990          
      R=EDFF-EDF                                                        DK211000          
      LAB1(100)=NAME                                                    DK211010          
      GO TO 9                                                           DK211020          
    8 TREDS=FY(K1)-SAB(K1)                                              DK211030          
      R=EDFF-EDFS                                                       DK211040          
      LAB1(100)=NAM5                                                    DK211050          
    9 SS=TREDS/R                                                        DK211060          
      K6=R                                                              DK211070          
      R=SS/SM                                                           DK211080          
      IF (MTY.EQ.4) GO TO 15                                            DK211090          
      FPRO=PROF(K6,EDF,R)                                               DK211100          
      WRITE (6,1003) LAB1(100),K6,TREDS,SS,R,FPRO,IRMD                  DK211110          
 1003 FORMAT (1H ,A6,9X,I7,F22.6,F18.6,F13.3,F10.4,4X,A8)               DK211120          
      IF (MTY.EQ.6) GO TO 7                                             DK211130          
      GO TO 6                                                           DK211140          
   15 SOD=SAB(K1)-SD                                                    DK211150          
      F=EDFS-EDF                                                        DK211160          
      SDF=SOD/F                                                         DK211170          
      L2=F                                                              DK211180          
      EL2=F                                                             DK211190          
      F=SDF/SM                                                          DK211200          
      R=SS/SDF                                                          DK211210          
      FPRO=PROF(K6,EL2,R)                                               DK211220          
      WRITE (6,1003) LAB1(100),K6,TREDS,SS,R,FPRO,NAME                  DK211230          
      FPRO=PROF(L2,EDF,F)                                               DK211240          
      WRITE (6,1003) NAME,L2,SOD,SDF,F,FPRO,IRMD                        DK211250          
      GO TO 6                                                           DK211260          
    7 L2=IFP*(K-1)+1                                                    DK211270          
      TREDS=FSQ(L2)                                                     DK211280          
      R=NDFA                                                            DK211290          
      SS=TREDS/R                                                        DK211300          
      K6=R                                                              DK211310          
      IF (MTY.NE.3) GO TO 17                                            DK211320          
      LAB1(97)=NAME                                                     DK211330          
      SOD=FY(K1)-SD                                                     DK211340          
      F=EDFF-EDF                                                        DK211350          
      GO TO 18                                                          DK211360          
   17 IF (MTY.NE.6) GO TO 19                                            DK211370          
      LAB1(97)=NAME                                                     DK211380          
      LAB1(100)=LAB1(98)                                                DK211390          
      SOD=SAB(K1)-SD                                                    DK211400          
      F=EDFS-EDF                                                        DK211410          
      GO TO 18                                                          DK211420          
   19 SOD=FY(K1)-SAB(K1)                                                DK211430          
      F=EDFF-EDFS                                                       DK211440          
      LAB1(97)=NAM5                                                     DK211450          
   18 SDF=SOD/F                                                         DK211460          
      L2=F                                                              DK211470          
      EL2=F                                                             DK211480          
      F=SDF/SM                                                          DK211490          
      R=SS/SDF                                                          DK211500          
      FPRO=PROF(K6,EL2,R)                                               DK211510          
      WRITE (6,1003) LAB1(100),K6,TREDS,SS,R,FPRO,LAB1(97)              DK211520          
      IF (IFP.EQ.1) GO TO 23                                            DK211530          
      DO 24 I=2,IFP                                                     DK211540          
      K3=I-1                                                            DK211550          
      K4=IFP*(K-1)+I                                                    DK211560          
      SS=FSQ(K4)                                                        DK211570          
      K6=1                                                              DK211580          
      IF (I.NE.7) GO TO 37                                              DK211590          
      K6=NDFA-5                                                         DK211600          
      SS=SS/FLOAT(K6)                                                   DK211610          
      R=SS/SDF                                                          DK211620          
      FPRO=PROF(K6,EL2,R)                                               DK211630          
      WRITE (6,1006) IRES,K6,FSQ(K4),SS,R,FPRO,LAB1(97)                 DK211640          
 1006 FORMAT (1H ,2X,A8,7X,I3,2X,F22.6,F18.6,F13.3,F10.4,4X,A8)         DK211650          
      GO TO 24                                                          DK211660          
   37 R=SS/SDF                                                          DK211670          
      FPRO=PROF(K6,EL2,R)                                               DK211680          
      WRITE (6,1006) LP(K3),K6,FSQ(K4),SS,R,FPRO,LAB1(97)               DK211690          
   24 CONTINUE                                                          DK211700          
   23 IF (MTY.EQ.5) GO TO 25                                            DK211710          
      IF (MTY.EQ.7) GO TO 26                                            DK211720          
      FPRO=PROF(L2,EDF,F)                                               DK211730          
      WRITE (6,1003) NAME,L2,SOD,SDF,F,FPRO,IRMD                        DK211740          
      GO TO 6                                                           DK211750          
   25 TREDS=SAB(K1)-SD                                                  DK211760          
      R=EDFS-EDF                                                        DK211770          
      SS=TREDS/R                                                        DK211780          
      K6=R                                                              DK211790          
      EL2=R                                                             DK211800          
      R=SS/SM                                                           DK211810          
      F=SDF/SS                                                          DK211820          
      FPRO=PROF(L2,EL2,F)                                               DK211830          
      WRITE (6,1003) NAM5,L2,SOD,SDF,F,FPRO,NAME                        DK211840          
      FPRO=PROF(K6,EDF,R)                                               DK211850          
      WRITE (6,1003) NAME,K6,TREDS,SS,R,FPRO,IRMD                       DK211860          
      GO TO 6                                                           DK211870          
   26 WK=(SAB(K1)-SD)/(EDFS-EDF)                                        DK211880          
      EL2=EDFS-EDF                                                      DK211890          
      F=SDF/SM                                                          DK211900          
      FPRO=PROF(L2,EDF,F)                                               DK211910          
      WRITE (6,1003) NAM5,L2,SOD,SDF,F,FPRO,IRMD                        DK211920          
      L2=IFPC*(K-1)+246                                                 DK211930          
      TREDS=FSQ(L2)                                                     DK211940          
      R=NDFC                                                            DK211950          
      SS=TREDS/R                                                        DK211960          
      K6=R                                                              DK211970          
      R=SS/WK                                                           DK211980          
      FPRO=PROF(K6,EL2,R)                                               DK211990          
      WRITE (6,1003) LAB1(98),K6,TREDS,SS,R,FPRO,NAME                   DK212000          
      IF (IFPC.EQ.1) GO TO 48                                           DK212010          
      DO 46 I=2,IFPC                                                    DK212020          
      K3=I-1                                                            DK212030          
      K4=IFPC*(K-1)+I+245                                               DK212040          
      SS=FSQ(K4)                                                        DK212050          
      K6=1                                                              DK212060          
      IF (I.NE.7) GO TO 47                                              DK212070          
      K6=NDFC-5                                                         DK212080          
      SS=SS/FLOAT(K6)                                                   DK212090          
      R=SS/WK                                                           DK212100          
      FPRO=PROF(K6,EL2,R)                                               DK212110          
      WRITE (6,1006)  IRES,K6,FSQ(K4),SS,R,FPRO,NAME                    DK212120          
      GO TO 46                                                          DK212130          
   47 R=SS/WK                                                           DK212140          
      FPRO=PROF(K6,EL2,R)                                               DK212150          
      WRITE (6,1006) LP(K3),K6,FSQ(K4),SS,R,FPRO,NAME                   DK212160          
   46 CONTINUE                                                          DK212170          
   48 A2=IXLAB                                                          DK212180          
      SS=FSQ(490+K)/FLOAT(NDFAC)                                        DK212190          
      R=SS/WK                                                           DK212200          
      FPRO=PROF(NDFAC,EL2,R)                                            DK212210          
      WRITE (6,1016) LAB1(100),A2,LAB1(98),NDFAC,FSQ(490+K),SS,R,FPRO,NADK212220          
     1ME                                                                DK212230          
      TREDS=SAB(K1)-SD                                                  DK212240          
      R=EDFS-EDF                                                        DK212250          
      SS=TREDS/R                                                        DK212260          
      K6=R                                                              DK212270          
      R=SS/SM                                                           DK212280          
      FPRO=PROF(K6,EDF,R)                                               DK212290          
      WRITE (6,1003) NAME,K6,TREDS,SS,R,FPRO,IRMD                       DK212300          
    6 K3=3                                                              DK212310          
      NERC=0                                                            DK212320          
      L2=1                                                              DK212330          
      IF (NLHM.EQ.0) GO TO 27                                           DK212340          
      DO 104 I3=1,NS                                                    DK212350          
      NBRC=NERC+1                                                       DK212360          
      NERC=IM(I3)                                                       DK212370          
      TREDS=0.0                                                         DK212380          
      DO 212 I=NBRC,NERC                                                DK212390          
      TEMP=0.0                                                          DK212400          
      DO 209 J=NBRC,NERC                                                DK212410          
      IF (I-J.LT.0) GO TO 204                                           DK212420          
  202 K1=NLHM*(J-1)-J*(J-3)/2+I-J                                       DK212430          
      GO TO 208                                                         DK212440          
  204 K1=NLHM*(I-1)-I*(I-3)/2+J-I                                       DK212450          
  208 K4=J                                                              DK212460          
  209 TEMP=TEMP+RHM(K4)*ARRAY(K1)                                       DK212470          
      K4=I                                                              DK212480          
  210 TREDS=TREDS+RHM(K4)*TEMP                                          DK212490          
  212 CONTINUE                                                          DK212500          
      IF (I3.EQ.1) F=TREDS                                              DK212510          
   94 K6=NERC-NBRC+1                                                    DK212520          
      K5=K5+K6                                                          DK212530          
      IF (K5.GE.IEI.AND.K5.LT.IE) GO TO 98                              DK212540          
      A2=IBLK                                                           DK212550          
      GO TO 100                                                         DK212560          
   98 A2=IXLAB                                                          DK212570          
      LAB3(K5)=LAB2(K5)                                                 DK212580          
  100 IF (K5.LT.IE) GO TO 102                                           DK212590          
      A2=IBLAB                                                          DK212600          
      IF (LAB4(K5).EQ.RGNL) A2=RL                                       DK212610          
      IF (LAB4(K5).EQ.RGNQ) A2=RQ                                       DK212620          
      IF (LAB4(K5).EQ.RGNC) A2=RC                                       DK212630          
      LAB1(K5)=LAB2(K5)                                                 DK212640          
  102 R=K6                                                              DK212650          
      SS=TREDS/R                                                        DK212660          
      R=SS/SM                                                           DK212670          
      K4=K5-K6+1                                                        DK212680          
      IF (K4.EQ.IEI.AND.KB.EQ.1.AND.MTY.EQ.7) GO TO 52                  DK212690          
      GO TO 53                                                          DK212700          
   52 FSQ(490+K)=TREDS                                                  DK212710          
      NDFAC=K6                                                          DK212720          
   53 FPRO=PROF(K6,EDF,R)                                               DK212730          
      IF (K5.EQ.IE.AND.NLHM.GE.IE.AND.NRN.EQ.NRUN) WRITE (6,1030) REG1,RDK212740          
     1EG2                                                               DK212750          
 1030 FORMAT (1H ,2X,2A6)                                               DK212760          
      IF (KB.EQ.2.OR.LIOP.NE.20) WRITE (6,1016) LAB1(K5),A2,LAB3(K5),K6,DK212770          
     1TREDS,SS,R,FPRO,LAB1(99)                                          DK212780          
 1016 FORMAT (1H ,A6,1X,A2,1X,A6,I6,F22.6,F18.6,F13.3,F10.4,4X,A8)      DK212790          
      J=0                                                               DK212800          
      IF (KD.GT.0.AND.I3.EQ.2) GO TO 31                                 DK212810          
      IF (MTY.EQ.6.AND.KB.EQ.1.AND.I3.EQ.1) GO TO 31                    DK212820          
      GO TO 30                                                          DK212830          
   31 IF (MTY.EQ.3.OR.MTY.GT.4) GO TO 32                                DK212840          
      GO TO 30                                                          DK212850          
   32 IFP=1                                                             DK212860          
      IF (IPL(1).NE.1) GO TO 33                                         DK212870          
      J=1                                                               DK212880          
      IF (K6-5) 34,34,35                                                DK212890          
   34 IFP=K6+1                                                          DK212900          
      GO TO 33                                                          DK212910          
   35 IFP=7                                                             DK212920          
   33 I=IFP*(K-1)+1                                                     DK212930          
      FSQ(I)=TREDS                                                      DK212940          
      NDFA=K6                                                           DK212950          
      LAB1(98)=LAB1(K5)                                                 DK212960          
      LAB1(100)=LAB1(K5)                                                DK212970          
   30 IF (KB.EQ.1.AND.MTY.EQ.7.AND.I3.EQ.1) GO TO 40                    DK212980          
      GO TO 41                                                          DK212990          
   40 IFPC=1                                                            DK213000          
      IF (IPL(1).NE.2) GO TO 42                                         DK213010          
      J=2                                                               DK213020          
      IF (K6-5) 43,43,44                                                DK213030          
   43 IFPC=K6+1                                                         DK213040          
      GO TO 42                                                          DK213050          
   44 IFPC=7                                                            DK213060          
   42 I=IFPC*(K-1)+1                                                    DK213070          
      FSQ(245+I)=TREDS                                                  DK213080          
      NDFC=K6                                                           DK213090          
      LAB1(98)=LAB1(K5)                                                 DK213100          
   41 IF (L2.GT.NSME.OR.IM(I3).NE.NND(L2)) GO TO 104                    DK213110          
      L2=L2+1                                                           DK213120          
      SS=0.                                                             DK213130          
      DO 20 L=1,K6                                                      DK213140          
      IF (L.GT.5) GO TO 20                                              DK213150          
      I=K*5+L+(L2-2)*NRHM*5                                             DK213160          
      IF (J.NE.1) GO TO 36                                              DK213170          
      K4=IFP*(K-1)+L+1                                                  DK213180          
      FSQ(K4)=SSS(I)                                                    DK213190          
   36 R=SSS(I)/SM                                                       DK213200          
      FPRO=PROF(1,EDF,R)                                                DK213210          
      IF (KB.EQ.2.OR.LIOP.NE.20) WRITE (6,1027) LP(L),SSS(I),SSS(I),R,FPDK213220          
     1RO,LAB1(99)                                                       DK213230          
 1027 FORMAT (1H ,2X,A6,11X,1H1,4X,F20.6,F18.6,F13.3,F10.4,4X,A8)       DK213240          
      SS=SS+SSS(I)                                                      DK213250          
      IF (J.NE.2) GO TO 20                                              DK213260          
      K4=IFPC*(K-1)+L+1                                                 DK213270          
      FSQ(245+K4)=SSS(I)                                                DK213280          
   20 CONTINUE                                                          DK213290          
      IF (K6.LT.6) GO TO 104                                            DK213300          
      TREDS=TREDS-SS                                                    DK213310          
      IF (J.NE.2) GO TO 45                                              DK213320          
      K4=IFPC*(K-1)+7                                                   DK213330          
      FSQ(245+K4)=TREDS                                                 DK213340          
   45 I=K6-5                                                            DK213350          
      SS=TREDS/FLOAT(I)                                                 DK213360          
      R=SS/SM                                                           DK213370          
      FPRO=PROF(I,EDF,R)                                                DK213380          
      IF (KB.EQ.2.OR.LIOP.NE.20) WRITE(6,1028)I,TREDS,SS,R,FPRO,LAB1(99)DK213390          
 1028 FORMAT (1H ,2X,8HRESIDUAL,4X,I6,4X,F20.6,F18.6,F13.3,F10.4,4X,A8) DK213400          
      IF (J.NE.1) GO TO 104                                             DK213410          
      K4=IFP*(K-1)+7                                                    DK213420          
      FSQ(K4)=TREDS                                                     DK213430          
  104 CONTINUE                                                          DK213440          
   27 K6=EDF                                                            DK213450          
      IF (NAB.GT.2) SM=SD/EDF                                           DK213460          
      IF (KB.EQ.2) GO TO 54                                             DK213470          
      IF (LIOP.NE.20) WRITE (6,1017) K6,SD,SM                           DK213480          
 1017 FORMAT (1H ,9HREMAINDER,9X,I6,F20.6,F18.6)                        DK213490          
      GO TO 310                                                         DK213500          
   54 WRITE (6,1005) K6,SD,SM                                           DK213510          
 1005 FORMAT (1H ,9HREMAINDER,7X,I6,F22.6,F18.6)                        DK213520          
  310 IF (MTY.GT.1.OR.NLHM.EQ.0) GO TO 311                              DK213530          
      IF (NCPR.EQ.0) GO TO 312                                          DK213540          
      K1=NRHM*(K-1)-K*(K-3)/2                                           DK213550          
      SS=SSCPR(K1)*EDF+TRED(K)                                          DK213560          
      GO TO 313                                                         DK213570          
  312 SS=SSCPR(K)                                                       DK213580          
  313 SD=DSQRT(SM)                                                      DK213590          
      CV=(SD/TOT4(K))*100.                                              DK213600          
      IF (NAB.EQ.0.OR.NAB.EQ.3) GO TO 314                               DK213610          
      SS=TRED(K)/SS                                                     DK213620          
      R=DSQRT(SS)                                                       DK213630          
      GO TO 315                                                         DK213640          
  314 IF (NAB.EQ.3) GO TO 311                                           DK213650          
      K6=NLHM+K                                                         DK213660          
      IF (IBET.EQ.0) SWT1=NCDS                                          DK213670          
      F=(TOT(K6)*TOT(K6))/SWT1                                          DK213680          
      SS=(TRED(K)-F)/(SS-F)                                             DK213690          
      R=DSQRT(SS)                                                       DK213700          
  315 WRITE (6,1029) TOT4(K),SD,CV,SS,R                                 DK213710          
 1029 FORMAT(1H0,5X,6HMEAN =,F12.5,2X,26HERROR STANDARD DEVIATION =,                      
     1F12.5,2X,4HCV =,F7.2,2X,11HR SQUARED =,F6.3,2X,3HR =,F6.3)                          
  311 IF (NCPR.EQ.1) GO TO 108                                          DK213740          
      K6=NRHM*(K-1)-K*(K-3)/2                                           DK213750          
      SSCPR(K6)=SM                                                      DK213760          
  108 CONTINUE                                                          DK213770          
      IF (NRN.LT.NRUN.OR.MTY.EQ.1.OR.MAN.EQ.1) GO TO 152                DK213780          
      WRITE (6,10)                                                      DK213790          
   10 FORMAT(1H1,15X,66HVARIANCE AND COVARIANCE COMPONENT ESTIMATES FROM                  
     1 INDIRECT ANALYSES)                                                                 
      IF (MTY.GT.3) GO TO 154                                           DK213820          
      SDF=EDFF-EDF                                                      DK213830          
      WK=(FLOAT(NCDS)-TOT4(100))/SDF                                    DK213840          
      IF (IBET.EQ.1) WK=(SWT1-TOT4(100))/SDF                            DK213850          
      GO TO 155                                                         DK213860          
  154 SDF=EDFS-EDF                                                      DK213870          
      WK=(FLOAT(NCDS)-TOT4(101))/SDF                                    DK213880          
      IF (IBET.EQ.1) WK=(SWT1-TOT4(101))/SDF                            DK213890          
  155 WRITE (6,1018) NAME,WK,SDF                                        DK213900          
 1018 FORMAT(1H0,10X,32HK FOR RANDOM EFFECTS COMPONENT (,A6,2H)=,F10.4,                   
     1 3X,20HDEGREES OF FREEDOM =,F6.0)                                                   
      WRITE (6,1019)                                                    DK213930          
 1019 FORMAT (1H0/25X,51HSS, CP, MS, MCP, VARIANCE AND COVARIANCE COMPONDK213940          
     1ENTS//1H ,25HJOB ROW COL   RHM     RHM,20X,8HSS OR CP,19X,9HMS OR DK213950          
     2COV,18X,10HCOMPONENTS)                                            DK213960          
      DO 153 I=1,NRHM                                                   DK213970          
      WRITE (6,1004)                                                    DK213980          
      K6=NRHM*(I-1)-I*(I-3)/2-I                                         DK213990          
      DO 153 J=I,NRHM                                                   DK214000          
      K=K6+J                                                            DK214010          
      IF (MTY.GT.3) GO TO 156                                           DK214020          
      SS= FY(K)-SSCPR(K)*EDF                                            DK214030          
      GO TO 157                                                         DK214040          
  156 SS=SAB(K)-SSCPR(K)*EDF                                            DK214050          
  157 SMS=SS/SDF                                                        DK214060          
      IF (I.EQ.J) XP(I)=SMS                                             DK214070          
      SOD=(SMS-SSCPR(K))/WK                                             DK214080          
      WRITE (6,1020) IJOB,I,J,LITY(I),LITY(J),SS,SMS,SOD                DK214090          
 1020 FORMAT (1H ,I3,2I4,2(2X,A6),3F27.8)                               DK214100          
      IF (I.EQ.J.AND.SOD.LT.0.0) SOD=0.0                                DK214110          
      IF (MTY.LT.4) SAB(K)=SOD                                          DK214120          
  153 SSS(K)=SOD                                                        DK214130          
      SOF=WK                                                            DK214140          
      R=SDF                                                             DK214150          
      IF (IAD.EQ.0.OR.MTY.GT.3) GO TO 158                               DK214160          
      L=14                                                              DK214170          
      CALL SVCVC                                                        DK214180          
  158 IF (MTY.LT.4) GO TO 152                                           DK214190          
      SDF=EDFF-EDFS                                                     DK214200          
      F=(FLOAT(NCDS)-TOT4(100))/SDF                                     DK214210          
      IF (IBET.EQ.1) F=(SWT1-TOT4(100))/SDF                             DK214220          
      IF (MTY.LT.6) TOT4(100)=TOT4(103)                                 DK214230          
      WK=(TOT4(101)-TOT4(100))/SDF                                      DK214240          
      WRITE (6,1004)                                                    DK214250          
      WRITE (6,11)  NAM5,WK,F,SDF                                       DK214260          
   11 FORMAT(1H0,10X,10HK VALUES (,A6,10H) ARE: K2=,F10.4,3X,                             
     1 3HK3=,F10.4,3X,20HDEGREES OF FREEDOM =,F6.0)                                       
      WRITE (6,1002) NAME,NAM5                                          DK214290          
 1002 FORMAT(1H0,10X,42HNEGATIVE VARIANCE COMPONENT ESTIMATES FOR ,                       
     1 A6,23HSET TO ZERO TO COMPUTE ,A6,20H VARIANCE COMPONENTS)                          
      WRITE (6,1019)                                                    DK214320          
      DO 159 I=1,NRHM                                                   DK214330          
      WRITE (6,1004)                                                    DK214340          
      K6=NRHM*(I-1)-I*(I-3)/2-I                                         DK214350          
      DO 159 J=I,NRHM                                                   DK214360          
      K=K6+J                                                            DK214370          
      SS=FY(K)-SAB(K)                                                   DK214380          
      SMS=SS/SDF                                                        DK214390          
      IF (I.EQ.J) YP(I)=SMS                                             DK214400          
      SOD=(SMS-SSCPR(K)-WK*SSS(K))/F                                    DK214410          
      WRITE (6,1020) IJOB,I,J,LITY(I),LITY(J),SS,SMS,SOD                DK214420          
      SAB(K)=SSS(K)                                                     DK214430          
      IF (I.EQ.J.AND.SOD.LT.0.0) SOD=0.0                                DK214440          
      FY(K)=SOD                                                         DK214450          
  159 SSS(K)=SOD                                                        DK214460          
      IF (MTY.GT.5) GO TO 160                                           DK214470          
      IF (IAD.EQ.0) GO TO 152                                           DK214480          
      NW(13)=NW(15)                                                     DK214490          
      NR1(13)=NR1(14)+NR1(15)                                           DK214500          
      L=13                                                              DK214510          
      DO 162 I=1,KA                                                     DK214520          
  162 SSS(I)=SSS(I)+SAB(I)                                              DK214530          
      WK=(F*SDF+SOF*R)/(SDF+R)                                          DK214540          
      SDF=(SDF+R)/2.0                                                   DK214550          
      WRITE (6,12)                                                      DK214560          
   12 FORMAT(1H0,40X,14HFROM FULL SIBS)                                                   
      CALL SVCVC                                                        DK214580          
      NW(13)=NW(14)+NR1(14)                                             DK214590          
      NR1(13)=NR1(15)                                                   DK214600          
      DO 163 I=1,KA                                                     DK214610          
      SSS(I)=SSS(I)-SAB(I)                                              DK214620          
  163 SSCPR(I)=SSCPR(I)+SAB(I)                                          DK214630          
      WK=F                                                              DK214640          
      SDF=SDF*2.0-R                                                     DK214650          
      WRITE (6,13)                                                      DK214660          
   13 FORMAT(1H0,40X,23HFROM PATERNAL HALF SIBS)                                          
      CALL SVCVC                                                        DK214680          
      NW(13)=NW(14)+NR1(15)                                             DK214690          
      NR1(13)=NR1(14)                                                   DK214700          
      DO 164 I=1,KA                                                     DK214710          
      SSCPR(I)=SSCPR(I)-SAB(I)+SSS(I)                                   DK214720          
  164 SSS(I)=SAB(I)                                                     DK214730          
      WK=SOF                                                            DK214740          
      SDF=R                                                             DK214750          
      WRITE (6,14)                                                      DK214760          
   14 FORMAT(1H0,40X,23HFROM MATERNAL HALF SIBS)                                          
      CALL SVCVC                                                        DK214780          
      DO 165 I=1,KA                                                     DK214790          
  165 SSCPR(I)=SSCPR(I)-FY(I)                                           DK214800          
      GO TO 152                                                         DK214810          
  160 IF (TOT4(102).EQ.0.0) GO TO 152                                   DK214820          
      L=15                                                              DK214830          
      DO 161 I=1,KA                                                     DK214840          
  161 SSCPR(I)=SSCPR(I)+SAB(I)                                          DK214850          
      WK=F                                                              DK214860          
      CALL SVCVC                                                        DK214870          
      DO 166 I=1,KA                                                     DK214880          
  166 SSCPR(I)=SSCPR(I)-SAB(I)                                          DK214890          
  152 RETURN                                                            DK214900          
      END                                                               DK214910          
