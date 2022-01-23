*TEXT                                                                                     
      SUBROUTINE LSANOV                                                 DK210010          
C     ------------------------------------------------------            DK210020          
C     SUBROUTINE WHICH COMPUTES AND LISTS THE ANOVA AND VARIANCE AND    DK210030          
C     COVARIANCE COMPONENTS FROM INDIRECT ANALYSES                      DK210040          
C     ------------------------------------------------------            DK210050          
      DIMENSION ARRAY(5000),SSCPR(630),SSS(630),RHM(2450),TOT(106),TOT2(DK210090          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100),NS2(15)    DK210100          
      DIMENSION FAB(5000),FY(630),TOT4(106),SAB(630),FSQ(525),TOT5(106) DK210110          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),MZ(10),IPL(13),NSDK210120          
     1P(13),NND(13),XP(81),YP(41),LP(5),NDC(10),NMI(10),MEN(20),NCL(20),DK210130          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(40),NCLN(40),LNE(40),NDEN(1DK210140          
     300),NLIT(40),NMA(30),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(40),DK210150          
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDK210160          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),IC(480),DK210170          
     6EFF1(50),EFF2(50),NOS(300),X(106),NMAC(40),I309(15),NR1(15),NW(15)DK210180          
     7,R1I(100),R2I(100)                                                DK210190          
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADK210200          
     1B4,LITY,TRED,YM,XP,YP,IM,MS,NEG,IPL,NSP,NND,FAB,FY,TOT4,SAB,FSQ,TODK210210          
     2T5,MZ,R1I,R2I                                                     DK210220          
      COMMON /CMBLK2/LIT,LITR,NLIT,XM,EFF1,EFF2,X,NDC,NMI,MEN,NCL,LME,IBDK210230          
     1EG,IDEN,NCLN,NEN,LNE,NBEG,NDEN,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQDK210240          
     2C,NREGP,LGTX,JBEG,NDECX,NEGY,LNY,LHY,KBEG,NDECY,IC,NOS,NMAC       DK210250          
      COMMON /CMBLK3/RR,REP,DF,SDF,EDF,WK,IJOB,NAB,NLHM,NRHM,NMEA,NME,NNDK210260          
     1EA,NNE,N2F,NPR,IRAN,MPOP,LIOP,IAD,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,DK210270          
     2NOM,NON,ML,MLB,NS,NOT,KPUT,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,IN,I30DK210280          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DK210290          
     1L,L7,KA,KB,KC,SOF,NX,KD,MTY,MAN,NRUN,NRN,EDFF,NAME,EDFS,NAM5,IFP,NDK210300          
     2DFA,IFPC,NDFC,NDFAC,NINT                                          DK210310          
      DATA MUMYM,IBLK,IXLAB,IBLAB/6HMU-YM ,6H      ,6HX     ,6HB     /  DK210320          
      DATA LP(1),LP(2),LP(3),LP(4),LP(5)/6HLINEAR,6HQUAD  ,6HCUBIC ,6HQUDK210330          
     1ARD ,6HQUIN  /,IRES,IRMD/8HRESIDUAL,8HREMAINDR/                   DK210340          
      IF (MTY.GT.1.AND.NRN.EQ.NRUN) GO TO 1                             DK210350          
      IF (LIOP.EQ.20) GO TO 2                                           DK210360          
      LAB1(99)=IBLK                                                     DK210370          
      WRITE (6,1011)                                                    DK210380          
 1011 FORMAT (1H1,28X,34HLEAST-SQUARES ANALYSIS OF VARIANCE)            DK210390          
      GO TO 2                                                           DK210400          
    1 KB=2                                                              DK210410          
      LAB1(99)=IRMD                                                     DK210420          
      WRITE (6,1000)                                                    DK210430          
 1000 FORMAT (1H1,24X,"COMBINED LEAST-SQUARES ANALYSIS OF VARIANCE")    DK210440          
    2 IF (NAB.EQ.0.OR.NAB.EQ.3) LAB1(1)=MUMYM                           DK210450          
      DO 108 K=1,NRHM                                                   DK210460          
      K5=0                                                              DK210470          
      IF (KB.EQ.2.OR.LIOP.NE.20) WRITE (6,1012) LITY(K)                 DK210480          
 1012 FORMAT (1H / 42X,A6)                                              DK210490          
      IF (KB.EQ.2) GO TO 3                                              DK210500          
      IF (LIOP.NE.20) WRITE (6,1013)                                    DK210510          
 1013 FORMAT (1H ,/ 5X,6HSOURCE,8X,4HD.F.,7X,14HSUM OF SQUARES,6X,12HMEADK210520          
     1N SQUARES,10X,1HF)                                                DK210530          
      GO TO 4                                                           DK210540          
    3 WRITE (6,1001)                                                    DK210550          
 1001 FORMAT (1H ,/ 5X,6HSOURCE,8X,4HD.F.,7X,14HSUM OF SQUARES,6X,12HMEADK210560          
     1N SQUARES,10X,1HF,7X,"ERROR LINE")                                DK210570          
      IF (NLHM.EQ.0) GO TO 16                                           DK210580          
    4 DO 80 I=1,NLHM                                                    DK210590          
      K1=MATX+(K-1)*NLHM+I                                              DK210600          
   80 RHM(I)=ARRAY(K1)                                                  DK210610          
   16 K6=DF                                                             DK210620          
   82 IF (KB.EQ.2.OR.LIOP.NE.20) WRITE (6,1004)                         DK210630          
 1004 FORMAT (1H )                                                      DK210640          
      IF (NLHM.EQ.0) TRED(K)=0.0                                        DK210650          
      K2=1                                                              DK210660          
      IF (NCPR.EQ.0) GO TO 84                                           DK210670          
      K1=NRHM*(K-1)-K*(K-3)/2                                           DK210680          
      SS=SSCPR(K1)*EDF+TRED(K)                                          DK210690          
      GO TO 86                                                          DK210700          
   84 SS=SSCPR(K)                                                       DK210710          
   86 IF (NAB.GT.1) GO TO 22                                            DK210720          
      IF (LIOP.NE.20) WRITE (6,1014) K6,SS                              DK210730          
 1014 FORMAT (1H ,5HTOTAL,13X,I5,F20.6,F18.6,F13.3)                     DK210740          
      GO TO 21                                                          DK210750          
   22 IF (KB.EQ.2) GO TO 21                                             DK210760          
      IF (LIOP.NE.20) WRITE (6,1026) K6,SS                              DK210770          
 1026 FORMAT (1H ,9HWITHIN SS,9X,I5,F20.6,1X,F17.6,2X,F11.3)            DK210780          
   21 K6=K6-NLHM                                                        DK210790          
      IF(K6.EQ.0) K6=1                                                  DK210800          
      EDF=K6                                                            DK210810          
      R=SS-TRED(K)                                                      DK210820          
      SD=R                                                              DK210830          
      SM=R/EDF                                                          DK210840          
      K3=2                                                              DK210850          
      RECIP=NLHM                                                        DK210860          
      IF (NLHM.EQ.0) RECIP=1.0                                          DK210870          
      RECIP=TRED(K)/RECIP                                               DK210880          
      F=RECIP/SM                                                        DK210890          
      IF (KB.EQ.2) GO TO 5                                              DK210900          
      IF (LIOP.NE.20) WRITE (6,1015) NLHM,TRED(K),RECIP,F               DK210910          
 1015 FORMAT (1H ,15HTOTAL REDUCTION,I8,F20.6,F18.6,F13.3)              DK210920          
      GO TO 6                                                           DK210930          
    5 IF (MTY.EQ.3.OR.MTY.EQ.5.OR.MTY.EQ.7) GO TO 7                     DK210940          
      IF (MTY.NE.2) GO TO 8                                             DK210950          
      TREDS=FY(K1)-SD                                                   DK210960          
      R=EDFF-EDF                                                        DK210970          
      LAB1(100)=NAME                                                    DK210980          
      GO TO 9                                                           DK210990          
    8 TREDS=FY(K1)-SAB(K1)                                              DK211000          
      R=EDFF-EDFS                                                       DK211010          
      LAB1(100)=NAM5                                                    DK211020          
    9 SS=TREDS/R                                                        DK211030          
      K6=R                                                              DK211040          
      R=SS/SM                                                           DK211050          
      IF (MTY.EQ.4) GO TO 15                                            DK211060          
      WRITE (6,1003) LAB1(100),K6,TREDS,SS,R,IRMD                       DK211070          
 1003 FORMAT (1H ,A6, 9X,I6,F22.6,F18.6,F13.3,4X,A8)                    DK211080          
      IF (MTY.EQ.6) GO TO 7                                             DK211090          
      GO TO 6                                                           DK211100          
   15 SOD=SAB(K1)-SD                                                    DK211110          
      F=EDFS-EDF                                                        DK211120          
      SDF=SOD/F                                                         DK211130          
      L2=F                                                              DK211140          
      F=SDF/SM                                                          DK211150          
      R=SS/SDF                                                          DK211160          
      WRITE (6,1003) LAB1(100),K6,TREDS,SS,R,NAME                       DK211170          
      WRITE (6,1003) NAME,L2,SOD,SDF,F,IRMD                             DK211180          
      GO TO 6                                                           DK211190          
    7 L2=IFP*(K-1)+1                                                    DK211200          
      TREDS=FSQ(L2)                                                     DK211210          
      R=NDFA                                                            DK211220          
      SS=TREDS/R                                                        DK211230          
      K6=R                                                              DK211240          
      IF (MTY.NE.3) GO TO 17                                            DK211250          
      LAB1(97)=NAME                                                     DK211260          
      SOD=FY(K1)-SD                                                     DK211270          
      F=EDFF-EDF                                                        DK211280          
      GO TO 18                                                          DK211290          
   17 IF (MTY.NE.6) GO TO 19                                            DK211300          
      LAB1(97)=NAME                                                     DK211310          
      LAB1(100)=LAB1(98)                                                DK211320          
      SOD=SAB(K1)-SD                                                    DK211330          
      F=EDFS-EDF                                                        DK211340          
      GO TO 18                                                          DK211350          
   19 SOD=FY(K1)-SAB(K1)                                                DK211360          
      F=EDFF-EDFS                                                       DK211370          
      LAB1(97)=NAM5                                                     DK211380          
   18 SDF=SOD/F                                                         DK211390          
      L2=F                                                              DK211400          
      F=SDF/SM                                                          DK211410          
      R=SS/SDF                                                          DK211420          
      WRITE (6,1003) LAB1(100),K6,TREDS,SS,R,LAB1(97)                   DK211430          
      IF (IFP.EQ.1) GO TO 23                                            DK211440          
      DO 24 I=2,IFP                                                     DK211450          
      K3=I-1                                                            DK211460          
      K4=IFP*(K-1)+I                                                    DK211470          
      SS=FSQ(K4)                                                        DK211480          
      K6=1                                                              DK211490          
      IF (I.NE.7) GO TO 37                                              DK211500          
      K6=NDFA-5                                                         DK211510          
      SS=SS/FLOAT(K6)                                                   DK211520          
      R=SS/SDF                                                          DK211530          
      WRITE (6,1006) IRES,K6,FSQ(K4),SS,R,LAB1(97)                      DK211540          
 1006 FORMAT (1H ,2X,A8,7X,I2,2X,F22.6,F18.6,F13.3,4X,A8)               DK211550          
      GO TO 24                                                          DK211560          
   37 R=SS/SDF                                                          DK211570          
      WRITE (6,1006) LP(K3),K6,FSQ(K4),SS,R,LAB1(97)                    DK211580          
   24 CONTINUE                                                          DK211590          
   23 IF (MTY.EQ.5) GO TO 25                                            DK211600          
      IF (MTY.EQ.7) GO TO 26                                            DK211610          
      WRITE (6,1003) NAME,L2,SOD,SDF,F,IRMD                             DK211620          
      GO TO 6                                                           DK211630          
   25 TREDS=SAB(K1)-SD                                                  DK211640          
      R=EDFS-EDF                                                        DK211650          
      SS=TREDS/R                                                        DK211660          
      K6=R                                                              DK211670          
      R=SS/SM                                                           DK211680          
      F=SDF/SS                                                          DK211690          
      WRITE (6,1003) NAM5,L2,SOD,SDF,F,NAME                             DK211700          
      WRITE (6,1003) NAME,K6,TREDS,SS,R,IRMD                            DK211710          
      GO TO 6                                                           DK211720          
   26 WK=(SAB(K1)-SD)/(EDFS-EDF)                                        DK211730          
      F=SDF/SM                                                          DK211740          
      WRITE (6,1003) NAM5,L2,SOD,SDF,F,IRMD                             DK211750          
      L2=IFPC*(K-1)+246                                                 DK211760          
      TREDS=FSQ(L2)                                                     DK211770          
      R=NDFC                                                            DK211780          
      SS=TREDS/R                                                        DK211790          
      K6=R                                                              DK211800          
      R=SS/WK                                                           DK211810          
      WRITE (6,1003) LAB1(98),K6,TREDS,SS,R,NAME                        DK211820          
      IF (IFPC.EQ.1) GO TO 48                                           DK211830          
      DO 46 I=2,IFPC                                                    DK211840          
      K3=I-1                                                            DK211850          
      K4=IFPC*(K-1)+I+245                                               DK211860          
      SS=FSQ(K4)                                                        DK211870          
      K6=1                                                              DK211880          
      IF (I.NE.7) GO TO 47                                              DK211890          
      K6=NDFC-5                                                         DK211900          
      SS=SS/FLOAT(K6)                                                   DK211910          
      R=SS/WK                                                           DK211920          
      WRITE (6,1006) IRES,K6,FSQ(K4),SS,R,NAME                          DK211930          
      GO TO 46                                                          DK211940          
   47 R=SS/WK                                                           DK211950          
      WRITE (6,1006) LP(K3),K6,FSQ(K4),SS,R,NAME                        DK211960          
   46 CONTINUE                                                          DK211970          
   48 A2=IXLAB                                                          DK211980          
      SS=FSQ(490+K)/FLOAT(NDFAC)                                        DK211990          
      R=SS/WK                                                           DK212000          
      WRITE (6,1016) LAB1(100),A2,LAB1(98),NDFAC,FSQ(490+K),SS,R,NAME   DK212010          
      TREDS=SAB(K1)-SD                                                  DK212020          
      R=EDFS-EDF                                                        DK212030          
      SS=TREDS/R                                                        DK212040          
      K6=R                                                              DK212050          
      R=SS/SM                                                           DK212060          
      WRITE (6,1003) NAME,K6,TREDS,SS,R,IRMD                            DK212070          
    6 K3=3                                                              DK212080          
      NERC=0                                                            DK212090          
      L2=1                                                              DK212100          
      IF (NLHM.EQ.0) GO TO 27                                           DK212110          
      DO 104 I3=1,NS                                                    DK212120          
      NBRC=NERC+1                                                       DK212130          
      NERC=IM(I3)                                                       DK212140          
      TREDS=0.0                                                         DK212150          
      DO 212 I=NBRC,NERC                                                DK212160          
      TEMP=0.0                                                          DK212170          
      DO 209 J=NBRC,NERC                                                DK212180          
      IF (I-J.LT.0) GO TO 204                                           DK212190          
  202 K1=NLHM*(J-1)-J*(J-3)/2+I-J                                       DK212200          
      GO TO 208                                                         DK212210          
  204 K1=NLHM*(I-1)-I*(I-3)/2+J-I                                       DK212220          
  208 K4=J                                                              DK212230          
  209 TEMP=TEMP+RHM(K4)*ARRAY(K1)                                       DK212240          
      K4=I                                                              DK212250          
  210 TREDS=TREDS+RHM(K4)*TEMP                                          DK212260          
  212 CONTINUE                                                          DK212270          
   94 K6=NERC-NBRC+1                                                    DK212280          
      K5=K5+K6                                                          DK212290          
      IF (K5.GE.IEI.AND.K5.LT.IE) GO TO 98                              DK212300          
      A2=IBLK                                                           DK212310          
      GO TO 100                                                         DK212320          
   98 A2=IXLAB                                                          DK212330          
      LAB3(K5)=LAB2(K5)                                                 DK212340          
  100 IF (K5.LT.IE) GO TO 102                                           DK212350          
      A2=IBLAB                                                          DK212360          
      LAB1(K5)=LAB2(K5)                                                 DK212370          
  102 R=K6                                                              DK212380          
      SS=TREDS/R                                                        DK212390          
      R=SS/SM                                                           DK212400          
      K4=K5-K6+1                                                        DK212410          
      IF (K4.EQ.IEI.AND.KB.EQ.1.AND.MTY.EQ.7) GO TO 52                  DK212420          
      GO TO 53                                                          DK212430          
   52 FSQ(490+K)=TREDS                                                  DK212440          
      NDFAC=K6                                                          DK212450          
   53 IF (KB.EQ.2.OR.LIOP.NE.20) WRITE (6,1016) LAB1(K5),A2,LAB3(K5),K6,DK212460          
     1TREDS,SS,R,LAB1(99)                                               DK212470          
 1016 FORMAT (1H ,A6,1X,A1,1X,A6,I6,F22.6,F18.6,F13.3,4X,A8)            DK212480          
      J=0                                                               DK212490          
      IF (KD.GT.0.AND.I3.EQ.2) GO TO 31                                 DK212500          
      IF (MTY.EQ.6.AND.KB.EQ.1.AND.I3.EQ.1) GO TO 31                    DK212510          
      GO TO 30                                                          DK212520          
   31 IF (MTY.EQ.3.OR.MTY.GT.4) GO TO 32                                DK212530          
      GO TO 30                                                          DK212540          
   32 IFP=1                                                             DK212550          
      IF (IPL(1).NE.1) GO TO 33                                         DK212560          
      J=1                                                               DK212570          
      IF (K6-5) 34,34,35                                                DK212580          
   34 IFP=K6+1                                                          DK212590          
      GO TO 33                                                          DK212600          
   35 IFP=7                                                             DK212610          
   33 I=IFP*(K-1)+1                                                     DK212620          
      FSQ(I)=TREDS                                                      DK212630          
      NDFA=K6                                                           DK212640          
      LAB1(98)=LAB1(K5)                                                 DK212650          
      LAB1(100)=LAB1(K5)                                                DK212660          
   30 IF (KB.EQ.1.AND.MTY.EQ.7.AND.I3.EQ.1) GO TO 40                    DK212670          
      GO TO 41                                                          DK212680          
   40 IFPC=1                                                            DK212690          
      IF (IPL(1).NE.2) GO TO 42                                         DK212700          
      J=2                                                               DK212710          
      IF (K6-5) 43,43,44                                                DK212720          
   43 IFPC=K6+1                                                         DK212730          
      GO TO 42                                                          DK212740          
   44 IFPC=7                                                            DK212750          
   42 I=IFPC*(K-1)+1                                                    DK212760          
      FSQ(245+I)=TREDS                                                  DK212770          
      NDFC=K6                                                           DK212780          
      LAB1(98)=LAB1(K5)                                                 DK212790          
   41 IF (L2.GT.NSME.OR.IM(I3).NE.NND(L2)) GO TO 104                    DK212800          
      L2=L2+1                                                           DK212810          
      SS=0.                                                             DK212820          
      DO 20 L=1,K6                                                      DK212830          
      IF (L.GT.5) GO TO 20                                              DK212840          
      I=K*5+L+(L2-2)*NRHM*5                                             DK212850          
      IF (J.NE.1) GO TO 36                                              DK212860          
      K4=IFP*(K-1)+L+1                                                  DK212870          
      FSQ(K4)=SSS(I)                                                    DK212880          
   36 R=SSS(I)/SM                                                       DK212890          
      IF (KB.EQ.2.OR.LIOP.NE.20) WRITE (6,1027) LP(L),SSS(I),SSS(I),R,LADK212900          
     1B1(99)                                                            DK212910          
 1027 FORMAT (1H ,2X,A6,10X,1H1,4X,F20.6,F18.6,F13.3,4X,A8)             DK212920          
      SS=SS+SSS(I)                                                      DK212930          
      IF (J.NE.2) GO TO 20                                              DK212940          
      K4=IFPC*(K-1)+L+1                                                 DK212950          
      FSQ(245+K4)=SSS(I)                                                DK212960          
   20 CONTINUE                                                          DK212970          
      IF (K6.LT.6) GO TO 104                                            DK212980          
      TREDS=TREDS-SS                                                    DK212990          
      IF (J.NE.2) GO TO 45                                              DK213000          
      K4=IFPC*(K-1)+7                                                   DK213010          
      FSQ(245+K4)=TREDS                                                 DK213020          
   45 I=K6-5                                                            DK213030          
      SS=TREDS/FLOAT(I)                                                 DK213040          
      R=SS/SM                                                           DK213050          
      IF (KB.EQ.2.OR.LIOP.NE.20) WRITE (6,1028) I,TREDS,SS,R,LAB1(99)   DK213060          
 1028 FORMAT (1H ,2X,8HRESIDUAL,4X,I5,4X,F20.6,F18.6,F13.3,4X,A8)       DK213070          
      IF (J.NE.1) GO TO 104                                             DK213080          
      K4=IFP*(K-1)+7                                                    DK213090          
      FSQ(K4)=TREDS                                                     DK213100          
  104 CONTINUE                                                          DK213110          
   27 K6=EDF                                                            DK213120          
      IF (NAB.GT.2) SM=SD/EDF                                           DK213130          
      IF (KB.EQ.2) GO TO 54                                             DK213140          
      IF (LIOP.NE.20) WRITE (6,1017) K6,SD,SM                           DK213150          
 1017 FORMAT (1H ,9HREMAINDER,9X,I5,F20.6,F18.6)                        DK213160          
      GO TO 310                                                         DK213170          
   54 WRITE (6,1005) K6,SD,SM                                           DK213180          
 1005 FORMAT (1H ,9HREMAINDER,7X,I5,F22.6,F18.6)                        DK213190          
  310 IF (NCPR.EQ.1) GO TO 108                                          DK213200          
      K6=NRHM*(K-1)-K*(K-3)/2                                           DK213210          
      SSCPR(K6)=SM                                                      DK213220          
  108 CONTINUE                                                          DK213230          
      IF (NRN.LT.NRUN.OR.MTY.EQ.1.OR.MAN.EQ.1) GO TO 152                DK213240          
      WRITE (6,10)                                                      DK213250          
   10 FORMAT (1H1,15X,"VARIANCE AND COVARIANCE COMPONENT ESTIMATES FROM DK213260          
     1INDIRECT ANALYSES")                                               DK213270          
      IF (MTY.GT.3) GO TO 154                                           DK213280          
      SDF=EDFF-EDF                                                      DK213290          
      WK=(FLOAT(NCDS)-TOT4(100))/SDF                                    DK213300          
      GO TO 155                                                         DK213310          
  154 SDF=EDFS-EDF                                                      DK213320          
      WK=(FLOAT(NCDS)-TOT4(101))/SDF                                    DK213330          
  155 WRITE (6,1018) NAME,WK,SDF                                        DK213340          
 1018 FORMAT (1H0,10X,"K FOR RANDOM EFFECTS COMPONENT (",A6,")=",F8.4,3XDK213350          
     1,"DEGREES OF FREEDOM =",F5.0)                                     DK213360          
      WRITE (6,1019)                                                    DK213370          
 1019 FORMAT (1H0/25X,51HSS, CP, MS, MCP, VARIANCE AND COVARIANCE COMPONDK213380          
     1ENTS//1H ,25HJOB ROW COL   RHM     RHM,20X,8HSS OR CP,19X,9HMS OR DK213390          
     2COV,18X,10HCOMPONENTS)                                            DK213400          
      DO 153 I=1,NRHM                                                   DK213410          
      WRITE (6,1004)                                                    DK213420          
      K6=NRHM*(I-1)-I*(I-3)/2-I                                         DK213430          
      DO 153 J=I,NRHM                                                   DK213440          
      K=K6+J                                                            DK213450          
      IF (MTY.GT.3) GO TO 156                                           DK213460          
      SS= FY(K)-SSCPR(K)*EDF                                            DK213470          
      GO TO 157                                                         DK213480          
  156 SS=SAB(K)-SSCPR(K)*EDF                                            DK213490          
  157 SMS=SS/SDF                                                        DK213500          
      IF (I.EQ.J) XP(I)=SMS                                             DK213510          
      SOD=(SMS-SSCPR(K))/WK                                             DK213520          
      WRITE (6,1020) IJOB,I,J,LITY(I),LITY(J),SS,SMS,SOD                DK213530          
 1020 FORMAT (1H ,I3,2I4,2(2X,A6),3F27.8)                               DK213540          
      IF (MTY.LT.4) SAB(K)=SOD                                          DK213550          
  153 SSS(K)=SOD                                                        DK213560          
      SOF=WK                                                            DK213570          
      R=SDF                                                             DK213580          
      IF (IAD.EQ.0.OR.MTY.GT.3) GO TO 158                               DK213590          
      L=14                                                              DK213600          
      CALL SVCVC                                                        DK213610          
  158 IF (MTY.LT.4) GO TO 152                                           DK213620          
      SDF=EDFF-EDFS                                                     DK213630          
      F=(FLOAT(NCDS)-TOT4(100))/SDF                                     DK213640          
      IF (MTY.LT.6) TOT4(100)=TOT4(103)                                 DK213650          
      WK=(TOT4(101)-TOT4(100))/SDF                                      DK213660          
      WRITE (6,1004)                                                    DK213670          
      WRITE (6,11)  NAM5,WK,F,SDF                                       DK213680          
   11 FORMAT (1H0,10X,"K VALUES (",A6,") ARE: K2=",F8.4,3X,"K3=",F8.4,3XDK213690          
     1,"DEGREES OF FREEDOM =",F5.0)                                     DK213700          
      WRITE (6,1019)                                                    DK213710          
      DO 159 I=1,NRHM                                                   DK213720          
      WRITE (6,1004)                                                    DK213730          
      K6=NRHM*(I-1)-I*(I-3)/2-I                                         DK213740          
      DO 159 J=I,NRHM                                                   DK213750          
      K=K6+J                                                            DK213760          
      SS=FY(K)-SAB(K)                                                   DK213770          
      SMS=SS/SDF                                                        DK213780          
      IF (I.EQ.J) YP(I)=SMS                                             DK213790          
      TEMP=SSS(K)                                                       DK213800          
      IF (I.EQ.J.AND.TEMP.LT.0.0) TEMP=0.0                              DK213810          
      SOD=(SMS-SSCPR(K)-WK*TEMP)/F                                      DK213820          
      WRITE (6,1020) IJOB,I,J,LITY(I),LITY(J),SS,SMS,SOD                DK213830          
      SAB(K)=SSS(K)                                                     DK213840          
      FY(K)=SOD                                                         DK213850          
  159 SSS(K)=SOD                                                        DK213860          
      IF (MTY.GT.5) GO TO 160                                           DK213870          
      IF (IAD.EQ.0) GO TO 152                                           DK213880          
      NW(13)=NW(15)                                                     DK213890          
      NR1(13)=NR1(14)+NR1(15)                                           DK213900          
      L=13                                                              DK213910          
      DO 162 I=1,KA                                                     DK213920          
  162 SSS(I)=SSS(I)+SAB(I)                                              DK213930          
      WK=(F*SDF+SOF*R)/(SDF+R)                                          DK213940          
      SDF=(SDF+R)/2.0                                                   DK213950          
      WRITE (6,12)                                                      DK213960          
   12 FORMAT (1H0,40X,"FROM FULL SIBS")                                 DK213970          
      CALL SVCVC                                                        DK213980          
      NW(13)=NW(14)+NR1(14)                                             DK213990          
      NR1(13)=NR1(15)                                                   DK214000          
      DO 163 I=1,KA                                                     DK214010          
      SSS(I)=SSS(I)-SAB(I)                                              DK214020          
  163 SSCPR(I)=SSCPR(I)+SAB(I)                                          DK214030          
      WK=F                                                              DK214040          
      SDF=SDF*2.0-R                                                     DK214050          
      WRITE (6,13)                                                      DK214060          
   13 FORMAT (1H0,40X,"FROM PATERNAL HALF SIBS")                        DK214070          
      CALL SVCVC                                                        DK214080          
      NW(13)=NW(14)+NR1(15)                                             DK214090          
      NR1(13)=NR1(14)                                                   DK214100          
      DO 164 I=1,KA                                                     DK214110          
      SSCPR(I)=SSCPR(I)-SAB(I)+SSS(I)                                   DK214120          
  164 SSS(I)=SAB(I)                                                     DK214130          
      WK=SOF                                                            DK214140          
      SDF=R                                                             DK214150          
      WRITE (6,14)                                                      DK214160          
   14 FORMAT (1H0,40X,"FROM MATERNAL HALF SIBS")                        DK214170          
      CALL SVCVC                                                        DK214180          
      DO 165 I=1,KA                                                     DK214190          
  165 SSCPR(I)=SSCPR(I)-FY(I)                                           DK214200          
      GO TO 152                                                         DK214210          
  160 IF (TOT4(102).EQ.0.0) GO TO 152                                   DK214220          
      L=15                                                              DK214230          
      DO 161 I=1,KA                                                     DK214240          
  161 SSCPR(I)=SSCPR(I)+SAB(I)                                          DK214250          
      WK=F                                                              DK214260          
      CALL SVCVC                                                        DK214270          
      DO 166 I=1,KA                                                     DK214280          
  166 SSCPR(I)=SSCPR(I)-SAB(I)                                          DK214290          
  152 RETURN                                                            DK214300          
      END                                                               DK214310          
*ENDTEXT                                                                                  
