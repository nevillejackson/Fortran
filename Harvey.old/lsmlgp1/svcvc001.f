*TEXT                                                                                     
      SUBROUTINE SVCVC                                                  DEC13000          
C     SUBROUTINE USED TO COMPUTE VARIANCE AND COVARIANCE COMPONENTS,    DEC13000          
C     HERITABILITIES, GENETIC, PHENOTYPIC AND ENVIRONMENTAL CORRELATIONSDEC13001          
C     AND STANDARD ERRORS                                               DEC13002          
C     ----------------------------------------------------              DEC13003          
      DIMENSION ARRAY(8550),SSCPR(1260),SSS(1260),RHM(3500),            DEC13005          
     1  TOT(136),TOT2(136),TOT3(136),                                   DEC13005          
     2  LAB1(100),LAB2(100),LAB3(100),LAB4(100)                         DEC13005          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),NEQ(6),IPL(13),NSDEC13007          
     1P(13),NND(13),XP(81),YP(41),      NDC(10),NMI(10),MEN(20),NCL(20),DEC13008          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(40),NCLN(40),LNE(40),NDEN(1DEC13009          
     300),NLIT(40),NMA(30),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(40),DEC13010          
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDEC13011          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),IC(480),DEC13012          
     6EFF1(50),EFF2(50),NOS(250),X(136),NMAC(40)                        DEC13013          
      DIMENSION LSKF(20),LBEG(20),IREJ(20)                              M0213013          
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADEC13014          
     1B4,LITY,TRED,YM,IM,MS,NEQ,IPL,NSP,NND,XP,YP                       DEC13015          
      COMMON /CMBLK2/NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NCLN,NEN,LNE,NBEGDEC13016          
     1,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQC,NREGP,LGTX,JBEG,NDDEC13017          
     2ECX,LITR,NEGY,LNY,LHY,KBEG,NDECY,XM,IC,EFF1,EFF2,NOS,X,NMAC       DEC13018          
      COMMON /CMBLK3/IJOB,NAB,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F,NPR,IRAN,MDEC13019          
     1POP,LIOP,IAD,REP,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,NOM,NON,ML,MLB,NSDEC13020          
     2,NOT,DF,KPUT,SDF,EDF,WK,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,RR       DEC13021          
     3,IN,NSKF,LSKF,LBEG,IREJ                                           M0213021          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DEC13022          
     1L,L7                                                              DEC13023          
      W=NW                                                              DEC13024          
      R1=NR1                                                            DEC13025          
      R1=R1/100.                                                        DEC13026          
      W=W/100.                                                          DEC13027          
      SK1=1.                                                            DEC13028          
      C=1./R1                                                           DEC13029          
      D=(1.-W)/R1                                                       DEC13030          
      E=W/R1                                                            DEC13031          
      TN=(SDF+1.)*WK                                                    DEC13032          
      SK3=WK-1.                                                         DEC13033          
      SK4=WK*WK                                                         DEC13034          
      C1=(C*C*2.)*(TN-1.)                                               DEC13035          
      C2=SK4*SDF                                                        DEC13036          
      C3=SK4*EDF                                                        DEC13037          
      SK5=2.*SK3                                                        DEC13038          
      C4=C3*SDF                                                         DEC13039          
      IDENT=0                                                           DEC13040          
      DO 10 K=1,MN2                                                     DEC13041          
   10 SSS(K)=(SSS(K)-SK1*SSCPR(K))/WK                                   DEC13042          
      WRITE (6,1000)                                                    DEC13043          
 1000 FORMAT (1H0/1H ,76HESTIMATES OF HERITABILITIES, GENETIC, PHENOTYPIDEC13044          
     1C, ENVIRONMENTAL CORRELATIONS//1H ,110H JOB ROW COL   RHM    RHM  DEC13045          
     2 HERITABILITY  STANDARD  PHENOTYPIC  ENVIRONMENTAL NEG   VARIANCE DEC13046          
     3OR COV COMPONENTS/ 1H ,28X, 47HOR GENETIC R   ERRORS   CORRELATIONDEC13047          
     4 CORRELATION,11X,4HSIRE,10X,8HHALF-SIB)                           DEC13048          
      DO 60 I=1,NRHM                                                    DEC13049          
      WRITE (6,1001)                                                    DEC13050          
 1001 FORMAT (1H )                                                      DEC13051          
      DO 60 J=I,NRHM                                                    DEC13052          
      IF (I-J) 16,12,50                                                 DEC13053          
   12 K=NRHM*(I-1)-I*(I-3)/2                                            DEC13054          
      H=(SSS(K)*C)/(D*SSS(K)+SSCPR(K))                                  DEC13055          
      T=SSS(K)/(SSS(K)+SSCPR(K))                                        DEC13056          
      T1=1.-T                                                           DEC13057          
      SEH=SQRT((C1*T1*T1*(1.+(SK3*T))*(1.+(SK3*T)))/C4)                 DEC13058          
      RP=1.                                                             DEC13059          
      RE=1.                                                             DEC13060          
      NEG=0                                                             DEC13061          
      GO TO 46                                                          DEC13062          
   16 K=NRHM*(I-1)-I*(I-3)/2                                            DEC13063          
      K1=NRHM*(J-1)-J*(J-3)/2                                           DEC13064          
      K2=K+J-I                                                          DEC13065          
      SUM=SSS(K)*SSS(K1)                                                DEC13066          
      IF (SUM) 18,18,20                                                 DEC13067          
   18 H=0.0                                                             DEC13068          
      SEH=0.                                                            DEC13069          
      GO TO 22                                                          DEC13070          
   20 SUM=SQRT(SUM)                                                     DEC13071          
      H=SSS(K2)/SUM                                                     DEC13072          
   22 DEM1=(SSCPR(K)+(D*SSS(K)))*(SSCPR(K1)+(D*SSS(K1)))                DEC13073          
      IF (DEM1.GT.0.0) GO TO 26                                         DEC13074          
   24 RP=0.0                                                            DEC13075          
      GO TO 28                                                          DEC13076          
   26 RP=(SSCPR(K2)+(D*SSS(K2)))/SQRT(DEM1)                             DEC13077          
   28 RG=H*H                                                            DEC13078          
      IF (H) 30,40,30                                                   DEC13079          
   30 T2=SSS(K1)/(SSS(K1)+SSCPR(K1))                                    DEC13080          
      T3=T*T2                                                           DEC13081          
      IF (D-1.) 32,34,32                                                DEC13082          
   32 RP1=(SSCPR(K2)+SSS(K2))/SQRT((SSCPR(K)+SSS(K))*(SSCPR(K1)+SSS(K1))DEC13083          
     1)                                                                 DEC13084          
      GO TO 36                                                          DEC13085          
   34 RP1=RP                                                            DEC13086          
   36 A1=1.+RG                                                          DEC13087          
      B1=SQRT(T3*RG)                                                    DEC13088          
      C5=(T+T2)/T3                                                      DEC13089          
      D1=(RG*((T-T2)**2))/(2.*T3)                                       DEC13090          
      TK=(1.+(SK3*T))*(1.+(SK3*T2))                                     DEC13091          
      RPB=RP1+(SK3*B1)                                                  DEC13092          
      R=RP1-B1                                                          DEC13093          
      SERG1=((A1*(TK+(RPB*RPB)))-(2.*B1*RPB*(C5+SK5))+D1)/(C2*T3)       DEC13094          
      SERG2=((A1*(T1*(1.-T2)+(R**2)))-(2.*B1*R*(C5-2.))+D1)/(C3+T3)     DEC13095          
      SEH=SQRT(SERG1+SERG2)                                             DEC13096          
   40 DEM2=(SSCPR(K)-(E*SSS(K)))*(SSCPR(K1)-(E*SSS(K1)))                DEC13097          
      IF (DEM2) 42,44,44                                                DEC13098          
   42 DEM2=-DEM2                                                        DEC13099          
      RE=(SSCPR(K2)-(E*SSS(K2)))/(SQRT(DEM2)*(-1.))                     DEC13100          
      NEG=1                                                             DEC13101          
      GO TO 48                                                          DEC13102          
   44 RE=(SSCPR(K2)-(E*SSS(K2)))/SQRT(DEM2)                             DEC13103          
      NEG=0                                                             DEC13104          
      GO TO 48                                                          DEC13105          
   46 K2=K                                                              DEC13106          
   48 WRITE (6,1002) IJOB,I,J,LITY(I),LITY(J),H,SEH,RP,RE,NEG,SSS(K2),SSDEC13107          
     1CPR(K2)                                                           DEC13108          
 1002 FORMAT (1H ,3I4,A8,A7,4F11.3,I9,2F18.8)                           DEC13109          
      GO TO 60                                                          DEC13110          
   50 WRITE (6,1003)                                                    DEC13111          
 1003 FORMAT (1H0,5HERROR)                                              DEC13112          
   60 CONTINUE                                                          DEC13113          
      RETURN                                                            DEC13114          
      END                                                               DEC13115          
*ENDTEXT                                                                                  
