*TEXT                                                                                     
      SUBROUTINE POLYNO                                                                   
C     SUBROUTINE FOR COMPUTING POLYNOMIALS FOR CROSS CLASSIFIED OR      DEC14000          
C     NESTED MAIN EFFECTS                                               DEC14001          
C     ----------------------------------------------------              DEC14002          
      DIMENSION ARRAY(2000),SSCPR(630),SSS(630),RHM(0250),TOT(106),TOT2(DEC14003          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100)            DEC14005          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),NEQ(6),IPL(13),NSDEC14006          
     1P(13),NND(13),XP(81),YP(41),LP(5),NDC(10),NMI(10),MEN(20),NCL(20),DEC14007          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(40),NCLN(40),LNE(40),NDEN(1DEC14008          
     300),NLIT(40),NMA(30),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(40),DEC14009          
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDEC14010          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),IC(480),DEC14011          
     6EFF1(50),EFF2(50),NOS(200),X(106),NMAC(40)                        DEC14012          
      DIMENSION LSKF(20),LBEG(20),IREJ(20)                              M0214012          
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADEC14013          
     1B4,LITY,TRED,YM,IM,MS,NEQ,IPL,NSP,NND,XP,YP                       DEC14014          
      COMMON /CMBLK2/NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NCLN,NEN,LNE,NBEGDEC14015          
     1,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQC,NREGP,LGTX,JBEG,NDDEC14016          
     2ECX,LITR,NEGY,LNY,LHY,KBEG,NDECY,XM,IC,EFF1,EFF2,NOS,X,NMAC       DEC14017          
      COMMON /CMBLK3/IJOB,NAB,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F,NPR,IRAN,MDEC14018          
     1POP,LIOP,IAD,REP,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,NOM,NON,ML,MLB,NSDEC14019          
     2,NOT,DF,KPUT,SDF,EDF,WK,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,RR       DEC14020          
     3,IN,NSKF,LSKF,LBEG,IREJ                                           M0214020          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DEC14021          
     1L,L7                                                              DEC14022          
      DATA (LP(1)=6HLINEAR)                                             M0114023          
      DATA (LP(2)=6HQUAD  )                                             M0114023          
      DATA (LP(3)=6HCUBIC )                                             M0114023          
      DATA (LP(4)=6HQUARD )                                             M0114023          
      DATA (LP(3)=6HCUBIC )                                             M0114023          
      DATA (LP(4)=6HQUARD )                                             M0114023          
      DATA (LP(5)=6HQUIN  )                                             M0114023          
      NCAS=0                                                            DEC14025          
      WRITE (6,1000)                                                    DEC14026          
 1000 FORMAT (1H0,16X,53HLISTING OF POLYNOMIAL REGRESSIONS AND STANDARD DEC14027          
     1ERRORS)                                                           DEC14028          
      WRITE (6,1001)                                                    DEC14029          
 1001 FORMAT (1H0,29H RHM    HIGH ORDER   STANDARD,17X,38HNON-ORTHOGONALDEC14030          
     1 REGRESSION COEFFICIENTS)                                         DEC14031          
      WRITE (6,1002)                                                    DEC14032          
 1002 FORMAT (1H ,27H NAME   REGRESSION    ERROR,9X,2HB0,9X,2HB1,9X,2HB2DEC14033          
     1,9X,2HB3,9X,2HB4,9X,2HB5)                                         DEC14034          
      DO 73 K1=1,NSME                                                   DEC14035          
      I2=NND(K1)-NSP(K1)+1                                              DEC14036          
      DF=FLOAT(I2)                                                      DEC14037          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DEC14038          
C     DEFINING CONSTANTS FOR TRANSFORMATION MATRIX                      DEC14039          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DEC14040          
      SD=DF/(DF+1.)*DF/(DF+1.)                                          DEC14041          
      F=DF/(DF+1.)*1.0/(DF+1.)*(-1.0)                                   DEC14042          
      R=1.0/(DF+1.)*1.0/(DF+1.)                                         DEC14043          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DEC14044          
C     TRANSFORMATION OF Z-1 MATRIX TO OBTAIN MATRIX OF WEIGHTS          DEC14045          
C     WHICH IS STORED IN RHM ARRAY                                      DEC14046          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DEC14047          
      DO 75 K3=1,I2                                                     DEC14048          
      DO 75 K4=K3,I2                                                    DEC14049          
      N2=NSP(K1)+K3-1                                                   DEC14050          
      N3=NSP(K1)+K4-1                                                   DEC14051          
      SUM=0.                                                            DEC14052          
      K7=NSP(K1)                                                        DEC14053          
      K6=NND(K1)                                                        DEC14054          
      DO 76 I=K7,K6                                                     DEC14055          
      DO 76 J=K7,K6                                                     DEC14056          
      IF (I-J.LE.0) GO TO 79                                            DEC14057          
      L=NLHM*(J-1)-J*(J-3)/2+I-J                                        DEC14058          
      GO TO 82                                                          DEC14059          
   79 L=NLHM*(I-1)-I*(I-3)/2+J-I                                        DEC14060          
   82 IF (N2.EQ.J.AND.N3.EQ.I) GO TO 77                                 DEC14061          
      IF (N3.EQ.I.AND.N2.NE.J.OR.N2.EQ.J.AND.N3.NE.I) GO TO 78          DEC14062          
      SUM=SUM+ARRAY(L)*R                                                DEC14063          
      GO TO 76                                                          DEC14064          
   78 SUM=SUM+ARRAY(L)*F                                                DEC14065          
      GO TO 76                                                          DEC14066          
   77 SUM=SUM+ARRAY(L)*SD                                               DEC14067          
   76 CONTINUE                                                          DEC14068          
      L=I2*(K3-1)-K3*(K3-3)/2+K4-K3                                     DEC14069          
   75 RHM(L)=SUM                                                        DEC14070          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DEC14071          
C     COMPUTATION OF MEAN OF P AND DEVIATIONS FROM THIS MEAN            DEC14072          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DEC14073          
      K3=NCAS+I2+1                                                      DEC14074          
      SUM=0.                                                            DEC14075          
      DO 83 I=1,I2                                                      DEC14076          
      K2=NCAS+I                                                         DEC14077          
   83 SUM=SUM+XP(K2)                                                    DEC14078          
      SUM=(SUM+XP(K3))/(DF+1.0)                                         DEC14079          
      PM2=0.                                                            DEC14080          
      PM4=0.                                                            DEC14081          
      DO 130 I=1,I2                                                     DEC14082          
      K2=NCAS+I                                                         DEC14083          
      XP(K2)=XP(K2)-SUM                                                 DEC14084          
      PM2=PM2+XP(K2)*XP(K2)                                             DEC14085          
  130 PM4=PM4+XP(K2)**4                                                 DEC14086          
      XP(K3)=XP(K3)-SUM                                                 DEC14087          
      F=FLOAT(I2+1)                                                     DEC14088          
      PM2=(PM2+XP(K3)*XP(K3))/F                                         DEC14089          
      PM4=(PM4+XP(K3)**4)/F                                             DEC14090          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DEC14091          
C     CALCULATION OF CONSTANT FOR THE LAST LEVEL FOR EACH RHM           DEC14092          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DEC14093          
      DO 87 I=1,NRHM                                                    DEC14094          
      SUM=0.                                                            DEC14095          
      DO 86 J=1,I2                                                      DEC14096          
      K2=MATX+(I-1)*NLHM+NSP(K1)+J-1                                    DEC14097          
   86 SUM=SUM+ARRAY(K2)*(-1.0)                                          DEC14098          
   87 YP(I)=SUM                                                         DEC14099          
      I3=I2*(I2+1)/2                                                    DEC14100          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DEC14101          
C     BEGINNING OF LOOP FOR STEPWISE POLYNOMIAL FITTING                 DEC14102          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DEC14103          
      DO 88 I=1,I2                                                      DEC14104          
      IF (I.GT.5) GO TO 88                                              DEC14105          
      IF (I.GT.1) GO TO 131                                             DEC14106          
      WRITE (6,1003)                                                    DEC14107          
 1003 FORMAT (1H )                                                      DEC14108          
  131 WRITE (6,1004) LAB1(K7),LP(I)                                     DEC14109          
 1004 FORMAT (1H ,39X,A6,2X,A6)                                         DEC14110          
      N2=I*(I+1)/2                                                      DEC14111          
      N3=N2+I*NRHM                                                      DEC14112          
      J=I3+1                                                            DEC14113          
      K2=N3+I3                                                          DEC14114          
      DO 90 K=J,K2                                                      DEC14115          
   90 RHM(K)=0.                                                         DEC14116          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DEC14117          
C     CALCULATION OF WEIGHTED SS AND CP FOR POLYNOMIALS AND RHM         DEC14118          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DEC14119          
      DO 91 L=1,I                                                       DEC14120          
      DO 91 K=L,I                                                       DEC14121          
      DO 89 K6=1,I2                                                     DEC14122          
      JJ=NCAS+K6                                                        DEC14123          
      SMS=XP(K3)                                                        DEC14124          
      SUM=XP(JJ)                                                        DEC14125          
      IF (L.EQ.1) GO TO 140                                             DEC14126          
      DO 93 L2=2,L                                                      DEC14127          
      SUM=SUM*XP(JJ)                                                    DEC14128          
   93 SMS=SMS*XP(K3)                                                    DEC14129          
  140 SUM=SUM-SMS                                                       DEC14130          
      DO 89 K2=1,I2                                                     DEC14131          
      JJ=NCAS+K2                                                        DEC14132          
      SMS=XP(K3)                                                        DEC14133          
      TEMP=XP(JJ)                                                       DEC14134          
      IF (K.EQ.1) GO TO 141                                             DEC14135          
      DO 94 L2=2,K                                                      DEC14136          
      TEMP=TEMP*XP(JJ)                                                  DEC14137          
   94 SMS=SMS*XP(K3)                                                    DEC14138          
  141 TEMP=TEMP-SMS                                                     DEC14139          
      IF (K6-K2.LE.0) GO TO 95                                          DEC14140          
      K11=I2*(K2-1)-K2*(K2-3)/2+K6-K2                                   DEC14141          
      GO TO 96                                                          DEC14142          
   95 K11=I2*(K6-1)-K6*(K6-3)/2+K2-K6                                   DEC14143          
   96 RHM(J)=RHM(J)+SUM*RHM(K11)*TEMP                                   DEC14144          
      IF (L-K.NE.0) GO TO 89                                            DEC14145          
      DO 97 K4=1,NRHM                                                   DEC14146          
      K5=MATX+(K4-1)*NLHM+NSP(K1)+K2-1                                  DEC14147          
      JJ=N2+I*(K4-1)+L+I3                                               DEC14148          
   97 RHM(JJ)=RHM(JJ)+SUM*RHM(K11)*(ARRAY(K5)-YP(K4))                   DEC14149          
   89 CONTINUE                                                          DEC14150          
   91 J=J+1                                                             DEC14151          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DEC14152          
C     INVERSION OF SS AND CP MATRIX                                     DEC14153          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DEC14154          
      DO 110 K=1,I                                                      DEC14155          
      K2=I*(K-1)-K*(K-3)/2+I3                                           DEC14156          
      RECIP=1./RHM(K2)                                                  DEC14157          
      RHM(K2)=-RECIP                                                    DEC14158          
      DO 110 L=1,I                                                      DEC14159          
      K11=I*(L-1)-L*(L-3)/2+I3                                          DEC14160          
      IF (L-K) 111,110,112                                              DEC14161          
  111 K4=K11+K-L                                                        DEC14162          
      GO TO 113                                                         DEC14163          
  112 K4=K2+L-K                                                         DEC14164          
  113 R=RHM(K4)*RECIP                                                   DEC14165          
      DO 114 J=L,I                                                      DEC14166          
      K5=K11+J-L                                                        DEC14167          
      IF (J-K) 115,114,116                                              DEC14168          
  115 K6=I*(J-1)-J*(J-3)/2+K-J+I3                                       DEC14169          
      GO TO 117                                                         DEC14170          
  116 K6=K2+J-K                                                         DEC14171          
  117 RHM(K5)=RHM(K5)-R*RHM(K6)                                         DEC14172          
  114 CONTINUE                                                          DEC14173          
      RHM(K4)=R                                                         DEC14174          
  110 CONTINUE                                                          DEC14175          
      DO 118 J=1,I                                                      DEC14176          
      DO 118 K=J,I                                                      DEC14177          
      K2=I*(J-1)-J*(J-3)/2+K-J+I3                                       DEC14178          
  118 RHM(K2)=-RHM(K2)                                                  DEC14179          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DEC14180          
C     COMPUTATION OF B VALUES, STANDARD ERRORS AND PRINTING OF RESULTS  DEC14181          
C     - - - - - - - - - - - - - - - - - - - - - - -                     DEC14182          
      DO 120 J=1,NRHM                                                   DEC14183          
      DO 122 K=1,I                                                      DEC14184          
      TEMP=0.                                                           DEC14185          
      DO 121 L=1,I                                                      DEC14186          
      IF (K-L.LT.0) GO TO 123                                           DEC14187          
      K2=I*(L-1)-L*(L-3)/2+K-L+I3                                       DEC14188          
      GO TO 124                                                         DEC14189          
  123 K2=I*(K-1)-K*(K-3)/2+L-K+I3                                       DEC14190          
  124 K4=I3+N2+I*(J-1)+L                                                DEC14191          
  121 TEMP=TEMP+RHM(K4)*RHM(K2)                                         DEC14192          
  122 SSS(K)=TEMP                                                       DEC14193          
      K=J*5+I+(K1-1)*NRHM*5                                             DEC14194          
      K5=I3+N2                                                          DEC14195          
      SSS(K)=(SSS(I)*SSS(I))/RHM(K5)                                    DEC14196          
      IF (NCPR.EQ.0) GO TO 125                                          DEC14197          
      K2=NRHM*(J-1)-J*(J-3)/2                                           DEC14198          
      TEMP=SSCPR(K2)                                                    DEC14199          
      GO TO 126                                                         DEC14200          
  125 IF (I309.EQ.1) GO TO 127                                          DEC14201          
      TEMP=(SSCPR(J)-TRED(J))/EDF                                       DEC14202          
      GO TO 126                                                         DEC14203          
  127 TEMP=SSCPR(J)                                                     DEC14204          
  126 IF (NAB.GT.2) TEMP=TEMP/(RR+1.0)                                  DEC14205          
      SD=SQRT(TEMP*RHM(K5))                                             DEC14206          
      IF(NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 132                    DEC14207          
      K=MATX+(J-1)*NLHM+1                                               DEC14208          
      RECIP=ARRAY(K)+YM(J)                                              DEC14209          
      GO TO 133                                                         DEC14210          
  132 K=NLHM+J                                                          DEC14211          
      RECIP=TOT(K)/FLOAT(NCDS)+YM(J)                                    DEC14212          
  133 IF (I.EQ.1) GO TO 120                                             DEC14213          
      RECIP=RECIP-SSS(2)*PM2                                            DEC14214          
      IF (I.LT.4) GO TO 120                                             DEC14215          
      RECIP=RECIP-SSS(4)*PM4                                            DEC14216          
  120 WRITE (6,1005) LITY(J),SSS(I),SD,RECIP,(SSS(K),K=1,I)             DEC14217          
 1005 FORMAT (1H ,A6,2X,F10.5,2X,F10.5,F12.5,1X,5(1X,F10.5))            DEC14218          
   88 CONTINUE                                                          DEC14219          
   73 NCAS=NCAS+I2+1                                                    DEC14220          
      DF=EDF+FLOAT(NLHM)                                                DEC14221          
      RETURN                                                            DEC14222          
      END                                                               DEC14223          
*ENDTEXT                                                                                  
