*TEXT                                                                                     
      SUBROUTINE LSCOMB                                                                   
C     SUBROUTINE WHICH CALLS MATINV, COMPUTES AND LISTS CONSTANTS AND   DEC11000          
C     ANOVA.  ALSO CALLS POLYNO AND SVCVC IF NEEDED                     DEC11001          
C     ----------------------------------------------------              DEC11002          
      DIMENSION ARRAY(2000),SSCPR(630),SSS(630),RHM(0250),TOT(106),TOT2(DEC11004          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100)            DEC11005          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),NEQ(6),IPL(13),NSDEC11006          
     1P(13),NND(13),XP(81),YP(41),LP(5),NDC(10),NMI(10),MEN(20),NCL(20),DEC11007          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(40),NCLN(40),LNE(40),NDEN(1DEC11008          
     300),NLIT(40),NMA(30),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(40),DEC11009          
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDEC11010          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),IC(480),DEC11011          
     6EFF1(50),EFF2(50),NOS(200),X(106),NMAC(40)                        DEC11012          
      DIMENSION LSKF(20),LBEG(20),IREJ(20)                              M0211012          
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADEC11013          
     1B4,LITY,TRED,YM,IM,MS,NEQ,IPL,NSP,NND,XP,YP                       DEC11014          
      COMMON /CMBLK2/NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NCLN,NEN,LNE,NBEGDEC11015          
     1,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQC,NREGP,LGTX,JBEG,NDDEC11016          
     2ECX,LITR,NEGY,LNY,LHY,KBEG,NDECY,XM,IC,EFF1,EFF2,NOS,X,NMAC       DEC11017          
      COMMON /CMBLK3/IJOB,NAB,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F,NPR,IRAN,MDEC11018          
     1POP,LIOP,IAD,REP,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,NOM,NON,ML,MLB,NSDEC11019          
     2,NOT,DF,KPUT,SDF,EDF,WK,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,RR       DEC11020          
     3,IN,NSKF,LSKF,LBEG,IREJ                                           M0211020          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DEC11021          
     1L,L7                                                              DEC11022          
      DATA(MUMYM=6HMU-XM )                                              M0111023          
      DATA(IBLK =6H      )                                              M0111023          
      DATA(IBLAB=6HB     )                                              M0111023          
      DATA(IXLAB=6HX     )                                              M0111023          
      DATA (LP(1)=6HLINEAR)                                             M0111023          
      DATA (LP(2)=6HQUAD  )                                             M0111023          
      DATA (LP(3)=6HCUBIC )                                             M0111023          
      DATA (LP(4)=6HQUARD )                                             M0111023          
      DATA (LP(5)=6HQUIN  )                                             M0111023          
C     ----------------------------------------------------              DEC11026          
C     INVERSION AND LISTING OF INVERSE MATRIX                           DEC11027          
C     ----------------------------------------------------              DEC11028          
      NBRC=1                                                            DEC11029          
      NERC=NLHM                                                         DEC11030          
      CALL MATINV (ARRAY,NBRC,NERC,NLHM)                                DEC11031          
   24 WRITE (6,1001) IJOB                                               DEC11032          
      WRITE (6,1004)                                                    DEC11033          
      IF (LIOP.NE.10) GO TO 26                                          DEC11034          
      WRITE (6,1025) (ARRAY(I), I=1,MATX)                               DEC11035          
      GO TO 19                                                          DEC11036          
 1025 FORMAT (7(2X,E15.8))                                              DEC11037          
 1001 FORMAT (1H0,29X,43HLISTING OF INVERSE ELEMENTS FOR PROBLEM NO.,I3)DEC11038          
   26 WRITE (6,1002)                                                    DEC11039          
 1002 FORMAT (1H0,8HROW  COL,15X,21HINDEPENDENT VARIABLES,34X,15HINVERSEDEC11040          
     1 ELEMENT)                                                         DEC11041          
      WRITE (6,1003)                                                    DEC11042          
 1003 FORMAT (1H ,9HCODE CODE,9X,3HROW,22X,6HCOLUMN,16X,41HFIXED POINT FDEC11043          
     1ORMAT  FLOATING POINT FORMAT)                                     DEC11044          
   28 DO 32 I=NBRC,NERC                                                 DEC11045          
      WRITE (6,1004)                                                    DEC11046          
 1004 FORMAT (1H )                                                      DEC11047          
   30 DO 32 J=I,NERC                                                    DEC11048          
      K1=NLHM*(I-1)-I*(I-3)/2+J-I                                       DEC11049          
      WRITE (6,1005) I,J,LAB1(I),LAB2(I),LAB3(I),LAB4(I),LAB1(J),LAB2(J)DEC11050          
     1,LAB3(J),LAB4(J),ARRAY(K1),ARRAY(K1)                              DEC11051          
 1005 FORMAT (1H ,2(I3,2X),2(A6,1X),2A6,2(A6,1X),2A6,2X,F17.8,8X,E15.8) DEC11052          
   32 CONTINUE                                                          DEC11053          
C     ----------------------------------------------------              DEC11054          
C     COMPUTATION AND LISTING OF CONSTANTS                              DEC11055          
C     ----------------------------------------------------              DEC11056          
   19 EDF=DF-FLOAT(NLHM)                                                DEC11057          
      IF (NAB.EQ.0.OR.NAB.EQ.3.AND.NMEA.EQ.0.AND.NNEA.EQ.0) GO TO 55    DEC11058          
      WRITE (6,1006) IJOB                                               DEC11059          
 1006 FORMAT (1H0,18X,45HLISTING OF CONSTANT ESTIMATES FOR PROBLEM NO.,IDEC11060          
     13)                                                                DEC11061          
      WRITE (6,1007)                                                    DEC11062          
 1007 FORMAT (1H0,10H RHM   ROW,43X,18HCONSTANT ESTIMATES/ 1H ,33H NAME DEC11063          
     1 CODE  INDEPENDENT VARIABLE,9X,41HFIXED POINT FORMAT  FLOATING POIDEC11064          
     2NT FORMAT)                                                        DEC11065          
   55 DO 54 K=1,NRHM                                                    DEC11066          
      WRITE (6,1004)                                                    DEC11067          
   34 TREDS=0.0                                                         DEC11068          
      DO 52 I=NBRC,NERC                                                 DEC11069          
      TEMP=0.0                                                          DEC11070          
      DO 44 J=NBRC,NERC                                                 DEC11071          
      IF (I-J.LT.0) GO TO 38                                            DEC11072          
   36 K1=NLHM*(J-1)-J*(J-3)/2+I-J                                       DEC11073          
      GO TO 40                                                          DEC11074          
   38 K1=NLHM*(I-1)-I*(I-3)/2+J-I                                       DEC11075          
   40 K4=(K-1)*NLHM+J                                                   DEC11076          
   44 TEMP=TEMP+RHM(K4)*ARRAY(K1)                                       DEC11077          
      K4=NLHM*(K-1)+I                                                   DEC11078          
   48 TREDS=TREDS+RHM(K4)*TEMP                                          DEC11079          
      TRED(K)=TREDS                                                     DEC11080          
      K1=MATX+(K-1)*NLHM+I                                              DEC11081          
      ARRAY(K1)=TEMP                                                    DEC11082          
      IF (NAB.EQ.0.OR.NAB.EQ.3.AND.NMEA.EQ.0.AND.NNEA.EQ.0) GO TO 52    DEC11083          
      IF ((NAB.EQ.0.OR.NAB.EQ.3).AND.I.EQ.1) TEMP=TEMP+YM(K)            DEC11084          
      WRITE (6,1008) LITY(K),I,LAB1(I),LAB2(I),LAB3(I),LAB4(I),TEMP,TEMPDEC11085          
 1008 FORMAT (1H ,A6,I4,2X,3(A6,1X),A5,2X,F17.8,8X,E15.8)               DEC11086          
   52 CONTINUE                                                          DEC11087          
   54 CONTINUE                                                          DEC11088          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 299                   DEC11089          
      IF (NMEA.NE.0.OR.NNEA.NE.0) GO TO 299                             DEC11090          
      CALL LSMNS                                                        DEC11091          
      IF (KPUT.EQ.1) GO TO 300                                          DEC11092          
  299 IF (NLC.EQ.0) GO TO 53                                            DEC11093          
      WRITE (6,1051)                                                    DEC11094          
 1051 FORMAT (1H0,25X,56HLISTING OF SELECTED LINEAR FUNCTIONS AND STANDADEC11095          
     1RD ERRORS)                                                        DEC11096          
      WRITE (6,1052)                                                    DEC11097          
 1052 FORMAT (1H0,2X,11HDESCRIPTION,2X,84HR(1) C(1) R(2) C(2) R(3) C(3) DEC11098          
     1R(4) C(4) R(5) C(5) R(6) C(6) R(7) C(7) R(8) C(8) ETC.)           DEC11099          
      DO 301 K1=1,NLC                                                   DEC11100          
      READ (5,1050) IA1,IA2,IA3,NERC,(JBEG(J),TOT2(J),J=1,NERC)         DEC11101          
 1050 FORMAT (2A6,A2,I2,12(I2,F3.0),4X/16(I2,F3.0))                     DEC11102          
      WRITE (6,1053) IA1,IA2,IA3,(JBEG(J),TOT2(J),J=1,NERC)             DEC11103          
 1053 FORMAT (1H0,2A6,A2,2X,9(I2,2X,F4.0,2X)/17X,9(I2,2X,F4.0,2X))      DEC11104          
      DO 302 J=1,NLHM                                                   DEC11105          
  302 X(J)=0.                                                           DEC11106          
      DO 303 J=1,NERC                                                   DEC11107          
      K=JBEG(J)                                                         DEC11108          
  303 X(K)=TOT2(J)                                                      DEC11109          
      DO 309 J=1,NLHM                                                   DEC11110          
  309 TOT2(J)=0.                                                        DEC11111          
      NCT=1                                                             DEC11112          
      DO 304 I=1,NRHM                                                   DEC11113          
      YT=YM(I)                                                          DEC11114          
      NR=MATX+(I-1)*NLHM+1                                              DEC11115          
      IF (NCPR.EQ.1) GO TO 305                                          DEC11116          
      K4=I                                                              DEC11117          
      GO TO 306                                                         DEC11118          
  305 K4=NRHM*(I-1)-I*(I-3)/2                                           DEC11119          
  306 WK=(SSCPR(K4)-TRED(I))/EDF                                        DEC11120          
      IF (NAB.GT.2) WK=WK/(RR+1.)                                       DEC11121          
      IF (WK.LT.0.) GO TO 300                                           DEC11122          
  308 CALL CANDSE (AC,ALS,REP,TOT2,X,ARRAY,NR,NLHM,WK,YT,I,SSS,NCT)     DEC11123          
  304 WRITE (6,1054) LITY(I),ALS,REP                                    DEC11124          
  301 CONTINUE                                                          DEC11125          
 1054 FORMAT (1H0,A6,5X,17HLINEAR FUNCTION =,F17.8,4X,16HSTANDARD ERROR DEC11126          
     1=,F17.8)                                                          DEC11127          
C     ----------------------------------------------------              DEC11128          
C     COMPUTATION AND LISTING, IF DESIRED, OF INVERSE ELEMENTS          DEC11129          
C             OF SEGMENTS                                               DEC11130          
C     ----------------------------------------------------              DEC11131          
   53 NERC=0                                                            DEC11132          
      IF (LIOP.LT.3.OR.LIOP.EQ.10) GO TO 221                            DEC11133          
      WRITE (6,1000) IJOB                                               DEC11134          
 1000 FORMAT (1H0,23X,55HLISTING OF INVERSE ELEMENTS OF SEGMENTS FOR PRODEC11135          
     1BLEM NO.,I3)                                                      DEC11136          
      WRITE (6,1002)                                                    DEC11137          
      WRITE (6,1003)                                                    DEC11138          
  221 DO 58 I2=1,NS                                                     DEC11139          
      NBRC=NERC+1                                                       DEC11140          
      NERC=IM(I2)                                                       DEC11141          
      CALL MATINV (ARRAY,NBRC,NERC,NLHM)                                DEC11142          
      IF (LIOP.LT.3.OR.LIOP.EQ.10) GO TO 58                             DEC11143          
      DO 220 I=NBRC,NERC                                                DEC11144          
      WRITE (6,1004)                                                    DEC11145          
      DO 220 J=I,NERC                                                   DEC11146          
      K1=NLHM*(I-1)-I*(I-3)/2+J-I                                       DEC11147          
      WRITE (6,1005) I,J,LAB1(I),LAB2(I),LAB3(I),LAB4(I),LAB1(J),LAB2(J)DEC11148          
     1,LAB3(J),LAB4(J),ARRAY(K1),ARRAY(K1)                              DEC11149          
  220 CONTINUE                                                          DEC11150          
   58 CONTINUE                                                          DEC11151          
C     ----------------------------------------------------              DEC11152          
C     COMPUTATION AND LISTING OF RESIDUAL MATRIX FOR RHM                DEC11153          
C     ----------------------------------------------------              DEC11154          
      IF (NCPR.EQ.0.AND.I309.EQ.0) GO TO 77                             DEC11155          
      WRITE (6,1009)                                                    DEC11156          
 1009 FORMAT (1H0,22X,40HRESIDUAL MATRICES FOR RIGHT HAND MEMBERS// 1H ,DEC11157          
     127H JOB ROW COL   RHM      RHM,11X,14HERROR SS OR CP,7X,15HERROR MDEC11158          
     2S OR COV,5X,11HCORRELATION)                                       DEC11159          
      DO 74 K=1,NRHM                                                    DEC11160          
      WRITE (6,1004)                                                    DEC11161          
      K3=NRHM*(K-1)-K*(K-3)/2                                           DEC11162          
      K1=(K-1)*NLHM                                                     DEC11163          
      DO 74 L=K,NRHM                                                    DEC11164          
      K2=(L-1)*NLHM                                                     DEC11165          
      TR=0.0                                                            DEC11166          
      DO 64 I=1,NLHM                                                    DEC11167          
      K4=K1+I                                                           DEC11168          
      K5=MATX+K2+I                                                      DEC11169          
   64 TR=TR+ARRAY(K5)*RHM(K4)                                           DEC11170          
      J=K3+L-K                                                          DEC11171          
      ESS=SSCPR(J)-TR                                                   DEC11172          
      EMS=ESS/EDF                                                       DEC11173          
      SSCPR(J)=EMS                                                      DEC11174          
      IF (K.EQ.L) GO TO 72                                              DEC11175          
      AK=0.0                                                            DEC11176          
      DO 68 I=1,NLHM                                                    DEC11177          
      K4=K2+I                                                           DEC11178          
      K5=MATX+K4                                                        DEC11179          
   68 AK=AK+ARRAY(K5)*RHM(K4)                                           DEC11180          
      JJ=NRHM*(L-1)-L*(L-3)/2                                           DEC11181          
      SM=(SSCPR(JJ)-AK)/EDF                                             DEC11182          
      F=EMS/SQRT(SSCPR(K3)*SM)                                          DEC11183          
      GO TO 74                                                          DEC11184          
   72 F=1.                                                              DEC11185          
   74 WRITE (6,1010) IJOB,K,L,LITY(K),LITY(L),ESS,EMS,F                 DEC11186          
 1010 FORMAT (1H ,I4,I3,I4,2X,2A8,F22.6, 3X,F20.8,F14.4)                DEC11187          
C     ----------------------------------------------------              DEC11188          
C     CALLS POLYNO SUBROUTINE IF POLYNOMIALS ARE TO BE FITTED           DEC11189          
C     ----------------------------------------------------              DEC11190          
   77 IF (NSME.EQ.0) GO TO 76                                           DEC11191          
      CALL POLYNO                                                       DEC11192          
C     ----------------------------------------------------              DEC11193          
C     COMPUTES AND LISTS SS, MS AND F FOR ANOVA FOR EACH RHM            DEC11194          
C     ----------------------------------------------------              DEC11195          
   76 WRITE (6,1011)                                                    DEC11196          
 1011 FORMAT (1H1,28X,34HLEAST-SQUARES ANALYSIS OF VARIANCE)            DEC11197          
      IF (NAB.EQ.0.OR.NAB.EQ.3) LAB1(1)=MUMYM                           DEC11198          
      DO 108 K=1,NRHM                                                   DEC11199          
      K5=0                                                              DEC11200          
      WRITE (6,1012) LITY(K)                                            DEC11201          
 1012 FORMAT (1H / 42X,A6)                                              DEC11202          
      WRITE (6,1013)                                                    DEC11203          
 1013 FORMAT (1H ,/ 5X,6HSOURCE,8X,4HD.F.,7X,14HSUM OF SQUARES,6X,12HMEADEC11204          
     1N SQUARES,10X,1HF)                                                DEC11205          
      DO 80 I=1,NLHM                                                    DEC11206          
      K1=MATX+(K-1)*NLHM+I                                              DEC11207          
   80 RHM(I)=ARRAY(K1)                                                  DEC11208          
      K6=DF                                                             DEC11209          
   82 WRITE (6,1004)                                                    DEC11210          
      K2=1                                                              DEC11211          
      IF (NCPR.EQ.0.AND.I309.EQ.0) GO TO 84                             DEC11212          
      K1=NRHM*(K-1)-K*(K-3)/2                                           DEC11213          
      SS=SSCPR(K1)*EDF+TRED(K)                                          DEC11214          
      GO TO 86                                                          DEC11215          
   84 SS=SSCPR(K)                                                       DEC11216          
   86 IF (NAB.GT.1) GO TO 22                                            DEC11217          
      WRITE (6,1014) K6,SS                                              DEC11218          
 1014 FORMAT (1H ,5HTOTAL,13X,I5,F20.6,F18.6,F13.3)                     DEC11219          
      GO TO 21                                                          DEC11220          
   22 WRITE (6,1026) K6,SS                                              DEC11221          
 1026 FORMAT (1H ,9HWITHIN SS,9X,I5,F20.6,1X,F17.6,2X,F11.3)            DEC11222          
   21 K6=K6-NLHM                                                        DEC11223          
      EDF=K6                                                            DEC11224          
      R=SS-TRED(K)                                                      DEC11225          
      SD=R                                                              DEC11226          
      SM=R/EDF                                                          DEC11227          
      IF (NAB.GT.2) SM=SM/(RR+1.)                                       DEC11228          
      K3=2                                                              DEC11229          
      RECIP=NLHM                                                        DEC11230          
      RECIP=TRED(K)/RECIP                                               DEC11231          
      F=RECIP/SM                                                        DEC11232          
      WRITE (6,1015) NLHM,TRED(K),RECIP,F                               DEC11233          
 1015 FORMAT (1H ,15HTOTAL REDUCTION,I8,F20.6,F18.6,F13.3)              DEC11234          
      K3=3                                                              DEC11235          
      NERC=0                                                            DEC11236          
      L2=1                                                              DEC11237          
      DO 104 I3=1,NS                                                    DEC11238          
      NBRC=NERC+1                                                       DEC11239          
      NERC=IM(I3)                                                       DEC11240          
      TREDS=0.0                                                         DEC11241          
      DO 212 I=NBRC,NERC                                                DEC11242          
      TEMP=0.0                                                          DEC11243          
      DO 209 J=NBRC,NERC                                                DEC11244          
      IF (I-J.LT.0) GO TO 204                                           DEC11245          
  202 K1=NLHM*(J-1)-J*(J-3)/2+I-J                                       DEC11246          
      GO TO 208                                                         DEC11247          
  204 K1=NLHM*(I-1)-I*(I-3)/2+J-I                                       DEC11248          
  208 K4=J                                                              DEC11249          
  209 TEMP=TEMP+RHM(K4)*ARRAY(K1)                                       DEC11250          
      K4=I                                                              DEC11251          
  210 TREDS=TREDS+RHM(K4)*TEMP                                          DEC11252          
  212 CONTINUE                                                          DEC11253          
   94 K6=NERC-NBRC+1                                                    DEC11254          
      K5=K5+K6                                                          DEC11255          
      IF (K5.GE.IEI.AND.K5.LT.IE) GO TO 98                              DEC11256          
      I2=IBLK                                                           DEC11257          
      GO TO 100                                                         DEC11258          
   98 I2=IXLAB                                                          DEC11259          
      LAB3(K5)=LAB2(K5)                                                 DEC11260          
  100 IF (K5.LT.IE) GO TO 102                                           DEC11261          
      I2=IBLAB                                                          DEC11262          
      LAB1(K5)=LAB2(K5)                                                 DEC11263          
  102 R=K6                                                              DEC11264          
      SS=TREDS/R                                                        DEC11265          
      R=SS/SM                                                           DEC11266          
      WRITE (6,1016) LAB1(K5),I2,LAB3(K5),K6,TREDS,SS,R                 DEC11267          
 1016 FORMAT (1H ,A6,1X,A1,A7,I6,2X,F20.6,F18.6,F13.3)                  DEC11268          
      IF (L2.GT.NSME.OR.IM(I3).NE.NND(L2)) GO TO 104                    DEC11269          
      L2=L2+1                                                           DEC11270          
      SS=0.                                                             DEC11271          
      DO 20 L=1,K6                                                      DEC11272          
      IF (L.GT.5) GO TO 20                                              DEC11273          
      I=K*5+L+(L2-2)*NRHM*5                                             DEC11274          
      R=SSS(I)/SM                                                       DEC11275          
      WRITE (6,1027) LP(L),SSS(I),SSS(I),R                              DEC11276          
 1027 FORMAT (1H ,2X,A6,10X,1H1,4X,F20.6,F18.6,F13.3)                   DEC11277          
      SS=SS+SSS(I)                                                      DEC11278          
   20 CONTINUE                                                          DEC11279          
      IF (K6.LT.6) GO TO 104                                            DEC11280          
      TREDS=TREDS-SS                                                    DEC11281          
      I=K6-5                                                            DEC11282          
      SS=TREDS/FLOAT(I)                                                 DEC11283          
      R=SS/SM                                                           DEC11284          
      WRITE (6,1028) I,TREDS,SS,R                                       DEC11285          
 1028 FORMAT (1H ,2X,8HRESIDUAL,4X,I5,4X,F20.6,F18.6,F13.3)             DEC11286          
  104 CONTINUE                                                          DEC11287          
      K6=EDF                                                            DEC11288          
      IF (NAB.GT.2) SM=SD/EDF                                           DEC11289          
      WRITE (6,1017) K6,SD,SM                                           DEC11290          
 1017 FORMAT (1H ,9HREMAINDER,9X,I5,F20.6,F18.6)                        DEC11291          
      IF (NAB.LT.3) GO TO 310                                           DEC11292          
      F=SM/(RR+1.)                                                      DEC11293          
      WRITE (6,1030) F                                                  DEC11294          
 1030 FORMAT (1H ,33HV(ERROR) = M.S. REMAINDER/(1+R) =,8X,F20.6)        DEC11295          
  310 IF (NCPR.NE.0.OR.I309.NE.0) GO TO 108                             DEC11296          
      K6=NRHM*(K-1)-K*(K-3)/2                                           DEC11297          
      SSCPR(K6)=SM                                                      DEC11298          
  108 CONTINUE                                                          DEC11299          
C     ----------------------------------------------------              DEC11300          
C     COMPUTATION AND LISTING OF K, SS, CP, MS, MCP AND VARIANCE AND    DEC11301          
C     COVARIANCE COMPONENTS FOR ONE SET OF MAIN OR NESTED EFFECTS, IF   DEC11302          
C     IRAN IS NOT EQUAL TO ZERO                                         DEC11303          
C     ----------------------------------------------------              DEC11304          
      IF (IRAN.EQ.0) GO TO 148                                          DEC11305          
      IF ( NS2.EQ.0) GO TO 148                                          DEC11306          
      MN2=NRHM*(NRHM+1)/2                                               DEC11307          
      SDF=0.0                                                           DEC11308          
      AK=0.0                                                            DEC11309          
      I3=0                                                              DEC11310          
      DO 114 J=1,MN2                                                    DEC11311          
  114 SSS(J)=0.0                                                        DEC11312          
      DO 144 JJ=1,NS2                                                   DEC11313          
  116 I3=I3+1                                                           DEC11314          
      IF (IM(I3).NE.MS(JJ)) GO TO 116                                   DEC11315          
      IF (I3.EQ.1) GO TO 120                                            DEC11316          
      MRANK=IM(I3)-IM(I3-1)                                             DEC11317          
      NBRC1=IM(I3-1)+1                                                  DEC11318          
      GO TO 122                                                         DEC11319          
  120 MRANK=IM(I3)                                                      DEC11320          
      NBRC1=1                                                           DEC11321          
  122 NERC1=IM(I3)                                                      DEC11322          
      RANK=MRANK                                                        DEC11323          
      I3=0                                                              DEC11324          
      SOD=0.0                                                           DEC11325          
      SD=0.0                                                            DEC11326          
      DO 130 I=NBRC1,NERC1                                              DEC11327          
      DO 130 J=I,NERC1                                                  DEC11328          
      IF (I-J.LT.0) GO TO 128                                           DEC11329          
      K=NLHM*(I-1)-I*(I-3)/2                                            DEC11330          
      SD=SD+ARRAY(K)                                                    DEC11331          
      GO TO 130                                                         DEC11332          
  128 K=NLHM*(I-1)-I*(I-3)/2+J-I                                        DEC11333          
      SOD=SOD+ARRAY(K)*2.                                               DEC11334          
  130 CONTINUE                                                          DEC11335          
      AK=AK+((SD-(SOD/RANK))/(RANK+1.))*RANK                            DEC11336          
      SDF=SDF+RANK                                                      DEC11337          
      DO 142 K=1,NRHM                                                   DEC11338          
      DO 142 I2=K,NRHM                                                  DEC11339          
      SUM=0.0                                                           DEC11340          
      DO 140 I=NBRC1,NERC1                                              DEC11341          
      N3=MATX+(K-1)*NLHM+I                                              DEC11342          
      DO 140 J=NBRC1,NERC1                                              DEC11343          
      IF (I-J.LE.0) GO TO 136                                           DEC11344          
      L=NLHM*(J-1)-J*(J-3)/2+I-J                                        DEC11345          
      GO TO 138                                                         DEC11346          
  136 L=NLHM*(I-1)-I*(I-3)/2+J-I                                        DEC11347          
  138 N2=MATX+(I2-1)*NLHM+J                                             DEC11348          
  140 SUM=SUM+ARRAY(N3)*ARRAY(L)*ARRAY(N2)                              DEC11349          
      L2=NRHM*(K-1)-K*(K-3)/2+I2-K                                      DEC11350          
  142 SSS(L2)=SSS(L2)+SUM                                               DEC11351          
  144 CONTINUE                                                          DEC11352          
      WK=AK/SDF                                                         DEC11353          
      WRITE (6,1018) WK,SDF                                             DEC11354          
 1018 FORMAT (1H1,10X,32HK FOR RANDOM EFFECTS COMPONENT =,F8.4,3X,20HDEGDEC11355          
     1REES OF FREEDOM =,F3.0)                                           DEC11356          
      WRITE (6,1019)                                                    DEC11357          
 1019 FORMAT (1H0/25X,51HSS, CP, MS, MCP, VARIANCE AND COVARIANCE COMPONDEC11358          
     1ENTS//1H ,25HJOB ROW COL   RHM     RHM,20X,8HSS OR CP,19X,9HMS OR DEC11359          
     2COV,18X,10HCOMPONENTS)                                            DEC11360          
      DO 146 I=1,NRHM                                                   DEC11361          
      WRITE (6,1004)                                                    DEC11362          
      DO 146 J=I,NRHM                                                   DEC11363          
      K=NRHM*(I-1)-I*(I-3)/2+J-I                                        DEC11364          
      SMS=SSS(K)/SDF                                                    DEC11365          
      SOD=(SMS-SSCPR(K))/WK                                             DEC11366          
      WRITE (6,1020) IJOB,I,J,LITY(I),LITY(J),SSS(K),SMS,SOD            DEC11367          
 1020 FORMAT (1H ,I3,2I4,2A8,3F27.8)                                    DEC11368          
  146 SSS(K)=SMS                                                        DEC11369          
  148 IF (IRAN.EQ.0) GO TO 150                                          DEC11370          
C     ----------------------------------------------------              DEC11371          
C     CALLS SVCVC SUBROUTINE TO COMPUTE HERITABILITIES, GENETIC         DEC11372          
C     CORRELATIONS, ETC., IF I309 IS NOT EQUAL TO ZERO                  DEC11373          
C     ----------------------------------------------------              DEC11374          
      IF (I309.EQ.0) GO TO 150                                          DEC11375          
      CALL SVCVC                                                        DEC11376          
  150 CONTINUE                                                          DEC11377          
      RETURN                                                            DEC11378          
  300 WRITE (6,1021)                                                    DEC11379          
 1021 FORMAT (1H0,10X,29HNEGATIVE ERROR SUM OF SQUARES)                 DEC11380          
      RETURN                                                            DEC11381          
      END                                                               DEC11382          
*ENDTEXT                                                                                  
