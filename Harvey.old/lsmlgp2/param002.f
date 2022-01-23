*TEXT                                                                                     
      SUBROUTINE PARAM (                                                  070001          
     1ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LAB4,LITY,TRED,    070002          
     2YM,IM,MS,IPL,NSP,NND,XP,YP,NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NEN,   070003          
     3NCLN,LNE,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NBEG,NEGX,LOGE,LQC,       070004          
     4NREGP,LGTX,JBEG,NDECX,LITR,XM,NEGY,LNY,LHY,KBEG,NDECY,IC,EFF1,      070005          
     5EFF2,NOS,X,NMAC,IED)                                                                
C     SUBROUTINE WHICH READS AND LISTS PARAMETER CARDS, AND SETS UP       070007          
C     ARRAYS FOR LABELING OUTPUT                                          070008          
C     ----------------------------------------------------                070009          
      DIMENSION ARRAY(1),SSCPR(1),SSS(1),RHM(1),TOT(1),TOT2(1),TOT3(1),   070010          
     1LAB1(1),LAB2(1),LAB3(1),LAB4(1),LITY(1),TRED(1),YM(1),IM(1),MS(1),  070011          
     2IPL(1),NSP(1),NND(1),XP(1),YP(1),NDC(1),NMI(1),MEN(1),NCL(1),LME(1  070012          
     3),IBEG(1),IDEN(1),LIT(1),NEN(1),NCLN(1),LNE(1),NDEN(1),NLIT(1),     070013          
     4NMA(1),NMC(1),INT1(1),INT2(1),MSCL(1),NBEG(1),NEGX(1),LOGE(1),      070014          
     5LQC(1),NREGP(1),LGTX(1),JBEG(1),NDECX(1),LITR(1),XM(1),NEGY(1),     070015          
     6LNY(1),LHY(1),KBEG(1),NDECY(1),IC(1),EFF1(1),EFF2(1),NOS(1),X(1),   070016          
     7NMAC(1),IED(1)                                                                      
      DIMENSION NEQ(10)                                                                   
      COMMON /CMBLK1/NEQ                                                  070019          
      COMMON /CMBLK2/MXNLHM,MXNRHM,MXNCAS,MXNCF,MXMN2P,MXNS2,MXNSME,      070020          
     1 MXNMJC,MXNMIC,MXME,MXMECL,MXNE,MXNECL,MX2F,MX2FMS,MXNPR,MXNCD,     070021          
     2 MXK9,MXNOS,MXNED ,MXI2                                             070022          
      COMMON /CMBLK3/IJOB,NAB,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F,NPR,IRAN,M  070023          
     1POP,LIOP,IAD,REP,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,NOM,NON,ML,MLB,NS  070024          
     2,NOT,DF,KPUT,SDF,EDF,WK,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,RR         070025          
     3,IN,NED                                                             070026          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3,   070027          
     1L,L7                                                                070028          
      INTEGER BLANK,CUBIC,QUAD,RGRSN                                      070029          
      DATA (BLANK=6H      )                                               070030          
      DATA (CUBIC=6HCUBIC )                                               070031          
      DATA (MU=6HMU    )                                                  070032          
      DATA (QUAD=6HQUAD  )                                                070033          
      DATA (RGRSN=6HRGRSN )                                               070034          
      DATA (LINEAR=6HLINEAR)                                              070035          
C     --------------------------------------------------                                  
C     READS AND LISTS PROBLEM TITLE CARD                                                  
C     --------------------------------------------------                                  
      READ(5,2050)(NEQ(I),I=1,10)                                                         
 2050 FORMAT(10A8)                                                                        
C     ----------------------------------------------------                070036          
C     READS AND LISTS PARAMETER CARD 0.  NOTE THAT N3F, NSIR, N2FR, N3FR  070037          
C     AND NSPIR ARE NOT USED IN THE PRESENT PROGRAM                       070038          
C     ----------------------------------------------------                070039          
    1 READ  (5,1000) IJOB,NAB,NCD,ICN1,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F,   070040          
     1NPR,NLC,NCPR,IRAN,MPOP,LIOP,IN,NED                                  070041          
 1000 FORMAT(21I3)                                                                        
      IF(MPOP.EQ.0 .OR. IJOB.EQ.1) GO TO 2                                                
      WRITE(6,2052)                                                                       
 2052 FORMAT(1H1)                                                                         
    2 CONTINUE                                                                            
      WRITE (6,1001) IJOB                                                 070043          
 1001 FORMAT (1H1,20X,42HLISTING OF PARAMETER CARDS FOR PROBLEM NO.,I3)   070044          
      WRITE(6,2051)(NEQ(I),I=1,10)                                                        
 2051 FORMAT(1H0,20X,10A8)                                                                
      WRITE (6,1002) IJOB,NAB,NCD,ICN1,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F,   070045          
     1NPR,NLC,NCPR,IRAN,MPOP,LIOP,IN,NED                                  070046          
 1002 FORMAT (1H0, 93HIJOB NAB  NCD  ICN1 NLHM NRHM NMEA NME NNEA  NNE    070047          
     1N2F  NPR  NLC NCPR IRAN MPOP LIOP   IN  NED/1H  I3,I4,17I5)              0          
      IF(MXNLHM-NLHM) 210,211,211                                         070049          
  210 WRITE(6,1102)                                                       070050          
 1102 FORMAT(16H0MXNLHM EXCEEDED)                                         070051          
      MULL=1                                                              070052          
  211 IF(MXNRHM-NRHM) 212,215,215                                         070054          
  212 WRITE(6,1103)                                                       070055          
 1103 FORMAT(16H0MXNRHM EXCEEDED)                                         070056          
      MULL=1                                                              070057          
  215 IF(MXME-NMEA-NME) 216,217,217                                       070064          
  216 WRITE(6,1105)                                                       070065          
 1105 FORMAT(14H0MXME EXCEEDED)                                           070066          
      MULL=1                                                              070067          
  217 IF(MXNE-NNEA-NNE) 218,219,219                                       070069          
  218 WRITE(6,1106)                                                       070070          
 1106 FORMAT(14H0MXNE EXCEEDED)                                           070071          
      MULL=1                                                              070072          
  219 IF(MX2F-N2F) 220,221,221                                            070074          
  220 WRITE(6,1107)                                                       070075          
 1107 FORMAT(14H0MX2F EXCEEDED)                                           070076          
      MULL=1                                                              070077          
  221 IF(MXNPR-NPR) 222,223,223                                           070079          
  222 WRITE(6,1108)                                                       070080          
 1108 FORMAT(15H0MXNPR EXCEEDED)                                          070081          
      MULL=1                                                              070082          
  223 IF(MXNCD-NCD) 224,225,225                                           070084          
  224 WRITE(6,1109)                                                       070085          
 1109 FORMAT(15H0MXNCD EXCEEDED)                                          070086          
      MULL=1                                                              070087          
  225 CONTINUE                                                            070089          
      IF(IN) 200,200,201                                                                  
  200 IN=05                                                                               
C     ----------------------------------------------------                070090          
C     READS AND LISTS PARAMETER CARD(S) 1.                                070091          
C     ----------------------------------------------------                070092          
  201 IF(NED) 202,202,203                                                                 
  203 READ(5,1100)(IED(I),I=1,NED)                                                        
 1100 FORMAT(I14)                                                                         
      DO 204 I=1,NED                                                                      
  204 WRITE(6,1101) I,IED(I)                                                              
 1101 FORMAT(1H0,8HFOR I = ,I2,9H   IED(I)/1H ,10X,I15)                                   
  202 IF(MXNED-NED) 226,227,227                                                           
  226 WRITE(6,1110)                                                                       
 1110 FORMAT(15H0MXNED EXCEEDED)                                                          
      MULL=1                                                                              
  227 CONTINUE                                                                            
C     ----------------------------------------------------                070110          
C     READS CONTROL IDENTIFICATION FROM PARAMETER CARDS IF ABSORPTION     070111          
C                   IS TO OCCUR                                           070112          
C     ----------------------------------------------------                070113          
      K8=1                                                                070114          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 4                       070115          
      IM(1)=1                                                             070116          
      LAB1(1)=MU                                                          070117          
      LAB2(1)=BLANK                                                       070118          
      LAB3(1)=BLANK                                                       070119          
      LAB4(1)=BLANK                                                       070120          
      K8=2                                                                070121          
    4 IF (NAB.NE.2) GO TO 5                                               070122          
      READ  (5,1000) IAD,NCC,(NDC(I),I=1,NCC)                             070123          
      WRITE (6,1003) IAD,NCC,(NDC(I),I=1,NCC)                             070124          
 1003 FORMAT (1H0,36HIAD  NCC  NDC(I) WHERE I=1,2,...NCC./1H ,23(I2,3X))  070125          
      IF(MXNMJC-NCC) 228,229,229                                          070126          
  228 WRITE(6,1111)                                                       070127          
 1111 FORMAT(16H0MXNMJC EXCEEDED)                                         070128          
      MULL=1                                                              070129          
  229 CONTINUE                                                            070131          
    5 IF (NAB.NE.3) GO TO 6                                               070132          
      READ  (5,1004) IAD,REP,NCC,(NDC(I),I=1,NCC)                         070133          
      WRITE (6,1005) IAD,REP,NCC,(NDC(I),I=1,NCC)                         070134          
 1004 FORMAT (I2,F8.6,31I2)                                               070135          
 1005 FORMAT (1H0,43HIAD    REP   NCC  NDC(I) WHERE I=1,2,...NCC/1H ,     070136          
     1I2,2X,F8.6,I4,20I5)                                                 070137          
      IF(MXNMJC-NCC) 230,231,231                                          070138          
  230 WRITE(6,1111)                                                       070139          
      MULL=1                                                              070140          
  231 CONTINUE                                                            070142          
      RR=REP                                                              070143          
      REP=(1.-REP)/REP                                                    070144          
    6 IF (NAB.NE.4) GO TO 13                                              070145          
      READ  (5,1004) IAD,REP,NMJC,(NDC(I),I=1,NMJC),NMIC,(NMI(I),I=1,NMI  070146          
     1C)                                                                  070147          
      WRITE (6,1006) IAD,REP,NMJC,(NDC(I),I=1,NMJC)                       070148          
 1006 FORMAT (1H0,46HIAD    REP   NMJC  NDC(I) WHERE I=1,2,...NMJC. /     070149          
     11H ,I2,2X,F8.6,I4,20I5)                                             070150          
      WRITE (6,1007) NMIC,(NMI(I),I=1,NMIC)                               070151          
 1007 FORMAT (1H0,33HNMIC  NMI(I) WHERE I=1,2,...NMIC. /1H ,1X,15(I2,3X)  070152          
     1)                                                                   070153          
      IF(MXNMJC-NMJC) 232,233,233                                         070154          
  232 WRITE(6,1111)                                                       070155          
      MULL=1                                                              070156          
  233 IF(MXNMIC-NMIC) 234,235,235                                         070158          
  234 WRITE(6,1112)                                                       070159          
 1112 FORMAT(16H0MXNMIC EXCEEDED)                                         070160          
      MULL=1                                                              070161          
  235 CONTINUE                                                            070163          
      RR=REP                                                              070164          
      REP=(1.-REP)/REP                                                    070165          
      LGT=10**NMIC                                                        070166          
C     ----------------------------------------------------                070167          
C     READS AND LISTS PARAMETER CARDS FOR MAIN EFFECTS                    070168          
C     ----------------------------------------------------                070169          
   13 K1=1                                                                070170          
      K2=0                                                                070171          
      IF (NMEA.EQ.0) GO TO 10                                             070172          
      DO 8 I=1,NMEA                                                       070173          
      READ  (5,1008)   MEN(I),LIT(I),K7,MPOL,LME(I),IBEG(I),(NOS(J),J=1,  070174          
     1K7)                                                                 070175          
 1008 FORMAT(I3,A6,I3,2I1,I2,16I4/(20I4))                                 070176          
      WRITE (6,1009) I,MEN(I),LIT(I),K7,MPOL,LME(I),IBEG(I)               070177          
 1009 FORMAT (1H0,6HFOR I=,I4,47H  MEN(I)  LIT(I)  NCL(I)  MPOL  LME(I)   070178          
     1 IBEG(I)/1H ,11X,I3,6X,A6,1X,I3,6X,I2,4X,I2,7X,I2)                  070179          
      NCL(I)=K7                                                           070180          
      WRITE (6,1010) (NOS(J),J=1,K7)                                      070181          
 1010 FORMAT (1H0,72HIDEN(J) WHERE J=K2 BY STEPS OF ONE AS LONG AS J IS   070182          
     1LESS THAN K2+NCL(I)+1/(1H ,I5,5X,I4,3X,20I5))                       070183          
      DO 7 J=1,K7                                                         070184          
      K3=K1+J-1                                                           070185          
      IF(MXMECL-K3) 236,237,237                                           070186          
  236 WRITE(6,1113)                                                       070187          
 1113 FORMAT(16H0MXMECL EXCEEDED)                                         070188          
      MULL=1                                                              070189          
  237 CONTINUE                                                            070191          
      IDEN(K3)=NOS(J)                                                     070192          
    7 NOS(J)=0                                                            070193          
      K1=K1+K7                                                            070194          
    8 K2=K2+K7                                                            070195          
   10 ML=K2                                                               070196          
      NOM=NME+NMEA                                                        070197          
      IF (NME.EQ.0) GO TO 12                                              070198          
      K3=NMEA+1                                                           070199          
      DO 11 I=K3,NOM                                                      070200          
      READ  (5,1008)   MEN(I),LIT(I),K7,MPOL,LME(I),IBEG(I),(NOS(J),J=1,  070201          
     1K7)                                                                 070202          
      WRITE (6,1009) I,MEN(I),LIT(I),K7,MPOL,LME(I),IBEG(I)               070203          
      WRITE (6,1010) (NOS(J),J=1,K7)                                      070204          
      NCL(I)=K7                                                           070205          
      DO 9 J=1,K7                                                         070206          
      K3=K1+J-1                                                           070207          
      IF(MXMECL-K3) 238,239,239                                           070208          
  238 WRITE(6,1113)                                                       070209          
      MULL=1                                                              070210          
  239 CONTINUE                                                            070212          
      IDEN(K3)=NOS(J)                                                     070213          
    9 NOS(J)=0                                                            070214          
      IF (K8.NE.1) GO TO 17                                               070215          
      IM(K8)=NCL(I)-1                                                     070216          
      K5=1                                                                070217          
      GO TO 18                                                            070218          
   17 IM(K8)=NCL(I)-1+IM(K8-1)                                            070219          
      K5=IM(K8-1)+1                                                       070220          
C     ----------------------------------------------------                070221          
C     SETS UP BEGINNING AND ENDING ROW NUMBERS FOR MAIN EFFECTS FOR       070222          
C     WHICH POLYNOMIALS ARE TO BE FITTED                                  070223          
C     ----------------------------------------------------                070224          
   18 IF (MPOL.EQ.0) GO TO 99                                             070225          
      NSME=NSME+1                                                         070226          
      IF(MXNSME-NSME) 240,241,241                                         070227          
  240 WRITE(6,1114)                                                       070228          
 1114 FORMAT(16H0MXNSME EXCEEDED)                                         070229          
      MULL=1                                                              070230          
  241 CONTINUE                                                            070232          
      IPL(NSME)=MEN(I)                                                    070233          
      NSP(NSME)=K5                                                        070234          
      NND(NSME)=IM(K8)                                                    070235          
      K6=NCL(I)                                                           070236          
      DO 100 J=1,K6                                                       070237          
      K=NCAS+J                                                            070238          
      L=K1+J-1                                                            070239          
  100 XP(K)=IDEN(L)                                                       070240          
      NCAS=NCAS+NCL(I)                                                    070241          
      IF(MXNCAS-NCAS) 242,243,243                                         070242          
  242 WRITE(6,1115)                                                       070243          
 1115 FORMAT(16H0MXNCAS EXCEEDED)                                         070244          
      MULL=1                                                              070245          
  243 CONTINUE                                                            070247          
C     ----------------------------------------------------                070248          
C     SETS UP LAB ARRAYS FOR LISTING LATER                                070249          
C     ----------------------------------------------------                070250          
   99 K=K1                                                                070251          
      K6=IM(K8)                                                           070252          
      DO 15 J=K5,K6                                                       070253          
      LAB1(J)=LIT(I)                                                      070254          
      CALL ITOA (IDEN(K),LAB2(J),4)                                       070255          
      LAB3(J)=BLANK                                                       070256          
      LAB4(J)=BLANK                                                       070257          
   15 K=K+1                                                               070258          
      K1=K1+NCL(I)                                                        070259          
      K2=K2+NCL(I)                                                        070260          
   11 K8=K8+1                                                             070261          
      IF(MXMN2P-K8) 244,245,245                                           070262          
  244 WRITE(6,1116)                                                       070263          
 1116 FORMAT(16H0MXMN2P EXCEEDED)                                         070264          
      MULL=1                                                              070265          
  245 CONTINUE                                                            070267          
C     ----------------------------------------------------                070268          
C     READS AND LISTS PARAMETER CARDS FOR NESTED MAIN EFFECTS             070269          
C     ----------------------------------------------------                070270          
   12 K1=1                                                                070271          
      K2=0                                                                070272          
      IF (NNEA.EQ.0) GO TO 20                                             070273          
      DO 19 I=1,NNEA                                                      070274          
      READ  (5,1011)   NMA(I),NMAC(I),NEN(I),NLIT(I),K7,MPOL,LNE(I),NBEG  070275          
     1(I),(NOS(J),J=1,K7)                                                 070276          
 1011 FORMAT(3I3,A6,I3,2I1,I2,14I4,2X/(20I4))                             070277          
      WRITE (6,1012) I,NMA(I),NMAC(I),NEN(I),NLIT(I),K7,MPOL,LNE(I),NBEG  070278          
     1(I)                                                                 070279          
 1012 FORMAT (1H0,6HFOR I=,I4,66H  NMA(I)  NMAC(I)  NEN(I)  NLIT(I)  NCL  070280          
     1N(I)  MPOL  LNE(I)  NBEG(I)/ 1H ,12X,I3,2I9,5X,A6,I5,I9,I6,I8)      070281          
      WRITE (6,1013) K1,K2,K7,(NOS(J),J=1,K7)                             070282          
 1013 FORMAT (1H0,16HNDEN(J) WHERE J=,I4,45H  INCREMENTED BY ONE UNTIL J  070283          
     1 IS GREATER THAN ,I4,1H+,I4/(1H ,19(I4,2X)))                        070284          
      NCLN(I)=K7                                                          070285          
      DO 14 J=1,K7                                                        070286          
      K3=K1+J-1                                                           070287          
      IF(MXNECL-K3) 246,247,247                                           070288          
  246 WRITE(6,1117)                                                       070289          
 1117 FORMAT(16H0MXNECL EXCEEDED)                                         070290          
      MULL=1                                                              070291          
  247 CONTINUE                                                            070293          
      NDEN(K3)=NOS(J)                                                     070294          
   14 NOS(J)=0                                                            070295          
      K1=K1+K7                                                            070296          
   19 K2=K2+K7                                                            070297          
   20 MLB=K2                                                              070298          
      NON=NNEA+NNE                                                        070299          
      IF (NNE.EQ.0) GO TO 25                                              070300          
      K3=NNEA+1                                                           070301          
      DO 24 I=K3,NON                                                      070302          
      READ  (5,1011)   NMA(I),NMAC(I),NEN(I),NLIT(I),K7,MPOL,LNE(I),NBEG  070303          
     1(I),(NOS(J),J=1,K7)                                                 070304          
      WRITE (6,1012) I,NMA(I),NMAC(I),NEN(I),NLIT(I),K7,MPOL,LNE(I),NBEG  070305          
     1(I)                                                                 070306          
      WRITE (6,1013) K1,K2,K7,(NOS(J),J=1,K7)                             070307          
      NCLN(I)=K7                                                          070308          
      DO 22 J=1,K7                                                        070309          
      K3=K1+J-1                                                           070310          
      IF(MXNECL-K3) 248,249,249                                           070311          
  248 WRITE(6,1117)                                                       070312          
      MULL=1                                                              070313          
  249 CONTINUE                                                            070315          
      NDEN(K3)=NOS(J)                                                     070316          
   22 NOS(J)=0                                                            070317          
      IF (K8.NE.1) GO TO 31                                               070318          
      IM(K8)=NCLN(I)-1                                                    070319          
      K5=1                                                                070320          
      GO TO 32                                                            070321          
   31 IM(K8)=NCLN(I)-1+IM(K8-1)                                           070322          
      K5=IM(K8-1)+1                                                       070323          
C     ----------------------------------------------------                070324          
C     SETS UP ARRAYS NEEDED WHEN POLYNOMIALS ARE TO BE FITTED             070325          
C     ----------------------------------------------------                070326          
   32 IF (MPOL.EQ.0) GO TO 98                                             070327          
      NSME=NSME+1                                                         070328          
      IF(MXNSME-NSME) 250,251,251                                         070329          
  250 WRITE(6,1114)                                                       070330          
      MULL=1                                                              070331          
  251 CONTINUE                                                            070333          
      IPL(NSME)=NEN(I)                                                    070334          
      NSP(NSME)=K5                                                        070335          
      NND(NSME)=IM(K8)                                                    070336          
      K6=NCLN(I)                                                          070337          
      DO 101 J=1,K6                                                       070338          
      K=NCAS+J                                                            070339          
      L=K1+J-1                                                            070340          
  101 XP(K)=NDEN(L)                                                       070341          
      NCAS=NCAS+NCLN(I)                                                   070342          
      IF(MXNCAS-NCAS) 252,253,253                                         070343          
  252 WRITE(6,1115)                                                       070344          
      MULL=1                                                              070345          
  253 CONTINUE                                                            070347          
C     ----------------------------------------------------                070348          
C     SETS UP LAB ARRAYS TO IDENTIFY LISTINGS                             070349          
C     ----------------------------------------------------                070350          
   98 K=K1                                                                070351          
      K6=IM(K8)                                                           070352          
      DO 23 J=K5,K6                                                       070353          
      LAB1(J)=NLIT(I)                                                     070354          
      CALL ITOA (NDEN(K),LAB2(J),4)                                       070355          
      LAB3(J)=BLANK                                                       070356          
      LAB4(J)=BLANK                                                       070357          
   23 K=K+1                                                               070358          
      K8=K8+1                                                             070359          
      K1=K1+K7                                                            070360          
   24 K2=K2+K7                                                            070361          
      IF(MXMN2P-K8) 254,255,255                                           070362          
  254 WRITE(6,1116)                                                       070363          
      MULL=1                                                              070364          
  255 CONTINUE                                                            070366          
C     ----------------------------------------------------                070367          
C     READS AND LISTS PARAMETER CARDS FOR TWO-FACTOR INTERACTION EFFECTS  070368          
C     ----------------------------------------------------                070369          
   25 K1=1                                                                070370          
      K2=0                                                                070371          
      NDRI=0                                                              070372          
      IEI=IM(K8-1)+1                                                      070373          
      IF (K8.EQ.1) IEI=1                                                  070374          
      IF (N2F.EQ.0) GO TO 50                                              070375          
      DO 49 I=1,N2F                                                       070376          
      READ  (5,1014)   INT1(I),INT2(I),K7,(NOS(J),J=1,K7)                 070377          
 1014 FORMAT(3I3,10I6,1X/(13I6,2X))                                       070378          
      WRITE (6,1015) I,INT1(I),INT2(I),K7                                 070379          
 1015 FORMAT(1H0,6HFOR I=,I4,25H  INT1(I) INT2(I)  NMC(I)/ 1H ,12X,3(I3,  070380          
     15X))                                                                070381          
      IF (K7.NE.0) WRITE (6,1016) K1,K2,K7,(NOS(J),J=1,K7)                070382          
 1016 FORMAT (1H0,16HMSCL(J) WHERE J=,I4,45H  INCREMENTED BY ONE UNTIL J  070383          
     1 IS GREATER THAN ,I4,1H+,I4/(1H ,I5,I9,12I8))                       070384          
      NMC(I)=K7                                                           070385          
      IF(K7) 266,266,267                                                                  
  267 DO 26 J=1,K7                                                                        
      K3=K1+J-1                                                           070387          
      IF(MX2FMS-K3) 256,257,257                                           070388          
  256 WRITE(6,1118)                                                       070389          
 1118 FORMAT(16H0MX2FMS EXCEEDED)                                         070390          
      MULL=1                                                              070391          
  257 CONTINUE                                                            070393          
      MSCL(K3)=NOS(J)                                                     070394          
   26 NOS(J)=0                                                            070395          
  266 CONTINUE                                                                            
      NSUM=0                                                              070396          
      MSUM=0                                                              070397          
      IF (INT1(I).GT.NOM) GO TO 29                                        070398          
      K6=INT1(I)-1                                                        070399          
      IF (K6.EQ.0) GO TO 28                                               070400          
      DO 27 J=1,K6                                                        070401          
   27 NSUM=NSUM+NCL(J)                                                    070402          
   28 K6=INT1(I)                                                          070403          
      NDF1=NCL(K6)-1                                                      070404          
      GO TO 34                                                            070405          
   29 K6=INT1(I)-NOM                                                      070406          
      NDF1=NCLN(K6)-1                                                     070407          
      K3=NOM+1                                                            070408          
      K4=INT1(I)-1                                                        070409          
      IF (K4.LT.K3) GO TO 34                                              070410          
      DO 30 J=K3,K4                                                       070411          
      K=J-NOM                                                             070412          
   30 NSUM=NSUM+NCLN(K)                                                   070413          
   34 IF (INT2(I).GT.NOM) GO TO 38                                        070414          
      K4=INT2(I)-1                                                        070415          
      IF (K4.EQ.0) GO TO 36                                               070416          
      DO 35 J=1,K4                                                        070417          
   35 MSUM=MSUM+NCL(J)                                                    070418          
   36 K6=INT2(I)                                                          070419          
      NDF2=NCL(K6)-1                                                      070420          
      GO TO 41                                                            070421          
   38 K6=INT2(I)-NOM                                                      070422          
      NDF2=NCLN(K6)-1                                                     070423          
      K3=NOM+1                                                            070424          
      K4=INT2(I)-1                                                        070425          
      IF (K4.LT.K3) GO TO 41                                              070426          
      DO 40 J=K3,K4                                                       070427          
      K=J-NOM                                                             070428          
   40 MSUM=MSUM+NCLN(K)                                                   070429          
   41 IF (K8.NE.1) GO TO 52                                               070430          
      IM(K8)=NDF1*NDF2-NMC(I)                                             070431          
      J=1                                                                 070432          
      GO TO 53                                                            070433          
   52 IM(K8)=IM(K8-1)+NDF1*NDF2-NMC(I)                                    070434          
      J=IM(K8-1)+1                                                        070435          
C     ----------------------------------------------------                070436          
C     SETS UP LAB ARRAYS TO IDENTIFY LISTINGS                             070437          
C     ----------------------------------------------------                070438          
   53 K8=K8+1                                                             070439          
      NDRI=NDRI+NDF1+NDF2+1                                               070440          
      DO 48 K=1,NDF1                                                      070441          
      DO 48 L=1,NDF2                                                      070442          
      K6=NSUM+K                                                           070443          
      K7=MSUM+L                                                           070444          
      IF (NMC(I).EQ.0) GO TO 43                                           070445          
      K5=K5*1000+L                                                        070446          
      K3=0                                                                070447          
      K4=K1-1                                                             070448          
   42 K3=K3+1                                                             070449          
      K4=K4+1                                                             070450          
      IF (K3.GT.NMC(I)) GO TO 43                                          070451          
      IF (K5.NE.MSCL(K4)) GO TO 42                                        070452          
      GO TO 48                                                            070453          
   43 IF (INT1(I).GT.NOM) GO TO 44                                        070454          
      N=INT1(I)                                                           070455          
      LAB1(J)=LIT(N)                                                      070456          
      CALL ITOA (IDEN(K6),LAB3(J),4)                                      070457          
      GO TO 45                                                            070458          
   44 N=INT1(I)-NOM                                                       070459          
      LAB1(J)=NLIT(N)                                                     070460          
      CALL ITOA (NDEN(K6),LAB3(J),4)                                      070461          
   45 IF (INT2(I).GT.NOM) GO TO 46                                        070462          
      N=INT2(I)                                                           070463          
      LAB2(J)=LIT(N)                                                      070464          
      CALL ITOA (IDEN(K7),LAB4(J),4)                                      070465          
      GO TO 47                                                            070466          
   46 N=INT2(I)-NOM                                                       070467          
      LAB2(J)=NLIT(N)                                                     070468          
      CALL ITOA (NDEN(K7),LAB4(J),4)                                      070469          
   47 J=J+1                                                               070470          
   48 CONTINUE                                                            070471          
      K1=K1+NMC(I)                                                        070472          
   49 K2=K2+NMC(I)                                                        070473          
      IF(MXMN2P-K8) 258,259,259                                           070474          
  258 WRITE(6,1116)                                                       070475          
      MULL=1                                                              070476          
  259 CONTINUE                                                            070478          
      IF(MXNOS-IM(K8-1)-NME-NNE-NDRI) 264,265,265                         070479          
  264 WRITE(6,1120)                                                       070480          
 1120 FORMAT(15H0MXNOS EXCEEDED)                                          070481          
      MULL=1                                                              070482          
  265 CONTINUE                                                            070484          
C     ----------------------------------------------------                070485          
C     READS AND LISTS PARAMETER CARDS FOR POOLED REGRESSIONS              070486          
C     ----------------------------------------------------                070487          
   50 IE=IM(K8-1)+1                                                       070488          
      IF (K8.EQ.1) IE=1                                                   070489          
      IF (NPR.EQ.0) GO TO 65                                              070490          
      DO 64 K=1,NPR                                                       070491          
      READ  (5,1017)   NEGX(K),LOGE(K),LQC(K),NREGP(K),LGTX(K),JBEG(K),   070492          
     1NDECX(K),XM(K),LITR(K)                                              070493          
 1017 FORMAT(2I1,I2,I3,I2,I3,I2,F10.5,A6)                                 070494          
      WRITE (6,1018) K,NEGX(K),LOGE(K),LQC(K),NREGP(K),LGTX(K),JBEG(K),   070495          
     1NDECX(K),XM(K),LITR(K)                                              070496          
 1018 FORMAT (1H0,6HFOR K=,I2,72H  NEGX(K) LOGE(K) LQC(K) NREGP(K) LGTX(  070497          
     1K) JBEG(K) NDECX(K) XM(K) LITR(K) /1H ,12X,I2,3I8,I7,2I9,2X,F10.5,  070498          
     22X,A6)                                                              070499          
C     ----------------------------------------------------                070500          
C     SETS UP LAB ARRAYS TO IDENTIFY LISTINGS                             070501          
C     ----------------------------------------------------                070502          
      K3=LQC(K)                                                           070503          
      DO 63 J=1,K3                                                        070504          
      IM(K8)=IM(K8-1)+1                                                   070505          
      IF (K8.EQ.1) IM(K8)=1                                               070506          
      I=IM(K8)                                                            070507          
      K8=K8+1                                                             070508          
      LAB1(I)=RGRSN                                                       070509          
      LAB2(I)=LITR(K)                                                     070510          
      LAB4(I)=BLANK                                                       070511          
      IF (J-2) 60,61,62                                                   070512          
   60 LAB3(I)=LINEAR                                                      070513          
      GO TO 63                                                            070514          
   61 LAB3(I)=QUAD                                                        070515          
      GO TO 63                                                            070516          
   62 LAB3(I)=CUBIC                                                       070517          
   63 I=I+1                                                               070518          
      IF(MXMN2P-K8) 260,261,261                                           070519          
  260 WRITE(6,1116)                                                       070520          
      MULL=1                                                              070521          
  261 CONTINUE                                                            070523          
   64 IF (LOGE(K).EQ.1) XM(K)=ALOG10(XM(K))                               070524          
C     ----------------------------------------------------                070525          
C     READS AND LISTS PARAMETER CARDS FOR RHM                             070526          
C     ----------------------------------------------------                070527          
   65 IF (NRHM.EQ.0) GO TO 67                                             070528          
      DO 66 I=1,NRHM                                                      070529          
      READ  (5,1019)   NEGY(I),LNY(I),LHY(I),KBEG(I),NDECY(I),YM(I),LITY  070530          
     1(I)                                                                 070531          
 1019 FORMAT (2I1,I2,I3,I2,F10.5,A6)                                      070532          
      WRITE (6,1020) I,NEGY(I),LNY(I),LHY(I),KBEG(I),NDECY(I),YM(I),LITY  070533          
     1(I)                                                                 070534          
 1020 FORMAT (1H0,6HFOR I=,I2,54H  NEGY(I) LNY(I) LHY(I) KBEG(I) NDECY(I  070535          
     1) YM(I) LITY(I)/ 1H ,12X,I2,2I7,2I8,2X,F10.5,1X,A6)                 070536          
   66 IF (LNY(I).EQ.1) YM(I)=ALOG10(YM(I))                                070537          
   67 IF (IRAN.EQ.0) GO TO 68                                             070538          
      READ(5,1023) I309,NR1,NW,NS2,(MS(I),I=1,NS2)                        070539          
 1023 FORMAT(3I2,I3,22I3/(25I3))                                          070540          
      WRITE (6,1021) I309,NR1,NW,NS2,(MS(I),I=1,NS2)                      070541          
 1021 FORMAT (1H0,56H  I309 NR1   NW  NS2  MS(1) MS(2) MS(3) MS(4) MS(5)  070542          
     1 ETC./(1H ,23I5))                                                   070543          
      IF(MXNS2-NS2) 262,263,263                                           070544          
  262 WRITE(6,1119)                                                       070545          
 1119 FORMAT(15H0MXNS2 EXCEEDED)                                          070546          
      MULL=1                                                              070547          
  263 CONTINUE                                                            070549          
   68 CONTINUE                                                            070550          
      IF (N2F.EQ.0) GO TO 75                                              070551          
C     ----------------------------------------------------                070552          
C     CHECKS FOR CONSECUTIVE NUMBERING OF ALL MAIN EFFECTS                070553          
C     ----------------------------------------------------                070554          
      J=1                                                                 070555          
      IF (NOM.EQ.0) GO TO 70                                              070556          
      DO 69 I=1,NOM                                                       070557          
      IF (MEN(I).NE.J) GO TO 900                                          070558          
   69 J=J+1                                                               070559          
   70 IF (NON.EQ.0) GO TO 75                                              070560          
      DO 71 I=1,NON                                                       070561          
      IF (NEN(I).NE.J) GO TO 900                                          070562          
   71 J=J+1                                                               070563          
      GO TO 75                                                            070564          
  900 WRITE (6,1022) IJOB                                                 070565          
 1022 FORMAT (1H0,73HCODES FOR EFFECTS NOT CONSECUTIVE---CHECK PARAMETER  070566          
     1 CARDS FOR PROBLEM NO.,I3)                                          070567          
      MULL=1                                                              070568          
   75 CONTINUE                                                            070569          
      NS=K8-1                                                             070570          
      RETURN                                                              070571          
      END                                                                 070572          
*ENDTEXT                                                                                  
