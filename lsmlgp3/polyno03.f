*TEXT                                                                                     
      SUBROUTINE POLYNO(                                                  170001          
     1ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LAB4,LITY,TRED,    170002          
     2YM,IM,MS,IPL,NSP,NND,XP,YP,NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NEN,   170003          
     3NCLN,LNE,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NBEG,NEGX,LOGE,LQC,       170004          
     4NREGP,LGTX,JBEG,NDECX,LITR,XM,NEGY,LNY,LHY,KBEG,NDECY,IC,EFF1,      170005          
     5EFF2,NOS,X,NMAC,IED)                                                                
C     SUBROUTINE FOR COMPUTING POLYNOMIALS FOR CROSS CLASSIFIED OR        170007          
C     NESTED MAIN EFFECTS                                                 170008          
C     ----------------------------------------------------                170009          
      DIMENSION ARRAY(1),SSCPR(1),SSS(1),RHM(1),TOT(1),TOT2(1),TOT3(1),   170010          
     1LAB1(1),LAB2(1),LAB3(1),LAB4(1),LITY(1),TRED(1),YM(1),IM(1),MS(1),  170011          
     2IPL(1),NSP(1),NND(1),XP(1),YP(1),NDC(1),NMI(1),MEN(1),NCL(1),LME(1  170012          
     3),IBEG(1),IDEN(1),LIT(1),NEN(1),NCLN(1),LNE(1),NDEN(1),NLIT(1),     170013          
     4NMA(1),NMC(1),INT1(1),INT2(1),MSCL(1),NBEG(1),NEGX(1),LOGE(1),      170014          
     5LQC(1),NREGP(1),LGTX(1),JBEG(1),NDECX(1),LITR(1),XM(1),NEGY(1),     170015          
     6LNY(1),LHY(1),KBEG(1),NDECY(1),IC(1),EFF1(1),EFF2(1),NOS(1),X(1),   170016          
     7NMAC(1),IED(1)                                                                      
      DIMENSION NEQ(10),LP(5)                                                             
      COMMON /CMBLK1/NEQ                                                  170019          
      COMMON /CMBLK2/MXNLHM,MXNRHM,MXNCAS,MXNCF,MXMN2P,MXNS2,MXNSME,      170020          
     1 MXNMJC,MXNMIC,MXME,MXMECL,MXNE,MXNECL,MX2F,MX2FMS,MXNPR,MXNCD,     170021          
     2 MXK9,MXNOS,MXNED ,MXI2                                             170022          
      COMMON /CMBLK3/IJOB,NAB,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F,NPR,IRAN,M  170023          
     1POP,LIOP,IAD,REP,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,NOM,NON,ML,MLB,NS  170024          
     2,NOT,DF,KPUT,SDF,EDF,WK,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,RR         170025          
     3,IN,NED,IBOP                                                                        
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3,   170027          
     1L,L7                                                                170028          
      DIMENSION PM(5)                                                                     
      DATA (LP(1)=6HLINEAR)                                               170029          
      DATA (LP(2)=6HQUAD  )                                               170030          
      DATA (LP(3)=6HCUBIC )                                               170031          
      DATA (LP(4)=6HQUARD )                                               170032          
      DATA (LP(5)=6HQUIN  )                                               170033          
      NCAS=0                                                              170034          
      WRITE (6,1000)                                                      170035          
 1000 FORMAT (1H1,16X,53HLISTING OF POLYNOMIAL REGRESSIONS AND STANDARD   170036          
     1ERRORS)                                                             170037          
      WRITE(6,5000)(NEQ(I),I=1,10)                                                        
 5000 FORMAT(1H0,20X,10A8)                                                                
      WRITE (6,1001)                                                      170038          
 1001 FORMAT (1H0,29H RHM    HIGH ORDER   STANDARD,17X,38HNON-ORTHOGONAL  170039          
     1 REGRESSION COEFFICIENTS)                                           170040          
      WRITE (6,1002)                                                      170041          
 1002 FORMAT (1H ,27H NAME   REGRESSION    ERROR,9X,2HB0,9X,2HB1,9X,2HB2  170042          
     1,9X,2HB3,9X,2HB4,9X,2HB5)                                           170043          
      DO 73 K1=1,NSME                                                     170044          
      I2=NND(K1)-NSP(K1)+1                                                170045          
      IF(MXI2-I2) 10,11,11                                                170046          
   10 WRITE(6,1010)                                                       170047          
 1010 FORMAT(14H0MXI2 EXCEEDED)                                           170048          
      MULL=1                                                              170049          
      RETURN                                                              170050          
   11 CONTINUE                                                            170051          
      DF=FLOAT(I2)                                                        170052          
      IF(IBOP)2000,2000,2001                                                              
 2001 WRITE(IBOP)K1,NND(K1),NSP(K1),I2,DF                                                 
 2000 CONTINUE                                                                            
C     - - - - - - - - - - - - - - - - - - - - - - -                       170053          
C     DEFINING CONSTANTS FOR TRANSFORMATION MATRIX                        170054          
C     - - - - - - - - - - - - - - - - - - - - - - -                       170055          
      SD=DF/(DF+1.)*DF/(DF+1.)                                            170056          
      F=DF/(DF+1.)*1.0/(DF+1.)*(-1.0)                                     170057          
      R=1.0/(DF+1.)*1.0/(DF+1.)                                           170058          
C     - - - - - - - - - - - - - - - - - - - - - - -                       170059          
C     TRANSFORMATION OF Z-1 MATRIX TO OBTAIN MATRIX OF WEIGHTS            170060          
C     WHICH IS STORED IN RHM ARRAY                                        170061          
C     - - - - - - - - - - - - - - - - - - - - - - -                       170062          
      DO 75 K3=1,I2                                                       170063          
      DO 75 K4=K3,I2                                                      170064          
      N2=NSP(K1)+K3-1                                                     170065          
      N3=NSP(K1)+K4-1                                                     170066          
      SUM=0.                                                              170067          
      K7=NSP(K1)                                                          170068          
      K6=NND(K1)                                                          170069          
      DO 76 I=K7,K6                                                       170070          
      DO 76 J=K7,K6                                                       170071          
      IF (I-J.LE.0) GO TO 79                                              170072          
      L=NLHM*(J-1)-J*(J-3)/2+I-J                                          170073          
      GO TO 82                                                            170074          
   79 L=NLHM*(I-1)-I*(I-3)/2+J-I                                          170075          
   82 IF (N2.EQ.J.AND.N3.EQ.I) GO TO 77                                   170076          
      IF (N3.EQ.I.AND.N2.NE.J.OR.N2.EQ.J.AND.N3.NE.I) GO TO 78            170077          
      SUM=SUM+ARRAY(L)*R                                                  170078          
      GO TO 76                                                            170079          
   78 SUM=SUM+ARRAY(L)*F                                                  170080          
      GO TO 76                                                            170081          
   77 SUM=SUM+ARRAY(L)*SD                                                 170082          
   76 CONTINUE                                                            170083          
      L=I2*(K3-1)-K3*(K3-3)/2+K4-K3                                       170084          
   75 RHM(L)=SUM                                                          170085          
C     - - - - - - - - - - - - - - - - - - - - - - -                       170086          
C     COMPUTATION OF MEAN OF P AND DEVIATIONS FROM THIS MEAN              170087          
C     - - - - - - - - - - - - - - - - - - - - - - -                       170088          
      K3=NCAS+I2+1                                                        170089          
      SUM=0.                                                              170090          
      DO 83 I=1,I2                                                        170091          
      K2=NCAS+I                                                           170092          
   83 SUM=SUM+XP(K2)                                                      170093          
      SUM=(SUM+XP(K3))/(DF+1.0)                                           170094          
      F=FLOAT(I2+1)                                                                       
      XP(K3)=XP(K3)-SUM                                                                   
      DO 80 I=2,5                                                                         
   80 PM(I)=XP(K3)**I/F                                                                   
      DO 130 I=1,I2                                                       170097          
      K2=NCAS+I                                                           170098          
      XP(K2)=XP(K2)-SUM                                                   170099          
      DO 130 II=2,5                                                                       
  130 PM(II)=PM(II)+(XP(K2)**II)/F                                                        
C     - - - - - - - - - - - - - - - - - - - - - - -                       170106          
C     CALCULATION OF CONSTANT FOR THE LAST LEVEL FOR EACH RHM             170107          
C     - - - - - - - - - - - - - - - - - - - - - - -                       170108          
      DO 87 I=1,NRHM                                                      170109          
      SUM=0.                                                              170110          
      DO 86 J=1,I2                                                        170111          
      K2=MATX+(I-1)*NLHM+NSP(K1)+J-1                                      170112          
   86 SUM=SUM+ARRAY(K2)*(-1.0)                                            170113          
   87 YP(I)=SUM                                                           170114          
      I3=I2*(I2+1)/2                                                      170115          
C     - - - - - - - - - - - - - - - - - - - - - - -                       170116          
C     BEGINNING OF LOOP FOR STEPWISE POLYNOMIAL FITTING                   170117          
C     - - - - - - - - - - - - - - - - - - - - - - -                       170118          
      DO 88 I=1,I2                                                        170119          
      IF (I.GT.5) GO TO 88                                                170120          
      IF (I.GT.1) GO TO 131                                               170121          
      WRITE (6,1003)                                                      170122          
 1003 FORMAT (1H )                                                        170123          
  131 WRITE (6,1004) LAB1(K7),LP(I)                                       170124          
 1004 FORMAT (1H ,39X,A6,2X,A6)                                           170125          
      IF(IBOP)2002,2002,2003                                                              
 2003 WRITE(IBOP)K7,I,LAB1(K7),LP(I)                                                      
 2002 CONTINUE                                                                            
      N2=I*(I+1)/2                                                        170126          
      N3=N2+I*NRHM                                                        170127          
      J=I3+1                                                              170128          
      K2=N3+I3                                                            170129          
      DO 90 K=J,K2                                                        170130          
   90 RHM(K)=0.                                                           170131          
C     - - - - - - - - - - - - - - - - - - - - - - -                       170132          
C     CALCULATION OF WEIGHTED SS AND CP FOR POLYNOMIALS AND RHM           170133          
C     - - - - - - - - - - - - - - - - - - - - - - -                       170134          
      DO 91 L=1,I                                                         170135          
      DO 91 K=L,I                                                         170136          
      DO 89 K6=1,I2                                                       170137          
      JJ=NCAS+K6                                                          170138          
      SMS=XP(K3)                                                          170139          
      SUM=XP(JJ)                                                          170140          
      IF (L.EQ.1) GO TO 140                                               170141          
      DO 93 L2=2,L                                                        170142          
      SUM=SUM*XP(JJ)                                                      170143          
   93 SMS=SMS*XP(K3)                                                      170144          
  140 SUM=SUM-SMS                                                         170145          
      DO 89 K2=1,I2                                                       170146          
      JJ=NCAS+K2                                                          170147          
      SMS=XP(K3)                                                          170148          
      TEMP=XP(JJ)                                                         170149          
      IF (K.EQ.1) GO TO 141                                               170150          
      DO 94 L2=2,K                                                        170151          
      TEMP=TEMP*XP(JJ)                                                    170152          
   94 SMS=SMS*XP(K3)                                                      170153          
  141 TEMP=TEMP-SMS                                                       170154          
      IF (K6-K2.LE.0) GO TO 95                                            170155          
      K11=I2*(K2-1)-K2*(K2-3)/2+K6-K2                                     170156          
      GO TO 96                                                            170157          
   95 K11=I2*(K6-1)-K6*(K6-3)/2+K2-K6                                     170158          
   96 RHM(J)=RHM(J)+SUM*RHM(K11)*TEMP                                     170159          
      IF (L-K.NE.0) GO TO 89                                              170160          
      DO 97 K4=1,NRHM                                                     170161          
      K5=MATX+(K4-1)*NLHM+NSP(K1)+K2-1                                    170162          
      JJ=N2+I*(K4-1)+L+I3                                                 170163          
   97 RHM(JJ)=RHM(JJ)+SUM*RHM(K11)*(ARRAY(K5)-YP(K4))                     170164          
   89 CONTINUE                                                            170165          
   91 J=J+1                                                               170166          
C     - - - - - - - - - - - - - - - - - - - - - - -                       170167          
C     INVERSION OF SS AND CP MATRIX                                       170168          
C     - - - - - - - - - - - - - - - - - - - - - - -                       170169          
      DO 110 K=1,I                                                        170170          
      K2=I*(K-1)-K*(K-3)/2+I3                                             170171          
      RECIP=1./RHM(K2)                                                    170172          
      RHM(K2)=-RECIP                                                      170173          
      DO 110 L=1,I                                                        170174          
      K11=I*(L-1)-L*(L-3)/2+I3                                            170175          
      IF (L-K) 111,110,112                                                170176          
  111 K4=K11+K-L                                                          170177          
      GO TO 113                                                           170178          
  112 K4=K2+L-K                                                           170179          
  113 R=RHM(K4)*RECIP                                                     170180          
      DO 114 J=L,I                                                        170181          
      K5=K11+J-L                                                          170182          
      IF (J-K) 115,114,116                                                170183          
  115 K6=I*(J-1)-J*(J-3)/2+K-J+I3                                         170184          
      GO TO 117                                                           170185          
  116 K6=K2+J-K                                                           170186          
  117 RHM(K5)=RHM(K5)-R*RHM(K6)                                           170187          
  114 CONTINUE                                                            170188          
      RHM(K4)=R                                                           170189          
  110 CONTINUE                                                            170190          
      DO 118 J=1,I                                                        170191          
      DO 118 K=J,I                                                        170192          
      K2=I*(J-1)-J*(J-3)/2+K-J+I3                                         170193          
  118 RHM(K2)=-RHM(K2)                                                    170194          
C     - - - - - - - - - - - - - - - - - - - - - - -                       170195          
C     COMPUTATION OF B VALUES, STANDARD ERRORS AND PRINTING OF RESULTS    170196          
C     - - - - - - - - - - - - - - - - - - - - - - -                       170197          
      DO 2120 J=1,NRHM                                                                    
      DO 122 K=1,I                                                        170199          
      TEMP=0.                                                             170200          
      DO 121 L=1,I                                                        170201          
      IF (K-L.LT.0) GO TO 123                                             170202          
      K2=I*(L-1)-L*(L-3)/2+K-L+I3                                         170203          
      GO TO 124                                                           170204          
  123 K2=I*(K-1)-K*(K-3)/2+L-K+I3                                         170205          
  124 K4=I3+N2+I*(J-1)+L                                                  170206          
  121 TEMP=TEMP+RHM(K4)*RHM(K2)                                           170207          
  122 SSS(K)=TEMP                                                         170208          
      K=J*5+I+(K1-1)*NRHM*5                                               170209          
      K5=I3+N2                                                            170210          
      SSS(K)=(SSS(I)*SSS(I))/RHM(K5)                                      170211          
      IF (NCPR.EQ.0) GO TO 125                                            170212          
      K2=NRHM*(J-1)-J*(J-3)/2                                             170213          
      TEMP=SSCPR(K2)                                                      170214          
      GO TO 126                                                           170215          
  125 IF(NCPR.EQ.1.OR.I309.EQ.1) GO TO 127                                                
      TEMP=(SSCPR(J)-TRED(J))/EDF                                         170217          
      GO TO 126                                                           170218          
  127 TEMP=SSCPR(J)                                                       170219          
  126 IF (NAB.GT.2) TEMP=TEMP/(RR+1.0)                                    170220          
      SD=0.0                                                                              
      IF(TEMP*RHM(K5).LE.0.0) GO TO 128                                                   
      SD=SQRT(TEMP*RHM(K5))                                                               
  128 IF(NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 132                                      
      K=MATX+(J-1)*NLHM+1                                                 170223          
      RECIP=ARRAY(K)+YM(J)                                                170224          
      GO TO 133                                                           170225          
  132 K=NLHM+J                                                            170226          
      RECIP=TOT(K)/FLOAT(NCDS)+YM(J)                                      170227          
  133 IF (I.EQ.1) GO TO 120                                               170228          
      DO 134 II=2,I                                                                       
  134 RECIP=RECIP-SSS(II)*PM(II)                                                          
  120 WRITE (6,1005) LITY(J),SSS(I),SD,RECIP,(SSS(K),K=1,I)               170232          
 1005 FORMAT (1H ,A6,2X,F10.5,2X,F10.5,F12.5,1X,5(1X,F10.5))              170233          
      IF(IBOP)2120,2120,2004                                                              
 2004 WRITE(IBOP)I,J,LITY(J),SSS(I),SD,RECIP,(SSS(K),K=1,I)                               
 2120 CONTINUE                                                                            
   88 CONTINUE                                                            170234          
   73 NCAS=NCAS+I2+1                                                      170235          
      IF(MXNCAS-NCAS) 12,13,13                                            170236          
   12 WRITE(6,1011)                                                       170237          
 1011 FORMAT(16H0MXNCAS EXCEEDED)                                         170238          
   13 CONTINUE                                                            170239          
      DF=EDF+FLOAT(NLHM)                                                  170240          
      RETURN                                                              170241          
      END                                                                 170242          
*ENDTEXT                                                                                  
