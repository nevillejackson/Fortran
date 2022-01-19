*TEXT                                                                                     
      SUBROUTINE FULSIB(IIA,IIB,IIC,IID,                                FLS30001          
     1T2,COMENV,WFSENV,DF,IDF,SS,SM,F,XMN,VC1,VC2,VC3,HS,HD,HSD,SE1,SE2,FLS30002          
     2SE3,THETA,RTHETA,SER,THETGS,THETPD,HDFS,VAR1,NAMVAR,              FLS30003          
     3X,XID,C2,C3,SX,RXS,OID,PROD1,PROD2,L,TEMP,PRODS,BV1,BV2,BN,AN,    FLS30004          
     4PROD3)                                                            FLS30005          
C-----VERSION 3                                                         FLS30006          
      COMMON/CMBLK1/I1,I2,N3,N4,N5,N6,IS,ID,IM,IF,N1,ITLE(9),Y,KPUT     FLS30007          
      COMMON/CMBLK2/IT2,ICOMEN,IWFSEN,IDFQ,IIDFQ,ISS,ISM,IFQ,IXMN,IVC1, FLS30008          
     1  IVC2,IVC3,IHS,IHD,IHSD,ISE1,ISE2,ISE3,ITHETA,IRTHET,ISER,ITHETG,FLS30009          
     3  ITHETP,IHDFS,IVAR1,INAMVA,LEN26                                 FLS30010          
      COMMON/CMBLK3/IX,IXID,IC2,IC3,ISX,IRXS,IOID,IPROD1,IPROD2,IL,ITEMPFLS30011          
     1  ,IPRODS,IBV1,IBV2,IBN,IAN,IPROD3,LEN44                          FLS30012          
      COMMON/CMBLK5/NPROB,Q1,Q2,Q3,BIVAR1,BIVAR2,BIVAR3,I6,I7,I8,I9     FLS30013          
      DIMENSION T2(3,7),COMENV(IIA,1),WFSENV(IIA,1),DF(1),IDF(1),       FLS30014          
     1 SS(IIC,IIA,1),SM(IIB,IIA,1),F(IIB,IIA,1),XMN(IIA,1),VC1(IIA,1),  FLS30015          
     2 VC2(IIA,1),VC3(IIA,1),HS(IIA,1),HD(IIA,1),HSD(IIA,1),SE1(IIA,1), FLS30016          
     3 SE2(IIA,1),SE3(IIA,1),THETA(7,IIA,1),RTHETA(7,IIA,1),SER(7,IIA,1)FLS30017          
     4 ,THETGS(IIA,1),THETPD(IIA,1),HDFS(IIA,1),VAR1(IIA,1),NAMVAR(1)   FLS30018          
      DIMENSION X(1),XID(1),C2(1),C3(1),SX(IIB,1),RXS(1),OID(1),        FLS30019          
     1PROD1(IIA,1),PROD2(IIA,1),L(7),TEMP(7),PRODS(IIA,1),BV1(1),       FLS30020          
     2BV2(IID,1),BN(1),AN(IID,1),PROD3(IIA,1)                           FLS30021          
      REAL L                                                            FLS30022          
      EQUIVALENCE(NAN,N5),(NB,N6)                                       FLS30023          
      I6=I2-1                                                           FLS30024          
      I7=I2                                                             FLS30025          
      I8=I2+1                                                           FLS30026          
      I9=I2+2                                                           FLS30027          
      Y=0                                                               FLS30028          
    1 C1 = 0.0                                                          FLS30029          
      DO 2 I=1,I9                                                       FLS30030          
      C2(I) = 0.0                                                       FLS30031          
      C3(I) = 0.0                                                       FLS30032          
    2 DF(I) = 0.0                                                       FLS30033          
      DO 3 I=1,I1                                                       FLS30034          
      RXS(I) = 0.0                                                      FLS30035          
      DO 3 J=1,I1                                                       FLS30036          
      XMN(I,J)=0.0                                                      FLS30037          
      VC1(I,J)=0.0                                                      FLS30038          
      VC2(I,J)=0.0                                                      FLS30039          
      VC3(I,J)=0.0                                                      FLS30040          
      HS(I,J)= 0.0                                                      FLS30041          
      HD(I,J)= 0.0                                                      FLS30042          
      HSD(I,J)=0.0                                                      FLS30043          
      THETGS(I,J)=0.0                                                   FLS30044          
      THETPD(I,J)=0.0                                                   FLS30045          
      HDFS(I,J)=0.0                                                     FLS30046          
      VAR1(I,J)=0.0                                                     FLS30047          
      PROD1(I,J)=0.0                                                    FLS30048          
      PROD2(I,J)=0.0                                                    FLS30049          
      PROD3(I,J)=0.0                                                    FLS30050          
      SE1(I,J)=0.0                                                      FLS30051          
      SE2(I,J)=0.0                                                      FLS30052          
      SE3(I,J)=0.0                                                      FLS30053          
      SS(7,I,J)=0.0                                                     FLS30054          
    3 CONTINUE                                                          FLS30055          
      DO 4 I=1,NAN                                                      FLS30056          
      BV1(I)=0.0                                                        FLS30057          
      BN(I)=0.0                                                         FLS30058          
      DO 4 J = 1,NB                                                     FLS30059          
      BV2(I,J)=0.0                                                      FLS30060          
    4 AN(I,J)=0.0                                                       FLS30061          
      BV = 0.0                                                          FLS30062          
      DO 5 I=1,I8                                                       FLS30063          
      DO 5 J=1,I1                                                       FLS30064          
      SX(I,J)= 0.0                                                      FLS30065          
      DO 5 K=1,I1                                                       FLS30066          
      F(I,J,K)=0.0                                                      FLS30067          
    5  SM(I,J,K)= 0.0                                                   FLS30068          
      DO 7 K1=1,7                                                       FLS30069          
      L(K1)=0.0                                                         FLS30070          
      TEMP(K1)=0.0                                                      FLS30071          
      DO 6 K2=1,3                                                       FLS30072          
    6 T2(K2,K1)=0.0                                                     FLS30073          
      DO 7 IK=1,I1                                                      FLS30074          
      DO 7 JK=1,I1                                                      FLS30075          
      THETA(K1,IK,JK)=0.0                                               FLS30076          
      RTHETA(K1,IK,JK)=0.0                                              FLS30077          
    7 SER(K1,IK,JK)=0.0                                                 FLS30078          
      I3 =1                                                             FLS30079          
      I11=1                                                             FLS30080          
      IN = 1                                                            FLS30081          
      JN = 1                                                            FLS30082          
C                                                                       FLS30083          
      I33=0                                                             FLS30084          
      GO TO (211,8,211),N4                                              FLS30085          
  211 PRINT 212,N1                                                      FLS30086          
  212 FORMAT(1H1,10X,63HLISTING OF OBSERVATION SETS AND IDENTIFICATIONS FLS30087          
     1FOR PROBLEM NO ,I3/)                                              FLS30088          
    8 CALL READX(XID,X)                                                 FLS30089          
      GO TO (9,13,213),N4                                               FLS30090          
  213 I33=I33+1                                                         FLS30091          
      IF(I33-3) 9,9,13                                                  FLS30092          
    9 PRINT 10,(XID(I),I=1,I2)                                          FLS30093          
   10 FORMAT(6H0IDENT,8F15.0/(6X,8F15.0))                               FLS30094          
      PRINT 11,(X(I),I=1,I1)                                            FLS30095          
   11 FORMAT(6H SET  ,8E15.7/(6X,8E15.7))                               FLS30096          
C     --------------------------------------------                      FLS30097          
C     BASIC COMPUTATIONS FOR NESTED ANALYSIS.                           FLS30098          
C     --------------------------------------------                      FLS30099          
   13 GO TO (50,14),I3                                                  FLS30100          
   14 IF(XID(1))16,15,16                                                FLS30101          
   15 I4=I2                                                             FLS30102          
      I11=2                                                             FLS30103          
      GO TO 64                                                          FLS30104          
   16 IF(XID(1)-OID(1)) 18,17,18                                        FLS30105          
   17 GO TO (52,21,21,21,21),I2                                         FLS30106          
   18 I4=I2                                                             FLS30107          
      IF (I4-1) 19,19,20                                                FLS30108          
   19 JN = JN+1                                                         FLS30109          
      GO TO 64                                                          FLS30110          
   20 IN = IN+1                                                         FLS30111          
      JN = 1                                                            FLS30112          
      GO TO 64                                                          FLS30113          
   21 IF(XID(2)-OID(2))25,22,25                                         FLS30114          
   22 GO TO (23,52,28,28,28),I2                                         FLS30115          
   23 WRITE(6,24)                                                       FLS30116          
   24 FORMAT(48H0I2 ON CONTROL CD. 2 NOT EQ. XID IN READ OF READ)       FLS30117          
      KPUT=1                                                            FLS30118          
      RETURN                                                            FLS30119          
   25 I4 = I2-1                                                         FLS30120          
      IF (I4-1) 26,26,27                                                FLS30121          
   26 JN= JN+1                                                          FLS30122          
      GO TO 64                                                          FLS30123          
   27 IN = IN+1                                                         FLS30124          
      JN = 1                                                            FLS30125          
      GO TO 64                                                          FLS30126          
   28 IF(XID(3)-OID(3)) 31,281,31                                       FLS30127          
  281 GO TO (29,29,52,34,34),I2                                         FLS30128          
   29 WRITE(6,24)                                                       FLS30129          
      KPUT=1                                                            FLS30130          
      RETURN                                                            FLS30131          
   31 I4 = I2 - 2                                                       FLS30132          
      IF (I4 - 1) 32,32,33                                              FLS30133          
   32 JN = JN + 1                                                       FLS30134          
      GO TO 64                                                          FLS30135          
   33 IN = IN + 1                                                       FLS30136          
      JN = 1                                                            FLS30137          
      GO TO 64                                                          FLS30138          
   34 IF (XID(4) - OID(4)) 38,35,38                                     FLS30139          
   35 GO TO (36,36,36,52,41),I2                                         FLS30140          
   36 WRITE(6,24)                                                       FLS30141          
      KPUT=1                                                            FLS30142          
      RETURN                                                            FLS30143          
   38 I4=I2-3                                                           FLS30144          
      IF (I4-1) 39,39,40                                                FLS30145          
   39 JN = JN+1                                                         FLS30146          
      GO TO 64                                                          FLS30147          
   40 IN = IN+1                                                         FLS30148          
      JN = 1                                                            FLS30149          
      GO TO 64                                                          FLS30150          
C  41 IF(XID(5)-OID(5))45,42,45                                         FLS30151          
   41 IF(XID(5)-OID(5))49,52,49                                         FLS30152          
   42 GO TO (43,43,43,43,52,48),I2                                      FLS30153          
   43 WRITE(6,24)                                                       FLS30154          
      KPUT=1                                                            FLS30155          
      RETURN                                                            FLS30156          
   45 I4 = I2-4                                                         FLS30157          
      IF (I4 - 1) 46,46,47                                              FLS30158          
   46 JN = JN + 1                                                       FLS30159          
      GO TO 64                                                          FLS30160          
   47 IN = IN + 1                                                       FLS30161          
      JN = 1                                                            FLS30162          
      GO TO 64                                                          FLS30163          
C  48 IF(XID(6)-OID(6))111,109,111                                      FLS30164          
   48 IF(XID(6)-OID(6))49,52,49                                         FLS30165          
   49 I4=1                                                              FLS30166          
      JN = JN + 1                                                       FLS30167          
      GO TO 64                                                          FLS30168          
   50 I3=2                                                              FLS30169          
      IF(XID(1)) 651,15,651                                             FLS30170          
  651 DO 51 I=1,I2                                                      FLS30171          
   51 OID(I) =XID(I)                                                    FLS30172          
   52 DO 53 I=1,I1                                                      FLS30173          
      SX(1,I)= SX(1,I)+X(I)                                             FLS30174          
   53 RXS(I) = RXS(I) + X(I)                                            FLS30175          
      IF (IN - NAN) 54,54,55                                            FLS30176          
   54 IF (JN - NB) 60,60,55                                             FLS30177          
   55 PRINT 56,(XID(I),I = 1,I2)                                        FLS30178          
   56 FORMAT(2X,39H0COUNTER EXCEEDS DIMENSION AT OBSERVN.-,/1H ,7F15.0) FLS30179          
   57 PRINT 58,IN,JN                                                    FLS30180          
   58 FORMAT(41H  COMPARE DATA TO N5,N6 ON CONTROL CARD 2,              FLS30181          
     1/7H  IN = , I5/7H  JN = ,I5)                                      FLS30182          
      KPUT=1                                                            FLS30183          
      RETURN                                                            FLS30184          
   60 IF (Y) 61,62,61                                                   FLS30185          
   61 BV = BV+(0.25/Y)                                                  FLS30186          
      BV1(IN) = BV1(IN)+(0.25/Y)                                        FLS30187          
      BV2(IN,JN) = BV2(IN,JN) + (0.25/Y)                                FLS30188          
   62 AN(IN,JN) = AN(IN,JN) + 1.0                                       FLS30189          
      BN(IN) = BN(IN) + 1.0                                             FLS30190          
      DO 63 K1 =1,I1                                                    FLS30191          
      DO 63 K2 = K1,I1                                                  FLS30192          
   63 VC1 (K1,K2) = VC1 (K1,K2)+ X(K1)* X(K2)                           FLS30193          
      C1 = C1 + 1.0                                                     FLS30194          
      C2(1) = C2(1)+1.0                                                 FLS30195          
      GO TO 8                                                           FLS30196          
   64 IF(C2(1)) 666,666,665                                             FLS30197          
  665 DO 65 J=1,I1                                                      FLS30198          
      DO 65 K=J,I1                                                      FLS30199          
   65  SM(1,J,K)=  SM(1,J,K)+((SX(1,J)*SX(1,K))/C2(1))                  FLS30200          
  666 IF (I4-2) 69,66,66                                                FLS30201          
   66 DO 67 I = 2,I4                                                    FLS30202          
      C2(I) = C2(I)+ C2(I-1)                                            FLS30203          
      DO 67 J = 1,I1                                                    FLS30204          
   67 SX(I,J)= SX(I,J)+SX(I-1,J)                                        FLS30205          
      DO 668 I=2,I4                                                     FLS30206          
      IF(C2(I)) 668,668,667                                             FLS30207          
  667 DO 68 J = 1,I1                                                    FLS30208          
      DO 68 K = J,I1                                                    FLS30209          
   68 SM (I,J,K) =  SM(I,J,K) +((SX(I,J)*SX(I,K))/C2(I))                FLS30210          
  668 CONTINUE                                                          FLS30211          
   69 DO 70 I =1,I4                                                     FLS30212          
   70 C3(I) = C3(I)+1.0                                                 FLS30213          
      DO 71 J=1,I1                                                      FLS30214          
   71 SX(I4+1,J)=SX(I4+1,J)+SX(I4,J)                                    FLS30215          
      C2(I4 +1) = C2(I4+1) +C2(I4)                                      FLS30216          
   72 GO TO (73,76),I11                                                 FLS30217          
   73 DO 74 I=1,I2                                                      FLS30218          
   74 OID(I)= XID(I)                                                    FLS30219          
      DO 75 I=1,I4                                                      FLS30220          
      C2(I) =0.0                                                        FLS30221          
      DO 75 J=1,I1                                                      FLS30222          
   75 SX(I,J) = 0.0                                                     FLS30223          
      GO TO 52                                                          FLS30224          
   76 DF(I2+2)=C1-1.0                                                   FLS30225          
      IF(C1)215,215,664                                                 FLS30226          
  215 WRITE(6,216)                                                      FLS30227          
  216 FORMAT(16H0NO OBSERVATIONS)                                       FLS30228          
      KPUT=-1                                                           FLS30229          
      RETURN                                                            FLS30230          
  664 I5 = I2-1                                                         FLS30231          
      I6 = 0                                                            FLS30232          
      DF(I2+1)= C1-C3(1)                                                FLS30233          
      DF(1)=C3(I2)-1.0                                                  FLS30234          
      DO 77 I = 1,I5                                                    FLS30235          
      I10 = I2-I6                                                       FLS30236          
      DF(I10) = C3(I)-C3(I+1)                                           FLS30237          
   77 I6 = I6+1                                                         FLS30238          
      I5 = I2-1                                                         FLS30239          
      I6 = 0                                                            FLS30240          
      DO 78  I=1, I1                                                    FLS30241          
      DO 78  J=I,I1                                                     FLS30242          
      XMN(I,J)=RXS(I)/C1                                                FLS30243          
      SS(I2+2,I,J)=  VC1(I,J)-((RXS(I)*RXS(J))/C1)                      FLS30244          
      SS(I2+1,I,J)=  VC1(I,J)-  SM(1,I,J)                               FLS30245          
   78 SS( 1,I,J) =  SM(I2,I,J) - ((RXS(I)*RXS(J))/C1)                   FLS30246          
      DO 80 I=1,I5                                                      FLS30247          
      I10 = I2 - I6                                                     FLS30248          
      DO 79 J = 1,I1                                                    FLS30249          
      DO 79 K = J,I1                                                    FLS30250          
   79 SS(I10,J,K) =  SM(I,J,K) -  SM(I+1,J,K)                           FLS30251          
   80 I6 = I6+1                                                         FLS30252          
      I6 = I2+1                                                         FLS30253          
      DO 81 I=1,I6                                                      FLS30254          
      DO 81 J=1,I1                                                      FLS30255          
      DO 81 K=J,I1                                                      FLS30256          
   81 SM(I,J,K)=DIVF(SS(I,J,K),DF(I))                                   FLS30257          
      I6 = I2+2                                                         FLS30258          
      DO 82 I= 1,I6                                                     FLS30259          
   82 IDF(I) = DF(I) +.5                                                FLS30260          
   83 DO 84 I=1,I2                                                      FLS30261          
      DO 84 J=1,I1                                                      FLS30262          
      DO 84 K=J,I1                                                      FLS30263          
   84 F(I,J,K)=DIVF(SM(I,J,K),SM(I+1,J,K))                              FLS30264          
      SUM = 0.0                                                         FLS30265          
      SUMB= 0.0                                                         FLS30266          
      DO  87  I=1,NAN                                                   FLS30267          
      IF(BN(I)) 85,87,85                                                FLS30268          
   85 SUM = 0.0                                                         FLS30269          
      DO  86   J=1,NB                                                   FLS30270          
   86 SUM = SUM + AN(I,J)**2                                            FLS30271          
      SUMB=SUMB+DIVF(SUM,BN(I))                                         FLS30272          
   87 CONTINUE                                                          FLS30273          
      Q1=DIVF(C1-SUMB,DF(I2))                                           FLS30274          
      SUM = 0.0                                                         FLS30275          
      DO 90  I =1,NAN                                                   FLS30276          
      IF (BN(I)) 88,90,88                                               FLS30277          
   88 DO 89  J=1,NB                                                     FLS30278          
      SUM=SUM+((AN(I,J)*DIVF(AN(I,J),BN(I)))-(AN(I,J)*AN(I,J)/C1))      FLS30279          
   89 CONTINUE                                                          FLS30280          
   90 CONTINUE                                                          FLS30281          
      SDF = 0.0                                                         FLS30282          
      JD = I2 - 1                                                       FLS30283          
      DO 91 I = 1,JD                                                    FLS30284          
   91 SDF = SDF + DF(I )                                                FLS30285          
      Q2=DIVF(SUM,SDF)                                                  FLS30286          
      SUM= 0.0                                                          FLS30287          
      DO 92 I= 1,NAN                                                    FLS30288          
   92 SUM = SUM + BN(I)* BN(I)                                          FLS30289          
      SUM = SUM/C1                                                      FLS30290          
      Q3=DIVF(C1-SUM,SDF)                                               FLS30291          
C                                                                       FLS30292          
      WRITE(6,214)                                                      FLS30293          
  214 FORMAT(34H0ERRORS AND/OR COMMENTS FOR FULSIB)                     FLS30294          
      I6=I2-1                                                           FLS30295          
      I7=I2                                                             FLS30296          
      I8=I2+1                                                           FLS30297          
      I9=I2+2                                                           FLS30298          
C                                                                       FLS30299          
C                                                                       FLS30300          
C     ANALYSIS-COMPLETE, NESTED ONLY, OR OFFSPRING-PARENT REGN. ONLY.   FLS30301          
C                                                                       FLS30302          
      GO TO (93,93,113),N3                                              FLS30303          
C     --------------------------------------------                      FLS30304          
C     SET UP TABLE OF COEFFICIENTS                                      FLS30305          
C     --------------------------------------------                      FLS30306          
   93 L(1) = Q3                                                         FLS30307          
      L(2) = Q1                                                         FLS30308          
      L(3) = Q1*Q3                                                      FLS30309          
      L(4) = Q3                                                         FLS30310          
      L(5) = Q1                                                         FLS30311          
      L(6) = L(3)                                                       FLS30312          
      L(7) = L(3)                                                       FLS30313          
      T2(1,1) = 1.0                                                     FLS30314          
      T2(2,1)=DIVF(-Q2,Q1)                                              FLS30315          
      T2(3,1)=DIVF(Q2-Q1,Q1)                                            FLS30316          
      T2(1,2) = 0.0                                                     FLS30317          
      T2(2,2) = 1.0                                                     FLS30318          
      T2(3,2) = -1.0                                                    FLS30319          
      T2(1,3) = Q1                                                      FLS30320          
      T2(2,3) = Q3-Q2                                                   FLS30321          
      T2(3,3) = Q2-Q1-Q3                                                FLS30322          
      T2(1,4) = -2.0                                                    FLS30323          
      T2(2,4) = -2.*T2(2,1)                                             FLS30324          
      T2(3,4)=DIVF(Q1-Q2,Q1)+Q3                                         FLS30325          
      T2(1,5) = 0.0                                                     FLS30326          
      T2(2,5) = -2.0                                                    FLS30327          
      T2(3,5) = Q1+2.0                                                  FLS30328          
      T2(1,6) = -Q1                                                     FLS30329          
      T2(2,6) =-T2(2,3)                                                 FLS30330          
      T2(3,6) = Q1-Q2+Q3*(Q1+1.)                                        FLS30331          
      T2(1,7) = Q1                                                      FLS30332          
      T2(2,7) = T2(2,3)                                                 FLS30333          
      T2(3,7) = Q2-Q1+Q3*(Q1-1.)                                        FLS30334          
C                                                                       FLS30335          
      IOF=IS+ID+1                                                       FLS30336          
C     --------------------------------------------                      FLS30337          
C     VARIATION.                                                        FLS30338          
C     --------------------------------------------                      FLS30339          
      DO 95 IK=IOF,I1                                                   FLS30340          
      DO 95 JK=IK,I1                                                    FLS30341          
      DO 94 KH=1,7                                                      FLS30342          
   94 TEMP(KH) = T2(1,KH)*SM(I2-1,IK,JK)+                               FLS30343          
     1           T2(2,KH)*SM(I2,  IK,JK)+                               FLS30344          
     2           T2(3,KH)*SM(I2+1,IK,JK)                                FLS30345          
C     GENIC VARIATION                                                   FLS30346          
      THETA(1,IK,JK)=DIVF(4.,Q3)*TEMP(1)                                FLS30347          
      THETA(2,IK,JK)=DIVF(4.,Q1)*TEMP(2)                                FLS30348          
      THETA(3,IK,JK)=DIVF(2.*TEMP(3),L(3))                              FLS30349          
C     ENVIRONMENTAL VARIATION                                           FLS30350          
      THETA(4,IK,JK)=DIVF(1.,Q3)*TEMP(4)                                FLS30351          
      THETA(5,IK,JK)=DIVF(1.,Q1)*TEMP(5)                                FLS30352          
      THETA(6,IK,JK)=DIVF(TEMP(6),L(6))                                 FLS30353          
C     PHENOTYPIC VARIATION                                              FLS30354          
      THETA(7,IK,JK)=DIVF(TEMP(7),L(7))                                 FLS30355          
C                                                                       FLS30356          
      IF(DF(I8)) 951,951,95                                             FLS30357          
  951 THETA(4,IK,JK)=THETA(7,IK,JK)-THETA(1,IK,JK)                      FLS30358          
      THETA(2,IK,JK)=0.0                                                FLS30359          
      THETA(3,IK,JK)=0.0                                                FLS30360          
      THETA(5,IK,JK)=0.0                                                FLS30361          
      THETA(6,IK,JK)=0.0                                                FLS30362          
   95 CONTINUE                                                          FLS30363          
      DO 96 IK=1,3                                                      FLS30364          
      DO 96 JK=1,7                                                      FLS30365          
   96 T2(IK,JK) = T2(IK,JK)*T2(IK,JK)                                   FLS30366          
C                                                                       FLS30367          
      DF(I6) = DF(I6)+2.                                                FLS30368          
      DF(I7) = DF(I7)+2.                                                FLS30369          
      DF(I8) = DF(I8)+2.                                                FLS30370          
      DO 98 IK=IOF,I1                                                   FLS30371          
      DO 98 JK=IK,I1                                                    FLS30372          
C     --------------------------------------------                      FLS30373          
C     VARIANCE COMPONENTS.                                              FLS30374          
C     --------------------------------------------                      FLS30375          
      VC3(IK,JK)=SM(I8,IK,JK)                                           FLS30376          
      VC2(IK,JK)=DIVF(SM(I7,IK,JK)-SM(I8,IK,JK),Q1)                     FLS30377          
      VC1(IK,JK)=DIVF(SM(I6,IK,JK)-Q2*VC2(IK,JK)-VC3(IK,JK),Q3)         FLS30378          
      COMENV(IK,JK)=VC2(IK,JK)-VC1(IK,JK)                               FLS30379          
      WFSENV(IK,JK)=VC3(IK,JK)-(2.*VC1(IK,JK))                          FLS30380          
      IF(DF(I8).EQ.2.0) WFSENV(IK,JK)=0.0                               FLS30381          
      IF(JK.NE.IK) GO TO 98                                             FLS30382          
      PROD1(IK,JK) = 2.*((SM(I6,IK,JK)**2/DF(I6))+(SM(I7,IK,JK)**2/DF(I7FLS30383          
     1)))*DIVF(1.,Q3*Q3)                                                FLS30384          
      PROD2(IK,JK) = 2.*((SM(I7,IK,JK)**2/DF(I7))+(SM(I8,IK,JK)**2/DF(I8FLS30385          
     1)))*DIVF(1.,Q1*Q1)                                                FLS30386          
      PROD3(IK,JK)=DIVF(-Q2*(PROD2(IK,JK)-DIVF(2.*SM(I8,IK,JK)**2,Q1*Q1*FLS30387          
     1DF(I8))),Q3)                                                      FLS30388          
   98 CONTINUE                                                          FLS30389          
      I1X = I1-1                                                        FLS30390          
      IF(I1X) 980,980,981                                               FLS30391          
  981 DO 106 IK=IOF,I1X                                                 FLS30392          
      K=IK+1                                                            FLS30393          
      DO 106 JK=K,I1                                                    FLS30394          
C     --------------------------------------------                      FLS30395          
C     HERITABILITY OF DIFFERENCE OF 2 TRAITS.                           FLS30396          
C     --------------------------------------------                      FLS30397          
      THETGS(IK,JK)=THETA(1,IK,IK)*THETA(1,JK,JK)                       FLS30398          
      IF(THETGS(IK,JK) .LE. 0.0) GO TO 991                              FLS30399          
      THETGS(IK,JK)=2.0*(SQRT(THETGS(IK,JK)))-2.0*THETA(1,IK,JK)        FLS30400          
      GO TO 992                                                         FLS30401          
  991 THETGS(IK,JK)=0.0                                                 FLS30402          
  992 THETPD(IK,JK)=THETA(7,IK,IK)*THETA(7,JK,JK)                       FLS30403          
      IF(THETPD(IK,JK) .LE. 0.0) GO TO 993                              FLS30404          
      THETPD(IK,JK)=2.0*(SQRT(THETPD(IK,JK)))-2.0*THETA(7,IK,JK)        FLS30405          
      GO TO 994                                                         FLS30406          
  993 THETPD(IK,JK)=0.0                                                 FLS30407          
  994 IF(THETPD(IK,JK) .GT. 0.0 .AND. THETGS(IK,JK) .NE. 0.0) GO TO 99  FLS30408          
      HDFS(I,J)=0.0                                                     FLS30409          
      PRINT 995,IK,JK                                                   FLS30410          
  995 FORMAT(27H0 HERIT. OF DIF. FOR CHARS.  ,I2,3HAND,I2,20HCANNOT BE CFLS30411          
     1ALCULATED)                                                        FLS30412          
      GO TO 100                                                         FLS30413          
   99 HDFS(IK,JK)=DIVF(THETGS(IK,JK),THETPD(IK,JK))                     FLS30414          
C     --------------------------------------------                      FLS30415          
C     COR. COEFS. AND THEIR STD. ERRORS                                 FLS30416          
C     --------------------------------------------                      FLS30417          
  100 DO 106 K1=1,7                                                     FLS30418          
      IF(THETA(K1,IK,IK) .GT. 0.0 .AND. THETA(K1,JK,JK) .GT. 0.0)       FLS30419          
     1GO TO 102                                                         FLS30420          
      RTHETA(K1,IK,JK) = 0.                                             FLS30421          
      SER(K1,IK,JK) = 0.                                                FLS30422          
      PRINT 101,K1,IK,JK                                                FLS30423          
  101 FORMAT(16H0 CORRELATION NO,I2,15H FOR CHARACTERS,I2,3HAND,I2,     FLS30424          
     1 20HCANNOT BE CALCULATED)                                         FLS30425          
      GO TO 106                                                         FLS30426          
  102 CONTINUE                                                          FLS30427          
      RTHETA(K1,IK,JK) = THETA(K1,IK,JK)/( SQRT(THETA(K1,IK,IK)*                          
     1                   THETA(K1,JK,JK)))                              FLS30429          
      IF(RTHETA(K1,IK,JK) .NE. 0.) GO TO 104                            FLS30430          
      SER(K1,IK,JK) = 0.                                                FLS30431          
      PRINT 103, K1,IK,JK                                               FLS30432          
  103 FORMAT( 1H029HSTD. ERROR FOR CORRELATION NO ,I2,15H FOR CHARACTERSFLS30433          
     1,I2,3HAND,I2,20HCANNOT BE CALCULATED )                            FLS30434          
      GO TO 106                                                         FLS30435          
  104 VAR = (T2(1,K1)*SM(I6,IK,IK)*SM(I6,IK,IK)/DF(I6)                  FLS30436          
     1      +T2(2,K1)*SM(I7,IK,IK)*SM(I7,IK,IK)/DF(I7)                  FLS30437          
     2      +T2(3,K1)*SM(I8,IK,IK)*SM(I8,IK,IK)/DF(I8))*                FLS30438          
     3DIVF(1.,4.*THETA(K1,IK,IK)*THETA(K1,IK,IK))                       FLS30439          
      VAR = VAR+(T2(1,K1)*SM(I6,JK,JK)*SM(I6,JK,JK)/DF(I6)              FLS30440          
     1         + T2(2,K1)*SM(I7,JK,JK)*SM(I7,JK,JK)/DF(I7)              FLS30441          
     2         + T2(3,K1)*SM(I8,JK,JK)*SM(I8,JK,JK)/DF(I8))             FLS30442          
     3*DIVF(1.,  4.*THETA(K1,JK,JK)*THETA(K1,JK,JK))                    FLS30443          
      VAR1(IK,JK) = VAR+(T2(1,K1)*SM(I6,IK,JK)*SM(I6,IK,JK)/DF(I6)      FLS30444          
     1         + T2(2,K1)*SM(I7,IK,JK)*SM(I7,IK,JK)/DF(I7)              FLS30445          
     2         + T2(3,K1)*SM(I8,IK,JK)*SM(I8,IK,JK)/DF(I8))             FLS30446          
     3*DIVF(1.,  2.*THETA(K1,IK,IK)*THETA(K1,JK,JK))                    FLS30447          
      VAR = VAR1(IK,JK)+(T2(1,K1)*(SM(I6,IK,IK)*SM(I6,JK,JK)+           FLS30448          
     1                     SM(I6,IK,JK)*SM(I6,IK,JK))/DF(I6)            FLS30449          
     2         + T2(2,K1)*(SM(I7,IK,IK)*SM(I7,JK,JK)+                   FLS30450          
     3                     SM(I7,IK,JK)*SM(I7,IK,JK))/DF(I7)            FLS30451          
     4         + T2(3,K1)*(SM(I8,IK,IK)*SM(I8,JK,JK)+                   FLS30452          
     5                     SM(I8,IK,JK)*SM(I8,IK,JK))/DF(I8))           FLS30453          
     6*DIVF(1.,  2.*THETA(K1,IK,JK)*THETA(K1,IK,JK))                    FLS30454          
      VAR = VAR-(T2(1,K1)*SM(I6,IK,JK)*SM(I6,IK,IK)/DF(I6)              FLS30455          
     1         + T2(2,K1)*SM(I7,IK,JK)*SM(I7,IK,IK)/DF(I7)              FLS30456          
     2         + T2(3,K1)*SM(I8,IK,JK)*SM(I8,IK,IK)/DF(I8))             FLS30457          
     3*DIVF(1.,  THETA(K1,IK,IK)*THETA(K1,IK,JK))                       FLS30458          
      VAR = VAR-(T2(1,K1)*SM(I6,JK,JK)*SM(I6,IK,JK)/DF(I6)              FLS30459          
     1         + T2(2,K1)*SM(I7,JK,JK)*SM(I7,IK,JK)/DF(I7)              FLS30460          
     2         + T2(3,K1)*SM(I8,JK,JK)*SM(I8,IK,JK)/DF(I8))             FLS30461          
     3*DIVF(1.,  THETA(K1,JK,JK)*THETA(K1,IK,JK))                       FLS30462          
      IF (K1 .NE. 7) VAR1(IK,JK) = 0.0                                  FLS30463          
      IF(VAR .LE. 0.) GO TO 105                                         FLS30464          
C                                                                       FLS30465          
      FKH = 2.0                                                         FLS30466          
      IF(K1 .EQ. 3) FKH = 8.0                                           FLS30467          
      IF(K1 .EQ. 1  .OR. K1 .EQ. 2) FKH = 32.0                          FLS30468          
      SER(K1,IK,JK)=SQTF(DIVF(FKH*RTHETA(K1,IK,JK)**2*VAR,L(K1)**2))    FLS30469          
      GO TO 106                                                         FLS30470          
  105 SER(K1,IK,JK) = 0.                                                FLS30471          
      PRINT 103, K1,IK,JK                                               FLS30472          
  106 CONTINUE                                                          FLS30473          
  980 DF(I6) = DF(I6)-2.                                                FLS30474          
      DF(I7) = DF(I7)-2.                                                FLS30475          
      DF(I8) = DF(I8)-2.                                                FLS30476          
C     --------------------------------------------                      FLS30477          
C     DATA BINOMIALLY DISTRIBUTED.                                      FLS30478          
C     --------------------------------------------                      FLS30479          
      BXA = 0.0                                                         FLS30480          
      DO 108 I=1,NAN                                                    FLS30481          
      IF(BN(I))107,108,107                                              FLS30482          
  107 BV1(I)=DIVF(BV1(I),BN(I))                                         FLS30483          
      BXA = BXA + BV1(I)                                                FLS30484          
  108 CONTINUE                                                          FLS30485          
      BXB = 0.0                                                         FLS30486          
      DO 110 I=1,NAN                                                    FLS30487          
      DO 110 J=1,NB                                                     FLS30488          
      IF(AN(I,J))109,110,109                                            FLS30489          
  109 BV2(I,J)=DIVF(BV2(I,J),AN(I,J))                                   FLS30490          
      BXB = BXB + BV2(I,J)                                              FLS30491          
  110 CONTINUE                                                          FLS30492          
      BIVAR1=DIVF(BXA-(BV/C1),SDF)                                      FLS30493          
      BIVAR2=DIVF(BXB-BXA,DF(I2))                                       FLS30494          
      BIVAR3=DIVF(BV-BXB,DF(I2+1))                                      FLS30495          
      DO 112 IK=IOF,I1                                                  FLS30496          
      IF(BIVAR1.LE.0.0) GO TO 111                                       FLS30497          
      VC3(IK,IK) = VC3(IK,IK)-BIVAR3                                    FLS30498          
      VC2(IK,IK)=DIVF(SM(I2,IK,IK)-VC3(IK,IK)-BIVAR2,Q1)                FLS30499          
      VC1(IK,IK)=DIVF(SM(I2-1,IK,IK)-Q2*VC2(IK,IK)-VC3(IK,IK)-BIVAR1,Q3)FLS30500          
      THETA(1,IK,IK)=4.0*VC1(IK,IK)                                     FLS30501          
      THETA(2,IK,IK)=4.0*VC2(IK,IK)                                     FLS30502          
      THETA(3,IK,IK)=2.0*(VC1(IK,IK)+VC2(IK,IK))                        FLS30503          
      THETA(7,IK,IK) = THETA(7,IK,IK)+0.25                              FLS30504          
C     --------------------------------------------                      FLS30505          
C     HERITABILITIES AND THEIR STD. ERRORS                              FLS30506          
C     --------------------------------------------                      FLS30507          
  111 HS(IK,IK)=DIVF(THETA(1,IK,IK),THETA(7,IK,IK))                     FLS30508          
      HD(IK,IK)=DIVF(THETA(2,IK,IK),THETA(7,IK,IK))                     FLS30509          
      HSD(IK,IK)=DIVF(THETA(3,IK,IK),THETA(7,IK,IK))                    FLS30510          
      SE1(IK,IK)=4.*DIVF(SQTF(PROD1(IK,IK)),THETA(7,IK,IK))             FLS30511          
      SE2(IK,IK)=4.*DIVF(SQTF(PROD2(IK,IK)),THETA(7,IK,IK))             FLS30512          
  112 SE3(IK,IK)=2.*DIVF(SQTF(PROD1(IK,IK)+PROD2(IK,IK)+2.*PROD3(IK,IK))FLS30513          
     1,THETA(7,IK,IK))                                                  FLS30514          
  113 CONTINUE                                                          FLS30515          
      RETURN                                                            FLS30516          
      END                                                               FLS30517          
*ENDTEXT                                                                                  
