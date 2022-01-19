*TEXT                                                                                     
      SUBROUTINE FULSIB(IIA,IIB,IIC,IID,                                FLS20001          
     1T2,COMENV,WFSENV,DF,IDF,SS,SM,F,XMN,VC1,VC2,VC3,HS,HD,HSD,SE1,SE2,FLS20002          
     2SE3,THETA,RTHETA,SER,THETGS,THETPD,HDFS,VAR1,NAMVAR,              FLS20003          
     3X,XID,C2,C3,SX,RXS,OID,PROD1,PROD2,L,TEMP,PRODS,BV1,BV2,BN,AN,    FLS20004          
     4PROD3)                                                            FLS20005          
C-----VERSION 2                                                         FLS20006          
      COMMON/CMBLK1/I1,I2,N3,N4,N5,N6,IS,ID,IM,IF,N1,ITLE(9),Y,KPUT     FLS20007          
      COMMON/CMBLK2/IT2,ICOMEN,IWFSEN,IDFQ,IIDFQ,ISS,ISM,IFQ,IXMN,IVC1, FLS20008          
     1  IVC2,IVC3,IHS,IHD,IHSD,ISE1,ISE2,ISE3,ITHETA,IRTHET,ISER,ITHETG,FLS20009          
     3  ITHETP,IHDFS,IVAR1,INAMVA,LEN26                                 FLS20010          
      COMMON/CMBLK3/IX,IXID,IC2,IC3,ISX,IRXS,IOID,IPROD1,IPROD2,IL,ITEMPFLS20011          
     1  ,IPRODS,IBV1,IBV2,IBN,IAN,IPROD3,LEN44                          FLS20012          
      COMMON/CMBLK5/NPROB,Q1,Q2,Q3,BIVAR1,BIVAR2,BIVAR3,I6,I7,I8,I9     FLS20013          
      DIMENSION T2(3,7),COMENV(IIA,1),WFSENV(IIA,1),DF(1),IDF(1),       FLS20014          
     1 SS(IIC,IIA,1),SM(IIB,IIA,1),F(IIB,IIA,1),XMN(IIA,1),VC1(IIA,1),  FLS20015          
     2 VC2(IIA,1),VC3(IIA,1),HS(IIA,1),HD(IIA,1),HSD(IIA,1),SE1(IIA,1), FLS20016          
     3 SE2(IIA,1),SE3(IIA,1),THETA(7,IIA,1),RTHETA(7,IIA,1),SER(7,IIA,1)FLS20017          
     4 ,THETGS(IIA,1),THETPD(IIA,1),HDFS(IIA,1),VAR1(IIA,1),NAMVAR(1)   FLS20018          
      DIMENSION X(1),XID(1),C2(1),C3(1),SX(IIB,1),RXS(1),OID(1),        FLS20019          
     1PROD1(IIA,1),PROD2(IIA,1),L(7),TEMP(7),PRODS(IIA,1),BV1(1),       FLS20020          
     2BV2(IID,1),BN(1),AN(IID,1),PROD3(IIA,1)                           FLS20021          
      REAL L                                                            FLS20022          
      EQUIVALENCE(NAN,N5),(NB,N6)                                       FLS20023          
      I6=I2-1                                                           FLS20024          
      I7=I2                                                             FLS20025          
      I8=I2+1                                                           FLS20026          
      I9=I2+2                                                           FLS20027          
      Y=0                                                               FLS20028          
    1 C1 = 0.0                                                          FLS20029          
      DO 2 I=1,I9                                                       FLS20030          
      C2(I) = 0.0                                                       FLS20031          
      C3(I) = 0.0                                                       FLS20032          
    2 DF(I) = 0.0                                                       FLS20033          
      DO 3 I=1,I1                                                       FLS20034          
      RXS(I) = 0.0                                                      FLS20035          
      DO 3 J=1,I1                                                       FLS20036          
      XMN(I,J)=0.0                                                      FLS20037          
      VC1(I,J)=0.0                                                      FLS20038          
      VC2(I,J)=0.0                                                      FLS20039          
      VC3(I,J)=0.0                                                      FLS20040          
      HS(I,J)= 0.0                                                      FLS20041          
      HD(I,J)= 0.0                                                      FLS20042          
      HSD(I,J)=0.0                                                      FLS20043          
      THETGS(I,J)=0.0                                                   FLS20044          
      THETPD(I,J)=0.0                                                   FLS20045          
      HDFS(I,J)=0.0                                                     FLS20046          
      VAR1(I,J)=0.0                                                     FLS20047          
      PROD1(I,J)=0.0                                                    FLS20048          
      PROD2(I,J)=0.0                                                    FLS20049          
      PROD3(I,J)=0.0                                                    FLS20050          
      SE1(I,J)=0.0                                                      FLS20051          
      SE2(I,J)=0.0                                                      FLS20052          
      SE3(I,J)=0.0                                                      FLS20053          
      SS(7,I,J)=0.0                                                     FLS20054          
    3 CONTINUE                                                          FLS20055          
      DO 4 I=1,NAN                                                      FLS20056          
      BV1(I)=0.0                                                        FLS20057          
      BN(I)=0.0                                                         FLS20058          
      DO 4 J = 1,NB                                                     FLS20059          
      BV2(I,J)=0.0                                                      FLS20060          
    4 AN(I,J)=0.0                                                       FLS20061          
      BV = 0.0                                                          FLS20062          
      DO 5 I=1,I8                                                       FLS20063          
      DO 5 J=1,I1                                                       FLS20064          
      SX(I,J)= 0.0                                                      FLS20065          
      DO 5 K=1,I1                                                       FLS20066          
      F(I,J,K)=0.0                                                      FLS20067          
    5  SM(I,J,K)= 0.0                                                   FLS20068          
      DO 7 K1=1,7                                                       FLS20069          
      L(K1)=0.0                                                         FLS20070          
      TEMP(K1)=0.0                                                      FLS20071          
      DO 6 K2=1,3                                                       FLS20072          
    6 T2(K2,K1)=0.0                                                     FLS20073          
      DO 7 IK=1,I1                                                      FLS20074          
      DO 7 JK=1,I1                                                      FLS20075          
      THETA(K1,IK,JK)=0.0                                               FLS20076          
      RTHETA(K1,IK,JK)=0.0                                              FLS20077          
    7 SER(K1,IK,JK)=0.0                                                 FLS20078          
      I3 =1                                                             FLS20079          
      I11=1                                                             FLS20080          
      IN = 1                                                            FLS20081          
      JN = 1                                                            FLS20082          
C                                                                       FLS20083          
      I33=1                                                             FLS20084          
      GO TO (211,8,211),N4                                              FLS20085          
  211 PRINT 212,N1                                                      FLS20086          
  212 FORMAT(1H1,10X,63HLISTING OF OBSERVATION SETS AND IDENTIFICATIONS FLS20087          
     1FOR PROBLEM NO ,I3/)                                              FLS20088          
    8 CALL READX(XID,X)                                                 FLS20089          
      GO TO (9,13,213),N4                                               FLS20090          
  213 I33=I33+1                                                         FLS20091          
      IF(I33-3) 9,9,13                                                  FLS20092          
    9 PRINT 10,(XID(I),I=1,I2)                                          FLS20093          
   10 FORMAT(6H0IDENT,8F15.0/(6X,8F15.0))                               FLS20094          
      PRINT 11,(X(I),I=1,I1)                                            FLS20095          
   11 FORMAT(6H SET  ,8E15.7/(6X,8E15.7))                               FLS20096          
C     --------------------------------------------                      FLS20097          
C     BASIC COMPUTATIONS FOR NESTED ANALYSIS.                           FLS20098          
C     --------------------------------------------                      FLS20099          
   13 GO TO (50,14),I3                                                  FLS20100          
   14 IF(XID(1))16,15,16                                                FLS20101          
   15 I4=I2                                                             FLS20102          
      I11=2                                                             FLS20103          
      GO TO 64                                                          FLS20104          
   16 IF(XID(1)-OID(1)) 18,17,18                                        FLS20105          
   17 GO TO (52,21,21,21,21),I2                                         FLS20106          
   18 I4=I2                                                             FLS20107          
      IF (I4-1) 19,19,20                                                FLS20108          
   19 JN = JN+1                                                         FLS20109          
      GO TO 64                                                          FLS20110          
   20 IN = IN+1                                                         FLS20111          
      JN = 1                                                            FLS20112          
      GO TO 64                                                          FLS20113          
   21 IF(XID(2)-OID(2))25,22,25                                         FLS20114          
   22 GO TO (23,52,28,28,28),I2                                         FLS20115          
   23 WRITE(6,24)                                                       FLS20116          
   24 FORMAT(48H0I2 ON CONTROL CD. 2 NOT EQ. XID IN READ OF READ)       FLS20117          
      KPUT=1                                                            FLS20118          
      RETURN                                                            FLS20119          
   25 I4 = I2-1                                                         FLS20120          
      IF (I4-1) 26,26,27                                                FLS20121          
   26 JN= JN+1                                                          FLS20122          
      GO TO 64                                                          FLS20123          
   27 IN = IN+1                                                         FLS20124          
      JN = 1                                                            FLS20125          
      GO TO 64                                                          FLS20126          
   28 IF(XID(3)-OID(3)) 31,281,31                                       FLS20127          
  281 GO TO (29,29,52,34,34),I2                                         FLS20128          
   29 WRITE(6,24)                                                       FLS20129          
      KPUT=1                                                            FLS20130          
      RETURN                                                            FLS20131          
   31 I4 = I2 - 2                                                       FLS20132          
      IF (I4 - 1) 32,32,33                                              FLS20133          
   32 JN = JN + 1                                                       FLS20134          
      GO TO 64                                                          FLS20135          
   33 IN = IN + 1                                                       FLS20136          
      JN = 1                                                            FLS20137          
      GO TO 64                                                          FLS20138          
   34 IF (XID(4) - OID(4)) 38,35,38                                     FLS20139          
   35 GO TO (36,36,36,52,41),I2                                         FLS20140          
   36 WRITE(6,24)                                                       FLS20141          
      KPUT=1                                                            FLS20142          
      RETURN                                                            FLS20143          
   38 I4=I2-3                                                           FLS20144          
      IF (I4-1) 39,39,40                                                FLS20145          
   39 JN = JN+1                                                         FLS20146          
      GO TO 64                                                          FLS20147          
   40 IN = IN+1                                                         FLS20148          
      JN = 1                                                            FLS20149          
      GO TO 64                                                          FLS20150          
C  41 IF(XID(5)-OID(5))45,42,45                                         FLS20151          
   41 IF(XID(5)-OID(5))49,52,49                                         FLS20152          
   42 GO TO (43,43,43,43,52,48),I2                                      FLS20153          
   43 WRITE(6,24)                                                       FLS20154          
      KPUT=1                                                            FLS20155          
      RETURN                                                            FLS20156          
   45 I4 = I2-4                                                         FLS20157          
      IF (I4 - 1) 46,46,47                                              FLS20158          
   46 JN = JN + 1                                                       FLS20159          
      GO TO 64                                                          FLS20160          
   47 IN = IN + 1                                                       FLS20161          
      JN = 1                                                            FLS20162          
      GO TO 64                                                          FLS20163          
C  48 IF(XID(6)-OID(6))111,109,111                                      FLS20164          
   48 IF(XID(6)-OID(6))49,52,49                                         FLS20165          
   49 I4=1                                                              FLS20166          
      JN = JN + 1                                                       FLS20167          
      GO TO 64                                                          FLS20168          
   50 I3=2                                                              FLS20169          
      DO 51 I=1,I2                                                      FLS20170          
   51 OID(I) =XID(I)                                                    FLS20171          
   52 DO 53 I=1,I1                                                      FLS20172          
      SX(1,I)= SX(1,I)+X(I)                                             FLS20173          
   53 RXS(I) = RXS(I) + X(I)                                            FLS20174          
      IF (IN - NAN) 54,54,55                                            FLS20175          
   54 IF (JN - NB) 60,60,55                                             FLS20176          
   55 PRINT 56,(XID(I),I = 1,I2)                                        FLS20177          
   56 FORMAT(2X,39H0COUNTER EXCEEDS DIMENSION AT OBSERVN.-,/1H ,7F15.0) FLS20178          
   57 PRINT 58,IN,JN                                                    FLS20179          
   58 FORMAT(41H  COMPARE DATA TO N5,N6 ON CONTROL CARD 2,              FLS20180          
     1/7H  IN = , I5/7H  JN = ,I5)                                      FLS20181          
      KPUT=1                                                            FLS20182          
      RETURN                                                            FLS20183          
   60 IF (Y) 61,62,61                                                   FLS20184          
   61 BV = BV+(0.25/Y)                                                  FLS20185          
      BV1(IN) = BV1(IN)+(0.25/Y)                                        FLS20186          
      BV2(IN,JN) = BV2(IN,JN) + (0.25/Y)                                FLS20187          
   62 AN(IN,JN) = AN(IN,JN) + 1.0                                       FLS20188          
      BN(IN) = BN(IN) + 1.0                                             FLS20189          
      DO 63 K1 =1,I1                                                    FLS20190          
      DO 63 K2 = K1,I1                                                  FLS20191          
   63 VC1 (K1,K2) = VC1 (K1,K2)+ X(K1)* X(K2)                           FLS20192          
      C1 = C1 + 1.0                                                     FLS20193          
      C2(1) = C2(1)+1.0                                                 FLS20194          
      GO TO 8                                                           FLS20195          
   64 DO 65 J=1,I1                                                      FLS20196          
      DO 65 K=J,I1                                                      FLS20197          
   65 SM(1,J,K)=SM(1,J,K)+DIVF(SX(1,J)*SX(1,K),C2(1))                   FLS20198          
      IF (I4-2) 69,66,66                                                FLS20199          
   66 DO 67 I = 2,I4                                                    FLS20200          
      C2(I) = C2(I)+ C2(I-1)                                            FLS20201          
      DO 67 J = 1,I1                                                    FLS20202          
   67 SX(I,J)= SX(I,J)+SX(I-1,J)                                        FLS20203          
      DO 68 I = 2,I4                                                    FLS20204          
      DO 68 J = 1,I1                                                    FLS20205          
      DO 68 K = J,I1                                                    FLS20206          
   68 SM(I,J,K)=SM(I,J,K)+DIVF(SX(I,J)*SX(I,K),C2(I))                   FLS20207          
   69 DO 70 I =1,I4                                                     FLS20208          
   70 C3(I) = C3(I)+1.0                                                 FLS20209          
      DO 71 J=1,I1                                                      FLS20210          
   71 SX(I4+1,J)=SX(I4+1,J)+SX(I4,J)                                    FLS20211          
      C2(I4 +1) = C2(I4+1) +C2(I4)                                      FLS20212          
   72 GO TO (73,76),I11                                                 FLS20213          
   73 DO 74 I=1,I2                                                      FLS20214          
   74 OID(I)= XID(I)                                                    FLS20215          
      DO 75 I=1,I4                                                      FLS20216          
      C2(I) =0.0                                                        FLS20217          
      DO 75 J=1,I1                                                      FLS20218          
   75 SX(I,J) = 0.0                                                     FLS20219          
      GO TO 52                                                          FLS20220          
   76 DF(I2+2)=C1-1.0                                                   FLS20221          
      IF(C1) 215,215,664                                                FLS20222          
  215 WRITE(6,216)                                                      FLS20223          
  216 FORMAT(16H0NO OBSERVATIONS)                                       FLS20224          
      KPUT=-1                                                           FLS20225          
      RETURN                                                            FLS20226          
  664 I5 = I2-1                                                         FLS20227          
      I6 = 0                                                            FLS20228          
      DF(I2+1)= C1-C3(1)                                                FLS20229          
      DF(1)=C3(I2)-1.0                                                  FLS20230          
      DO 77 I = 1,I5                                                    FLS20231          
      I10 = I2-I6                                                       FLS20232          
      DF(I10) = C3(I)-C3(I+1)                                           FLS20233          
   77 I6 = I6+1                                                         FLS20234          
      I5 = I2-1                                                         FLS20235          
      I6 = 0                                                            FLS20236          
      DO 78  I=1, I1                                                    FLS20237          
      DO 78  J=I,I1                                                     FLS20238          
      XMN(I,J)=RXS(I)/C1                                                FLS20239          
      SS(I2+2,I,J)=  VC1(I,J)-((RXS(I)*RXS(J))/C1)                      FLS20240          
      SS(I2+1,I,J)=  VC1(I,J)-  SM(1,I,J)                               FLS20241          
   78 SS( 1,I,J) =  SM(I2,I,J) - ((RXS(I)*RXS(J))/C1)                   FLS20242          
      DO 80 I=1,I5                                                      FLS20243          
      I10 = I2 - I6                                                     FLS20244          
      DO 79 J = 1,I1                                                    FLS20245          
      DO 79 K = J,I1                                                    FLS20246          
   79 SS(I10,J,K) =  SM(I,J,K) -  SM(I+1,J,K)                           FLS20247          
   80 I6 = I6+1                                                         FLS20248          
      I6 = I2+1                                                         FLS20249          
      DO 81 I=1,I6                                                      FLS20250          
      DO 81 J=1,I1                                                      FLS20251          
      DO 81 K=J,I1                                                      FLS20252          
   81 SM(I,J,K)=DIVF(SS(I,J,K),DF(I))                                   FLS20253          
      I6 = I2+2                                                         FLS20254          
      DO 82 I= 1,I6                                                     FLS20255          
   82 IDF(I) = DF(I) +.5                                                FLS20256          
   83 DO 84 I=1,I2                                                      FLS20257          
      DO 84 J=1,I1                                                      FLS20258          
      DO 84 K=J,I1                                                      FLS20259          
   84 F(I,J,K)=DIVF(SM(I,J,K),SM(I+1,J,K))                              FLS20260          
      SUM = 0.0                                                         FLS20261          
      SUMB= 0.0                                                         FLS20262          
      DO  87  I=1,NAN                                                   FLS20263          
      IF(BN(I)) 85,87,85                                                FLS20264          
   85 SUM = 0.0                                                         FLS20265          
      DO  86   J=1,NB                                                   FLS20266          
   86 SUM = SUM + AN(I,J)**2                                            FLS20267          
      SUMB=SUMB+DIVF(SUM,BN(I))                                         FLS20268          
   87 CONTINUE                                                          FLS20269          
      Q1=DIVF(C1-SUMB,DF(I2))                                           FLS20270          
      SUM = 0.0                                                         FLS20271          
      DO 90  I =1,NAN                                                   FLS20272          
      IF (BN(I)) 88,90,88                                               FLS20273          
   88 DO 89  J=1,NB                                                     FLS20274          
      SUM=SUM+((AN(I,J)*DIVF(AN(I,J),BN(I)))-(AN(I,J)*AN(I,J)/C1))      FLS20275          
   89 CONTINUE                                                          FLS20276          
   90 CONTINUE                                                          FLS20277          
      SDF = 0.0                                                         FLS20278          
      JD = I2 - 1                                                       FLS20279          
      DO 91 I = 1,JD                                                    FLS20280          
   91 SDF = SDF + DF(I )                                                FLS20281          
      Q2=DIVF(SUM,SDF)                                                  FLS20282          
      SUM= 0.0                                                          FLS20283          
      DO 92 I= 1,NAN                                                    FLS20284          
   92 SUM = SUM + BN(I)* BN(I)                                          FLS20285          
      SUM = SUM/C1                                                      FLS20286          
      Q3=DIVF(C1-SUM,SDF)                                               FLS20287          
C                                                                       FLS20288          
      I6=I2-1                                                           FLS20289          
      I7=I2                                                             FLS20290          
      I8=I2+1                                                           FLS20291          
      I9=I2+2                                                           FLS20292          
      WRITE(6,214)                                                      FLS20293          
  214 FORMAT(34H0ERRORS AND/OR COMMENTS FOR FULSIB)                     FLS20294          
C                                                                       FLS20295          
C                                                                       FLS20296          
C     ANALYSIS-COMPLETE, NESTED ONLY, OR OFFSPRING-PARENT REGN. ONLY.   FLS20297          
C                                                                       FLS20298          
      GO TO (93,93,113),N3                                              FLS20299          
C     --------------------------------------------                      FLS20300          
C     SET UP TABLE OF COEFFICIENTS                                      FLS20301          
C     --------------------------------------------                      FLS20302          
   93 L(1) = Q3                                                         FLS20303          
      L(2) = Q1                                                         FLS20304          
      L(3) = Q1*Q3                                                      FLS20305          
      L(4) = Q3                                                         FLS20306          
      L(5) = Q1                                                         FLS20307          
      L(6) = L(3)                                                       FLS20308          
      L(7) = L(3)                                                       FLS20309          
      T2(1,1) = 1.0                                                     FLS20310          
      T2(2,1)=DIVF(-Q2,Q1)                                              FLS20311          
      T2(3,1)=DIVF(Q2-Q1,Q1)                                            FLS20312          
      T2(1,2) = 0.0                                                     FLS20313          
      T2(2,2) = 1.0                                                     FLS20314          
      T2(3,2) = -1.0                                                    FLS20315          
      T2(1,3) = Q1                                                      FLS20316          
      T2(2,3) = Q3-Q2                                                   FLS20317          
      T2(3,3) = Q2-Q1-Q3                                                FLS20318          
      T2(1,4) = -2.0                                                    FLS20319          
      T2(2,4) = -2.*T2(2,1)                                             FLS20320          
      T2(3,4)=DIVF(Q1-Q2,Q1)+Q3                                         FLS20321          
      T2(1,5) = 0.0                                                     FLS20322          
      T2(2,5) = -2.0                                                    FLS20323          
      T2(3,5) = Q1+2.0                                                  FLS20324          
      T2(1,6) = -Q1                                                     FLS20325          
      T2(2,6) =-T2(2,3)                                                 FLS20326          
      T2(3,6) = Q1-Q2+Q3*(Q1+1.)                                        FLS20327          
      T2(1,7) = Q1                                                      FLS20328          
      T2(2,7) = T2(2,3)                                                 FLS20329          
      T2(3,7) = Q2-Q1+Q3*(Q1-1.)                                        FLS20330          
C                                                                       FLS20331          
      IOF=IS+ID+1                                                       FLS20332          
C     --------------------------------------------                      FLS20333          
C     VARIATION.                                                        FLS20334          
C     --------------------------------------------                      FLS20335          
      DO 95 IK=IOF,I1                                                   FLS20336          
      DO 95 JK=IK,I1                                                    FLS20337          
      DO 94 KH=1,7                                                      FLS20338          
   94 TEMP(KH) = T2(1,KH)*SM(I2-1,IK,JK)+                               FLS20339          
     1           T2(2,KH)*SM(I2,  IK,JK)+                               FLS20340          
     2           T2(3,KH)*SM(I2+1,IK,JK)                                FLS20341          
C     GENIC VARIATION                                                   FLS20342          
      THETA(1,IK,JK)=DIVF(4.,Q3)*TEMP(1)                                FLS20343          
      THETA(2,IK,JK)=DIVF(4.,Q1)*TEMP(2)                                FLS20344          
      THETA(3,IK,JK)=DIVF(2.*TEMP(3),L(3))                              FLS20345          
C     ENVIRONMENTAL VARIATION                                           FLS20346          
      THETA(4,IK,JK)=DIVF(1.,Q3)*TEMP(4)                                FLS20347          
      THETA(5,IK,JK)=DIVF(1.,Q1)*TEMP(5)                                FLS20348          
      THETA(6,IK,JK)=DIVF(TEMP(6),L(6))                                 FLS20349          
C     PHENOTYPIC VARIATION                                              FLS20350          
      THETA(7,IK,JK)=DIVF(TEMP(7),L(7))                                 FLS20351          
      IF(DF(I8)) 951,951,95                                             FLS20352          
  951 THETA(4,IK,JK)=THETA(7,IK,JK)-THETA(1,IK,JK)                      FLS20353          
      THETA(2,IK,JK)=0.0                                                FLS20354          
      THETA(3,IK,JK)=0.0                                                FLS20355          
      THETA(5,IK,JK)=0.0                                                FLS20356          
      THETA(6,IK,JK)=0.0                                                FLS20357          
C                                                                       FLS20358          
   95 CONTINUE                                                          FLS20359          
      DO 96 IK=1,3                                                      FLS20360          
      DO 96 JK=1,7                                                      FLS20361          
   96 T2(IK,JK) = T2(IK,JK)*T2(IK,JK)                                   FLS20362          
C                                                                       FLS20363          
      DF(I6) = DF(I6)+2.                                                FLS20364          
      DF(I7) = DF(I7)+2.                                                FLS20365          
      DF(I8) = DF(I8)+2.                                                FLS20366          
      DO 98 IK=IOF,I1                                                   FLS20367          
      DO 98 JK=IK,I1                                                    FLS20368          
C     --------------------------------------------                      FLS20369          
C     VARIANCE COMPONENTS.                                              FLS20370          
C     --------------------------------------------                      FLS20371          
      VC3(IK,JK)=SM(I8,IK,JK)                                           FLS20372          
      VC2(IK,JK)=DIVF(SM(I7,IK,JK)-SM(I8,IK,JK),Q1)                     FLS20373          
      VC1(IK,JK)=DIVF(SM(I6,IK,JK)-Q2*VC2(IK,JK)-VC3(IK,JK),Q3)         FLS20374          
      COMENV(IK,JK)=VC2(IK,JK)-VC1(IK,JK)                               FLS20375          
      WFSENV(IK,JK)=VC3(IK,JK)-(2.*VC1(IK,JK))                          FLS20376          
      IF(DF(I8).EQ.2.0) WFSENV(IK,JK)=0.0                               FLS20377          
      IF(JK.NE.IK) GO TO 98                                             FLS20378          
      PROD1(IK,JK) = 2.*((SM(I6,IK,JK)**2/DF(I6))+(SM(I7,IK,JK)**2/DF(I7FLS20379          
     1)))*DIVF(1.,Q3*Q3)                                                FLS20380          
      PROD2(IK,JK) = 2.*((SM(I7,IK,JK)**2/DF(I7))+(SM(I8,IK,JK)**2/DF(I8FLS20381          
     1)))*DIVF(1.,Q1*Q1)                                                FLS20382          
      PROD3(IK,JK)=DIVF(-Q2*(PROD2(IK,JK)-DIVF(2.*SM(I8,IK,JK)**2,Q1*Q1*FLS20383          
     1DF(I8))),Q3)                                                      FLS20384          
   98 CONTINUE                                                          FLS20385          
      I1X = I1-1                                                        FLS20386          
      IF(I1X) 980,980,981                                               FLS20387          
  981 DO 106 IK=IOF,I1X                                                 FLS20388          
      K=IK+1                                                            FLS20389          
      DO 106 JK=K,I1                                                    FLS20390          
C     --------------------------------------------                      FLS20391          
C     HERITABILITY OF DIFFERENCE OF 2 TRAITS.                           FLS20392          
C     --------------------------------------------                      FLS20393          
      THETGS(IK,JK)=THETA(1,IK,IK)*THETA(1,JK,JK)                       FLS20394          
      IF(THETGS(IK,JK) .LE. 0.0) GO TO 991                              FLS20395          
      THETGS(IK,JK)=2.0*(SQRT(THETGS(IK,JK)))-2.0*THETA(1,IK,JK)        FLS20396          
      GO TO 992                                                         FLS20397          
  991 THETGS(IK,JK)=0.0                                                 FLS20398          
  992 THETPD(IK,JK)=THETA(7,IK,IK)*THETA(7,JK,JK)                       FLS20399          
      IF(THETPD(IK,JK) .LE. 0.0) GO TO 993                              FLS20400          
      THETPD(IK,JK)=2.0*(SQRT(THETPD(IK,JK)))-2.0*THETA(7,IK,JK)        FLS20401          
      GO TO 994                                                         FLS20402          
  993 THETPD(IK,JK)=0.0                                                 FLS20403          
  994 IF(THETPD(IK,JK) .GT. 0.0 .AND. THETGS(IK,JK) .NE. 0.0) GO TO 99  FLS20404          
      HDFS(I,J)=0.0                                                     FLS20405          
      PRINT 995,IK,JK                                                   FLS20406          
  995 FORMAT(27H0 HERIT. OF DIF. FOR CHARS.  ,I2,3HAND,I2,20HCANNOT BE CFLS20407          
     1ALCULATED)                                                        FLS20408          
      GO TO 100                                                         FLS20409          
   99 HDFS(IK,JK)=DIVF(THETGS(IK,JK),THETPD(IK,JK))                     FLS20410          
C     --------------------------------------------                      FLS20411          
C     COR. COEFS. AND THEIR STD. ERRORS                                 FLS20412          
C     --------------------------------------------                      FLS20413          
  100 DO 106 K1=1,7                                                     FLS20414          
      IF(THETA(K1,IK,IK) .GT. 0.0 .AND. THETA(K1,JK,JK) .GT. 0.0)       FLS20415          
     1GO TO 102                                                         FLS20416          
      RTHETA(K1,IK,JK) = 0.                                             FLS20417          
      SER(K1,IK,JK) = 0.                                                FLS20418          
      PRINT 101,K1,IK,JK                                                FLS20419          
  101 FORMAT(16H0 CORRELATION NO,I2,15H FOR CHARACTERS,I2,3HAND,I2,     FLS20420          
     1 20HCANNOT BE CALCULATED)                                         FLS20421          
      GO TO 106                                                         FLS20422          
  102 CONTINUE                                                          FLS20423          
      RTHETA(K1,IK,JK)=THETA(K1,IK,JK)/(SQTF(THETA(K1,IK,JK)*                             
     1                   THETA(K1,JK,JK)))                              FLS20425          
      IF(RTHETA(K1,IK,JK) .NE. 0.) GO TO 104                            FLS20426          
      SER(K1,IK,JK) = 0.                                                FLS20427          
      PRINT 103, K1,IK,JK                                               FLS20428          
  103 FORMAT( 1H029HSTD. ERROR FOR CORRELATION NO ,I2,15H FOR CHARACTERSFLS20429          
     1,I2,3HAND,I2,20HCANNOT BE CALCULATED )                            FLS20430          
      GO TO 106                                                         FLS20431          
  104 VAR = (T2(1,K1)*SM(I6,IK,IK)*SM(I6,IK,IK)/DF(I6)                  FLS20432          
     1      +T2(2,K1)*SM(I7,IK,IK)*SM(I7,IK,IK)/DF(I7)                  FLS20433          
     2      +T2(3,K1)*SM(I8,IK,IK)*SM(I8,IK,IK)/DF(I8))*                FLS20434          
     3DIVF(1.,4.*THETA(K1,IK,IK)*THETA(K1,IK,IK))                       FLS20435          
      VAR = VAR+(T2(1,K1)*SM(I6,JK,JK)*SM(I6,JK,JK)/DF(I6)              FLS20436          
     1         + T2(2,K1)*SM(I7,JK,JK)*SM(I7,JK,JK)/DF(I7)              FLS20437          
     2         + T2(3,K1)*SM(I8,JK,JK)*SM(I8,JK,JK)/DF(I8))             FLS20438          
     3*DIVF(1.,  4.*THETA(K1,JK,JK)*THETA(K1,JK,JK))                    FLS20439          
      VAR1(IK,JK) = VAR+(T2(1,K1)*SM(I6,IK,JK)*SM(I6,IK,JK)/DF(I6)      FLS20440          
     1         + T2(2,K1)*SM(I7,IK,JK)*SM(I7,IK,JK)/DF(I7)              FLS20441          
     2         + T2(3,K1)*SM(I8,IK,JK)*SM(I8,IK,JK)/DF(I8))             FLS20442          
     3*DIVF(1.,  2.*THETA(K1,IK,IK)*THETA(K1,JK,JK))                    FLS20443          
      VAR = VAR1(IK,JK)+(T2(1,K1)*(SM(I6,IK,IK)*SM(I6,JK,JK)+           FLS20444          
     1                     SM(I6,IK,JK)*SM(I6,IK,JK))/DF(I6)            FLS20445          
     2         + T2(2,K1)*(SM(I7,IK,IK)*SM(I7,JK,JK)+                   FLS20446          
     3                     SM(I7,IK,JK)*SM(I7,IK,JK))/DF(I7)            FLS20447          
     4         + T2(3,K1)*(SM(I8,IK,IK)*SM(I8,JK,JK)+                   FLS20448          
     5                     SM(I8,IK,JK)*SM(I8,IK,JK))/DF(I8))           FLS20449          
     6*DIVF(1.,  2.*THETA(K1,IK,JK)*THETA(K1,IK,JK))                    FLS20450          
      VAR = VAR-(T2(1,K1)*SM(I6,IK,JK)*SM(I6,IK,IK)/DF(I6)              FLS20451          
     1         + T2(2,K1)*SM(I7,IK,JK)*SM(I7,IK,IK)/DF(I7)              FLS20452          
     2         + T2(3,K1)*SM(I8,IK,JK)*SM(I8,IK,IK)/DF(I8))             FLS20453          
     3*DIVF(1.,  THETA(K1,IK,IK)*THETA(K1,IK,JK))                       FLS20454          
      VAR = VAR-(T2(1,K1)*SM(I6,JK,JK)*SM(I6,IK,JK)/DF(I6)              FLS20455          
     1         + T2(2,K1)*SM(I7,JK,JK)*SM(I7,IK,JK)/DF(I7)              FLS20456          
     2         + T2(3,K1)*SM(I8,JK,JK)*SM(I8,IK,JK)/DF(I8))             FLS20457          
     3*DIVF(1.,  THETA(K1,JK,JK)*THETA(K1,IK,JK))                       FLS20458          
      IF (K1 .NE. 7) VAR1(IK,JK) = 0.0                                  FLS20459          
      IF(VAR .LE. 0.) GO TO 105                                         FLS20460          
C                                                                       FLS20461          
      FKH = 2.0                                                         FLS20462          
      IF(K1 .EQ. 3) FKH = 8.0                                           FLS20463          
      IF(K1 .EQ. 1  .OR. K1 .EQ. 2) FKH = 32.0                          FLS20464          
      SER(K1,IK,JK)=SQTF(DIVF(FKH*RTHETA(K1,IK,JK)**2*VAR,L(K1)**2))    FLS20465          
      GO TO 106                                                         FLS20466          
  105 SER(K1,IK,JK) = 0.                                                FLS20467          
      PRINT 103, K1,IK,JK                                               FLS20468          
  106 CONTINUE                                                          FLS20469          
  980 DF(I6) = DF(I6)-2.                                                FLS20470          
      DF(I7) = DF(I7)-2.                                                FLS20471          
      DF(I8) = DF(I8)-2.                                                FLS20472          
C     --------------------------------------------                      FLS20473          
C     DATA BINOMIALLY DISTRIBUTED.                                      FLS20474          
C     --------------------------------------------                      FLS20475          
      BXA = 0.0                                                         FLS20476          
      DO 108 I=1,NAN                                                    FLS20477          
      IF(BN(I))107,108,107                                              FLS20478          
  107 BV1(I)=DIVF(BV1(I),BN(I))                                         FLS20479          
      BXA = BXA + BV1(I)                                                FLS20480          
  108 CONTINUE                                                          FLS20481          
      BXB = 0.0                                                         FLS20482          
      DO 110 I=1,NAN                                                    FLS20483          
      DO 110 J=1,NB                                                     FLS20484          
      IF(AN(I,J))109,110,109                                            FLS20485          
  109 BV2(I,J)=DIVF(BV2(I,J),AN(I,J))                                   FLS20486          
      BXB = BXB + BV2(I,J)                                              FLS20487          
  110 CONTINUE                                                          FLS20488          
      BIVAR1=DIVF(BXA-(BV/C1),SDF)                                      FLS20489          
      BIVAR2=DIVF(BXB-BXA,DF(I2))                                       FLS20490          
      BIVAR3=DIVF(BV-BXB,DF(I2+1))                                      FLS20491          
      DO 112 IK=IOF,I1                                                  FLS20492          
      IF(BIVAR1.LE.0.0) GO TO 111                                       FLS20493          
      VC3(IK,IK) = VC3(IK,IK)-BIVAR3                                    FLS20494          
      VC2(IK,IK)=DIVF(SM(I2,IK,IK)-VC3(IK,IK)-BIVAR2,Q1)                FLS20495          
      VC1(IK,IK)=DIVF(SM(I2-1,IK,IK)-Q2*VC2(IK,IK)-VC3(IK,IK)-BIVAR1,Q3)FLS20496          
      THETA(1,IK,IK)=4.0*VC1(IK,IK)                                     FLS20497          
      THETA(2,IK,IK)=4.0*VC2(IK,IK)                                     FLS20498          
      THETA(3,IK,IK)=2.0*(VC1(IK,IK)+VC2(IK,IK))                        FLS20499          
      THETA(7,IK,IK) = THETA(7,IK,IK)+0.25                              FLS20500          
C     --------------------------------------------                      FLS20501          
C     HERITABILITIES AND THEIR STD. ERRORS                              FLS20502          
C     --------------------------------------------                      FLS20503          
  111 HS(IK,IK)=DIVF(THETA(1,IK,IK),THETA(7,IK,IK))                     FLS20504          
      HD(IK,IK)=DIVF(THETA(2,IK,IK),THETA(7,IK,IK))                     FLS20505          
      HSD(IK,IK)=DIVF(THETA(3,IK,IK),THETA(7,IK,IK))                    FLS20506          
      SE1(IK,IK)=4.*DIVF(SQTF(PROD1(IK,IK)),THETA(7,IK,IK))             FLS20507          
      SE2(IK,IK)=4.*DIVF(SQTF(PROD2(IK,IK)),THETA(7,IK,IK))             FLS20508          
  112 SE3(IK,IK)=2.*DIVF(SQTF(PROD1(IK,IK)+PROD2(IK,IK)+2.*PROD3(IK,IK))FLS20509          
     1,THETA(7,IK,IK))                                                  FLS20510          
  113 CONTINUE                                                          FLS20511          
      RETURN                                                            FLS20512          
      END                                                               FLS20513          
*ENDTEXT                                                                                  
