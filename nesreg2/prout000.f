*TEXT                                                                                     
      SUBROUTINE PROUT(IIA,IIB,IIC,                                     PRT20001          
     1T2,COMENV,WFSENV,DF,IDF,SS,SM,F,XMN,VC1,VC2,VC3,HS,HD,HSD,SE1,SE2,PRT20002          
     2SE3,THETA,RTHETA,SER,THETGS,THETPD,HDFS,VAR1,NAMVAR,              PRT20003          
     3REGSS,DEVSS,SE4,HB,HR,RB,SRR,DEVSM,FREG,B,FDEV,CORS,CORD,CHS,VA,  PRT20004          
     4VD,VAA,SL1,SL2,SL3,SCR1,SESCR1,SCR2,SESCR2,A,FKH,LS,XMAT1,XMAT2,  PRT20005          
     5TS)                                                               PRT20006          
C-----VERSION 2                                                         PRT20007          
      COMMON/CMBLK1/I1,I2,N3,N4,N5,N6,IS,ID,IM,IF,N1,ITLE(9),Y,KPUT     PRT20008          
      COMMON/CMBLK5/NPROB,Q1,Q2,Q3,BIVAR1,BIVAR2,BIVAR3,I6,I7,I8,I9     PRT20009          
      COMMON/CMBLK6/ DFS,DFD,IDFS,IDFD                                  PRT20010          
      DIMENSION T2(3,7),COMENV(IIA,1),WFSENV(IIA,1),DF(1),IDF(1),       PRT20011          
     1 SS(IIC,IIA,1),SM(IIB,IIA,1),F(IIB,IIA,1),XMN(IIA,1),VC1(IIA,1),  PRT20012          
     2 VC2(IIA,1),VC3(IIA,1),HS(IIA,1),HD(IIA,1),HSD(IIA,1),SE1(IIA,1), PRT20013          
     3 SE2(IIA,1),SE3(IIA,1),THETA(7,IIA,1),RTHETA(7,IIA,1),SER(7,IIA,1)PRT20014          
     4 ,THETGS(IIA,1),THETPD(IIA,1),HDFS(IIA,1),VAR1(IIA,1),NAMVAR(1)   PRT20015          
      DIMENSION REGSS(4,IIA,1),DEVSS(4,IIA,1),SE4(4,IIA,1),HB(4,IIA,1), PRT20016          
     1HR(4,IIA,1),RB(4,IIA,1),SRR(4,IIA,1),DEVSM(4,IIA,1),FREG(4,IIA,1),PRT20017          
     2B(4,IIA,1),FDEV(4,IIA,1),CORS(IIA,1),CORD(IIA,1),CHS(IIA,1),      PRT20018          
     3VA(IIA,1),VD(IIA,1),VAA(IIA,1),SL1(IIA,1),SL2(IIA,1),SL3(IIA,1),  PRT20019          
     4SCR1(3,IIA,1),SESCR1(3,IIA,1),SCR2(4,IIA,1),SESCR2(4,IIA,1),      PRT20020          
     5A(4,IIA,1),FKH(3),LS(3),XMAT1(IIA,1),XMAT2(IIA,1),TS(3,7)         PRT20021          
      REAL LS                                                           PRT20022          
      IOF=IS+ID+1                                                       PRT20023          
      IOFM=IOF+IM-1                                                     PRT20024          
   10 DO 520 I=IOF,I1                                                   PRT20025          
      II=I-IS-ID                                                        PRT20026          
      KK=I-IOFM                                                         PRT20027          
C-----P-P CORRELATION                                                   PRT20028          
C-----MM IS SIRE TRAIT , NN IS DAM TRAIT                                PRT20029          
      IF(IS) 11,11,30                                                   PRT20030          
   30 IF(ID) 11,11,31                                                   PRT20031          
   31 CONTINUE                                                          PRT20032          
      IF(KK) 16,16,17                                                   PRT20033          
   16 MM=II                                                             PRT20034          
      NN=I-ID                                                           PRT20035          
      GO TO 18                                                          PRT20036          
   17 MM=II-IM                                                          PRT20037          
      NN=I-ID-IM                                                        PRT20038          
   18 CONTINUE                                                          PRT20039          
      R=SS(I6,MM,MM)*(SS(I6,NN,NN)+SS(I7,NN,NN))                        PRT20040          
      IF(R) 11,11,13                                                    PRT20041          
   11 R=0.0                                                             PRT20042          
      GO TO 12                                                          PRT20043          
   13 R=SS(I6,MM,NN)/SQRT(R)                                            PRT20044          
   12 CONTINUE                                                          PRT20045          
C-----                                                                  PRT20046          
      DO 520 J= I,I1                                                    PRT20047          
      JJ=J-IS-ID                                                        PRT20048          
      LL=J-IOFM                                                         PRT20049          
      WRITE (6,40)N1,I,J,NAMVAR(I),NAMVAR(J)                            PRT20050          
   40 FORMAT(1H1 50X11HPROBLEM NO. I4,   /32X11HVARIABLES  I2,2X5HAND  IPRT20051          
     12,10X,A8,7H  AND  ,A8)                                            PRT20052          
      IF(J .NE. I) GO TO 90                                             PRT20053          
      GO TO (50,70,50),N3                                               PRT20054          
   50 WRITE(6,60) XMN(II,JJ),R                                          PRT20055          
   60 FORMAT(1H 80X13HPARENT MEAN =,E13.6,17H    P-P CORLN. = ,F7.4)    PRT20056          
   70 WRITE(6,80) XMN(I,J)                                              PRT20057          
   80 FORMAT(1H 79X14HPROGENY MEAN =,E13.6)                             PRT20058          
   90 WRITE(6,100)                                                      PRT20059          
  100 FORMAT(1H  41X34HANALYSIS OF VARIANCE (COVARIANCE)                PRT20060          
     1//14X9HLEVEL NO.,12X10HDEGREES OF 13X6HSUM OF 15X4HMEAN 14X8HCOMPUPRT20061          
     2TED / 14X8H(SOURCE) 14X7HFREEDOM 15X7HSQUARES 13X6HSQUARE 13X8HF-VPRT20062          
     3ALUES)                                                            PRT20063          
      I20=I2+1                                                          PRT20064          
      DO 140 K=1,I20                                                    PRT20065          
      WRITE (6,130) K,IDF(K),SS(K,I,J),SM(K,I,J),F(K,I,J)               PRT20066          
  130 FORMAT(1H  16XI1,18XI5,12XE15.8,5XE15.8, 8XE15.8)                 PRT20067          
  140 CONTINUE                                                          PRT20068          
      IF (N3 .EQ. 2 .OR. I .NE. J) GO TO 220                            PRT20069          
      IF (IS .LE. 0) GO TO 180                                          PRT20070          
      IF (I .GT. IOFM) GO TO 160                                        PRT20071          
      WRITE(6,150) REGSS(1,II,JJ),REGSS(1,II,JJ),FREG(1,II,JJ),IDFS,    PRT20072          
     1DEVSS(1,II,JJ),DEVSM(1,II,JJ),FDEV(1,II,JJ)                       PRT20073          
  150 FORMAT(1H07X16HSIRES REGRESSION,16X1H1,12XE15.8,5XE15.8,8XE15.8,  PRT20074          
     1  /14X10HDEVIATIONS,12XI5,12XE15.8,5XE15.8,8XE15.8)               PRT20075          
      GO TO 180                                                         PRT20076          
  160 WRITE(6,170) REGSS(2,KK,LL),REGSS(2,KK,LL),FREG(2,KK,LL),IDFS,    PRT20077          
     1DEVSS(2,KK,LL),DEVSM(2,KK,LL),FDEV(2,KK,LL)                       PRT20078          
  170 FORMAT(1H07X16HSIRES REGRESSION,16X1H1,12XE15.8,5XE15.8,8XE15.8,  PRT20079          
     1  /14X10HDEVIATIONS,12XI5,12XE15.8,5XE15.8,8XE15.8)               PRT20080          
  180 IF (ID .LE. 0) GO TO 220                                          PRT20081          
      IF (I .GT. IOFM) GO TO 200                                        PRT20082          
      WRITE(6,190) REGSS(3,II,JJ),REGSS(3,II,JJ),FREG(3,II,JJ),IDFD,    PRT20083          
     1DEVSS(3,II,JJ),DEVSM(3,II,JJ),FDEV(3,II,JJ)                       PRT20084          
  190 FORMAT(1H06X17HDAMS/S REGRESSION,16X1H1,12XE15.8,5XE15.8,8XE15.8, PRT20085          
     1  /14X10HDEVIATIONS,12XI5,12XE15.8,5XE15.8,8XE15.8)               PRT20086          
      GO TO 220                                                         PRT20087          
  200 WRITE(6,210) REGSS(4,KK,LL),REGSS(4,KK,LL),FREG(4,KK,LL),IDFD,    PRT20088          
     1DEVSS(4,KK,LL),DEVSM(4,KK,LL),FDEV(4,KK,LL)                       PRT20089          
  210 FORMAT(1H06X17HDAMS/S REGRESSION,16X1H1,12XE15.8,5XE15.8,8XE15.8, PRT20090          
     1  /14X10HDEVIATIONS,12XI5,12XE15.8,5XE15.8,8XE15.8)               PRT20091          
  220 WRITE (6,230) IDF(I2+2),SS(I2+2,I,J)                              PRT20092          
  230 FORMAT(1H0 12X5HTOTAL 18XI5,12XE15.8)                             PRT20093          
      GO TO (240,240,251),N3                                            PRT20094          
  240 WRITE (6,250) VC3(I,J),THETA(7,I,J),THETA(3,I,J),THETA(6,I,J),    PRT20095          
     1VC2(I,J),THETA(2,I,J),THETA(5,I,J),VC1(I,J),THETA(1,I,J),THETA(4,IPRT20096          
     2,J)                                                               PRT20097          
  250 FORMAT(1H ,90X21HVARIANCE (COVARIANCE)//76X10HPHENOTYPIC,8X7HGENETPRT20098          
     1IC,10X13HENVIRONMENTAL,//4X28HVARIANCE COMPONENT 3 (ERROR),5XE20.8PRT20099          
     2,13X3E18.8,3X5H(S+D),//4X26HVARIANCE COMPONENT 2 (DAM),7XE20.8,31XPRT20100          
     32E18.8,3X5H(DAM),//4X27HVARIANCE COMPONENT 1 (SIRE),6XE20.8,31X2E1PRT20101          
     48.8,2X6H(SIRE))                                                   PRT20102          
  251 IF(J .NE. I) GO TO 350                                            PRT20103          
      WRITE(6,252)                                                      PRT20104          
      GO TO (253,253,270),N3                                            PRT20105          
  252 FORMAT(1H //45X30HHERITABILITIES AND STD. ERRORS ,//14X6H(SIRE),  PRT20106          
     137X5H(DAM),37X5H(S+D))                                            PRT20107          
  253 WRITE(6,260)  HS(I,J),SE1(I,J),HD(I,J),SE2(I,J),HSD(I,J),SE3(I,J) PRT20108          
  260 FORMAT(1H0,56X16HOF SIB ANALYSIS ,/1H02X3(3X,2E20.8),/)           PRT20109          
      GO TO (270,460),N3                                                PRT20110          
  270 WRITE(6,400)                                                      PRT20111          
      IF (IS .LE. 0) GO TO 310                                          PRT20112          
      IF (I .GT. IOFM) GO TO 290                                        PRT20113          
      WRITE(6,280) HB(1,II,JJ),SE4(1,II,JJ),CHS(II,JJ), HR(1,II,JJ)     PRT20114          
  280 FORMAT( 1H0 5X2E20.8,21X,41HHOMEOSTATIC COEF.(=0.0 IF NO NAT. SEL.PRT20115          
     1) =,E15.8,/  11H O-P CORLN.,E15.8)                                PRT20116          
      GO TO 310                                                         PRT20117          
  290 WRITE(6,300) HB(2,KK,LL),SE4(2,KK,LL),CORS(KK,LL),HR(2,KK,LL)     PRT20118          
  300 FORMAT( 1H0 5X2E20.8,20H CORRECTION APPLD. =,E15.8,/  11H O-P CORLPRT20119          
     1N.,E15.8)                                                         PRT20120          
  310 IF (ID .LE. 0) GO TO 460                                          PRT20121          
      IF (I .GT. IOFM) GO TO 330                                        PRT20122          
      WRITE(6,320) HB(3,II,JJ),SE4(3,II,JJ),CORD(II,JJ),HR(3,II,JJ)     PRT20123          
  320 FORMAT( 1H048X2E20.8,20H CORRECTION APPLD. =,E15.8,/44X10HO-P CORLPRT20124          
     1N.,E15.8)                                                         PRT20125          
      GO TO 500                                                         PRT20126          
  330 WRITE(6,340) HB(4,KK,LL),SE4(4,KK,LL),            HR(4,KK,LL)     PRT20127          
  340 FORMAT( 1H048X2E20.8,/43X11H O-P CORLN.,E15.8/)                   PRT20128          
      GO TO 460                                                         PRT20129          
  350 GO TO (351,351,381),N3                                            PRT20130          
  351 WRITE(6,360)I,J,HDFS(I,J),THETPD(I,J),THETGS(I,J)                 PRT20131          
  360 FORMAT(1H0 25H DIFFERENCE OF VARIABLES ,I2,4H AND,I3,6X,11HHERIT. PRT20132          
     1SIRE,E17.8,E19.8,E18.8,                           ///50X28HCORRELAPRT20133          
     2TIONS AND STD. ERRORS,//23X6H(SIRE),33X5H(DAM),34X5H(S+D),        PRT20134          
     3//57X16HOF SIB ANALYSIS )                                         PRT20135          
  370 WRITE (6,380)  (RTHETA(K1,I,J),SER(K1,I,J),K1=1,7),(SCR1(K,I,J),SEPRT20136          
     1SCR1(K,I,J),K=1,3)                                                PRT20137          
  380 FORMAT( 1H013HGENETIC      ,3(3X,2E18.8) /1H013HENVIRONMENTAL,3(3XPRT20138          
     12E18.8) /1H013HPHENOTYPIC   ,81X2E18.8,//1H013HS.C.R.       ,3(3X,PRT20139          
     22E18.8)//)                                                        PRT20140          
      GO TO (390,501),N3                                                PRT20141          
  381 WRITE(6,382)                                                      PRT20142          
  382 FORMAT(1H0,50X,28HCORRELATIONS AND STD. ERRORS )                  PRT20143          
  390 IF(IS .LE. 1) GO TO 430                                           PRT20144          
      IF (J .GT. IOFM) GO TO 420                                        PRT20145          
      WRITE(6,400)                                                      PRT20146          
  400 FORMAT(1H 56X17HOF O-P REGRESSION)                                PRT20147          
      WRITE(6,410) RB(1,II,JJ),SRR(1,II,JJ),A(1,II,JJ),SCR2(1,II,JJ),   PRT20148          
     1SESCR2(1,II,JJ)                                                   PRT20149          
  410 FORMAT( 1H013HSIRES GENETIC,3X2E18.8,19H  C.V.(DEN.COVS.) =,E15.8,PRT20150          
     1/14H       S.C.R. ,3X2E18.8)                                      PRT20151          
      GO TO 430                                                         PRT20152          
  420 IF (I .LE. IOFM) GO TO 430                                        PRT20153          
      WRITE(6,400)                                                      PRT20154          
      WRITE(6,410) RB(2,KK,LL),SRR(2,KK,LL),A(2,KK,LL),SCR2(2,KK,LL),   PRT20155          
     1SESCR2(2,KK,LL)                                                   PRT20156          
  430 IF (ID .LE. 1) GO TO 460                                          PRT20157          
      IF (J .GT. IOFM) GO TO 450                                        PRT20158          
      IF (IS .GT. 1) GO TO 431                                          PRT20159          
      WRITE(6,400)                                                      PRT20160          
  431 WRITE(6,440) RB(3,II,JJ),SRR(3,II,JJ),A(3,II,JJ),SCR2(3,II,JJ),   PRT20161          
     1SESCR2(3,II,JJ)                                                   PRT20162          
  440 FORMAT( 1H041X12HDAMS GENETIC,2X2E18.8,19H  C.V.(DEN.COVS.) =,    PRT20163          
     1E15.8,/42X12H     S.C.R. ,2X2E18.8/)                              PRT20164          
      GO TO 500                                                         PRT20165          
  450 IF (I .LE. IOFM) GO TO 500                                        PRT20166          
  451 IF (IS .GT. 1) GO TO 452                                          PRT20167          
      WRITE(6,400)                                                      PRT20168          
  452 WRITE(6,440) RB(4,KK,LL),SRR(4,KK,LL),A(4,KK,LL),SCR2(4,KK,LL),   PRT20169          
     1SESCR2(4,KK,LL)                                                   PRT20170          
  460 GO TO (461,462,511),N3                                            PRT20171          
  462 IF(IM .NE. IF) GO TO 470                                          PRT20172          
      IF(I .LE. IM) GO TO 500                                           PRT20173          
  461 IF (J .NE.I) GO TO 501                                            PRT20174          
  470 IF(IF .EQ. 0) GO TO 480                                           PRT20175          
      WRITE(6,490) VA(KK,LL),VD(KK,LL),VAA(KK,LL),SL1(KK,LL),SL2(KK,LL),PRT20176          
     1SL3(KK,LL),XMAT1(KK,LL),XMAT2(KK,LL)                              PRT20177          
      GO TO 501                                                         PRT20178          
  480 WRITE(6,490) VA(II,JJ),VD(II,JJ),VAA(II,JJ),SL1(II,JJ),SL2(II,JJ),PRT20179          
     1SL3(II,JJ),XMAT1(II,JJ),XMAT2(II,JJ)                              PRT20180          
  490 FORMAT(1H  37X43HPRELIMINARY PARTITIONING OF PHEN. VARIANCE ,//21XPRT20181          
     18HADDITIVE,E19.8,/21X9HDOMINANCE,E18.8,/21X11HADD. * ADD.,E16.8,8XPRT20182          
     2,3H(2),15X,3H(3),/21X,9HSEX LINK.,3E18.8,/21X,11HMAT. EFFECT,E16.8PRT20183          
     3,E18.8)                                                           PRT20184          
  500 GO TO (501,501,511),N3                                            PRT20185          
  501 WRITE (6,510) COMENV(I,J),WFSENV(I,J)                             PRT20186          
  510 FORMAT(1H0 1X27HCOMMON ENVIR. VAR. (COVAR.),E19.8,/30H  W/IN FS ENPRT20187          
     1VIR. VAR. (COVAR.),E18.8)                                         PRT20188          
C 501 WRITE (6,510) Q3, Q2, Q1,BIVAR1,BIVAR2,BIVAR3                     PRT20189          
C 501 FORMAT(1H0,13X42HCOEFFICIENTS OF VARIANCE COMPONENTS (1) = ,F10.5,PRT20190          
C    14X,6H(2) = ,F10.5,4X,6H(3) = ,F10.5,  //13X102HBINOMIAL VARIANCE CPRT20191          
C    2OMPONENTS FOR ARCSIN TRANSFORM.(FOR ADJUSTING GIVEN HERITABILITY  PRT20192          
C    3ESTIMATES IF SUCH,  //13X19HVARIABLES WERE USED,17X6H(3) = ,E15.8,PRT20193          
C    44X6H(2) = ,E15.8,4X6H(1) = ,E15.8)                                PRT20194          
  511 WRITE(6,512)Q1,Q2,Q3                                              PRT20195          
  512 FORMAT(1H ,6X55HCOEFFICIENTS OF VARIANCE (COVARIANCE) COMPONENTS (PRT20196          
     11) = ,F10.5,4X,6H(2) = ,F10.5,4X,6H(3) = ,F10.5)                  PRT20197          
  520 CONTINUE                                                          PRT20198          
      RETURN                                                            PRT20199          
      END                                                               PRT20200          
*ENDTEXT                                                                                  
