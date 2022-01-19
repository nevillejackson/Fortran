*TEXT                                                                                     
      SUBROUTINE PROUT(IIA,IIB,IIC,                                     PRT30001          
     1T2,COMENV,WFSENV,DF,IDF,SS,SM,F,XMN,VC1,VC2,VC3,HS,HD,HSD,SE1,SE2,PRT30002          
     2SE3,THETA,RTHETA,SER,THETGS,THETPD,HDFS,VAR1,NAMVAR,              PRT30003          
     3REGSS,DEVSS,SE4,HB,HR,RB,SRR,DEVSM,FREG,B,FDEV,CORS,CORD,CHS,VA,  PRT30004          
     4VD,VAA,SL1,SL2,SL3,SCR1,SESCR1,SCR2,SESCR2,A,FKH,LS,XMAT1,XMAT2,  PRT30005          
     5TS)                                                               PRT30006          
C-----VERSION 3                                                         PRT30007          
      COMMON/CMBLK1/I1,I2,N3,N4,N5,N6,IS,ID,IM,IF,N1,ITLE(9),Y,KPUT     PRT30008          
      COMMON/CMBLK5/NPROB,Q1,Q2,Q3,BIVAR1,BIVAR2,BIVAR3,I6,I7,I8,I9     PRT30009          
      COMMON/CMBLK6/ DFS,DFD,IDFS,IDFD                                  PRT30010          
      COMMON/CMBLK7/ MI1,MIS,MID,MIM,MIF,M1,M2,ICYC,LUI,LUS             PRT30011          
      DIMENSION T2(3,7),COMENV(IIA,1),WFSENV(IIA,1),DF(1),IDF(1),       PRT30012          
     1 SS(IIC,IIA,1),SM(IIB,IIA,1),F(IIB,IIA,1),XMN(IIA,1),VC1(IIA,1),  PRT30013          
     2 VC2(IIA,1),VC3(IIA,1),HS(IIA,1),HD(IIA,1),HSD(IIA,1),SE1(IIA,1), PRT30014          
     3 SE2(IIA,1),SE3(IIA,1),THETA(7,IIA,1),RTHETA(7,IIA,1),SER(7,IIA,1)PRT30015          
     4 ,THETGS(IIA,1),THETPD(IIA,1),HDFS(IIA,1),VAR1(IIA,1),NAMVAR(1)   PRT30016          
      DIMENSION REGSS(4,IIA,1),DEVSS(4,IIA,1),SE4(4,IIA,1),HB(4,IIA,1), PRT30017          
     1HR(4,IIA,1),RB(4,IIA,1),SRR(4,IIA,1),DEVSM(4,IIA,1),FREG(4,IIA,1),PRT30018          
     2B(4,IIA,1),FDEV(4,IIA,1),CORS(IIA,1),CORD(IIA,1),CHS(IIA,1),      PRT30019          
     3VA(IIA,1),VD(IIA,1),VAA(IIA,1),SL1(IIA,1),SL2(IIA,1),SL3(IIA,1),  PRT30020          
     4SCR1(3,IIA,1),SESCR1(3,IIA,1),SCR2(4,IIA,1),SESCR2(4,IIA,1),      PRT30021          
     5A(4,IIA,1),FKH(3),LS(3),XMAT1(IIA,1),XMAT2(IIA,1),TS(3,7)         PRT30022          
      REAL LS                                                           PRT30023          
      IOF=IS+ID+1                                                       PRT30024          
      IOFM=IOF+IM-1                                                     PRT30025          
   10 DO 520 I=IOF,I1                                                   PRT30026          
      II=I-IS-ID                                                        PRT30027          
      KK=I-IOFM                                                         PRT30028          
C-----P-P CORRELATION                                                   PRT30029          
C-----MM IS SIRE TRAIT , NN IS DAM TRAIT                                PRT30030          
      IF(IS) 11,11,30                                                   PRT30031          
   30 IF(ID) 11,11,29                                                   PRT30032          
   29 CONTINUE                                                          PRT30033          
      IF(KK) 16,16,17                                                   PRT30034          
   16 MM=II                                                             PRT30035          
      NN=I-ID                                                           PRT30036          
      GO TO 18                                                          PRT30037          
   17 MM=II-IM                                                          PRT30038          
      NN=I-ID-IM                                                        PRT30039          
   18 CONTINUE                                                          PRT30040          
      R=SS(I6,MM,MM)*(SS(I6,NN,NN)+SS(I7,NN,NN))                        PRT30041          
      IF(R) 11,11,13                                                    PRT30042          
   11 R=0.0                                                             PRT30043          
      GO TO 12                                                          PRT30044          
   13 R=SS(I6,MM,NN)/SQRT(R)                                            PRT30045          
   12 CONTINUE                                                          PRT30046          
C-----                                                                  PRT30047          
      DO 520 J= I,I1                                                    PRT30048          
      JJ=J-IS-ID                                                        PRT30049          
      LL=J-IOFM                                                         PRT30050          
      GO TO(1,2,2,2),ICYC                                               PRT30051          
    1 IPR=I                                                             PRT30052          
      JPR=J                                                             PRT30053          
      GO TO 3                                                           PRT30054          
    2 IF(M1-M2) 4,5,4                                                   PRT30055          
    5 GO TO (41,42),II                                                  PRT30056          
   41 IPR=MIS+MID+M1                                                    PRT30057          
      GO TO 43                                                          PRT30058          
   42 IPR=MIS+MID+MIM+M1                                                PRT30059          
   43 GO TO (44,45),JJ                                                  PRT30060          
   44 JPR=MIS+MID+M1                                                    PRT30061          
      GO TO 3                                                           PRT30062          
   45 JPR=MIS+MID+MIM+M1                                                PRT30063          
      GO TO 3                                                           PRT30064          
    4 GO TO (31,32,33,34),II                                            PRT30065          
   31 IPR=MIS+MID+M1                                                    PRT30066          
      GO TO 35                                                          PRT30067          
   32 IPR=MIS+MID+M2                                                    PRT30068          
      GO TO 35                                                          PRT30069          
   33 IPR=MIS+MID+MIM+M1                                                PRT30070          
      GO TO 35                                                          PRT30071          
   34 IPR=MIS+MID+MIM+M2                                                PRT30072          
   35 GO TO (36,37,38,39),JJ                                            PRT30073          
   36 JPR=MIS+MID+M1                                                    PRT30074          
      GO TO 3                                                           PRT30075          
   37 JPR=MIS+MID+M2                                                    PRT30076          
      GO TO 3                                                           PRT30077          
   38 JPR=MIS+MID+MIM+M1                                                PRT30078          
      GO TO 3                                                           PRT30079          
   39 JPR=MIS+MID+MIM+M2                                                PRT30080          
    3 WRITE(6,40) N1,IPR,JPR,NAMVAR(IPR),NAMVAR(JPR)                    PRT30081          
   40 FORMAT(1H1 50X11HPROBLEM NO. I4,   /32X11HVARIABLES  I2,2X5HAND  IPRT30082          
     12,10X,A8,7H  AND  ,A8)                                            PRT30083          
      IF(J .NE. I) GO TO 90                                             PRT30084          
      GO TO (50,70,50),N3                                               PRT30085          
   50 WRITE(6,60) XMN(II,JJ),R                                          PRT30086          
   60 FORMAT(1H 80X13HPARENT MEAN =,E13.6,17H    P-P CORLN. = ,F7.4)    PRT30087          
   70 WRITE(6,80) XMN(I,J)                                              PRT30088          
   80 FORMAT(1H 79X14HPROGENY MEAN =,E13.6)                             PRT30089          
   90 WRITE(6,100)                                                      PRT30090          
  100 FORMAT(1H  41X34HANALYSIS OF VARIANCE (COVARIANCE)                PRT30091          
     1//14X9HLEVEL NO.,12X10HDEGREES OF 13X6HSUM OF 15X4HMEAN 14X8HCOMPUPRT30092          
     2TED / 14X8H(SOURCE) 14X7HFREEDOM 15X7HSQUARES 13X6HSQUARE 13X8HF-VPRT30093          
     3ALUES)                                                            PRT30094          
      I20=I2+1                                                          PRT30095          
      DO 140 K=1,I20                                                    PRT30096          
      WRITE (6,130) K,IDF(K),SS(K,I,J),SM(K,I,J),F(K,I,J)               PRT30097          
  130 FORMAT(1H  16XI1,18XI5,12XE15.8,5XE15.8, 8XE15.8)                 PRT30098          
  140 CONTINUE                                                          PRT30099          
      IF (N3 .EQ. 2 .OR. I .NE. J) GO TO 220                            PRT30100          
      IF (IS .LE. 0) GO TO 180                                          PRT30101          
      IF (I .GT. IOFM) GO TO 160                                        PRT30102          
      WRITE(6,150) REGSS(1,II,JJ),REGSS(1,II,JJ),FREG(1,II,JJ),IDFS,    PRT30103          
     1DEVSS(1,II,JJ),DEVSM(1,II,JJ),FDEV(1,II,JJ)                       PRT30104          
  150 FORMAT(1H07X16HSIRES REGRESSION,16X1H1,12XE15.8,5XE15.8,8XE15.8,  PRT30105          
     1  /14X10HDEVIATIONS,12XI5,12XE15.8,5XE15.8,8XE15.8)               PRT30106          
      GO TO 180                                                         PRT30107          
  160 WRITE(6,170) REGSS(2,KK,LL),REGSS(2,KK,LL),FREG(2,KK,LL),IDFS,    PRT30108          
     1DEVSS(2,KK,LL),DEVSM(2,KK,LL),FDEV(2,KK,LL)                       PRT30109          
  170 FORMAT(1H07X16HSIRES REGRESSION,16X1H1,12XE15.8,5XE15.8,8XE15.8,  PRT30110          
     1  /14X10HDEVIATIONS,12XI5,12XE15.8,5XE15.8,8XE15.8)               PRT30111          
  180 IF (ID .LE. 0) GO TO 220                                          PRT30112          
      IF (I .GT. IOFM) GO TO 200                                        PRT30113          
      WRITE(6,190) REGSS(3,II,JJ),REGSS(3,II,JJ),FREG(3,II,JJ),IDFD,    PRT30114          
     1DEVSS(3,II,JJ),DEVSM(3,II,JJ),FDEV(3,II,JJ)                       PRT30115          
  190 FORMAT(1H06X17HDAMS/S REGRESSION,16X1H1,12XE15.8,5XE15.8,8XE15.8, PRT30116          
     1  /14X10HDEVIATIONS,12XI5,12XE15.8,5XE15.8,8XE15.8)               PRT30117          
      GO TO 220                                                         PRT30118          
  200 WRITE(6,210) REGSS(4,KK,LL),REGSS(4,KK,LL),FREG(4,KK,LL),IDFD,    PRT30119          
     1DEVSS(4,KK,LL),DEVSM(4,KK,LL),FDEV(4,KK,LL)                       PRT30120          
  210 FORMAT(1H06X17HDAMS/S REGRESSION,16X1H1,12XE15.8,5XE15.8,8XE15.8, PRT30121          
     1  /14X10HDEVIATIONS,12XI5,12XE15.8,5XE15.8,8XE15.8)               PRT30122          
  220 WRITE (6,230) IDF(I2+2),SS(I2+2,I,J)                              PRT30123          
  230 FORMAT(1H0 12X5HTOTAL 18XI5,12XE15.8)                             PRT30124          
      GO TO (240,240,251),N3                                            PRT30125          
  240 WRITE (6,250) VC3(I,J),THETA(7,I,J),THETA(3,I,J),THETA(6,I,J),    PRT30126          
     1VC2(I,J),THETA(2,I,J),THETA(5,I,J),VC1(I,J),THETA(1,I,J),THETA(4,IPRT30127          
     2,J)                                                               PRT30128          
  250 FORMAT(1H ,90X21HVARIANCE (COVARIANCE)//76X10HPHENOTYPIC,8X7HGENETPRT30129          
     1IC,10X13HENVIRONMENTAL,//4X28HVARIANCE COMPONENT 3 (ERROR),5XE20.8PRT30130          
     2,13X3E18.8,3X5H(S+D),//4X26HVARIANCE COMPONENT 2 (DAM),7XE20.8,31XPRT30131          
     32E18.8,3X5H(DAM),//4X27HVARIANCE COMPONENT 1 (SIRE),6XE20.8,31X2E1PRT30132          
     48.8,2X6H(SIRE))                                                   PRT30133          
  251 IF(J .NE. I) GO TO 350                                            PRT30134          
      WRITE(6,252)                                                      PRT30135          
      GO TO (253,253,270),N3                                            PRT30136          
  252 FORMAT(1H //45X30HHERITABILITIES AND STD. ERRORS ,//14X6H(SIRE),  PRT30137          
     137X5H(DAM),37X5H(S+D))                                            PRT30138          
  253 WRITE(6,260)  HS(I,J),SE1(I,J),HD(I,J),SE2(I,J),HSD(I,J),SE3(I,J) PRT30139          
  260 FORMAT(1H0,56X16HOF SIB ANALYSIS ,/1H02X3(3X,2E20.8),/)           PRT30140          
      GO TO (270,460),N3                                                PRT30141          
  270 WRITE(6,400)                                                      PRT30142          
      IF (IS .LE. 0) GO TO 310                                          PRT30143          
      IF (I .GT. IOFM) GO TO 290                                        PRT30144          
      WRITE(6,280) HB(1,II,JJ),SE4(1,II,JJ),CHS(II,JJ), HR(1,II,JJ)     PRT30145          
  280 FORMAT( 1H0 5X2E20.8,21X,41HHOMEOSTATIC COEF.(=0.0 IF NO NAT. SEL.PRT30146          
     1) =,E15.8,/  11H O-P CORLN.,E15.8)                                PRT30147          
      GO TO 310                                                         PRT30148          
  290 WRITE(6,300) HB(2,KK,LL),SE4(2,KK,LL),CORS(KK,LL),HR(2,KK,LL)     PRT30149          
  300 FORMAT( 1H0 5X2E20.8,20H CORRECTION APPLD. =,E15.8,/  11H O-P CORLPRT30150          
     1N.,E15.8)                                                         PRT30151          
  310 IF (ID .LE. 0) GO TO 460                                          PRT30152          
      IF (I .GT. IOFM) GO TO 330                                        PRT30153          
      WRITE(6,320) HB(3,II,JJ),SE4(3,II,JJ),CORD(II,JJ),HR(3,II,JJ)     PRT30154          
  320 FORMAT( 1H048X2E20.8,20H CORRECTION APPLD. =,E15.8,/44X10HO-P CORLPRT30155          
     1N.,E15.8)                                                         PRT30156          
      GO TO 500                                                         PRT30157          
  330 WRITE(6,340) HB(4,KK,LL),SE4(4,KK,LL),            HR(4,KK,LL)     PRT30158          
  340 FORMAT( 1H048X2E20.8,/43X11H O-P CORLN.,E15.8/)                   PRT30159          
      GO TO 460                                                         PRT30160          
  350 GO TO (351,351,381),N3                                            PRT30161          
  351 WRITE(6,360)IPR,JPR,HDFS(I,J),THETPD(I,J),THETGS(I,J)             PRT30162          
  360 FORMAT(1H0 25H DIFFERENCE OF VARIABLES ,I2,4H AND,I3,6X,11HHERIT. PRT30163          
     1SIRE,E17.8,E19.8,E18.8,                           ///50X28HCORRELAPRT30164          
     2TIONS AND STD. ERRORS,//23X6H(SIRE),33X5H(DAM),34X5H(S+D),        PRT30165          
     3//57X16HOF SIB ANALYSIS )                                         PRT30166          
  370 WRITE (6,380)  (RTHETA(K1,I,J),SER(K1,I,J),K1=1,7),(SCR1(K,I,J),SEPRT30167          
     1SCR1(K,I,J),K=1,3)                                                PRT30168          
  380 FORMAT( 1H013HGENETIC      ,3(3X,2E18.8) /1H013HENVIRONMENTAL,3(3XPRT30169          
     12E18.8) /1H013HPHENOTYPIC   ,81X2E18.8,//1H013HS.C.R.       ,3(3X,PRT30170          
     22E18.8)//)                                                        PRT30171          
      GO TO (390,501),N3                                                PRT30172          
  381 WRITE(6,382)                                                      PRT30173          
  382 FORMAT(1H0,50X,28HCORRELATIONS AND STD. ERRORS )                  PRT30174          
  390 IF(IS .LE. 1) GO TO 430                                           PRT30175          
      IF (J .GT. IOFM) GO TO 420                                        PRT30176          
      WRITE(6,400)                                                      PRT30177          
  400 FORMAT(1H 56X17HOF O-P REGRESSION)                                PRT30178          
      WRITE(6,410) RB(1,II,JJ),SRR(1,II,JJ),A(1,II,JJ),SCR2(1,II,JJ),   PRT30179          
     1SESCR2(1,II,JJ)                                                   PRT30180          
  410 FORMAT( 1H013HSIRES GENETIC,3X2E18.8,19H  C.V.(DEN.COVS.) =,E15.8,PRT30181          
     1/14H       S.C.R. ,3X2E18.8)                                      PRT30182          
      GO TO 430                                                         PRT30183          
  420 IF (I .LE. IOFM) GO TO 430                                        PRT30184          
      WRITE(6,400)                                                      PRT30185          
      WRITE(6,410) RB(2,KK,LL),SRR(2,KK,LL),A(2,KK,LL),SCR2(2,KK,LL),   PRT30186          
     1SESCR2(2,KK,LL)                                                   PRT30187          
  430 IF (ID .LE. 1) GO TO 460                                          PRT30188          
      IF (J .GT. IOFM) GO TO 450                                        PRT30189          
      IF (IS .GT. 1) GO TO 431                                          PRT30190          
      WRITE(6,400)                                                      PRT30191          
  431 WRITE(6,440) RB(3,II,JJ),SRR(3,II,JJ),A(3,II,JJ),SCR2(3,II,JJ),   PRT30192          
     1SESCR2(3,II,JJ)                                                   PRT30193          
  440 FORMAT( 1H041X12HDAMS GENETIC,2X2E18.8,19H  C.V.(DEN.COVS.) =,    PRT30194          
     1E15.8,/42X12H     S.C.R. ,2X2E18.8/)                              PRT30195          
      GO TO 500                                                         PRT30196          
  450 IF (I .LE. IOFM) GO TO 500                                        PRT30197          
  451 IF (IS .GT. 1) GO TO 452                                          PRT30198          
      WRITE(6,400)                                                      PRT30199          
  452 WRITE(6,440) RB(4,KK,LL),SRR(4,KK,LL),A(4,KK,LL),SCR2(4,KK,LL),   PRT30200          
     1SESCR2(4,KK,LL)                                                   PRT30201          
  460 GO TO (461,462,511),N3                                            PRT30202          
  462 IF(IM .NE. IF) GO TO 470                                          PRT30203          
      IF(I .LE. IM) GO TO 500                                           PRT30204          
  461 IF (J .NE.I) GO TO 501                                            PRT30205          
  470 IF(IF .EQ. 0) GO TO 480                                           PRT30206          
      WRITE(6,490) VA(KK,LL),VD(KK,LL),VAA(KK,LL),SL1(KK,LL),SL2(KK,LL),PRT30207          
     1SL3(KK,LL),XMAT1(KK,LL),XMAT2(KK,LL)                              PRT30208          
      GO TO 501                                                         PRT30209          
  480 WRITE(6,490) VA(II,JJ),VD(II,JJ),VAA(II,JJ),SL1(II,JJ),SL2(II,JJ),PRT30210          
     1SL3(II,JJ),XMAT1(II,JJ),XMAT2(II,JJ)                              PRT30211          
  490 FORMAT(1H  37X43HPRELIMINARY PARTITIONING OF PHEN. VARIANCE ,//21XPRT30212          
     18HADDITIVE,E19.8,/21X9HDOMINANCE,E18.8,/21X11HADD. * ADD.,E16.8,8XPRT30213          
     2,3H(2),15X,3H(3),/21X,9HSEX LINK.,3E18.8,/21X,11HMAT. EFFECT,E16.8PRT30214          
     3,E18.8)                                                           PRT30215          
  500 GO TO (501,501,511),N3                                            PRT30216          
  501 WRITE (6,510) COMENV(I,J),WFSENV(I,J)                             PRT30217          
  510 FORMAT(1H0 1X27HCOMMON ENVIR. VAR. (COVAR.),E19.8,/30H  W/IN FS ENPRT30218          
     1VIR. VAR. (COVAR.),E18.8)                                         PRT30219          
C 501 WRITE (6,510) Q3, Q2, Q1,BIVAR1,BIVAR2,BIVAR3                     PRT30220          
C 501 FORMAT(1H0,13X42HCOEFFICIENTS OF VARIANCE COMPONENTS (1) = ,F10.5,PRT30221          
C    14X,6H(2) = ,F10.5,4X,6H(3) = ,F10.5,  //13X102HBINOMIAL VARIANCE CPRT30222          
C    2OMPONENTS FOR ARCSIN TRANSFORM.(FOR ADJUSTING GIVEN HERITABILITY  PRT30223          
C    3ESTIMATES IF SUCH,  //13X19HVARIABLES WERE USED,17X6H(3) = ,E15.8,PRT30224          
C    44X6H(2) = ,E15.8,4X6H(1) = ,E15.8)                                PRT30225          
  511 WRITE(6,512)Q1,Q2,Q3                                              PRT30226          
  512 FORMAT(1H ,6X55HCOEFFICIENTS OF VARIANCE (COVARIANCE) COMPONENTS (PRT30227          
     11) = ,F10.5,4X,6H(2) = ,F10.5,4X,6H(3) = ,F10.5)                  PRT30228          
  520 CONTINUE                                                          PRT30229          
      RETURN                                                            PRT30230          
      END                                                               PRT30231          
*ENDTEXT                                                                                  
