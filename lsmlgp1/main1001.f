*TEXT                                                                                     
      SUBROUTINE MAIN1                                                  DEC04000          
C     SUBROUTINE WHICH READS DATA CARDS, CALLS CODEX, COMPUTES LS OR ML DEC04000          
C     MATRICES, CALLS SCLNOS AND LISTS CERTAIN MATRICES, MEANS, SD AND  DEC04001          
C     CORRELATIONS                                                      DEC04002          
C     ----------------------------------------------------              DEC04003          
      DIMENSION IRWC(10)                                                M0104004          
      DIMENSION ARRAY(8550),SSCPR(1260),SSS(1260),RHM(3500),            DEC04005          
     1  TOT(136),TOT2(136),TOT3(136),                                   DEC04005          
     2  LAB1(100),LAB2(100),LAB3(100),LAB4(100)                         DEC04005          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),NEQ(6),IPL(13),NSDEC04007          
     1P(13),NND(13),XP(81),YP(41),      NDC(10),NMI(10),MEN(20),NCL(20),DEC04008          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(40),NCLN(40),LNE(40),NDEN(1DEC04009          
     300),NLIT(40),NMA(30),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(40),DEC04010          
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDEC04011          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),IC(480),DEC04012          
     6EFF1(50),EFF2(50),NOS(250),X(136),NMAC(40)                                          
      DIMENSION LSKF(20),LBEG(20),IREJ(20)                              M0204013          
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADEC04014          
     1B4,LITY,TRED,YM,IM,MS,NEQ,IPL,NSP,NND,XP,YP                       DEC04015          
      COMMON /CMBLK2/NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NCLN,NEN,LNE,NBEGDEC04016          
     1,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQC,NREGP,LGTX,JBEG,NDDEC04017          
     2ECX,LITR,NEGY,LNY,LHY,KBEG,NDECY,XM,IC,EFF1,EFF2,NOS,X,NMAC       DEC04018          
      COMMON /CMBLK3/IJOB,NAB,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F,NPR,IRAN,MDEC04019          
     1POP,LIOP,IAD,REP,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,NOM,NON,ML,MLB,NSDEC04020          
     2,NOT,DF,KPUT,SDF,EDF,WK,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,RR       DEC04021          
     3,IN,NSKF,LSKF,LBEG,IREJ                                           M0204021          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DEC04022          
     1L,L7                                                              DEC04023          
      DATA (IBLK=1H )                                                   M0104024          
      DATA(JBLK=1H-)                                                    M0104024          
      DATA(IMIN=8H00000000)                                             M0104024          
      IF (LIOP.EQ.1.OR.LIOP.EQ.3) GO TO 1                               DEC04025          
      GO TO 2                                                           DEC04026          
    1 WRITE (6,1001) IJOB                                               DEC04027          
 1001 FORMAT (1H1,20X,35HLISTING OF X MATRIX FOR PROBLEM NO.,I3)        DEC04028          
    2 MATX=NLHM*(NLHM+1)/2                                              DEC04029          
      DO 4 I=1,MATX                                                     DEC04030          
    4 ARRAY(I)=0.0                                                      DEC04031          
      K1=NLHM*NRHM                                                      DEC04032          
      DO 5 I=1,K1                                                       DEC04033          
    5 RHM(I)=0.0                                                        DEC04034          
      IF (NCPR.EQ.0) GO TO 6                                            DEC04035          
      K1=NRHM*(NRHM+1)/2                                                DEC04036          
      GO TO 7                                                           DEC04037          
    6 K1=NRHM                                                           DEC04038          
    7 DO 8 I=1,K1                                                       DEC04039          
    8 SSCPR(I)=0.0                                                      DEC04040          
      NOT=NLHM+NRHM                                                     DEC04041          
      DO 9 I=1,NOT                                                      DEC04042          
      TOT(I)=0.0                                                        DEC04043          
      TOT2(I)=0.0                                                       DEC04044          
    9 TOT3(I)=0.0                                                       DEC04045          
      DO 10 I=1,250                                                     DEC04046          
   10 NOS(I)=0                                                          DEC04047          
      K1=0                                                              DEC04048          
      NCDS=0                                                            DEC04049          
      MIN=0                                                             DEC04050          
      GNI=0.                                                            DEC04051          
      MJN=0                                                             DEC04052          
      NCDG=0                                                            DEC04053          
      NMIG=1                                                            DEC04054          
      CALL PRERED(IC,IN,NTRA,IBLK,1,80,1)                               M0304055          
   20 CALL READIN(IC,IN,NTRA,IBLK,1,80,1)                               M0304056          
      IF (NTRA.EQ.0) GO TO 156                                          DEC04061          
      NCDS=NCDS+1                                                       DEC04065          
      IF (NCD.LT.2) GO TO 32                                            DEC04066          
      DO 30 J=2,NCD                                                     DEC04067          
      L=J*80                                                            DEC04068          
      K=L-79                                                            DEC04069          
      CALL READIN(IC,IN,NTRA,IBLK,K,L,J)                                M0304070          
      K1=ICN1+K-1                                                       DEC04074          
      K2=K1-80                                                          DEC04075          
      IF (IC(K1).LE.IC(K2)) GO TO 901                                   DEC04076          
   30 CONTINUE                                                          DEC04077          
   32 CONTINUE                                                          DEC04078          
C     ----------------------------------------------------              M0204078          
C      TEST DETAIL CARD FOR REJECTION                                   M0204078          
C     ----------------------------------------------------              M0204078          
      IF(NSKF) 790,790,791                                              M0204078          
  791 CALL REJECT(IC,IN,NSKF,LSKF,LBEG,IREJ,ICOD)                       M0204078          
      IF(ICOD) 900,790,792                                              M0204078          
  792 NCDS=NCDS-1                                                       M0204078          
      GO TO 20                                                          M0204078          
  790 CONTINUE                                                          M0204078          
      IF (NAB.GT.1) GO TO 100                                           DEC04079          
   36 CONTINUE                                                          DEC04080          
      CALL CODEX                                                        DEC04081          
      IF (MULL.EQ.1) RETURN                                             DEC04082          
      IF (LIOP.EQ.1.OR.LIOP.EQ.3) GO TO 41                              DEC04083          
      GO TO 39                                                          DEC04084          
   41 WRITE (6,1002) (X(I),I=1,NOT)                                     DEC04085          
 1002 FORMAT (1H ,17F7.2)                                               DEC04086          
   39 IF (NLHM.EQ.0) GO TO 47                                           DEC04087          
   42 DO 46 I=1,NLHM                                                    DEC04088          
      IF (X(I).EQ.0.0) GO TO 46                                         DEC04089          
      DO 45 J=I,NLHM                                                    DEC04090          
      K=NLHM*(I-1)-I*(I-3)/2+J-I                                        DEC04091          
   45 ARRAY(K)=ARRAY(K)+X(I)*X(J)                                       DEC04092          
   46 CONTINUE                                                          DEC04093          
   47 DO 50 I=1,NOT                                                     DEC04094          
   50 TOT(I)=TOT(I)+X(I)                                                DEC04095          
      IF (NRHM.EQ.0) GO TO 60                                           DEC04096          
      IF (NCPR.EQ.0) GO TO 56                                           DEC04097          
      DO 55 I=1,NRHM                                                    DEC04098          
      DO 55 J=I,NRHM                                                    DEC04099          
      K=NRHM*(I-1)-I*(I-3)/2+J-I                                        DEC04100          
      K2=NLHM+I                                                         DEC04101          
      K3=NLHM+J                                                         DEC04102          
   55 SSCPR(K)=SSCPR(K)+X(K2)*X(K3)                                     DEC04103          
      GO TO 58                                                          DEC04104          
   56 CONTINUE                                                          DEC04105          
      DO 57 I=1,NRHM                                                    DEC04106          
      K2=NLHM+I                                                         DEC04107          
   57 SSCPR(I)=SSCPR(I)+X(K2)*X(K2)                                     DEC04108          
   58 K=0                                                               DEC04109          
      DO 59 I=1,NRHM                                                    DEC04110          
      J2=NLHM+I                                                         DEC04111          
      DO 59 J=1,NLHM                                                    DEC04112          
      K=K+1                                                             DEC04113          
   59 RHM(K)=RHM(K)+X(J)*X(J2)                                          DEC04114          
   60 GO TO 20                                                          DEC04115          
C     ----------------------------------------------------              DEC04116          
C     ABSORPTION PROCESS                                                DEC04117          
C     ----------------------------------------------------              DEC04118          
  100 CONTINUE                                                          DEC04119          
      IF (NAB.LT.4) GO TO 110                                           DEC04120          
      IMJ1=0                                                            DEC04121          
      DO 102 I=1,NMJC                                                   DEC04122          
      K=NDC(I)                                                          DEC04123          
      IF (IC(K).NE.JBLK) GO TO 102                                      DEC04124          
      IF (I.EQ.NMJC) GO TO 900                                          DEC04125          
      IC(K)=0                                                           DEC04126          
  102 IMJ1=IC(K)+IMJ1*10                                                DEC04127          
      IMI1=0                                                            DEC04128          
      DO 104 I=1,NMIC                                                   DEC04129          
      K=NMI(I)                                                          DEC04130          
      IF (IC(K).NE.JBLK) GO TO 104                                      DEC04131          
      IF (I.EQ.NMIC) GO TO 900                                          DEC04132          
      IC(K)=0                                                           DEC04133          
  104 IMI1=IC(K)+IMI1*10                                                DEC04134          
      IF (NCDS.EQ.1) GO TO 108                                          DEC04135          
      IF (IMJ2-IMJ1) 164,107,901                                        DEC04136          
  107 IF (IMI2-IMI1) 164,106,901                                        DEC04137          
  106 NMIG=NMIG+1                                                       DEC04138          
  108 IMI2=IMI1                                                         DEC04139          
  109 IMJ2=IMJ1                                                         DEC04140          
      GO TO 36                                                          DEC04141          
  110 IMJ1=0                                                            DEC04142          
      DO 112 I=1,NCC                                                    DEC04143          
      K=NDC(I)                                                          DEC04144          
      IF (IC(K).NE.JBLK) GO TO 112                                      DEC04145          
      IC(K)=0                                                           DEC04147          
      IF (I.EQ.NCC) GO TO 900                                           DEC04146          
  112 IMJ1=IC(K)+IMJ1*10                                                DEC04148          
      IF (NCDS.EQ.1) GO TO 114                                          DEC04149          
      IF (IMJ2-IMJ1) 116,114,901                                        DEC04150          
  114 NCDG=NCDG+1                                                       DEC04151          
      GO TO 109                                                         DEC04152          
  116 DM=NCDG                                                           DEC04153          
      IF (NAB.EQ.2) GO TO 118                                           DEC04154          
      ROWN=1./(DM+REP)                                                  DEC04155          
      GO TO 120                                                         DEC04156          
  118 ROWN=1./DM                                                        DEC04157          
  120 IF (NLHM.EQ.0) GO TO 130                                          DEC04158          
      DO 124 I=1,NLHM                                                   DEC04159          
      IF (TOT(I).EQ.0.0) GO TO 124                                      DEC04160          
      DO 122 J=I,NLHM                                                   DEC04161          
      K=NLHM*(I-1)-I*(I-3)/2+J-I                                        DEC04162          
  122 ARRAY(K)=ARRAY(K)-TOT(I)*TOT(J)*ROWN                              DEC04163          
  124 CONTINUE                                                          DEC04164          
  130 IF (LIOP.EQ.0.OR.LIOP.EQ.5.OR.LIOP.EQ.10) GO TO 135               DEC04165          
      IF (NAB.EQ.4) GO TO 132                                           DEC04166          
      WRITE (6,1050) IMJ2,NCDG                                          DEC04167          
 1050 FORMAT (1H ,10X,14HMINOR CLASS IS,I12,6H   NI=,I4/1H ,   6HTOTALS)DEC04168          
      GO TO 134                                                         DEC04169          
  132 WRITE (6,1050) IMI2,NMIG                                          DEC04170          
  134 WRITE (6,1052) (TOT(I),I=1,NOT)                                   DEC04171          
  135 IF (NRHM.EQ.0) GO TO 146                                          DEC04172          
      IF (NCPR.EQ.0) GO TO 138                                          DEC04173          
      DO 136 I=1,NRHM                                                   DEC04174          
      DO 136 J=I,NRHM                                                   DEC04175          
      K=NRHM*(I-1)-I*(I-3)/2+J-I                                        DEC04176          
      K2=NLHM+I                                                         DEC04177          
      K3=NLHM+J                                                         DEC04178          
  136 SSCPR(K)=SSCPR(K)-TOT(K2)*TOT(K3)*ROWN                            DEC04179          
      GO TO 142                                                         DEC04180          
  138 DO 140 I=1,NRHM                                                   DEC04181          
      K2=NLHM+I                                                         DEC04182          
  140 SSCPR(I)=SSCPR(I)-TOT(K2)*TOT(K2)*ROWN                            DEC04183          
  142 K=0                                                               DEC04184          
      DO 144 I=1,NRHM                                                   DEC04185          
      J2=NLHM+I                                                         DEC04186          
      DO 144 J=1,NLHM                                                   DEC04187          
      K=K+1                                                             DEC04188          
  144 RHM(K)=RHM(K)-TOT(J)*TOT(J2)*ROWN                                 DEC04189          
  146 IF (NAB.EQ.1) GO TO 299                                           DEC04190          
       DO 148 I=1,NOT                                                   DEC04191          
      TOT3(I)=TOT3(I)+TOT(I)                                            DEC04192          
  148 TOT2(I)=TOT2(I)+TOT(I)                                            DEC04193          
      IF (NAB.NE.4) GO TO 152                                           DEC04194          
      DO 150 I=1,NOT                                                    DEC04195          
      TOT2(I)=TOT2(I)-DM*TOT(I)*ROWN                                    DEC04196          
  150 TOT(I)=0.0                                                        DEC04197          
      GNI=GNI-DM*DM*ROWN                                                DEC04198          
      GO TO 166                                                         DEC04199          
  152 MJN=MJN+1                                                         DEC04200          
      DO 154 I=1,NOT                                                    DEC04201          
  154 TOT(I)=0.0                                                        DEC04202          
      NCDG=1                                                            DEC04203          
      IF (NTRA.EQ.0) GO TO 193                                          DEC04204          
      GO TO 109                                                         DEC04205          
  156 IF (NAB.NE.0) GO TO 158                                           DEC04206          
      DF=NCDS                                                           DEC04207          
      GO TO 299                                                         DEC04208          
  158 IF (NAB.NE.1) GO TO 160                                           DEC04209          
      DF=NCDS-1                                                         DEC04210          
      GO TO 162                                                         DEC04211          
  160 IF (NAB.LT.4) GO TO 116                                           DEC04212          
      DF=NCDS-MIN-1                                                     DEC04213          
      GO TO 164                                                         DEC04214          
  162 DM=NCDS                                                           DEC04215          
      ROWN=1./DM                                                        DEC04216          
      GO TO 120                                                         DEC04217          
  164 DM=NMIG                                                           DEC04218          
      NCDG=NCDG+NMIG                                                    DEC04219          
      ROWN=1./(DM+REP)                                                  DEC04220          
      GNI=GNI+DM                                                        DEC04221          
      GO TO 120                                                         DEC04222          
  166 MIN=MIN+1                                                         DEC04223          
      NMIG=1                                                            DEC04224          
      IF (NTRA.EQ.0.OR.IMJ1.NE.IMJ2) GO TO 168                          DEC04225          
      GO TO 108                                                         DEC04226          
  168 ROWN=1./GNI                                                       DEC04227          
      IF (NLHM.EQ.0) GO TO 172                                          DEC04228          
      DO 170 I=1,NLHM                                                   DEC04229          
      DO 170 J=I,NLHM                                                   DEC04230          
      K=NLHM*(I-1)-I*(I-3)/2+J-I                                        DEC04231          
  170 ARRAY(K)=ARRAY(K)-TOT2(I)*TOT2(J)*ROWN                            DEC04232          
  172 IF (NRHM.EQ.0) GO TO 184                                          DEC04233          
      IF (NCPR.EQ.0) GO TO 176                                          DEC04234          
      DO 174 I=1,NRHM                                                   DEC04235          
      DO 174 J=I,NRHM                                                   DEC04236          
      K=NRHM*(I-1)-I*(I-3)/2+J-I                                        DEC04237          
      K3=NLHM+J                                                         DEC04238          
      K2=NLHM+I                                                         DEC04239          
  174 SSCPR(K)=SSCPR(K)-TOT2(K2)*TOT2(K3)*ROWN                          DEC04240          
      GO TO 180                                                         DEC04241          
  176 DO 178 I=1,NRHM                                                   DEC04242          
      K2=NLHM+I                                                         DEC04243          
  178 SSCPR(I)=SSCPR(I)-TOT2(K2)*TOT2(K2)*ROWN                          DEC04244          
  180 K=0                                                               DEC04245          
      DO 182 I=1,NRHM                                                   DEC04246          
      J2=NLHM+I                                                         DEC04247          
      DO 182 J=1,NLHM                                                   DEC04248          
      K=K+1                                                             DEC04249          
  182 RHM(K)=RHM(K)-TOT2(J)*TOT2(J2)*ROWN                               DEC04250          
  184 IF (LIOP.EQ.0.OR.LIOP.EQ.5) GO TO 190                             DEC04251          
      WRITE (6,1051) IMJ2,GNI                                           DEC04252          
 1051 FORMAT (1H0,5X,14HMAJOR CLASS IS,I12,3X,31HREDUCED DIAGONAL ELEMENDEC04253          
     1T (GNI)=,F18.8)                                                   DEC04254          
      WRITE (6,1052) (TOT2(I),I=1,NOT)                                  DEC04255          
 1052 FORMAT (1H ,9F13.3)                                               DEC04256          
  190 DO 191 I=1,NOT                                                    DEC04257          
  191 TOT2(I)=0.0                                                       DEC04258          
      MJN=MJN+1                                                         DEC04259          
      GNI=0.0                                                           DEC04260          
      NCDG=0                                                            DEC04261          
      IF (NTRA.EQ.0) GO TO 196                                          DEC04262          
      GO TO 108                                                         DEC04263          
  193 DO 194 I=1,NOT                                                    DEC04264          
  194 TOT(I)=TOT2(I)                                                    DEC04265          
      DF=NCDS-MJN                                                       DEC04266          
      IF (NAB.EQ.3) DF=DF+1.                                            DEC04267          
      IF (NAB.EQ.3.AND.IAD.EQ.4) DF=DF+FLOAT(NCL(1))-1.0                DEC04268          
      GO TO 299                                                         DEC04269          
  196 DO 198 I=1,NOT                                                    DEC04270          
  198 TOT(I)=TOT3(I)                                                    DEC04271          
      GO TO 299                                                         DEC04272          
  900 WRITE (6,1053) NCDS                                               DEC04273          
 1053 FORMAT (1H0,69HUNITS POSITION OF AN ID FIELD OR A CONTROL FIELD ISDEC04274          
     1 BLANK ON CARD NO.,I5)                                            DEC04275          
      GO TO 902                                                         DEC04276          
  901 WRITE (6,1054) NCDS,IJOB                                          DEC04277          
 1054 FORMAT (1H0,38HCARDS OUT OF SEQUENCE---CHECK CARD NO.,I5,5X,15HFORDEC04278          
     1 PROBLEM NO.,I3)                                                  DEC04279          
  902 MULL=1                                                            DEC04280          
      RETURN                                                            DEC04281          
  299 CONTINUE                                                          DEC04282          
      IF (NAB.EQ.0) GO TO 300                                           DEC04283          
      GO TO (301,302,303,304), NAB                                      DEC04284          
  300 WRITE (6,1060) NCDS                                               DEC04285          
 1060 FORMAT (1H1,5X,53HTOTAL LEAST-SQUARES ANALYSIS.  NO EQUATIONS ABSODEC04286          
     1RBED.,5X,13HDF=NO. CARDS=,I5)                                     DEC04287          
      GO TO 310                                                         DEC04288          
  301 WRITE (6,1061) DF                                                 DEC04289          
 1061 FORMAT (1H1,21X,38HEQUATION FOR THE OVERALL MEAN ABSORBED,5X,3HDF=DEC04290          
     1,F5.0)                                                            DEC04291          
      GO TO 310                                                         DEC04292          
  302 WRITE (6,1062) MJN,NCDS,DF                                        DEC04293          
 1062 FORMAT (1H1,10X,55HNO. OF CLASSES OR SUBCLASSES ABSORBED BY LEAST DEC04294          
     1SQUARES=,I4,5X,10HNO. CARDS=,I5,5X,3HDF=,F5.0)                    DEC04295          
      GO TO 310                                                         DEC04296          
  303 WRITE (6,1063) MJN,REP,NCDS,DF                                    DEC04297          
 1063 FORMAT (1H1,60HNO. OF CLASSES OR SUBCLASSES ABSORBED BY MAXIMUM LIDEC04298          
     1KELIHOOD=,I4,3X,7H1-R/R =,F7.4,3X,10HNO. CARDS=,I5,3X,3HDF=,F5.0) DEC04299          
      GO TO 310                                                         DEC04300          
  304 WRITE (6,1064) MJN,MIN,REP,NCDS,DF                                DEC04301          
 1064 FORMAT (1H1,37H NO. OF MAJOR CLASSES ABSORBED BY ML=,I4,3X,21HNO. DEC04302          
     1MINOR SUBCLASSES=,I4,3X,7H1-R/R =,F7.4,3X,10HNO. CARDS=,I5,3X,3HDFDEC04303          
     3=,F5.0)                                                           DEC04304          
  310 IF (NOM+NON.EQ.0) GO TO 320                                       DEC04305          
      CALL SCLNOS                                                       DEC04306          
      IF (NLHM.EQ.0.OR.(NPR.EQ.0.AND.LIOP.EQ.10)) GO TO 330             DEC04307          
  320 WRITE (6,1100) IJOB                                               DEC04308          
 1100 FORMAT (1H0,60HOVERALL MEANS AND STANDARD DEVIATIONS OF LHM FOR PRDEC04309          
     1OBLEM NO.,I3)                                                     DEC04310          
      WRITE (6,1101)                                                    DEC04311          
 1101 FORMAT (1H0,35H CODED LHM    INDEPENDENT VARIABLES,11X,4HMEAN,9X,4DEC04312          
     1HS.D.)                                                            DEC04313          
      WRITE (6,1102)                                                    DEC04314          
 1102 FORMAT (1H )                                                      DEC04315          
      L=1                                                               DEC04316          
      K3=1                                                              DEC04317          
      DO 329 I=1,NLHM                                                   DEC04318          
      IF (I.LT.IE.AND.LIOP.EQ.10) GO TO 329                             DEC04319          
      AMN=TOT(I)/FLOAT(NCDS)                                            DEC04320          
      K=NLHM*(I-1)-I*(I-3)/2                                            DEC04321          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 324                   DEC04322          
      TOT(I)=ARRAY(I)                                                   DEC04323          
      SD=SQRT((ARRAY(K)-(TOT(I)*TOT(I)/ARRAY(1)))/(DF-1.))              DEC04324          
      GO TO 326                                                         DEC04325          
  324 SD=SQRT(ARRAY(K)/DF)                                              DEC04326          
  326 IF (I.LT.IE) GO TO 328                                            DEC04327          
      L=L-1                                                             DEC04328          
      IF (L.NE.0) GO TO 328                                             DEC04329          
      IF (LQC(K3)-2) 390,391,392                                        DEC04330          
  390 AMN=AMN+XM(K3)                                                    DEC04331          
      K3=K3+1                                                           DEC04332          
      L=1                                                               DEC04333          
      GO TO 328                                                         DEC04334          
  391 K3=K3+1                                                           DEC04335          
      L=2                                                               DEC04336          
      GO TO 328                                                         DEC04337          
  392 K3=K3+1                                                           DEC04338          
      L=3                                                               DEC04339          
  328 WRITE (6,1103) I,LAB1(I),LAB2(I),LAB3(I),LAB4(I),AMN,SD           DEC04340          
  329 CONTINUE                                                          DEC04341          
 1103 FORMAT(1H ,I6,6X,A6,2(1X,A6),A6,2F13.5)                           M0204342          
  330 IF (NRHM.EQ.0) GO TO 342                                          DEC04343          
      WRITE (6,1104)                                                    DEC04344          
 1104 FORMAT (1H0,15X,44HOVERALL MEANS AND STANDARD DEVIATIONS OF RHM)  DEC04345          
      WRITE (6,1102)                                                    DEC04346          
      L=NLHM*(NLHM+1)/2                                                 DEC04347          
      DO 340 I=1,NRHM                                                   DEC04348          
      K2=NLHM+I                                                         DEC04349          
      AMN=TOT(K2)/FLOAT(NCDS)                                           DEC04350          
      K=NLHM*(I-1)+1                                                    DEC04351          
      IF (NAB.EQ.0.OR.NAB.EQ.3) TOT(K2)=RHM(K)                          DEC04352          
      IF (NCPR.EQ.0) GO TO 332                                          DEC04353          
      J=NRHM*(I-1)-I*(I-3)/2                                            DEC04354          
      GO TO 334                                                         DEC04355          
  332 J=I                                                               DEC04356          
  334 IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 336                   DEC04357          
      SD=SQRT((SSCPR(J)-(TOT(K2)*TOT(K2)/ARRAY(1)))/(DF-1.))            DEC04358          
      GO TO 338                                                         DEC04359          
  336 SD=SQRT(SSCPR(J)/DF)                                              DEC04360          
  338 AMN=AMN+YM(I)                                                     DEC04361          
  340 WRITE (6,1105) LITY(I),AMN,SD                                     DEC04362          
 1105 FORMAT (1H ,12X,A6,4X,5HMEAN=,F12.5,10X,5HS.D.=,F12.5)            DEC04363          
  342 IF (LIOP.EQ.7) GO TO 500                                          DEC04364          
  344 IF (NLHM.EQ.0) GO TO 359                                          DEC04365          
      IF (IE.GT.NLHM.AND.LIOP.EQ.10) GO TO 359                          DEC04366          
      WRITE (6,1102)                                                    DEC04367          
      WRITE (6,1106) IJOB                                               DEC04368          
 1106 FORMAT (1H0,15X,64HSUMS OF SQUARES, C.P. AND CORRELATIONS AMONG LHDEC04369          
     1M FOR PROBLEM NO.,I3)                                             DEC04370          
      WRITE (6,1107)                                                    DEC04371          
 1107 FORMAT (1H0,4H ROW,5H  COL,15X,21HINDEPENDENT VARIABLES)          DEC04372          
      WRITE (6,1108)                                                    DEC04373          
 1108 FORMAT (1H ,9HCODE CODE,9X,3HROW,22X,6HCOLUMN,23X,28HS.SQS. OR C.PDEC04374          
     1.   CORRELATION)                                                  DEC04375          
      DO 360 I=1,NLHM                                                   DEC04376          
      DO 360 J=I,NLHM                                                   DEC04377          
      IF (I.LT.IE.AND.J.LT.IE.AND.LIOP.EQ.10) GO TO 360                 DEC04378          
      K=NLHM*(I-1)-I*(I-3)/2                                            DEC04379          
      K1=NLHM*(J-1)-J*(J-3)/2                                           DEC04380          
      K3=K+J-I                                                          DEC04381          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 354                   DEC04382          
      SCP=ARRAY(K3)-(TOT(I)*TOT(J))/ARRAY(1)                            DEC04383          
      IF (I.GE.IE.AND.I.NE.J) GO TO 350                                 DEC04384          
      SSY1=ARRAY(K)-(TOT(I)*TOT(I))/ARRAY(1)                            DEC04385          
      GO TO 352                                                         DEC04386          
  350 SSY1=ARRAY(K)                                                     DEC04387          
  352 SSY2=ARRAY(K1)-(TOT(J)*TOT(J))/ARRAY(1)                           DEC04388          
      RLHM=SCP/SQRT(SSY1*SSY2)                                          DEC04389          
      IF (I.LT.IE.AND.J.LT.IE) GO TO 356                                DEC04390          
      ARRAY(K3)=SCP                                                     DEC04391          
      GO TO 356                                                         DEC04392          
  354 RLHM=ARRAY(K3)/SQRT(ARRAY(K)*ARRAY(K1))                           DEC04393          
  356 IF (I.LT.IE.AND.LIOP.EQ.10) GO TO 360                             DEC04394          
      IF (I.NE.J) GO TO 358                                             DEC04395          
      RLHM=1.                                                           DEC04396          
      WRITE (6,1102)                                                    DEC04397          
  358 WRITE (6,1109) I,J,LAB1(I),LAB2(I),LAB3(I),LAB4(I),LAB1(J),LAB2(J)DEC04398          
     1,LAB3(J),LAB4(J),ARRAY(K3),RLHM                                   DEC04399          
  360 CONTINUE                                                          DEC04400          
 1109 FORMAT(1H ,I3,I5,2X,A6,2(1X,A6),2A6,2(1X,A6),A6,2X,F21.8,F13.4)   M0204401          
  359 IF (NLHM.EQ.0) GO TO 386                                          DEC04402          
      IF (IE.GT.NLHM.AND.LIOP.EQ.10) GO TO 376                          DEC04403          
      WRITE (6,1102)                                                    DEC04404          
      WRITE (6,1110) IJOB                                               DEC04405          
 1110 FORMAT (1H0,71H SUMS OF CROSSPRODUCTS AND CORRELATIONS OF LHM WITHDEC04406          
     1 RHM FOR PROBLEM NO.,I3)                                          DEC04407          
      WRITE (6,1111)                                                    DEC04408          
 1111 FORMAT (1H0,41HRHM  LHM  RHM NAME   INDEPENDENT VARIABLE,16X,4HC.PDEC04409          
     1.,7X,11HCORRELATION)                                              DEC04410          
      DO 375 I=1,NRHM                                                   DEC04411          
      K4=NLHM+I                                                         DEC04412          
      WRITE (6,1102)                                                    DEC04413          
      DO 375 J=1,NLHM                                                   DEC04414          
      IF (J.LT.IE.AND.LIOP.EQ.10) GO TO 375                             DEC04415          
      K=NLHM*(J-1)-J*(J-3)/2                                            DEC04416          
      IF (NCPR.EQ.0) GO TO 362                                          DEC04417          
      K1=NRHM*(I-1)-I*(I-3)/2                                           DEC04418          
      GO TO 364                                                         DEC04419          
  362 K1=I                                                              DEC04420          
  364 K2=NLHM*(I-1)+J                                                   DEC04421          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 372                   DEC04422          
      SCP=RHM(K2)-(TOT(J)*TOT(K4))/ARRAY(1)                             DEC04423          
      IF (J.GE.IE) GO TO 368                                            DEC04424          
      SSY1=ARRAY(K)-(TOT(J)*TOT(J))/ARRAY(1)                            DEC04425          
      GO TO 370                                                         DEC04426          
  368 SSY1=ARRAY(K)                                                     DEC04427          
  370 SSY2=SSCPR(K1)-(TOT(K4)*TOT(K4))/ARRAY(1)                         DEC04428          
      RLR=SCP/SQRT(SSY1*SSY2)                                           DEC04429          
      IF (J.LT.IE) GO TO 374                                            DEC04430          
      RHM(K2)=SCP                                                       DEC04431          
      GO TO 374                                                         DEC04432          
  372 RLR=RHM(K2)/SQRT(ARRAY(K)*SSCPR(K1))                              DEC04433          
  374 WRITE (6,1112) I,J,LITY(I),LAB1(J),LAB2(J),LAB3(J),LAB4(J),RHM(K2)DEC04434          
     1,RLR                                                              DEC04435          
  375 CONTINUE                                                          DEC04436          
 1112 FORMAT (1H ,I3,I5,A9,2X,2(A6,1X),2A6,2X,F21.8,F10.4)              DEC04437          
  376 IF (NCPR.EQ.0) GO TO 386                                          DEC04438          
      WRITE (6,1102)                                                    DEC04439          
      WRITE (6,1113) IJOB                                               DEC04440          
 1113 FORMAT (1H0,15X,64HSUMS OF SQUARES, C.P. AND CORRELATIONS AMONG RHDEC04441          
     1M FOR PROBLEM NO.,I3)                                             DEC04442          
      WRITE (6,1114)                                                    DEC04443          
 1114 FORMAT (1H0,20X,3HROW,6H   COL,4X,3HRHM,4X,3HRHM,15X,14HS.SQS. OR DEC04444          
     1C.P.,4X,11HCORRELATION)                                           DEC04445          
      DO 384 I=1,NRHM                                                   DEC04446          
      DO 384 J=I,NRHM                                                   DEC04447          
      K=NRHM*(I-1)-I*(I-3)/2                                            DEC04448          
      K1=NRHM*(J-1)-J*(J-3)/2                                           DEC04449          
      K3=K+J-I                                                          DEC04450          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 380                   DEC04451          
      K4=NLHM+I                                                         DEC04452          
      K5=NLHM+J                                                         DEC04453          
      SCP=SSCPR(K3)-(TOT(K4)*TOT(K5))/ARRAY(1)                          DEC04454          
      SSY1=SSCPR(K)-(TOT(K4)*TOT(K4))/ARRAY(1)                          DEC04455          
      SSY2=SSCPR(K1)-(TOT(K5)*TOT(K5))/ARRAY(1)                         DEC04456          
      RLHM=SCP/SQRT(SSY1*SSY2)                                          DEC04457          
      GO TO 382                                                         DEC04458          
  380 RLHM=SSCPR(K3)/SQRT(SSCPR(K)*SSCPR(K1))                           DEC04459          
  382 IF (I.NE.J) GO TO 384                                             DEC04460          
      WRITE (6,1102)                                                    DEC04461          
  384 WRITE (6,1115) I,J,LITY(I),LITY(J),SSCPR(K3),RLHM                 DEC04462          
 1115 FORMAT (1H ,20X,I3,I6,A9,A8,2X,F23.8,F14.4)                       DEC04463          
  386 N=NLHM                                                            DEC04464          
  500 CONTINUE                                                          DEC04465          
      RETURN                                                            DEC04466          
      END                                                               DEC04467          
*ENDTEXT                                                                                  
