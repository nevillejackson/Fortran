*TEXT                                                                                     
      SUBROUTINE MAIN1 (                                                  060001          
     1ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LAB4,LITY,TRED,    060002          
     2YM,IM,MS,IPL,NSP,NND,XP,YP,NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NEN,   060003          
     3NCLN,LNE,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NBEG,NEGX,LOGE,LQC,       060004          
     4NREGP,LGTX,JBEG,NDECX,LITR,XM,NEGY,LNY,LHY,KBEG,NDECY,IC,EFF1,      060005          
     5EFF2,NOS,X,NMAC,IED)                                                                
C     SUBROUTINE WHICH READS DATA CARDS, CALLS CODEX, COMPUTES LS OR ML   060007          
C     MATRICES, CALLS SCLNOS AND LISTS CERTAIN MATRICES, MEANS, SD AND    060008          
C     CORRELATIONS                                                        060009          
C     ----------------------------------------------------                060010          
      DIMENSION ARRAY(1),SSCPR(1),SSS(1),RHM(1),TOT(1),TOT2(1),TOT3(1),   060012          
     1LAB1(1),LAB2(1),LAB3(1),LAB4(1),LITY(1),TRED(1),YM(1),IM(1),MS(1),  060013          
     2IPL(1),NSP(1),NND(1),XP(1),YP(1),NDC(1),NMI(1),MEN(1),NCL(1),LME(1  060014          
     3),IBEG(1),IDEN(1),LIT(1),NEN(1),NCLN(1),LNE(1),NDEN(1),NLIT(1),     060015          
     4NMA(1),NMC(1),INT1(1),INT2(1),MSCL(1),NBEG(1),NEGX(1),LOGE(1),      060016          
     5LQC(1),NREGP(1),LGTX(1),JBEG(1),NDECX(1),LITR(1),XM(1),NEGY(1),     060017          
     6LNY(1),LHY(1),KBEG(1),NDECY(1),IC(1),EFF1(1),EFF2(1),NOS(1),X(1),   060018          
     7NMAC(1),IED(1)                                                                      
      DIMENSION NEQ(10)                                                                   
      COMMON /CMBLK1/NEQ                                                  060021          
      COMMON /CMBLK2/MXNLHM,MXNRHM,MXNCAS,MXNCF,MXMN2P,MXNS2,MXNSME,      060022          
     1 MXNMJC,MXNMIC,MXME,MXMECL,MXNE,MXNECL,MX2F,MX2FMS,MXNPR,MXNCD,     060023          
     2 MXK9,MXNOS,MXNED ,MXI2                                             060024          
      COMMON /CMBLK3/IJOB,NAB,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F,NPR,IRAN,M  060025          
     1POP,LIOP,IAD,REP,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,NOM,NON,ML,MLB,NS  060026          
     2,NOT,DF,KPUT,SDF,EDF,WK,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,RR         060027          
     3,IN,NED                                                             060028          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3,   060029          
     1L,L7                                                                060030          
      DATA (IBLK=1H )                                                     060031          
      DATA(JBLK=8H        )                                                               
      IF (LIOP.EQ.1.OR.LIOP.EQ.3) GO TO 1                                 060034          
      GO TO 2                                                             060035          
    1 WRITE (6,1001) IJOB                                                 060036          
 1001 FORMAT (1H1,20X,35HLISTING OF X MATRIX FOR PROBLEM NO.,I3)          060037          
      WRITE(6,5000)(NEQ(I),I=1,10)                                                        
 5000 FORMAT(1H0,20X,10A8/)                                                               
    2 MATX=NLHM*(NLHM+1)/2                                                060038          
      DO 4 I=1,MATX                                                       060039          
    4 ARRAY(I)=0.0                                                        060040          
      K1=NLHM*NRHM                                                        060041          
      DO 5 I=1,K1                                                         060042          
    5 RHM(I)=0.0                                                          060043          
      IF (NCPR.EQ.0) GO TO 6                                              060044          
      K1=NRHM*(NRHM+1)/2                                                  060045          
      GO TO 7                                                             060046          
    6 K1=NRHM                                                             060047          
    7 DO 8 I=1,K1                                                         060048          
    8 SSCPR(I)=0.0                                                        060049          
      NOT=NLHM+NRHM                                                       060050          
      DO 9 I=1,NOT                                                        060051          
      TOT(I)=0.0                                                          060052          
      TOT2(I)=0.0                                                         060053          
    9 TOT3(I)=0.0                                                         060054          
      DO 10 I=1,MXNOS                                                     060055          
   10 NOS(I)=0                                                            060056          
      K1=0                                                                060057          
      NCDS=0                                                              060058          
      MIN=0                                                               060059          
      GNI=0.                                                              060060          
      MJN=0                                                               060061          
      NCDG=0                                                              060062          
      NMIG=1                                                              060063          
      CALL PRERED(IC,IN,NTRA,IBLK,1,80,1)                                 060064          
   20 CALL READIN(IC,IN,NTRA,IBLK,1,80,1)                                 060065          
      IF (NTRA.EQ.0) GO TO 156                                            060066          
      NCDS=NCDS+1                                                         060067          
      IF (NCD.LT.2) GO TO 32                                              060068          
      DO 30 J=2,NCD                                                       060069          
      L=J*80                                                              060070          
      K=L-79                                                              060071          
      CALL READIN(IC,IN,NTRA,IBLK,K,L,J)                                  060072          
      K1=ICN1+K-1                                                         060073          
      K2=K1-80                                                            060074          
      IF (IC(K1).LE.IC(K2)) GO TO 901                                     060075          
   30 CONTINUE                                                            060076          
   32 CONTINUE                                                            060077          
C     ----------------------------------------------------                060078          
C     EDIT DETAIL CARD AND TEST REJECT                                                    
C     ----------------------------------------------------                060080          
      IF(NED) 790,790,791                                                                 
  791 CALL EDIT(IC,IN,NED,IED,ICOD)                                                       
      IF(ICOD) 900,790,792                                                060083          
  792 NCDS=NCDS-1                                                         060084          
      GO TO 20                                                            060085          
  790 CONTINUE                                                            060086          
      IF (NAB.GT.1) GO TO 100                                             060087          
   36 CONTINUE                                                            060088          
      CALL CODEX(                                                         060089          
     1ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LAB4,LITY,TRED,    060090          
     2YM,IM,MS,IPL,NSP,NND,XP,YP,NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NEN,   060091          
     3NCLN,LNE,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NBEG,NEGX,LOGE,LQC,       060092          
     4NREGP,LGTX,JBEG,NDECX,LITR,XM,NEGY,LNY,LHY,KBEG,NDECY,IC,EFF1,      060093          
     5EFF2,NOS,X,NMAC,IED)                                                                
      IF (MULL.EQ.1) RETURN                                               060095          
      IF (LIOP.EQ.1.OR.LIOP.EQ.3) GO TO 41                                060096          
      GO TO 39                                                            060097          
   41 WRITE (6,1002) (X(I),I=1,NOT)                                       060098          
 1002 FORMAT (1H ,17F7.2)                                                 060099          
   39 IF (NLHM.EQ.0) GO TO 47                                             060100          
   42 DO 46 I=1,NLHM                                                      060101          
      IF (X(I).EQ.0.0) GO TO 46                                           060102          
      DO 45 J=I,NLHM                                                      060103          
      K=NLHM*(I-1)-I*(I-3)/2+J-I                                          060104          
   45 ARRAY(K)=ARRAY(K)+X(I)*X(J)                                         060105          
   46 CONTINUE                                                            060106          
   47 DO 50 I=1,NOT                                                       060107          
   50 TOT(I)=TOT(I)+X(I)                                                  060108          
      IF (NRHM.EQ.0) GO TO 60                                             060109          
      IF (NCPR.EQ.0) GO TO 56                                             060110          
      DO 55 I=1,NRHM                                                      060111          
      DO 55 J=I,NRHM                                                      060112          
      K=NRHM*(I-1)-I*(I-3)/2+J-I                                          060113          
      K2=NLHM+I                                                           060114          
      K3=NLHM+J                                                           060115          
   55 SSCPR(K)=SSCPR(K)+X(K2)*X(K3)                                       060116          
      GO TO 58                                                            060117          
   56 CONTINUE                                                            060118          
      DO 57 I=1,NRHM                                                      060119          
      K2=NLHM+I                                                           060120          
   57 SSCPR(I)=SSCPR(I)+X(K2)*X(K2)                                       060121          
   58 K=0                                                                 060122          
      DO 59 I=1,NRHM                                                      060123          
      J2=NLHM+I                                                           060124          
      DO 59 J=1,NLHM                                                      060125          
      K=K+1                                                               060126          
   59 RHM(K)=RHM(K)+X(J)*X(J2)                                            060127          
   60 GO TO 20                                                            060128          
C     ----------------------------------------------------                060129          
C     ABSORPTION PROCESS                                                  060130          
C     ----------------------------------------------------                060131          
  100 CONTINUE                                                            060132          
      IF (NAB.LT.4) GO TO 110                                             060133          
      IMJ1=0                                                              060134          
      DO 102 I=1,NMJC                                                     060135          
      K=NDC(I)                                                            060136          
      IF (IC(K).NE.JBLK) GO TO 102                                        060137          
      IF (I.EQ.NMJC) GO TO 900                                            060138          
      IC(K)=0                                                             060139          
  102 IMJ1=IC(K)+IMJ1*10                                                  060140          
      IMI1=0                                                              060141          
      DO 104 I=1,NMIC                                                     060142          
      K=NMI(I)                                                            060143          
      IF (IC(K).NE.JBLK) GO TO 104                                        060144          
      IF (I.EQ.NMIC) GO TO 900                                            060145          
      IC(K)=0                                                             060146          
  104 IMI1=IC(K)+IMI1*10                                                  060147          
      IF (NCDS.EQ.1) GO TO 108                                            060148          
      IF (IMJ2-IMJ1) 164,107,901                                          060149          
  107 IF (IMI2-IMI1) 164,106,901                                          060150          
  106 NMIG=NMIG+1                                                         060151          
  108 IMI2=IMI1                                                           060152          
  109 IMJ2=IMJ1                                                           060153          
      GO TO 36                                                            060154          
  110 IMJ1=0                                                              060155          
      DO 112 I=1,NCC                                                      060156          
      K=NDC(I)                                                            060157          
      IF (IC(K).NE.JBLK) GO TO 112                                        060158          
      IC(K)=0                                                             060159          
      IF (I.EQ.NCC) GO TO 900                                             060160          
  112 IMJ1=IC(K)+IMJ1*10                                                  060161          
      IF (NCDS.EQ.1) GO TO 114                                            060162          
      IF (IMJ2-IMJ1) 116,114,901                                          060163          
  114 NCDG=NCDG+1                                                         060164          
      GO TO 109                                                           060165          
  116 DM=NCDG                                                             060166          
      IF (NAB.EQ.2) GO TO 118                                             060167          
      ROWN=1./(DM+REP)                                                    060168          
      GO TO 120                                                           060169          
  118 ROWN=1./DM                                                          060170          
  120 IF (NLHM.EQ.0) GO TO 130                                            060171          
      DO 124 I=1,NLHM                                                     060172          
      IF (TOT(I).EQ.0.0) GO TO 124                                        060173          
      DO 122 J=I,NLHM                                                     060174          
      K=NLHM*(I-1)-I*(I-3)/2+J-I                                          060175          
  122 ARRAY(K)=ARRAY(K)-TOT(I)*TOT(J)*ROWN                                060176          
  124 CONTINUE                                                            060177          
  130 IF (LIOP.EQ.0.OR.LIOP.EQ.5.OR.LIOP.EQ.10) GO TO 135                 060178          
      IF (NAB.EQ.4) GO TO 132                                             060179          
      WRITE (6,1050) IMJ2,NCDG                                            060180          
 1050 FORMAT (1H ,10X,14HMINOR CLASS IS,I12,6H   NI=,I4/1H ,   6HTOTALS)  060181          
      GO TO 134                                                           060182          
  132 WRITE (6,1050) IMI2,NMIG                                            060183          
  134 WRITE (6,1052) (TOT(I),I=1,NOT)                                     060184          
  135 IF (NRHM.EQ.0) GO TO 146                                            060185          
      IF (NCPR.EQ.0) GO TO 138                                            060186          
      DO 136 I=1,NRHM                                                     060187          
      DO 136 J=I,NRHM                                                     060188          
      K=NRHM*(I-1)-I*(I-3)/2+J-I                                          060189          
      K2=NLHM+I                                                           060190          
      K3=NLHM+J                                                           060191          
  136 SSCPR(K)=SSCPR(K)-TOT(K2)*TOT(K3)*ROWN                              060192          
      GO TO 142                                                           060193          
  138 DO 140 I=1,NRHM                                                     060194          
      K2=NLHM+I                                                           060195          
  140 SSCPR(I)=SSCPR(I)-TOT(K2)*TOT(K2)*ROWN                              060196          
  142 K=0                                                                 060197          
      DO 144 I=1,NRHM                                                     060198          
      J2=NLHM+I                                                           060199          
      DO 144 J=1,NLHM                                                     060200          
      K=K+1                                                               060201          
  144 RHM(K)=RHM(K)-TOT(J)*TOT(J2)*ROWN                                   060202          
  146 IF (NAB.EQ.1) GO TO 299                                             060203          
       DO 148 I=1,NOT                                                     060204          
      TOT3(I)=TOT3(I)+TOT(I)                                              060205          
  148 TOT2(I)=TOT2(I)+TOT(I)                                              060206          
      IF (NAB.NE.4) GO TO 152                                             060207          
      DO 150 I=1,NOT                                                      060208          
      TOT2(I)=TOT2(I)-DM*TOT(I)*ROWN                                      060209          
  150 TOT(I)=0.0                                                          060210          
      GNI=GNI-DM*DM*ROWN                                                  060211          
      GO TO 166                                                           060212          
  152 MJN=MJN+1                                                           060213          
      DO 154 I=1,NOT                                                      060214          
  154 TOT(I)=0.0                                                          060215          
      NCDG=1                                                              060216          
      IF (NTRA.EQ.0) GO TO 193                                            060217          
      GO TO 109                                                           060218          
  156 IF (NAB.NE.0) GO TO 158                                             060219          
      DF=NCDS                                                             060220          
      GO TO 299                                                           060221          
  158 IF (NAB.NE.1) GO TO 160                                             060222          
      DF=NCDS-1                                                           060223          
      GO TO 162                                                           060224          
  160 IF (NAB.LT.4) GO TO 116                                             060225          
      DF=NCDS-MIN-1                                                       060226          
      GO TO 164                                                           060227          
  162 DM=NCDS                                                             060228          
      ROWN=1./DM                                                          060229          
      GO TO 120                                                           060230          
  164 DM=NMIG                                                             060231          
      NCDG=NCDG+NMIG                                                      060232          
      ROWN=1./(DM+REP)                                                    060233          
      GNI=GNI+DM                                                          060234          
      GO TO 120                                                           060235          
  166 MIN=MIN+1                                                           060236          
      NMIG=1                                                              060237          
      IF (NTRA.EQ.0.OR.IMJ1.NE.IMJ2) GO TO 168                            060238          
      GO TO 108                                                           060239          
  168 ROWN=1./GNI                                                         060240          
      IF (NLHM.EQ.0) GO TO 172                                            060241          
      DO 170 I=1,NLHM                                                     060242          
      DO 170 J=I,NLHM                                                     060243          
      K=NLHM*(I-1)-I*(I-3)/2+J-I                                          060244          
  170 ARRAY(K)=ARRAY(K)-TOT2(I)*TOT2(J)*ROWN                              060245          
  172 IF (NRHM.EQ.0) GO TO 184                                            060246          
      IF (NCPR.EQ.0) GO TO 176                                            060247          
      DO 174 I=1,NRHM                                                     060248          
      DO 174 J=I,NRHM                                                     060249          
      K=NRHM*(I-1)-I*(I-3)/2+J-I                                          060250          
      K3=NLHM+J                                                           060251          
      K2=NLHM+I                                                           060252          
  174 SSCPR(K)=SSCPR(K)-TOT2(K2)*TOT2(K3)*ROWN                            060253          
      GO TO 180                                                           060254          
  176 DO 178 I=1,NRHM                                                     060255          
      K2=NLHM+I                                                           060256          
  178 SSCPR(I)=SSCPR(I)-TOT2(K2)*TOT2(K2)*ROWN                            060257          
  180 K=0                                                                 060258          
      DO 182 I=1,NRHM                                                     060259          
      J2=NLHM+I                                                           060260          
      DO 182 J=1,NLHM                                                     060261          
      K=K+1                                                               060262          
  182 RHM(K)=RHM(K)-TOT2(J)*TOT2(J2)*ROWN                                 060263          
  184 IF (LIOP.EQ.0.OR.LIOP.EQ.5) GO TO 190                               060264          
      WRITE (6,1051) IMJ2,GNI                                             060265          
 1051 FORMAT (1H0,5X,14HMAJOR CLASS IS,I12,3X,31HREDUCED DIAGONAL ELEMEN  060266          
     1T (GNI)=,F18.8)                                                     060267          
      WRITE (6,1052) (TOT2(I),I=1,NOT)                                    060268          
 1052 FORMAT (1H ,9F13.3)                                                 060269          
  190 DO 191 I=1,NOT                                                      060270          
  191 TOT2(I)=0.0                                                         060271          
      MJN=MJN+1                                                           060272          
      GNI=0.0                                                             060273          
      NCDG=0                                                              060274          
      IF (NTRA.EQ.0) GO TO 196                                            060275          
      GO TO 108                                                           060276          
  193 DO 194 I=1,NOT                                                      060277          
  194 TOT(I)=TOT2(I)                                                      060278          
      DF=NCDS-MJN                                                         060279          
      IF (NAB.EQ.3) DF=DF+1.                                              060280          
      IF (NAB.EQ.3.AND.IAD.EQ.4) DF=DF+FLOAT(NCL(1))-1.0                  060281          
      GO TO 299                                                           060282          
  196 DO 198 I=1,NOT                                                      060283          
  198 TOT(I)=TOT3(I)                                                      060284          
      GO TO 299                                                           060285          
  900 WRITE (6,1053) NCDS                                                 060286          
 1053 FORMAT (1H0,69HUNITS POSITION OF AN ID FIELD OR A CONTROL FIELD IS  060287          
     1 BLANK ON CARD NO.,I5)                                              060288          
      GO TO 902                                                           060289          
  901 WRITE (6,1054) NCDS,IJOB                                            060290          
 1054 FORMAT (1H0,38HCARDS OUT OF SEQUENCE---CHECK CARD NO.,I5,5X,15HFOR  060291          
     1 PROBLEM NO.,I3)                                                    060292          
  902 MULL=1                                                              060293          
      RETURN                                                              060294          
  299 CONTINUE                                                            060295          
      IF (NAB.EQ.0) GO TO 300                                             060296          
      GO TO (301,302,303,304), NAB                                        060297          
  300 WRITE (6,1060) NCDS                                                 060298          
 1060 FORMAT (1H1,5X,53HTOTAL LEAST-SQUARES ANALYSIS.  NO EQUATIONS ABSO  060299          
     1RBED.,5X,13HDF=NO. CARDS=,I5)                                       060300          
      WRITE(6,5000)(NEQ(I),I=1,10)                                                        
      GO TO 310                                                           060301          
  301 WRITE (6,1061) DF                                                   060302          
 1061 FORMAT (1H1,21X,38HEQUATION FOR THE OVERALL MEAN ABSORBED,5X,3HDF=  060303          
     1,F5.0)                                                              060304          
      WRITE(6,5000)(NEQ(I),I=1,10)                                                        
      GO TO 310                                                           060305          
  302 WRITE (6,1062) MJN,NCDS,DF                                          060306          
 1062 FORMAT (1H1,10X,55HNO. OF CLASSES OR SUBCLASSES ABSORBED BY LEAST   060307          
     1SQUARES=,I4,5X,10HNO. CARDS=,I5,5X,3HDF=,F5.0)                      060308          
      WRITE(6,5000)(NEQ(I),I=1,10)                                                        
      GO TO 310                                                           060309          
  303 WRITE (6,1063) MJN,REP,NCDS,DF                                      060310          
 1063 FORMAT (1H1,60HNO. OF CLASSES OR SUBCLASSES ABSORBED BY MAXIMUM LI  060311          
     1KELIHOOD=,I4,3X,7H1-R/R =,F7.4,3X,10HNO. CARDS=,I5,3X,3HDF=,F5.0)   060312          
      WRITE(6,5000)(NEQ(I),I=1,10)                                                        
      GO TO 310                                                           060313          
  304 WRITE (6,1064) MJN,MIN,REP,NCDS,DF                                  060314          
 1064 FORMAT (1H1,37H NO. OF MAJOR CLASSES ABSORBED BY ML=,I4,3X,21HNO.   060318          
     1MINOR SUBCLASSES=,I4,3X,7H1-R/R =,F7.4,3X,10HNO. CARDS=,I5,3X,3HDF  060319          
     3=,F5.0)                                                             060320          
      WRITE(6,5000)(NEQ(I),I=1,10)                                                        
  310 IF (NOM+NON.EQ.0) GO TO 320                                         060321          
      CALL SCLNOS(                                                        060322          
     1ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LAB4,LITY,TRED,    060323          
     2YM,IM,MS,IPL,NSP,NND,XP,YP,NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NEN,   060324          
     3NCLN,LNE,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NBEG,NEGX,LOGE,LQC,       060325          
     4NREGP,LGTX,JBEG,NDECX,LITR,XM,NEGY,LNY,LHY,KBEG,NDECY,IC,EFF1,      060326          
     5EFF2,NOS,X,NMAC,IED)                                                                
      IF (NLHM.EQ.0.OR.(NPR.EQ.0.AND.LIOP.EQ.10)) GO TO 330               060328          
  320 WRITE (6,1100) IJOB                                                 060329          
 1100 FORMAT (1H1,60HOVERALL MEANS AND STANDARD DEVIATIONS OF LHM FOR PR  060330          
     1OBLEM NO.,I3)                                                       060331          
      WRITE(6,5000)(NEQ(I),I=1,10)                                                        
      WRITE (6,1101)                                                      060332          
 1101 FORMAT (1H0,35H CODED LHM    INDEPENDENT VARIABLES,11X,4HMEAN,9X,4  060333          
     1HS.D.)                                                              060334          
      WRITE (6,1102)                                                      060335          
 1102 FORMAT (1H )                                                        060336          
      L=1                                                                 060337          
      K3=1                                                                060338          
      DO 329 I=1,NLHM                                                     060339          
      IF (I.LT.IE.AND.LIOP.EQ.10) GO TO 329                               060340          
      AMN=TOT(I)/FLOAT(NCDS)                                              060341          
      K=NLHM*(I-1)-I*(I-3)/2                                              060342          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 324                     060343          
      TOT(I)=ARRAY(I)                                                     060344          
      SD=SQRT((ARRAY(K)-(TOT(I)*TOT(I)/ARRAY(1)))/(DF-1.))                060345          
      GO TO 326                                                           060346          
  324 SD=SQRT(ARRAY(K)/DF)                                                060347          
  326 IF (I.LT.IE) GO TO 328                                              060348          
      L=L-1                                                               060349          
      IF (L.NE.0) GO TO 328                                               060350          
      IF (LQC(K3)-2) 390,391,392                                          060351          
  390 AMN=AMN+XM(K3)                                                      060352          
      K3=K3+1                                                             060353          
      L=1                                                                 060354          
      GO TO 328                                                           060355          
  391 K3=K3+1                                                             060356          
      L=2                                                                 060357          
      GO TO 328                                                           060358          
  392 K3=K3+1                                                             060359          
      L=3                                                                 060360          
  328 WRITE (6,1103) I,LAB1(I),LAB2(I),LAB3(I),LAB4(I),AMN,SD             060361          
  329 CONTINUE                                                            060362          
 1103 FORMAT(1H ,I6,6X,A6,2(1X,A6),A6,2F13.5)                             060363          
  330 IF (NRHM.EQ.0) GO TO 342                                            060364          
      WRITE (6,1104)                                                      060365          
 1104 FORMAT (1H1,15X,44HOVERALL MEANS AND STANDARD DEVIATIONS OF RHM)    060366          
      WRITE(6,5000)(NEQ(I),I=1,10)                                                        
      WRITE (6,1102)                                                      060367          
      L=NLHM*(NLHM+1)/2                                                   060368          
      DO 340 I=1,NRHM                                                     060369          
      K2=NLHM+I                                                           060370          
      AMN=TOT(K2)/FLOAT(NCDS)                                             060371          
      K=NLHM*(I-1)+1                                                      060372          
      IF (NAB.EQ.0.OR.NAB.EQ.3) TOT(K2)=RHM(K)                            060373          
      IF (NCPR.EQ.0) GO TO 332                                            060374          
      J=NRHM*(I-1)-I*(I-3)/2                                              060375          
      GO TO 334                                                           060376          
  332 J=I                                                                 060377          
  334 IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 336                     060378          
      SD=SQRT((SSCPR(J)-(TOT(K2)*TOT(K2)/ARRAY(1)))/(DF-1.))              060379          
      GO TO 338                                                           060380          
  336 SD=SQRT(SSCPR(J)/DF)                                                060381          
  338 AMN=AMN+YM(I)                                                       060382          
  340 WRITE (6,1105) LITY(I),AMN,SD                                       060383          
 1105 FORMAT (1H ,12X,A6,4X,5HMEAN=,F12.5,10X,5HS.D.=,F12.5)              060384          
  342 IF (LIOP.EQ.7) GO TO 500                                            060385          
  344 IF (NLHM.EQ.0) GO TO 359                                            060386          
      IF (IE.GT.NLHM.AND.LIOP.EQ.10) GO TO 359                            060387          
      WRITE (6,1102)                                                      060388          
      WRITE (6,1106) IJOB                                                 060389          
 1106 FORMAT (1H1,15X,64HSUMS OF SQUARES, C.P. AND CORRELATIONS AMONG LH  060390          
     1M FOR PROBLEM NO.,I3)                                               060391          
      WRITE(6,5000)(NEQ(I),I=1,10)                                                        
      WRITE (6,1107)                                                      060392          
 1107 FORMAT (1H0,4H ROW,5H  COL,15X,21HINDEPENDENT VARIABLES)            060393          
      WRITE (6,1108)                                                      060394          
 1108 FORMAT (1H ,9HCODE CODE,9X,3HROW,22X,6HCOLUMN,23X,28HS.SQS. OR C.P  060395          
     1.   CORRELATION)                                                    060396          
      DO 360 I=1,NLHM                                                     060397          
      DO 360 J=I,NLHM                                                     060398          
      IF (I.LT.IE.AND.J.LT.IE.AND.LIOP.EQ.10) GO TO 360                   060399          
      K=NLHM*(I-1)-I*(I-3)/2                                              060400          
      K1=NLHM*(J-1)-J*(J-3)/2                                             060401          
      K3=K+J-I                                                            060402          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 354                     060403          
      SCP=ARRAY(K3)-(TOT(I)*TOT(J))/ARRAY(1)                              060404          
      IF (I.GE.IE.AND.I.NE.J) GO TO 350                                   060405          
      SSY1=ARRAY(K)-(TOT(I)*TOT(I))/ARRAY(1)                              060406          
      GO TO 352                                                           060407          
  350 SSY1=ARRAY(K)                                                       060408          
  352 SSY2=ARRAY(K1)-(TOT(J)*TOT(J))/ARRAY(1)                             060409          
      RLHM=SCP/SQRT(SSY1*SSY2)                                            060410          
      IF (I.LT.IE.AND.J.LT.IE) GO TO 356                                  060411          
      ARRAY(K3)=SCP                                                       060412          
      GO TO 356                                                           060413          
  354 RLHM=ARRAY(K3)/SQRT(ARRAY(K)*ARRAY(K1))                             060414          
  356 IF (I.LT.IE.AND.LIOP.EQ.10) GO TO 360                               060415          
      IF (I.NE.J) GO TO 358                                               060416          
      RLHM=1.                                                             060417          
      WRITE (6,1102)                                                      060418          
  358 WRITE (6,1109) I,J,LAB1(I),LAB2(I),LAB3(I),LAB4(I),LAB1(J),LAB2(J)  060419          
     1,LAB3(J),LAB4(J),ARRAY(K3),RLHM                                     060420          
  360 CONTINUE                                                            060421          
 1109 FORMAT(1H ,I3,I5,2X,A6,2(1X,A6),2A6,2(1X,A6),A6,2X,F21.8,F13.4)     060422          
  359 IF (NLHM.EQ.0) GO TO 386                                            060423          
      IF (IE.GT.NLHM.AND.LIOP.EQ.10) GO TO 376                            060424          
      WRITE (6,1102)                                                      060425          
      WRITE (6,1110) IJOB                                                 060426          
 1110 FORMAT (1H1,71H SUMS OF CROSSPRODUCTS AND CORRELATIONS OF LHM WITH  060427          
     1 RHM FOR PROBLEM NO.,I3)                                            060428          
      WRITE(6,5000)(NEQ(I),I=1,10)                                                        
      WRITE (6,1111)                                                      060429          
 1111 FORMAT (1H0,41HRHM  LHM  RHM NAME   INDEPENDENT VARIABLE,16X,4HC.P  060430          
     1.,7X,11HCORRELATION)                                                060431          
      DO 375 I=1,NRHM                                                     060432          
      K4=NLHM+I                                                           060433          
      WRITE (6,1102)                                                      060434          
      DO 375 J=1,NLHM                                                     060435          
      IF (J.LT.IE.AND.LIOP.EQ.10) GO TO 375                               060436          
      K=NLHM*(J-1)-J*(J-3)/2                                              060437          
      IF (NCPR.EQ.0) GO TO 362                                            060438          
      K1=NRHM*(I-1)-I*(I-3)/2                                             060439          
      GO TO 364                                                           060440          
  362 K1=I                                                                060441          
  364 K2=NLHM*(I-1)+J                                                     060442          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 372                     060443          
      SCP=RHM(K2)-(TOT(J)*TOT(K4))/ARRAY(1)                               060444          
      IF (J.GE.IE) GO TO 368                                              060445          
      SSY1=ARRAY(K)-(TOT(J)*TOT(J))/ARRAY(1)                              060446          
      GO TO 370                                                           060447          
  368 SSY1=ARRAY(K)                                                       060448          
  370 SSY2=SSCPR(K1)-(TOT(K4)*TOT(K4))/ARRAY(1)                           060449          
      RLR=SCP/SQRT(SSY1*SSY2)                                             060450          
      IF (J.LT.IE) GO TO 374                                              060451          
      RHM(K2)=SCP                                                         060452          
      GO TO 374                                                           060453          
  372 RLR=RHM(K2)/SQRT(ARRAY(K)*SSCPR(K1))                                060454          
  374 WRITE (6,1112) I,J,LITY(I),LAB1(J),LAB2(J),LAB3(J),LAB4(J),RHM(K2)  060455          
     1,RLR                                                                060456          
  375 CONTINUE                                                            060457          
 1112 FORMAT (1H ,I3,I5,A9,2X,2(A6,1X),2A6,2X,F21.8,F10.4)                060458          
  376 IF (NCPR.EQ.0) GO TO 386                                            060459          
      WRITE (6,1102)                                                      060460          
      WRITE (6,1113) IJOB                                                 060461          
 1113 FORMAT (1H1,15X,64HSUMS OF SQUARES, C.P. AND CORRELATIONS AMONG RH  060462          
     1M FOR PROBLEM NO.,I3)                                               060463          
      WRITE(6,5000)(NEQ(I),I=1,10)                                                        
      WRITE (6,1114)                                                      060464          
 1114 FORMAT (1H0,20X,3HROW,6H   COL,4X,3HRHM,4X,3HRHM,15X,14HS.SQS. OR   060465          
     1C.P.,4X,11HCORRELATION)                                             060466          
      DO 384 I=1,NRHM                                                     060467          
      DO 384 J=I,NRHM                                                     060468          
      K=NRHM*(I-1)-I*(I-3)/2                                              060469          
      K1=NRHM*(J-1)-J*(J-3)/2                                             060470          
      K3=K+J-I                                                            060471          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 380                     060472          
      K4=NLHM+I                                                           060473          
      K5=NLHM+J                                                           060474          
      SCP=SSCPR(K3)-(TOT(K4)*TOT(K5))/ARRAY(1)                            060475          
      SSY1=SSCPR(K)-(TOT(K4)*TOT(K4))/ARRAY(1)                            060476          
      SSY2=SSCPR(K1)-(TOT(K5)*TOT(K5))/ARRAY(1)                           060477          
      RLHM=SCP/SQRT(SSY1*SSY2)                                            060478          
      GO TO 382                                                           060479          
  380 RLHM=SSCPR(K3)/SQRT(SSCPR(K)*SSCPR(K1))                             060480          
  382 IF (I.NE.J) GO TO 384                                               060481          
      WRITE (6,1102)                                                      060482          
  384 WRITE (6,1115) I,J,LITY(I),LITY(J),SSCPR(K3),RLHM                   060483          
 1115 FORMAT (1H ,20X,I3,I6,A9,A8,2X,F23.8,F14.4)                         060484          
  386 N=NLHM                                                              060485          
  500 CONTINUE                                                            060486          
      RETURN                                                              060487          
      END                                                                 060488          
*ENDTEXT                                                                                  
