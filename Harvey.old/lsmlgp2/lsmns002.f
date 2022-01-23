*TEXT                                                                                     
      SUBROUTINE LSMNS (                                                  180001          
     1ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LAB4,LITY,TRED,    180002          
     2YM,IM,MS,IPL,NSP,NND,XP,YP,NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NEN,   180003          
     3NCLN,LNE,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NBEG,NEGX,LOGE,LQC,       180004          
     4NREGP,LGTX,JBEG,NDECX,LITR,XM,NEGY,LNY,LHY,KBEG,NDECY,IC,EFF1,      180005          
     5EFF2,NOS,X,NMAC,IED)                                                                
C     SUBROUTINE FOR COMPUTING AND LISTING LEAST-SQUARES MEANS AND        180007          
C     STANDARD ERRORS WHEN NAB=0 OR NAB=3 AND BOTH NMEA AND NNEA          180008          
C     EQUAL ZERO.                                                         180009          
C     -------------------------------------------------------             180010          
      DIMENSION ARRAY(1),SSCPR(1),SSS(1),RHM(1),TOT(1),TOT2(1),TOT3(1),   180011          
     1LAB1(1),LAB2(1),LAB3(1),LAB4(1),LITY(1),TRED(1),YM(1),IM(1),MS(1),  180012          
     2IPL(1),NSP(1),NND(1),XP(1),YP(1),NDC(1),NMI(1),MEN(1),NCL(1),LME(1  180013          
     3),IBEG(1),IDEN(1),LIT(1),NEN(1),NCLN(1),LNE(1),NDEN(1),NLIT(1),     180014          
     4NMA(1),NMC(1),INT1(1),INT2(1),MSCL(1),NBEG(1),NEGX(1),LOGE(1),      180015          
     5LQC(1),NREGP(1),LGTX(1),JBEG(1),NDECX(1),LITR(1),XM(1),NEGY(1),     180016          
     6LNY(1),LHY(1),KBEG(1),NDECY(1),IC(1),EFF1(1),EFF2(1),NOS(1),X(1),   180017          
     7NMAC(1),IED(1)                                                                      
      DIMENSION NEQ(10)                                                                   
      COMMON /CMBLK1/NEQ                                                  180020          
      COMMON /CMBLK2/MXNLHM,MXNRHM,MXNCAS,MXNCF,MXMN2P,MXNS2,MXNSME,      180021          
     1 MXNMJC,MXNMIC,MXME,MXMECL,MXNE,MXNECL,MX2F,MX2FMS,MXNPR,MXNCD,     180022          
     2 MXK9,MXNOS,MXNED ,MXI2                                             180023          
      COMMON /CMBLK3/IJOB,NAB,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F,NPR,IRAN,M  180024          
     1POP,LIOP,IAD,REP,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,NOM,NON,ML,MLB,NS  180025          
     2,NOT,DF,KPUT,SDF,EDF,WK,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,RR         180026          
     3,IN,NED                                                             180027          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3,   180028          
     1L,L7                                                                180029          
      DATA (MU=6HMU    )                                                  180030          
      WRITE (6,1001) IJOB                                                 180031          
 1001 FORMAT (1H1,7X,77HLISTING OF CONSTANTS, LEAST-SQUARES MEANS AND ST  180032          
     1ANDARD ERRORS FOR PROBLEM NO.,I3)                                   180033          
      WRITE(6,5000)(NEQ(I),I=1,10)                                                        
 5000 FORMAT(1H0,20X,10A8)                                                                
      WRITE (6,1002)                                                      180034          
 1002 FORMAT (1H0,10H RHM   ROW,30X,3HNO.,7X,8HCONSTANT,6X,13HLEAST-SQUA  180035          
     1RES,7X,8HSTANDARD)                                                  180036          
      WRITE (6,1003)                                                      180037          
 1003 FORMAT (1H ,33H NAME  CODE  INDEPENDENT VARIABLE,7X,4HOBS.,6X,8HES  180038          
     1TIMATE,10X,4HMEAN,13X,5HERROR)                                      180039          
      I=0                                                                 180040          
    1 I=I+1                                                               180041          
      NCT=1                                                               180042          
      WRITE (6,1004)                                                      180043          
 1004 FORMAT (1H )                                                        180044          
      IF (NCPR.EQ.1) GO TO 2                                              180045          
      J=I                                                                 180046          
      GO TO 3                                                             180047          
    2 J=NRHM*(I-1)-I*(I-3)/2                                              180048          
    3 WK=(SSCPR(J)-TRED(I))/EDF                                           180049          
      IF (NAB.EQ.3) WK=WK/(RR+1.0)                                        180050          
      IF (WK.LT.0.) GO TO 151                                             180051          
      REP=SQRT(WK*ARRAY(1))                                               180052          
      J=MATX+(I-1)*NLHM+1                                                 180053          
      SDF=ARRAY(J)+YM(I)                                                  180054          
      WRITE (6,1005) LITY(I),MU,NCDS,SDF,SDF,REP                          180055          
 1005 FORMAT (1H ,A6,3X,1H1,A8,20X,I5,3F17.8)                             180056          
      L=1                                                                 180057          
      L2=1                                                                180058          
      K=ML+1                                                              180059          
      IF (NOM.EQ.0) GO TO 4                                               180060          
      DO 5 J=1,NOM                                                        180061          
      K2=NCL(J)                                                           180062          
      DO 5 K1=1,K2                                                        180063          
      IF (K1.EQ.K2) GO TO 7                                               180064          
      K3=IM(J)+K1                                                         180065          
      K4=NLHM*(K3-1)-K3*(K3-3)/2                                          180066          
      REP=SQRT(WK*(ARRAY(1)+ARRAY(K4)+2.0*ARRAY(K3)))                     180067          
      RAP=SQRT(WK*ARRAY(K4))                                                              
      K4=MATX+(I-1)*NLHM+K3                                               180068          
      AC=ARRAY(K4)                                                        180069          
      ALS=AC+SDF                                                          180070          
      L2=L2+1                                                             180071          
      L1=L2                                                               180072          
      GO TO 8                                                             180073          
    7 K3=IM(J)+1                                                          180074          
      K4=MATX+(I-1)*NLHM+K3                                               180075          
      AC=0.                                                               180076          
      US=0.                                                               180077          
      I5=K2-1                                                             180078          
      DO 9 M=1,I5                                                         180079          
      AC=AC-ARRAY(K4)                                                     180080          
      US=US-ARRAY(K3)                                                     180081          
      K4=K4+1                                                             180082          
    9 K3=K3+1                                                             180083          
      K5=IM(J)+K2-1                                                       180084          
      SS=0.                                                               180085          
      K3=IM(J)+1                                                          180086          
      DO 81 M=K3,K5                                                       180087          
      K4=NLHM*(M-1)-M*(M-3)/2                                             180088          
      DO 10 N=M,K5                                                        180089          
      IF (M.EQ.N) GO TO 80                                                180090          
      K6=K4+N-M                                                           180091          
      SS=SS+2.0*ARRAY(K6)                                                 180092          
      GO TO 10                                                            180093          
   80 SS=SS+ARRAY(K4)                                                     180094          
   10 CONTINUE                                                            180095          
   81 CONTINUE                                                            180096          
      REP=SQRT(WK*(ARRAY(1)+SS+2.0*US))                                   180097          
      RAP=SQRT(WK*SS)                                                                     
      ALS=AC+SDF                                                          180098          
      L1=0                                                                180099          
    8 WRITE (6,1006) LITY(I),L1,LIT(J),IDEN(K),NOS(L),AC,ALS,REP          180100          
 1006 FORMAT (1H ,A6,I4,A8,I5,15X,I5,3F17.8)                              180101          
      K=K+1                                                               180102          
    5 L=L+1                                                               180103          
C     -------------------------------------------------------             180104          
C     CONSTRUCTION OF X AND TOT2 VECTORS FOR NESTED MAIN EFFECTS          180105          
C     -------------------------------------------------------             180106          
    4 K=MLB+1                                                             180107          
      IF (NON.EQ.0) GO TO 6                                               180108          
      DO 11 J=1,NON                                                       180109          
      K4=NMA(J)-1                                                         180110          
      NSUM=1                                                              180111          
      IF (K4.EQ.0) GO TO 13                                               180112          
      DO 12 K3=1,K4                                                       180113          
   12 NSUM=NSUM+NCL(K3)-1                                                 180114          
   13 K2=NCLN(J)                                                          180115          
      DO 11 K1=1,K2                                                       180116          
      DO 14 K3=1,NLHM                                                     180117          
      TOT2(K3)=0.                                                         180118          
   14 X(K3)=0.                                                            180119          
      X(1)=1.                                                             180120          
      I2=NOM+J                                                            180121          
      K3=IM(I2)+K1                                                        180122          
      IF (NMAC(J).NE.NCL(K4+1)) GO TO 15                                  180123          
      I3=NCL(K4+1)-1                                                      180124          
      DO 16 K5=1,I3                                                       180125          
      I4=NSUM+K5                                                          180126          
   16 X(I4)=-1.0                                                          180127          
      IF (K1.NE.K2) GO TO 17                                              180128          
   20 I5=K2-1                                                             180129          
      DO 18 K5=1,I5                                                       180130          
      I4=IM(I2)+K5                                                        180131          
      X(I4)=-1.0                                                          180132          
   18 TOT2(I4)=-1.0                                                       180133          
      L1=0                                                                180134          
      GO TO 19                                                            180135          
   15 I4=NSUM+NMAC(J)                                                     180136          
      X(I4)=1.                                                            180137          
      IF (K1.NE.K2) GO TO 17                                              180138          
      GO TO 20                                                            180139          
   17 TOT2(K3)=1.                                                         180140          
      L2=L2+1                                                             180141          
      L1=L2                                                               180142          
      X(K3)=1.                                                            180143          
   19 YT=YM(I)                                                            180144          
      NR=MATX+(I-1)*NLHM+1                                                180145          
C     ------------------------------------------------------              180146          
C     CALL SUBROUTINE WHICH COMPUTES CONSTANT, LS MEAN AND SE, AND LISTS  180147          
C     ------------------------------------------------------              180148          
      CALL CANDSE (AC,ALS,REP,TOT2,X,ARRAY,NR,NLHM,WK,YT,I,SSS,NCT)       180149          
      WRITE (6,1006) LITY(I),L1,NLIT(J),NDEN(K),NOS(L),AC,ALS,REP         180150          
      RAP=0.0                                                                             
      K=K+1                                                               180151          
   11 L=L+1                                                               180152          
C     ------------------------------------------------------              180153          
C     CONSTRUCTION OF X AND TOT2 VECTORS FOR TWO-WAY SUBCLASSES           180154          
C     ------------------------------------------------------              180155          
    6 NOT=NOM+NON+1                                                       180156          
      K8=IM(NOT)                                                          180157          
      K6=0                                                                180158          
      IF (N2F.EQ.0) GO TO 21                                              180159          
      DO 22 J=1,N2F                                                       180160          
      MJ1=0                                                               180161          
      MJ2=0                                                               180162          
      MJ3=0                                                               180163          
      MJ4=0                                                               180164          
C     ------------------------------------------------------              180165          
C     SETS X AND TOT2 ARRAYS TO ZERO AND SETS X(1) TO 1 FOR MU            180166          
C     ------------------------------------------------------              180167          
      DO 23 K1=1,NLHM                                                     180168          
      TOT2(K1)=0.                                                         180169          
   23 X(K1)=0.                                                            180170          
      X(1)=1.                                                             180171          
      K6=K6+NMC(J)                                                        180172          
      NSUM=0                                                              180173          
      K4=INT2(J)-1                                                        180174          
      MSUM=0                                                              180175          
      K5=INT1(J)-1                                                        180176          
      IF (INT1(J).GT.NOM) GO TO 24                                        180177          
      MJ1=INT1(J)                                                         180178          
      IF (K5.EQ.0) GO TO 25                                               180179          
      DO 26 K3=1,K5                                                       180180          
   26 NSUM=NSUM+NCL(K3)                                                   180181          
   25 K2=INT1(J)                                                          180182          
      I1=IM(K2)+1                                                         180183          
      I2=IM(K2+1)+1                                                       180184          
      ID1=LIT(K2)                                                         180185          
      GO TO 27                                                            180186          
C     ------------------------------------------------------              180187          
C     SETS UP +1 AND -1 FOR MAJOR CLASS WHEN FIRST MAIN EFFECT IS         180188          
C     NESTED                                                              180189          
C     ------------------------------------------------------              180190          
   24 K2=NOM+1                                                            180191          
      IF (K2.GT.K5) GO TO 28                                              180192          
      DO 29 K3=K2,K5                                                      180193          
      K=K3-NOM                                                            180194          
   29 NSUM=NSUM+NCLN(K)                                                   180195          
   28 K2=INT1(J)-NOM                                                      180196          
      K3=INT1(J)                                                          180197          
      I1=IM(K3)+1                                                         180198          
      I2=IM(K3+1)+1                                                       180199          
      ID1=NLIT(K2)                                                        180200          
      K=NMA(K2)                                                           180201          
      MJ2=NMA(K2)                                                         180202          
      NI=NMAC(K2)                                                         180203          
      IF (NMAC(K2).EQ.NCL(K)) GO TO 60                                    180204          
      K7=IM(K)+NMAC(K2)                                                   180205          
      X(K7)=1.                                                            180206          
      GO TO 27                                                            180207          
   60 K1=NCL(K)-1                                                         180208          
      DO 61 K3=1,K1                                                       180209          
      K2=IM(K)+K3                                                         180210          
   61 X(K2)=-1.                                                           180211          
   27 IF (INT2(J).GT.NOM) GO TO 30                                        180212          
      MJ3=INT2(J)                                                         180213          
      IF (K4.EQ.0) GO TO 31                                               180214          
      DO 32 K3=1,K4                                                       180215          
   32 MSUM=MSUM+NCL(K3)                                                   180216          
   31 K2=INT2(J)                                                          180217          
      I3=IM(K2)+1                                                         180218          
      I4=IM(K2+1)+1                                                       180219          
      ID2=LIT(K2)                                                         180220          
      GO TO 33                                                            180221          
C     ------------------------------------------------------              180222          
C     SETS UP +1 AND -1 FOR MAJOR CLASS WHEN SECOND MAIN EFFECT IS        180223          
C     NESTED                                                              180224          
C     ------------------------------------------------------              180225          
   30 K2=NOM+1                                                            180226          
      IF (K2.GT.K4) GO TO 34                                              180227          
      DO 35 K3=K2,K4                                                      180228          
      K=K3-NOM                                                            180229          
   35 MSUM=MSUM+NCLN(K)                                                   180230          
   34 K2=INT2(J)-NOM                                                      180231          
      K3=INT2(J)                                                          180232          
      I3=IM(K3)+1                                                         180233          
      I4=IM(K3+1)+1                                                       180234          
      ID2=NLIT(K2)                                                        180235          
      K=NMA(K2)                                                           180236          
      MJ4=NMA(K2)                                                         180237          
      NJ=NMAC(K2)                                                         180238          
      IF (NMAC(K2).EQ.NCL(K)) GO TO 62                                    180239          
      K7=IM(K)+NMAC(K2)                                                   180240          
      X(K7)=1.                                                            180241          
      GO TO 111                                                           180242          
   62 K1=NCL(K)-1                                                         180243          
      DO 63 K3=1,K1                                                       180244          
      K2=IM(K)+K3                                                         180245          
   63 X(K2)=-1.                                                           180246          
C     ------------------------------------------------------              180247          
C     SETS UP X MATRIX FOR MAIN EFFECTS AND BOTH X AND TOT2 ARRAYS FOR    180248          
C     INTERACTION CONSTANTS                                               180249          
C     ------------------------------------------------------              180250          
  111 IF (MJ2.NE.MJ4.AND.MJ1.EQ.MJ3) GO TO 100                            180251          
      GO TO 33                                                            180252          
  100 ME1=MJ2                                                             180253          
      ME2=MJ4                                                             180254          
      CALL INTACT (ME1,ME2,NI,NJ,N2F,NOT,INT1,INT2,X,NMC,MSCL,NCL,EFF1,E  180255          
     1FF2,IM,NCLN,NOM)                                                    180256          
   33 DO 36 K=I1,I2                                                       180257          
      J2=K-I1+1                                                           180258          
C     ------------------------------------------------------              180259          
C     SETS UP X ARRAY FOR FIRST MAIN EFFECT                               180260          
C     ------------------------------------------------------              180261          
      L3=I2-1                                                             180262          
      IF (K.EQ.I2) GO TO 42                                               180263          
      DO 64 K1=I1,L3                                                      180264          
   64 X(K1)=0.                                                            180265          
      X(K)=1.                                                             180266          
      GO TO 41                                                            180267          
   42 DO 46 J4=I1,L3                                                      180268          
   46 X(J4)=-1.                                                           180269          
C     ------------------------------------------------------              180270          
C     GETS IDENTIFICATION OF CLASS FOR FIRST MAIN EFFECT                  180271          
C     ------------------------------------------------------              180272          
   41 K2=NSUM+J2                                                          180273          
      IF (INT1(J).GT.NOM) GO TO 37                                        180274          
      ID3=IDEN(K2)                                                        180275          
      GO TO 38                                                            180276          
   37 ID3=NDEN(K2)                                                        180277          
   38 DO 36 J1=I3,I4                                                      180278          
      AC=0.0                                                                              
      ALS=0.0                                                                             
      RAP=0.0                                                                             
      REP=0.0                                                                             
      J3=J1-I3+1                                                          180279          
C     ------------------------------------------------------              180280          
C     CHECKS FOR MISSING SUBCLASS                                         180281          
C     ------------------------------------------------------              180282          
      IF (NMC(J).EQ.0) GO TO 45                                           180283          
      K7=J2*1000+J3                                                       180284          
      K1=K6-NMC(J)                                                        180285          
   43 K1=K1+1                                                             180286          
      IF (K1.GT.K6) GO TO 45                                              180287          
      IF (K7-MSCL(K1)) 43,44,43                                           180288          
C     ------------------------------------------------------              180289          
C     SETS UP X ARRAY FOR SECOND MAIN EFFECT                              180290          
C     ------------------------------------------------------              180291          
   45 I5=I4-1                                                             180292          
      IF (J1.EQ.I4) GO TO 47                                              180293          
      DO 40 K1=I3,I5                                                      180294          
   40 X(K1)=0.                                                            180295          
      X(J1)=1.                                                            180296          
      GO TO 48                                                            180297          
   47 DO 49 J4=I3,I5                                                      180298          
   49 X(J4)=-1.                                                           180299          
C     ------------------------------------------------------              180300          
C     GETS IDENTIFICATION OF CLASS FOR SECOND MAIN EFFECT                 180301          
C     ------------------------------------------------------              180302          
   48 K2=MSUM+J3                                                          180303          
      IF (INT2(J).GT.NOM) GO TO 39                                        180304          
      ID4=IDEN(K2)                                                        180305          
      GO TO 110                                                           180306          
   39 ID4=NDEN(K2)                                                        180307          
C     ------------------------------------------------------              180308          
C     SETS UP X AND TOT2 ARRAYS FOR INTERACTION EFFECT                    180309          
C     ------------------------------------------------------              180310          
  110 IF (MJ1.NE.0.AND.MJ3.EQ.0) GO TO 101                                180311          
      GO TO 102                                                           180312          
  101 ME1=MJ1                                                             180313          
      ME2=MJ4                                                             180314          
      NI=J2                                                               180315          
      CALL INTACT (ME1,ME2,NI,NJ,N2F,NOT,INT1,INT2,X,NMC,MSCL,NCL,EFF1,E  180316          
     1FF2,IM,NCLN,NOM)                                                    180317          
  102 IF (MJ3.NE.0.AND.MJ1.EQ.0) GO TO 103                                180318          
      GO TO 104                                                           180319          
  103 ME1=MJ2                                                             180320          
      ME2=MJ3                                                             180321          
      NJ=J3                                                               180322          
      CALL INTACT (ME1,ME2,NI,NJ,N2F,NOT,INT1,INT2,X,NMC,MSCL,NCL,EFF1,E  180323          
     1FF2,IM,NCLN,NOM)                                                    180324          
  104 IF (MJ2.NE.MJ4.AND.MJ1.EQ.MJ3) GO TO 105                            180325          
      GO TO 74                                                            180326          
  105 ME1=INT1(J)                                                         180327          
      ME2=MJ4                                                             180328          
      NI=J2                                                               180329          
      K2=INT2(J)-NOM                                                      180330          
      NJ=NMAC(K2)                                                         180331          
      CALL INTACT (ME1,ME2,NI,NJ,N2F,NOT,INT1,INT2,X,NMC,MSCL,NCL,EFF1,E  180332          
     1FF2,IM,NCLN,NOM)                                                    180333          
      ME1=MJ2                                                             180334          
      ME2=INT2(J)                                                         180335          
      K2=INT1(J)-NOM                                                      180336          
      NI=NMAC(K2)                                                         180337          
      NJ=J3                                                               180338          
      CALL INTACT (ME1,ME2,NI,NJ,N2F,NOT,INT1,INT2,X,NMC,MSCL,NCL,EFF1,E  180339          
     1FF2,IM,NCLN,NOM)                                                    180340          
   74 L3=I2-I1                                                            180341          
      I5=I4-I3                                                            180342          
      N=NOT+J-1                                                           180343          
      K2=IM(N)+1                                                          180344          
      N=L3*I5-NMC(J)                                                      180345          
      M=K2+N-1                                                            180346          
      DO 59 J4=K2,M                                                       180347          
      TOT2(J4)=0.                                                         180348          
   59 X(J4)=0.                                                            180349          
      DO 50 J4=1,L3                                                       180350          
   50 EFF1(J4)=0.                                                         180351          
      DO 51 J4=1,I5                                                       180352          
   51 EFF2(J4)=0.                                                         180353          
      IF  (K.EQ.I2) GO TO 52                                              180354          
      EFF1(J2)=1.                                                         180355          
      GO TO 53                                                            180356          
   52 DO 54 J4=1,L3                                                       180357          
   54 EFF1(J4)=-1.                                                        180358          
   53 IF (J1.EQ.I4) GO TO 55                                              180359          
      EFF2(J3)=1.                                                         180360          
      GO TO 56                                                            180361          
   55 DO 76 J4=1,I5                                                       180362          
   76 EFF2(J4)=-1.                                                        180363          
   56 K5=K8+1                                                             180364          
      DO 82 K1=1,L3                                                       180365          
      DO 58 K2=1,I5                                                       180366          
      K7=K1*1000+K2                                                       180367          
      IF (NMC(J).EQ.0) GO TO 71                                           180368          
      J4=K6-NMC(J)                                                        180369          
   70 J4=J4+1                                                             180370          
      IF (J4.GT.K6) GO TO 71                                              180371          
      IF (K7-MSCL(J4)) 70,58,70                                           180372          
   71 X(K5)=EFF1(K1)*EFF2(K2)                                             180373          
      TOT2(K5)=X(K5)                                                      180374          
      K5=K5+1                                                             180375          
   58 CONTINUE                                                            180376          
   82 CONTINUE                                                            180377          
      IF  (K.EQ.I2.OR.J1.EQ.I4) GO TO 72                                  180378          
      L2=L2+1                                                             180379          
      L1=L2                                                               180380          
      GO TO 73                                                            180381          
   72 L1=0                                                                180382          
   73 NR=MATX+(I-1)*NLHM+1                                                180383          
      YT=YM(I)                                                            180384          
      CALL CANDSE (AC,ALS,REP,TOT2,X,ARRAY,NR,NLHM,WK,YT,I,SSS,NCT)       180385          
      WRITE (6,1007) LITY(I),L1,ID1,ID2,ID3,ID4,NOS(L),AC,ALS,REP         180386          
 1007 FORMAT (1H ,A6,I4,A8,3H X ,A6,2I5,I6,3F17.8)                        180387          
   44 L=L+1                                                               180388          
      RAP=0.0                                                                             
   36 CONTINUE                                                            180389          
      K8=K8+L3*I5-NMC(J)                                                  180390          
   22 CONTINUE                                                            180391          
C     ------------------------------------------------------              180392          
C     LISTING OF PARTIAL REGRESSIONS AND STANDARD ERRORS FOR              180393          
C     CONTINUOUS INDEPENDENT VARIABLES                                    180394          
C     ------------------------------------------------------              180395          
   21 IF (NPR.EQ.0) GO TO 75                                              180396          
      L1=L2+1                                                             180397          
      DO 57 J=IE,NLHM                                                     180398          
      L=NLHM*(J-1)-J*(J-3)/2                                              180399          
      K=MATX+(I-1)*NLHM+J                                                 180400          
      REP=SQRT(ARRAY(L)*WK)                                               180401          
      WRITE (6,1008) LITY(I),L1,LAB1(J),LAB2(J),LAB3(J),LAB4(J),ARRAY(K)  180402          
     1,REP                                                                180403          
 1008 FORMAT (1H ,A6,I4,4A7,5X,F17.8,17X,F17.8)                           180404          
   57 L1=L1+1                                                             180405          
   75 IF (I.NE.NRHM) GO TO 1                                              180406          
      RETURN                                                              180407          
  151 KPUT=1                                                              180408          
      RETURN                                                              180409          
      END                                                                 180410          
*ENDTEXT                                                                                  
