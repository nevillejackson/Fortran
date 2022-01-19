*TEXT                                                                                     
      SUBROUTINE OPREG(IIA,IIB,IIC,                                     OPR30001          
     1T2,COMENV,WFSENV,DF,IDF,SS,SM,F,XMN,VC1,VC2,VC3,HS,HD,HSD,SE1,SE2,OPR30002          
     2SE3,THETA,RTHETA,SER,THETGS,THETPD,HDFS,VAR1,NAMVAR,              OPR30003          
     3REGSS,DEVSS,SE4,HB,HR,RB,SRR,DEVSM,FREG,B,FDEV,CORS,CORD,CHS,VA,  OPR30004          
     4VD,VAA,SL1,SL2,SL3,SCR1,SESCR1,SCR2,SESCR2,A,FKH,LS,XMAT1,XMAT2,  OPR30005          
     5TS)                                                               OPR30006          
C-----VERSION 3                                                         OPR30007          
      COMMON/CMBLK1/I1,I2,N3,N4,N5,N6,IS,ID,IM,IF,N1,ITLE(9),Y,KPUT     OPR30008          
      COMMON/CMBLK5/NPROB,Q1,Q2,Q3,BIVAR1,BIVAR2,BIVAR3,I6,I7,I8,I9     OPR30009          
      COMMON/CMBLK6/ DFS,DFD,IDFS,IDFD                                  OPR30010          
      DIMENSION T2(3,7),COMENV(IIA,1),WFSENV(IIA,1),DF(1),IDF(1),       OPR30011          
     1 SS(IIC,IIA,1),SM(IIB,IIA,1),F(IIB,IIA,1),XMN(IIA,1),VC1(IIA,1),  OPR30012          
     2 VC2(IIA,1),VC3(IIA,1),HS(IIA,1),HD(IIA,1),HSD(IIA,1),SE1(IIA,1), OPR30013          
     3 SE2(IIA,1),SE3(IIA,1),THETA(7,IIA,1),RTHETA(7,IIA,1),SER(7,IIA,1)OPR30014          
     4 ,THETGS(IIA,1),THETPD(IIA,1),HDFS(IIA,1),VAR1(IIA,1),NAMVAR(1)   OPR30015          
      DIMENSION REGSS(4,IIA,1),DEVSS(4,IIA,1),SE4(4,IIA,1),HB(4,IIA,1), OPR30016          
     1HR(4,IIA,1),RB(4,IIA,1),SRR(4,IIA,1),DEVSM(4,IIA,1),FREG(4,IIA,1),OPR30017          
     2B(4,IIA,1),FDEV(4,IIA,1),CORS(IIA,1),CORD(IIA,1),CHS(IIA,1),      OPR30018          
     3VA(IIA,1),VD(IIA,1),VAA(IIA,1),SL1(IIA,1),SL2(IIA,1),SL3(IIA,1),  OPR30019          
     4SCR1(3,IIA,1),SESCR1(3,IIA,1),SCR2(4,IIA,1),SESCR2(4,IIA,1),      OPR30020          
     5A(4,IIA,1),FKH(3),LS(3),XMAT1(IIA,1),XMAT2(IIA,1),TS(3,7)         OPR30021          
      REAL LS                                                           OPR30022          
      DFS=DF(I6)-1.                                                     OPR30023          
      DFD=DF(I7)-1.                                                     OPR30024          
      DFSDV=DFS+DF(I7)+DF(I8)                                           OPR30025          
      DFDDV=DFD+DF(I8)                                                  OPR30026          
      IDFS=DFS                                                          OPR30027          
      IDFD=DFD                                                          OPR30028          
      QC1=(Q3+Q1)/2.0-1.0                                               OPR30029          
      QC2=DIVF(Q3+Q1-2.0,2.0*Q3)                                        OPR30030          
      ITEMP=IS+ID+IM+IF                                                 OPR30031          
      IF (ITEMP .EQ. I1) GO TO 2                                        OPR30032          
      PRINT 1,IS,ID,IM,IF                                               OPR30033          
    1 FORMAT(1H0,43HTHE SUM OF THE PARTS DO NOT EQUAL THE WHOLE /,4H IS=OPR30034          
     1I2,3HID=I2,3HIM=I2,3HIF=I2)                                       OPR30035          
    2 CONTINUE                                                          OPR30036          
C     SIRE         (1 TO IS,                   1 TO IS)                 OPR30037          
C     DAM          (IS+1 TO IS+ID,             IS+1 TO IS+ID)           OPR30038          
C     S-O  (MALES) (IS+ID+1 TO IS+ID+IM,       1 TO IS)                 OPR30039          
C     S-O  (MALES) (IS+ID+1 TO IS+ID+IM,       1 TO IS)                 OPR30040          
C          (FEMLS) (IS+ID+1 TO IS+ID+IM+IF,    1 TO IS)                 OPR30041          
C     D-O  (MALES) (IS+ID+1 TO IS+ID+IM,       IS+1 TO IS+ID)           OPR30042          
C          (FEMLS) (IS+ID+IM+1 TO IS+ID+IM+IF, IS+1 TO IS+ID)           OPR30043          
C                                                                       OPR30044          
C     --------------------------------------------                      OPR30045          
C     SIRE REGRESSION AND CORRELATION.                                  OPR30046          
C     --------------------------------------------                      OPR30047          
      IF (IS .LE. 0) GO TO 15                                           OPR30048          
      A1=DIVF(1.,2.*SQTF(DF(I6)))                                       OPR30049          
C     --------------------------------------------                      OPR30050          
C     AOV AND HERITABILITIES AND ITS CORRECTION.                        OPR30051          
C     --------------------------------------------                      OPR30052          
      DO 4 I=1,IS                                                       OPR30053          
      DO 4 J=I,IS                                                       OPR30054          
      JS=I                                                              OPR30055          
      JM=J+IS+ID                                                        OPR30056          
      JF=J+IS+ID+IM                                                     OPR30057          
C                                                                       OPR30058          
      IF(IM .EQ. IF) GO TO 21                                           OPR30059          
      IF(IF .GT. 0) GO TO 20                                            OPR30060          
      CORS(I,J)=1.0                                                     OPR30061          
      GO TO 22                                                          OPR30062          
   20 CORS(I,J)=SQTF(DIVF(SM(I6,JS,JS),SM(I6,JF,JF)))                   OPR30063          
      GO TO 22                                                          OPR30064          
   21 CORS(I,J)=SQTF(DIVF(SM(I6,JM,JM),SM(I6,JF,JF)))                   OPR30065          
   22 B(1,I,J)=DIVF(SS(I6,JS,JM),SS(I6,JS,JS))                          OPR30066          
      B(2,I,J)=DIVF(SS(I6,JS,JF),SS(I6,JS,JS))                          OPR30067          
      REGSS(1,I,J)=B(1,I,J)*SS(I6,JS,JM)                                OPR30068          
      REGSS(2,I,J)=B(2,I,J)*SS(I6,JS,JF)                                OPR30069          
      DEVSS(1,I,J)=SS(I6,JM,JM)-REGSS(1,I,J)                            OPR30070          
      DEVSS(2,I,J)=SS(I6,JF,JF)-REGSS(2,I,J)                            OPR30071          
      HB(1,I,J)=2.*B(1,I,J)                                             OPR30072          
      HB(2,I,J)=2.*B(2,I,J)*CORS(I,J)                                   OPR30073          
      HR(1,I,J)=2.*CORF(SS(I6,JS,JM),SS(I6,JS,JS),SS(I6,JM,JM)+SS(I7,JM,OPR30074          
     1JM)+SS(I8,JM,JM))                                                 OPR30075          
      HR(2,I,J)=2.*CORF(SS(I6,JS,JF),SS(I6,JS,JS),SS(I6,JF,JF)+SS(I7,JF,OPR30076          
     1JF)+SS(I8,JF,JF))                                                 OPR30077          
      DO 3 K=1,2                                                        OPR30078          
      DEVSM(K,I,J)=DIVF(DEVSS(K,I,J),DFS)                               OPR30079          
      FREG(K,I,J)=DIVF(REGSS(K,I,J),DEVSM(K,I,J))                       OPR30080          
    3 CONTINUE                                                          OPR30081          
      FDEV(1,I,J)=DIVF(DEVSM(1,I,J),SM(I7,JM,JM))                       OPR30082          
      FDEV(2,I,J)=DIVF(DEVSM(2,I,J),SM(I7,JF,JF))                       OPR30083          
      SE4(1,I,J)=2.*SQTF(DIVF(SS(I6,JM,JM)+SS(I7,JM,JM)+SS(I8,JM,JM)-REGOPR30084          
     1SS(1,I,J),DFSDV*SS(I6,JS,JS)))                                    OPR30085          
      SE4(2,I,J)=2.*SQTF(DIVF(SS(I6,JF,JF)+SS(I7,JF,JF)+SS(I8,JF,JF)-REGOPR30086          
     1SS(2,I,J),DFSDV*SS(I6,JS,JS)))*CORS(I,J)                          OPR30087          
    4 CONTINUE                                                          OPR30088          
C     --------------------------------------------                      OPR30089          
C     GENETIC CORRLN.,STD. CORRLD. RESPONSE. AND STD. ERRORS.           OPR30090          
C     --------------------------------------------                      OPR30091          
      ISX=IS-1                                                          OPR30092          
      IF(ISX) 15,15,981                                                 OPR30093          
  981 DO 12 I=1,ISX                                                     OPR30094          
      JMIN=I+1                                                          OPR30095          
      DO 12 J=JMIN,IS                                                   OPR30096          
      JS1=I                                                             OPR30097          
      JS2=J                                                             OPR30098          
      JM1=I+IS+ID                                                       OPR30099          
      JM2=J+IS+ID                                                       OPR30100          
      JF1=I+IS+ID+IM                                                    OPR30101          
      JF2=J+IS+ID+IM                                                    OPR30102          
      SDPARS=SQTF(SS(I6,JS1,JS1)*SS(I6,JS2,JS2))                        OPR30103          
      IF (IM .LE. 1) GO TO 7                                            OPR30104          
      IF(SS(I6,JS1,JM1).GT.0. .AND. SS(I6,JS2,JM2).GT.0.) GO TO 6       OPR30105          
      RB(1,I,J)=0.                                                      OPR30106          
      PRINT 5,JM1,JM2                                                   OPR30107          
    5 FORMAT(32H0 P-O CORRELATION FOR CHARACTERS ,I2,3HAND,I2,20HCANNOT OPR30108          
     1BE CALCULATED)                                                    OPR30109          
      GO TO  62                                                         OPR30110          
    6 RB(1,I,J)=(SS(I6,JS1,JM2)+SS(I6,JS2,JM1))/(2.*(SQRT(SS(I6,JS1,JM1)OPR30111          
     1             *SS(I6,JS2,JM2))))                                   OPR30112          
   62 SCR2(1,I,J)=DIVF(SS(I6,JS1,JM2)+SS(I6,JS2,JM1),SDPARS)            OPR30113          
    7 IF (IF .LE. 1) GO TO 9                                            OPR30114          
      IF(SS(I6,JS1,JF1).GT.0. .AND. SS(I6,JS2,JF2).GT.0.) GO TO 8       OPR30115          
      RB(2,I,J)=0.                                                      OPR30116          
      PRINT 5,JF1,JF2                                                   OPR30117          
      GO TO  82                                                         OPR30118          
    8 RB(2,I,J)=(SS(I6,JS1,JF2)+SS(I6,JS2,JF1))/(2.*(SQRT(SS(I6,JS1,JF1)OPR30119          
     1             *SS(I6,JS2,JF2))))                                   OPR30120          
   82 SCR2(2,I,J)=DIVF(SS(I6,JS1,JF2)+SS(I6,JS2,JF1),SDPARS)            OPR30121          
    9 C1=SQTF(HB(1,I,I)*HB(1,J,J))                                      OPR30122          
      C2=SQTF(HB(2,I,I)*HB(2,J,J))                                      OPR30123          
      C1S=C1*C1                                                         OPR30124          
      C2S=C2*C2                                                         OPR30125          
      RECD1=0.5*(DIVF(1.,HB(1,I,I))+DIVF(1.,HB(1,J,J)))                 OPR30126          
      RECD2=0.5*(DIVF(1.,HB(2,I,I))+DIVF(1.,HB(2,J,J)))                 OPR30127          
      GO TO (10,10,11),N3                                               OPR30128          
   10 CON1=1.-RB(1,I,J)*RB(1,I,J)                                       OPR30129          
      CON2=1.-RB(2,I,J)*RB(2,I,J)                                       OPR30130          
      RPS1=RTHETA(7,JM1,JM2)*RTHETA(7,JM1,JM2)                          OPR30131          
      RPS2=RTHETA(7,JF1,JF2)*RTHETA(7,JF1,JF2)                          OPR30132          
      SRR(1,I,J)=SQTF(DIVF(CON1,Q3*DFS)*(Q3*CON1/2.+QC1*(RECD1-DIVF(RTHEOPR30133          
     1TA(7,JM1,JM2)*RB(1,I,J),C1))+DIVF(4.0,CON1)*(RB(1,I,J)*RECD1-DIVF(OPR30134          
     2RTHETA(7,JM1,JM2),C1))**2+2.*DIVF(1.-RPS1,C1S)))                  OPR30135          
      SRR(2,I,J)=SQTF(DIVF(CON2,Q3*DFS)*(Q3*CON2/2.+QC1*(RECD2-DIVF(RTHEOPR30136          
     1TA(7,JF1,JF2)*RB(2,I,J),C2))+DIVF(4.0,CON2)*(RB(2,I,J)*RECD2-DIVF(OPR30137          
     2RTHETA(7,JF1,JF2),C2))**2+2.0*DIVF(1.-RPS2,C2S)))                 OPR30138          
      SS1=SCR2(1,I,J)*SCR2(1,I,J)                                       OPR30139          
      SS2=SCR2(2,I,J)*SCR2(2,I,J)                                       OPR30140          
      SRP1=SCR2(1,I,J)*RTHETA(7,JM1,JM2)                                OPR30141          
      SRP2=SCR2(2,I,J)*RTHETA(7,JF1,JF2)                                OPR30142          
      SESCR2(1,I,J)=SQTF(DIVF((SS1+DIVF(2.,Q3))*(1.+RPS1)+QC2*(C1S*RECD1OPR30143          
     1 +SRP1)+0.5*(C1S*(1.-4.*SRP1*RECD1)-3.*SS1),DFS))                 OPR30144          
      SESCR2(2,I,J)=SQTF(DIVF((SS2+DIVF(2.,Q3))*(1.+RPS2)+QC2*(C2S*RECD2OPR30145          
     1 +SRP2)+0.5*(C2S*(1.-4.*SRP2*RECD2)-3.*SS2),DFS))                 OPR30146          
   11 DO 12 K=1,2                                                       OPR30147          
      GO TO (14,14,13),N3                                               OPR30148          
   13 SRR(K,I,J)=(1.-RB(K,I,J)*RB(K,I,J))/1.414214*                     OPR30149          
     1   (SQTF(DIVF(SE4(K,I,I)*SE4(K,J,J),HB(K,I,I)*HB(K,J,J))))        OPR30150          
   14 HBS1=HB(K,I,I)*HB(K,I,I)                                          OPR30151          
      HBS2=HB(K,J,J)*HB(K,J,J)                                          OPR30152          
      A(K,I,J)=A1*(SQTF(DIVF(4.+HBS1,HBS1))+SQTF(DIVF(4.+HBS2,HBS2)))   OPR30153          
   12 CONTINUE                                                          OPR30154          
   15 CONTINUE                                                          OPR30155          
C                                                                       OPR30156          
C     --------------------------------------------                      OPR30157          
C     DAM REGRESSION AND CORRELATION.                                   OPR30158          
C     --------------------------------------------                      OPR30159          
      IF (ID .LE. 0) GO TO 35                                           OPR30160          
      A2=DIVF(1.,2.*SQTF(DF(I7)))                                       OPR30161          
      DO 24 I=1,ID                                                      OPR30162          
      DO 24 J=I,ID                                                      OPR30163          
      JD=I+IS                                                           OPR30164          
      JM=J+IS+ID                                                        OPR30165          
      JF=J+IS+ID+IM                                                     OPR30166          
C                                                                       OPR30167          
      IF(IM .EQ. IF) GO TO 221                                          OPR30168          
      IF(IM .GT. 0) GO TO 220                                           OPR30169          
      CORD(I,J)=1.0                                                     OPR30170          
      GO TO 222                                                         OPR30171          
  220 CORD(I,J)=SQTF(DIVF(SM(I7,JD,JD),SM(I7,JM,JM)))                   OPR30172          
      GO TO 222                                                         OPR30173          
  221 CORD(I,J)=SQTF(DIVF(SM(I7,JF,JF),SM(I7,JM,JM)))                   OPR30174          
  222 B(3,I,J)=DIVF(SS(I7,JD,JM),SS(I7,JD,JD))                          OPR30175          
      B(4,I,J)=DIVF(SS(I7,JD,JF),SS(I7,JD,JD))                          OPR30176          
      REGSS(3,I,J)=B(3,I,J)*SS(I7,JD,JM)                                OPR30177          
      REGSS(4,I,J)=B(4,I,J)*SS(I7,JD,JF)                                OPR30178          
      DEVSS(3,I,J)=SS(I7,JM,JM)-REGSS(3,I,J)                            OPR30179          
      DEVSS(4,I,J)=SS(I7,JF,JF)-REGSS(4,I,J)                            OPR30180          
      HB(3,I,J)=2.*B(3,I,J)*CORD(I,J)                                   OPR30181          
      HB(4,I,J)=2.*B(4,I,J)                                             OPR30182          
      HR(3,I,J)=2.*CORF(SS(I7,JD,JM),SS(I7,JD,JD),SS(I7,JM,JM)+SS(I8,JM,OPR30183          
     1 JM))                                                             OPR30184          
      HR(4,I,J)=2.*CORF(SS(I7,JD,JF),SS(I7,JD,JD),SS(I7,JF,JF)+SS(I8,JF,OPR30185          
     1 JF))                                                             OPR30186          
      DO 23 K=3,4                                                       OPR30187          
      DEVSM(K,I,J)=DIVF(DEVSS(K,I,J),DFD)                               OPR30188          
      FREG(K,I,J)=DIVF(REGSS(K,I,J),DEVSM(K,I,J))                       OPR30189          
   23 CONTINUE                                                          OPR30190          
      FDEV(3,I,J)=DIVF(DEVSM(3,I,J),SM(I8,JM,JM))                       OPR30191          
      FDEV(4,I,J)=DIVF(DEVSM(4,I,J),SM(I8,JF,JF))                       OPR30192          
      SE4(3,I,J)=2.*SQTF(DIVF(SS(I7,JM,JM)+SS(I8,JM,JM)-REGSS(3,I,J),   OPR30193          
     1    DFDDV*SS(I7,JD,JD)))*CORD(I,J)                                OPR30194          
      SE4(4,I,J)=2.*SQTF(DIVF(SS(I7,JF,JF)+SS(I8,JF,JF)-REGSS(4,I,J),   OPR30195          
     1    DFDDV*SS(I7,JD,JD)))                                          OPR30196          
   24 CONTINUE                                                          OPR30197          
C                                                                       OPR30198          
      IDX=ID-1                                                          OPR30199          
      IF(IDX) 35,35,980                                                 OPR30200          
  980 DO 32 I=1,IDX                                                     OPR30201          
      JMIN=I+1                                                          OPR30202          
      DO 32 J=JMIN,ID                                                   OPR30203          
      JD1=I+IS                                                          OPR30204          
      JD2=J+IS                                                          OPR30205          
      JM1=I+IS+ID                                                       OPR30206          
      JM2=J+IS+ID                                                       OPR30207          
      JF1=I+IS+ID+IM                                                    OPR30208          
      JF2=J+IS+ID+IM                                                    OPR30209          
      SDPARD=SQTF(SS(I7,JD1,JD1)*SS(I7,JD2,JD2))                        OPR30210          
      IF (IM .LE. 1) GO TO 27                                           OPR30211          
      IF(SS(I7,JD1,JM1).GT.0. .AND. SS(I7,JD2,JM2).GT.0.) GO TO 26      OPR30212          
      RB(3,I,J)=0.                                                      OPR30213          
      PRINT 5,JM1,JM2                                                   OPR30214          
      GO TO 262                                                         OPR30215          
   26 RB(3,I,J)=(SS(I7,JD1,JM2)+SS(I7,JD2,JM1))/(2.*(SQRT(SS(I7,JD1,JM1)OPR30216          
     1             *SS(I7,JD2,JM2))))                                   OPR30217          
  262 SCR2(3,I,J)=(SS(I7,JD1,JM2)+SS(I7,JD2,JM1))*DIVF(1.,SDPARD)       OPR30218          
   27 IF (IF .LE. 1) GO TO 29                                           OPR30219          
      IF(SS(I7,JD1,JF1).GT.0. .AND. SS(I7,JD2,JF2).GT.0.) GO TO 28      OPR30220          
      RB(4,I,J)=0.                                                      OPR30221          
      PRINT 5,JF1,JF2                                                   OPR30222          
      GO TO 282                                                         OPR30223          
   28 RB(4,I,J)=(SS(I7,JD1,JF2)+SS(I7,JD2,JF1))/(2.*(SQRT(SS(I7,JD1,JF1)OPR30224          
     1             *SS(I7,JD2,JF2))))                                   OPR30225          
  282 SCR2(4,I,J)=(SS(I7,JD1,JF2)+SS(I7,JD2,JF1))*DIVF(1.,SDPARD)       OPR30226          
   29 C3=SQTF(HB(3,I,I)*HB(3,J,J))                                      OPR30227          
      C4=SQTF(HB(4,I,I)*HB(4,J,J))                                      OPR30228          
      C3S=C3*C3                                                         OPR30229          
      C4S=C4*C4                                                         OPR30230          
      RECD3=0.5*(DIVF(1.,HB(3,I,I))+DIVF(1.,HB(3,J,J)))                 OPR30231          
      RECD4=0.5*(DIVF(1.,HB(4,I,I))+DIVF(1.,HB(4,J,J)))                 OPR30232          
      GO TO (30,30,31),N3                                               OPR30233          
   30 CON3=1.-RB(3,I,J)*RB(3,I,J)                                       OPR30234          
      CON4=1.-RB(4,I,J)*RB(4,I,J)                                       OPR30235          
      RPS3=RTHETA(7,JM1,JM2)*RTHETA(7,JM1,JM2)                          OPR30236          
      RPS4=RTHETA(7,JF1,JF2)*RTHETA(7,JF1,JF2)                          OPR30237          
      SRR(3,I,J)=SQTF(DIVF(CON3,Q3*DFD)*(Q3*CON3/2.+QC1*(RECD3-DIVF(RTHEOPR30238          
     1TA(7,JM1,JM2)*RB(3,I,J),C3))+DIVF(4.,CON3)*(RB(3,I,J)*RECD3-DIVF( OPR30239          
     2RTHETA(7,JM1,JM2),C3))**2+2.0*DIVF(1.-RPS3,C3S)))                 OPR30240          
      SRR(4,I,J)=SQTF(DIVF(CON4,Q3*DFD)*(Q3*CON4/2.+QC1*(RECD4-DIVF(RTHEOPR30241          
     1TA(7,JF1,JF2)*RB(4,I,J),C4))+DIVF(4.,CON4)*(RB(4,I,J)*RECD4-DIVF( OPR30242          
     2RTHETA(7,JF1,JF2),C4))**2+2.*DIVF(1.-RPS4,C4S)))                  OPR30243          
      SS3=SCR2(3,I,J)*SCR2(3,I,J)                                       OPR30244          
      SS4=SCR2(4,I,J)*SCR2(4,I,J)                                       OPR30245          
      SRP3=SCR2(3,I,J)*RTHETA(7,JM1,JM2)                                OPR30246          
      SRP4=SCR2(4,I,J)*RTHETA(7,JF1,JF2)                                OPR30247          
      SESCR2(3,I,J)=SQTF(DIVF((SS3+DIVF(2.,Q3))*(1.+RPS3)+QC2*(C3S*RECD3OPR30248          
     1 +SRP3)+0.5*(C3S*(1.-4.*SRP3*RECD3)-3.*SS3),DFD))                 OPR30249          
      SESCR2(4,I,J)=SQTF(DIVF((SS4+DIVF(2.,Q3))*(1.+RPS4)+QC2*(C4S*RECD4OPR30250          
     1 +SRP4)+0.5*(C4S*(1.-4.*SRP4*RECD4)-3.*SS4),DFD))                 OPR30251          
   31 DO 32 K=3,4                                                       OPR30252          
      GO TO (34,34,33),N3                                               OPR30253          
   33 SRR(K,I,J)=(1.-RB(K,I,J)*RB(K,I,J))/1.414214*                     OPR30254          
     1    (SQTF(DIVF(SE4(K,I,I)*SE4(K,J,J),HB(K,I,I)*HB(K,J,J))))       OPR30255          
   34 HBS3=HB(K,I,I)*HB(K,I,I)                                          OPR30256          
      HBS4=HB(K,J,J)*HB(K,J,J)                                          OPR30257          
      A(K,I,J)=A2*(SQTF(DIVF(4.+HBS3,HBS3))+SQTF(DIVF(4.+HBS4,HBS4)))   OPR30258          
   32 CONTINUE                                                          OPR30259          
   35 CONTINUE                                                          OPR30260          
      RETURN                                                            OPR30261          
      END                                                               OPR30262          
*ENDTEXT                                                                                  
