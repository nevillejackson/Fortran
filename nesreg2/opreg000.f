*TEXT                                                                                     
      SUBROUTINE OPREG(IIA,IIB,IIC,                                     OPR20001          
     1T2,COMENV,WFSENV,DF,IDF,SS,SM,F,XMN,VC1,VC2,VC3,HS,HD,HSD,SE1,SE2,OPR20002          
     2SE3,THETA,RTHETA,SER,THETGS,THETPD,HDFS,VAR1,NAMVAR,              OPR20003          
     3REGSS,DEVSS,SE4,HB,HR,RB,SRR,DEVSM,FREG,B,FDEV,CORS,CORD,CHS,VA,  OPR20004          
     4VD,VAA,SL1,SL2,SL3,SCR1,SESCR1,SCR2,SESCR2,A,FKH,LS,XMAT1,XMAT2,  OPR20005          
     5TS)                                                               OPR20006          
C-----VERSION 2                                                         OPR20007          
      COMMON/CMBLK1/I1,I2,N3,N4,N5,N6,IS,ID,IM,IF,N1,ITLE(9),Y,KPUT     OPR20008          
      COMMON/CMBLK5/NPROB,Q1,Q2,Q3,BIVAR1,BIVAR2,BIVAR3,I6,I7,I8,I9     OPR20009          
      COMMON/CMBLK6/ DFS,DFD,IDFS,IDFD                                  OPR20010          
      DIMENSION T2(3,7),COMENV(IIA,1),WFSENV(IIA,1),DF(1),IDF(1),       OPR20011          
     1 SS(IIC,IIA,1),SM(IIB,IIA,1),F(IIB,IIA,1),XMN(IIA,1),VC1(IIA,1),  OPR20012          
     2 VC2(IIA,1),VC3(IIA,1),HS(IIA,1),HD(IIA,1),HSD(IIA,1),SE1(IIA,1), OPR20013          
     3 SE2(IIA,1),SE3(IIA,1),THETA(7,IIA,1),RTHETA(7,IIA,1),SER(7,IIA,1)OPR20014          
     4 ,THETGS(IIA,1),THETPD(IIA,1),HDFS(IIA,1),VAR1(IIA,1),NAMVAR(1)   OPR20015          
      DIMENSION REGSS(4,IIA,1),DEVSS(4,IIA,1),SE4(4,IIA,1),HB(4,IIA,1), OPR20016          
     1HR(4,IIA,1),RB(4,IIA,1),SRR(4,IIA,1),DEVSM(4,IIA,1),FREG(4,IIA,1),OPR20017          
     2B(4,IIA,1),FDEV(4,IIA,1),CORS(IIA,1),CORD(IIA,1),CHS(IIA,1),      OPR20018          
     3VA(IIA,1),VD(IIA,1),VAA(IIA,1),SL1(IIA,1),SL2(IIA,1),SL3(IIA,1),  OPR20019          
     4SCR1(3,IIA,1),SESCR1(3,IIA,1),SCR2(4,IIA,1),SESCR2(4,IIA,1),      OPR20020          
     5A(4,IIA,1),FKH(3),LS(3),XMAT1(IIA,1),XMAT2(IIA,1),TS(3,7)         OPR20021          
      REAL LS                                                           OPR20022          
      DFS=DF(I6)-1.                                                     OPR20023          
      DFD=DF(I7)-1.                                                     OPR20024          
      DFSDV=DFS+DF(I7)+DF(I8)                                           OPR20025          
      DFDDV=DFD+DF(I8)                                                  OPR20026          
      IDFS=DFS                                                          OPR20027          
      IDFD=DFD                                                          OPR20028          
      QC1=(Q3+Q1)/2.0-1.0                                               OPR20029          
      QC2=DIVF(Q3+Q1-2.0,2.0*Q3)                                        OPR20030          
      ITEMP=IS+ID+IM+IF                                                 OPR20031          
      IF (ITEMP .EQ. I1) GO TO 2                                        OPR20032          
      PRINT 1,IS,ID,IM,IF                                               OPR20033          
    1 FORMAT(1H0,43HTHE SUM OF THE PARTS DO NOT EQUAL THE WHOLE /,4H IS=OPR20034          
     1I2,3HID=I2,3HIM=I2,3HIF=I2)                                       OPR20035          
    2 CONTINUE                                                          OPR20036          
C     SIRE         (1 TO IS,                   1 TO IS)                 OPR20037          
C     DAM          (IS+1 TO IS+ID,             IS+1 TO IS+ID)           OPR20038          
C     S-O  (MALES) (IS+ID+1 TO IS+ID+IM,       1 TO IS)                 OPR20039          
C     S-O  (MALES) (IS+ID+1 TO IS+ID+IM,       1 TO IS)                 OPR20040          
C          (FEMLS) (IS+ID+1 TO IS+ID+IM+IF,    1 TO IS)                 OPR20041          
C     D-O  (MALES) (IS+ID+1 TO IS+ID+IM,       IS+1 TO IS+ID)           OPR20042          
C          (FEMLS) (IS+ID+IM+1 TO IS+ID+IM+IF, IS+1 TO IS+ID)           OPR20043          
C                                                                       OPR20044          
C     --------------------------------------------                      OPR20045          
C     SIRE REGRESSION AND CORRELATION.                                  OPR20046          
C     --------------------------------------------                      OPR20047          
      IF (IS .LE. 0) GO TO 15                                           OPR20048          
      A1=DIVF(1.,2.*SQTF(DF(I6)))                                       OPR20049          
C     --------------------------------------------                      OPR20050          
C     AOV AND HERITABILITIES AND ITS CORRECTION.                        OPR20051          
C     --------------------------------------------                      OPR20052          
      DO 4 I=1,IS                                                       OPR20053          
      DO 4 J=I,IS                                                       OPR20054          
      JS=I                                                              OPR20055          
      JM=J+IS+ID                                                        OPR20056          
      JF=J+IS+ID+IM                                                     OPR20057          
C                                                                       OPR20058          
      IF(IM .EQ. IF) GO TO 21                                           OPR20059          
      IF(IF .GT. 0) GO TO 20                                            OPR20060          
      CORS(I,J)=1.0                                                     OPR20061          
      GO TO 22                                                          OPR20062          
   20 CORS(I,J)=SQTF(DIVF(SM(I6,JS,JS),SM(I6,JF,JF)))                   OPR20063          
      GO TO 22                                                          OPR20064          
   21 CORS(I,J)=SQTF(DIVF(SM(I6,JM,JM),SM(I6,JF,JF)))                   OPR20065          
   22 B(1,I,J)=DIVF(SS(I6,JS,JM),SS(I6,JS,JS))                          OPR20066          
      B(2,I,J)=DIVF(SS(I6,JS,JF),SS(I6,JS,JS))                          OPR20067          
      REGSS(1,I,J)=B(1,I,J)*SS(I6,JS,JM)                                OPR20068          
      REGSS(2,I,J)=B(2,I,J)*SS(I6,JS,JF)                                OPR20069          
      DEVSS(1,I,J)=SS(I6,JM,JM)-REGSS(1,I,J)                            OPR20070          
      DEVSS(2,I,J)=SS(I6,JF,JF)-REGSS(2,I,J)                            OPR20071          
      HB(1,I,J)=2.*B(1,I,J)                                             OPR20072          
      HB(2,I,J)=2.*B(2,I,J)*CORS(I,J)                                   OPR20073          
      HR(1,I,J)=2.*CORF(SS(I6,JS,JM),SS(I6,JS,JS),SS(I6,JM,JM)+SS(I7,JM,OPR20074          
     1JM)+SS(I8,JM,JM))                                                 OPR20075          
      HR(2,I,J)=2.*CORF(SS(I6,JS,JF),SS(I6,JS,JS),SS(I6,JF,JF)+SS(I7,JF,OPR20076          
     1JF)+SS(I8,JF,JF))                                                 OPR20077          
      DO 3 K=1,2                                                        OPR20078          
      DEVSM(K,I,J)=DIVF(DEVSS(K,I,J),DFS)                               OPR20079          
      FREG(K,I,J)=DIVF(REGSS(K,I,J),DEVSM(K,I,J))                       OPR20080          
    3 CONTINUE                                                          OPR20081          
      FDEV(1,I,J)=DIVF(DEVSM(1,I,J),SM(I7,JM,JM))                       OPR20082          
      FDEV(2,I,J)=DIVF(DEVSM(2,I,J),SM(I7,JF,JF))                       OPR20083          
      SE4(1,I,J)=2.*SQTF(DIVF(SS(I6,JM,JM)+SS(I7,JM,JM)+SS(I8,JM,JM)-REGOPR20084          
     1SS(1,I,J),DFSDV*SS(I6,JS,JS)))                                    OPR20085          
      SE4(2,I,J)=2.*SQTF(DIVF(SS(I6,JF,JF)+SS(I7,JF,JF)+SS(I8,JF,JF)-REGOPR20086          
     1SS(2,I,J),DFSDV*SS(I6,JS,JS)))*CORS(I,J)                          OPR20087          
    4 CONTINUE                                                          OPR20088          
C     --------------------------------------------                      OPR20089          
C     GENETIC CORRLN.,STD. CORRLD. RESPONSE. AND STD. ERRORS.           OPR20090          
C     --------------------------------------------                      OPR20091          
      ISX=IS-1                                                          OPR20092          
      IF(ISX) 15,15,981                                                 OPR20093          
  981 DO 12 I=1,ISX                                                     OPR20094          
      JMIN=I+1                                                          OPR20095          
      DO 12 J=JMIN,IS                                                   OPR20096          
      JS1=I                                                             OPR20097          
      JS2=J                                                             OPR20098          
      JM1=I+IS+ID                                                       OPR20099          
      JM2=J+IS+ID                                                       OPR20100          
      JF1=I+IS+ID+IM                                                    OPR20101          
      JF2=J+IS+ID+IM                                                    OPR20102          
      SDPARS=SQTF(SS(I6,JS1,JS1)*SS(I6,JS2,JS2))                        OPR20103          
      IF (IM .LE. 1) GO TO 7                                            OPR20104          
      IF(SS(I6,JS1,JM1).GT.0. .AND. SS(I6,JS2,JM2).GT.0.) GO TO 6       OPR20105          
      RB(1,I,J)=0.                                                      OPR20106          
      PRINT 5,JM1,JM2                                                   OPR20107          
    5 FORMAT(32H0 P-O CORRELATION FOR CHARACTERS ,I2,3HAND,I2,20HCANNOT OPR20108          
     1BE CALCULATED)                                                    OPR20109          
      GO TO  62                                                         OPR20110          
    6 RB(1,I,J)=(SS(I6,JS1,JM2)+SS(I6,JS2,JM1))/(2.*(SQRT(SS(I6,JS1,JM1)OPR20111          
     1             *SS(I6,JS2,JM2))))                                   OPR20112          
   62 SCR2(1,I,J)=DIVF(SS(I6,JS1,JM2)+SS(I6,JS2,JM1),SDPARS)            OPR20113          
    7 IF (IF .LE. 1) GO TO 9                                            OPR20114          
      IF(SS(I6,JS1,JF1).GT.0. .AND. SS(I6,JS2,JF2).GT.0.) GO TO 8       OPR20115          
      RB(2,I,J)=0.                                                      OPR20116          
      PRINT 5,JF1,JF2                                                   OPR20117          
      GO TO  82                                                         OPR20118          
    8 RB(2,I,J)=(SS(I6,JS1,JF2)+SS(I6,JS2,JF1))/(2.*(SQRT(SS(I6,JS1,JF1)OPR20119          
     1             *SS(I6,JS2,JF2))))                                   OPR20120          
   82 SCR2(2,I,J)=DIVF(SS(I6,JS1,JF2)+SS(I6,JS2,JF1),SDPARS)            OPR20121          
    9 C1=SQTF(HB(1,I,I)*HB(1,J,J))                                      OPR20122          
      C2=SQTF(HB(2,I,I)*HB(2,J,J))                                      OPR20123          
      C1S=C1*C1                                                         OPR20124          
      C2S=C2*C2                                                         OPR20125          
      RECD1=0.5*(DIVF(1.,HB(1,I,I))+DIVF(1.,HB(1,J,J)))                 OPR20126          
      RECD2=0.5*(DIVF(1.,HB(2,I,I))+DIVF(1.,HB(2,J,J)))                 OPR20127          
      GO TO (10,10,11),N3                                               OPR20128          
   10 CON1=1.-RB(1,I,J)*RB(1,I,J)                                       OPR20129          
      CON2=1.-RB(2,I,J)*RB(2,I,J)                                       OPR20130          
      RPS1=RTHETA(7,JM1,JM2)*RTHETA(7,JM1,JM2)                          OPR20131          
      RPS2=RTHETA(7,JF1,JF2)*RTHETA(7,JF1,JF2)                          OPR20132          
      SRR(1,I,J)=SQTF(DIVF(CON1,Q3*DFS)*(Q3*CON1/2.+QC1*(RECD1-DIVF(RTHEOPR20133          
     1TA(7,JM1,JM2)*RB(1,I,J),C1))+DIVF(4.0,CON1)*(RB(1,I,J)*RECD1-DIVF(OPR20134          
     2RTHETA(7,JM1,JM2),C1))**2+2.*DIVF(1.-RPS1,C1S)))                  OPR20135          
      SRR(2,I,J)=SQTF(DIVF(CON2,Q3*DFS)*(Q3*CON2/2.+QC1*(RECD2-DIVF(RTHEOPR20136          
     1TA(7,JF1,JF2)*RB(2,I,J),C2))+DIVF(4.0,CON2)*(RB(2,I,J)*RECD2-DIVF(OPR20137          
     2RTHETA(7,JF1,JF2),C2))**2+2.0*DIVF(1.-RPS2,C2S)))                 OPR20138          
      SS1=SCR2(1,I,J)*SCR2(1,I,J)                                       OPR20139          
      SS2=SCR2(2,I,J)*SCR2(2,I,J)                                       OPR20140          
      SRP1=SCR2(1,I,J)*RTHETA(7,JM1,JM2)                                OPR20141          
      SRP2=SCR2(2,I,J)*RTHETA(7,JF1,JF2)                                OPR20142          
      SESCR2(1,I,J)=SQTF(DIVF((SS1+DIVF(2.,Q3))*(1.+RPS1)+QC2*(C1S*RECD1OPR20143          
     1 +SRP1)+0.5*(C1S*(1.-4.*SRP1*RECD1)-3.*SS1),DFS))                 OPR20144          
      SESCR2(2,I,J)=SQTF(DIVF((SS2+DIVF(2.,Q3))*(1.+RPS2)+QC2*(C2S*RECD2OPR20145          
     1 +SRP2)+0.5*(C2S*(1.-4.*SRP2*RECD2)-3.*SS2),DFS))                 OPR20146          
   11 DO 12 K=1,2                                                       OPR20147          
      GO TO (14,14,13),N3                                               OPR20148          
   13 SRR(K,I,J)=(1.-RB(K,I,J)*RB(K,I,J))/1.414214*                     OPR20149          
     1   (SQTF(DIVF(SE4(K,I,I)*SE4(K,J,J),HB(K,I,I)*HB(K,J,J))))        OPR20150          
   14 HBS1=HB(K,I,I)*HB(K,I,I)                                          OPR20151          
      HBS2=HB(K,J,J)*HB(K,J,J)                                          OPR20152          
      A(K,I,J)=A1*(SQTF(DIVF(4.+HBS1,HBS1))+SQTF(DIVF(4.+HBS2,HBS2)))   OPR20153          
   12 CONTINUE                                                          OPR20154          
   15 CONTINUE                                                          OPR20155          
C                                                                       OPR20156          
C     --------------------------------------------                      OPR20157          
C     DAM REGRESSION AND CORRELATION.                                   OPR20158          
C     --------------------------------------------                      OPR20159          
      IF (ID .LE. 0) GO TO 35                                           OPR20160          
      A2=DIVF(1.,2.*SQTF(DF(I7)))                                       OPR20161          
      DO 24 I=1,ID                                                      OPR20162          
      DO 24 J=I,ID                                                      OPR20163          
      JD=I+IS                                                           OPR20164          
      JM=J+IS+ID                                                        OPR20165          
      JF=J+IS+ID+IM                                                     OPR20166          
C                                                                       OPR20167          
      IF(IM .EQ. IF) GO TO 221                                          OPR20168          
      IF(IM .GT. 0) GO TO 220                                           OPR20169          
      CORD(I,J)=1.0                                                     OPR20170          
      GO TO 222                                                         OPR20171          
  220 CORD(I,J)=SQTF(DIVF(SM(I7,JD,JD),SM(I7,JM,JM)))                   OPR20172          
      GO TO 222                                                         OPR20173          
  221 CORD(I,J)=SQTF(DIVF(SM(I7,JF,JF),SM(I7,JM,JM)))                   OPR20174          
  222 B(3,I,J)=DIVF(SS(I7,JD,JM),SS(I7,JD,JD))                          OPR20175          
      B(4,I,J)=DIVF(SS(I7,JD,JF),SS(I7,JD,JD))                          OPR20176          
      REGSS(3,I,J)=B(3,I,J)*SS(I7,JD,JM)                                OPR20177          
      REGSS(4,I,J)=B(4,I,J)*SS(I7,JD,JF)                                OPR20178          
      DEVSS(3,I,J)=SS(I7,JM,JM)-REGSS(3,I,J)                            OPR20179          
      DEVSS(4,I,J)=SS(I7,JF,JF)-REGSS(4,I,J)                            OPR20180          
      HB(3,I,J)=2.*B(3,I,J)*CORD(I,J)                                   OPR20181          
      HB(4,I,J)=2.*B(4,I,J)                                             OPR20182          
      HR(3,I,J)=2.*CORF(SS(I7,JD,JM),SS(I7,JD,JD),SS(I7,JM,JM)+SS(I8,JM,OPR20183          
     1 JM))                                                             OPR20184          
      HR(4,I,J)=2.*CORF(SS(I7,JD,JF),SS(I7,JD,JD),SS(I7,JF,JF)+SS(I8,JF,OPR20185          
     1 JF))                                                             OPR20186          
      DO 23 K=3,4                                                       OPR20187          
      DEVSM(K,I,J)=DIVF(DEVSS(K,I,J),DFD)                               OPR20188          
      FREG(K,I,J)=DIVF(REGSS(K,I,J),DEVSM(K,I,J))                       OPR20189          
   23 CONTINUE                                                          OPR20190          
      FDEV(3,I,J)=DIVF(DEVSM(3,I,J),SM(I8,JM,JM))                       OPR20191          
      FDEV(4,I,J)=DIVF(DEVSM(4,I,J),SM(I8,JF,JF))                       OPR20192          
      SE4(3,I,J)=2.*SQTF(DIVF(SS(I7,JM,JM)+SS(I8,JM,JM)-REGSS(3,I,J),   OPR20193          
     1    DFDDV*SS(I7,JD,JD)))*CORD(I,J)                                OPR20194          
      SE4(4,I,J)=2.*SQTF(DIVF(SS(I7,JF,JF)+SS(I8,JF,JF)-REGSS(4,I,J),   OPR20195          
     1    DFDDV*SS(I7,JD,JD)))                                          OPR20196          
   24 CONTINUE                                                          OPR20197          
C                                                                       OPR20198          
      IDX=ID-1                                                          OPR20199          
      IF(IDX) 35,35,980                                                 OPR20200          
  980 DO 32 I=1,IDX                                                     OPR20201          
      JMIN=I+1                                                          OPR20202          
      DO 32 J=JMIN,ID                                                   OPR20203          
      JD1=I+IS                                                          OPR20204          
      JD2=J+IS                                                          OPR20205          
      JM1=I+IS+ID                                                       OPR20206          
      JM2=J+IS+ID                                                       OPR20207          
      JF1=I+IS+ID+IM                                                    OPR20208          
      JF2=J+IS+ID+IM                                                    OPR20209          
      SDPARD=SQTF(SS(I7,JD1,JD1)*SS(I7,JD2,JD2))                        OPR20210          
      IF (IM .LE. 1) GO TO 27                                           OPR20211          
      IF(SS(I7,JD1,JM1).GT.0. .AND. SS(I7,JD2,JM2).GT.0.) GO TO 26      OPR20212          
      RB(3,I,J)=0.                                                      OPR20213          
      PRINT 5,JM1,JM2                                                   OPR20214          
      GO TO 262                                                         OPR20215          
   26 RB(3,I,J)=(SS(I7,JD1,JM2)+SS(I7,JD2,JM1))/(2.*(SQRT(SS(I7,JD1,JM1)OPR20216          
     1             *SS(I7,JD2,JM2))))                                   OPR20217          
  262 SCR2(3,I,J)=(SS(I7,JD1,JM2)+SS(I7,JD2,JM1))*DIVF(1.,SDPARD)       OPR20218          
   27 IF (IF .LE. 1) GO TO 29                                           OPR20219          
      IF(SS(I7,JD1,JF1).GT.0. .AND. SS(I7,JD2,JF2).GT.0.) GO TO 28      OPR20220          
      RB(4,I,J)=0.                                                      OPR20221          
      PRINT 5,JF1,JF2                                                   OPR20222          
      GO TO 282                                                         OPR20223          
   28 RB(4,I,J)=(SS(I7,JD1,JF2)+SS(I7,JD2,JF1))/(2.*(SQRT(SS(I7,JD1,JF1)OPR20224          
     1             *SS(I7,JD2,JF2))))                                   OPR20225          
  282 SCR2(4,I,J)=(SS(I7,JD1,JF2)+SS(I7,JD2,JF1))*DIVF(1.,SDPARD)       OPR20226          
   29 C3=SQTF(HB(3,I,I)*HB(3,J,J))                                      OPR20227          
      C4=SQTF(HB(4,I,I)*HB(4,J,J))                                      OPR20228          
      C3S=C3*C3                                                         OPR20229          
      C4S=C4*C4                                                         OPR20230          
      RECD3=0.5*(DIVF(1.,HB(3,I,I))+DIVF(1.,HB(3,J,J)))                 OPR20231          
      RECD4=0.5*(DIVF(1.,HB(4,I,I))+DIVF(1.,HB(4,J,J)))                 OPR20232          
      GO TO (30,30,31),N3                                               OPR20233          
   30 CON3=1.-RB(3,I,J)*RB(3,I,J)                                       OPR20234          
      CON4=1.-RB(4,I,J)*RB(4,I,J)                                       OPR20235          
      RPS3=RTHETA(7,JM1,JM2)*RTHETA(7,JM1,JM2)                          OPR20236          
      RPS4=RTHETA(7,JF1,JF2)*RTHETA(7,JF1,JF2)                          OPR20237          
      SRR(3,I,J)=SQTF(DIVF(CON3,Q3*DFD)*(Q3*CON3/2.+QC1*(RECD3-DIVF(RTHEOPR20238          
     1TA(7,JM1,JM2)*RB(3,I,J),C3))+DIVF(4.,CON3)*(RB(3,I,J)*RECD3-DIVF( OPR20239          
     2RTHETA(7,JM1,JM2),C3))**2+2.0*DIVF(1.-RPS3,C3S)))                 OPR20240          
      SRR(4,I,J)=SQTF(DIVF(CON4,Q3*DFD)*(Q3*CON4/2.+QC1*(RECD4-DIVF(RTHEOPR20241          
     1TA(7,JF1,JF2)*RB(4,I,J),C4))+DIVF(4.,CON4)*(RB(4,I,J)*RECD4-DIVF( OPR20242          
     2RTHETA(7,JF1,JF2),C4))**2+2.*DIVF(1.-RPS4,C4S)))                  OPR20243          
      SS3=SCR2(3,I,J)*SCR2(3,I,J)                                       OPR20244          
      SS4=SCR2(4,I,J)*SCR2(4,I,J)                                       OPR20245          
      SRP3=SCR2(3,I,J)*RTHETA(7,JM1,JM2)                                OPR20246          
      SRP4=SCR2(4,I,J)*RTHETA(7,JF1,JF2)                                OPR20247          
      SESCR2(3,I,J)=SQTF(DIVF((SS3+DIVF(2.,Q3))*(1.+RPS3)+QC2*(C3S*RECD3OPR20248          
     1 +SRP3)+0.5*(C3S*(1.-4.*SRP3*RECD3)-3.*SS3),DFD))                 OPR20249          
      SESCR2(4,I,J)=SQTF(DIVF((SS4+DIVF(2.,Q3))*(1.+RPS4)+QC2*(C4S*RECD4OPR20250          
     1 +SRP4)+0.5*(C4S*(1.-4.*SRP4*RECD4)-3.*SS4),DFD))                 OPR20251          
   31 DO 32 K=3,4                                                       OPR20252          
      GO TO (34,34,33),N3                                               OPR20253          
   33 SRR(K,I,J)=(1.-RB(K,I,J)*RB(K,I,J))/1.414214*                     OPR20254          
     1    (SQTF(DIVF(SE4(K,I,I)*SE4(K,J,J),HB(K,I,I)*HB(K,J,J))))       OPR20255          
   34 HBS3=HB(K,I,I)*HB(K,I,I)                                          OPR20256          
      HBS4=HB(K,J,J)*HB(K,J,J)                                          OPR20257          
      A(K,I,J)=A2*(SQTF(DIVF(4.+HBS3,HBS3))+SQTF(DIVF(4.+HBS4,HBS4)))   OPR20258          
   32 CONTINUE                                                          OPR20259          
   35 CONTINUE                                                          OPR20260          
      RETURN                                                            OPR20261          
      END                                                               OPR20262          
*ENDTEXT                                                                                  
