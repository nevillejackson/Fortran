*TEXT                                                                                     
      SUBROUTINE MIXTUR(IIA,IIB,IIC,                                    MXT30001          
     1T2,COMENV,WFSENV,DF,IDF,SS,SM,F,XMN,VC1,VC2,VC3,HS,HD,HSD,SE1,SE2,MXT30002          
     2SE3,THETA,RTHETA,SER,THETGS,THETPD,HDFS,VAR1,NAMVAR,              MXT30003          
     3REGSS,DEVSS,SE4,HB,HR,RB,SRR,DEVSM,FREG,B,FDEV,CORS,CORD,CHS,VA,  MXT30004          
     4VD,VAA,SL1,SL2,SL3,SCR1,SESCR1,SCR2,SESCR2,A,FKH,LS,XMAT1,XMAT2,  MXT30005          
     5TS)                                                               MXT30006          
C-----VERSION 3                                                         MXT30007          
      COMMON/CMBLK1/I1,I2,N3,N4,N5,N6,IS,ID,IM,IF,N1,ITLE(9),Y,KPUT     MXT30008          
      COMMON/CMBLK2/IT2,ICOMEN,IWFSEN,IDFQ,IIDFQ,ISS,ISM,IFQ,IXMN,IVC1, MXT30009          
     1  IVC2,IVC3,IHS,IHD,IHSD,ISE1,ISE2,ISE3,ITHETA,IRTHET,ISER,ITHETG,MXT30010          
     3  ITHETP,IHDFS,IVAR1,INAMVA,LEN26                                 MXT30011          
      COMMON/CMBLK4/IREGSS,IDEVSS,ISE4,IHB,IHR,IRB,ISRR,IDEVSM,IFREG,   MXT30012          
     1  IB,IFDEV,ICORS,ICORD,ICHS,IVA,IVD,IVAA,ISL1,ISL2,ISL3,ISCR1,    MXT30013          
     2  ISESC1,ISCR2,ISESC2,IA,IFKH,ILS,IXMAT1,IXMAT2,ITS,LEN56         MXT30014          
      COMMON/CMBLK5/NPROB,Q1,Q2,Q3,BIVAR1,BIVAR2,BIVAR3,I6,I7,I8,I9     MXT30015          
      DIMENSION T2(3,7),COMENV(IIA,1),WFSENV(IIA,1),DF(1),IDF(1),       MXT30016          
     1 SS(IIC,IIA,1),SM(IIB,IIA,1),F(IIB,IIA,1),XMN(IIA,1),VC1(IIA,1),  MXT30017          
     2 VC2(IIA,1),VC3(IIA,1),HS(IIA,1),HD(IIA,1),HSD(IIA,1),SE1(IIA,1), MXT30018          
     3 SE2(IIA,1),SE3(IIA,1),THETA(7,IIA,1),RTHETA(7,IIA,1),SER(7,IIA,1)MXT30019          
     4 ,THETGS(IIA,1),THETPD(IIA,1),HDFS(IIA,1),VAR1(IIA,1),NAMVAR(1)   MXT30020          
      DIMENSION REGSS(4,IIA,1),DEVSS(4,IIA,1),SE4(4,IIA,1),HB(4,IIA,1), MXT30021          
     1HR(4,IIA,1),RB(4,IIA,1),SRR(4,IIA,1),DEVSM(4,IIA,1),FREG(4,IIA,1),MXT30022          
     2B(4,IIA,1),FDEV(4,IIA,1),CORS(IIA,1),CORD(IIA,1),CHS(IIA,1),      MXT30023          
     3VA(IIA,1),VD(IIA,1),VAA(IIA,1),SL1(IIA,1),SL2(IIA,1),SL3(IIA,1),  MXT30024          
     4SCR1(3,IIA,1),SESCR1(3,IIA,1),SCR2(4,IIA,1),SESCR2(4,IIA,1),      MXT30025          
     5A(4,IIA,1),FKH(3),LS(3),XMAT1(IIA,1),XMAT2(IIA,1),TS(3,7)         MXT30026          
      REAL LS                                                           MXT30027          
      PRINT 92                                                          MXT30028          
   92 FORMAT(34H- ERRORS AND/OR COMMENTS FOR REGOP)                     MXT30029          
      DO 1 I=1,I1                                                       MXT30030          
      DO 1 J=1,I1                                                       MXT30031          
      IF (N3 .EQ. 3) VC1(I,J)=0.0                                       MXT30032          
      CHS(I,J) =99.999999                                               MXT30033          
      VA(I,J) =99.999999                                                MXT30034          
      VD(I,J) =99.999999                                                MXT30035          
      VAA(I,J)=99.999999                                                MXT30036          
      XMAT1(I,J)=-99.999999                                             MXT30037          
      XMAT2(I,J)=-99.999999                                             MXT30038          
      SL1(I,J)=99.999999                                                MXT30039          
      SL2(I,J)=99.999999                                                MXT30040          
    1 SL3(I,J)=99.999999                                                MXT30041          
      GO TO (21,21,2),N3                                                MXT30042          
C     --------------------------------------------                      MXT30043          
C     STANDARDIZED CORRLD. RESPONSE AND STD. ERROR.                     MXT30044          
C     --------------------------------------------                      MXT30045          
   21 IOF=IS+ID+1                                                       MXT30046          
      I1X=I1-1                                                          MXT30047          
      DF(I6)=DF(I6)+2.0                                                 MXT30048          
      DF(I7)=DF(I7)+2.0                                                 MXT30049          
      DF(I8)=DF(I8)+2.0                                                 MXT30050          
      FKH(1)=32.0                                                       MXT30051          
      FKH(2)=32.0                                                       MXT30052          
      FKH(3)=8.0                                                        MXT30053          
      LS(1)=Q3*Q3                                                       MXT30054          
      LS(2)=Q1*Q1                                                       MXT30055          
      LS(3)=LS(1)*LS(2)                                                 MXT30056          
      T2(1,1) = 1.0                                                     MXT30057          
      T2(2,1)=DIVF(-Q2,Q1)                                              MXT30058          
      T2(3,1)=DIVF(Q2-Q1,Q1)                                            MXT30059          
      T2(1,2) = 0.0                                                     MXT30060          
      T2(2,2) = 1.0                                                     MXT30061          
      T2(3,2) = -1.0                                                    MXT30062          
      T2(1,3) = Q1                                                      MXT30063          
      T2(2,3) = Q3-Q2                                                   MXT30064          
      T2(3,3) = Q2-Q1-Q3                                                MXT30065          
      T2(1,7) = Q1                                                      MXT30066          
      T2(2,7) = T2(2,3)                                                 MXT30067          
      T2(3,7) = Q2-Q1+Q3*(Q1-1.)                                        MXT30068          
      DO 23 K1=1,7                                                      MXT30069          
      DO 23 K2=1,3                                                      MXT30070          
   23 TS(K2,K1)=T2(K2,K1)*T2(K2,K1)                                     MXT30071          
      IF(I1X-IOF) 980,981,981                                           MXT30072          
  981 DO 22 IK=IOF,I1X                                                  MXT30073          
      K=IK+1                                                            MXT30074          
      DO 22 JK=K,I1                                                     MXT30075          
      VAR1(IK,JK)=2.*DIVF(VAR1(IK,JK),LS(3))                            MXT30076          
      DO 22 K1=1,3                                                      MXT30077          
      SCR1(K1,IK,JK)=CORF(THETA(K1,IK,JK),THETA(7,IK,IK),THETA(7,JK,JK))MXT30078          
      VAR =  VAR1(IK,JK)+((TS(1,K1)*(SM(I6,IK,IK)*SM(I6,JK,JK)+         MXT30079          
     1                     SM(I6,IK,JK)*SM(I6,IK,JK))/DF(I6)            MXT30080          
     2         + TS(2,K1)*(SM(I7,IK,IK)*SM(I7,JK,JK)+                   MXT30081          
     3                     SM(I7,IK,JK)*SM(I7,IK,JK))/DF(I7)            MXT30082          
     4         + TS(3,K1)*(SM(I8,IK,IK)*SM(I8,JK,JK)+                   MXT30083          
     5                     SM(I8,IK,JK)*SM(I8,IK,JK))/DF(I8))           MXT30084          
     6*DIVF(1.,  2.*THETA(K1,IK,JK)*THETA(K1,IK,JK))*FKH(K1)*DIVF(1.,   MXT30085          
     7 LS(K1)))                                                         MXT30086          
C-----                                                                  MXT30087          
C-----BEWARE OF COMPILER BUG IN NEXT TWO STATEMENTS                     MXT30088          
C-----                                                                  MXT30089          
      VAR3 = (T2(1,K1)*T2(1,7)*SM(I6,IK,JK)*SM(I6,IK,IK)/DF(I6)         MXT30090          
     1         + T2(2,K1)*T2(2,7)*SM(I7,IK,JK)*SM(I7,IK,IK)/DF(I7)      MXT30091          
     2         + T2(3,K1)*T2(3,7)*SM(I8,IK,JK)*SM(I8,IK,IK)/DF(I8))     MXT30092          
      VAR3=DIVF(VAR3,THETA(7,IK,IK)*THETA(K1,IK,JK))                    MXT30093          
      VAR3 = VAR3+(T2(1,K1)*T2(1,7)*SM(I6,JK,JK)*SM(I6,IK,JK)/DF(I6)    MXT30094          
     1         + T2(2,K1)*T2(2,7)*SM(I7,JK,JK)*SM(I7,IK,JK)/DF(I7)      MXT30095          
     2         + T2(3,K1)*T2(3,7)*SM(I8,JK,JK)*SM(I8,IK,JK)/DF(I8))     MXT30096          
     3  *DIVF(1.,THETA(7,JK,JK)*THETA(K1,IK,JK))                        MXT30097          
      VAR3=2.*VAR3*SQTF(DIVF(FKH(K1)*0.5,LS(K1)*LS(3)))                 MXT30098          
      VAR=VAR-VAR3                                                      MXT30099          
   22 SESCR1(K1,IK,JK)=SQTF(SCR1(K1,IK,JK)*SCR1(K1,IK,JK)*VAR)          MXT30100          
  980 DF(I6)=DF(I6)-2.0                                                 MXT30101          
      DF(I7)=DF(I7)-2.0                                                 MXT30102          
      DF(I8)=DF(I8)-2.0                                                 MXT30103          
      GO TO (2,3),N3                                                    MXT30104          
C     --------------------------------------------                      MXT30105          
C     DO O-P REGRESSION ANALYSIS.                                       MXT30106          
C     --------------------------------------------                      MXT30107          
    2 CALL OPREG(IIA,IIB,IIC,                                           MXT30108          
     1T2,COMENV,WFSENV,DF,IDF,SS,SM,F,XMN,VC1,VC2,VC3,HS,HD,HSD,SE1,SE2,MXT30109          
     2SE3,THETA,RTHETA,SER,THETGS,THETPD,HDFS,VAR1,NAMVAR,              MXT30110          
     3REGSS,DEVSS,SE4,HB,HR,RB,SRR,DEVSM,FREG,B,FDEV,CORS,CORD,CHS,VA,  MXT30111          
     4VD,VAA,SL1,SL2,SL3,SCR1,SESCR1,SCR2,SESCR2,A,FKH,LS,XMAT1,XMAT2,  MXT30112          
     5 TS)                                                              MXT30113          
C     --------------------------------------------                      MXT30114          
C     PARTITION PHENOTYPIC VARIANCE.                                    MXT30115          
C     --------------------------------------------                      MXT30116          
    3 IF(DF(I8)) 99,99,33                                               MXT30117          
   33 IF(IM .EQ. 0) GO TO 99                                            MXT30118          
      DO 9 I=1,IM                                                       MXT30119          
      DO 9 J=I,IM                                                       MXT30120          
      JM=J+IS+ID                                                        MXT30121          
      JF=J+IS+ID+IM                                                     MXT30122          
      IF(IF .EQ. 0) GO TO 6                                             MXT30123          
      GO TO (5,5,9),N3                                                  MXT30124          
    5 IF (IS .NE. ID) GO TO 9                                           MXT30125          
      SL1(I,J)=(HS(JF,JF)-HS(JM,JM))/2.0                                MXT30126          
      IF (SL1(I,J) .LT. 0.0) SL1(I,J)=0.0                               MXT30127          
      SL2(I,J)=HD(JM,JM)-HD(JF,JF)                                      MXT30128          
    6 GO TO (7,12,9),N3                                                 MXT30129          
    7 VAA(I,J)=(HB(1,I,J)-HS(JM,JM))*4.0                                MXT30130          
      IF (VAA(I,J) .GT. 0.0) GO TO 10                                   MXT30131          
      VAA(I,J)=0.0                                                      MXT30132          
      VA(I,J)=HS(JM,JM)                                                 MXT30133          
      GO TO 11                                                          MXT30134          
   10 VA(I,J)=HS(JM,JM)-VAA(I,J)/4.0                                    MXT30135          
   11 CHS(I,J)=1.-DIVF(HS(JM,JM),HB(1,I,J))                             MXT30136          
      IF(IF .EQ. 0) GO TO 8                                             MXT30137          
      VD(I,J)=HD(JM,JM)-(VA(I,J)+0.75*VAA(I,J)+2.*SL1(I,J))             MXT30138          
    8 IF(IS .NE. ID) GO TO 9                                            MXT30139          
      SL3(I,J)=(HB(3,I,J)-HB(1,I,J))                                    MXT30140          
      IF(IF .EQ. 0) GO TO 17                                            MXT30141          
      XMAT1(I,J)=HB(3,I,J)-HB(2,I,J)                                    MXT30142          
      XMAT2(I,J)=HB(4,I,J)-HB(2,I,J)                                    MXT30143          
      GO TO 13                                                          MXT30144          
   12 VA(I,J)=HS(JM,JM)                                                 MXT30145          
      IF(IF .EQ. 0) GO TO 9                                             MXT30146          
      VD(I,J)=HD(JM,JM)-(VA(I,J)+2.0*SL1(I,J))                          MXT30147          
      GO TO (13,9),N3                                                   MXT30148          
   13 IF(XMAT1(I,J) .LT. 0.0) XMAT1(I,J)=0.0                            MXT30149          
      IF(XMAT2(I,J) .LT. 0.0) XMAT2(I,J)=0.0                            MXT30150          
      VD(I,J)=VD(I,J)-4.0*XMAT1(I,J)                                    MXT30151          
      SL3(I,J)=(SL3(I,J)-XMAT1(I,J))*0.707                              MXT30152          
      GO TO 9                                                           MXT30153          
   17 SL3(I,J)=SL3(I,J)*0.707                                           MXT30154          
    9 CONTINUE                                                          MXT30155          
   99 CONTINUE                                                          MXT30156          
      CALL PROUT(IIA,IIB,IIC,                                           MXT30157          
     1T2,COMENV,WFSENV,DF,IDF,SS,SM,F,XMN,VC1,VC2,VC3,HS,HD,HSD,SE1,SE2,MXT30158          
     2SE3,THETA,RTHETA,SER,THETGS,THETPD,HDFS,VAR1,NAMVAR,              MXT30159          
     3REGSS,DEVSS,SE4,HB,HR,RB,SRR,DEVSM,FREG,B,FDEV,CORS,CORD,CHS,VA,  MXT30160          
     4VD,VAA,SL1,SL2,SL3,SCR1,SESCR1,SCR2,SESCR2,A,FKH,LS,XMAT1,XMAT2,  MXT30161          
     5 TS)                                                              MXT30162          
   16 CONTINUE                                                          MXT30163          
      RETURN                                                            MXT30164          
      END                                                               MXT30165          
*ENDTEXT                                                                                  
