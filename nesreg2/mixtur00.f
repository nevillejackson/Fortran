*TEXT                                                                                     
      SUBROUTINE MIXTUR(IIA,IIB,IIC,                                    MXT20001          
     1T2,COMENV,WFSENV,DF,IDF,SS,SM,F,XMN,VC1,VC2,VC3,HS,HD,HSD,SE1,SE2,MXT20002          
     2SE3,THETA,RTHETA,SER,THETGS,THETPD,HDFS,VAR1,NAMVAR,              MXT20003          
     3REGSS,DEVSS,SE4,HB,HR,RB,SRR,DEVSM,FREG,B,FDEV,CORS,CORD,CHS,VA,  MXT20004          
     4VD,VAA,SL1,SL2,SL3,SCR1,SESCR1,SCR2,SESCR2,A,FKH,LS,XMAT1,XMAT2,  MXT20005          
     5TS)                                                               MXT20006          
C-----VERSION 2                                                         MXT20007          
      COMMON/CMBLK1/I1,I2,N3,N4,N5,N6,IS,ID,IM,IF,N1,ITLE(9),Y,KPUT     MXT20008          
      COMMON/CMBLK2/IT2,ICOMEN,IWFSEN,IDFQ,IIDFQ,ISS,ISM,IFQ,IXMN,IVC1, MXT20009          
     1  IVC2,IVC3,IHS,IHD,IHSD,ISE1,ISE2,ISE3,ITHETA,IRTHET,ISER,ITHETG,MXT20010          
     3  ITHETP,IHDFS,IVAR1,INAMVA,LEN26                                 MXT20011          
      COMMON/CMBLK4/IREGSS,IDEVSS,ISE4,IHB,IHR,IRB,ISRR,IDEVSM,IFREG,   MXT20012          
     1  IB,IFDEV,ICORS,ICORD,ICHS,IVA,IVD,IVAA,ISL1,ISL2,ISL3,ISCR1,    MXT20013          
     2  ISESC1,ISCR2,ISESC2,IA,IFKH,ILS,IXMAT1,IXMAT2,ITS,LEN56         MXT20014          
      COMMON/CMBLK5/NPROB,Q1,Q2,Q3,BIVAR1,BIVAR2,BIVAR3,I6,I7,I8,I9     MXT20015          
      DIMENSION T2(3,7),COMENV(IIA,1),WFSENV(IIA,1),DF(1),IDF(1),       MXT20016          
     1 SS(IIC,IIA,1),SM(IIB,IIA,1),F(IIB,IIA,1),XMN(IIA,1),VC1(IIA,1),  MXT20017          
     2 VC2(IIA,1),VC3(IIA,1),HS(IIA,1),HD(IIA,1),HSD(IIA,1),SE1(IIA,1), MXT20018          
     3 SE2(IIA,1),SE3(IIA,1),THETA(7,IIA,1),RTHETA(7,IIA,1),SER(7,IIA,1)MXT20019          
     4 ,THETGS(IIA,1),THETPD(IIA,1),HDFS(IIA,1),VAR1(IIA,1),NAMVAR(1)   MXT20020          
      DIMENSION REGSS(4,IIA,1),DEVSS(4,IIA,1),SE4(4,IIA,1),HB(4,IIA,1), MXT20021          
     1HR(4,IIA,1),RB(4,IIA,1),SRR(4,IIA,1),DEVSM(4,IIA,1),FREG(4,IIA,1),MXT20022          
     2B(4,IIA,1),FDEV(4,IIA,1),CORS(IIA,1),CORD(IIA,1),CHS(IIA,1),      MXT20023          
     3VA(IIA,1),VD(IIA,1),VAA(IIA,1),SL1(IIA,1),SL2(IIA,1),SL3(IIA,1),  MXT20024          
     4SCR1(3,IIA,1),SESCR1(3,IIA,1),SCR2(4,IIA,1),SESCR2(4,IIA,1),      MXT20025          
     5A(4,IIA,1),FKH(3),LS(3),XMAT1(IIA,1),XMAT2(IIA,1),TS(3,7)         MXT20026          
      REAL LS                                                           MXT20027          
      PRINT 92                                                          MXT20028          
   92 FORMAT(34H- ERRORS AND/OR COMMENTS FOR REGOP)                     MXT20029          
      DO 1 I=1,I1                                                       MXT20030          
      DO 1 J=1,I1                                                       MXT20031          
      IF (N3 .EQ. 3) VC1(I,J)=0.0                                       MXT20032          
      CHS(I,J) =99.999999                                               MXT20033          
      VA(I,J) =99.999999                                                MXT20034          
      VD(I,J) =99.999999                                                MXT20035          
      VAA(I,J)=99.999999                                                MXT20036          
      XMAT1(I,J)=-99.999999                                             MXT20037          
      XMAT2(I,J)=-99.999999                                             MXT20038          
      SL1(I,J)=99.999999                                                MXT20039          
      SL2(I,J)=99.999999                                                MXT20040          
    1 SL3(I,J)=99.999999                                                MXT20041          
      GO TO (21,21,2),N3                                                MXT20042          
C     --------------------------------------------                      MXT20043          
C     STANDARDIZED CORRLD. RESPONSE AND STD. ERROR.                     MXT20044          
C     --------------------------------------------                      MXT20045          
   21 IOF=IS+ID+1                                                       MXT20046          
      I1X=I1-1                                                          MXT20047          
      DF(I6)=DF(I6)+2.0                                                 MXT20048          
      DF(I7)=DF(I7)+2.0                                                 MXT20049          
      DF(I8)=DF(I8)+2.0                                                 MXT20050          
      FKH(1)=32.0                                                       MXT20051          
      FKH(2)=32.0                                                       MXT20052          
      FKH(3)=8.0                                                        MXT20053          
      LS(1)=Q3*Q3                                                       MXT20054          
      LS(2)=Q1*Q1                                                       MXT20055          
      LS(3)=LS(1)*LS(2)                                                 MXT20056          
      T2(1,1) = 1.0                                                     MXT20057          
      T2(2,1)=DIVF(-Q2,Q1)                                              MXT20058          
      T2(3,1)=DIVF(Q2-Q1,Q1)                                            MXT20059          
      T2(1,2) = 0.0                                                     MXT20060          
      T2(2,2) = 1.0                                                     MXT20061          
      T2(3,2) = -1.0                                                    MXT20062          
      T2(1,3) = Q1                                                      MXT20063          
      T2(2,3) = Q3-Q2                                                   MXT20064          
      T2(3,3) = Q2-Q1-Q3                                                MXT20065          
      T2(1,7) = Q1                                                      MXT20066          
      T2(2,7) = T2(2,3)                                                 MXT20067          
      T2(3,7) = Q2-Q1+Q3*(Q1-1.)                                        MXT20068          
      DO 23 K1=1,7                                                      MXT20069          
      DO 23 K2=1,3                                                      MXT20070          
   23 TS(K2,K1)=T2(K2,K1)*T2(K2,K1)                                     MXT20071          
      IF(I1X-IOF) 980,981,981                                           MXT20072          
  981 DO 22 IK=IOF,I1X                                                  MXT20073          
      K=IK+1                                                            MXT20074          
      DO 22 JK=K,I1                                                     MXT20075          
      VAR1(IK,JK)=2.*DIVF(VAR1(IK,JK),LS(3))                            MXT20076          
      DO 22 K1=1,3                                                      MXT20077          
      SCR1(K1,IK,JK)=CORF(THETA(K1,IK,JK),THETA(7,IK,IK),THETA(7,JK,JK))MXT20078          
      VAR =  VAR1(IK,JK)+((TS(1,K1)*(SM(I6,IK,IK)*SM(I6,JK,JK)+         MXT20079          
     1                     SM(I6,IK,JK)*SM(I6,IK,JK))/DF(I6)            MXT20080          
     2         + TS(2,K1)*(SM(I7,IK,IK)*SM(I7,JK,JK)+                   MXT20081          
     3                     SM(I7,IK,JK)*SM(I7,IK,JK))/DF(I7)            MXT20082          
     4         + TS(3,K1)*(SM(I8,IK,IK)*SM(I8,JK,JK)+                   MXT20083          
     5                     SM(I8,IK,JK)*SM(I8,IK,JK))/DF(I8))           MXT20084          
     6*DIVF(1.,  2.*THETA(K1,IK,JK)*THETA(K1,IK,JK))*FKH(K1)*DIVF(1.,   MXT20085          
     7 LS(K1)))                                                         MXT20086          
C-----                                                                  MXT20087          
C-----BEWARE OF COMPILER BUG IN NEXT TWO STATEMENTS                     MXT20088          
C-----                                                                  MXT20089          
      VAR3 = (T2(1,K1)*T2(1,7)*SM(I6,IK,JK)*SM(I6,IK,IK)/DF(I6)         MXT20090          
     1         + T2(2,K1)*T2(2,7)*SM(I7,IK,JK)*SM(I7,IK,IK)/DF(I7)      MXT20091          
     2         + T2(3,K1)*T2(3,7)*SM(I8,IK,JK)*SM(I8,IK,IK)/DF(I8))     MXT20092          
      VAR3=DIVF(VAR3,THETA(7,IK,IK)*THETA(K1,IK,JK))                    MXT20093          
      VAR3 = VAR3+(T2(1,K1)*T2(1,7)*SM(I6,JK,JK)*SM(I6,IK,JK)/DF(I6)    MXT20094          
     1         + T2(2,K1)*T2(2,7)*SM(I7,JK,JK)*SM(I7,IK,JK)/DF(I7)      MXT20095          
     2         + T2(3,K1)*T2(3,7)*SM(I8,JK,JK)*SM(I8,IK,JK)/DF(I8))     MXT20096          
     3  *DIVF(1.,THETA(7,JK,JK)*THETA(K1,IK,JK))                        MXT20097          
      VAR3=2.*VAR3*SQTF(DIVF(FKH(K1)*0.5,LS(K1)*LS(3)))                 MXT20098          
      VAR=VAR-VAR3                                                      MXT20099          
   22 SESCR1(K1,IK,JK)=SQTF(SCR1(K1,IK,JK)*SCR1(K1,IK,JK)*VAR)          MXT20100          
  980 DF(I6)=DF(I6)-2.0                                                 MXT20101          
      DF(I7)=DF(I7)-2.0                                                 MXT20102          
      DF(I8)=DF(I8)-2.0                                                 MXT20103          
      GO TO (2,3),N3                                                    MXT20104          
C     --------------------------------------------                      MXT20105          
C     DO O-P REGRESSION ANALYSIS.                                       MXT20106          
C     --------------------------------------------                      MXT20107          
    2 CALL OPREG(IIA,IIB,IIC,                                           MXT20108          
     1T2,COMENV,WFSENV,DF,IDF,SS,SM,F,XMN,VC1,VC2,VC3,HS,HD,HSD,SE1,SE2,MXT20109          
     2SE3,THETA,RTHETA,SER,THETGS,THETPD,HDFS,VAR1,NAMVAR,              MXT20110          
     3REGSS,DEVSS,SE4,HB,HR,RB,SRR,DEVSM,FREG,B,FDEV,CORS,CORD,CHS,VA,  MXT20111          
     4VD,VAA,SL1,SL2,SL3,SCR1,SESCR1,SCR2,SESCR2,A,FKH,LS,XMAT1,XMAT2,  MXT20112          
     5 TS)                                                              MXT20113          
C     --------------------------------------------                      MXT20114          
C     PARTITION PHENOTYPIC VARIANCE.                                    MXT20115          
C     --------------------------------------------                      MXT20116          
    3 IF(DF(I8)) 99,99,33                                               MXT20117          
   33 IF(IM .EQ. 0) GO TO 99                                            MXT20118          
      DO 9 I=1,IM                                                       MXT20119          
      DO 9 J=I,IM                                                       MXT20120          
      JM=J+IS+ID                                                        MXT20121          
      JF=J+IS+ID+IM                                                     MXT20122          
      IF(IF .EQ. 0) GO TO 6                                             MXT20123          
      GO TO (5,5,9),N3                                                  MXT20124          
    5 IF (IS .NE. ID) GO TO 9                                           MXT20125          
      SL1(I,J)=(HS(JF,JF)-HS(JM,JM))/2.0                                MXT20126          
      IF (SL1(I,J) .LT. 0.0) SL1(I,J)=0.0                               MXT20127          
      SL2(I,J)=HD(JM,JM)-HD(JF,JF)                                      MXT20128          
    6 GO TO (7,12,9),N3                                                 MXT20129          
    7 VAA(I,J)=(HB(1,I,J)-HS(JM,JM))*4.0                                MXT20130          
      IF (VAA(I,J) .GT. 0.0) GO TO 10                                   MXT20131          
      VAA(I,J)=0.0                                                      MXT20132          
      VA(I,J)=HS(JM,JM)                                                 MXT20133          
      GO TO 11                                                          MXT20134          
   10 VA(I,J)=HS(JM,JM)-VAA(I,J)/4.0                                    MXT20135          
   11 CHS(I,J)=1.-DIVF(HS(JM,JM),HB(1,I,J))                             MXT20136          
      IF(IF .EQ. 0) GO TO 8                                             MXT20137          
      VD(I,J)=HD(JM,JM)-(VA(I,J)+0.75*VAA(I,J)+2.*SL1(I,J))             MXT20138          
    8 IF(IS .NE. ID) GO TO 9                                            MXT20139          
      SL3(I,J)=(HB(3,I,J)-HB(1,I,J))                                    MXT20140          
      IF(IF .EQ. 0) GO TO 17                                            MXT20141          
      XMAT1(I,J)=HB(3,I,J)-HB(2,I,J)                                    MXT20142          
      XMAT2(I,J)=HB(4,I,J)-HB(2,I,J)                                    MXT20143          
      GO TO 13                                                          MXT20144          
   12 VA(I,J)=HS(JM,JM)                                                 MXT20145          
      IF(IF .EQ. 0) GO TO 9                                             MXT20146          
      VD(I,J)=HD(JM,JM)-(VA(I,J)+2.0*SL1(I,J))                          MXT20147          
      GO TO (13,9),N3                                                   MXT20148          
   13 IF(XMAT1(I,J) .LT. 0.0) XMAT1(I,J)=0.0                            MXT20149          
      IF(XMAT2(I,J) .LT. 0.0) XMAT2(I,J)=0.0                            MXT20150          
      VD(I,J)=VD(I,J)-4.0*XMAT1(I,J)                                    MXT20151          
      SL3(I,J)=(SL3(I,J)-XMAT1(I,J))*0.707                              MXT20152          
      GO TO 9                                                           MXT20153          
   17 SL3(I,J)=SL3(I,J)*0.707                                           MXT20154          
    9 CONTINUE                                                          MXT20155          
   99 CONTINUE                                                          MXT20156          
      CALL PROUT(IIA,IIB,IIC,                                           MXT20157          
     1T2,COMENV,WFSENV,DF,IDF,SS,SM,F,XMN,VC1,VC2,VC3,HS,HD,HSD,SE1,SE2,MXT20158          
     2SE3,THETA,RTHETA,SER,THETGS,THETPD,HDFS,VAR1,NAMVAR,              MXT20159          
     3REGSS,DEVSS,SE4,HB,HR,RB,SRR,DEVSM,FREG,B,FDEV,CORS,CORD,CHS,VA,  MXT20160          
     4VD,VAA,SL1,SL2,SL3,SCR1,SESCR1,SCR2,SESCR2,A,FKH,LS,XMAT1,XMAT2,  MXT20161          
     5 TS)                                                              MXT20162          
   16 CONTINUE                                                          MXT20163          
      RETURN                                                            MXT20164          
      END                                                               MXT20165          
*ENDTEXT                                                                                  
