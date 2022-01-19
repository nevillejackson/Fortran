*TEXT                                                                                     
      SUBROUTINE STEP2 (IIA,IIB,IIC,                                    ST230001          
     1T2,COMENV,WFSENV,DF,IDF,SS,SM,F,XMN,VC1,VC2,VC3,HS,HD,HSD,SE1,SE2,ST230002          
     2SE3,THETA,RTHETA,SER,THETGS,THETPD,HDFS,VAR1,NAMVAR,              ST230003          
     3 A)                                                               ST230004          
C-----VERSION 3                                                         ST230005          
      COMMON/CMBLK1/I1,I2,N3,N4,N5,N6,IS,ID,IM,IF,N1,ITLE(9),Y,KPUT     ST230006          
      COMMON/CMBLK2/IT2,ICOMEN,IWFSEN,IDFQ,IIDFQ,ISS,ISM,IFQ,IXMN,IVC1, ST230007          
     1  IVC2,IVC3,IHS,IHD,IHSD,ISE1,ISE2,ISE3,ITHETA,IRTHET,ISER,ITHETG,ST230008          
     3  ITHETP,IHDFS,IVAR1,INAMVA,LEN26                                 ST230009          
      COMMON/CMBLK4/IREGSS,IDEVSS,ISE4,IHB,IHR,IRB,ISRR,IDEVSM,IFREG,   ST230010          
     1  IB,IFDEV,ICORS,ICORD,ICHS,IVA,IVD,IVAA,ISL1,ISL2,ISL3,ISCR1,    ST230011          
     2  ISESC1,ISCR2,ISESC2,IA,IFKH,ILS,IXMAT1,IXMAT2,ITS,LEN56         ST230012          
      DIMENSION T2(3,7),COMENV(IIA,1),WFSENV(IIA,1),DF(1),IDF(1),       ST230013          
     1 SS(IIC,IIA,1),SM(IIB,IIA,1),F(IIB,IIA,1),XMN(IIA,1),VC1(IIA,1),  ST230014          
     2 VC2(IIA,1),VC3(IIA,1),HS(IIA,1),HD(IIA,1),HSD(IIA,1),SE1(IIA,1), ST230015          
     3 SE2(IIA,1),SE3(IIA,1),THETA(7,IIA,1),RTHETA(7,IIA,1),SER(7,IIA,1)ST230016          
     4 ,THETGS(IIA,1),THETPD(IIA,1),HDFS(IIA,1),VAR1(IIA,1),NAMVAR(1)   ST230017          
      DIMENSION A(1)                                                    ST230018          
      I=1                                                               ST230019          
      CALL MIXTUR(IIA,IIB,IIC,                                          ST230020          
     1T2,COMENV,WFSENV,DF,IDF,SS,SM,F,XMN,VC1,VC2,VC3,HS,HD,HSD,SE1,SE2,ST230021          
     2SE3,THETA,RTHETA,SER,THETGS,THETPD,HDFS,VAR1,NAMVAR,              ST230022          
     3A(IREGSS),A(IDEVSS),A(ISE4),A(IHB),A(IHR),A(IRB),A(ISRR),A(IDEVSM)ST230023          
     4,A(IFREG),A(IB),A(IFDEV),A(ICORS),A(ICORD),A(ICHS),A(IVA),A(IVD), ST230024          
     5A(IVAA),A(ISL1),A(ISL2),A(ISL3),A(ISCR1),A(ISESC1),A(ISCR2),      ST230025          
     6A(ISESC2),A(IA),A(IFKH),A(ILS),A(IXMAT1),A(IXMAT2),A(ITS))        ST230026          
      I=I+1                                                             ST230027          
      RETURN                                                            ST230028          
      END                                                               ST230029          
*ENDTEXT                                                                                  
