*TEXT                                                                                     
      SUBROUTINE STEP1 (IIA,IIB,IIC,IID,                                ST120001          
     1T2,COMENV,WFSENV,DF,IDF,SS,SM,F,XMN,VC1,VC2,VC3,HS,HD,HSD,SE1,SE2,ST120002          
     2SE3,THETA,RTHETA,SER,THETGS,THETPD,HDFS,VAR1,NAMVAR,              ST120003          
     3 A)                                                               ST120004          
C-----VERSION 2                                                         ST120005          
      COMMON/CMBLK1/I1,I2,N3,N4,N5,N6,IS,ID,IM,IF,N1,ITLE(9),Y,KPUT     ST120006          
      COMMON/CMBLK2/IT2,ICOMEN,IWFSEN,IDFQ,IIDFQ,ISS,ISM,IFQ,IXMN,IVC1, ST120007          
     1  IVC2,IVC3,IHS,IHD,IHSD,ISE1,ISE2,ISE3,ITHETA,IRTHET,ISER,ITHETG,ST120008          
     3  ITHETP,IHDFS,IVAR1,INAMVA,LEN26                                 ST120009          
      COMMON/CMBLK3/IX,IXID,IC2,IC3,ISX,IRXS,IOID,IPROD1,IPROD2,IL,ITEMPST120010          
     1  ,IPRODS,IBV1,IBV2,IBN,IAN,IPROD3,LEN44                          ST120011          
      DIMENSION A(1)                                                    ST120012          
      DIMENSION T2(3,7),COMENV(IIA,1),WFSENV(IIA,1),DF(1),IDF(1),       ST120013          
     1 SS(IIC,IIA,1),SM(IIB,IIA,1),F(IIB,IIA,1),XMN(IIA,1),VC1(IIA,1),  ST120014          
     2 VC2(IIA,1),VC3(IIA,1),HS(IIA,1),HD(IIA,1),HSD(IIA,1),SE1(IIA,1), ST120015          
     3 SE2(IIA,1),SE3(IIA,1),THETA(7,IIA,1),RTHETA(7,IIA,1),SER(7,IIA,1)ST120016          
     4 ,THETGS(IIA,1),THETPD(IIA,1),HDFS(IIA,1),VAR1(IIA,1),NAMVAR(1)   ST120017          
C-----                                                                  ST120018          
C-----READ VARIABLE NAMES PARAMETER CARD                                ST120019          
C-----                                                                  ST120020          
      READ (5,6) (NAMVAR(I),I=1,I1)                                     ST120021          
    6 FORMAT(9A8)                                                       ST120022          
C-----                                                                  ST120023          
      WRITE(6,7)(NAMVAR(I),I=1,I1)                                      ST120024          
    7 FORMAT(//15H0VARIABLE NAMES/(1H ,A8))                             ST120025          
C-----                                                                  ST120026          
      CALL FULSIB(IIA,IIB,IIC,IID,                                      ST120027          
     1T2,COMENV,WFSENV,DF,IDF,SS,SM,F,XMN,VC1,VC2,VC3,HS,HD,HSD,SE1,SE2,ST120028          
     2SE3,THETA,RTHETA,SER,THETGS,THETPD,HDFS,VAR1,NAMVAR,              ST120029          
     3A(IX),A(IXID),A(IC2),A(IC3),A(ISX),A(IRXS),A(IOID),A(IPROD1),     ST120030          
     4A(IPROD2),A(IL),A(ITEMP),A(IPRODS),A(IBV1),A(IBV2),A(IBN),A(IAN), ST120031          
     5A(IPROD3))                                                        ST120032          
      RETURN                                                            ST120033          
      END                                                               ST120034          
*ENDTEXT                                                                                  
