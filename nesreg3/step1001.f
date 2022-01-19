*TEXT                                                                                     
      SUBROUTINE STEP1 (IIA,IIB,IIC,IID,                                ST130001          
     1T2,COMENV,WFSENV,DF,IDF,SS,SM,F,XMN,VC1,VC2,VC3,HS,HD,HSD,SE1,SE2,ST130002          
     2SE3,THETA,RTHETA,SER,THETGS,THETPD,HDFS,VAR1,NAMVAR,              ST130003          
     3 A)                                                               ST130004          
C-----VERSION 3                                                         ST130005          
      COMMON/CMBLK1/I1,I2,N3,N4,N5,N6,IS,ID,IM,IF,N1,ITLE(9),Y,KPUT     ST130006          
      COMMON/CMBLK2/IT2,ICOMEN,IWFSEN,IDFQ,IIDFQ,ISS,ISM,IFQ,IXMN,IVC1, ST130007          
     1  IVC2,IVC3,IHS,IHD,IHSD,ISE1,ISE2,ISE3,ITHETA,IRTHET,ISER,ITHETG,ST130008          
     3  ITHETP,IHDFS,IVAR1,INAMVA,LEN26                                 ST130009          
      COMMON/CMBLK3/IX,IXID,IC2,IC3,ISX,IRXS,IOID,IPROD1,IPROD2,IL,ITEMPST130010          
     1  ,IPRODS,IBV1,IBV2,IBN,IAN,IPROD3,LEN44                          ST130011          
      COMMON/CMBLK7/ MI1,MIS,MID,MIM,MIF,M1,M2,ICYC,LUI,LUS             ST130012          
      DIMENSION A(1)                                                    ST130013          
      DIMENSION T2(3,7),COMENV(IIA,1),WFSENV(IIA,1),DF(1),IDF(1),       ST130014          
     1 SS(IIC,IIA,1),SM(IIB,IIA,1),F(IIB,IIA,1),XMN(IIA,1),VC1(IIA,1),  ST130015          
     2 VC2(IIA,1),VC3(IIA,1),HS(IIA,1),HD(IIA,1),HSD(IIA,1),SE1(IIA,1), ST130016          
     3 SE2(IIA,1),SE3(IIA,1),THETA(7,IIA,1),RTHETA(7,IIA,1),SER(7,IIA,1)ST130017          
     4 ,THETGS(IIA,1),THETPD(IIA,1),HDFS(IIA,1),VAR1(IIA,1),NAMVAR(1)   ST130018          
      DIMENSION KARD(1000)                                                                
      DATA(IBLK=8H        )                                             ST130020          
C-----                                                                  ST130021          
C-----READ VARIABLE NAMES PARAMETER CARD                                ST130022          
C-----ON FIRST CALL ONLY                                                ST130023          
C-----                                                                  ST130024          
      IF(M1) 8,10,8                                                     ST130025          
   10 READ (5,6) (NAMVAR(I),I=1,MI1)                                    ST130026          
    6 FORMAT(9A8)                                                       ST130027          
      WRITE(6,7)(I,NAMVAR(I),I=1,MI1)                                   ST130028          
    7 FORMAT(//15H0VARIABLE NAMES//16H NUMBER     NAME//(1H ,I4,3X,A8)) ST130029          
C-----                                                                  ST130030          
C-----COPY DATA TO UNIT LUN IF IN CYCLIC MODE ON FIRST CALL             ST130031          
C-----                                                                  ST130032          
      IF(ICYC-1) 8,8,9                                                  ST130033          
    9 CALL DEFINE                                                                         
      CALL LNREAD(LREC,LRECW,LUI)                                                         
   29 CALL READUN(LUI,KARD,IFLAG,LRECW)                                                   
      IF(IFLAG) 15,17,16                                                                  
   15 WRITE(6,18) LUI                                                   ST130037          
   18 FORMAT(13H0EOF ON UNIT ,I2)                                       ST130038          
      GO TO 14                                                                            
   16 WRITE(6,19) LUI                                                   ST130040          
   19 FORMAT(22H0PARITY ERROR ON UNIT ,I2)                              ST130041          
      STOP                                                              ST130042          
   17 CONTINUE                                                          ST130043          
      CALL WRITUN(LUS,KARD,LRECW)                                                         
C-----CHECK FOR BLANK RECORD INDICATING END                                               
      IF(KARD(1)-IBLK) 29,14,29                                                           
   14 END FILE LUS                                                                        
      GO TO 11                                                          ST130049          
C-----                                                                  ST130050          
    8 CALL FULSIB(IIA,IIB,IIC,IID,                                      ST130051          
     1T2,COMENV,WFSENV,DF,IDF,SS,SM,F,XMN,VC1,VC2,VC3,HS,HD,HSD,SE1,SE2,ST130052          
     2SE3,THETA,RTHETA,SER,THETGS,THETPD,HDFS,VAR1,NAMVAR,              ST130053          
     3A(IX),A(IXID),A(IC2),A(IC3),A(ISX),A(IRXS),A(IOID),A(IPROD1),     ST130054          
     4A(IPROD2),A(IL),A(ITEMP),A(IPRODS),A(IBV1),A(IBV2),A(IBN),A(IAN), ST130055          
     5A(IPROD3))                                                        ST130056          
   11 RETURN                                                            ST130057          
      END                                                               ST130058          
*ENDTEXT                                                                                  
