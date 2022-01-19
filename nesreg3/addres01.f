      SUBROUTINE ADDRES(IIA,IIB,IIC,IID)                                ADD30001          
C-----VERSION 3                                                         ADD30002          
      COMMON/CMBLK1/I1,I2,N3,N4,N5,N6,IS,ID,IM,IF,N1,ITLE(9),Y,KPUT     ADD30003          
      COMMON/CMBLK2/IT2,ICOMEN,IWFSEN,IDFQ,IIDFQ,ISS,ISM,IFQ,IXMN,IVC1, ADD30004          
     1  IVC2,IVC3,IHS,IHD,IHSD,ISE1,ISE2,ISE3,ITHETA,IRTHET,ISER,ITHETG,ADD30005          
     3  ITHETP,IHDFS,IVAR1,INAMVA,LEN26                                 ADD30006          
      COMMON/CMBLK3/IX,IXID,IC2,IC3,ISX,IRXS,IOID,IPROD1,IPROD2,IL,ITEMPADD30007          
     1  ,IPRODS,IBV1,IBV2,IBN,IAN,IPROD3,LEN44                          ADD30008          
      COMMON/CMBLK4/IREGSS,IDEVSS,ISE4,IHB,IHR,IRB,ISRR,IDEVSM,IFREG,   ADD30009          
     1  IB,IFDEV,ICORS,ICORD,ICHS,IVA,IVD,IVAA,ISL1,ISL2,ISL3,ISCR1,    ADD30010          
     2  ISESC1,ISCR2,ISESC2,IA,IFKH,ILS,IXMAT1,IXMAT2,ITS,LEN56         ADD30011          
      COMMON/CMBLK7/ MI1,MIS,MID,MIM,MIF,M1,M2,ICYC,LUI,LUS             ADD30012          
C-----                                                                  ADD30013          
C-----CALCULATE STARTING POSITION OF ALL ARRAYS COMMON TO STEPS 1 AND 2 ADD30014          
C-----                                                                  ADD30015          
      J1=I1*I1                                                          ADD30016          
      IT2=1                                                             ADD30017          
      ICOMEN=IT2+21                                                     ADD30018          
      IWFSEN=ICOMEN+J1                                                  ADD30019          
      IDFQ=IWFSEN+J1                                                    ADD30020          
      IIDFQ=IDFQ+I2+3                                                   ADD30021          
      ISS=IIDFQ+I2+3                                                    ADD30022          
      ISM=ISS+(I2+2)*J1                                                 ADD30023          
      IFQ=ISM+(I2+1)*J1                                                 ADD30024          
      IXMN=IFQ+(I2+1)*J1                                                ADD30025          
      IVC1=IXMN+J1                                                      ADD30026          
      IVC2=IVC1+J1                                                      ADD30027          
      IVC3=IVC2+J1                                                      ADD30028          
      IHS=IVC3+J1                                                       ADD30029          
      IHD=IHS+J1                                                        ADD30030          
      IHSD=IHD+J1                                                       ADD30031          
      ISE1=IHSD+J1                                                      ADD30032          
      ISE2=ISE1+J1                                                      ADD30033          
      ISE3=ISE2+J1                                                      ADD30034          
      ITHETA=ISE3+J1                                                    ADD30035          
      IRTHET=ITHETA+7*J1                                                ADD30036          
      ISER=IRTHET+7*J1                                                  ADD30037          
      ITHETG=ISER+7*J1                                                  ADD30038          
      ITHETP=ITHETG+J1                                                  ADD30039          
      IHDFS=ITHETP+J1                                                   ADD30040          
      IVAR1=IHDFS+J1                                                    ADD30041          
      INAMVA=IVAR1+J1                                                   ADD30042          
      LEN26=INAMVA+MI1-1                                                ADD30043          
C-----                                                                  ADD30044          
C-----CALCULATE STARTING POSITIONS OF ALL ARRAYS SPECIAL TO STEP 1      ADD30045          
C-----                                                                  ADD30046          
      IX=LEN26+1                                                        ADD30047          
      IXID=IX+I1                                                        ADD30048          
      IC2=IXID+I2+1                                                     ADD30049          
      IC3=IC2+I2+3                                                      ADD30050          
      ISX=IC3+I2+3                                                      ADD30051          
      IRXS=ISX+(I2+1)*I1                                                ADD30052          
      IOID=IRXS+I1                                                      ADD30053          
      IPROD1=IOID+I2                                                    ADD30054          
      IPROD2=IPROD1+J1                                                  ADD30055          
      IL=IPROD2+J1                                                      ADD30056          
      ITEMP=IL+7                                                        ADD30057          
      IPRODS=ITEMP+7                                                    ADD30058          
      IBV1=IPRODS+J1                                                    ADD30059          
      IBV2=IBV1+N5                                                      ADD30060          
      IBN=IBV2+N5*N6                                                    ADD30061          
      IAN=IBN+N5                                                        ADD30062          
      IPROD3=IAN+N5*N6                                                  ADD30063          
      LEN44=IPROD3+J1-1                                                 ADD30064          
      WRITE(6,10) LEN44,LEN44                                           ADD30065          
   10 FORMAT(43H0FIELD LENGTH OF ARRAYS USED BY STEP ONE = ,I8,         ADD30066          
     1 13H (DECIMAL) = ,O8,8H (OCTAL))                                  ADD30067          
C-----                                                                  ADD30068          
C-----CALCULATE STARTING POSITIONS OF ALL ARRAYS SPECIAL TO STEP 2      ADD30069          
      J4=4*J1                                                           ADD30070          
      IREGSS=LEN26+1                                                    ADD30071          
      IDEVSS=IREGSS+J4                                                  ADD30072          
      ISE4=IDEVSS+J4                                                    ADD30073          
      IHB=ISE4+J4                                                       ADD30074          
      IHR=IHB+J4                                                        ADD30075          
      IRB=IHR+J4                                                        ADD30076          
      ISRR=IRB+J4                                                       ADD30077          
      IDEVSM=ISRR+J4                                                    ADD30078          
      IFREG=IDEVSM+J4                                                   ADD30079          
      IB=IFREG+J4                                                       ADD30080          
      IFDEV=IB+J4                                                       ADD30081          
      ICORS=IFDEV+J4                                                    ADD30082          
      ICORD=ICORS+J1                                                    ADD30083          
      ICHS=ICORD+J1                                                     ADD30084          
      IVA=ICHS+J1                                                       ADD30085          
      IVD=IVA+J1                                                        ADD30086          
      IVAA=IVD+J1                                                       ADD30087          
      ISL1=IVAA+J1                                                      ADD30088          
      ISL2=ISL1+J1                                                      ADD30089          
      ISL3=ISL2+J1                                                      ADD30090          
      ISCR1=ISL3+J1                                                     ADD30091          
      ISESC1=ISCR1+3*J1                                                 ADD30092          
      ISCR2=ISESC1+3*J1                                                 ADD30093          
      ISESC2=ISCR2+4*J1                                                 ADD30094          
      IA=ISESC2+4*J1                                                    ADD30095          
      IFKH=IA+4*J1                                                      ADD30096          
      ILS=IFKH+3                                                        ADD30097          
      IXMAT1=ILS+3                                                      ADD30098          
      IXMAT2=IXMAT1+J1                                                  ADD30099          
      ITS=IXMAT2+J1                                                     ADD30100          
      LEN56=ITS+21-1                                                    ADD30101          
C-----                                                                  ADD30102          
      WRITE(6,12) LEN56,LEN56                                           ADD30103          
   12 FORMAT(43H0FIELD LENGTH OF ARRAYS USED BY STEP TWO = ,I8,         ADD30104          
     1  13H (DECIMAL) = ,O8,8H (OCTAL))                                 ADD30105          
      RETURN                                                            ADD30106          
      END                                                               ADD30107          
