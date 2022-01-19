*TEXT                                                                                     
      SUBROUTINE STEPS(IIA,IIB,IIC,IID,LPROG,A)                         STP30001          
C-----VERSION 3                                                         STP30002          
      DIMENSION A(1)                                                    STP30003          
      COMMON/CMBLK1/I1,I2,N3,N4,N5,N6,IS,ID,IM,IF,N1,ITLE(9),Y,KPUT     STP30004          
      COMMON/CMBLK2/IT2,ICOMEN,IWFSEN,IDFQ,IIDFQ,ISS,ISM,IFQ,IXMN,IVC1, STP30005          
     1  IVC2,IVC3,IHS,IHD,IHSD,ISE1,ISE2,ISE3,ITHETA,IRTHET,ISER,ITHETG,STP30006          
     3  ITHETP,IHDFS,IVAR1,INAMVA,LEN26                                 STP30007          
      COMMON/CMBLK3/IX,IXID,IC2,IC3,ISX,IRXS,IOID,IPROD1,IPROD2,IL,ITEMPSTP30008          
     1  ,IPRODS,IBV1,IBV2,IBN,IAN,IPROD3,LEN44                          STP30009          
      COMMON/CMBLK4/IREGSS,IDEVSS,ISE4,IHB,IHR,IRB,ISRR,IDEVSM,IFREG,   STP30010          
     1  IB,IFDEV,ICORS,ICORD,ICHS,IVA,IVD,IVAA,ISL1,ISL2,ISL3,ISCR1,    STP30011          
     2  ISESC1,ISCR2,ISESC2,IA,IFKH,ILS,IXMAT1,IXMAT2,ITS,LEN56         STP30012          
      COMMON/CMBLK7/ MI1,MIS,MID,MIM,MIF,M1,M2,ICYC,LUI,LUS             STP30013          
      IF(ICYC-1) 42,42,43                                               STP30014          
C-----                                                                  STP30015          
C-----CALCULATE TOTAL FIELD LENGTH FOR STEP 1                           STP30016          
C-----                                                                  STP30017          
   42 LFIELD=LPROG+LEN44                                                STP30018          
      WRITE(6,13) LFIELD,LFIELD                                         STP30019          
   13 FORMAT(52H0FIELD LENGTH OF PROGRAM PLUS ARRAYS FOR STEP ONE = ,   STP30020          
     1  I8,13H (DECIMAL) = ,O8,8H (OCTAL))                              STP30021          
C-----                                                                  STP30022          
C-----SET FIELD LENGTH FOR STEP ONE                                     STP30023          
C-----                                                                  STP30024          
      CALL SETFL(LFIELD)                                                STP30025          
C-----                                                                  STP30026          
C-----STEP ONE                                                          STP30027          
C-----BRANCH TO SUBROUTINE STEP1                                        STP30028          
C-----                                                                  STP30029          
   43 CALL STEP1 (  IIA,IIB,IIC,IID,                                    STP30030          
     1A(IT2),A(ICOMEN),A(IWFSEN),A(IDFQ),A(IIDFQ),A(ISS),A(ISM),A(IFQ), STP30031          
     2A(IXMN),A(IVC1),A(IVC2),A(IVC3),A(IHS),A(IHD),A(IHSD),A(ISE1),    STP30032          
     3A(ISE2),A(ISE3),A(ITHETA),A(IRTHET),A(ISER),A(ITHETG),A(ITHETP),  STP30033          
     4A(IHDFS),A(IVAR1),A(INAMVA),                                      STP30034          
     5 A)                                                               STP30035          
C-----                                                                  STP30036          
      IF(KPUT) 7,31,30                                                  STP30037          
   31 IF(ICYC-1) 40,40,41                                               STP30038          
C-----                                                                  STP30039          
C-----CALCULATE TOTAL FIELD LENGTH FOR STEP 2                           STP30040          
C-----                                                                  STP30041          
   40 LFIELD=LPROG+LEN56                                                STP30042          
      WRITE(6,14) LFIELD,LFIELD                                         STP30043          
   14 FORMAT(52H0FIELD LENGTH OF PROGRAM PLUS ARRAYS FOR STEP TWO = ,   STP30044          
     1 I8,13H (DECIMAL) = ,O8,8H (OCTAL))                               STP30045          
C-----                                                                  STP30046          
C-----SET FIELD LENGTH FOR STEP TWO                                     STP30047          
C-----                                                                  STP30048          
      CALL SETFL(LFIELD)                                                STP30049          
C-----                                                                  STP30050          
C-----STEP TWO                                                          STP30051          
C-----BRANCH TO SUBROUTINE STEP2                                        STP30052          
C-----                                                                  STP30053          
   41 CALL STEP2 ( IIA,IIB,IIC,                                         STP30054          
     1A(IT2),A(ICOMEN),A(IWFSEN),A(IDFQ),A(IIDFQ),A(ISS),A(ISM),A(IFQ), STP30055          
     2A(IXMN),A(IVC1),A(IVC2),A(IVC3),A(IHS),A(IHD),A(IHSD),A(ISE1),    STP30056          
     3A(ISE2),A(ISE3),A(ITHETA),A(IRTHET),A(ISER),A(ITHETG),A(ITHETP),  STP30057          
     4A(IHDFS),A(IVAR1),A(INAMVA),                                      STP30058          
     5 A)                                                               STP30059          
      GO TO 7                                                           STP30060          
C-----                                                                  STP30061          
C-----FLUSH THRU OBSERVATION SETS FOR THIS PROBLEM                      STP30062          
   30 CALL READX(A,X)                                                   STP30063          
      IF(A(1)) 30,7,30                                                  STP30064          
C-----                                                                  STP30065          
    7 CONTINUE                                                          STP30066          
      RETURN                                                            STP30067          
      END                                                               STP30068          
*ENDTEXT                                                                                  
