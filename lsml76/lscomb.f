      SUBROUTINE LSCOMB                                                 DK170010          
C     -------------------------------------------                       DK170020          
C     SUBROUTINE WHICH CALLS MATINV, COMPUTES AND LISTS CONSTANTS.      DK170030          
C     ALSO CALLS POLYNO AND SVCVC IF NEEDED                             DK170040          
C     ----------------------------------------------------              DK170050          
      EXTERNAL DSQRT                                                                      
      DIMENSION ARRAY(5000),SSCPR(630),SSS(630),RHM(2450),TOT(106),TOT2(DK170080          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100),NS2(15)    DK170090          
      DIMENSION FAB(5000),FY(630),TOT4(106),SAB(630),FSQ(525),TOT5(106) DK170100          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),MZ(10),IPL(13),NSDK170110          
     1P(13),NND(13),XP(81),YP(41),      NDC(10),NMI(10),MEN(20),NCL(20),DK170120          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(50),NCLN(50),LNE(50),NDEN(1                  
     300),NLIT(50),NMA(50),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(50),                  
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDK170150          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),                          
     6EFF1(50),EFF2(50),NOS(300),X(106),NMAC(50),I309(15),NR1(15),NW(15)                  
     7,R1I(100),R2I(100),WW(200),ICLR(90),IRM(40),LAD(90)               DK170180          
      DIMENSION IC(1000)                                                                  
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADK170190          
     1B4,LITY,TRED,YM,XP,YP,IM,MS,NEG,IPL,NSP,NND,FAB,FY,TOT4,SAB,FSQ,TODK170200          
     2T5,MZ,R1I,R2I,WW,ICLR,IRM,LAD                                     DK170210          
      COMMON /CMBLK2/LIT,LITR,NLIT,XM,EFF1,EFF2,X,NDC,NMI,MEN,NCL,LME,IBDK170220          
     1EG,IDEN,NCLN,NEN,LNE,NBEG,NDEN,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQDK170230          
     2C,NREGP,LGTX,JBEG,NDECX,NEGY,LNY,LHY,KBEG,NDECY,IC,NOS,NMAC       DK170240          
      COMMON /CMBLK3/RR,REP,DF,SDF,EDF,WK,IJOB,NAB,NLHM,NRHM,NMEA,NME,NNDK170250          
     1EA,NNE,N2F,NPR,IRAN,MPOP,LIOP,IAD,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,DK170260          
     2NOM,NON,ML,MLB,NS,NOT,KPUT,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,IN,I30DK170270          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DK170280          
     1L,L7,KA,KB,KC,SOF,NX,KD,MTY,MAN,NRUN,NRN,EDFF,NAME,EDFS,NAM5,IFP,NDK170290          
     2DFA,IFPC,NDFC,NDFAC,NINT,IBET,LBEG,LTHN,NNDC,WTT,SWT1             DK170300          
C     ----------------------------------------------------              DK170310          
C     INVERSION AND LISTING OF INVERSE MATRIX                           DK170320          
C     ----------------------------------------------------              DK170330          
      NBRC=1                                                            DK170340          
      NERC=NLHM                                                         DK170350          
      K=1                                                               DK170360          
      IF (NLHM.EQ.0) GO TO 9                                            DK170370          
      DO 18 I=1,NLHM                                                    DK170380          
      X(I)=ARRAY(K)                                                     DK170390          
   18 K=K+NLHM-I+1                                                      DK170400          
      DET=1.0                                                           DK170410          
      CALL MATINV (ARRAY,NBRC,NERC,NLHM,X,DET)                          DK170420          
      K=1                                                               DK170430          
      DO 224 I=1,NLHM                                                   DK170440          
      X(I)=ARRAY(K)                                                     DK170450          
  224 K=K+NLHM-I+1                                                      DK170460          
      DO 225 I=1,NLHM                                                   DK170470          
      IF(X(I).LE.0.0E+00) GO TO 226                                                       
      IF(X(I).LT.0.0E+00) GO TO 226                                                       
      IF(X(I).GT.1.0E+05) GO TO 226                                                       
  225 CONTINUE                                                          DK170500          
      GO TO 24                                                          DK170510          
  226 WRITE (6,1001) IJOB                                               DK170520          
      WRITE (6,1004)                                                    DK170530          
      WRITE (6,1025) (ARRAY(I),I=1,MATX)                                DK170540          
      WRITE (6,1022)                                                    DK170550          
      WRITE (6,1004)                                                    DK170560          
      WRITE (6,1025) (X(K),K=1,NLHM)                                    DK170570          
      WRITE (6,1024) DET,DET                                            DK170580          
      WRITE (6,2226)                                                    DK170590          
 2226 FORMAT (//5X,56HDEPENDENCY FOUND IN THE INVERSE MATRIX.  JOB TERMIDK170600          
     *NATED.)                                                           DK170610          
      MULL=1                                                            DK170620          
      RETURN                                                            DK170630          
   24 IF (LIOP.EQ.20) GO TO 19                                          DK170640          
      WRITE (6,1001) IJOB                                               DK170650          
      WRITE (6,1004)                                                    DK170660          
      IF (LIOP.NE.10) GO TO 26                                          DK170670          
      WRITE (6,1025) (ARRAY(I), I=1,MATX)                               DK170680          
      WRITE (6,1022)                                                    DK170690          
 1022 FORMAT (1H0,29X,36HLISTING OF DIAGONAL INVERSE ELEMENTS)          DK170700          
      WRITE (6,1004)                                                    DK170710          
      WRITE (6,1025) (X(K),K=1,NLHM)                                    DK170760          
      GO TO 19                                                          DK170770          
 1025 FORMAT (7(2X,E15.8))                                              DK170780          
 1001 FORMAT (1H0,29X,43HLISTING OF INVERSE ELEMENTS FOR PROBLEM NO.,I3)DK170790          
   26 WRITE (6,1002)                                                    DK170800          
 1002 FORMAT (1H0,8HROW  COL,15X,21HINDEPENDENT VARIABLES,34X,15HINVERSEDK170810          
     1 ELEMENT)                                                         DK170820          
      WRITE (6,1003)                                                    DK170830          
 1003 FORMAT (1H ,9HCODE CODE,9X,3HROW,22X,6HCOLUMN,16X,41HFIXED POINT FDK170840          
     1ORMAT  FLOATING POINT FORMAT)                                     DK170850          
   28 DO 32 I=NBRC,NERC                                                 DK170860          
      WRITE (6,1004)                                                    DK170870          
 1004 FORMAT (1H )                                                      DK170880          
   30 DO 32 J=I,NERC                                                    DK170890          
      K1=NLHM*(I-1)-I*(I-3)/2+J-I                                       DK170900          
      WRITE (6,1005) I,J,LAB1(I),LAB2(I),LAB3(I),LAB4(I),LAB1(J),LAB2(J)DK170910          
     1,LAB3(J),LAB4(J),ARRAY(K1),ARRAY(K1)                              DK170920          
 1005 FORMAT (1H ,2(I3,2X),2(A6,1X),2A6,2(A6,1X),2A6,2X,F17.8,8X,E15.8) DK170930          
   32 CONTINUE                                                          DK170940          
C     ----------------------------------------------------              DK170950          
C     COMPUTATION AND LISTING OF CONSTANTS                              DK170960          
C     ----------------------------------------------------              DK170970          
   19 WRITE (6,1024) DET,DET                                            DK170980          
 1024 FORMAT(1H0,44HTHE DETERMINANT OF THE CORRELATION MATRIX IS,F30.16,DK170990          
     1D28.16)                                                           DK171000          
    9 EDF=DF-FLOAT(NLHM)                                                DK171010          
      IF (EDF.GT.0) GO TO 57                                            DK171020          
      WRITE (6,1060)                                                    DK171030          
 1060 FORMAT (85H0THE DEGREES OF FREEDOM FOR THE REMAINDER(RDF) ARE ZERODK171040          
     1. HOWEVER, THE PROGRAM HAS SET/84H THIS VALUE TO 1 SO THAT THE PRODK171050          
     2BLEM CAN BE FINISHED WITHOUT ENCOUNTERING INTERRUPTS/78H CAUSED BYDK171060          
     3 DIVISION BY ZERO. ALL RESULTS WHICH ARE A FUNCTION OF RDF SHOULD DK171070          
     4BE/13H DISREGARDED.)                                              DK171080          
      EDF=1.0                                                           DK171090          
   57 IF (KB.EQ.0.AND.KD.EQ.0) GO TO 50                                 DK171100          
      CALL MIXEDF(MATX,KB,NLHM,ARRAY,FAB,TOT4,SOF,NX,KD,LIOP)           DK171110          
   50 IF (NLHM.EQ.0.OR.NRHM.EQ.0) GO TO 8                               DK171120          
      IF (LIOP.EQ.20) GO TO 55                                          DK171130          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 56                    DK171140          
      GO TO 55                                                          DK171150          
   56 WRITE (6,1006) IJOB                                               DK171160          
 1006 FORMAT (1H0,18X,45HLISTING OF CONSTANT ESTIMATES FOR PROBLEM NO.,IDK171170          
     13)                                                                DK171180          
      WRITE (6,1007)                                                    DK171190          
 1007 FORMAT (1H0,10H RHM   ROW,43X,18HCONSTANT ESTIMATES/ 1H ,33H NAME DK171200          
     1 CODE  INDEPENDENT VARIABLE,9X,41HFIXED POINT FORMAT  FLOATING POIDK171210          
     2NT FORMAT)                                                        DK171220          
   55 DO 54 K=1,NRHM                                                    DK171230          
      WRITE (6,1004)                                                    DK171240          
   34 TREDS=0.0                                                         DK171250          
      DO 52 I=NBRC,NERC                                                 DK171260          
      TEMP=0.0                                                          DK171270          
      DO 44 J=NBRC,NERC                                                 DK171280          
      IF (I-J.LT.0) GO TO 38                                            DK171290          
   36 K1=NLHM*(J-1)-J*(J-3)/2+I-J                                       DK171300          
      GO TO 40                                                          DK171310          
   38 K1=NLHM*(I-1)-I*(I-3)/2+J-I                                       DK171320          
   40 K4=(K-1)*NLHM+J                                                   DK171330          
   44 TEMP=TEMP+RHM(K4)*ARRAY(K1)                                       DK171340          
      K4=NLHM*(K-1)+I                                                   DK171350          
   48 TREDS=TREDS+RHM(K4)*TEMP                                          DK171360          
      TRED(K)=TREDS                                                     DK171370          
      K1=MATX+(K-1)*NLHM+I                                              DK171380          
      ARRAY(K1)=TEMP                                                    DK171390          
      IF (LIOP.EQ.20.OR.NAB.EQ.0.OR.NAB.EQ.3) GO TO 52                  DK171400          
      WRITE (6,1008) LITY(K),I,LAB1(I),LAB2(I),LAB3(I),LAB4(I),TEMP,TEMPDK171410          
 1008 FORMAT (1H ,A6,I4,2X,4(A6,1X),F17.8,8X,E15.8)                     DK171420          
   52 CONTINUE                                                          DK171430          
   54 CONTINUE                                                          DK171440          
      IF (MTY.EQ.1.OR.NRN.GT.1) GO TO 10                                DK171450          
      DO 20 I=1,NOT                                                     DK171460          
      TOT5(I)=TOT(I)                                                    DK171470          
   20 CONTINUE                                                          DK171490          
      GO TO (11,12,13,12,13,13,17),MTY                                  DK171500          
C     ------------------------------------------------------            DK171510          
C     STORE INVERSE ELEMENTS FOR MU                                     DK171520          
C     ------------------------------------------------------            DK171530          
   12 J=5000-NLHM                                                       DK171540          
      K1=1                                                              DK171550          
      DO 21 I=J,4999                                                    DK171560          
      FAB(I)=ARRAY(K1)                                                  DK171570          
   21 K1=K1+1                                                           DK171580          
      GO TO 11                                                          DK171590          
C     ------------------------------------------------------            DK171600          
C     STORE IDEN, NOS AND INVERSE ELEMENTS FOR MU AND A CONSTANTS       DK171610          
C     ------------------------------------------------------            DK171620          
   13 K=NCL(1)-1                                                        DK171630          
      K2=NCL(1)                                                         DK171640          
      K1=K2                                                             DK171650          
      J=301-K2                                                          DK171660          
      K4=100                                                            DK171670          
      IB=K4                                                                               
      JB=K1                                                                               
      CALL MOVER(IDEN,K4,K1,IB,JB,K2,0,0)                                                 
      K4=K4-1                                                                             
      K1=K1-1                                                                             
      IB=J                                                                                
      JB=1                                                                                
      CALL MOVER(NOS,J,I,IB,JB,K2,1,1)                                                    
      J=J+1                                                                               
      IF(MTY.EQ.6) GO TO 12                                                               
      J=5000-NLHM*K2+(K2*K)/2                                           DK171740          
      K1=1                                                              DK171750          
      DO 22 I=J,4999                                                    DK171760          
      FAB(I)=ARRAY(K1)                                                  DK171770          
   22 K1=K1+1                                                           DK171780          
C     ------------------------------------------------------            DK171790          
C     STORE CONSTANTS FOR A                                             DK171800          
C     ------------------------------------------------------            DK171810          
   29 K4=4999                                                           DK171820          
      DO 23 I=1,NRHM                                                    DK171830          
      K=MATX+(NRHM-I+1)*NLHM-(NLHM-K2)                                  DK171840          
      NEL=K2-1                                                                            
      IB=K4                                                                               
      JB=K                                                                                
      CALL MOVER(ARRAY,K4,K1,IB,JB,NEL,0,0)                                               
      K4=K4-1                                                                             
   23 CONTINUE                                                                            
      GO TO 11                                                          DK171890          
C     ------------------------------------------------------            DK171900          
C     STORE INVERSE ELEMENTS FOR MU AND A FOR MODEL 7                   DK171910          
C     ------------------------------------------------------            DK171920          
   17 K=NCL(1)-1                                                        DK171930          
      J=NCL(2)-1                                                        DK171940          
      K2=NCL(1)                                                         DK171950          
      K3=K2+NCL(2)                                                      DK171960          
      K1=K3+K2*NCL(2)                                                   DK171970          
      I2=301-K1                                                         DK171980          
      K4=100                                                            DK171990          
      K1=K3                                                             DK172000          
      IB=K4                                                                               
      JB=K1                                                                               
      CALL MOVER(IDEN,K4,K1,IB,JB,K3,0,0)                                                 
      K4=K4-1                                                                             
      K1=K1-1                                                                             
      IB=I2                                                                               
      JB=1                                                                                
      CALL MOVER(NOS,I2,I,IB,JB,K3,1,1)                                                   
      I2=I2+1                                                                             
      K1=0                                                              DK172070          
      DO 88 I=1,NME                                                     DK172080          
   88 K1=K1+NCL(I)                                                      DK172090          
      IF (NNE.EQ.0) GO TO 91                                            DK172100          
      DO 90 I=1,NNE                                                     DK172110          
   90 K1=K1+NCLN(I)                                                     DK172120          
   91 K3=K1+K2*NCL(2)                                                   DK172130          
      K1=K1+1                                                           DK172140          
      NEL=K3-K1+1                                                                         
      IB=I2                                                                               
      JB=K1                                                                               
      CALL MOVER(NOS,I2,I,IB,JB,NEL,1,1)                                                  
      I2=I2+1                                                                             
      K1=NLHM-K2-J                                                      DK172180          
      J=5000-MATX+(K1*(K1+1))/2-(NLHM-IEI+1)*(K*J)+((K*J)*(K*J-1))/2    DK172190          
      NDEN(100)=J                                                       DK172200          
      K1=J+NLHM*K2-(K2*(K2-1))/2-1                                      DK172210          
      NDEN(99)=K1                                                       DK172220          
      J=((NLHM-K2)*(NLHM-K2+1))/2+1                                     DK172230          
      K1=J+NLHM*K2-(K2*(K2-1))/2-1                                      DK172240          
      NDEN(98)=J                                                        DK172250          
      K4=1                                                              DK172260          
      DO 27 I=J,K1                                                      DK172270          
      FAB(I)=ARRAY(K4)                                                  DK172280          
   27 K4=K4+1                                                           DK172290          
      K3=4999                                                           DK172300          
      DO 92 I=1,NRHM                                                    DK172310          
      K=MATX+NLHM*(NRHM-I)+K2                                           DK172320          
      NEL=K2-1                                                                            
      IB=K3                                                                               
      JB=K                                                                                
      CALL MOVER(ARRAY,K3,K1,IB,JB,NEL,0,0)                                               
      K3=K3-1                                                                             
   92 CONTINUE                                                                            
      GO TO 11                                                          DK172370          
   10 IF (MTY.EQ.7.AND.NRN.EQ.2) GO TO 14                               DK172380          
      GO TO 80                                                          DK172390          
C     ------------------------------------------------------            DK172400          
C     STORE INVERSE ELEMENTS FOR C AND AC CONSTANTS                     DK172410          
C     ------------------------------------------------------            DK172420          
   14 J=NDEN(100)                                                       DK172430          
      K1=NDEN(99)                                                       DK172440          
      NEL=K1-J+1                                                                          
      IB=J                                                                                
      JB=NDEN(98)                                                                         
      CALL MOVER(FAB,I,NDEN(98),IB,JB,NEL,1,1)                                            
      NDEN(98)=NDEN(98)+1                                                                 
      J=NCL(2)-1                                                        DK172480          
      K3=NLHM*J-(J*(J-1))/2                                             DK172490          
      K1=NLHM-J-NDFA*J+NMC(1)                                           DK172500          
      J=5000-K3-(NLHM-IEI+1)*J*NDFA+((J*NDFA)*(J*NDFA-1))/2             DK172510          
      K3=J+K3-1                                                         DK172520          
      K4=1                                                              DK172530          
      DO 15 I=J,K3                                                      DK172540          
      FAB(I)=ARRAY(K4)                                                  DK172550          
   15 K4=K4+1                                                           DK172560          
      K2=IEI-1                                                          DK172570          
C     ------------------------------------------------------            DK172580          
C     STORE C AND AC CONSTANTS                                          DK172590          
C     ------------------------------------------------------            DK172600          
      J=K2*NLHM-(K2*(K2-1))/2+1                                         DK172610          
      K3=K3+1                                                           DK172620          
      DO 75 I=K3,4999                                                   DK172630          
      FAB(I)=ARRAY(J)                                                   DK172640          
   75 J=J+1                                                             DK172650          
      K3=NCL(2)-1                                                       DK172660          
      K1=K3+NDFA*K3-NMC(1)                                              DK172670          
      K3=NDFA*K3-NMC(1)                                                 DK172680          
      K4=4999-NDFA*NRHM                                                 DK172690          
      DO 16 I=1,NRHM                                                    DK172700          
      I2=MATX+NLHM*(NRHM-I)+K2+K3                                       DK172710          
      IB=K4                                                                               
      JB=I2                                                                               
      CALL MOVER(ARRAY,K4,K,IB,JB,K3,0,0)                                                 
      K4=K4-1                                                                             
      NEL=K1-K3                                                                           
      IB=K4                                                                               
      JB=I2-K3-K2+NCL(2)-1                                                                
      CALL MOVER(ARRAY,K4,K,IB,JB,NEL,0,0)                                                
      K4=K4-1                                                                             
   16 CONTINUE                                                                            
   80 IF (MTY.EQ.6.AND.NRN.EQ.2) GO TO 81                               DK172770          
      IF (MTY.EQ.5.AND.NRN.EQ.2) GO TO 94                               DK172780          
      GO TO 11                                                          DK172790          
   81 K=NCL(1)-1                                                        DK172800          
      J=5000-(NLHM+1)*NCL(1)+(NCL(1)*K)/2                                                 
      K4=5000-NLHM-1                                                                      
      NEL=5000-K4                                                                         
      IB=J                                                                                
      JB=K4                                                                               
      CALL MOVER(FAB,J,I,IB,JB,NEL,1,1)                                                   
      J=J+1                                                                               
      K1=1                                                              DK172820          
      DO 82 I=J,4999                                                    DK172830          
      FAB(I)=ARRAY(K1)                                                  DK172840          
   82 K1=K1+1                                                           DK172850          
      K4=4999                                                           DK172860          
      DO 83 I=1,NRHM                                                    DK172870          
      K3=MATX+(NRHM-I+1)*NLHM-(NLHM-K)                                  DK172880          
      IB=K4                                                                               
      JB=K3                                                                               
      CALL MOVER(ARRAY,K4,K1,IB,JB,K,0,0)                                                 
      K4=K4-1                                                                             
   83 CONTINUE                                                                            
   94 K2=NCL(1)                                                         DK172930          
      J=K2                                                              DK172940          
      K4=100                                                            DK172950          
      IB=K4                                                                               
      JB=J                                                                                
      CALL MOVER(IDEN,K4,J,IB,JB,K2,0,0)                                                  
      J=J-1                                                                               
      K4=K4-1                                                                             
   11 IF (LIOP.EQ.20) GO TO 53                                          DK173000          
      IF (MTY.GT.1.AND.NRUN.EQ.NRN) GO TO 53                            DK173010          
      MN2=0                                                             DK173020          
      CALL LSMNS                                                        DK173030          
C     ----------------------------------------------------              DK173040          
C     COMPUTATION AND LISTING, IF DESIRED, OF INVERSE ELEMENTS          DK173050          
C             OF SEGMENTS                                               DK173060          
C     ----------------------------------------------------              DK173070          
   53 IF (MTY.EQ.1.OR.NRN.NE.NRUN) GO TO 85                             DK173080          
      DO 86 I=1,MATX                                                    DK173090          
   86 FAB(I)=ARRAY(I)                                                   DK173100          
   85 NERC=0                                                            DK173110          
      IF (LIOP.LT.3.OR.LIOP.GT.9)  GO TO 221                            DK173120          
      WRITE (6,1000) IJOB                                               DK173130          
 1000 FORMAT (1H0,23X,55HLISTING OF INVERSE ELEMENTS OF SEGMENTS FOR PRODK173140          
     1BLEM NO.,I3)                                                      DK173150          
      WRITE (6,1002)                                                    DK173160          
      WRITE (6,1003)                                                    DK173170          
  221 DO 58 I2=1,NS                                                     DK173180          
      NBRC=NERC+1                                                       DK173190          
      NERC=IM(I2)                                                       DK173200          
      DET=0.0                                                           DK173210          
      CALL MATINV (ARRAY,NBRC,NERC,NLHM,X,DET)                          DK173220          
      IF (LIOP.LT.3.OR.LIOP.GT.9)  GO TO 58                             DK173230          
      DO 220 I=NBRC,NERC                                                DK173240          
      WRITE (6,1004)                                                    DK173250          
      DO 220 J=I,NERC                                                   DK173260          
      K1=NLHM*(I-1)-I*(I-3)/2+J-I                                       DK173270          
      WRITE (6,1005) I,J,LAB1(I),LAB2(I),LAB3(I),LAB4(I),LAB1(J),LAB2(J)DK173280          
     1,LAB3(J),LAB4(J),ARRAY(K1),ARRAY(K1)                              DK173290          
  220 CONTINUE                                                          DK173300          
   58 CONTINUE                                                          DK173310          
C     ----------------------------------------------------              DK173320          
C     COMPUTATION AND LISTING OF RESIDUAL MATRIX FOR RHM                DK173330          
C     ----------------------------------------------------              DK173340          
    8 IF (NCPR.EQ.0.) GO TO 77                                          DK173350          
      WRITE (6,1009)                                                    DK173360          
 1009 FORMAT (1H0,22X,40HRESIDUAL MATRICES FOR RIGHT HAND MEMBERS// 1H ,DK173370          
     127H JOB ROW COL   RHM      RHM,11X,14HERROR SS OR CP,7X,15HERROR MDK173380          
     2S OR COV,5X,11HCORRELATION)                                       DK173390          
      DO 74 K=1,NRHM                                                    DK173400          
      WRITE (6,1004)                                                    DK173410          
      K3=NRHM*(K-1)-K*(K-3)/2                                           DK173420          
      K1=(K-1)*NLHM                                                     DK173430          
      DO 74 L=K,NRHM                                                    DK173440          
      K2=(L-1)*NLHM                                                     DK173450          
      TR=0.0                                                            DK173460          
      IF (NLHM.EQ.0) GO TO 7                                            DK173470          
      DO 64 I=1,NLHM                                                    DK173480          
      K4=K1+I                                                           DK173490          
      K5=MATX+K2+I                                                      DK173500          
   64 TR=TR+ARRAY(K5)*RHM(K4)                                           DK173510          
    7 J=K3+L-K                                                          DK173520          
      ESS=SSCPR(J)-TR                                                   DK173530          
C     ------------------------------------------------------            DK173540          
C     RETAINS SS AND DF FOR RESIDUAL FOR INDIRECT ANALYSIS WHEN RUN     DK173550          
C     NO. IS LESS THAN NO. RUNS                                         DK173560          
C     ------------------------------------------------------            DK173570          
      IF (KB.EQ.0.AND.KD.EQ.0) GO TO 66                                 DK173580          
      IF (KD.EQ.0) GO TO 65                                             DK173590          
      FY(J)=ESS                                                         DK173600          
      EDFF=EDF                                                          DK173610          
      GO TO 66                                                          DK173620          
   65 SAB(J)=ESS                                                        DK173630          
      EDFS=EDF                                                          DK173640          
   66 EMS=ESS/EDF                                                       DK173650          
      SSCPR(J)=EMS                                                      DK173660          
      IF (K.EQ.L) GO TO 72                                              DK173670          
      AK=0.0                                                            DK173680          
      IF (NLHM.EQ.0) GO TO 6                                            DK173690          
      DO 68 I=1,NLHM                                                    DK173700          
      K4=K2+I                                                           DK173710          
      K5=MATX+K4                                                        DK173720          
   68 AK=AK+ARRAY(K5)*RHM(K4)                                           DK173730          
    6 JJ=NRHM*(L-1)-L*(L-3)/2                                           DK173740          
      SM=(SSCPR(JJ)-AK)/EDF                                             DK173750          
      F=EMS/DSQRT(SSCPR(K3)*SM)                                         DK173760          
      GO TO 74                                                          DK173770          
   72 F=1.                                                              DK173780          
   74 WRITE (6,1010) IJOB,K,L,LITY(K),LITY(L),ESS,EMS,F                 DK173790          
 1010 FORMAT (1H ,I4,I3,I4,2X,2(2X,A6),F22.6,F23.8,F14.4)               DK173800          
C     ----------------------------------------------------              DK173810          
C     CALLS POLYNO SUBROUTINE IF POLYNOMIALS ARE TO BE FITTED           DK173820          
C     ----------------------------------------------------              DK173830          
   77 IF (NSME.EQ.0) GO TO 76                                           DK173840          
      CALL POLYNO                                                       DK173850          
C     ----------------------------------------------------              DK173860          
C     COMPUTES AND LISTS SS, MS AND F FOR ANOVA FOR EACH RHM            DK173870          
C     ----------------------------------------------------              DK173880          
   76 IF (NRHM.EQ.0) GO TO 5                                            DK173890          
      CALL LSANOV                                                       DK173900          
C     ----------------------------------------------------              DK173910          
C     COMPUTATION AND LISTING OF K, SS, CP, MS, MCP AND VARIANCE AND    DK173920          
C     COVARIANCE COMPONENTS FOR ONE OR MORE SETS OF MAIN OR NESTED      DK173930          
C     EFFECTS, IF IRAN IS NOT EQUAL TO ZERO                             DK173940          
C     ----------------------------------------------------              DK173950          
    5 MN2=NRHM*(NRHM+1)/2                                               DK173960          
      IF (NPR.EQ.0) GO TO 4                                             DK173970          
      IF (NRN.EQ.NRUN) GO TO 4                                          DK173980          
      DO 3 I=1,NPR                                                      DK173990          
    3 X(I)=XM(I)                                                        DK174000          
    4 IF (IRAN.EQ.0) GO TO 151                                          DK174010          
      WRITE (6,1017)                                                    DK174020          
 1017 FORMAT(1H1,15X,64HVARIANCE AND COVARIANCE COMPONENT ESTIMATES FROM                  
     1 DIRECT ANALYSIS)                                                                   
      KK=0                                                              DK174050          
      LK=NS+1                                                           DK174060          
      DO 150 M=1,IRAN                                                   DK174070          
      IF (NS2(M).EQ.0.OR.NS2(M).GT.2) GO TO 147                         DK174080          
      KK=KK+NS2(M)                                                      DK174090          
      SDF=0.                                                            DK174100          
      AK=0.                                                             DK174110          
      DO 114 J=1,KA                                                     DK174120          
  114 SSS(J)=0.                                                         DK174130          
      IF (NS2(M).EQ.1) GO TO 115                                        DK174140          
      I3=0                                                              DK174150          
  116 I3=I3+1                                                           DK174160          
      IF (IM(I3).EQ.MS(KK)) GO TO 120                                   DK174170          
      IF (I3.EQ.LK) GO TO 147                                           DK174180          
      GO TO 116                                                         DK174190          
  120 L=KK-1                                                            DK174200          
      J=0                                                               DK174210          
  122 J=J+1                                                             DK174220          
      IF (MS(L).LE.IM(J)) GO TO 123                                     DK174230          
      IF (J.EQ.LK) GO TO 147                                            DK174240          
      GO TO 122                                                         DK174250          
  123 IF (J.EQ.1) GO TO 139                                             DK174260          
      IF (IM(J-1).NE.MS(L)-1) GO TO 147                                 DK174270          
  139 DO 126 I2=J,I3                                                    DK174280          
      IF (I2.EQ.1) GO TO 124                                            DK174290          
      NBRC=IM(I2-1)+1                                                   DK174300          
      GO TO 125                                                         DK174310          
  124 NBRC=1                                                            DK174320          
      IF (MS(L).NE.1) GO TO 147                                         DK174330          
  125 NERC=IM(I2)                                                       DK174340          
      DET=0.                                                            DK174350          
      CALL MATINV (ARRAY,NBRC,NERC,NLHM,X,DET)                          DK174360          
  126 CONTINUE                                                          DK174370          
      NBRC=MS(L)                                                        DK174380          
      NERC=MS(KK)                                                       DK174390          
      DET=0.                                                            DK174400          
      CALL MATINV (ARRAY,NBRC,NERC,NLHM,X,DET)                          DK174410          
  135 DO 127 I2=J,I3                                                    DK174420          
      IF (I2.EQ.1) GO TO 128                                            DK174430          
      NBRC1=IM(I2-1)+1                                                  DK174440          
      MRANK=IM(I2)-IM(I2-1)                                             DK174450          
      GO TO 129                                                         DK174460          
  128 MRANK=IM(I2)                                                      DK174470          
      NBRC1=1                                                           DK174480          
  129 NERC1=IM(I2)                                                      DK174490          
      RANK=MRANK                                                        DK174500          
      SOD=0.                                                            DK174510          
      SD=0.                                                             DK174520          
      DO 130 I=NBRC1,NERC1                                              DK174530          
      DO 130 K=I,NERC1                                                  DK174540          
      IF (I-K.LT.0) GO TO 131                                           DK174550          
      L=NLHM*(I-1)-I*(I-3)/2                                            DK174560          
      SD=SD+ARRAY(L)                                                    DK174570          
      GO TO 130                                                         DK174580          
  131 L=NLHM*(I-1)-I*(I-3)/2+K-I                                        DK174590          
      SOD=SOD+ARRAY(L)*2.                                               DK174600          
  130 CONTINUE                                                          DK174610          
      AK=AK+((SD-(SOD/RANK))/(RANK+1.))*RANK                            DK174620          
      SDF=SDF+RANK                                                      DK174630          
  127 CONTINUE                                                          DK174640          
      GO TO 132                                                         DK174650          
  115 I3=0                                                              DK174660          
  133 I3=I3+1                                                           DK174670          
      IF (IM(I3).EQ.MS(KK)) GO TO 134                                   DK174680          
      IF (I3.EQ.LK) GO TO 147                                           DK174690          
      GO TO 133                                                         DK174700          
  134 J=I3                                                              DK174710          
      GO TO 135                                                         DK174720          
  132 IF (NS2(M).EQ.2) GO TO 136                                        DK174730          
      NBRC=NBRC1                                                        DK174740          
      NERC=NERC1                                                        DK174750          
  136 DO 142 K=1,NRHM                                                   DK174760          
      DO 142 I2=K,NRHM                                                  DK174770          
      SUM=0.0                                                           DK174780          
      DO 140 I=NBRC,NERC                                                DK174790          
      N3=MATX+(K-1)*NLHM+I                                              DK174800          
      DO 140 J=NBRC,NERC                                                DK174810          
      IF (I-J.LE.0) GO TO 137                                           DK174820          
      L=NLHM*(J-1)-J*(J-3)/2+I-J                                        DK174830          
      GO TO 138                                                         DK174840          
  137 L=NLHM*(I-1)-I*(I-3)/2+J-I                                        DK174850          
  138 N2=MATX+(I2-1)*NLHM+J                                             DK174860          
  140 SUM=SUM+ARRAY(N3)*ARRAY(L)*ARRAY(N2)                              DK174870          
      L2=NRHM*(K-1)-K*(K-3)/2+I2-K                                      DK174880          
  142 SSS(L2)=SSS(L2)+SUM                                               DK174890          
      WK=AK/SDF                                                         DK174900          
      K=101-M                                                           DK174910          
      WRITE (6,1004)                                                    DK174920          
      WRITE (6,1018) LAB4(K),WK,SDF                                     DK174930          
 1018 FORMAT(1H0,10X,32HK FOR RANDOM EFFECTS COMPONENT (,A6,                              
     1 2H)=,F8.4,3X,20HDEGREES OF FREEDOM =,F5.0)                                         
      WRITE (6,1019)                                                    DK174960          
 1019 FORMAT (1H0/25X,51HSS, CP, MS, MCP, VARIANCE AND COVARIANCE COMPONDK174970          
     1ENTS//1H ,25HJOB ROW COL   RHM     RHM,20X,8HSS OR CP,19X,9HMS OR DK174980          
     2COV,18X,10HCOMPONENTS)                                            DK174990          
      DO 146 I=1,NRHM                                                   DK175000          
      WRITE (6,1004)                                                    DK175010          
      DO 146 J=I,NRHM                                                   DK175020          
      K=NRHM*(I-1)-I*(I-3)/2+J-I                                        DK175030          
      SMS=SSS(K)/SDF                                                    DK175040          
      SOD=(SMS-SSCPR(K))/WK                                             DK175050          
      WRITE (6,1020) IJOB,I,J,LITY(I),LITY(J),SSS(K),SMS,SOD            DK175060          
 1020 FORMAT (1H ,I3,2I4,2(2X,A6),3F27.8)                               DK175070          
      IF (I.EQ.J.AND.SOD.LT.0.0) SOD=0.0                                DK175080          
  146 SSS(K)=SOD                                                        DK175090          
C     ----------------------------------------------------              DK175100          
C     CALLS SVCVC SUBROUTINE TO COMPUTE HERITABILITIES, GENETIC         DK175110          
C     CORRELATIONS, ETC., IF I309 IS NOT EQUAL TO ZERO                  DK175120          
C     ----------------------------------------------------              DK175130          
      IF (I309(M).EQ.0) GO TO 150                                       DK175140          
      L=M                                                               DK175150          
      CALL SVCVC                                                        DK175160          
      GO TO 150                                                         DK175170          
  147 K=101-M                                                           DK175180          
      WRITE (6,1023) M,LAB4(K)                                          DK175190          
 1023 FORMAT(33H0ERROR IN IRAN PARAMETER CARD NO.,I5,4X,A6)                               
  150 CONTINUE                                                          DK175210          
  151 RETURN                                                            DK175220          
      END                                                               DK175230          
