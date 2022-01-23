      SUBROUTINE RCBM                                                   DK050010          
C     -------------------------------------------                       DK050020          
C     SUBROUTINE WHICH READS DATA CARDS, CALLS CODEX, COMPUTES LS OR ML DK050030          
C     MATRICES, CALLS SCLNOS AND LISTS CERTAIN MATRICES, MEANS, SD AND  DK050040          
C     CORRELATIONS                                                      DK050050          
C     ----------------------------------------------------              DK050060          
      EXTERNAL DSQRT                                                                      
      DIMENSION ARRAY(5000),SSCPR(630),SSS(630),RHM(2450),TOT(106),TOT2(DK050090          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100),NS2(15)    DK050100          
      DIMENSION FAB(5000),FY(630),TOT4(106),SAB(630),FSQ(525),TOT5(106) DK050110          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),MZ(10),IPL(13),NSDK050120          
     1P(13),NND(13),XP(81),YP(41),      NDC(10),NMI(10),MEN(20),NCL(20),DK050130          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(50),NCLN(50),LNE(50),NDEN(1                  
     300),NLIT(50),NMA(50),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(50),                  
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDK050160          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),                          
     6EFF1(50),EFF2(50),NOS(300),X(106),NMAC(50),I309(15),NR1(15),NW(15)                  
     7,R1I(100),R2I(100),WW(200),ICLR(90),IRM(40),LAD(90)               DK050190          
      DIMENSION IC(1000)                                                                  
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADK050200          
     1B4,LITY,TRED,YM,XP,YP,IM,MS,NEG,IPL,NSP,NND,FAB,FY,TOT4,SAB,FSQ,TODK050210          
     2T5,MZ,R1I,R2I,WW,ICLR,IRM,LAD                                     DK050220          
      COMMON /CMBLK2/LIT,LITR,NLIT,XM,EFF1,EFF2,X,NDC,NMI,MEN,NCL,LME,IBDK050230          
     1EG,IDEN,NCLN,NEN,LNE,NBEG,NDEN,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQDK050240          
     2C,NREGP,LGTX,JBEG,NDECX,NEGY,LNY,LHY,KBEG,NDECY,IC,NOS,NMAC       DK050250          
      COMMON /CMBLK3/RR,REP,DF,SDF,EDF,WK,IJOB,NAB,NLHM,NRHM,NMEA,NME,NNDK050260          
     1EA,NNE,N2F,NPR,IRAN,MPOP,LIOP,IAD,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,DK050270          
     2NOM,NON,ML,MLB,NS,NOT,KPUT,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,IN,I30DK050280          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DK050290          
     1L,L7,KA,KB,KC,SOF,NX,KD,MTY,MAN,NRUN,NRN,EDFF,NAME,EDFS,NAM5,IFP,NDK050300          
     2DFA,IFPC,NDFC,NDFAC,NINT,IBET,LBEG,LTHN,NNDC,WTT,SWT1             DK050310          
      INTEGER RGRSN                                                                       
      COMMON /CMBLK5/ LREC,LRECW,LAST                                                     
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
      DATA RGRSN/6HRGRSN /                                                                
      IF (IN.NE.5) REWIND IN                                            DK050330          
      IF (NPR.EQ.0.OR.NRN.GT.1) GO TO 10                                DK050340          
      K=0                                                               DK050350          
      K1=0                                                              DK050360          
      DO 12 I=1,NPR                                                     DK050370          
      IF (LAD(I).NE.3) GO TO 12                                         DK050380          
      IF (IN.NE.5) GO TO 26                                             DK050390          
      K1=1                                                              DK050400          
      GO TO 12                                                          DK050410          
   26 K=1                                                               DK050420          
      XM(I)=0.0                                                         DK050430          
   12 CONTINUE                                                          DK050440          
      IF (K1.EQ.0) GO TO 13                                             DK050450          
      WRITE (6,1000)                                                    DK050460          
 1000 FORMAT(84H0MEANS FOR X VARIABLES CANNOT BE COMPUTED BEFORE ANALYSI                  
     1S. ADJUSTMENT IS MADE TO XM./)                                                      
      GO TO 10                                                          DK050490          
   13 IF (K.EQ.0) GO TO 10                                              DK050500          
      NCDS=0                                                            DK050510          
      SWT1=0.0                                                          DK050520          
      CALL FCARD(IN,K,L,IC,IDUM,IFLAG)                                                    
      IF(IFLAG) 902,25,902                                                                
   25 K=1                                                               DK050530          
      L=LREC                                                                              
      CALL RCARD(IN,K,L,IC,IDUM,IFLAG)                                                    
      IF(IFLAG) 14,600,902                                                                
  600 CONTINUE                                                                            
      NCDS=NCDS+1                                                       DK050560          
      IF (NCD.LT.2) GO TO 16                                            DK050570          
      DO 19 I=2,NCD                                                     DK050580          
      L=I*LREC                                                                            
      K=L-LREC+1                                                                          
      CALL RCARD(IN,K,L,IC,I,IFLAG)                                                       
      IF(IFLAG) 22,22,902                                                                 
   22 K1=ICN1+K-1                                                       DK050620          
      K2=K1-LREC                                                                          
      IF(IJCHCM(INST(IC,K1),INST(IC,K2))) 901,901,19                                      
   19 CONTINUE                                                          DK050650          
   16 IF (IBET.EQ.0) GO TO 23                                           DK050660          
      L7=LBEG                                                           DK050670          
      L=LTHN+L7-1                                                       DK050680          
      J=NCDS                                                            DK050690          
      CALL FIELD (ICOD,L,IC,L7,J)                                       DK050700          
      WTT=ICOD                                                          DK050710          
      WTT=WTT/(10.**NNDC)                                               DK050720          
      SWT1=SWT1+WTT                                                     DK050730          
   23 DO 15 I=1,NPR                                                     DK050740          
      IF (LAD(I).NE.3) GO TO 15                                         DK050750          
      L7=JBEG(I)                                                        DK050760          
      L=LGTX(I)+L7-1                                                    DK050770          
      J=NCDS                                                            DK050780          
      CALL FIELD (ICOD,L,IC,L7,J)                                       DK050790          
      XR=ICOD                                                           DK050800          
      XR=XR/(10.**NDECX(I))                                             DK050810          
      IF(LOGE(I).EQ.1.AND.XR.LE.0) GO TO 640                                              
      IF(LOGE(I).EQ.2.AND.XR.LT.0) GO TO 640                                              
      IF(LOGE(I).EQ.1) XR=ALOG10(XR)                                                      
      IF(LOGE(I).EQ.2) XR=SQRT(XR)                                                        
      IF (IBET.EQ.1) XR=WTT*XR                                          DK050820          
      XM(I)=XM(I)+XR                                                    DK050830          
   15 CONTINUE                                                          DK050840          
      GO TO 25                                                          DK050850          
   14 DO 24 I=1,NPR                                                     DK050860          
      IF (LAD(I).NE.3) GO TO 24                                         DK050870          
      IF (IBET.EQ.0) SWT1=NCDS                                          DK050880          
      XM(I)=XM(I)/SWT1                                                  DK050890          
      IF (MTY.EQ.1) WRITE (6,1003) I,XM(I)                              DK050900          
 1003 FORMAT(1H0,10X,3HXM(,I2,2H)=,F15.5)                                                 
   24 CONTINUE                                                          DK050920          
      REWIND IN                                                         DK050930          
   10 IF (LIOP.EQ.1.OR.LIOP.EQ.3) GO TO 1                               DK050940          
      GO TO 2                                                           DK050950          
  640 WRITE(6,1120) I,XR                                                                  
 1120 FORMAT(11H0LOGE(I) = ,I1,9H FOR I = ,I2,9H AND X = ,F13.5)                          
      GO TO 902                                                                           
    1 WRITE (6,1001) IJOB                                               DK050960          
 1001 FORMAT (1H1,20X,35HLISTING OF X MATRIX FOR PROBLEM NO.,I3)        DK050970          
    2 MATX=NLHM*(NLHM+1)/2                                              DK050980          
      DO 4 I=1,MATX                                                     DK050990          
    4 ARRAY(I)=0.0                                                      DK051000          
      K1=NLHM*NRHM                                                      DK051010          
      DO 5 I=1,K1                                                       DK051020          
    5 RHM(I)=0.0                                                        DK051030          
      MIN=0                                                             DK051040          
      IF (NCPR.EQ.0) GO TO 6                                            DK051050          
      KA=NRHM*(NRHM+1)/2                                                DK051060          
      IF (NAB.EQ.2) MIN=1                                               DK051070          
      GO TO 7                                                           DK051080          
    6 KA=NRHM                                                           DK051090          
    7 IF (NAB.EQ.5.OR.NAB.EQ.6) GO TO 3                                 DK051100          
      GO TO 11                                                          DK051110          
    3 CALL MIXED (NAB,MATX,KA,KB,KC,NOT,TOT,NMIG,NLHM,NCPR,NRHM,TOT3,ARRDK051120          
     1AY,NCDG,FAB,TOT4,SOF,NX,KD,IBET,SWT2,SWT3,WTT,SOFN)               DK051130          
      IF ((MTY.EQ.4.OR.MTY.EQ.5).AND.NRN.EQ.1) KD=2                     DK051140          
   11 DO 8 I=1,KA                                                       DK051150          
    8 SSCPR(I)=0.0                                                      DK051160          
      NOT=NLHM+NRHM                                                     DK051170          
      DO 9 I=1,NOT                                                      DK051180          
      TOT(I)=0.0                                                        DK051190          
      TOT2(I)=0.0                                                       DK051200          
    9 TOT3(I)=0.0                                                       DK051210          
      K1=0                                                              DK051220          
      NCDS=0                                                            DK051230          
      SSUM=0.                                                           DK051240          
      SWT1=0.                                                           DK051250          
      SWT2=0.                                                           DK051260          
      SWT4=0.                                                           DK051270          
      SNSM=0.                                                           DK051280          
      LL=1                                                              DK051290          
      NSM=0                                                             DK051300          
      NSUM=0                                                            DK051310          
      GNI=0.                                                            DK051320          
      MJN=0                                                             DK051330          
      NCDG=0                                                            DK051340          
      NMIG=1                                                            DK051350          
      CALL FCARD(IN,K,L,IC,IDUM,IFLAG)                                                    
      IF(IFLAG) 902,630,902                                                               
  630 CONTINUE                                                                            
      IF (MTY.GT.5.AND.NRN.EQ.NRUN) GO TO 227                           DK051360          
      GO TO 228                                                         DK051370          
  227 DO 229 I=1,99                                                     DK051380          
  229 TOT4(I)=R2I(I)                                                    DK051390          
  228 IF (KB.EQ.1.AND.MTY.GT.5) GO TO 225                               DK051400          
      GO TO 20                                                          DK051410          
  225 DO 226 I=1,100                                                    DK051420          
      R1I(I)=0.0                                                        DK051430          
  226 R2I(I)=0.0                                                        DK051440          
   20 K=1                                                               DK051450          
      NAB2=0                                                            DK051460          
      IF (NAB.EQ.2.AND.(MTY.EQ.3.OR.MTY.EQ.5.OR.MTY.EQ.7)) NAB2=1       DK051470          
      L=LREC                                                                              
      CALL RCARD(IN,K,L,IC,IDUM,IFLAG)                                                    
      IF(IFLAG) 17,601,902                                                                
  601 CONTINUE                                                                            
      NTRA=0                                                            DK051500          
      DO 21 I=1,LREC                                                                      
      IF(LETTER(INST(IC,I))) 21,603,603                                                   
  603 NTRA=1                                                                              
      GO TO 18                                                                            
   21 CONTINUE                                                                            
      GO TO 18                                                          DK051530          
   17 NTRA=0                                                            DK051540          
   18 IF (NTRA.EQ.0) GO TO 156                                          DK051550          
      NCDS=NCDS+1                                                       DK051560          
      IF (NCD.LT.2) GO TO 32                                            DK051570          
      DO 30 J=2,NCD                                                     DK051580          
      L=J*LREC                                                                            
      K=L-LREC+1                                                                          
      CALL RCARD(IN,K,L,IC,J,IFLAG)                                                       
      IF(IFLAG) 9999,9999,902                                                             
 9999 K1=ICN1+K-1                                                       DK051620          
      K2=K1-LREC                                                                          
      IF(IJCHCM(INST(IC,K1),INST(IC,K2))) 901,901,30                                      
   30 CONTINUE                                                          DK051650          
   32 CONTINUE                                                          DK051660          
      IF (IBET.EQ.0) GO TO 35                                           DK051670          
      L7=LBEG                                                           DK051680          
      L=LTHN+L7-1                                                       DK051690          
      J=NCDS                                                            DK051700          
      CALL FIELD(ICOD,L,IC,L7,J)                                        DK051710          
      WTT=ICOD                                                          DK051720          
      WTT=WTT/(10.**NNDC)                                               DK051730          
   35 IF (NAB.GT.1.OR.KD.GT.0) GO TO 100                                DK051740          
   36 CONTINUE                                                          DK051750          
      CALL XMAT                                                         DK051760          
      IF (MULL.EQ.1) RETURN                                             DK051770          
      IF (NCDS.EQ.1) SWT3=WTT                                           DK051780          
      SWT1=SWT1+WTT                                                     DK051790          
      IF (NCDS.EQ.1) SWT4=WTT                                           DK051800          
      IF (LIOP.EQ.1.OR.LIOP.EQ.3) GO TO 41                              DK051810          
      GO TO 39                                                          DK051820          
   41 WRITE (6,1002) (X(I),I=1,NOT)                                     DK051830          
 1002 FORMAT (1H ,17F7.2)                                               DK051840          
   39 IF (NLHM.EQ.0) GO TO 47                                           DK051850          
   42 DO 46 I=1,NLHM                                                    DK051860          
      IF (X(I).EQ.0.0) GO TO 46                                         DK051870          
      DO 45 J=I,NLHM                                                    DK051880          
      K=NLHM*(I-1)-I*(I-3)/2+J-I                                        DK051890          
      IF (IBET.EQ.0) GO TO 48                                           DK051900          
      ARRAY(K)=ARRAY(K)+(X(I)*X(J))/WTT                                 DK051910          
      GO TO 45                                                          DK051920          
   48 ARRAY(K)=ARRAY(K)+X(I)*X(J)                                       DK051930          
   45 CONTINUE                                                          DK051940          
   46 CONTINUE                                                          DK051950          
   47 DO 50 I=1,NOT                                                     DK051960          
      IF (KB.EQ.0.AND.KD.EQ.0) GO TO 50                                 DK051970          
      TOT3(I)=TOT3(I)+X(I)                                              DK051980          
   50 TOT(I)=TOT(I)+X(I)                                                DK051990          
      IF (NRHM.EQ.0) GO TO 20                                           DK052000          
      IF (NCPR.EQ.0) GO TO 56                                           DK052010          
      DO 55 I=1,NRHM                                                    DK052020          
      DO 55 J=I,NRHM                                                    DK052030          
      K=NRHM*(I-1)-I*(I-3)/2+J-I                                        DK052040          
      K2=NLHM+I                                                         DK052050          
      K3=NLHM+J                                                         DK052060          
      IF (IBET.EQ.0) GO TO 44                                           DK052070          
      SSCPR(K)=SSCPR(K)+(X(K2)*X(K3))/WTT                               DK052080          
      GO TO 55                                                          DK052090          
   44 SSCPR(K)=SSCPR(K)+X(K2)*X(K3)                                     DK052100          
   55 CONTINUE                                                          DK052110          
      GO TO 58                                                          DK052120          
   56 CONTINUE                                                          DK052130          
      DO 57 I=1,NRHM                                                    DK052140          
      K2=NLHM+I                                                         DK052150          
      IF (IBET.EQ.0) GO TO 43                                           DK052160          
      SSCPR(I)=SSCPR(I)+(X(K2)*X(K2))/WTT                               DK052170          
      GO TO 57                                                          DK052180          
   43 SSCPR(I)=SSCPR(I)+X(K2)*X(K2)                                     DK052190          
   57 CONTINUE                                                          DK052200          
   58 K=0                                                               DK052210          
      DO 59 I=1,NRHM                                                    DK052220          
      J2=NLHM+I                                                         DK052230          
      DO 59 J=1,NLHM                                                    DK052240          
      K=K+1                                                             DK052250          
      IF (IBET.EQ.0) GO TO 49                                           DK052260          
      RHM(K)=RHM(K)+(X(J)*X(J2))/WTT                                    DK052270          
      GO TO 59                                                          DK052280          
   49 RHM(K)=RHM(K)+X(J)*X(J2)                                          DK052290          
   59 CONTINUE                                                          DK052300          
      GO TO 20                                                          DK052310          
C     ----------------------------------------------------              DK052320          
C     ABSORPTION PROCESS                                                DK052330          
C     ----------------------------------------------------              DK052340          
  100 CONTINUE                                                          DK052350          
      IF (NAB.LT.4.AND.KB.EQ.0) GO TO 110                               DK052360          
      IMJ1=0                                                            DK052370          
      DO 102 I=1,NMJC                                                   DK052380          
      K=NDC(I)                                                          DK052390          
      IF(LETTER(INST(IC,K))) 610,620,620                                                  
  610 CONTINUE                                                                            
      IF (I.EQ.NMJC) GO TO 900                                          DK052410          
      CALL OUT(IC,K,INST(KONST(28),MACHC))                                                
  620 CALL COPYC(IC,K,J,1,1)                                                              
  102 IMJ1=LNUM(J,1)+IMJ1*10                                                              
      IMI1=0                                                            DK052440          
      DO 104 I=1,NMIC                                                   DK052450          
      K=NMI(I)                                                          DK052460          
      IF(LETTER(INST(IC,K))) 611,621,621                                                  
  611 CONTINUE                                                                            
      IF (I.EQ.NMIC) GO TO 900                                          DK052480          
      CALL OUT(IC,K,INST(KONST(28),MACHC))                                                
  621 CALL COPYC(IC,K,J,1,1)                                                              
  104 IMI1=LNUM(J,1)+IMI1*10                                                              
      IF (NCDS.EQ.1) GO TO 108                                          DK052510          
      IF (IMJ2-IMJ1) 164,107,901                                        DK052520          
  107 IF (IMI2-IMI1) 164,106,901                                        DK052530          
  106 NMIG=NMIG+1                                                       DK052540          
      SWT3=SWT3+WTT                                                     DK052550          
  108 IMI2=IMI1                                                         DK052560          
  109 IMJ2=IMJ1                                                         DK052570          
      GO TO 36                                                          DK052580          
  110 IMJ1=0                                                            DK052590          
      DO 112 I=1,NCC                                                    DK052600          
      K=NDC(I)                                                          DK052610          
      IF(LETTER(INST(IC,K))) 612,622,622                                                  
  612 CONTINUE                                                                            
      IF (I.EQ.NCC) GO TO 900                                           DK052630          
      CALL OUT(IC,K,INST(KONST(28),MACHC))                                                
  622 CALL COPYC(IC,K,J,1,1)                                                              
  112 IMJ1=LNUM(J,1)+IMJ1*10                                                              
      IF (NAB.EQ.2) GO TO 201                                           DK052660          
      IF (NCDS.EQ.1) GO TO 114                                          DK052670          
      IF (IMJ2-IMJ1) 116,114,901                                        DK052680          
  114 NCDG=NCDG+1                                                       DK052690          
      SWT2=SWT2+WTT                                                     DK052700          
      GO TO 109                                                         DK052710          
  201 IIN1=0                                                            DK052720          
      K2=0                                                              DK052730          
      IF (NINT.EQ.0) GO TO 203                                          DK052740          
      DO 202 I=1,NINT                                                   DK052750          
      K=MZ(I)                                                           DK052760          
      IF(LETTER(INST(IC,K))) 613,623,623                                                  
  613 CONTINUE                                                                            
      IF (I.EQ.NINT) GO TO 900                                          DK052780          
      CALL OUT(IC,K,INST(KONST(28),MACHC))                                                
  623 CALL COPYC(IC,K,J,1,1)                                                              
  202 IIN1=LNUM(J,1)+IIN1*10                                                              
      IF (MTY.EQ.3.AND.NCDS.EQ.1) GO TO 212                             DK052810          
      IMI1=0                                                            DK052820          
      IF (MTY.EQ.3.OR.NMIC.EQ.0) GO TO 203                              DK052830          
      DO 204 I=1,NMIC                                                   DK052840          
      K=NMI(I)                                                          DK052850          
      IF(LETTER(INST(IC,K))) 614,624,624                                                  
  614 CONTINUE                                                                            
      IF (I.EQ.NMIC) GO TO 900                                          DK052870          
      CALL OUT(IC,K,INST(KONST(28),MACHC))                                                
  624 CALL COPYC(IC,K,J,1,1)                                                              
  204 IMI1=LNUM(J,1)+IMI1*10                                                              
  203 IF (NCDS.EQ.1) GO TO 213                                          DK052900          
      IF (IMJ2-IMJ1) 206,207,901                                        DK052910          
  207 NMIG=NMIG+1                                                       DK052920          
      SWT3=SWT3+WTT                                                     DK052930          
      IF (NINT.EQ.0) GO TO 212                                          DK052940          
      IF (IIN2-IIN1) 206,209,901                                        DK052950          
  209 MIN=MIN+1                                                         DK052960          
      SWT4=SWT4+WTT                                                     DK052970          
      IF (MTY.EQ.3.OR.NMIC.EQ.0) GO TO 212                              DK052980          
      IF (IMI2-IMI1) 206,213,901                                        DK052990          
  213 IMI2=IMI1                                                         DK053000          
  212 IIN2=IIN1                                                         DK053010          
      IF (K2.EQ.1) GO TO 116                                            DK053020          
      GO TO 114                                                         DK053030          
  206 IF (NAB2.EQ.0) GO TO 217                                          DK053040          
      NSUM=NSUM+NCDG*NCDG                                               DK053050          
      SSUM=SSUM+SWT2*SWT2                                               DK053060          
      IF (IMJ2.LT.IMJ1.OR.IIN2.LT.IIN1) GO TO 214                       DK053070          
  217 K2=1                                                                                
      GO TO 213                                                         DK053090          
  214 NSM=NSM+MIN*MIN                                                   DK053120          
      SNSM=SNSM+SWT4*SWT4                                               DK053130          
      SWT4=WTT                                                          DK053140          
      MIN=1                                                             DK053150          
      IF (IMJ2.LT.IMJ1) GO TO 215                                       DK053160          
      K2=1                                                              DK053170          
      GO TO 213                                                         DK053180          
  215 R1I(LL)=FLOAT(NSM)/FLOAT(NMIG)                                    DK053190          
      R2I(LL)=FLOAT(NSUM)/FLOAT(NMIG)                                   DK053200          
      IF (IBET.EQ.1) R1I(LL)=SNSM/SWT3                                  DK053210          
      IF (IBET.EQ.1) R2I(LL)=SSUM/SWT3                                  DK053220          
      SWT3=WTT                                                          DK053230          
      NMIG=1                                                            DK053240          
      NSM=0                                                             DK053250          
      NSUM=0                                                            DK053260          
      SSUM=0.                                                           DK053270          
      SNSM=0.                                                           DK053280          
      LL=LL+1                                                           DK053290          
      K2=1                                                              DK053300          
      GO TO 213                                                         DK053310          
  116 DM=NCDG                                                           DK053320          
      IF (IBET.EQ.1) DM=SWT2                                            DK053330          
      IF (KD.EQ.0) GO TO 115                                            DK053340          
      KC=1                                                              DK053350          
      NMIG=NCDG                                                         DK053360          
      SWT3=SWT2                                                         DK053370          
      CALL MIXED (NAB,MATX,KA,KB,KC,NOT,TOT,NMIG,NLHM,NCPR,NRHM,TOT3,ARRDK053380          
     1AY,NCDG,FAB,TOT4,SOF,NX,KD,IBET,SWT2,SWT3,WTT,SOFN)               DK053390          
      GO TO 109                                                         DK053400          
  115 IF (NAB.EQ.2) GO TO 118                                           DK053410          
      ROWN=1./(DM+REP)                                                  DK053420          
      GO TO 120                                                         DK053430          
  118 ROWN=1./DM                                                        DK053440          
  120 IF (NLHM.EQ.0) GO TO 130                                          DK053450          
      DO 124 I=1,NLHM                                                   DK053460          
      IF (TOT(I).EQ.0.0) GO TO 124                                      DK053470          
      DO 122 J=I,NLHM                                                   DK053480          
      K=NLHM*(I-1)-I*(I-3)/2+J-I                                        DK053490          
  122 ARRAY(K)=ARRAY(K)-TOT(I)*TOT(J)*ROWN                              DK053500          
  124 CONTINUE                                                          DK053510          
  130 IF (LIOP.EQ.0.OR.LIOP.EQ.5.OR.LIOP.GT.9)  GO TO 135               DK053520          
      IF (NAB.NE.2) IIN2=0                                              DK053530          
      IF (NAB.EQ.0.OR.NAB.EQ.1.OR.NAB.EQ.3) IMI2=0                      DK053540          
      IF (NAB.EQ.4) GO TO 132                                           DK053550          
      WRITE (6,1050) IMJ2,IIN2,IMI2,NCDG                                DK053560          
 1050 FORMAT (1H ,10X,14HMINOR CLASS IS,3I10,5H  NI=,I4/1H ,6HTOTALS)   DK053570          
      GO TO 134                                                         DK053580          
  132 WRITE (6,1050) IMJ2,IMI2,IIN2,NMIG                                DK053590          
  134 WRITE (6,1052) (TOT(I),I=1,NOT)                                   DK053600          
  135 IF (NRHM.EQ.0) GO TO 146                                          DK053610          
      IF (NCPR.EQ.0) GO TO 138                                          DK053620          
      DO 136 I=1,NRHM                                                   DK053630          
      DO 136 J=I,NRHM                                                   DK053640          
      K=NRHM*(I-1)-I*(I-3)/2+J-I                                        DK053650          
      K2=NLHM+I                                                         DK053660          
      K3=NLHM+J                                                         DK053670          
  136 SSCPR(K)=SSCPR(K)-TOT(K2)*TOT(K3)*ROWN                            DK053680          
      GO TO 142                                                         DK053690          
  138 DO 140 I=1,NRHM                                                   DK053700          
      K2=NLHM+I                                                         DK053710          
  140 SSCPR(I)=SSCPR(I)-TOT(K2)*TOT(K2)*ROWN                            DK053720          
  142 IF (NLHM.EQ.0) GO TO 146                                          DK053730          
      K=0                                                               DK053740          
      DO 144 I=1,NRHM                                                   DK053750          
      J2=NLHM+I                                                         DK053760          
      DO 144 J=1,NLHM                                                   DK053770          
      K=K+1                                                             DK053780          
  144 RHM(K)=RHM(K)-TOT(J)*TOT(J2)*ROWN                                 DK053790          
  146 IF (NAB.EQ.1) GO TO 299                                           DK053800          
      DO 148 I=1,NOT                                                    DK053810          
      IF (KB.EQ.1) GO TO 148                                            DK053820          
      TOT3(I)=TOT3(I)+TOT(I)                                            DK053830          
  148 TOT2(I)=TOT2(I)+TOT(I)                                            DK053840          
      IF (NAB.NE.4) GO TO 152                                           DK053850          
      DO 150 I=1,NOT                                                    DK053860          
      TOT2(I)=TOT2(I)-DM*TOT(I)*ROWN                                    DK053870          
  150 TOT(I)=0.0                                                        DK053880          
      GNI=GNI-DM*DM*ROWN                                                DK053890          
      GO TO 166                                                         DK053900          
  152 MJN=MJN+1                                                         DK053910          
      DO 154 I=1,NOT                                                    DK053920          
  154 TOT(I)=0.0                                                        DK053930          
      NCDG=1                                                            DK053940          
      SWT2=WTT                                                          DK053950          
      IF (KB.EQ.1) SWT2=0.0                                             DK053960          
      IF (KB.EQ.1) NCDG=0                                               DK053970          
      IF (NTRA.EQ.0) GO TO 193                                          DK053980          
      IF (KB.EQ.1) GO TO 108                                            DK053990          
      GO TO 109                                                         DK054000          
  156 IF (KB.EQ.1) GO TO 158                                            DK054010          
      IF (NAB.EQ.2.AND.(MTY.EQ.3.OR.MTY.EQ.5.OR.MTY.EQ.7)) GO TO 216    DK054020          
      IF (NAB.NE.0) GO TO 158                                           DK054030          
      DF=NCDS                                                           DK054040          
      GO TO 299                                                         DK054050          
  216 NSUM=NSUM+NCDG*NCDG                                               DK054060          
      SSUM=SSUM+SWT2*SWT2                                               DK054070          
      SNSM=SNSM+SWT4*SWT4                                               DK054080          
      NSM=NSM+MIN*MIN                                                   DK054090          
      R1I(LL)=FLOAT(NSM)/FLOAT(NMIG)                                    DK054100          
      R2I(LL)=FLOAT(NSUM)/FLOAT(NMIG)                                   DK054110          
      IF (IBET.EQ.1) R1I(LL)=SNSM/SWT3                                  DK054120          
      IF (IBET.EQ.1) R2I(LL)=SSUM/SWT3                                  DK054130          
  158 IF (NAB.NE.1) GO TO 160                                           DK054140          
      DF=NCDS-1                                                         DK054150          
      GO TO 162                                                         DK054160          
  160 IF (KB.NE.1) GO TO 161                                            DK054170          
      NCDG=NCDG+NMIG                                                    DK054180          
      SWT2=SWT2+SWT3                                                    DK054190          
      IF (MTY.LT.6) GO TO 230                                           DK054200          
      K=NCL(1)                                                          DK054210          
      IF (MTY.EQ.7) K=NCL(2)                                            DK054220          
      DO 220 I=1,K                                                      DK054230          
      R2I(I)=R2I(I)+R1I(I)*R1I(I)                                       DK054240          
      IF (IBET.EQ.0) GO TO 219                                          DK054250          
      R2I(I)=R2I(I)/WW(I)                                               DK054260          
      GO TO 220                                                         DK054270          
  219 R2I(I)=R2I(I)/FLOAT(NOS(I))                                       DK054280          
  220 R1I(I)=0.0                                                        DK054290          
  230 KC=4                                                              DK054300          
      CALL MIXED (NAB,MATX,KA,KB,KC,NOT,TOT,NMIG,NLHM,NCPR,NRHM,TOT3,ARRDK054310          
     1AY,NCDG,FAB,TOT4,SOF,NX,KD,IBET,SWT2,SWT3,WTT,SOFN)               DK054320          
  161 IF (NAB.LT.4) GO TO 116                                           DK054330          
      GO TO 164                                                         DK054340          
  162 DM=NCDS                                                           DK054350          
      IF (IBET.EQ.1) DM=SWT1                                            DK054360          
      ROWN=1./DM                                                        DK054370          
      GO TO 120                                                         DK054380          
  164 NCDG=NCDG+NMIG                                                    DK054390          
      SWT2=SWT2+SWT3                                                    DK054400          
      IF (KB.NE.1) GO TO 163                                            DK054410          
      IF (MTY.LT.6) GO TO 231                                           DK054420          
      K=NCL(1)                                                          DK054430          
      IF (MTY.EQ.7) K=NCL(2)                                            DK054440          
      DO 221 I=1,K                                                      DK054450          
      R2I(I)=R2I(I)+R1I(I)*R1I(I)                                       DK054460          
  221 R1I(I)=0.0                                                        DK054470          
  231 KC=1                                                              DK054480          
      CALL MIXED (NAB,MATX,KA,KB,KC,NOT,TOT,NMIG,NLHM,NCPR,NRHM,TOT3,ARRDK054490          
     1AY,NCDG,FAB,TOT4,SOF,NX,KD,IBET,SWT2,SWT3,WTT,SOFN)               DK054500          
      IF (IMJ2.EQ.IMJ1) GO TO 108                                       DK054510          
      KC=2                                                              DK054520          
      CALL MIXED (NAB,MATX,KA,KB,KC,NOT,TOT,NMIG,NLHM,NCPR,NRHM,TOT3,ARRDK054530          
     1AY,NCDG,FAB,TOT4,SOF,NX,KD,IBET,SWT2,SWT3,WTT,SOFN)               DK054540          
      GO TO 116                                                         DK054550          
  163 DM=NMIG                                                           DK054560          
      IF (IBET.EQ.1) DM=SWT3                                            DK054570          
      SWT2=SWT2+SWT3                                                    DK054580          
      NCDG=NCDG+NMIG                                                    DK054590          
      ROWN=1./(DM+REP)                                                  DK054600          
      GNI=GNI+DM                                                        DK054610          
      GO TO 120                                                         DK054620          
  166 MIN=MIN+1                                                         DK054630          
      NMIG=1                                                            DK054640          
      SWT3=WTT                                                          DK054650          
      IF (NTRA.EQ.0.OR.IMJ1.NE.IMJ2) GO TO 168                          DK054660          
      GO TO 108                                                         DK054670          
  168 ROWN=1./GNI                                                       DK054680          
      IF (NLHM.EQ.0) GO TO 172                                          DK054690          
      DO 170 I=1,NLHM                                                   DK054700          
      DO 170 J=I,NLHM                                                   DK054710          
      K=NLHM*(I-1)-I*(I-3)/2+J-I                                        DK054720          
  170 ARRAY(K)=ARRAY(K)-TOT2(I)*TOT2(J)*ROWN                            DK054730          
  172 IF (NRHM.EQ.0) GO TO 184                                          DK054740          
      IF (NCPR.EQ.0) GO TO 176                                          DK054750          
      DO 174 I=1,NRHM                                                   DK054760          
      DO 174 J=I,NRHM                                                   DK054770          
      K=NRHM*(I-1)-I*(I-3)/2+J-I                                        DK054780          
      K3=NLHM+J                                                         DK054790          
      K2=NLHM+I                                                         DK054800          
  174 SSCPR(K)=SSCPR(K)-TOT2(K2)*TOT2(K3)*ROWN                          DK054810          
      GO TO 180                                                         DK054820          
  176 DO 178 I=1,NRHM                                                   DK054830          
      K2=NLHM+I                                                         DK054840          
  178 SSCPR(I)=SSCPR(I)-TOT2(K2)*TOT2(K2)*ROWN                          DK054850          
  180 K=0                                                               DK054860          
      DO 182 I=1,NRHM                                                   DK054870          
      J2=NLHM+I                                                         DK054880          
      DO 182 J=1,NLHM                                                   DK054890          
      K=K+1                                                             DK054900          
  182 RHM(K)=RHM(K)-TOT2(J)*TOT2(J2)*ROWN                               DK054910          
  184 IF (LIOP.EQ.0.OR.LIOP.EQ.5.OR.LIOP.GT.9)  GO TO 190               DK054920          
      WRITE (6,1051) IMJ2,GNI                                           DK054930          
 1051 FORMAT (1H0,5X,14HMAJOR CLASS IS,I12,3X,31HREDUCED DIAGONAL ELEMENDK054940          
     1T (GNI)=,F18.8)                                                   DK054950          
      WRITE (6,1052) (TOT2(I),I=1,NOT)                                  DK054960          
 1052 FORMAT (1H ,9F13.3)                                               DK054970          
  190 DO 191 I=1,NOT                                                    DK054980          
  191 TOT2(I)=0.0                                                       DK054990          
      MJN=MJN+1                                                         DK055000          
      GNI=0.0                                                           DK055010          
      NCDG=0                                                            DK055020          
      SWT2=0.0                                                          DK055030          
      IF (NTRA.EQ.0) GO TO 196                                          DK055040          
      GO TO 108                                                         DK055050          
  193 DO 194 I=1,NOT                                                    DK055060          
  194 TOT(I)=TOT2(I)                                                    DK055070          
      DF=NCDS-MJN                                                       DK055080          
      IF (NAB.EQ.3) DF=DF+FLOAT(MJN)                                    DK055090          
      GO TO 299                                                         DK055100          
  196 DO 198 I=1,NOT                                                    DK055110          
  198 TOT(I)=TOT3(I)                                                    DK055120          
      DF=NCDS-MJN                                                       DK055130          
      GO TO 299                                                         DK055140          
  900 WRITE (6,1053) NCDS                                               DK055150          
 1053 FORMAT (1H0,69HUNITS POSITION OF AN ID FIELD OR A CONTROL FIELD ISDK055160          
     1 BLANK ON CARD NO.,I5)                                            DK055170          
      GO TO 902                                                         DK055180          
  901 WRITE (6,1054) NCDS,IJOB                                          DK055190          
 1054 FORMAT (1H0,38HCARDS OUT OF SEQUENCE---CHECK CARD NO.,I5,5X,15HFORDK055200          
     1 PROBLEM NO.,I3)                                                  DK055210          
  902 MULL=1                                                            DK055220          
      CALL WCARD(IN,K,L,IC,IDUM,IFLAG)                                                    
      RETURN                                                            DK055230          
  299 CONTINUE                                                          DK055240          
      IF (NAB.EQ.0) GO TO 300                                           DK055250          
      GO TO (301,302,303,304), NAB                                      DK055260          
  300 IF (KD.EQ.0) GO TO 305                                            DK055270          
      KC=4                                                              DK055280          
      NMIG=NCDG                                                         DK055290          
      SWT3=SWT2                                                         DK055300          
      SWT2=SWT1                                                         DK055310          
      NCDG=NCDS                                                         DK055320          
      CALL MIXED (NAB,MATX,KA,KB,KC,NOT,TOT,NMIG,NLHM,NCPR,NRHM,TOT3,ARRDK055330          
     1AY,NCDG,FAB,TOT4,SOF,NX,KD,IBET,SWT2,SWT3,WTT,SOFN)               DK055340          
  305 WRITE (6,1060) NCDS                                               DK055350          
 1060 FORMAT (1H1,5X,53HTOTAL LEAST-SQUARES ANALYSIS.  NO EQUATIONS ABSODK055360          
     1RBED.,5X,13HDF=NO. CARDS=,I6)                                     DK055370          
      GO TO 310                                                         DK055380          
  301 WRITE (6,1061) DF                                                 DK055390          
 1061 FORMAT (1H1,21X,38HEQUATION FOR THE OVERALL MEAN ABSORBED,5X,3HDF=DK055400          
     1,F7.0)                                                            DK055410          
      GO TO 310                                                         DK055420          
  302 WRITE (6,1062) MJN,NCDS,DF                                        DK055430          
 1062 FORMAT (1H1,10X,55HNO. OF CLASSES OR SUBCLASSES ABSORBED BY LEAST DK055440          
     1SQUARES=,I6,5X,10HNO. CARDS=,I6,5X,3HDF=,F7.0)                    DK055450          
      GO TO 310                                                         DK055460          
  303 WRITE (6,1063) MJN,REP,NCDS,DF                                    DK055470          
 1063 FORMAT (1H1,60HNO. OF CLASSES OR SUBCLASSES ABSORBED BY MAXIMUM LIDK055480          
     1KELIHOOD=,I6,3X,7H1-R/R =,F7.4,3X,10HNO. CARDS=,I6,3X,3HDF=,F7.0) DK055490          
      GO TO 310                                                         DK055500          
  304 WRITE (6,1064) MJN,MIN,REP,NCDS,DF                                DK055510          
 1064 FORMAT (1H1,37H NO. OF MAJOR CLASSES ABSORBED BY ML=,I4,3X,21HNO. DK055520          
     1MINOR SUBCLASSES=,I6,3X,7H1-R/R =,F7.4,3X,10HNO. CARDS=,I6,3X,3HDFDK055530          
     3=,F7.0)                                                           DK055540          
  310 IF (IBET.EQ.0) GO TO 309                                          DK055550          
      WRITE (6,1116) SWT1                                               DK055560          
 1116 FORMAT(1H0,30X,49HWEIGHTED LEAST SQUARES ANALYSIS--SUM OF WEIGHTS                   
     1=,F10.2)                                                                            
  309 NCC=0                                                             DK055590          
      IF (NOM+NON.EQ.0) GO TO 320                                       DK055600          
      CALL SCLNOS                                                       DK055610          
  320 IF (NLHM.EQ.0.OR.(NPR.EQ.0.AND.LIOP.GT.9))  GO TO 330             DK055620          
      WRITE (6,1100) IJOB                                               DK055630          
 1100 FORMAT (1H0,60HOVERALL MEANS AND STANDARD DEVIATIONS OF LHM FOR PRDK055640          
     1OBLEM NO.,I3)                                                     DK055650          
      WRITE (6,1101)                                                    DK055660          
 1101 FORMAT (1H0,35H CODED LHM    INDEPENDENT VARIABLES,11X,4HMEAN,9X,4DK055670          
     1HS.D.)                                                            DK055680          
      WRITE (6,1102)                                                    DK055690          
 1102 FORMAT (1H )                                                      DK055700          
      L=1                                                               DK055710          
      K3=1                                                              DK055720          
      DO 329 I=1,NLHM                                                   DK055730          
      AMN=TOT(I)/FLOAT(NCDS)                                            DK055760          
      IF (IBET.EQ.1) AMN=TOT(I)/SWT1                                    DK055770          
      IF(NAB.EQ.3) TOT(I)=ARRAY(I)                                                        
      IF(I.LT.IE.AND.LIOP.GT.9) GO TO 329                                                 
      K=NLHM*(I-1)-I*(I-3)/2                                            DK055780          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 324                   DK055790          
      SD=DSQRT((ARRAY(K)-(TOT(I)*TOT(I)/ARRAY(1)))/(DF-1.))             DK055800          
      GO TO 326                                                         DK055810          
  324 SD=DSQRT(ARRAY(K)/DF)                                             DK055820          
  326 IF (I.LT.IE) GO TO 328                                            DK055830          
      IF (LAB1(I).NE.RGRSN) GO TO 328                                   DK055840          
      L=L-1                                                             DK055850          
      IF (L.NE.0) GO TO 328                                             DK055860          
      IF (LQC(K3).EQ.1) GO TO 393                                       DK055870          
      AC=AMN+XM(K3)                                                     DK055880          
      WRITE (6,1117) AC                                                 DK055890          
 1117 FORMAT(10X,5HXBAR=,F13.5)                                                           
  393 IF (LQC(K3)-2) 390,391,392                                        DK055910          
  390 AMN=AMN+XM(K3)                                                    DK055920          
      K3=K3+1                                                           DK055930          
      L=1                                                               DK055940          
      GO TO 328                                                         DK055950          
  391 K3=K3+1                                                           DK055960          
      L=2                                                               DK055970          
      GO TO 328                                                         DK055980          
  392 K3=K3+1                                                           DK055990          
      L=3                                                               DK056000          
  328 WRITE (6,1103) I,LAB1(I),LAB2(I),LAB3(I),LAB4(I),AMN,SD           DK056010          
  329 CONTINUE                                                          DK056020          
 1103 FORMAT (1H ,I6,6X,A6,2(1X,A6),A6,2F13.5)                          DK056030          
  330 IF (NRHM.EQ.0) GO TO 342                                          DK056040          
      WRITE (6,1104)                                                    DK056050          
 1104 FORMAT (1H0,15X,44HOVERALL MEANS AND STANDARD DEVIATIONS OF RHM)  DK056060          
      WRITE (6,1102)                                                    DK056070          
      DO 340 I=1,NRHM                                                   DK056080          
      K2=NLHM+I                                                         DK056090          
      AMN=TOT(K2)/FLOAT(NCDS)                                           DK056100          
      IF (IBET.EQ.1) AMN=TOT(K2)/SWT1                                   DK056110          
      K=NLHM*(I-1)+1                                                    DK056120          
      IF (NAB.EQ.0.OR.NAB.EQ.3) TOT(K2)=RHM(K)                          DK056130          
      IF (NCPR.EQ.0) GO TO 332                                          DK056140          
      J=NRHM*(I-1)-I*(I-3)/2                                            DK056150          
      GO TO 334                                                         DK056160          
  332 J=I                                                               DK056170          
  334 IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 336                   DK056180          
      SD=DSQRT((SSCPR(J)-(TOT(K2)*TOT(K2)/ARRAY(1)))/(DF-1.))           DK056190          
      GO TO 338                                                         DK056200          
  336 SD=DSQRT(SSCPR(J)/DF)                                             DK056210          
  338 AMN=AMN+YM(I)                                                     DK056220          
      IF (MTY.EQ.1) TOT4(I)=AMN                                         DK056230          
  340 WRITE (6,1105) LITY(I),AMN,SD                                     DK056240          
 1105 FORMAT (1H ,12X,A6,4X,5HMEAN=,F12.5,10X,5HS.D.=,F12.5)            DK056250          
  342 IF (LIOP.EQ.7) GO TO 500                                          DK056260          
  344 IF (NLHM.EQ.0) GO TO 359                                          DK056270          
      IF(NAB.NE.3) GO TO 345                                                              
      DO 343 I=1,NOT                                                                      
  343 TOT5(I)=TOT2(I)                                                                     
  345 IF(IE.GT.NLHM.AND.LIOP.GT.9) GO TO 359                                              
      K1=IE-1                                                           DK056290          
      DO 399 I=1,NLHM                                                   DK056300          
      TOT2(I)=0.0                                                       DK056310          
  399 TOT3(I)=0.0                                                       DK056320          
      IF (NPR.EQ.0) GO TO 421                                           DK056330          
      K5=1                                                              DK056340          
      DO 400 I=1,NPR                                                    DK056350          
      K2=LQC(I)                                                         DK056360          
      DO 401 J=1,K2                                                     DK056370          
      K1=K1+1                                                           DK056380          
      IF (J.EQ.1) AMN=(-1.)*(TOT(K1)/FLOAT(NCDS))                       DK056390          
      IF (IBET.EQ.1) AMN=(-1.)*(TOT(K1)/SWT1)                           DK056400          
      TOT3(K1)=AMN**J                                                   DK056410          
      IF (NAB.EQ.0.OR.NAB.EQ.3) TOT2(K1)=1.0                            DK056420          
      IF(LAD(I).NE.0) TOT2(K1)=TOT2(K1)+100.                                              
      IF (ICLR(I).EQ.0) GO TO 401                                       DK056440          
      IF (J.GT.1) K5=K5-K3                                              DK056450          
      K3=ICLR(I)                                                        DK056460          
      DO 402 K=1,K3                                                     DK056470          
      J1=IRM(K5)                                                        DK056480          
      IF (J1.GT.NOM) GO TO 403                                          DK056490          
      K4=K1+1                                                           DK056500          
      K6=K1+NCL(J1)-1                                                   DK056510          
      IF (J1.LE.NMEA) GO TO 408                                         DK056520          
      J1=NMEA+1                                                         DK056530          
      J2=IRM(K5)-1                                                      DK056550          
      IF (NAB.EQ.0.OR.NAB.EQ.3) NSUM=2                                  DK056560          
      IF(J2.LT.J1) GO TO 405                                            DK056570          
      DO 404 N=J1,J2                                                    DK056580          
  404 NSUM=NSUM+NCL(N)-1                                                DK056590          
      GO TO 405                                                         DK056600          
  403 J1=IRM(K5)-NOM                                                    DK056610          
      K4=K1+1                                                           DK056620          
      K6=K1+NCLN(J1)-1                                                  DK056630          
      IF (J1.LE.NNEA) GO TO 408                                         DK056640          
      J1=NNEA+1                                                         DK056650          
      J2=IRM(K5)-NOM-1                                                  DK056660          
      NSUM=MN2-NME+1                                                    DK056670          
      IF (NAB.EQ.0.OR.NAB.EQ.3) NSUM=NSUM+1                             DK056680          
      IF (J2.LT.J1) GO TO 405                                           DK056690          
      DO 406 N=J1,J2                                                    DK056700          
  406 NSUM=NSUM+NCLN(N)-1                                               DK056710          
  405 DO 407 N=K4,K6                                                    DK056720          
      TOT3(N)=AMN**J                                                    DK056730          
      TOT2(N)=FLOAT(NSUM)                                               DK056740          
      IF(LAD(I).NE.0) TOT2(N)=TOT2(N)+100.                              DK056750          
  407 NSUM=NSUM+1                                                       DK056760          
  408 K5=K5+1                                                           DK056770          
  402 K1=K6                                                             DK056780          
  401 CONTINUE                                                          DK056790          
  400 CONTINUE                                                          DK056800          
      DO 415 N=1,NLHM                                                   DK056810          
      N3=NLHM-N+1                                                       DK056820          
      M=TOT2(N3)                                                        DK056830          
      IF (M.GE.100) M=M-100                                             DK056840          
      IF (M.EQ.0) GO TO 415                                             DK056850          
      AC=TOT3(N3)                                                       DK056860          
      DO 416 LL=N,NLHM                                                  DK056870          
      M=TOT2(N3)                                                        DK056880          
      K4=1                                                                                
      K6=1                                                                                
      N4=NLHM-LL+1                                                      DK056890          
      J7=NLHM*(N4-1)-N4*(N4-3)/2                                        DK056900          
      J2=J7+N3-N4                                                       DK056910          
      AD=TOT3(N4)                                                       DK056920          
      M1=TOT2(N4)                                                       DK056930          
      IF (M.GE.100.AND.M1.GE.100) GO TO 416                             DK056940          
      IF(M.LT.100.AND.M1.GE.100) K4=0                                                     
      IF(M.GE.100.AND.M1.LT.100) K6=0                                                     
      IF (M1.GE.100) M1=M1-100                                          DK056950          
      IF (M.GE.100.AND.M1.EQ.0) GO TO 416                               DK056960          
      IF (M.GE.100) M=M-100                                             DK056970          
      J1=NLHM*(M-1)-M*(M-3)/2                                           DK056980          
      IF(M1.EQ.0.OR.K4.EQ.0) GO TO 410                                                    
      J3=NLHM*(M1-1)-M1*(M1-3)/2                                        DK057000          
      J4=J1+N4-M                                                        DK057010          
      IF (N4.LT.M) J4=J7+M-N4                                           DK057020          
      J5=J3+M-M1                                                        DK057030          
      IF (M1.GT.M) J5=J1+M1-M                                           DK057040          
      J6=J3+N3-M1                                                       DK057050          
      IF(K6.EQ.0) GO TO 411                                                               
      ARRAY(J2)=ARRAY(J2)+AC*(AD*ARRAY(J5)+ARRAY(J4))+AD*ARRAY(J6)      DK057060          
      GO TO 416                                                         DK057070          
  410 J6=J1+N4-M                                                        DK057080          
      IF (N4.LT.M) J6=J7+M-N4                                           DK057090          
      ARRAY(J2)=ARRAY(J2)+AC*ARRAY(J6)                                  DK057100          
      IF (N4.EQ.1.AND.(NAB.EQ.0.OR.NAB.EQ.3)) TOT(N3)=ARRAY(J2)         DK057110          
      GO TO 416                                                                           
  411 ARRAY(J2)=ARRAY(J2)+AD*ARRAY(J6)                                                    
  416 CONTINUE                                                          DK057120          
  415 CONTINUE                                                          DK057130          
      DO 418 N=1,NRHM                                                   DK057140          
      J1=NLHM*(N-1)                                                     DK057150          
      DO 419 LL=1,NLHM                                                  DK057160          
      IF (LL.LT.IE) GO TO 419                                           DK057170          
      J2=J1+LL                                                          DK057180          
      M=TOT2(LL)                                                        DK057190          
      IF (M.EQ.0.OR.M.GE.100) GO TO 419                                 DK057200          
      J4=J1+M                                                           DK057210          
      RHM(J2)=RHM(J2)+RHM(J4)*TOT3(LL)                                  DK057220          
  419 CONTINUE                                                          DK057230          
  418 CONTINUE                                                          DK057240          
  421 WRITE (6,1102)                                                    DK057250          
      WRITE (6,1106) IJOB                                               DK057260          
 1106 FORMAT (1H0,15X,64HSUMS OF SQUARES, C.P. AND CORRELATIONS AMONG LHDK057270          
     1M FOR PROBLEM NO.,I3)                                             DK057280          
      WRITE (6,1107)                                                    DK057290          
 1107 FORMAT (1H0,4H ROW,5H  COL,15X,21HINDEPENDENT VARIABLES)          DK057300          
      WRITE (6,1108)                                                    DK057310          
 1108 FORMAT (1H ,9HCODE CODE,9X,3HROW,22X,6HCOLUMN,23X,28HS.SQS. OR C.PDK057320          
     1.   CORRELATION)                                                  DK057330          
      DO 360 I=1,NLHM                                                   DK057340          
      DO 360 J=I,NLHM                                                   DK057350          
      IF (I.LT.IE.AND.J.LT.IE.AND.LIOP.GT.9)  GO TO 360                 DK057360          
      K=NLHM*(I-1)-I*(I-3)/2                                            DK057370          
      K1=NLHM*(J-1)-J*(J-3)/2                                           DK057380          
      K3=K+J-I                                                          DK057390          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 354                   DK057400          
      SCP=ARRAY(K3)-(TOT(I)*TOT(J))/ARRAY(1)                            DK057410          
      SSY1=ARRAY(K)-(TOT(I)*TOT(I))/ARRAY(1)                            DK057460          
      SSY2=ARRAY(K1)-(TOT(J)*TOT(J))/ARRAY(1)                                             
      RLHM=SCP/DSQRT(DIV(SSY1*SSY2))                                    DK057500          
      GO TO 356                                                         DK057530          
  354 RLHM=ARRAY(K3)/DSQRT(ARRAY(K)*ARRAY(K1))                          DK057540          
  356 IF (I.LT.IE.AND.LIOP.GT.9)  GO TO 360                             DK057550          
      IF (I.NE.J) GO TO 358                                             DK057560          
      RLHM=1.                                                           DK057570          
      WRITE (6,1102)                                                    DK057580          
  358 WRITE (6,1109) I,J,LAB1(I),LAB2(I),LAB3(I),LAB4(I),LAB1(J),LAB2(J)DK057590          
     1,LAB3(J),LAB4(J),ARRAY(K3),RLHM                                   DK057600          
  360 CONTINUE                                                          DK057610          
 1109 FORMAT (1H ,I3,I5,2X,A6,2(1X,A6),2A6,2(1X,A6),A6,F23.8,F13.4)     DK057620          
  359 IF (NRHM.EQ.0) GO TO 386                                          DK057630          
      IF (NLHM.EQ.0) GO TO 376                                          DK057640          
      IF (IE.GT.NLHM.AND.LIOP.GT.9)  GO TO 376                          DK057650          
      WRITE (6,1102)                                                    DK057660          
      WRITE (6,1110) IJOB                                               DK057670          
 1110 FORMAT (1H0,71H SUMS OF CROSSPRODUCTS AND CORRELATIONS OF LHM WITHDK057680          
     1 RHM FOR PROBLEM NO.,I3)                                          DK057690          
      WRITE (6,1111)                                                    DK057700          
 1111 FORMAT (1H0,41HRHM  LHM  RHM NAME   INDEPENDENT VARIABLE,16X,4HC.PDK057710          
     1.,7X,11HCORRELATION)                                              DK057720          
      DO 375 I=1,NRHM                                                   DK057730          
      K4=NLHM+I                                                         DK057740          
      WRITE (6,1102)                                                    DK057750          
      DO 375 J=1,NLHM                                                   DK057760          
      IF (J.LT.IE.AND.LIOP.GT.9)  GO TO 375                             DK057770          
      K=NLHM*(J-1)-J*(J-3)/2                                            DK057780          
      IF (NCPR.EQ.0) GO TO 362                                          DK057790          
      K1=NRHM*(I-1)-I*(I-3)/2                                           DK057800          
      GO TO 364                                                         DK057810          
  362 K1=I                                                              DK057820          
  364 K2=NLHM*(I-1)+J                                                   DK057830          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 372                   DK057840          
      SCP=RHM(K2)-(TOT(J)*TOT(K4))/ARRAY(1)                             DK057850          
      SSY1=ARRAY(K)-(TOT(J)*TOT(J))/ARRAY(1)                            DK057870          
      SSY2=SSCPR(K1)-(TOT(K4)*TOT(K4))/ARRAY(1)                                           
      RLR=SCP/DSQRT(DIV(SSY1*SSY2))                                     DK057910          
      GO TO 374                                                         DK057940          
  372 RLR=RHM(K2)/DSQRT(ARRAY(K)*SSCPR(K1))                             DK057950          
  374 WRITE (6,1112) I,J,LITY(I),LAB1(J),LAB2(J),LAB3(J),LAB4(J),RHM(K2)DK057960          
     1,RLR                                                              DK057970          
  375 CONTINUE                                                          DK057980          
 1112 FORMAT (1H ,I3,I5,3X,A6,2X,2(A6,1X),2A6,F23.8,F10.4)              DK057990          
  376 IF (NCPR.EQ.0) GO TO 386                                          DK058000          
      IF (NRN.NE.NRUN.AND.LIOP.EQ.20) GO TO 386                         DK058010          
      WRITE (6,1102)                                                    DK058020          
      WRITE (6,1113) IJOB                                               DK058030          
 1113 FORMAT (1H0,15X,64HSUMS OF SQUARES, C.P. AND CORRELATIONS AMONG RHDK058040          
     1M FOR PROBLEM NO.,I3)                                             DK058050          
      WRITE (6,1114)                                                    DK058060          
 1114 FORMAT (1H0,20X,3HROW,6H   COL,4X,3HRHM,4X,3HRHM,15X,14HS.SQS. OR DK058070          
     1C.P.,4X,11HCORRELATION)                                           DK058080          
      DO 384 I=1,NRHM                                                   DK058090          
      DO 384 J=I,NRHM                                                   DK058100          
      K=NRHM*(I-1)-I*(I-3)/2                                            DK058110          
      K1=NRHM*(J-1)-J*(J-3)/2                                           DK058120          
      K3=K+J-I                                                          DK058130          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 380                   DK058140          
      K4=NLHM+I                                                         DK058150          
      K5=NLHM+J                                                         DK058160          
      SCP=SSCPR(K3)-(TOT(K4)*TOT(K5))/ARRAY(1)                          DK058170          
      SSY1=SSCPR(K)-(TOT(K4)*TOT(K4))/ARRAY(1)                          DK058180          
      SSY2=SSCPR(K1)-(TOT(K5)*TOT(K5))/ARRAY(1)                         DK058190          
      RLHM=SCP/DSQRT(SSY1*SSY2)                                         DK058200          
      GO TO 382                                                         DK058210          
  380 RLHM=SSCPR(K3)/DSQRT(SSCPR(K)*SSCPR(K1))                          DK058220          
  382 IF (I.NE.J) GO TO 384                                             DK058230          
      WRITE (6,1102)                                                    DK058240          
  384 WRITE (6,1115) I,J,LITY(I),LITY(J),SSCPR(K3),RLHM                 DK058250          
 1115 FORMAT (1H ,20X,I3,I6,3X,A6,2X,A6,F25.8,F14.4)                    DK058260          
  386 N=NLHM                                                            DK058270          
  500 CONTINUE                                                          DK058280          
      IF(NAB.NE.3) RETURN                                                                 
      DO 501 I=1,NOT                                                                      
  501 TOT(I)=TOT5(I)                                                                      
      RETURN                                                            DK058290          
      END                                                               DK058300          
