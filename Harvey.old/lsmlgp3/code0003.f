*TEXT                                                                                     
      SUBROUTINE CODE  (                                                  100001          
     1ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LAB4,LITY,TRED,    100002          
     2YM,IM,MS,IPL,NSP,NND,XP,YP,NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NEN,   100003          
     3NCLN,LNE,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NBEG,NEGX,LOGE,LQC,       100004          
     4NREGP,LGTX,JBEG,NDECX,LITR,XM,NEGY,LNY,LHY,KBEG,NDECY,IC,EFF1,      100005          
     5EFF2,NOS,X,NMAC,IED)                                                                
C     SUBROUTINE USED TO GET DATA FROM CARD                               100007          
C     CODE PARAMETERS (ML3,INTV,NOM,NCL,IBEG,LME,NCLN,NBEG,LNE,IC,IDEN,   100008          
C     NDEN,ML,K1,ICOD,ISW2,ISW3,L,L7,IJOB)                                100009          
C     ----------------------------------------------------                100010          
      DIMENSION ARRAY(1),SSCPR(1),SSS(1),RHM(1),TOT(1),TOT2(1),TOT3(1),   100011          
     1LAB1(1),LAB2(1),LAB3(1),LAB4(1),LITY(1),TRED(1),YM(1),IM(1),MS(1),  100012          
     2IPL(1),NSP(1),NND(1),XP(1),YP(1),NDC(1),NMI(1),MEN(1),NCL(1),LME(1  100013          
     3),IBEG(1),IDEN(1),LIT(1),NEN(1),NCLN(1),LNE(1),NDEN(1),NLIT(1),     100014          
     4NMA(1),NMC(1),INT1(1),INT2(1),MSCL(1),NBEG(1),NEGX(1),LOGE(1),      100015          
     5LQC(1),NREGP(1),LGTX(1),JBEG(1),NDECX(1),LITR(1),XM(1),NEGY(1),     100016          
     6LNY(1),LHY(1),KBEG(1),NDECY(1),IC(1),EFF1(1),EFF2(1),NOS(1),X(1),   100017          
     7NMAC(1),IED(1)                                                                      
      DIMENSION NEQ(10)                                                                   
      COMMON /CMBLK1/NEQ                                                  100020          
      COMMON /CMBLK2/MXNLHM,MXNRHM,MXNCAS,MXNCF,MXMN2P,MXNS2,MXNSME,      100021          
     1 MXNMJC,MXNMIC,MXME,MXMECL,MXNE,MXNECL,MX2F,MX2FMS,MXNPR,MXNCD,     100022          
     2 MXK9,MXNOS,MXNED ,MXI2                                             100023          
      COMMON /CMBLK3/IJOB,NAB,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F,NPR,IRAN,M  100024          
     1POP,LIOP,IAD,REP,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,NOM,NON,ML,MLB,NS  100025          
     2,NOT,DF,KPUT,SDF,EDF,WK,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,RR         100026          
     3,IN,NED,IBOP                                                                        
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3,   100028          
     1L,L7                                                                100029          
      ISW2=0                                                              100030          
      ISW3=0                                                              100031          
      ML3=0                                                               100032          
      INTVM1=INTV-1                                                       100033          
      IF (INTV.GT.NOM) GO TO 6                                            100034          
      K1=NCL(INTV)                                                        100035          
      L7=IBEG(INTV)                                                       100036          
      ISW=0                                                               100037          
      L=LME(INTV)+L7-1                                                    100038          
      IF (INTVM1.EQ.0) GO TO 12                                           100039          
      DO 4 J=1,INTVM1                                                     100040          
    4 ML3=NCL(J)+ML3                                                      100041          
      GO TO 12                                                            100042          
    6 K2=NOM+1                                                            100043          
      IF (K2.GT.INTVM1) GO TO 9                                           100044          
      DO 8 J=K2,INTVM1                                                    100045          
      K3=J-NOM                                                            100046          
    8 ML3=NCLN(K3)+ML3                                                    100047          
    9 KIX=INTV-NOM                                                        100048          
      L7=NBEG(KIX)                                                        100049          
      K1=NCLN(KIX)                                                        100050          
      L=LNE(KIX)+L7-1                                                     100051          
      ISW=1                                                               100052          
   12 K=K1+ML3                                                            100053          
      CALL FIELD (ICOD,L,IC,L7,J)                                         100054          
      L=0                                                                 100055          
      IF (J.EQ.1) GO TO 50                                                100056          
      ML1=ML3                                                             100057          
   16 ML3=ML3+1                                                           100058          
      IF (ISW.EQ.1) GO TO 20                                              100059          
      IF (ML3.GT.K) GO TO 900                                             100060          
      IF (ICOD.NE.IDEN(ML3)) GO TO 16                                     100061          
      GO TO 26                                                            100062          
   20 IF (ML3.LE.K) GO TO 24                                              100063          
      ISW2=1                                                              100064          
      GO TO 49                                                            100065          
   24 IF (ICOD.NE.NDEN(ML3)) GO TO 16                                     100066          
   26 ML3=ML3-ML1                                                         100067          
      GO TO 49                                                            100068          
  900 WRITE (6,1000) IJOB                                                 100069          
 1000 FORMAT (1H0,57HCLASS CODE MISSING. CHECK PARAMETER CARDS FOR PROBL  100070          
     1EM NO.,I3)                                                          100071          
      ISW3=1                                                              100072          
   49 RETURN                                                              100073          
   50 L=1                                                                 100074          
      RETURN                                                              100075          
      END                                                                 100076          
*ENDTEXT                                                                                  
