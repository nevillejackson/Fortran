*TEXT                                                                                     
      SUBROUTINE SCLNOS                                                                   
C     SUBROUTINE TO LIST DISTRIBUTION OF CLASS AND SUBCLASS NUMBERS     DEC10000          
C     ------------------------------------------------------            DEC10001          
      DIMENSION ARRAY(2000),SSCPR(630),SSS(630),RHM(0250),TOT(106),TOT2(DEC10003          
     1106),TOT3(106),LAB1(100),LAB2(100),LAB3(100),LAB4(100)            DEC10004          
      DIMENSION LITY(35),TRED(35),YM(35),IM(90),MS(50),NEQ(6),IPL(13),NSDEC10005          
     1P(13),NND(13),XP(81),YP(41),      NDC(10),NMI(10),MEN(20),NCL(20),DEC10006          
     2LME(20),IBEG(20),IDEN(100),LIT(20),NEN(40),NCLN(40),LNE(40),NDEN(1DEC10007          
     300),NLIT(40),NMA(30),NMC(30),INT1(30),INT2(30),MSCL(100),NBEG(40),DEC10008          
     4NEGX(90),LOGE(90),LQC(90),NREGP(90),LGTX(90),JBEG(90),NDECX(90),LIDEC10009          
     5TR(90),XM(90),NEGY(35),LNY(35),LHY(35),KBEG(35),NDECY(35),IC(480),DEC10010          
     6EFF1(50),EFF2(50),NOS(200),X(106),NMAC(40)                        DEC10011          
      DIMENSION LSKF(20),LBEG(20),IREJ(20)                              M0210011          
      COMMON /CMBLK1/ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LADEC10012          
     1B4,LITY,TRED,YM,IM,MS,NEQ,IPL,NSP,NND,XP,YP                       DEC10013          
      COMMON /CMBLK2/NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NCLN,NEN,LNE,NBEGDEC10014          
     1,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NEGX,LOGE,LQC,NREGP,LGTX,JBEG,NDDEC10015          
     2ECX,LITR,NEGY,LNY,LHY,KBEG,NDECY,XM,IC,EFF1,EFF2,NOS,X,NMAC       DEC10016          
      COMMON /CMBLK3/IJOB,NAB,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F,NPR,IRAN,MDEC10017          
     1POP,LIOP,IAD,REP,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,NOM,NON,ML,MLB,NSDEC10018          
     2,NOT,DF,KPUT,SDF,EDF,WK,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,RR       DEC10019          
     3,IN,NSKF,LSKF,LBEG,IREJ                                           M0210019          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3, DEC10020          
     1L,L7                                                              DEC10021          
      DATA(IBLK=6H      )                                               M0110022          
      DATA(MIS =6HMISS  )                                               M0110022          
      DATA(MISN=6HMISS N)                                               M0110022          
      DATA(MISI=6HMISS I)                                               M0110022          
      WRITE (6,1001) IJOB                                               DEC10023          
 1001 FORMAT (1H0,60H  DISTRIBUTION OF CLASS AND SUBCLASS NUMBERS FOR PRDEC10024          
     1OBLEM NO.,I4)                                                     DEC10025          
      WRITE (6,1002)                                                    DEC10026          
 1002 FORMAT (1H0,19X,14HIDENTIFICATION,13X,3HNO.)                      DEC10027          
      L=1                                                               DEC10028          
      K=ML+1                                                            DEC10029          
      MISS=IBLK                                                         DEC10030          
      K1=NMEA+1                                                         DEC10031          
      IF (K1.GT.NOM) GO TO 17                                           DEC10032          
      DO 16 I=K1,NOM                                                    DEC10033          
      WRITE (6,1000)                                                    DEC10034          
 1000 FORMAT (1H )                                                      DEC10035          
      K2=NCL(I)                                                         DEC10036          
      DO 16 J=1,K2                                                      DEC10037          
      IF (NOS(L).NE.0) GO TO 10                                         DEC10038          
      MISS=MIS                                                          DEC10039          
      KPUT=1                                                            DEC10040          
   10 WRITE (6,1003) LIT(I),IDEN(K),NOS(L),MISS                         DEC10041          
 1003 FORMAT (1H ,17X,A6,I5,16X,I5,A8)                                  DEC10042          
      K=K+1                                                             DEC10043          
      MISS=IBLK                                                         DEC10044          
   16 L=L+1                                                             DEC10045          
   17 K=MLB+1                                                           DEC10046          
      K1=NNEA+1                                                         DEC10047          
      IF (K1.GT.NON) GO TO 23                                           DEC10048          
      DO 22 I=K1,NON                                                    DEC10049          
      WRITE (6,1000)                                                    DEC10050          
      K2=NCLN(I)                                                        DEC10051          
      DO 22 J=1,K2                                                      DEC10052          
      IF (NOS(L).NE.0) GO TO 20                                         DEC10053          
      MISS=MIS                                                          DEC10054          
      KPUT=1                                                            DEC10055          
   20 WRITE (6,1003) NLIT(I),NDEN(K),NOS(L),MISS                        DEC10056          
      MISS=IBLK                                                         DEC10057          
      K=K+1                                                             DEC10058          
   22 L=L+1                                                             DEC10059          
   23 K6=0                                                              DEC10060          
      IF (N2F.EQ.0) GO TO 81                                            DEC10061          
      DO 80 I=1,N2F                                                     DEC10062          
      WRITE (6,1000)                                                    DEC10063          
      K6=K6+NMC(I)                                                      DEC10064          
      NSUM=0                                                            DEC10065          
      MSUM=0                                                            DEC10066          
      IF (INT1(I).GT.NOM) GO TO 34                                      DEC10067          
      K1=INT1(I)-1                                                      DEC10068          
      IF (K1.EQ.0) GO TO 31                                             DEC10069          
      DO 30 J=1,K1                                                      DEC10070          
   30 NSUM=NSUM+NCL(J)                                                  DEC10071          
   31 K1=INT1(I)                                                        DEC10072          
      K3=NCL(K1)                                                        DEC10073          
      ID1=LIT(K1)                                                       DEC10074          
      GO TO 40                                                          DEC10075          
   34 K1=NOM+1                                                          DEC10076          
      K2=INT1(I)-1                                                      DEC10077          
      IF (K1.GT.K2) GO TO 37                                            DEC10078          
      DO 36 J=K1,K2                                                     DEC10079          
      K=J-NOM                                                           DEC10080          
   36 NSUM=NSUM+NCLN(K)                                                 DEC10081          
   37 K1=INT1(I)-NOM                                                    DEC10082          
      K3=NCLN(K1)                                                       DEC10083          
      ID1=NLIT(K1)                                                      DEC10084          
   40 IF (INT2(I).GT.NOM) GO TO 46                                      DEC10085          
      K2=INT2(I)-1                                                      DEC10086          
      IF (K2.EQ.0) GO TO 43                                             DEC10087          
      DO 42 J=1,K2                                                      DEC10088          
   42 MSUM=MSUM+NCL(J)                                                  DEC10089          
   43 K2=INT2(I)                                                        DEC10090          
      K4=NCL(K2)                                                        DEC10091          
      ID2=LIT(K2)                                                       DEC10092          
      GO TO 50                                                          DEC10093          
   46 K1=NOM+1                                                          DEC10094          
      K2=INT2(I)-1                                                      DEC10095          
      IF (K1.GT.K2) GO TO 49                                            DEC10096          
      DO 48 J=K1,K2                                                     DEC10097          
      K=J-NOM                                                           DEC10098          
   48 MSUM=MSUM+NCLN(K)                                                 DEC10099          
   49 K1=INT2(I)-NOM                                                    DEC10100          
      K4=NCLN(K1)                                                       DEC10101          
      ID2=NLIT(K1)                                                      DEC10102          
   50 DO 80 J=1,K3                                                      DEC10103          
      K2=NSUM+J                                                         DEC10104          
      IF (INT1(I).GT.NOM) GO TO 54                                      DEC10105          
      ID3=IDEN(K2)                                                      DEC10106          
      GO TO 56                                                          DEC10107          
   54 ID3=NDEN(K2)                                                      DEC10108          
   56 DO 80 K=1,K4                                                      DEC10109          
      K5=MSUM+K                                                         DEC10110          
      IF (NOS(L).NE.0) GO TO 70                                         DEC10111          
      MISS=MISN                                                         DEC10112          
      IF (NMC(I).EQ.0) KPUT=1                                           DEC10113          
      IF (NMC(I).EQ.0) GO TO 70                                         DEC10114          
      K7=J*100+K                                                        DEC10115          
      K1=K6-NMC(I)                                                      DEC10116          
   60 K1=K1+1                                                           DEC10117          
      IF (K1.GT.K6) GO TO 68                                            DEC10118          
      IF (K7-MSCL(K1)) 60,64,60                                         DEC10119          
   64 MISS=MISI                                                         DEC10120          
      IF (J.EQ.K3.OR.K.EQ.K4) KPUT=1                                    DEC10121          
      GO TO 70                                                          DEC10122          
   68 MISS=MISN                                                         DEC10123          
      KPUT=1                                                            DEC10124          
   70 IF (INT2(I).GT.NOM) GO TO 74                                      DEC10125          
      ID4=IDEN(K5)                                                      DEC10126          
      GO TO 76                                                          DEC10127          
   74 ID4=NDEN(K5)                                                      DEC10128          
   76 WRITE (6,1004) ID1,ID2,ID3,ID4,NOS(L),MISS                        DEC10129          
 1004 FORMAT (1H ,17X,A6,3H X ,A6,2I5,I7,A8)                            DEC10130          
      MISS=IBLK                                                         DEC10131          
   80 L=L+1                                                             DEC10132          
   81 CONTINUE                                                          DEC10133          
      RETURN                                                            DEC10134          
      END                                                               DEC10135          
*ENDTEXT                                                                                  
