*TEXT                                                                                     
      SUBROUTINE SCLNOS(                                                  130001          
     1ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LAB4,LITY,TRED,    130002          
     2YM,IM,MS,IPL,NSP,NND,XP,YP,NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NEN,   130003          
     3NCLN,LNE,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NBEG,NEGX,LOGE,LQC,       130004          
     4NREGP,LGTX,JBEG,NDECX,LITR,XM,NEGY,LNY,LHY,KBEG,NDECY,IC,EFF1,      130005          
     5EFF2,NOS,X,NMAC,IED)                                                                
C     SUBROUTINE TO LIST DISTRIBUTION OF CLASS AND SUBCLASS NUMBERS       130007          
C     ------------------------------------------------------              130008          
      DIMENSION ARRAY(1),SSCPR(1),SSS(1),RHM(1),TOT(1),TOT2(1),TOT3(1),   130009          
     1LAB1(1),LAB2(1),LAB3(1),LAB4(1),LITY(1),TRED(1),YM(1),IM(1),MS(1),  130010          
     2IPL(1),NSP(1),NND(1),XP(1),YP(1),NDC(1),NMI(1),MEN(1),NCL(1),LME(1  130011          
     3),IBEG(1),IDEN(1),LIT(1),NEN(1),NCLN(1),LNE(1),NDEN(1),NLIT(1),     130012          
     4NMA(1),NMC(1),INT1(1),INT2(1),MSCL(1),NBEG(1),NEGX(1),LOGE(1),      130013          
     5LQC(1),NREGP(1),LGTX(1),JBEG(1),NDECX(1),LITR(1),XM(1),NEGY(1),     130014          
     6LNY(1),LHY(1),KBEG(1),NDECY(1),IC(1),EFF1(1),EFF2(1),NOS(1),X(1),   130015          
     7NMAC(1),IED(1)                                                                      
      DIMENSION NEQ(10)                                                                   
      COMMON /CMBLK1/NEQ                                                  130018          
      COMMON /CMBLK2/MXNLHM,MXNRHM,MXNCAS,MXNCF,MXMN2P,MXNS2,MXNSME,      130019          
     1 MXNMJC,MXNMIC,MXME,MXMECL,MXNE,MXNECL,MX2F,MX2FMS,MXNPR,MXNCD,     130020          
     2 MXK9,MXNOS,MXNED ,MXI2                                             130021          
      COMMON /CMBLK3/IJOB,NAB,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F,NPR,IRAN,M  130022          
     1POP,LIOP,IAD,REP,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,NOM,NON,ML,MLB,NS  130023          
     2,NOT,DF,KPUT,SDF,EDF,WK,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,RR         130024          
     3,IN,NED                                                             130025          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3,   130026          
     1L,L7                                                                130027          
      DATA(IBLK=6H      )                                                 130028          
      DATA(MIS =6HMISS  )                                                 130029          
      DATA(MISN=6HMISS N)                                                 130030          
      DATA(MISI=6HMISS I)                                                 130031          
      WRITE (6,1001) IJOB                                                 130032          
 1001 FORMAT (1H0,60H  DISTRIBUTION OF CLASS AND SUBCLASS NUMBERS FOR PR  130033          
     1OBLEM NO.,I4)                                                       130034          
      WRITE (6,1002)                                                      130035          
 1002 FORMAT (1H0,19X,14HIDENTIFICATION,13X,3HNO.)                        130036          
      L=1                                                                 130037          
      K=ML+1                                                              130038          
      MISS=IBLK                                                           130039          
      K1=NMEA+1                                                           130040          
      IF (K1.GT.NOM) GO TO 17                                             130041          
      DO 16 I=K1,NOM                                                      130042          
      WRITE (6,1000)                                                      130043          
 1000 FORMAT (1H )                                                        130044          
      K2=NCL(I)                                                           130045          
      DO 16 J=1,K2                                                        130046          
      IF (NOS(L).NE.0) GO TO 10                                           130047          
      MISS=MIS                                                            130048          
      KPUT=1                                                              130049          
   10 WRITE (6,1003) LIT(I),IDEN(K),NOS(L),MISS                           130050          
 1003 FORMAT (1H ,17X,A6,I5,16X,I5,A8)                                    130051          
      K=K+1                                                               130052          
      MISS=IBLK                                                           130053          
      IF(MXNOS-L) 100,16,16                                                               
  100 WRITE(6,103)                                                                        
  103 FORMAT(15H0MXNOS EXCEEDED)                                                          
      KPUT=1                                                                              
   16 L=L+1                                                               130054          
   17 K=MLB+1                                                             130055          
      K1=NNEA+1                                                           130056          
      IF (K1.GT.NON) GO TO 23                                             130057          
      DO 22 I=K1,NON                                                      130058          
      WRITE (6,1000)                                                      130059          
      K2=NCLN(I)                                                          130060          
      DO 22 J=1,K2                                                        130061          
      IF (NOS(L).NE.0) GO TO 20                                           130062          
      MISS=MIS                                                            130063          
      KPUT=1                                                              130064          
   20 WRITE (6,1003) NLIT(I),NDEN(K),NOS(L),MISS                          130065          
      MISS=IBLK                                                           130066          
      K=K+1                                                               130067          
      IF(MXNOS-L) 104,22,22                                                               
  104 WRITE(6,103)                                                                        
      KPUT=1                                                                              
   22 L=L+1                                                               130068          
   23 K6=0                                                                130069          
      IF (N2F.EQ.0) GO TO 81                                              130070          
      DO 80 I=1,N2F                                                       130071          
      WRITE (6,1000)                                                      130072          
      K6=K6+NMC(I)                                                        130073          
      NSUM=0                                                              130074          
      MSUM=0                                                              130075          
      IF (INT1(I).GT.NOM) GO TO 34                                        130076          
      K1=INT1(I)-1                                                        130077          
      IF (K1.EQ.0) GO TO 31                                               130078          
      DO 30 J=1,K1                                                        130079          
   30 NSUM=NSUM+NCL(J)                                                    130080          
   31 K1=INT1(I)                                                          130081          
      K3=NCL(K1)                                                          130082          
      ID1=LIT(K1)                                                         130083          
      GO TO 40                                                            130084          
   34 K1=NOM+1                                                            130085          
      K2=INT1(I)-1                                                        130086          
      IF (K1.GT.K2) GO TO 37                                              130087          
      DO 36 J=K1,K2                                                       130088          
      K=J-NOM                                                             130089          
   36 NSUM=NSUM+NCLN(K)                                                   130090          
   37 K1=INT1(I)-NOM                                                      130091          
      K3=NCLN(K1)                                                         130092          
      ID1=NLIT(K1)                                                        130093          
   40 IF (INT2(I).GT.NOM) GO TO 46                                        130094          
      K2=INT2(I)-1                                                        130095          
      IF (K2.EQ.0) GO TO 43                                               130096          
      DO 42 J=1,K2                                                        130097          
   42 MSUM=MSUM+NCL(J)                                                    130098          
   43 K2=INT2(I)                                                          130099          
      K4=NCL(K2)                                                          130100          
      ID2=LIT(K2)                                                         130101          
      GO TO 50                                                            130102          
   46 K1=NOM+1                                                            130103          
      K2=INT2(I)-1                                                        130104          
      IF (K1.GT.K2) GO TO 49                                              130105          
      DO 48 J=K1,K2                                                       130106          
      K=J-NOM                                                             130107          
   48 MSUM=MSUM+NCLN(K)                                                   130108          
   49 K1=INT2(I)-NOM                                                      130109          
      K4=NCLN(K1)                                                         130110          
      ID2=NLIT(K1)                                                        130111          
   50 DO 80 J=1,K3                                                        130112          
      K2=NSUM+J                                                           130113          
      IF (INT1(I).GT.NOM) GO TO 54                                        130114          
      ID3=IDEN(K2)                                                        130115          
      GO TO 56                                                            130116          
   54 ID3=NDEN(K2)                                                        130117          
   56 DO 80 K=1,K4                                                        130118          
      K5=MSUM+K                                                           130119          
      IF (NOS(L).NE.0) GO TO 70                                           130120          
      MISS=MISN                                                           130121          
      IF (NMC(I).EQ.0) KPUT=1                                             130122          
      IF (NMC(I).EQ.0) GO TO 70                                           130123          
      K7=J*1000+K                                                         130124          
      K1=K6-NMC(I)                                                        130125          
   60 K1=K1+1                                                             130126          
      IF (K1.GT.K6) GO TO 68                                              130127          
      IF (K7-MSCL(K1)) 60,64,60                                           130128          
   64 MISS=MISI                                                           130129          
      IF (J.EQ.K3.OR.K.EQ.K4) KPUT=1                                      130130          
      GO TO 70                                                            130131          
   68 MISS=MISN                                                           130132          
      KPUT=1                                                              130133          
   70 IF (INT2(I).GT.NOM) GO TO 74                                        130134          
      ID4=IDEN(K5)                                                        130135          
      GO TO 76                                                            130136          
   74 ID4=NDEN(K5)                                                        130137          
   76 WRITE (6,1004) ID1,ID2,ID3,ID4,NOS(L),MISS                          130138          
 1004 FORMAT (1H ,17X,A6,3H X ,A6,2I5,I7,A8)                              130139          
      MISS=IBLK                                                           130140          
      IF(MXNOS-L) 105,80,80                                                               
  105 WRITE(6,103)                                                                        
      KPUT=1                                                                              
   80 L=L+1                                                               130141          
   81 CONTINUE                                                            130142          
      RETURN                                                              130143          
      END                                                                 130144          
*ENDTEXT                                                                                  
