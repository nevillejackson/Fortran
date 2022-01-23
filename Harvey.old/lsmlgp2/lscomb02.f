*TEXT                                                                                     
      SUBROUTINE LSCOMB(                                                  140001          
     1ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LAB4,LITY,TRED,    140002          
     2YM,IM,MS,IPL,NSP,NND,XP,YP,NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NEN,   140003          
     3NCLN,LNE,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NBEG,NEGX,LOGE,LQC,       140004          
     4NREGP,LGTX,JBEG,NDECX,LITR,XM,NEGY,LNY,LHY,KBEG,NDECY,IC,EFF1,      140005          
     5EFF2,NOS,X,NMAC,IED)                                                                
C     SUBROUTINE WHICH CALLS MATINV, COMPUTES AND LISTS CONSTANTS AND     140007          
C     ANOVA.  ALSO CALLS POLYNO AND SVCVC IF NEEDED                       140008          
C     ----------------------------------------------------                140009          
      DIMENSION ARRAY(1),SSCPR(1),SSS(1),RHM(1),TOT(1),TOT2(1),TOT3(1),   140010          
     1LAB1(1),LAB2(1),LAB3(1),LAB4(1),LITY(1),TRED(1),YM(1),IM(1),MS(1),  140011          
     2IPL(1),NSP(1),NND(1),XP(1),YP(1),NDC(1),NMI(1),MEN(1),NCL(1),LME(1  140012          
     3),IBEG(1),IDEN(1),LIT(1),NEN(1),NCLN(1),LNE(1),NDEN(1),NLIT(1),     140013          
     4NMA(1),NMC(1),INT1(1),INT2(1),MSCL(1),NBEG(1),NEGX(1),LOGE(1),      140014          
     5LQC(1),NREGP(1),LGTX(1),JBEG(1),NDECX(1),LITR(1),XM(1),NEGY(1),     140015          
     6LNY(1),LHY(1),KBEG(1),NDECY(1),IC(1),EFF1(1),EFF2(1),NOS(1),X(1),   140016          
     7NMAC(1),IED(1)                                                                      
      DIMENSION NEQ(10),LP(5)                                                             
      COMMON /CMBLK1/NEQ                                                  140019          
      COMMON /CMBLK2/MXNLHM,MXNRHM,MXNCAS,MXNCF,MXMN2P,MXNS2,MXNSME,      140020          
     1 MXNMJC,MXNMIC,MXME,MXMECL,MXNE,MXNECL,MX2F,MX2FMS,MXNPR,MXNCD,     140021          
     2 MXK9,MXNOS,MXNED ,MXI2                                             140022          
      COMMON /CMBLK3/IJOB,NAB,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F,NPR,IRAN,M  140023          
     1POP,LIOP,IAD,REP,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,NOM,NON,ML,MLB,NS  140024          
     2,NOT,DF,KPUT,SDF,EDF,WK,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,RR         140025          
     3,IN,NED                                                             140026          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3,   140027          
     1L,L7                                                                140028          
      DATA(MUMYM=6HMU-XM )                                                140029          
      DATA(IBLK =6H      )                                                140030          
      DATA(IBLAB=6HB     )                                                140031          
      DATA(IXLAB=6HX     )                                                140032          
      DATA (LP(1)=6HLINEAR)                                               140033          
      DATA (LP(2)=6HQUAD  )                                               140034          
      DATA (LP(3)=6HCUBIC )                                               140035          
      DATA (LP(4)=6HQUARD )                                               140036          
      DATA (LP(5)=6HQUIN  )                                               140037          
C     ----------------------------------------------------                140038          
C     INVERSION AND LISTING OF INVERSE MATRIX                             140039          
C     ----------------------------------------------------                140040          
      NBRC=1                                                              140041          
      NERC=NLHM                                                           140042          
      CALL MATINV (ARRAY,NBRC,NERC,NLHM)                                  140043          
   24 WRITE (6,1001) IJOB                                                 140044          
      WRITE(6,5000)(NEQ(I),I=1,10)                                                        
 5000 FORMAT(1H0,20X,10A8)                                                                
      WRITE (6,1004)                                                      140045          
      IF (LIOP.NE.10) GO TO 26                                            140046          
      DO 5001 I=NBRC,NERC                                                                 
      K1=NLHM*(I-1)-I*(I-3)/2-I                                                           
      IX1=K1+I                                                                            
      IX2=K1+NERC                                                                         
 5001 WRITE(6,1025)(ARRAY(J),J=IX1,IX2)                                                   
      GO TO 19                                                            140048          
 1025 FORMAT (7(2X,E15.8))                                                140049          
 1001 FORMAT (1H1,29X,43HLISTING OF INVERSE ELEMENTS FOR PROBLEM NO.,I3)  140050          
   26 WRITE (6,1002)                                                      140051          
 1002 FORMAT (1H0,8HROW  COL,15X,21HINDEPENDENT VARIABLES,34X,15HINVERSE  140052          
     1 ELEMENT)                                                           140053          
      WRITE (6,1003)                                                      140054          
 1003 FORMAT (1H ,9HCODE CODE,9X,3HROW,22X,6HCOLUMN,16X,41HFIXED POINT F  140055          
     1ORMAT  FLOATING POINT FORMAT)                                       140056          
   28 DO 32 I=NBRC,NERC                                                   140057          
      WRITE (6,1004)                                                      140058          
 1004 FORMAT (1H )                                                        140059          
   30 DO 32 J=I,NERC                                                      140060          
      K1=NLHM*(I-1)-I*(I-3)/2+J-I                                         140061          
      WRITE (6,1005) I,J,LAB1(I),LAB2(I),LAB3(I),LAB4(I),LAB1(J),LAB2(J)  140062          
     1,LAB3(J),LAB4(J),ARRAY(K1),ARRAY(K1)                                140063          
 1005 FORMAT (1H ,2(I3,2X),2(A6,1X),2A6,2(A6,1X),2A6,2X,F17.8,8X,E15.8)   140064          
   32 CONTINUE                                                            140065          
C     ----------------------------------------------------                140066          
C     COMPUTATION AND LISTING OF CONSTANTS                                140067          
C     ----------------------------------------------------                140068          
   19 EDF=DF-FLOAT(NLHM)                                                  140069          
      IF (NAB.EQ.0.OR.NAB.EQ.3.AND.NMEA.EQ.0.AND.NNEA.EQ.0) GO TO 55      140070          
      WRITE (6,1006) IJOB                                                 140071          
 1006 FORMAT (1H1,18X,45HLISTING OF CONSTANT ESTIMATES FOR PROBLEM NO.,I  140072          
     13)                                                                  140073          
      WRITE(6,5000)(NEQ(I),I=1,10)                                                        
      WRITE (6,1007)                                                      140074          
 1007 FORMAT (1H0,10H RHM   ROW,43X,18HCONSTANT ESTIMATES/ 1H ,33H NAME   140075          
     1 CODE  INDEPENDENT VARIABLE,9X,41HFIXED POINT FORMAT  FLOATING POI  140076          
     2NT FORMAT)                                                          140077          
   55 DO 54 K=1,NRHM                                                      140078          
      WRITE (6,1004)                                                      140079          
   34 TREDS=0.0                                                           140080          
      DO 52 I=NBRC,NERC                                                   140081          
      TEMP=0.0                                                            140082          
      DO 44 J=NBRC,NERC                                                   140083          
      IF (I-J.LT.0) GO TO 38                                              140084          
   36 K1=NLHM*(J-1)-J*(J-3)/2+I-J                                         140085          
      GO TO 40                                                            140086          
   38 K1=NLHM*(I-1)-I*(I-3)/2+J-I                                         140087          
   40 K4=(K-1)*NLHM+J                                                     140088          
   44 TEMP=TEMP+RHM(K4)*ARRAY(K1)                                         140089          
      K4=NLHM*(K-1)+I                                                     140090          
   48 TREDS=TREDS+RHM(K4)*TEMP                                            140091          
      TRED(K)=TREDS                                                       140092          
      K1=MATX+(K-1)*NLHM+I                                                140093          
      ARRAY(K1)=TEMP                                                      140094          
      IF (NAB.EQ.0.OR.NAB.EQ.3.AND.NMEA.EQ.0.AND.NNEA.EQ.0) GO TO 52      140095          
      IF ((NAB.EQ.0.OR.NAB.EQ.3).AND.I.EQ.1) TEMP=TEMP+YM(K)              140096          
      WRITE (6,1008) LITY(K),I,LAB1(I),LAB2(I),LAB3(I),LAB4(I),TEMP,TEMP  140097          
 1008 FORMAT (1H ,A6,I4,2X,3(A6,1X),A5,2X,F17.8,8X,E15.8)                 140098          
   52 CONTINUE                                                            140099          
   54 CONTINUE                                                            140100          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 299                     140101          
      IF (NMEA.NE.0.OR.NNEA.NE.0) GO TO 299                               140102          
      CALL LSMNS (                                                        140103          
     1ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LAB4,LITY,TRED,    140104          
     2YM,IM,MS,IPL,NSP,NND,XP,YP,NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NEN,   140105          
     3NCLN,LNE,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NBEG,NEGX,LOGE,LQC,       140106          
     4NREGP,LGTX,JBEG,NDECX,LITR,XM,NEGY,LNY,LHY,KBEG,NDECY,IC,EFF1,      140107          
     5EFF2,NOS,X,NMAC,IED)                                                                
      IF (KPUT.EQ.1) GO TO 300                                            140109          
  299 IF (NLC.EQ.0) GO TO 53                                              140110          
      WRITE (6,1051)                                                      140111          
 1051 FORMAT (1H1,25X,56HLISTING OF SELECTED LINEAR FUNCTIONS AND STANDA  140112          
     1RD ERRORS)                                                          140113          
      WRITE(6,5000)(NEQ(I),I=1,10)                                                        
      WRITE (6,1052)                                                      140114          
 1052 FORMAT (1H0,2X,11HDESCRIPTION,7X,84HR(1) C(1) R(2) C(2) R(3) C(3)   140115          
     1R(4) C(4) R(5) C(5) R(6) C(6) R(7) C(7) R(8) C(8) ETC.)             140116          
      DO 301 K1=1,NLC                                                     140117          
      READ (5,1050) IA1,IA2,IA3,NERC,NBRC,(JBEG(J),TOT2(J),J=1,NERC)           1          
 1050 FORMAT(2A6,A2,2I3,10(I3,F3.0),3X/(13(I3,F3.0),2X))                   14011          
      IF(NERC-MXNCF) 2098,2098,2097                                                       
 2097 WRITE(6,2096)                                                                       
 2096 FORMAT(15H0MXNCF EXCEEDED)                                                          
      RETURN                                                                              
 2098 WRITE (6,1053) IA1,IA2,IA3,NBRC,(JBEG(J),TOT2(J),J=1,NERC)               1          
 1053 FORMAT(1H0,2A6,A2,2X,I3,2X,9(I3,1X,F4.0,2X)/17X,9(I3,1X,F4.0,2X))                   
      DO 302 J=1,NLHM                                                     140122          
  302 X(J)=0.                                                             140123          
      DO 303 J=1,NERC                                                     140124          
      K=JBEG(J)                                                           140125          
  303 X(K)=TOT2(J)                                                        140126          
      DO 309 J=1,NLHM                                                     140127          
  309 TOT2(J)=0.                                                          140128          
      DO 304 I=1,NRHM                                                     140130          
      NCT=1                                                                               
      YT=YM(I)                                                            140131          
      NR=MATX+(I-1)*NLHM+1                                                140132          
      IF (NCPR.EQ.1) GO TO 305                                            140133          
      K4=I                                                                140134          
      GO TO 306                                                           140135          
  305 K4=NRHM*(I-1)-I*(I-3)/2                                             140136          
  306 WK=(SSCPR(K4)-TRED(I))/EDF                                          140137          
      IF (NAB.GT.2) WK=WK/(RR+1.)                                         140138          
      IF (WK.LT.0.) GO TO 300                                             140139          
  308 CALL CANDSE (AC,ALS,REP,TOT2,X,ARRAY,NR,NLHM,WK,YT,I,SSS,NCT)       140140          
      IF(JBEG(1).EQ.1) 315,316                                                            
  316 ALS=(ALS-YT)/NBRC                                                                   
      REP=REP/NBRC                                                                        
      GO TO 304                                                                           
  315 ALS=ALS+YT*(X(1)-1.0)                                                               
  304 WRITE (6,1054) LITY(I),ALS,REP                                      140141          
  301 CONTINUE                                                            140142          
 1054 FORMAT (1H0,A6,5X,17HLINEAR FUNCTION =,F17.8,4X,16HSTANDARD ERROR   140143          
     1=,F17.8)                                                            140144          
C     ----------------------------------------------------                140145          
C     COMPUTATION AND LISTING, IF DESIRED, OF INVERSE ELEMENTS            140146          
C             OF SEGMENTS                                                 140147          
C     ----------------------------------------------------                140148          
   53 NERC=0                                                              140149          
      WRITE (6,1000) IJOB                                                 140151          
 1000 FORMAT(1H1,10X,71HLISTING OF INVERSE ELEMENTS OF SEGMENTS AND/OR K                  
     1-VALUES FOR PROBLEM NO.,I3)                                                         
      WRITE(6,5000)(NEQ(I),I=1,10)                                                        
      WRITE(6,3022)                                                                       
 3022 FORMAT(1H0,8HROW  COL,15X,21HINDEPENDENT VARIABLES,26X,                             
     1 32HELEMENTS OF RE-INVERTED SEGMENTS,15X,8HK-VALUES)                                
      WRITE (6,1003)                                                      140155          
  221 DO 58 I2=1,NS                                                       140156          
      NBRC=NERC+1                                                         140157          
      NERC=IM(I2)                                                         140158          
      SUMD=0.0                                                                            
      SUMO=0.0                                                                            
      CALL MATINV (ARRAY,NBRC,NERC,NLHM)                                  140159          
      DO 220 I=NBRC,NERC                                                  140161          
      IF(LIOP.LT.3.OR.LIOP.EQ.10) GO TO 2099                                              
      WRITE (6,1004)                                                      140162          
 2099 DO 220 J=I,NERC                                                     140163          
      K1=NLHM*(I-1)-I*(I-3)/2+J-I                                         140164          
      IF(J-I) 2133,2134,2133                                                              
 2134 SUMD=SUMD+ARRAY(K1)                                                                 
      GO TO 2135                                                                          
 2133 SUMO=SUMO+2.*ARRAY(K1)                                                              
 2135 IF(LIOP.LT.3.OR.LIOP.EQ.10) GO TO 220                                               
      WRITE (6,1005) I,J,LAB1(I),LAB2(I),LAB3(I),LAB4(I),LAB1(J),LAB2(J)  140165          
     1,LAB3(J),LAB4(J),ARRAY(K1),ARRAY(K1)                                140166          
  220 CONTINUE                                                            140167          
      WK=SUMD-SUMO/(NERC-NBRC+1)                                                          
      IF(I2.EQ.1.OR.I2.GT.(1+NME+NNE+N2F))3005,3006                                       
 3006 IF(I2.GT.1.AND.I2.LE.(1+NME+NNE))3007,3008                                          
 3008 IF(I2.GT.(1+NME+NNE).AND.I2.LE.(1+NME+NNE+N2F))3000,3009                            
 3005 NDEL=0                                                                              
      GO TO 3001                                                                          
 3007 NDEL=1                                                                              
      GO TO 3001                                                                          
 3009 KPUT=1                                                                              
      WRITE(6,3010) I2                                                                    
      RETURN                                                                              
 3010 FORMAT(//,10H     I2 = ,I3,13H  NOT LOCATED)                                        
 3000 DO 3002 IXZ=1,N2F                                                                   
      IF(I2.EQ.(1+NME+NNE+IXZ))3003,3002                                                  
 3003 IXY=IXZ                                                                             
      GO TO 3004                                                                          
 3002 CONTINUE                                                                            
 3004 IX1=INT1(IXY)                                                                       
      IX2=INT2(IXY)                                                                       
      IF(IX1.LE.NME.AND.IX2.LE.NME)3011,3012                                              
 3012 IF(IX1.LE.NME.AND.IX2.GT.NME)3013,3014                                              
 3014 IF(IX1.GT.NME.AND.IX2.LE.NME)3015,3016                                              
 3016 IF(IX1.GT.NME.AND.IX2.GT.NME)3017,3018                                              
 3018 KPUT=1                                                                              
      WRITE(6,3019) IX1,IX2                                                               
      RETURN                                                                              
 3019 FORMAT(//,11H     IX1 = ,I3,13H  AND  IX2 = ,I3,11H  NOT FOUND)                     
 3011 NDEL=NCL(IX1)+NCL(IX2)-1                                                            
      GO TO 3001                                                                          
 3013 NDEL=NCL(IX1)+NCLN(IX2-NME)-1                                                       
      GO TO 3001                                                                          
 3015 NDEL=NCLN(IX1-NME)+NCL(IX2)-1                                                       
      GO TO 3001                                                                          
 3017 NDEL=NCLN(IX1-NME)+NCLN(IX2-NME)-1                                                  
      GO TO 3001                                                                          
 3001 CK=WK/(NERC-NBRC+1+NDEL)                                                            
      WRITE(6,2136)NERC,NERC,LAB1(NERC),LAB2(NERC),LAB3(NERC),LAB4(NERC)                  
     1 ,LAB1(NERC),LAB2(NERC),LAB3(NERC),LAB4(NERC),CK                                    
 2136 FORMAT(1H ,2(I3,2X),2(A6,1X),2A6,2(A6,1X),2A6,46X,F20.8)                            
   58 CONTINUE                                                            140168          
C     ----------------------------------------------------                140169          
C     COMPUTATION AND LISTING OF RESIDUAL MATRIX FOR RHM                  140170          
C     ----------------------------------------------------                140171          
      IF (NCPR.EQ.0.AND.I309.EQ.0) GO TO 77                               140172          
      WRITE(6,1009)(NEQ(I),I=1,10)                                                        
 1009 FORMAT (1H1,22X,40HRESIDUAL MATRICES FOR RIGHT HAND MEMBERS// 1H ,  140174          
     1 20X,10A8//1H ,                                                                     
     227H JOB ROW COL   RHM      RHM,11X,14HERROR SS OR CP,7X,15HERROR M  140175          
     3S OR COV,5X,11HCORRELATION)                                         140176          
      DO 74 K=1,NRHM                                                      140177          
      WRITE (6,1004)                                                      140178          
      K3=NRHM*(K-1)-K*(K-3)/2                                             140179          
      K1=(K-1)*NLHM                                                       140180          
      DO 74 L=K,NRHM                                                      140181          
      K2=(L-1)*NLHM                                                       140182          
      TR=0.0                                                              140183          
      DO 64 I=1,NLHM                                                      140184          
      K4=K1+I                                                             140185          
      K5=MATX+K2+I                                                        140186          
   64 TR=TR+ARRAY(K5)*RHM(K4)                                             140187          
      J=K3+L-K                                                            140188          
      ESS=SSCPR(J)-TR                                                     140189          
      EMS=ESS/EDF                                                         140190          
      SSCPR(J)=EMS                                                        140191          
      IF (K.EQ.L) GO TO 72                                                140192          
      AK=0.0                                                              140193          
      DO 68 I=1,NLHM                                                      140194          
      K4=K2+I                                                             140195          
      K5=MATX+K4                                                          140196          
   68 AK=AK+ARRAY(K5)*RHM(K4)                                             140197          
      JJ=NRHM*(L-1)-L*(L-3)/2                                             140198          
      SM=(SSCPR(JJ)-AK)/EDF                                               140199          
      F=EMS/SQRT(SSCPR(K3)*SM)                                            140200          
      GO TO 74                                                            140201          
   72 F=1.                                                                140202          
   74 WRITE (6,1010) IJOB,K,L,LITY(K),LITY(L),ESS,EMS,F                   140203          
 1010 FORMAT (1H ,I4,I3,I4,2X,2A8,F22.6, 3X,F20.8,F14.4)                  140204          
C     ----------------------------------------------------                140205          
C     CALLS POLYNO SUBROUTINE IF POLYNOMIALS ARE TO BE FITTED             140206          
C     ----------------------------------------------------                140207          
   77 IF (NSME.EQ.0) GO TO 76                                             140208          
      CALL POLYNO(                                                        140209          
     1ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LAB4,LITY,TRED,    140210          
     2YM,IM,MS,IPL,NSP,NND,XP,YP,NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NEN,   140211          
     3NCLN,LNE,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NBEG,NEGX,LOGE,LQC,       140212          
     4NREGP,LGTX,JBEG,NDECX,LITR,XM,NEGY,LNY,LHY,KBEG,NDECY,IC,EFF1,      140213          
     5EFF2,NOS,X,NMAC,IED)                                                                
C     ----------------------------------------------------                140215          
C     COMPUTES AND LISTS SS, MS AND F FOR ANOVA FOR EACH RHM              140216          
C     ----------------------------------------------------                140217          
   76 WRITE (6,1011)                                                      140218          
 1011 FORMAT (1H1,28X,34HLEAST-SQUARES ANALYSIS OF VARIANCE)              140219          
      IF (NAB.EQ.0.OR.NAB.EQ.3) LAB1(1)=MUMYM                             140220          
      WRITE(6,5000)(NEQ(I),I=1,10)                                                        
      DO 108 K=1,NRHM                                                     140221          
      K5=0                                                                140222          
      WRITE (6,1012) LITY(K)                                              140223          
 1012 FORMAT (1H / 42X,A6)                                                140224          
      WRITE (6,1013)                                                      140225          
 1013 FORMAT (1H ,/ 5X,6HSOURCE,8X,4HD.F.,7X,14HSUM OF SQUARES,6X,12HMEA  140226          
     1N SQUARES,10X,1HF)                                                  140227          
      DO 80 I=1,NLHM                                                      140228          
      K1=MATX+(K-1)*NLHM+I                                                140229          
   80 RHM(I)=ARRAY(K1)                                                    140230          
      K6=DF                                                               140231          
   82 WRITE (6,1004)                                                      140232          
      K2=1                                                                140233          
      IF (NCPR.EQ.0.AND.I309.EQ.0) GO TO 84                               140234          
      K1=NRHM*(K-1)-K*(K-3)/2                                             140235          
      SS=SSCPR(K1)*EDF+TRED(K)                                            140236          
      GO TO 86                                                            140237          
   84 SS=SSCPR(K)                                                         140238          
   86 IF (NAB.GT.1) GO TO 22                                              140239          
      WRITE (6,1014) K6,SS                                                140240          
 1014 FORMAT (1H ,5HTOTAL,13X,I5,F20.6,F18.6,F13.3)                       140241          
      GO TO 21                                                            140242          
   22 WRITE (6,1026) K6,SS                                                140243          
 1026 FORMAT (1H ,9HWITHIN SS,9X,I5,F20.6,1X,F17.6,2X,F11.3)              140244          
   21 K6=K6-NLHM                                                          140245          
      EDF=K6                                                              140246          
      R=SS-TRED(K)                                                        140247          
      SD=R                                                                140248          
      SM=R/EDF                                                            140249          
      IF (NAB.GT.2) SM=SM/(RR+1.)                                         140250          
      K3=2                                                                140251          
      RECIP=NLHM                                                          140252          
      RECIP=TRED(K)/RECIP                                                 140253          
      F=RECIP/SM                                                          140254          
      ALS=ALS/NBRC                                                                        
      REP=REP/NBRC                                                                        
      WRITE (6,1015) NLHM,TRED(K),RECIP,F                                 140255          
 1015 FORMAT (1H ,15HTOTAL REDUCTION,I8,F20.6,F18.6,F13.3)                140256          
      K3=3                                                                140257          
      NERC=0                                                              140258          
      L2=1                                                                140259          
      DO 104 I3=1,NS                                                      140260          
      NBRC=NERC+1                                                         140261          
      NERC=IM(I3)                                                         140262          
      TREDS=0.0                                                           140263          
      DO 212 I=NBRC,NERC                                                  140264          
      TEMP=0.0                                                            140265          
      DO 209 J=NBRC,NERC                                                  140266          
      IF (I-J.LT.0) GO TO 204                                             140267          
  202 K1=NLHM*(J-1)-J*(J-3)/2+I-J                                         140268          
      GO TO 208                                                           140269          
  204 K1=NLHM*(I-1)-I*(I-3)/2+J-I                                         140270          
  208 K4=J                                                                140271          
  209 TEMP=TEMP+RHM(K4)*ARRAY(K1)                                         140272          
      K4=I                                                                140273          
  210 TREDS=TREDS+RHM(K4)*TEMP                                            140274          
  212 CONTINUE                                                            140275          
   94 K6=NERC-NBRC+1                                                      140276          
      K5=K5+K6                                                            140277          
      IF (K5.GE.IEI.AND.K5.LT.IE) GO TO 98                                140278          
      I2=IBLK                                                             140279          
      GO TO 100                                                           140280          
   98 I2=IXLAB                                                            140281          
      LAB3(K5)=LAB2(K5)                                                   140282          
  100 IF (K5.LT.IE) GO TO 102                                             140283          
      I2=IBLAB                                                            140284          
      LAB1(K5)=LAB2(K5)                                                   140285          
  102 R=K6                                                                140286          
      SS=TREDS/R                                                          140287          
      R=SS/SM                                                             140288          
      WRITE (6,1016) LAB1(K5),I2,LAB3(K5),K6,TREDS,SS,R                   140289          
 1016 FORMAT (1H ,A6,1X,A1,A7,I6,2X,F20.6,F18.6,F13.3)                    140290          
      IF (L2.GT.NSME.OR.IM(I3).NE.NND(L2)) GO TO 104                      140291          
      L2=L2+1                                                             140292          
      SS=0.                                                               140293          
      DO 20 L=1,K6                                                        140294          
      IF (L.GT.5) GO TO 20                                                140295          
      I=K*5+L+(L2-2)*NRHM*5                                               140296          
      R=SSS(I)/SM                                                         140297          
      WRITE (6,1027) LP(L),SSS(I),SSS(I),R                                140298          
 1027 FORMAT (1H ,2X,A6,10X,1H1,4X,F20.6,F18.6,F13.3)                     140299          
      SS=SS+SSS(I)                                                        140300          
   20 CONTINUE                                                            140301          
      IF (K6.LT.6) GO TO 104                                              140302          
      TREDS=TREDS-SS                                                      140303          
      I=K6-5                                                              140304          
      SS=TREDS/FLOAT(I)                                                   140305          
      R=SS/SM                                                             140306          
      WRITE (6,1028) I,TREDS,SS,R                                         140307          
 1028 FORMAT (1H ,2X,8HRESIDUAL,4X,I5,4X,F20.6,F18.6,F13.3)               140308          
  104 CONTINUE                                                            140309          
      K6=EDF                                                              140310          
      IF (NAB.GT.2) SM=SD/EDF                                             140311          
      WRITE (6,1017) K6,SD,SM                                             140312          
 1017 FORMAT (1H ,9HREMAINDER,9X,I5,F20.6,F18.6)                          140313          
      IF (NAB.LT.3) GO TO 310                                             140314          
      F=SM/(RR+1.)                                                        140315          
      WRITE (6,1030) F                                                    140316          
 1030 FORMAT (1H ,33HV(ERROR) = M.S. REMAINDER/(1+R) =,8X,F20.6)          140317          
  310 IF (NCPR.NE.0.OR.I309.NE.0) GO TO 108                               140318          
      K6=NRHM*(K-1)-K*(K-3)/2                                             140319          
      SSCPR(K6)=SM                                                        140320          
  108 CONTINUE                                                            140321          
C     ----------------------------------------------------                140322          
C     COMPUTATION AND LISTING OF K, SS, CP, MS, MCP AND VARIANCE AND      140323          
C     COVARIANCE COMPONENTS FOR ONE SET OF MAIN OR NESTED EFFECTS, IF     140324          
C     IRAN IS NOT EQUAL TO ZERO                                           140325          
C     ----------------------------------------------------                140326          
      IF (IRAN.EQ.0) GO TO 148                                            140327          
      IF ( NS2.EQ.0) GO TO 148                                            140328          
      MN2=NRHM*(NRHM+1)/2                                                 140329          
      SDF=0.0                                                             140330          
      AK=0.0                                                              140331          
      I3=0                                                                140332          
      DO 114 J=1,MN2                                                      140333          
  114 SSS(J)=0.0                                                          140334          
      DO 144 JJ=1,NS2                                                     140335          
  116 I3=I3+1                                                             140336          
      IF (IM(I3).NE.MS(JJ)) GO TO 116                                     140337          
      IF (I3.EQ.1) GO TO 120                                              140338          
      MRANK=IM(I3)-IM(I3-1)                                               140339          
      NBRC1=IM(I3-1)+1                                                    140340          
      GO TO 122                                                           140341          
  120 MRANK=IM(I3)                                                        140342          
      NBRC1=1                                                             140343          
  122 NERC1=IM(I3)                                                        140344          
      RANK=MRANK                                                          140345          
      I3=0                                                                140346          
      SOD=0.0                                                             140347          
      SD=0.0                                                              140348          
      DO 130 I=NBRC1,NERC1                                                140349          
      DO 130 J=I,NERC1                                                    140350          
      IF (I-J.LT.0) GO TO 128                                             140351          
      K=NLHM*(I-1)-I*(I-3)/2                                              140352          
      SD=SD+ARRAY(K)                                                      140353          
      GO TO 130                                                           140354          
  128 K=NLHM*(I-1)-I*(I-3)/2+J-I                                          140355          
      SOD=SOD+ARRAY(K)*2.                                                 140356          
  130 CONTINUE                                                            140357          
      AK=AK+((SD-(SOD/RANK))/(RANK+1.))*RANK                              140358          
      SDF=SDF+RANK                                                        140359          
      DO 142 K=1,NRHM                                                     140360          
      DO 142 I2=K,NRHM                                                    140361          
      SUM=0.0                                                             140362          
      DO 140 I=NBRC1,NERC1                                                140363          
      N3=MATX+(K-1)*NLHM+I                                                140364          
      DO 140 J=NBRC1,NERC1                                                140365          
      IF (I-J.LE.0) GO TO 136                                             140366          
      L=NLHM*(J-1)-J*(J-3)/2+I-J                                          140367          
      GO TO 138                                                           140368          
  136 L=NLHM*(I-1)-I*(I-3)/2+J-I                                          140369          
  138 N2=MATX+(I2-1)*NLHM+J                                               140370          
  140 SUM=SUM+ARRAY(N3)*ARRAY(L)*ARRAY(N2)                                140371          
      L2=NRHM*(K-1)-K*(K-3)/2+I2-K                                        140372          
  142 SSS(L2)=SSS(L2)+SUM                                                 140373          
  144 CONTINUE                                                            140374          
      WK=AK/SDF                                                           140375          
      WRITE (6,1018) WK,SDF                                               140376          
 1018 FORMAT (1H1,10X,32HK FOR RANDOM EFFECTS COMPONENT =,F8.4,3X,20HDEG  140377          
     1REES OF FREEDOM =,F5.0)                                             140378          
      WRITE(6,5000)(NEQ(I),I=1,10)                                                        
      WRITE (6,1019)                                                      140379          
 1019 FORMAT (1H0/25X,51HSS, CP, MS, MCP, VARIANCE AND COVARIANCE COMPON  140380          
     1ENTS//1H ,25HJOB ROW COL   RHM     RHM,20X,8HSS OR CP,19X,9HMS OR   140381          
     2COV,18X,10HCOMPONENTS)                                              140382          
      DO 146 I=1,NRHM                                                     140383          
      WRITE (6,1004)                                                      140384          
      DO 146 J=I,NRHM                                                     140385          
      K=NRHM*(I-1)-I*(I-3)/2+J-I                                          140386          
      SMS=SSS(K)/SDF                                                      140387          
      SOD=(SMS-SSCPR(K))/WK                                               140388          
      WRITE (6,1020) IJOB,I,J,LITY(I),LITY(J),SSS(K),SMS,SOD              140389          
 1020 FORMAT (1H ,I3,2I4,2A8,3F27.8)                                      140390          
  146 SSS(K)=SMS                                                          140391          
  148 IF (IRAN.EQ.0) GO TO 150                                            140392          
C     ----------------------------------------------------                140393          
C     CALLS SVCVC SUBROUTINE TO COMPUTE HERITABILITIES, GENETIC           140394          
C     CORRELATIONS, ETC., IF I309 IS NOT EQUAL TO ZERO                    140395          
C     ----------------------------------------------------                140396          
      IF (I309.EQ.0) GO TO 150                                            140397          
      CALL SVCVC (                                                        140398          
     1ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LAB4,LITY,TRED,    140399          
     2YM,IM,MS,IPL,NSP,NND,XP,YP,NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NEN,   140400          
     3NCLN,LNE,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NBEG,NEGX,LOGE,LQC,       140401          
     4NREGP,LGTX,JBEG,NDECX,LITR,XM,NEGY,LNY,LHY,KBEG,NDECY,IC,EFF1,      140402          
     5EFF2,NOS,X,NMAC,IED)                                                                
  150 CONTINUE                                                            140404          
      RETURN                                                              140405          
  300 WRITE (6,1021)                                                      140406          
 1021 FORMAT (1H0,10X,29HNEGATIVE ERROR SUM OF SQUARES)                   140407          
      RETURN                                                              140408          
      END                                                                 140409          
*ENDTEXT                                                                                  
