*TEXT                                                                                     
      SUBROUTINE SVCVC (                                                  160001          
     1ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LAB4,LITY,TRED,    160002          
     2YM,IM,MS,IPL,NSP,NND,XP,YP,NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NEN,   160003          
     3NCLN,LNE,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NBEG,NEGX,LOGE,LQC,       160004          
     4NREGP,LGTX,JBEG,NDECX,LITR,XM,NEGY,LNY,LHY,KBEG,NDECY,IC,EFF1,      160005          
     5EFF2,NOS,X,NMAC,IED)                                                                
C     SUBROUTINE USED TO COMPUTE VARIANCE AND COVARIANCE COMPONENTS,      160007          
C     HERITABILITIES, GENETIC, PHENOTYPIC AND ENVIRONMENTAL CORRELATIONS  160008          
C     AND STANDARD ERRORS                                                 160009          
C     ----------------------------------------------------                160010          
      DIMENSION ARRAY(1),SSCPR(1),SSS(1),RHM(1),TOT(1),TOT2(1),TOT3(1),   160011          
     1LAB1(1),LAB2(1),LAB3(1),LAB4(1),LITY(1),TRED(1),YM(1),IM(1),MS(1),  160012          
     2IPL(1),NSP(1),NND(1),XP(1),YP(1),NDC(1),NMI(1),MEN(1),NCL(1),LME(1  160013          
     3),IBEG(1),IDEN(1),LIT(1),NEN(1),NCLN(1),LNE(1),NDEN(1),NLIT(1),     160014          
     4NMA(1),NMC(1),INT1(1),INT2(1),MSCL(1),NBEG(1),NEGX(1),LOGE(1),      160015          
     5LQC(1),NREGP(1),LGTX(1),JBEG(1),NDECX(1),LITR(1),XM(1),NEGY(1),     160016          
     6LNY(1),LHY(1),KBEG(1),NDECY(1),IC(1),EFF1(1),EFF2(1),NOS(1),X(1),   160017          
     7NMAC(1),IED(1)                                                                      
      DIMENSION NEQ(10)                                                                   
      COMMON /CMBLK1/NEQ                                                  160020          
      COMMON /CMBLK2/MXNLHM,MXNRHM,MXNCAS,MXNCF,MXMN2P,MXNS2,MXNSME,      160021          
     1 MXNMJC,MXNMIC,MXME,MXMECL,MXNE,MXNECL,MX2F,MX2FMS,MXNPR,MXNCD,     160022          
     2 MXK9,MXNOS,MXNED ,MXI2                                             160023          
      COMMON /CMBLK3/IJOB,NAB,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F,NPR,IRAN,M  160024          
     1POP,LIOP,IAD,REP,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,NOM,NON,ML,MLB,NS  160025          
     2,NOT,DF,KPUT,SDF,EDF,WK,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,RR         160026          
     3,IN,NED,IBOP                                                                        
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3,   160028          
     1L,L7                                                                160029          
      W=NW                                                                160030          
      R1=NR1                                                              160031          
      R1=R1/100.                                                          160032          
      W=W/100.                                                            160033          
      SK1=1.                                                              160034          
      C=1./R1                                                             160035          
      D=(1.-W)/R1                                                         160036          
      E=W/R1                                                              160037          
      TN=(SDF+1.)*WK                                                      160038          
      SK3=WK-1.                                                           160039          
      SK4=WK*WK                                                           160040          
      C1=(C*C*2.)*(TN-1.)                                                 160041          
      C2=SK4*SDF                                                          160042          
      C3=SK4*EDF                                                          160043          
      SK5=2.*SK3                                                          160044          
      C4=C3*SDF                                                           160045          
      IDENT=0                                                             160046          
      DO 10 K=1,MN2                                                       160047          
   10 SSS(K)=(SSS(K)-SK1*SSCPR(K))/WK                                     160048          
      WRITE (6,1000)                                                      160049          
      WRITE(6,5000)(NEQ(I),I=1,10)                                                        
 5000 FORMAT(1H0,20X,10A8)                                                                
      WRITE(6,5001)                                                                       
 1000 FORMAT (1H1/1H ,76HESTIMATES OF HERITABILITIES, GENETIC, PHENOTYPI  160050          
     1C, ENVIRONMENTAL CORRELATIONS)                                                      
 5001 FORMAT(/1H ,                       110H JOB ROW COL   RHM    RHM                    
     2 HERITABILITY  STANDARD  PHENOTYPIC  ENVIRONMENTAL NEG   VARIANCE   160052          
     3OR COV COMPONENTS/ 1H ,28X, 47HOR GENETIC R   ERRORS   CORRELATION  160053          
     4 CORRELATION,11X,4HSIRE,10X,8HHALF-SIB)                             160054          
      DO 60 I=1,NRHM                                                      160055          
      WRITE (6,1001)                                                      160056          
 1001 FORMAT (1H )                                                        160057          
      DO 60 J=I,NRHM                                                      160058          
      IF (I-J) 16,12,50                                                   160059          
   12 K=NRHM*(I-1)-I*(I-3)/2                                              160060          
      H=(SSS(K)*C)/(D*SSS(K)+SSCPR(K))                                    160061          
      T=SSS(K)/(SSS(K)+SSCPR(K))                                          160062          
      T1=1.-T                                                             160063          
      SEH=SQRT((C1*T1*T1*(1.+(SK3*T))*(1.+(SK3*T)))/C4)                   160064          
      RP=1.                                                               160065          
      RE=1.                                                               160066          
      NEG=0                                                               160067          
      GO TO 46                                                            160068          
   16 K=NRHM*(I-1)-I*(I-3)/2                                              160069          
      K1=NRHM*(J-1)-J*(J-3)/2                                             160070          
      K2=K+J-I                                                            160071          
      SUM=SSS(K)*SSS(K1)                                                  160072          
      IF (SUM) 18,18,20                                                   160073          
   18 H=0.0                                                               160074          
      SEH=0.                                                              160075          
      GO TO 22                                                            160076          
   20 SUM=SQRT(SUM)                                                       160077          
      H=SSS(K2)/SUM                                                       160078          
   22 DEM1=(SSCPR(K)+(D*SSS(K)))*(SSCPR(K1)+(D*SSS(K1)))                  160079          
      IF (DEM1.GT.0.0) GO TO 26                                           160080          
   24 RP=0.0                                                              160081          
      GO TO 28                                                            160082          
   26 RP=(SSCPR(K2)+(D*SSS(K2)))/SQRT(DEM1)                               160083          
   28 RG=H*H                                                              160084          
      IF (H) 30,40,30                                                     160085          
   30 T2=SSS(K1)/(SSS(K1)+SSCPR(K1))                                      160086          
      T3=T*T2                                                             160087          
      IF (D-1.) 32,34,32                                                  160088          
   32 RP1=(SSCPR(K2)+SSS(K2))/SQRT((SSCPR(K)+SSS(K))*(SSCPR(K1)+SSS(K1))  160089          
     1)                                                                   160090          
      GO TO 36                                                            160091          
   34 RP1=RP                                                              160092          
   36 A1=1.+RG                                                            160093          
      B1=SQRT(T3*RG)                                                      160094          
      C5=(T+T2)/T3                                                        160095          
      D1=(RG*((T-T2)**2))/(2.*T3)                                         160096          
      TK=(1.+(SK3*T))*(1.+(SK3*T2))                                       160097          
      RPB=RP1+(SK3*B1)                                                    160098          
      R=RP1-B1                                                            160099          
      SERG1=((A1*(TK+(RPB*RPB)))-(2.*B1*RPB*(C5+SK5))+D1)/(C2*T3)         160100          
      SERG2=((A1*(T1*(1.-T2)+(R**2)))-(2.*B1*R*(C5-2.))+D1)/(C3+T3)       160101          
      SEH=SQRT(SERG1+SERG2)                                               160102          
   40 DEM2=(SSCPR(K)-(E*SSS(K)))*(SSCPR(K1)-(E*SSS(K1)))                  160103          
      IF (DEM2) 42,44,44                                                  160104          
   42 DEM2=-DEM2                                                          160105          
      RE=(SSCPR(K2)-(E*SSS(K2)))/(SQRT(DEM2)*(-1.))                       160106          
      NEG=1                                                               160107          
      GO TO 48                                                            160108          
   44 RE=(SSCPR(K2)-(E*SSS(K2)))/SQRT(DEM2)                               160109          
      NEG=0                                                               160110          
      GO TO 48                                                            160111          
   46 K2=K                                                                160112          
   48 WRITE (6,1002) IJOB,I,J,LITY(I),LITY(J),H,SEH,RP,RE,NEG,SSS(K2),SS  160113          
     1CPR(K2)                                                             160114          
 1002 FORMAT (1H ,3I4,A8,A7,4F11.3,I9,2F18.8)                             160115          
      IF(IBOP)2000,2000,2001                                                              
 2001 WRITE(IBOP)I,J,K,LITY(I),LITY(J),H,SEH,RP,RE,NEG,SSS(K2),SSCPR(K2)                  
 2000 CONTINUE                                                                            
      GO TO 60                                                            160116          
   50 WRITE (6,1003)                                                      160117          
 1003 FORMAT (1H0,5HERROR)                                                160118          
   60 CONTINUE                                                            160119          
      RETURN                                                              160120          
      END                                                                 160121          
*ENDTEXT                                                                                  
