*TEXT                                                                                     
      SUBROUTINE CODEX (                                                  080001          
     1ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LAB4,LITY,TRED,    080002          
     2YM,IM,MS,IPL,NSP,NND,XP,YP,NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NEN,   080003          
     3NCLN,LNE,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NBEG,NEGX,LOGE,LQC,       080004          
     4NREGP,LGTX,JBEG,NDECX,LITR,XM,NEGY,LNY,LHY,KBEG,NDECY,IC,EFF1,      080005          
     5EFF2,NOS,X,NMAC,IED)                                                                
C     CODING SUBROUTINE - - SETS UP X ARRAY FOR EACH OBSERVATION          080007          
C     ----------------------------------------------------                080008          
      DIMENSION ARRAY(1),SSCPR(1),SSS(1),RHM(1),TOT(1),TOT2(1),TOT3(1),   080009          
     1LAB1(1),LAB2(1),LAB3(1),LAB4(1),LITY(1),TRED(1),YM(1),IM(1),MS(1),  080010          
     2IPL(1),NSP(1),NND(1),XP(1),YP(1),NDC(1),NMI(1),MEN(1),NCL(1),LME(1  080011          
     3),IBEG(1),IDEN(1),LIT(1),NEN(1),NCLN(1),LNE(1),NDEN(1),NLIT(1),     080012          
     4NMA(1),NMC(1),INT1(1),INT2(1),MSCL(1),NBEG(1),NEGX(1),LOGE(1),      080013          
     5LQC(1),NREGP(1),LGTX(1),JBEG(1),NDECX(1),LITR(1),XM(1),NEGY(1),     080014          
     6LNY(1),LHY(1),KBEG(1),NDECY(1),IC(1),EFF1(1),EFF2(1),NOS(1),X(1),   080015          
     7NMAC(1),IED(1)                                                                      
      DIMENSION NEQ(10)                                                                   
      COMMON /CMBLK1/NEQ                                                  080018          
      COMMON /CMBLK2/MXNLHM,MXNRHM,MXNCAS,MXNCF,MXMN2P,MXNS2,MXNSME,      080019          
     1 MXNMJC,MXNMIC,MXME,MXMECL,MXNE,MXNECL,MX2F,MX2FMS,MXNPR,MXNCD,     080020          
     2 MXK9,MXNOS,MXNED ,MXI2                                             080021          
      COMMON /CMBLK3/IJOB,NAB,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F,NPR,IRAN,M  080022          
     1POP,LIOP,IAD,REP,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,NOM,NON,ML,MLB,NS  080023          
     2,NOT,DF,KPUT,SDF,EDF,WK,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,RR         080024          
     3,IN,NED,IBOP                                                                        
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3,   080026          
     1L,L7                                                                080027          
      DO 1 I=1,NOT                                                        080028          
    1 X(I)=0.                                                             080029          
      N=1                                                                 080030          
      K=0                                                                 080031          
      IF (NAB.EQ.1.OR.NAB.EQ.2.OR.NAB.EQ.4) GO TO 2                       080032          
      X(1)=1                                                              080033          
      N=N+1                                                               080034          
C     ----------------------------------------------------                080035          
C     PLACES CODES IN X FOR MAIN EFFECTS                                  080036          
C     ----------------------------------------------------                080037          
    2 IF (NME.EQ.0) GO TO 11                                              080038          
      K1=NMEA+1                                                           080039          
      DO 10 I=K1,NOM                                                      080040          
      K=NCL(I)+K                                                          080041          
      L7=IBEG(I)                                                          080042          
      L=LME(I)+L7-1                                                       080043          
      J=NCDS                                                              080044          
      CALL FIELD (ICOD,L,IC,L7,J)                                         080045          
      IF (J.EQ.1) GO TO 902                                               080046          
      ML1=ML+K                                                            080047          
      ML2=ML1-NCL(I)                                                      080048          
      ML3=ML2                                                             080049          
      NEND=N+NCL(I)-1                                                     080050          
    4 ML3=ML3+1                                                           080051          
      IF (ML3.GT.ML1) GO TO 901                                           080052          
      IF (ICOD.NE.IDEN(ML3)) GO TO 4                                      080053          
      MLOC=ML3-ML                                                         080054          
      NOS(MLOC)=NOS(MLOC)+1                                               080055          
      ML3=ML3-ML2                                                         080056          
      MLOC=N+ML3-1                                                        080057          
      IF (ML3.EQ.NCL(I)) GO TO 6                                          080058          
      X(MLOC)=1.                                                          080059          
      GO TO 10                                                            080060          
    6 K2=NEND-1                                                           080061          
      DO 8 J=N,K2                                                         080062          
    8 X(J)=-1.                                                            080063          
   10 N=NEND                                                              080064          
C     ----------------------------------------------------                080065          
C     PLACES CODES IN X FOR NESTED MAIN EFFECTS                           080066          
C     ----------------------------------------------------                080067          
   11 J1=K                                                                080068          
      K=0                                                                 080069          
      IF (NNE.EQ.0) GO TO 19                                              080070          
      K1=NNEA+1                                                           080071          
      DO 18 I=K1,NON                                                      080072          
      K=NCLN(I)+K                                                         080073          
      L7=NBEG(I)                                                          080074          
      L=LNE(I)+L7-1                                                       080075          
      J=NCDS                                                              080076          
      CALL FIELD (ICOD,L,IC,L7,J)                                         080077          
      IF (J.EQ.1) GO TO 902                                               080078          
      ML1=MLB+K                                                           080079          
      ML2=ML1-NCLN(I)                                                     080080          
      ML3=ML2                                                             080081          
      NEND=N+NCLN(I)-1                                                    080082          
   12 ML3=ML3+1                                                           080083          
      IF (ML3.GT.ML1) GO TO 18                                            080084          
      IF (ICOD.NE.NDEN(ML3)) GO TO 12                                     080085          
      MLOC=J1+ML3-MLB                                                     080086          
      NOS(MLOC)=NOS(MLOC)+1                                               080087          
      ML3=ML3-ML2                                                         080088          
      MLOC=N+ML3-1                                                        080089          
      IF (ML3.EQ.NCLN(I)) GO TO 14                                        080090          
      X(MLOC)=1.                                                          080091          
      GO TO 18                                                            080092          
   14 K2=NEND-1                                                           080093          
      DO 16 J=N,K2                                                        080094          
   16 X(J)=-1.                                                            080095          
   18 N=NEND                                                              080096          
C     ----------------------------------------------------                080097          
C     PLACES CODES IN X FOR TWO-FACTOR INTERACTIONS                       080098          
C     ----------------------------------------------------                080099          
   19 J1=J1+K                                                             080100          
      K5=0                                                                080101          
      NSUB=0                                                              080102          
      IF (N2F.EQ.0) GO TO 51                                              080103          
      DO 50 I=1,N2F                                                       080104          
      INTV=INT1(I)                                                        080105          
      ISW4=0                                                              080106          
      CALL CODE  (                                                        080107          
     1ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LAB4,LITY,TRED,    080108          
     2YM,IM,MS,IPL,NSP,NND,XP,YP,NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NEN,   080109          
     3NCLN,LNE,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NBEG,NEGX,LOGE,LQC,       080110          
     4NREGP,LGTX,JBEG,NDECX,LITR,XM,NEGY,LNY,LHY,KBEG,NDECY,IC,EFF1,      080111          
     5EFF2,NOS,X,NMAC,IED)                                                                
      IF (L.EQ.1) GO TO 902                                               080113          
      IF (ISW2.NE.1) GO TO 20                                             080114          
      ISW4=1                                                              080115          
   20 IF (ISW3.EQ.1) GO TO 904                                            080116          
      ICON=ML3*1000                                                       080117          
      ME3=ML3                                                             080118          
      K3=K1                                                               080119          
      INTV=INT2(I)                                                        080120          
      CALL CODE  (                                                        080121          
     1ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LAB4,LITY,TRED,    080122          
     2YM,IM,MS,IPL,NSP,NND,XP,YP,NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NEN,   080123          
     3NCLN,LNE,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NBEG,NEGX,LOGE,LQC,       080124          
     4NREGP,LGTX,JBEG,NDECX,LITR,XM,NEGY,LNY,LHY,KBEG,NDECY,IC,EFF1,      080125          
     5EFF2,NOS,X,NMAC,IED)                                                                
      IF (L.EQ.1) GO TO 902                                               080127          
      IF (ISW3.EQ.1) GO TO 904                                            080128          
      ICON=ICON+ML3                                                       080129          
      ME2=ML3                                                             080130          
      K4=K1                                                               080131          
      K6=K5+NMC(I)                                                        080132          
      IF (ISW2.EQ.1.OR.ISW4.EQ.1) GO TO 48                                080133          
      MLOC=J1+NSUB+(ME3-1)*K1+ME2                                         080134          
      NOS(MLOC)=NOS(MLOC)+1                                               080135          
      K7=K6-NMC(I)                                                        080136          
   22 K7=K7+1                                                             080137          
      IF (K7.GT.K6) GO TO 24                                              080138          
      IF (ICON-MSCL(K7)) 22,48,22                                         080139          
   24 K9=K3-1                                                             080140          
      IF(MXK9-K9) 200,201,201                                             080141          
  200 WRITE(6,1100)                                                       080142          
 1100 FORMAT(14H0MXK9 EXCEEDED)                                           080143          
      MULL=1                                                              080144          
      RETURN                                                              080145          
  201 CONTINUE                                                            080146          
      DO 26 J=1,K9                                                        080147          
   26 EFF1(J)=0.0                                                         080148          
      K9=K4-1                                                             080149          
      IF(MXK9-K9) 200,203,203                                             080150          
  203 CONTINUE                                                            080151          
      DO 28 J=1,K9                                                        080152          
   28 EFF2(J)=0.0                                                         080153          
      IF (ME3.EQ.K3) GO TO 30                                             080154          
      EFF1(ME3)=1.                                                        080155          
      GO TO 34                                                            080156          
   30 K9=K3-1                                                             080157          
      DO 32 J=1,K9                                                        080158          
   32 EFF1(J)=-1.                                                         080159          
   34 IF (ME2.EQ.K4) GO TO 36                                             080160          
      EFF2(ME2)=1.                                                        080161          
      GO TO 40                                                            080162          
   36 K9=K4-1                                                             080163          
      DO 38 J=1,K9                                                        080164          
   38 EFF2(J)=-1.                                                         080165          
   40 L1=0                                                                080166          
      K9=K3-1                                                             080167          
      K2=K4-1                                                             080168          
      DO 46 J=1,K9                                                        080169          
      DO 46 J2=1,K2                                                       080170          
      ICO=J*1000+J2                                                       080171          
      IF (NMC(I).EQ.0) GO TO 44                                           080172          
      K7=K6-NMC(I)                                                        080173          
   42 K7=K7+1                                                             080174          
      IF (K7.GT.K6) GO TO 44                                              080175          
      IF (ICO-MSCL(K7)) 42,46,42                                          080176          
   44 L1=L1+1                                                             080177          
      MLOC=N+L1-1                                                         080178          
      X(MLOC)=EFF1(J)*EFF2(J2)                                            080179          
   46 CONTINUE                                                            080180          
   48 K5=K6                                                               080181          
      NSUB=NSUB+K3*K4                                                     080182          
   50 N=N+(K3-1)*(K4-1)-NMC(I)                                            080183          
C     ----------------------------------------------------                080184          
C     PLACES CODES (X-XM), ETC, IN X ARRAY FOR POOLED REGRESSIONS         080185          
C     ----------------------------------------------------                080186          
   51 IF (NPR.EQ.0) GO TO 81                                              080187          
   52 DO 80 I=1,NPR                                                       080188          
      IF (NEGX(I)-1) 58,54,56                                             080189          
   54 CALL NEGN (TX,JBEG,LGTX,IC,I)                                       080190          
      GO TO 58                                                            080191          
   56 CALL NEGNO (TX,JBEG,LGTX,IC,I)                                      080192          
   58 L7=JBEG(I)                                                          080193          
      L=LGTX(I)+L7-1                                                      080194          
      J=NCDS                                                              080195          
      CALL FIELD (ICOD,L,IC,L7,J)                                         080196          
      IF (J.EQ.1) GO TO 903                                               080197          
      XR=ICOD                                                             080198          
      IF (NEGX(I).EQ.0) GO TO 60                                          080199          
      XR=XR*TX                                                            080200          
   60 XR=XR/10.**NDECX(I)                                                 080201          
      IF (LOGE(I).EQ.0) GO TO 62                                          080202          
      XR=ALOG10(XR)-XM(I)                                                 080203          
      GO TO 68                                                            080204          
   62 XR=XR-XM(I)                                                         080205          
   68 YR=1.                                                               080206          
      K9=LQC(I)                                                           080207          
      DO 80 J=1,K9                                                        080208          
      X(N)=YR*XR                                                          080209          
      YR=X(N)                                                             080210          
   80 N=N+1                                                               080211          
C     ----------------------------------------------------                080212          
C     PLACES (Y-YM) IN X ARRAY FOR RHM                                    080213          
C     ----------------------------------------------------                080214          
   81 IF (NRHM.EQ.0)  GO TO 96                                            080215          
      DO 95 I=1,NRHM                                                      080216          
      IF (NEGY(I)-1) 86,82,84                                             080217          
   82 CALL NEGN (TX,KBEG,LHY,IC,I)                                        080218          
      GO TO 86                                                            080219          
   84 CALL NEGNO (TX,KBEG,LHY,IC,I)                                       080220          
   86 L7=KBEG(I)                                                          080221          
      L=LHY(I)+L7-1                                                       080222          
      J=NCDS                                                              080223          
      CALL FIELD (ICOD,L,IC,L7,J)                                         080224          
      IF (J.EQ.1) GO TO 903                                               080225          
      YR=ICOD                                                             080226          
      IF (NEGY(I).EQ.0) GO TO 88                                          080227          
      YR=YR*TX                                                            080228          
   88 YR=YR/(10.**NDECY(I))                                               080229          
      IF (LNY(I).EQ.0) GO TO 90                                           080230          
      YR=ALOG10(YR)-YM(I)                                                 080231          
      GO TO 92                                                            080232          
   90 YR=YR-YM(I)                                                         080233          
   92 X(N)=YR                                                             080234          
      N=N+1                                                               080235          
   95 CONTINUE                                                            080236          
   96 IF (N-1.NE.NOT) GO TO 900                                           080237          
      GO TO 100                                                           080238          
  900 WRITE (6,1000)                                                      080239          
 1000 FORMAT (1H0,46HNO. LHM PLUS RHM DO NOT AGREE WITH PAR. CARD 0)      080240          
      GO TO 904                                                           080241          
  901 WRITE (6,1001) NCDS,IJOB                                            080242          
 1001 FORMAT (1H0,30HCLASS CODE MISSING ON CARD NO.,I8,5X,37HCHECK PARAM  080243          
     1ETER CARDS FOR PROBLEM NO.,I3)                                      080244          
      GO TO 904                                                           080245          
  902 WRITE (6,1002) NCDS                                                 080246          
 1002 FORMAT (1H0,69HUNITS POSITION OF AN ID FIELD OR A CONTROL FIELD IS  080247          
     4 BLANK ON CARD NO.,I5)                                              080248          
      GO TO 904                                                           080249          
  903 WRITE (6,1003) NCDS                                                 080250          
 1003 FORMAT (1H0,55HUNITS POSITION OF AN X OR Y FIELD IS BLANK ON CARD   080251          
     1 NO.,I5)                                                            080252          
  904 MULL=1                                                              080253          
  100 CONTINUE                                                            080254          
      RETURN                                                              080255          
      END                                                                 080256          
*ENDTEXT                                                                                  
