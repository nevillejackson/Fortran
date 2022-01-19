*TEXT                                                                                     
      SUBROUTINE MIXED (NAB,MATX,KA,KB,KC,NOT,TOT,NMIG,NLHM,NCPR,NRHM,TODK060010          
     1T3,ARRAY,NCDG,FAB,TOT4,SOF,NX,KD)                                 DK060020          
C     ------------------------------------------------------            DK060030          
C     SUBROUTINE TO COMPUTE THE REDUCED F MATRIX AND K VALUES           DK060040          
C     -------------------------------------------------------           DK060050          
      IMPLICIT REAL*8(A-H,O-Z)                                          DK060060          
      DIMENSION FAB(5000),TOT4(106),ARRAY(5000),TOT(106),TOT3(106)      DK060070          
      IF (KC.NE.0) GO TO 13                                             DK060080          
      NX=0                                                              DK060090          
      IF (NAB.EQ.5) NAB=2                                               DK060100          
      IF (NAB.EQ.6) NAB=0                                               DK060110          
      DO 11 I=1,MATX                                                    DK060120          
   11 FAB(I)=0.0                                                        DK060130          
      SOFN=0.0                                                          DK060140          
      SOF=0.0                                                           DK060150          
      IF (NLHM.EQ.0) GO TO 12                                           DK060160          
      DO 10 I=1,NLHM                                                    DK060170          
   10 TOT4(I)=0.0                                                       DK060180          
   12 IF (NAB.EQ.2) KB=1                                                DK060190          
      IF (NAB.EQ.0) KD=1                                                DK060200          
      IF (KB.EQ.1) TOT4(105)=0.0                                        DK060210          
      RETURN                                                            DK060220          
   13 IF (KC.EQ.2) GO TO 18                                             DK060230          
      SN2=NMIG                                                          DK060240          
      DM=SN2*SN2                                                        DK060250          
      SOFN=SOFN+DM                                                      DK060260          
      IF (KB.EQ.1) TOT4(105)=TOT4(105)+DM                               DK060270          
      IF (NLHM.EQ.0) GO TO 9                                            DK060280          
      DO 14 I=1,NLHM                                                    DK060290          
   14 TOT4(I)=TOT4(I)+SN2*TOT3(I)                                       DK060300          
      DO 16 I=1,NLHM                                                    DK060310          
      IF (TOT3(I).EQ.0.0) GO TO 16                                      DK060320          
      K1=NLHM*(I-1)-I*(I-3)/2-I                                         DK060330          
      DO 15 J=I,NLHM                                                    DK060340          
      K=K1+J                                                            DK060350          
   15 FAB(K)=FAB(K)+TOT3(I)*TOT3(J)                                     DK060360          
   16 CONTINUE                                                          DK060370          
    9 DO 17 I=1,NOT                                                     DK060380          
   17 TOT3(I)=0.0                                                       DK060390          
      NMIG=1                                                            DK060400          
      NX=NX+1                                                           DK060410          
      IF (KC.EQ.4) GO TO 18                                             DK060420          
      IF (KD.NE.0) NCDG=1                                               DK060430          
      RETURN                                                            DK060440          
   18 CDG=NCDG                                                          DK060450          
      DM=1./CDG                                                         DK060460          
      ROWN=DM*2.                                                        DK060470          
      SN2=SOFN*DM                                                       DK060480          
      SOF=SOF+SN2                                                       DK060490          
      DM2=SOFN/(CDG*CDG)                                                DK060500          
      IF (NLHM.EQ.0) GO TO 8                                            DK060510          
      DO 20 I=1,NLHM                                                    DK060520          
      IF (TOT(I).EQ.0.0) GO TO 20                                       DK060530          
      DO 19 J=1,NLHM                                                    DK060540          
      K1=I-J                                                            DK060550          
      IF (K1.GT.0) GO TO 37                                             DK060560          
      K=NLHM*(I-1)-I*(I-3)/2+J-I                                        DK060570          
      IF (K1.EQ.0) GO TO 38                                             DK060580          
      GO TO 39                                                          DK060590          
   37 K=NLHM*(J-1)-J*(J-3)/2+I-J                                        DK060600          
      FAB(K)=FAB(K)-TOT(I)*TOT4(J)*DM                                   DK060610          
      GO TO 19                                                          DK060620          
   38 FAB(K)=FAB(K)+TOT(I)*(TOT(J)*DM2-TOT4(J)*ROWN)                    DK060630          
      GO TO 19                                                          DK060640          
   39 FAB(K)=FAB(K)+TOT(I)*(TOT(J)*DM2-TOT4(J)*DM)                      DK060650          
   19 CONTINUE                                                          DK060660          
   20 CONTINUE                                                          DK060670          
      DO 21 I=1,NLHM                                                    DK060680          
   21 TOT4(I)=0.0                                                       DK060690          
    8 SOFN=0.0                                                          DK060700          
      RETURN                                                            DK060710          
      END                                                               DK060720          
*ENDTEXT                                                                                  
