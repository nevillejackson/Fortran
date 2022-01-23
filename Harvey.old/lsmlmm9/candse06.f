*TEXT                                                                                     
      SUBROUTINE CANDSE(AC,ALS,REP,TOT2,X,ARRAY,NR,NLHM,WK,YT,I,SSS,NCT)DK160010          
C     -------------------------------------------                       DK160020          
C     SUBROUTINE WHICH COMPUTES CONSTANT, LS MEAN AND STANDARD ERROR    DK160030          
C     FROM THE X, TOT2 AND ARRAY ARRAYS                                 DK160040          
C     -----------------------------------------------------             DK160050          
      DIMENSION X(106),ARRAY(5000),TOT2(106),SSS(630)                   DK160080          
      ALS=0.                                                                              
      AC=0.                                                             DK160090          
      DO 100 L=1,NLHM                                                   DK160100          
      IF (X(L).EQ.0.) GO TO 100                                         DK160110          
      ALS=ALS+X(L)*ARRAY(NR)                                            DK160120          
      AC=AC+TOT2(L)*ARRAY(NR)                                           DK160130          
  100 NR=NR+1                                                           DK160140          
      ALS=ALS+YT                                                        DK160150          
      IF (I.GT.1) GO TO 105                                             DK160160          
      REP=0.                                                            DK160170          
      DO 101 J=1,NLHM                                                   DK160180          
      TEMP=0.                                                           DK160190          
      DO 102 K=1,NLHM                                                   DK160200          
      IF (X(K).EQ.0.) GO TO 102                                         DK160210          
      IF (J-K.LT.0) GO TO 103                                           DK160220          
      K1=NLHM*(K-1)-K*(K-3)/2+J-K                                       DK160230          
      GO TO 104                                                         DK160240          
  103 K1=NLHM*(J-1)-J*(J-3)/2+K-J                                       DK160250          
  104 TEMP=TEMP+X(K)*ARRAY(K1)                                          DK160260          
  102 CONTINUE                                                          DK160270          
      IF (X(J).EQ.0.) GO TO 101                                         DK160280          
      REP=REP+X(J)*TEMP                                                 DK160290          
  101 CONTINUE                                                          DK160300          
      SSS(NCT)=REP                                                      DK160310          
      NCT=NCT+1                                                         DK160320          
      REP=REP*WK                                                        DK160330          
      IF (REP.LT.0.0) REP=0.0                                           DK160340          
      REP=SQTF(REP)                                                                       
      RETURN                                                            DK160360          
  105 REP=SSS(NCT)*WK                                                   DK160370          
      IF (REP.LT.0.0) REP=0.0                                           DK160380          
      REP=SQTF(REP)                                                                       
      NCT=NCT+1                                                         DK160400          
      RETURN                                                            DK160410          
      END                                                               DK160420          
*ENDTEXT                                                                                  
