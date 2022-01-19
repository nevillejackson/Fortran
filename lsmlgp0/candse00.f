*TEXT                                                                                     
      SUBROUTINE CANDSE(AC,ALS,REP,TOT2,X,ARRAY,NR,NLHM,WK,YT,I,SSS,NCT)                  
C     SUBROUTINE WHICH COMPUTES CONSTANT, LS MEAN AND STANDARD ERROR    DEC16000          
C     FROM THE X, TOT2 AND ARRAY ARRAYS                                 DEC16001          
C     -----------------------------------------------------             DEC16002          
      DIMENSION X(106),ARRAY(2000),TOT2(106),SSS(630)                   M0116004          
      ALS=0.                                                            DEC16006          
      AC=0.                                                             DEC16007          
      DO 100 L=1,NLHM                                                   DEC16008          
      IF (X(L).EQ.0.) GO TO 100                                         DEC16009          
      ALS=ALS+X(L)*ARRAY(NR)                                            DEC16010          
      AC=AC+TOT2(L)*ARRAY(NR)                                           DEC16011          
  100 NR=NR+1                                                           DEC16012          
      ALS=ALS+YT                                                        DEC16013          
      IF (I.GT.1) GO TO 105                                             DEC16014          
      REP=0.                                                            DEC16015          
      DO 101 J=1,NLHM                                                   DEC16016          
      TEMP=0.                                                           DEC16017          
      DO 102 K=1,NLHM                                                   DEC16018          
      IF (X(K).EQ.0.) GO TO 102                                         DEC16019          
      IF (J-K.LT.0) GO TO 103                                           DEC16020          
      K1=NLHM*(K-1)-K*(K-3)/2+J-K                                       DEC16021          
      GO TO 104                                                         DEC16022          
  103 K1=NLHM*(J-1)-J*(J-3)/2+K-J                                       DEC16023          
  104 TEMP=TEMP+X(K)*ARRAY(K1)                                          DEC16024          
  102 CONTINUE                                                          DEC16025          
      IF (X(J).EQ.0.) GO TO 101                                         DEC16026          
      REP=REP+X(J)*TEMP                                                 DEC16027          
  101 CONTINUE                                                          DEC16028          
      SSS(NCT)=REP                                                      DEC16029          
      NCT=NCT+1                                                         DEC16030          
      REP=SQRT(REP*WK)                                                  DEC16031          
      RETURN                                                            DEC16032          
  105 REP=SQRT(SSS(NCT)*WK)                                             DEC16033          
      NCT=NCT+1                                                         DEC16034          
      RETURN                                                            DEC16035          
      END                                                               DEC16036          
*ENDTEXT                                                                                  
