*TEXT                                                                                     
      SUBROUTINE CANDSE(AC,ALS,REP,TOT2,X,ARRAY,NR,NLHM,WK,YT,I,SSS,NCT)  190001          
C     SUBROUTINE WHICH COMPUTES CONSTANT, LS MEAN AND STANDARD ERROR      190002          
C     FROM THE X, TOT2 AND ARRAY ARRAYS                                   190003          
C     -----------------------------------------------------               190004          
      DIMENSION X(1),ARRAY(1),TOT2(1),SSS(1)                              190005          
      ALS=0.                                                              190006          
      AC=0.                                                               190007          
      DO 100 L=1,NLHM                                                     190008          
      IF (X(L).EQ.0.) GO TO 100                                           190009          
      ALS=ALS+X(L)*ARRAY(NR)                                              190010          
      AC=AC+TOT2(L)*ARRAY(NR)                                             190011          
  100 NR=NR+1                                                             190012          
      ALS=ALS+YT                                                          190013          
      IF (I.GT.1) GO TO 105                                               190014          
      REP=0.                                                              190015          
      DO 101 J=1,NLHM                                                     190016          
      TEMP=0.                                                             190017          
      DO 102 K=1,NLHM                                                     190018          
      IF (X(K).EQ.0.) GO TO 102                                           190019          
      IF (J-K.LT.0) GO TO 103                                             190020          
      K1=NLHM*(K-1)-K*(K-3)/2+J-K                                         190021          
      GO TO 104                                                           190022          
  103 K1=NLHM*(J-1)-J*(J-3)/2+K-J                                         190023          
  104 TEMP=TEMP+X(K)*ARRAY(K1)                                            190024          
  102 CONTINUE                                                            190025          
      IF (X(J).EQ.0.) GO TO 101                                           190026          
      REP=REP+X(J)*TEMP                                                   190027          
  101 CONTINUE                                                            190028          
      SSS(NCT)=REP                                                        190029          
      NCT=NCT+1                                                           190030          
      REP=SQRT(REP*WK)                                                    190031          
      RETURN                                                              190032          
  105 REP=SQRT(SSS(NCT)*WK)                                               190033          
      NCT=NCT+1                                                           190034          
      RETURN                                                              190035          
      END                                                                 190036          
*ENDTEXT                                                                                  
