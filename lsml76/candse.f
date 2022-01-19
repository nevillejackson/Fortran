      SUBROUTINE CANDSE(AC,REPAC,ALS,REP,TOT2,X,ARRAY,NR,NLHM,WK,YT,I,  DK160010          
     *SSS,NCT)                                                          DK160020          
      EXTERNAL DSQRT                                                                      
C     -------------------------------------------                       DK160030          
C     SUBROUTINE WHICH COMPUTES CONSTANT, LS MEAN AND STANDARD ERRORS   DK160040          
C     FROM THE X, TOT2 AND ARRAY ARRAYS                                 DK160050          
C     -----------------------------------------------------             DK160060          
      DIMENSION X(106),ARRAY(5000),TOT2(106),SSS(630)                   DK160080          
      ALS=0.                                                            DK160090          
      AC=0.                                                             DK160100          
      DO 100 L=1,NLHM                                                   DK160110          
      IF (X(L).EQ.0.) GO TO 100                                         DK160120          
      ALS=ALS+X(L)*ARRAY(NR)                                            DK160130          
      AC=AC+TOT2(L)*ARRAY(NR)                                           DK160140          
  100 NR=NR+1                                                           DK160150          
      ALS=ALS+YT                                                        DK160160          
      IF (I.GT.1) GO TO 105                                             DK160170          
      REP=0.                                                            DK160180          
      REPAC=0.                                                          DK160190          
      DO 101 J=1,NLHM                                                   DK160200          
      TEMP=0.                                                           DK160210          
      TEMPAC=0.                                                         DK160220          
      DO 102 K=1,NLHM                                                   DK160230          
      IF (X(K).EQ.0.) GO TO 102                                         DK160240          
      IF (J-K.LT.0) GO TO 103                                           DK160250          
      K1=NLHM*(K-1)-K*(K-3)/2+J-K                                       DK160260          
      GO TO 104                                                         DK160270          
  103 K1=NLHM*(J-1)-J*(J-3)/2+K-J                                       DK160280          
  104 TEMP=TEMP+X(K)*ARRAY(K1)                                          DK160290          
      TEMPAC=TEMPAC+TOT2(K)*ARRAY(K1)                                   DK160300          
  102 CONTINUE                                                          DK160310          
      IF (X(J).EQ.0.) GO TO 101                                         DK160320          
      REP=REP+X(J)*TEMP                                                 DK160330          
      REPAC=REPAC+TOT2(J)*TEMPAC                                        DK160340          
  101 CONTINUE                                                          DK160350          
      SSS(NCT)=REP                                                      DK160360          
      SSS(NCT+300)=REPAC                                                DK160370          
      NCT=NCT+1                                                         DK160380          
      REP=REP*WK                                                        DK160390          
      REPAC=REPAC*WK                                                    DK160400          
      IF (REP.LT.0.0) REP=0.0                                           DK160410          
      IF (REPAC.LT.0.0) REPAC=0.0                                       DK160420          
      REP=DSQRT(REP)                                                    DK160430          
      REPAC=DSQRT(REPAC)                                                DK160440          
      RETURN                                                            DK160450          
  105 REP=SSS(NCT)*WK                                                   DK160460          
      REPAC=SSS(NCT+300)*WK                                             DK160470          
      IF (REP.LT.0.0) REP=0.0                                           DK160480          
      IF (REPAC.LT.0.0) REPAC=0.0                                       DK160490          
      REP=DSQRT(REP)                                                    DK160500          
      REPAC=DSQRT(REPAC)                                                DK160510          
      NCT=NCT+1                                                         DK160520          
      RETURN                                                            DK160530          
      END                                                               DK160540          
