      SUBROUTINE LINFUN(NLC,NLHM,X,JBEG,TOT2,NRHM,MATX,NCPR,SSCPR,TRED,EDK250010          
     1DF,XP,YP,AC,ALS,REP,ARRAY,SSS,NAB,LITY,MN2,YM,MTY)                DK250020          
C     ------------------------------------------------------            DK250030          
C     SUBROUTINE TO COMPUTE LINEAR FUNCTIONS                            DK250040          
C     ------------------------------------------------------            DK250050          
      DIMENSION X(106),JBEG(90),TOT2(106),SSCPR(630),XP(81),YP(41),ARRAYDK250080          
     1(5000),SSS(630),LITY(35),TRED(35),YM(35)                          DK250090          
      WRITE (6,1051)                                                    DK250100          
 1051 FORMAT (1H0,23X,66HLISTING OF SELECTED LINEAR FUNCTIONS, STANDARD DK250110          
     1ERRORS AND T VALUES)                                              DK250120          
      WRITE (6,1052)                                                    DK250130          
 1052 FORMAT (1H0,2X,11HDESCRIPTION,2X,12HERR T  NO.  ,84HR(1) C(1) R(2)DK250140          
     1 C(2) R(3) C(3) R(4) C(4) R(5) C(5) R(6) C(6) R(7) C(7) R(8) C(8) DK250150          
     2ETC.)                                                             DK250160          
      DO 301 K1=1,NLC                                                   DK250170          
      READ (5,1050) A1,A2,A3,KB,NERC,(JBEG(J),TOT2(J),J=1,NERC)         DK250180          
 1050 FORMAT (2A6,A1,I1,I2,12(I2,F3.0),4X/(16(I2,F3.0)))                DK250190          
      IF (MTY.EQ.1) KB=1                                                DK250200          
      IF (KB.EQ.0.OR.KB.GT.3) KB=1                                      DK250210          
      WRITE (6,1053) A1,A2,A3,KB,NERC,(JBEG(J),TOT2(J),J=1,NERC)        DK250220          
 1053 FORMAT (1H0,2A6,A1,2X,I2,5X,I2,5X,9(I2,2X,F4.0,2X)/(17X,9(I2,2X,F4DK250230          
     1.0,2X)))                                                          DK250240          
      WRITE (6,1004)                                                    DK250250          
 1004 FORMAT (1H )                                                      DK250260          
      DO 302 J=1,NLHM                                                   DK250270          
  302 X(J)=0.                                                           DK250280          
      DO 303 J=1,NERC                                                   DK250290          
      K=JBEG(J)                                                         DK250300          
  303 X(K)=TOT2(J)                                                      DK250310          
      DO 309 J=1,NLHM                                                   DK250320          
  309 TOT2(J)=0.                                                        DK250330          
      YT=0.0                                                            DK250340          
      DO 304 I=1,NRHM                                                   DK250350          
      NCT=1                                                             DK250360          
      NR=MATX+(I-1)*NLHM+1                                              DK250370          
      IF (NCPR.EQ.1) GO TO 305                                          DK250380          
      K4=I                                                              DK250390          
      GO TO 306                                                         DK250400          
  305 K4=NRHM*(I-1)-I*(I-3)/2                                           DK250410          
  306 IF (KB.EQ.1) WK=(SSCPR(K4)-TRED(I))/EDF                           DK250420          
      IF (KB.EQ.1.AND.MN2.EQ.1) WK=SSCPR(K4)                            DK250430          
      IF (KB.EQ.2) WK=XP(I)                                             DK250440          
      IF (KB.EQ.3) WK=YP(I)                                             DK250450          
      IF (WK.LT.0.) GO TO 300                                           DK250460          
      CALL CANDSE (AC,REPAC,ALS,REP,TOT2,X,ARRAY,NR,NLHM,WK,YT,I,SSS,   DK250470          
     *NCT)                                                              DK250480          
      IF (NAB.EQ.0.OR.NAB.EQ.3) ALS=ALS+X(1)*YM(I)                      DK250490          
      TEMP=ALS/REP                                                      DK250500          
  304 WRITE (6,1054) LITY(I),ALS,REP,TEMP                               DK250510          
  301 CONTINUE                                                          DK250520          
 1054 FORMAT (1H ,A6,5X,17HLINEAR FUNCTION =,F17.8,4X,16HSTANDARD ERROR DK250530          
     1=,F17.8,2X,2HT=,F12.3)                                            DK250540          
      RETURN                                                            DK250550          
  300 WRITE (6,1021)                                                    DK250560          
 1021 FORMAT (1H0,10X,29HNEGATIVE ERROR SUM OF SQUARES)                 DK250570          
      RETURN                                                            DK250580          
      END                                                               DK250590          
