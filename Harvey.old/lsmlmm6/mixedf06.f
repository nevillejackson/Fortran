*TEXT                                                                                     
      SUBROUTINE MIXEDF(NAB,MATX,KA,KB,KC,NOT,TOT,NMIG,NLHM,NCPR,NRHM,TODK190010          
     1T3,ARRAY,NCDG,FAB,TOT4,SOF,NX,KD,LIOP)                            DK190020          
C     ------------------------------------------------------            DK190030          
C     SUBROUTINE TO COMPLETE COMPUTATIONS AND LISTING OF SUMS OF        DK190040          
C     SQUARES, K VALUES, ETC FOR MIXED MODELS                           DK190050          
C     ------------------------------------------------------            DK190060          
      IMPLICIT REAL*8(A-H,O-Z)                                          DK190070          
      DIMENSION FAB(5000),TOT4(106),ARRAY(5000),TOT(106),TOT3(106)      DK190080          
      SUM=0.0                                                           DK190090          
      IF (NLHM.EQ.0) GO TO 5                                            DK190100          
      DO 27 I=1,NLHM                                                    DK190110          
      K=NLHM*(I-1)-I*(I-3)/2                                            DK190120          
      DO 27 J=I,NLHM                                                    DK190130          
      IF (KD.GT.0.AND.I.EQ.1) GO TO 27                                  DK190140          
      IF (I-J.LT.0) GO TO 28                                            DK190150          
      SUM=SUM+ARRAY(K)*FAB(K)                                           DK190160          
      GO TO 27                                                          DK190170          
   28 K1=K+J-I                                                          DK190180          
      SUM=SUM+ARRAY(K1)*FAB(K1)*2.                                      DK190190          
   27 CONTINUE                                                          DK190200          
    5 AK=SUM+SOF                                                        DK190210          
      WRITE (6,1) NX,AK,SOF,SUM                                         DK190220          
    1 FORMAT (1H0,3HNS=,I4,3X,6HK--AB=,F11.6,4X,9HTR D-1 U=,F11.6,4X,27HDK190230          
     1TR C-1 (REDUCED F MATRIX) =,F11.6)                                DK190240          
      IF (KD.EQ.1) TOT4(100)=AK                                         DK190250          
      IF (KD.EQ.1) TOT4(104)=SOF                                        DK190260          
      IF (KD.EQ.2) TOT4(103)=AK                                         DK190270          
      IF (KD.EQ.2) TOT4(106)=SOF                                        DK190280          
      IF (KB.EQ.1) TOT4(101)=AK                                         DK190290          
      IF (NLHM.EQ.0.OR.LIOP.EQ.20) RETURN                               DK190300          
      WRITE (6,2)                                                       DK190310          
    2 FORMAT (1H0,20X,31HLISTING OF THE REDUCED F MATRIX)               DK190320          
      WRITE (6,3)                                                       DK190330          
    3 FORMAT (1H0)                                                      DK190340          
      WRITE (6,4) (FAB(I),I=1,MATX)                                     DK190350          
    4 FORMAT (7(1X,E15.8))                                              DK190360          
      RETURN                                                            DK190370          
      END                                                               DK190380          
*ENDTEXT                                                                                  
