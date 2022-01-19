      SUBROUTINE MIXEDF(MATX,KB,NLHM,ARRAY,FAB,TOT4,SOF,NX,KD,LIOP)     DK190010          
C     ------------------------------------------------------            DK190020          
C     SUBROUTINE TO COMPLETE COMPUTATIONS AND LISTING OF SUMS OF        DK190030          
C     SQUARES, K VALUES, ETC FOR MIXED MODELS                           DK190040          
C     ------------------------------------------------------            DK190050          
      DIMENSION FAB(5000),TOT4(106),ARRAY(5000)                         DK190070          
      SUM=0.0                                                           DK190080          
      IF (NLHM.EQ.0) GO TO 5                                            DK190090          
      DO 27 I=1,NLHM                                                    DK190100          
      K=NLHM*(I-1)-I*(I-3)/2                                            DK190110          
      DO 27 J=I,NLHM                                                    DK190120          
      IF (KD.GT.0.AND.I.EQ.1) GO TO 27                                  DK190130          
      IF (I-J.LT.0) GO TO 28                                            DK190140          
      SUM=SUM+ARRAY(K)*FAB(K)                                           DK190150          
      GO TO 27                                                          DK190160          
   28 K1=K+J-I                                                          DK190170          
      SUM=SUM+ARRAY(K1)*FAB(K1)*2.                                      DK190180          
   27 CONTINUE                                                          DK190190          
    5 AK=SUM+SOF                                                        DK190200          
      WRITE (6,1) NX,AK,SOF,SUM                                         DK190210          
    1 FORMAT (1H0,3HNS=,I4,3X,6HK--AB=,F14.6,4X,9HTR D-1 U=,F14.6,4X,27HDK190220          
     1TR C-1 (REDUCED F MATRIX) =,F14.6)                                DK190230          
      IF (KD.EQ.1) TOT4(100)=AK                                         DK190240          
      IF (KD.EQ.1) TOT4(104)=SOF                                        DK190250          
      IF (KD.EQ.2) TOT4(103)=AK                                         DK190260          
      IF (KD.EQ.2) TOT4(106)=SOF                                        DK190270          
      IF (KB.EQ.1) TOT4(101)=AK                                         DK190280          
      IF (NLHM.EQ.0.OR.LIOP.EQ.20) RETURN                               DK190290          
      WRITE (6,2)                                                       DK190300          
    2 FORMAT (1H0,20X,31HLISTING OF THE REDUCED F MATRIX)               DK190310          
      WRITE (6,3)                                                       DK190320          
    3 FORMAT (1H0)                                                      DK190330          
      WRITE (6,4) (FAB(I),I=1,MATX)                                     DK190340          
    4 FORMAT (7(1X,E15.8))                                              DK190350          
      RETURN                                                            DK190360          
      END                                                               DK190370          
