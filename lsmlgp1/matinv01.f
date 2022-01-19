*TEXT                                                                                     
      SUBROUTINE MATINV(ARRAY,NBRC,NERC,NLHM)                           DEC12000          
C     SUBROUTINE FOR INVERSION OF SYMMETRICAL HALF-STORED MATRIX        DEC12000          
C     ----------------------------------------------------              DEC12001          
      DIMENSION ARRAY(8550)                                             M0112003          
    5 DO 20 L=NBRC,NERC                                                 DEC12004          
      K3=NLHM*(L-1)-L*(L-3)/2                                           DEC12005          
      RECIP=1./ARRAY(K3)                                                DEC12006          
      ARRAY(K3)=-RECIP                                                  DEC12007          
      DO 20 I=NBRC,NERC                                                 DEC12008          
      K11=NLHM*(I-1)-I*(I-3)/2                                          DEC12009          
      IF (I-L) 6,20,8                                                   DEC12010          
    6 K1=K11+L-I                                                        DEC12011          
      GO TO 10                                                          DEC12012          
    8 K1=K3+I-L                                                         DEC12013          
   10 R=RECIP*ARRAY(K1)                                                 DEC12014          
      DO 18 J=I,NERC                                                    DEC12015          
      K4=K11+J-I                                                        DEC12016          
      IF (J-L) 12,18,14                                                 DEC12017          
   12 K5=NLHM*(J-1)-J*(J-3)/2+L-J                                       DEC12018          
      GO TO 16                                                          DEC12019          
   14 K5=K3+J-L                                                         DEC12020          
   16 ARRAY(K4)=ARRAY(K4)-R*ARRAY(K5)                                   DEC12021          
   18 CONTINUE                                                          DEC12022          
      ARRAY(K1)=R                                                       DEC12023          
   20 CONTINUE                                                          DEC12024          
      DO 32 I=NBRC,NERC                                                 DEC12025          
      DO 32 J=I,NERC                                                    DEC12026          
      K1=NLHM*(I-1)-I*(I-3)/2+J-I                                       DEC12027          
      ARRAY(K1)=-ARRAY(K1)                                              DEC12028          
   32 CONTINUE                                                          DEC12029          
      RETURN                                                            DEC12030          
      END                                                               DEC12031          
*ENDTEXT                                                                                  
