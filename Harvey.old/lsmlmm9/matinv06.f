*TEXT                                                                                     
      SUBROUTINE MATINV (ARRAY,NBRC,NERC,NLHM,X,D)                      DK180010          
C     -------------------------------------------                       DK180020          
C     SUBROUTINE FOR INVERSION OF SYMMETRICAL HALF-STORED MATRIX        DK180030          
C     ----------------------------------------------------              DK180040          
      DIMENSION ARRAY(5000),X(106)                                      DK180060          
      IB=1                                                              DK180070          
      IF (D.EQ.0.0) IB=0                                                DK180080          
      ID=0                                                              DK180090          
      D=1.0                                                             DK180100          
    5 DO 20 L=NBRC,NERC                                                 DK180110          
      K3=NLHM*(L-1)-L*(L-3)/2                                           DK180120          
      IF (IB.EQ.0) GO TO 4                                              DK180130          
      D=D*(ARRAY(K3)/X(L))                                              DK180140          
      IF (ID.GT.0) GO TO 4                                              DK180150          
      IF (D.GT.0.10D-20) GO TO 4                                        DK180160          
      ID=1                                                              DK180170          
      WRITE (6,1000) L                                                  DK180180          
 1000 FORMAT (41H0THE DETERMINANT WENT TO ZERO AT DIAGONAL,I3,2H .,/64H0DK180190          
     1NOTE-THE RESULTS OF THIS ANALYSIS ARE PROBABLY OF LITTLE VALUE.)  DK180200          
    4 RECIP=1./ARRAY(K3)                                                DK180210          
      ARRAY(K3)=-RECIP                                                  DK180220          
      DO 20 I=NBRC,NERC                                                 DK180230          
      K11=NLHM*(I-1)-I*(I-3)/2                                          DK180240          
      IF (I-L) 6,20,8                                                   DK180250          
    6 K1=K11+L-I                                                        DK180260          
      GO TO 10                                                          DK180270          
    8 K1=K3+I-L                                                         DK180280          
   10 R=RECIP*ARRAY(K1)                                                 DK180290          
      DO 18 J=I,NERC                                                    DK180300          
      K4=K11+J-I                                                        DK180310          
      IF (J-L) 12,18,14                                                 DK180320          
   12 K5=NLHM*(J-1)-J*(J-3)/2+L-J                                       DK180330          
      GO TO 16                                                          DK180340          
   14 K5=K3+J-L                                                         DK180350          
   16 ARRAY(K4)=ARRAY(K4)-R*ARRAY(K5)                                   DK180360          
   18 CONTINUE                                                          DK180370          
      ARRAY(K1)=R                                                       DK180380          
   20 CONTINUE                                                          DK180390          
      DO 32 I=NBRC,NERC                                                 DK180400          
      DO 32 J=I,NERC                                                    DK180410          
      K1=NLHM*(I-1)-I*(I-3)/2+J-I                                       DK180420          
      ARRAY(K1)=-ARRAY(K1)                                              DK180430          
   32 CONTINUE                                                          DK180440          
      RETURN                                                            DK180450          
      END                                                               DK180460          
*ENDTEXT                                                                                  
