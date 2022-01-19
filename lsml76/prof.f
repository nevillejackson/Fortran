      FUNCTION PROF(MM,E,F)                                                               
      INTEGER A,B                                                                         
C--ALGORITHM TAKEN FROM COMM. ACM, FEB., 1968, PP 116-117.              DK240050          
C--FOR MOD. SEE COMM. ACM, FEB., 1971, PP 117.                          DK240060          
C--F IS THE F-RATIO                                                     DK240070          
C--M IS THE NUMERATOR DEGREES OF FREEDOM.                               DK240080          
C--E IS THE DENOMINATOR DEGREES OF FREEDOM.                             DK240090          
C--PROF IS THE PROBABILITY OF A VALUE GREATER THAN F9                   DK240100          
      DATA PI/0.3183098862    /                                         DK240110          
      N=E                                                               DK240120          
      M=MM                                                              DK240130          
      X=F                                                               DK240140          
      ZK=0.0                                                            DK240150          
      IF (X.LT.100) GO TO 60                                            DK240160          
      IF (N.LE.3) GO TO 60                                              DK240170          
      PROF=0.00000001                                                   DK240180          
      RETURN                                                            DK240190          
   60 IF(X.GT.0.0) GO TO 65                                             DK240200          
      PROF=1.0                                                          DK240210          
      RETURN                                                            DK240220          
   65 IF(X.GT.0.01) GO TO 70                                            DK240221          
      PROF=0.999                                                        DK240222          
      RETURN                                                            DK240223          
   70 A=2*(M/2)-M+2                                                     DK240230          
      AA=FLOAT(A)                                                       DK240240          
      B=2*(N/2)-N+2                                                     DK240250          
      W=X*FLOAT(M)/FLOAT(N)                                             DK240260          
      Z=1.0/(1.0+W)                                                     DK240270          
      IF (A.NE.1) GO TO 90                                              DK240280          
      IF (B.NE.1) GO TO 80                                              DK240290          
      P=SQRT(W)                                                         DK240300          
      Y=PI                                                              DK240310          
      D=Y*Z/P                                                           DK240320          
      P=2.0*Y*ATAN(P)                                                   DK240330          
      GO TO 110                                                         DK240340          
   80 P=SQRT(W*Z)                                                       DK240350          
      D=0.5*P*Z/W                                                       DK240360          
      GO TO 110                                                         DK240370          
   90 IF (B.NE.1) GO TO 100                                             DK240380          
      P=SQRT(Z)                                                         DK240390          
      D=0.5*Z*P                                                         DK240400          
      P=1.0-P                                                           DK240410          
      GO TO 110                                                         DK240420          
  100 D=Z*Z                                                             DK240430          
      P=W*Z                                                             DK240440          
  110 Y=2.0*W/Z                                                         DK240450          
      K=B+2                                                             DK240460          
      IF (K.GT.N) GO TO 140                                             DK240470          
      IF (A.NE.1) GO TO 130                                             DK240480          
      DO 120 J=K,N,2                                                    DK240490          
      D=(1.0+AA/FLOAT(J-2))*D*Z                                         DK240500          
      IF (D.LT.0.10D-20) GO TO 160                                      DK240510          
  120 P=P+D*Y/FLOAT(J-1)                                                DK240520          
      GO TO 140                                                         DK240530          
  130 IF (Z.LT.0.5.AND.N.GT.100) GO TO 170                              DK240540          
      IF (Z.LT.0.6.AND.N.GT.200) GO TO 170                              DK240550          
      IF (Z.LT.0.7.AND.N.GT.400) GO TO 170                              DK240560          
      IF (Z.LT.0.8.AND.N.GT.800) GO TO 170                              DK240570          
      IF (Z.LT.0.9.AND.N.GT.1600) GO TO 170                             DK240580          
      IF (Z.LT.0.95.AND.N.GT.4000) GO TO 170                            DK240590          
      ZK=Z**((N-1)/2)                                                   DK240600          
      D=D*ZK*FLOAT(N)/FLOAT(B)                                                            
      P=P*ZK+W*Z*(ZK-1.)/(Z-1.0)                                        DK240620          
  140 Y=W*Z                                                             DK240630          
      Z=2.0/Z                                                           DK240640          
      B=N-2                                                             DK240650          
      K=A+2                                                             DK240660          
      IF (K.GT.M) GO TO 160                                             DK240670          
      IF(D.EQ.0.0) GO TO 160                                            DK240671          
      DO 150 I=K,M,2                                                    DK240680          
      J=I+B                                                             DK240690          
      D=Y*D*FLOAT(J)/FLOAT(I-2)                                         DK240700          
  150 P=P-Z*D/FLOAT(J)                                                  DK240710          
  160 PROF=1.0-P                                                        DK240720          
      IF (PROF.LE.0.0) PROF=0.00000001                                  DK240730          
      RETURN                                                            DK240740          
  170 PROF=0.00000001                                                                     
      RETURN                                                                              
      END                                                               DK240750          
