      FUNCTION ERFC(X)                                                  ERFC0002          
      DIMENSION C1(8),C2(11),C3(9)                                      ERFC0003          
C     V W MASLEN   (OCTOBER,1968)                                       ERFC0004          
C     ERFC IS APPROXIMATED BY SEPARATE CHEBYSHEV SERIES FOR THE RANGES OERFC0005          
C         (0,1),(1,2),(2,INF).  ERFC(-X)=2-ERFC(X). ACCURACY 8 SIGNIF. FERFC0006          
      GOTO 330                                                          ERFC0007          
      ENTRY SETERFC                                                     ERFC0008          
      C1(1)=-3E-10 $ C1(2)=9.35E-09 $ C1(3)=-2.6645E-07                 ERFC0009          
      C1(4)=6.5789E-06 $ C1(5)=-1.3736415E-04 $ C1(6)=2.36467645E-03    ERFC0010          
      C1(7)=-3.305640575E-02 $ C1(8)=4.521731686E-01                    ERFC0011          
      C2(1)=-2E-10 $ C2(2)=9E-10 $ C2(3)=-1.07E-08 $ C2(4)=1.172E-07    ERFC0012          
      C2(5)=-1.2144E-06 $ C2(6)=1.18472E-05 $ C2(7)=-1.075830E-04       ERFC0013          
      C2(8)=8.937067E-04 $ C2(9)=-6.5941156E-03 $ C2(10)=4.06982161E-02 ERFC0014          
      C2(11)=9.517807761E-01                                            ERFC0015          
      C3(1)=1.53E-08 $ C3(2)=-8.29E-08 $ C3(3)=4.608E-07                ERFC0016          
      C3(4)=-2.8436E-06 $ C3(5)=1.98713E-05 $ C3(6)=-1.637517E-04       ERFC0017          
      C3(7)=1.7111539E-03 $ C3(8)=-2.65324343E-02 $ C3(9)=1.0715179310  ERFC0018          
      GOTO 540                                                          ERFC0019          
330   IF(.NOT.(X.EQ.0.)) GOTO 360                                       ERFC0020          
      ERFC=1.                                                           ERFC0021          
      GOTO 540                                                          ERFC0022          
360   IF(.NOT.(X+4.4.LT.0.)) GOTO 390                                   ERFC0023          
      ERFC=2.                                                           ERFC0024          
      GOTO 540                                                          ERFC0025          
390   Y=ABS(X)                                                          ERFC0026          
      IF(.NOT.(Y.LE.1.)) GOTO 440                                       ERFC0027          
      Z=2.*(Y*Y-0.5)                                                    ERFC0028          
      W=2.0*(0.5-Y*TCHEBY(C1,8,1,Z))                                    ERFC0029          
      GOTO 510                                                          ERFC0030          
440   IF(.NOT.(Y.LT.2.)) GOTO 480                                       ERFC0031          
      Z=Y+Y-3.                                                          ERFC0032          
      W=TCHEBY(C2,11,0,Z)                                               ERFC0033          
      GOTO 500                                                          ERFC0034          
480   Z=2.*(4./(Y*Y)-0.5)                                               ERFC0035          
      W=TCHEBY(C3,9,2,Z)                                                ERFC0036          
500   W=0.5*EXP(-Y*Y)/Y*W                                               ERFC0037          
510   IF(X.EQ.Y) GOTO 530                                               ERFC0038          
      W=2.-W                                                            ERFC0039          
530   ERFC=W                                                            ERFC0040          
540   RETURN                                                            ERFC0041          
      END                                                               ERFC0042          
      FUNCTION TCHEBY(C,N,M,X)                                          ERFC0043          
      DIMENSION C(11)                                                   ERFC0044          
C     THIS BELONGS TO FUNCTION TCHEBY                                   ERFC0045          
C     CHEBYSHEV SUM. SEE CLENSHAW, NUM. MATH. 4,410,1963.               ERFC0046          
      B=0.                                                              ERFC0047          
      D=C(1)                                                            ERFC0048          
      X2=X+X                                                            ERFC0049          
      I=2                                                               ERFC0050          
51    IF(I.GT.N) GOTO 59                                                ERFC0051          
      A=B                                                               ERFC0052          
      B=D                                                               ERFC0053          
      D=C(I)-A+X2*B                                                     ERFC0054          
      I=I+1                                                             ERFC0055          
      GOTO 51                                                           ERFC0056          
59    CONTINUE                                                          ERFC0057          
      IF(.NOT.(M.EQ.1)) GOTO 110                                        ERFC0058          
      A=B                                                               ERFC0059          
110   TCHEBY=D-A                                                        ERFC0060          
      RETURN                                                            ERFC0061          
      END                                                               ERFC0062          
