      SUBROUTINE ROOT(X)                                                ROOT0002          
      DIMENSION X(12)                                                   ROOT0003          
      COMMON/1/L                                                        ROOT0004          
      EQUIVALENCE(A,I),(B,J),(C,N)                                      ROOT0005          
      C=X(12)                                                           ROOT0006          
      A=X(10) $ B=X(11)                                                 ROOT0007          
      IF(X(1)) 690,60,100                                               ROOT0008          
60    IF(X(3)) 690,70,490                                               ROOT0009          
70    X(1)=+1.                                                          ROOT0010          
      I=1                                                               ROOT0011          
      GOTO 690                                                          ROOT0012          
100   KQZ001=I                                                          ROOT0013          
      IF(KQZ001.LT.1) KQZ001=1                                          ROOT0014          
      IF(KQZ001.GT.9) KQZ001=9                                          ROOT0015          
      GOTO(110,110,110,390,430,260,470,530,600),KQZ001                  ROOT0016          
110   X(I+5)=X(3)                                                       ROOT0017          
      X(I+7)=X(2)                                                       ROOT0018          
      X(2)=X(4)                                                         ROOT0019          
      I=I+1                                                             ROOT0020          
      IF(.NOT.(I.EQ.3)) GOTO 690                                        ROOT0021          
      IF(X(3)*X(I+3)) 230,170,360                                       ROOT0022          
170   IF(X(3).NE.0) GOTO 200                                            ROOT0023          
180   X(1)=0                                                            ROOT0024          
      GOTO 690                                                          ROOT0025          
200   IF(X(I+3).NE.0) GOTO 70                                           ROOT0026          
      X(2)=X(8)                                                         ROOT0027          
      GOTO 180                                                          ROOT0028          
230   X(2)=X(8)-((X(8)-X(9))*X(6))/(X(6)-X(7))                          ROOT0029          
      I=6                                                               ROOT0030          
      GOTO 690                                                          ROOT0031          
260   IF(ABS(X(3))-1E-09) 180,180,270                                   ROOT0032          
270   IF(ABS(X(2)-X(8))-ABS(X(5)*X(2))) 180,180,280                     ROOT0033          
280   IF(X(3)*X(6)) 290,170,320                                         ROOT0034          
290   X(9)=X(2) $ X(7)=X(3) $ GOTO 230                                  ROOT0035          
320   X(6)=X(3) $ X(8)=X(2) $ X(7)=X(7)/2. $ GOTO 230                   ROOT0036          
360   X(2)=X(8)*1.001                                                   ROOT0037          
      I=4                                                               ROOT0038          
      GOTO 690                                                          ROOT0039          
390   SLOPE=(X(3)-X(6))/.001*X(8)                                       ROOT0040          
      X(2)=X(8)-X(6)/SLOPE                                              ROOT0041          
      I=5                                                               ROOT0042          
      GOTO 690                                                          ROOT0043          
430   IF(X(3)*X(6)) 440,180,620                                         ROOT0044          
440   X(9)=X(2) $ I=7 $ GOTO 690                                        ROOT0045          
470   X(7)=X(3) $ GOTO 230                                              ROOT0046          
490   X(1)=1. $ N=X(3) $ I=8 $ GOTO 690                                 ROOT0047          
530   X(6)=X(3) $ X(8)=X(2) $ IF(.NOT.(X(6).NE.0)) GOTO 180             ROOT0048          
      J=1                                                               ROOT0049          
561   IF(J.GT.N) GOTO 569                                               ROOT0050          
      X(2)=X(8)+FLOAT(J)*X(4) $ I=9 $ GOTO 690                          ROOT0051          
600   IF(X(6)*X(3)) 640,180,610                                         ROOT0052          
610   CONTINUE                                                          ROOT0053          
      J=J+1                                                             ROOT0054          
      GOTO 561                                                          ROOT0055          
569   CONTINUE                                                          ROOT0056          
620   X(1)=-1 $ GOTO 690                                                ROOT0057          
640   X(9)=X(2) $ X(4)=X(2) $ X(2)=X(8) $ X(7)=X(3) $ GOTO 230          ROOT0058          
690   X(12)=C $ X(10)=A $ X(11)=B                                       ROOT0059          
      RETURN                                                            ROOT0060          
      END                                                               ROOT0061          
