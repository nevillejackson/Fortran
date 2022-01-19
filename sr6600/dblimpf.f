      FUNCTION DBLIMPF(A,B,F1,F2,F,DELTA,ERRTYPE)                       DBLSI002          
      REAL KX,KY                                                        DBLSI003          
      INTEGER ERRTYPE                                                   DBLSI004          
C                                                                       DBLSI005          
C     COMPUTATION OF THE REPEATED INTEGRAL OF F(X,Y) TAKEN OVER         DBLSI006          
C     THE REGION (F1(X).LE.Y.LE.F2(X),A.LE.X.LE.B) USING ADAPTIVE       DBLSI007          
C     SIMPSON"S INTEGRATION.  IF THE PARAMETER ERRTYPE IS ZERO THE      DBLSI008          
C     RELATIVE ERROR IS USED ELSE THE ABSOLUTE ERROR IS USED.           DBLSI009          
C                                                                       DBLSI010          
      X1=A                                                              DBLSI011          
      X=X1 $ NI=1 $ GOTO 300                                            DBLSI012          
50    S1=G                                                              DBLSI013          
      X2=B                                                              DBLSI014          
      X=X2 $ NI=2 $ GOTO 300                                            DBLSI015          
90    S1=S1+G                                                           DBLSI016          
      S0=S1                                                             DBLSI017          
      KX=X2-X1                                                          DBLSI018          
110   S2=0. $ HX=0.5*KX                                                 DBLSI019          
      X=X1+HX                                                           DBLSI020          
140   NI=3 $ GOTO 300                                                   DBLSI021          
160   S2=S2+G                                                           DBLSI022          
      X=X+KX $ IF(.NOT.(X.GT.X2)) GOTO 140                              DBLSI023          
      S1=S1+4.*S2                                                       DBLSI024          
      IF(ERRTYPE.GT.0) GOTO 230                                         DBLSI025          
C     RELATIVE ERROR.                                                   DBLSI026          
      IF(HX*ABS((S1-S0-S0)/S1).LT.DELTA) GOTO 280                       DBLSI027          
      GOTO 240                                                          DBLSI028          
C     ABSOLUTE ERROR.                                                   DBLSI029          
230   IF(HX*ABS(S1-S0-S0).LT.DELTA) GOTO 280                            DBLSI030          
240   S0=S1 $ S1=S1-S2-S2 $ KX=HX $ GOTO 110                            DBLSI031          
280   DBLIMPF=0.1111111111*HX*S1 $ GOTO 560                             DBLSI032          
C                                                                       DBLSI033          
C     COMPUTE INNER INTEGRAL.                                           DBLSI034          
300   Y1=F1(X) $ Y2=F2(X) $ KY=Y2-Y1                                    DBLSI035          
C     CHECK IF INNER INTEGRAL IS ZERO OR NOT.                           DBLSI036          
      IF(ABS(KY).GE.DELTA) GOTO 370                                     DBLSI037          
      G=0. $ GOTO 550                                                   DBLSI038          
370   T1=F(X,Y1) $ T1=T1+F(X,Y2)                                        DBLSI039          
      T0=T1                                                             DBLSI040          
390   T2=0. $ HY=0.5*KY                                                 DBLSI041          
      Y=Y1+HY                                                           DBLSI042          
420   T2=T2+F(X,Y)                                                      DBLSI043          
      Y=Y+KY $ IF(.NOT.(Y.GT.Y2)) GOTO 420                              DBLSI044          
      T1=T1+4.*T2                                                       DBLSI045          
      IF(ERRTYPE.GT.0) GOTO 490                                         DBLSI046          
C     RELATIVE ERROR.                                                   DBLSI047          
      IF(HY*ABS((T1-T0-T0)/T1).LT.DELTA) GOTO 540                       DBLSI048          
      GOTO 500                                                          DBLSI049          
C     ABSOLUTE ERROR.                                                   DBLSI050          
490   IF(HY*ABS(T1-T0-T0).LT.DELTA) GOTO 540                            DBLSI051          
500   T0=T1 $ T1=T1-T2-T2 $ KY=HY $ GOTO 390                            DBLSI052          
540   G=HY*T1                                                           DBLSI053          
550   KQZ001=NI                                                         DBLSI054          
      IF(KQZ001.LT.1) KQZ001=1                                          DBLSI055          
      IF(KQZ001.GT.3) KQZ001=3                                          DBLSI056          
      GOTO(50,90,160),KQZ001                                            DBLSI057          
560   RETURN                                                            DBLSI058          
      END                                                               DBLSI059          
