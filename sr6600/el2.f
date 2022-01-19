      FUNCTION EL2(X,KC,AA,BB)                                          EL200002          
      REAL KC,KC1,M                                                     EL200003          
C         EVALUATES THE GENERAL ELLIPTIC INTEGRAL OF THE SECOND KIND.   EL200004          
C     YIELDS FOR A*B.GE.0 ABOUT 10 VALID FIGURES, WHICH ARE SIGNIFICANT EL200005          
C     IF /A/.GE./B/. THE C*Z PART OF THE VALUE OF EL2 GIVES THE VALUE OFEL200006          
C     THE JACOBIAN ZETA FN FOR Z= K*K. KC=0 IS CALCULATED AS 10**-290   EL200007          
      PI=3.1415926536 $ CA=1.0E-5 $ CB=1.0E-12                          EL200008          
      A=AA $ B=BB                                                       EL200009          
      IF(.NOT.(X.EQ.0.)) GOTO 100                                       EL200010          
      EL2=0. $ GOTO 510                                                 EL200011          
100   KC1=ABS(KC)                                                       EL200012          
      IF(.NOT.(KC.EQ.0.)) GOTO 130                                      EL200013          
      KC1=1.E-290                                                       EL200014          
130   C=X*X $ D=1E0+C $ P=SQRT((1.+KC1*KC1*C)/D)                        EL200015          
      D=X/D $ C=D/(P*2.) $ Z=A-B $ R=A $ A=(B+A)/2.                     EL200016          
      Y=ABS(1./X) $ F=0. $ L=0 $ M=1.                                   EL200017          
250   B=R*KC1+B $ E=M*KC1 $ G=E/P $ D=F*G+D $ F=C                       EL200018          
      R=A $ P=G+P $ C=(D/P+C)/2. $ G=M $ M=KC1+M                        EL200019          
      A=(B/M+A)/2. $ Y=-E/Y+Y                                           EL200020          
      IF(.NOT.(Y.EQ.0.)) GOTO 390                                       EL200021          
      Y=SQRT(E)*CB                                                      EL200022          
390   IF(.NOT.(ABS(G-KC1).GT.CA*G)) GOTO 450                            EL200023          
      KC1=SQRT(E)*2. $ L=L*2                                            EL200024          
      IF(.NOT.(Y.LT.0.)) GOTO 440                                       EL200025          
      L=L+1                                                             EL200026          
440   GOTO 250                                                          EL200027          
450   IF(.NOT.(Y.LT.0.)) GOTO 470                                       EL200028          
      L=L+1                                                             EL200029          
470   E=(ATAN(M/Y)+PI*FLOAT(L))*A/M                                     EL200030          
      IF(.NOT.(X.LT.0.)) GOTO 500                                       EL200031          
      E=-E                                                              EL200032          
500   EL2=E+C*Z                                                         EL200033          
510   RETURN                                                            EL200034          
      END                                                               EL200035          
