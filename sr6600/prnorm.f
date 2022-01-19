      FUNCTION PRNORM(X1)                                               PRNOR002          
C                                                                       PRNOR003          
C PRNORM CALCULATES LOWER TAIL AREA OF NORMAL DISTRIBUTION BY MEANS OF  PRNOR004          
C POLYNOMIAL APPROXIMATIONS WITH AT LEAST 8 DECIMAL PLACE ACCURACY.     PRNOR005          
C PRNORM HAS AT LEAST 8 DECIMAL PLACE ACCURACY.                         PRNOR006          
C REFERENCE - ALGORITHM 209, COMM. ACM 6,10(OCT,1963),616               PRNOR007          
C ADAPTED FOR USE ON 32/36/6400 BY M.LOUGHHEAD, DIVISION OF MATHEMATICALPRNOR008          
C STATISTICS, C.S.I.R.O., ADELAIDE                                      PRNOR009          
C                                                                       PRNOR010          
C IF ERROR FUNCTION IS REQUIRED USE                                     PRNOR011          
C     FUNCTION ERRFN(X) $ERRFN=2.*PRNORM(X*1.1414213562)-1. $END        PRNOR012          
C                                                                       PRNOR013          
C FOR SIGNIFICANCE LEVEL (TWO-TAILED) USE                               PRNOR014          
C     FUNCTION SIGLEVL(X) $SIGLEVL=2.*PRNORM(-ABS(X)) $END              PRNOR015          
C                                                                       PRNOR016          
      X=X1                                                              PRNOR017          
      Z=1.0                                                             PRNOR018          
      IF(X) 70,50,90                                                    PRNOR019          
50    PRNORM=0.5                                                        PRNOR020          
      GOTO 180                                                          PRNOR021          
70    X=-X                                                              PRNOR022          
      Z=-Z                                                              PRNOR023          
90    IF(X-6.) 100,100,160                                              PRNOR024          
100   IF(X-2.) 110,140,140                                              PRNOR025          
110   X=X*X*.25                                                         PRNOR026          
      Z=((((((((.00012481899*X-.00107520405)*X+.00519877502)*X-         PRNOR027          
     ..01919829200)*X+.05905403564)*X-.15196875136)*X+.31915293269)*X-  PRNOR028          
     ..5319230073)*X+.79788456059)*X1                                   PRNOR029          
      GOTO 160                                                          PRNOR030          
140   X=X*.5-2.0                                                        PRNOR031          
      Z=((((((((((((((-.00004525566*X+.00015252929)*X-.00001953813)*X-  PRNOR032          
     ..00067690499)*X+.00139060428)*X-.00079462082)*X-.00203425487)*X+  PRNOR033          
     ..00654979121)*X-.01055762501)*X+.01163044732)*X-.00927945334)*X+  PRNOR034          
     ..00535357911)*X-.00214126874)*X+.00053531085)*X+.99993665752)*Z   PRNOR035          
160   PRNORM=(Z+1.0)*.5                                                 PRNOR036          
180   RETURN                                                            PRNOR037          
      END                                                               PRNOR038          
