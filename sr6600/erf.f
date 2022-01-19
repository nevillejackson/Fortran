      FUNCTION ERF(X1)                                                  ERF00002          
C                                                                       ERF00003          
C *ERF* EVALUATES THE ERROR FUNCTION, THE NORMAL DISTRIBUTION           ERF00004          
C  FUNCTION, OR THE SIGNIFICANCE LEVEL OF THE NORMAL DISTRIBUTION       ERF00005          
C  BY THE FUNCTION CALLS *ERF(X)* FOR THE ERROR FUNCTION, *PROBNORM(X)* ERF00006          
C  FOR THE NORMAL DISTRIBUTION AND *SIGNIF(X)* FOR THE SIGNIFICANCE     ERF00007          
C  LEVEL.                                                               ERF00008          
C THE ROUTINE IS THE SAME AS *C3 CSIR PRNORM* WITH EXTRA ENTRY          ERF00009          
C  POINTS TO MAKE IT MORE VERSATILE WITH ONLY A SLIGHT LOSS OF SPEED.   ERF00010          
C                                                                       ERF00011          
C MODIFIED BY N.R.PUMMEROY, CSIRO DIV. OF COMPUTING RESEARCH, 17/2/71.  ERF00012          
C                                                                       ERF00013          
      NTRY=-1 $ XT=X1 $ X1=X1*1.414213562 $ GOTO 130                    ERF00014          
      ENTRY SIGNIF                                                      ERF00015          
      NTRY=1 $ XT=X1 $ X1=-ABS(X1) $ GOTO 130                           ERF00016          
      ENTRY PRONORM                                                     ERF00017          
      NTRY=0                                                            ERF00018          
C                                                                       ERF00019          
130   X=X1                                                              ERF00020          
      Z=1.0                                                             ERF00021          
      IF(X) 180,160,200                                                 ERF00022          
160   ERF=0.5                                                           ERF00023          
      GOTO 300                                                          ERF00024          
180   X=-X                                                              ERF00025          
      Z=-Z                                                              ERF00026          
200   IF(X.GT.6.) GOTO 290                                              ERF00027          
      IF(X.GE.2.) GOTO 270                                              ERF00028          
      X=X*X*.25                                                         ERF00029          
      Z=((((((((.00012481899*X-.00107520405)*X+.00519877502)*X-         ERF00030          
     ..01919829200)*X+.05905403564)*X-.15196875136)*X+.31915293269)*X-  ERF00031          
     ..5319230073)*X+.79788456059)*X1                                   ERF00032          
      GOTO 290                                                          ERF00033          
270   X=X*0.5-2.0                                                       ERF00034          
      Z=((((((((((((((-.00004525566*X+.00015252929)*X-.00001953813)*X-  ERF00035          
     ..00067690499)*X+.00139060428)*X-.00079462082)*X-.00203425487)*X+  ERF00036          
     ..00654979121)*X-.01055762501)*X+.01163044732)*X-.00927945334)*X+  ERF00037          
     ..00535357911)*X-.00214126874)*X+.00053531085)*X+.99993665752)*Z   ERF00038          
290   ERF=(Z+1.0)*.5                                                    ERF00039          
300   IF(NTRY) 310,360,340                                              ERF00040          
310   X1=XT $ ERF=ERF+ERF-1.0 $ GOTO 370                                ERF00041          
340   X1=XT $ ERF=ERF+ERF                                               ERF00042          
360   CONTINUE                                                          ERF00043          
370   RETURN                                                            ERF00044          
      END                                                               ERF00045          
