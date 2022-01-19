      FUNCTION FISHER(M,N,X)                                            FISHE002          
C CALCULATION OF PROBABILITY FOR FISHERS DISTRIBUTION                   FISHE003          
C AUTHOR- J. FIELD, MARCH 1969, FROM ALGOL VERSION BY E. DORRER, FEB. 19FISHE004          
CALLED AS FORTRAN FUNCTION FISHER(M,N,X)                                FISHE005          
C WHERE M,N ARE DEGREES OF FREEDOM, X IS REAL NUMBER                    FISHE006          
      NA=2*IFIX(FLOAT(M)/2.)-M+2                                        FISHE007          
      NB=2*IFIX(FLOAT(N)/2.)-N+2                                        FISHE008          
      W=X*FLOAT(M)/FLOAT(N)                                             FISHE009          
      Z=1./(1.+W)                                                       FISHE010          
      IF(NA-1) 70,70,170                                                FISHE011          
70    IF(NB-1) 80,80,130                                                FISHE012          
80    P=SQRT(W)                                                         FISHE013          
      Y=0.318309886183791                                               FISHE014          
      D=Y*Z/P                                                           FISHE015          
      P=2.*Y*ATAN(P)                                                    FISHE016          
      GOTO 240                                                          FISHE017          
130   R=W*Z                                                             FISHE018          
      P=SQRT(R)                                                         FISHE019          
      D=0.5*P*Z/W                                                       FISHE020          
      GOTO 240                                                          FISHE021          
170   IF(NB-1) 180,180,220                                              FISHE022          
180   P=SQRT(Z)                                                         FISHE023          
      D=0.5*Z*P                                                         FISHE024          
      P=1.-P                                                            FISHE025          
      GOTO 240                                                          FISHE026          
220   D=Z*Z                                                             FISHE027          
      P=W*Z                                                             FISHE028          
240   Y=2.*W/Z                                                          FISHE029          
      L=NB+2                                                            FISHE030          
      IF(L-N) 270,270,340                                               FISHE031          
270   J=L                                                               FISHE032          
271   IF(J.GT.N) GOTO 279                                               FISHE033          
      D=(1.+FLOAT(NA)/FLOAT(J-2))*D*Z                                   FISHE034          
      IF(NA-1) 300,300,320                                              FISHE035          
300   P=P+D*Y/FLOAT(J-1)                                                FISHE036          
      GOTO 330                                                          FISHE037          
320   P=(P+W)*Z                                                         FISHE038          
330   CONTINUE                                                          FISHE039          
      J=J+2                                                             FISHE040          
      GOTO 271                                                          FISHE041          
279   CONTINUE                                                          FISHE042          
340   L=NA+2                                                            FISHE043          
      IF(L-M) 360,360,440                                               FISHE044          
360   Y=W*Z                                                             FISHE045          
      Z=2./Z                                                            FISHE046          
      NB=N-2                                                            FISHE047          
      I=L                                                               FISHE048          
391   IF(I.GT.M) GOTO 399                                               FISHE049          
      J=I+NB                                                            FISHE050          
      D=Y*D*FLOAT(J)/FLOAT(I-2)                                         FISHE051          
      P=P-Z*D/FLOAT(J)                                                  FISHE052          
      I=I+2                                                             FISHE053          
      GOTO 391                                                          FISHE054          
399   CONTINUE                                                          FISHE055          
440   IF(P) 450,500,480                                                 FISHE056          
450   P=0.0                                                             FISHE057          
      FISHER=P                                                          FISHE058          
      GOTO 510                                                          FISHE059          
480   IF(P-1.0) 500,500,490                                             FISHE060          
490   P=1.                                                              FISHE061          
500   FISHER=P                                                          FISHE062          
510   RETURN                                                            FISHE063          
      END                                                               FISHE064          
