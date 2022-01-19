      SUBROUTINE BESHLFJ(NU,Z,RJ)                                       BESHL002          
      REAL NU                                                           BESHL003          
      INTEGER ORDER,D,E,C                                               BESHL004          
      DIMENSION T(350)                                                  BESHL005          
      R=NU                                                              BESHL006          
      IF(NU) 70,40,120                                                  BESHL007          
40    WRITE(61,50)                                                      BESHL008          
50    FORMAT(8H NU=0 IS,8H UNDEFIN,3HED.)                               BESHL009          
      GOTO 90                                                           BESHL010          
70    IF(.NOT.(ABS(.5+R).LE.1.E-6)) GOTO 110                            BESHL011          
      RJ=SQRT(.6366197723/Z)*COS(Z)                                     BESHL012          
90    NU=R                                                              BESHL013          
      GOTO 450                                                          BESHL014          
110   NU=.5                                                             BESHL015          
120   ORDER=IABS(IFIX(NU))                                              BESHL016          
      D=2*IFIX(ABS(Z))+12                                               BESHL017          
      E=ORDER+12                                                        BESHL018          
      IF(.NOT.(D.GE.E)) GOTO 180                                        BESHL019          
      M=D                                                               BESHL020          
      GOTO 190                                                          BESHL021          
180   M=E                                                               BESHL022          
190   L=M                                                               BESHL023          
      T(M+1)=0                                                          BESHL024          
      T(M)=1.E-20                                                       BESHL025          
220   A=FLOAT(M)-.5                                                     BESHL026          
      T(M-1)=(2E0*A/Z)*T(M)-T(M+1)                                      BESHL027          
      M=M-1                                                             BESHL028          
      IF(.NOT.(M.EQ.1)) GOTO 220                                        BESHL029          
      ALPHA=SQRT(.6366197723/Z)*SIN(Z)/T(1)                             BESHL030          
      K=L+1                                                             BESHL031          
      C=1                                                               BESHL032          
291   IF(C.GT.K) GOTO 299                                               BESHL033          
      T(C)=T(C)*ALPHA                                                   BESHL034          
      C=C+1                                                             BESHL035          
      GOTO 291                                                          BESHL036          
299   CONTINUE                                                          BESHL037          
      C=NU+.5                                                           BESHL038          
      S=T(C)                                                            BESHL039          
      IF(R) 370,90,350                                                  BESHL040          
350   RJ=S                                                              BESHL041          
      GOTO 90                                                           BESHL042          
370   F=-.5                                                             BESHL043          
      V=SQRT(.6366197723/Z)*COS(Z)                                      BESHL044          
      GOTO 410                                                          BESHL045          
400   V=RJ                                                              BESHL046          
410   RJ=(2E0*F/Z)*V-S                                                  BESHL047          
      S=V                                                               BESHL048          
      F=F-1E0                                                           BESHL049          
      IF(ABS(F-R).LE.1.E-6) GOTO 90                                     BESHL050          
      GOTO 400                                                          BESHL051          
450   RETURN                                                            BESHL052          
      END                                                               BESHL053          
