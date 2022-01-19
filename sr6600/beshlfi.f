      SUBROUTINE BESHLFI(NU,Z,RI)                                       BESHL002          
      REAL NU                                                           BESHL003          
      INTEGER ORDER,D,E,C                                               BESHL004          
      DIMENSION T(350)                                                  BESHL005          
C     USE WITH SINH AND COSH                                            BESHL006          
      R=NU                                                              BESHL007          
      IF(NU) 80,40,120                                                  BESHL008          
40    WRITE(61,50)                                                      BESHL009          
50    FORMAT(8H NU=0 IS,8H UNDEFIN,3HED.)                               BESHL010          
60    NU=R                                                              BESHL011          
      GOTO 450                                                          BESHL012          
80    IF(.NOT.(ABS(.5+R).LE.1.E-6)) GOTO 110                            BESHL013          
      RI=SQRT(.6366197723/Z)*COSH(Z)                                    BESHL014          
      GOTO 60                                                           BESHL015          
110   NU=.5                                                             BESHL016          
120   ORDER=ABS(IFIX(NU))                                               BESHL017          
      D=2*IFIX(ABS(Z))+12                                               BESHL018          
      E=ORDER+12                                                        BESHL019          
      IF(.NOT.(D.GE.E)) GOTO 180                                        BESHL020          
      M=D                                                               BESHL021          
      GOTO 190                                                          BESHL022          
180   M=E                                                               BESHL023          
190   L=M                                                               BESHL024          
      T(M+1)=0                                                          BESHL025          
      T(M)=1.E-20                                                       BESHL026          
220   A=FLOAT(M)-.5                                                     BESHL027          
      T(M-1)=(2E0*A/Z)*T(M)+T(M+1)                                      BESHL028          
      M=M-1                                                             BESHL029          
      IF(.NOT.(M.EQ.1)) GOTO 220                                        BESHL030          
      ALPHA=SQRT(.6366197723/Z)*SINH(Z)/T(1)                            BESHL031          
      K=L+1                                                             BESHL032          
      C=1                                                               BESHL033          
291   IF(C.GT.K) GOTO 299                                               BESHL034          
      T(C)=T(C)*ALPHA                                                   BESHL035          
      C=C+1                                                             BESHL036          
      GOTO 291                                                          BESHL037          
299   CONTINUE                                                          BESHL038          
      C=NU+.5                                                           BESHL039          
      S=T(C)                                                            BESHL040          
      IF(R) 370,60,350                                                  BESHL041          
350   RI=S                                                              BESHL042          
      GOTO 60                                                           BESHL043          
370   F=-.5                                                             BESHL044          
      V=SQRT(.6366197723/Z)*COSH(Z)                                     BESHL045          
      GOTO 410                                                          BESHL046          
400   V=RI                                                              BESHL047          
410   RI=S+(2E0*F/Z)*V                                                  BESHL048          
      S=V                                                               BESHL049          
      F=F-1E0                                                           BESHL050          
      IF(ABS(F-R).LE.1.E-6) GOTO 60                                     BESHL051          
      GOTO 400                                                          BESHL052          
450   RETURN                                                            BESHL053          
      END                                                               BESHL054          
