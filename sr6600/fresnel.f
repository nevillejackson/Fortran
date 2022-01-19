      SUBROUTINE FRESNEL(U,CB,SB)                                       FRESN002          
      DIMENSION A(12),B(12),C(12),D(12)                                 FRESN003          
      EQUIVALENCE(SUMA,SUMC),(SUMB,SUMD)                                FRESN004          
      A(1)=1.59576914 $ A(2)=-1.702E-6 $ A(3)=-6.808568854              FRESN005          
      A(4)=-5.76361E-4 $ A(5)=6.920691902 $ A(6)=-1.6898657E-2          FRESN006          
      A(7)=-3.05048566 $ A(8)=-7.5752419E-2 $ A(9)=0.850663781          FRESN007          
      A(10)=-2.5639041E-2 $ A(11)=-0.15023096 $ A(12)=3.4404779E-2      FRESN008          
      B(1)=33E-9 $ B(2)=4.255387524 $ B(3)=-9.281E-5                    FRESN009          
      B(4)=-7.7800204 $ B(5)=-9.520895E-3 $ B(6)=5.075161298            FRESN010          
      B(7)=-0.138341947 $ B(8)=-1.363729124 $ B(9)=-0.403349276         FRESN011          
      B(10)=0.702222016 $ B(11)=-0.216195929 $ B(12)=1.9547031E-2       FRESN012          
      C(1)=0. $ C(2)=-2.4933975E-2 $ C(3)=3.936E-6 $ C(4)=5.770956E-3   FRESN013          
      C(5)=6.89892E-4 $ C(6)=-9.497136E-3 $ C(7)=1.1948809E-2           FRESN014          
      C(8)=-6.748873E-3 $ C(9)=2.4642E-4 $ C(10)=2.102967E-3            FRESN015          
      C(11)=-1.21793E-3 $ C(12)=2.33939E-4                              FRESN016          
      D(1)=0.19947114 $ D(2)=23E-9 $ D(3)=-9.351341E-3 $ D(4)=2.3006E-5 FRESN017          
      D(5)=4.851466E-3 $ D(6)=1.903218E-3 $ D(7)=-1.7122914E-2          FRESN018          
      D(8)=2.9064067E-2 $ D(9)=-2.7928955E-2 $ D(10)=1.6497308E-2       FRESN019          
      D(11)=-5.598515E-3 $ D(12)=8.38386E-4                             FRESN020          
      GOTO 910                                                          FRESN021          
      ENTRY FRETRIG                                                     FRESN022          
      X=1.5707963268*U*U $ GOTO 560                                     FRESN023          
      ENTRY FRENEXP                                                     FRESN024          
      X=U                                                               FRESN025          
560   IF(.NOT.(ABS(X).LT.1E-4)) GOTO 600                                FRESN026          
      SB=0. $ CB=SQRT(X/1.5707963268) $ GOTO 910                        FRESN027          
600   IF(.NOT.(U.LT.0.)) GOTO 660                                       FRESN028          
      WRITE(61,620) U                                                   FRESN029          
620   FORMAT(8H  **NEG.,8HARGUMENT,8H IN FRES,8HNEL**  U,2H= ,E10.2)    FRESN030          
      CB=0. $ SB=0.                                                     FRESN031          
      GOTO 910                                                          FRESN032          
660   CF=COS(X) $ SF=SIN(X)                                             FRESN033          
      IF(.NOT.(X.LT.4.0)) GOTO 800                                      FRESN034          
      SUMA=0. $ SUMB=0.                                                 FRESN035          
      X4=X/4.0 $ XFAC=1./X4                                             FRESN036          
      N=1                                                               FRESN037          
731   IF(N.GT.12) GOTO 739                                              FRESN038          
      XFAC=XFAC*X4                                                      FRESN039          
      SUMA=SUMA+A(N)*XFAC                                               FRESN040          
      SUMB=SUMB+B(N)*XFAC                                               FRESN041          
      N=N+1                                                             FRESN042          
      GOTO 731                                                          FRESN043          
739   CONTINUE                                                          FRESN044          
      CB=SQRT(X4)*(SUMA*CF+SUMB*SF)                                     FRESN045          
      SB=SQRT(X4)*(SUMA*SF-SUMB*CF)                                     FRESN046          
      GOTO 910                                                          FRESN047          
800   SUMC=0. $ SUMD=0.                                                 FRESN048          
      X4=4./X $ XFAC=1./X4                                              FRESN049          
      N=1                                                               FRESN050          
841   IF(N.GT.12) GOTO 849                                              FRESN051          
      XFAC=XFAC*X4                                                      FRESN052          
      SUMC=SUMC+C(N)*XFAC                                               FRESN053          
      SUMD=SUMD+D(N)*XFAC                                               FRESN054          
      N=N+1                                                             FRESN055          
      GOTO 841                                                          FRESN056          
849   CONTINUE                                                          FRESN057          
      CB=0.5+SQRT(X4)*(SUMC*CF+SUMD*SF)                                 FRESN058          
      SB=0.5+SQRT(X4)*(SUMC*SF-SUMD*CF)                                 FRESN059          
910   RETURN                                                            FRESN060          
      END                                                               FRESN061          
