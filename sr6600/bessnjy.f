      SUBROUTINE BESSNJY(NORDER,X,KODE,BESS,T)                          BESSN002          
      DIMENSION T(50)                                                   BESSN003          
C KODE=0 FOR J, 1 FOR Y                                                 BESSN004          
      IF(ABS(X).LE.1.E-28) GOTO 350                                     BESSN005          
      L=2*IFIX(ABS(X))+12                                               BESSN006          
      IF(L-12-NORDER) 50,60,60                                          BESSN007          
50    L=NORDER/2*2+12                                                   BESSN008          
60    RNEUM0=0.                                                         BESSN009          
      T(L+1)=RNEUM0                                                     BESSN010          
      BESS=T(L+1)                                                       BESSN011          
      K=L                                                               BESSN012          
      MULT=-1                                                           BESSN013          
      XINV=2./X                                                         BESSN014          
      T(L)=1.E-15                                                       BESSN015          
110   IFLAG=0                                                           BESSN016          
120   L=L-1                                                             BESSN017          
      T(L)=FLOAT(L)*XINV*T(L+1)-T(L+2)                                  BESSN018          
      IF(-IFLAG) 110,150,340                                            BESSN019          
150   IFLAG=1                                                           BESSN020          
      MULT=-MULT                                                        BESSN021          
      BESS=BESS+2.*T(L+2)                                               BESSN022          
      RNEUM0=RNEUM0+4.*T(L+2)*FLOAT(MULT)/FLOAT(L+1)                    BESSN023          
      IF(L-1) 200,200,120                                               BESSN024          
200   BESS=BESS+T(1)                                                    BESSN025          
      IF(MULT) 220,340,230                                              BESSN026          
220   RNEUM0=FLOAT(MULT)*RNEUM0                                         BESSN027          
230   RNEUM0=RNEUM0/BESS                                                BESSN028          
      L=NORDER+1                                                        BESSN029          
      IF(KODE) 340,400,260                                              BESSN030          
260   T(2)=T(2)/BESS                                                    BESSN031          
      BESS=T(1)/BESS                                                    BESSN032          
      RNEUM0=.6366197724*(BESS*(ALOG(X)-.1159315157)+RNEUM0)            BESSN033          
      T(2)=(T(2)*RNEUM0-.3183098862*XINV)/BESS                          BESSN034          
      T(1)=RNEUM0                                                       BESSN035          
      IFLAG=1                                                           BESSN036          
311   IF(IFLAG.GT.L) GOTO 319                                           BESSN037          
      T(IFLAG+2)=FLOAT(IFLAG)*XINV*T(IFLAG+1)-T(IFLAG)                  BESSN038          
      IFLAG=IFLAG+1                                                     BESSN039          
      GOTO 311                                                          BESSN040          
319   CONTINUE                                                          BESSN041          
      BESS=T(L)                                                         BESSN042          
340   GOTO 410                                                          BESSN043          
350   T(1)=1                                                            BESSN044          
      T(2)=X/2E0                                                        BESSN045          
      WRITE(61,380)                                                     BESSN046          
380   FORMAT(8H ARG .LE,8H. 1.E-28,8H, VALUES,8H OF J0 A,8HND J1 ON,    BESSN047          
     .8HLY ARE R,8HETURNED ,8HIN 1ST T,8HWO ELEME,8HNTS OF A,8HRRAY.FUT BESSN048          
     .,8HHER JN"S,8H ARE UND,8HEFINED.Y,8HN"S ARE ,8HUNDEFINE,6HD     ) BESSN049          
      GOTO 410                                                          BESSN050          
400   BESS=T(L)/BESS                                                    BESSN051          
410   RETURN                                                            BESSN052          
      END                                                               BESSN053          
