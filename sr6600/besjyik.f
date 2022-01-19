      SUBROUTINE BESJYIK(NORDER,X,KODE,BESS)                            BESNJ002          
      DIMENSION T(50),A(25)                                             BESNJ003          
C KODE = 0 FOR J , 1 FOR I , 2 FOR Y , 3 FOR K                          BESNJ004          
C  MAXIMUM ORDER = 18 UNLESS DIMENSION CARD IS CHANGED                  BESNJ005          
      L=2*IFIX(ABS(X)+12E0)                                             BESNJ006          
      IF(L-12-NORDER) 40,50,50                                          BESNJ007          
40    L=NORDER/2*2+12                                                   BESNJ008          
50    ETOX=EXP(X)                                                       BESNJ009          
      K=0                                                               BESNJ010          
      RNEUM0=K                                                          BESNJ011          
      T(L+1)=RNEUM0                                                     BESNJ012          
      BESS=T(L+1)                                                       BESNJ013          
      M=1                                                               BESNJ014          
      T0=M                                                              BESNJ015          
      KODMULT=T0                                                        BESNJ016          
      IF(.NOT.(KODE.EQ.1.OR.KODE.EQ.3)) GOTO 110                        BESNJ017          
      KODMULT=-KODMULT                                                  BESNJ018          
      K=1                                                               BESNJ019          
110   MULT=-1                                                           BESNJ020          
      XINV=2./X                                                         BESNJ021          
      T(L)=ABS(1.+FLOAT(KODMULT)*ETOX)*1.E-20                           BESNJ022          
140   IFLAG=0                                                           BESNJ023          
150   L=L-1                                                             BESNJ024          
      T(L)=FLOAT(L)*XINV*T(L+1)-FLOAT(KODMULT)*T(L+2)                   BESNJ025          
      IF(K-IFLAG) 140,180,190                                           BESNJ026          
180   IFLAG=1                                                           BESNJ027          
190   BESS=BESS+2.*T(L+2)                                               BESNJ028          
      MULT=-MULT                                                        BESNJ029          
      RNEUM0=RNEUM0+4.*T(L+2)*FLOAT(MULT)/FLOAT(L+1)                    BESNJ030          
      IF(L-1) 230,230,150                                               BESNJ031          
230   BESS=BESS+2.*FLOAT(K)*T(2)+T(1)                                   BESNJ032          
      IF(MULT) 250,320,260                                              BESNJ033          
250   RNEUM0=FLOAT(MULT)*RNEUM0                                         BESNJ034          
260   RNEUM0=RNEUM0/BESS                                                BESNJ035          
      L=NORDER+1                                                        BESNJ036          
      K=KODE+1                                                          BESNJ037          
      KQZ001=K                                                          BESNJ038          
      IF(KQZ001.LT.1) KQZ001=1                                          BESNJ039          
      IF(KQZ001.GT.4) KQZ001=4                                          BESNJ040          
      GOTO(310,300,330,420),KQZ001                                      BESNJ041          
300   BESS=BESS/ETOX                                                    BESNJ042          
310   BESS=T(L)/BESS                                                    BESNJ043          
320   GOTO 920                                                          BESNJ044          
330   T(2)=T(2)/BESS                                                    BESNJ045          
      BESS=T(1)/BESS                                                    BESNJ046          
      RNEUM0=.6366197724*(BESS*(ALOG(X)-.1159315157)+RNEUM0)            BESNJ047          
      T(2)=(T(2)*RNEUM0-.3183098862*XINV)/BESS                          BESNJ048          
      T(1)=RNEUM0                                                       BESNJ049          
380   IFLAG=1                                                           BESNJ050          
381   IF(IFLAG.GT.L) GOTO 389                                           BESNJ051          
      T(IFLAG+2)=FLOAT(IFLAG)*XINV*T(IFLAG+1)-T(IFLAG)*FLOAT(KODMULT)   BESNJ052          
      IFLAG=IFLAG+1                                                     BESNJ053          
      GOTO 381                                                          BESNJ054          
389   CONTINUE                                                          BESNJ055          
      BESS=T(L)                                                         BESNJ056          
      GOTO 920                                                          BESNJ057          
420   BESS=BESS/ETOX                                                    BESNJ058          
      RNEUM0=T(2)/BESS                                                  BESNJ059          
      BESS=T(1)/BESS                                                    BESNJ060          
      M=1                                                               BESNJ061          
      T0=M                                                              BESNJ062          
      IF(ABS(X)-8.) 470,650,650                                         BESNJ063          
470   Y=X/8.                                                            BESNJ064          
      T1=Y                                                              BESNJ065          
      A(1)=-4.5634335864                                                BESNJ066          
      A(2)=8.005368869                                                  BESNJ067          
      A(3)=5.2836328668                                                 BESNJ068          
      A(4)=1.511535676                                                  BESNJ069          
      A(5)=2.590844324E-1                                               BESNJ070          
      A(6)=3.008072242E-2                                               BESNJ071          
      A(7)=2.536308188E-3                                               BESNJ072          
      A(8)=1.62708379E-4                                                BESNJ073          
      A(9)=8.21602594E-6                                                BESNJ074          
      A(10)=3.351952556E-7                                              BESNJ075          
      A(11)=1.128121139E-8                                              BESNJ076          
      A(12)=3.185879796E-10                                             BESNJ077          
      A(13)=7.65757438E-12                                              BESNJ078          
      A(14)=1.5855413E-13                                               BESNJ079          
      T(1)=-10.528830089                                                BESNJ080          
      JP=14                                                             BESNJ081          
      GOTO 760                                                          BESNJ082          
650   T1=-8./X                                                          BESNJ083          
      Y=T1                                                              BESNJ084          
      T(1)=.3991658517+Y*6.278240303E-3                                 BESNJ085          
      A(1)=2.251087357E-4                                               BESNJ086          
      A(2)=1.527877872E-5                                               BESNJ087          
      A(3)=1.57817791E-6                                                BESNJ088          
      A(4)=2.27083E-7                                                   BESNJ089          
      A(5)=4.342656E-8                                                  BESNJ090          
      A(6)=1.045317E-8                                                  BESNJ091          
      A(7)=2.87935E-9                                                   BESNJ092          
      T1=-8./X                                                          BESNJ093          
      Y=T1                                                              BESNJ094          
      JP=7                                                              BESNJ095          
760   J=1                                                               BESNJ096          
761   IF(J.GT.JP) GOTO 769                                              BESNJ097          
      K=1                                                               BESNJ098          
771   IF(K.GT.M) GOTO 779                                               BESNJ099          
      T2=T1                                                             BESNJ100          
      T2=2.*Y*T1-T0                                                     BESNJ101          
      T0=T1                                                             BESNJ102          
      T1=T2                                                             BESNJ103          
      K=K+1                                                             BESNJ104          
      GOTO 771                                                          BESNJ105          
779   CONTINUE                                                          BESNJ106          
      T(1)=T(1)+A(J)*T2                                                 BESNJ107          
      IF(ABS(X)-8.) 840,850,850                                         BESNJ108          
840   M=2                                                               BESNJ109          
850   CONTINUE                                                          BESNJ110          
      J=J+1                                                             BESNJ111          
      GOTO 761                                                          BESNJ112          
769   CONTINUE                                                          BESNJ113          
      IF(ABS(X)-8.) 870,890,890                                         BESNJ114          
870   T(1)=T(1)-ALOG(Y)*BESS                                            BESNJ115          
      GOTO 900                                                          BESNJ116          
890   T(1)=3.141592654*T(1)*EXP(-X)/SQRT(X)                             BESNJ117          
C     BESSKO=KO,BESS=IO,RNEUMO=I1,K1 TO BE CALCD. FROM WRONSKIAN        BESNJ118          
900   T(2)=(0.5*XINV-RNEUM0*T(1))/BESS                                  BESNJ119          
      GOTO 380                                                          BESNJ120          
920   RETURN                                                            BESNJ121          
      END                                                               BESNJ122          
