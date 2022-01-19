      SUBROUTINE BESSEL(NORDER,X,KODE,BESS,T)                           BESSE002          
      DIMENSION T(1)                                                    BESSE003          
      IF(ABS(X).LE.1.E-28) GOTO 260                                     BESSE004          
      L=2*IFIX(ABS(X))+12                                               BESSE005          
      IF(L-12-NORDER) 50,60,60                                          BESSE006          
50    L=NORDER/2*2+12                                                   BESSE007          
60    ETOX=EXP(X)                                                       BESSE008          
      T(L+1)=0.                                                         BESSE009          
      BESS=T(L+1)                                                       BESSE010          
      KODMULT=1-2*KODE                                                  BESSE011          
      XINV=2./X                                                         BESSE012          
      T(L)=ABS(1.+FLOAT(KODE)*ETOX)*1.E-15                              BESSE013          
110   IFLAG=0                                                           BESSE014          
120   L=L-1                                                             BESSE015          
      T(L)=FLOAT(L)*XINV*T(L+1)-FLOAT(KODMULT)*T(L+2)                   BESSE016          
      IF(KODE-IFLAG) 110,150,160                                        BESSE017          
150   IFLAG=1                                                           BESSE018          
160   BESS=BESS+2.*T(L+2)                                               BESSE019          
      IF(L-1) 180,180,120                                               BESSE020          
180   BESS=BESS+T(1)+2E0*FLOAT(KODE)*T(2)                               BESSE021          
      IF(KODE) 200,210,200                                              BESSE022          
200   BESS=BESS/ETOX                                                    BESSE023          
210   IFLAG=NORDER+1                                                    BESSE024          
      L=1                                                               BESSE025          
221   IF(L.GT.IFLAG) GOTO 229                                           BESSE026          
      T(L)=T(L)/BESS                                                    BESSE027          
      L=L+1                                                             BESSE028          
      GOTO 221                                                          BESSE029          
229   CONTINUE                                                          BESSE030          
      BESS=T(IFLAG)                                                     BESSE031          
      GOTO 300                                                          BESSE032          
260   T(1)=1                                                            BESSE033          
      T(2)=X/2E0                                                        BESSE034          
      WRITE(61,290)                                                     BESSE035          
290   FORMAT(8H ARG .LE,8H. 1.E-28,8H, VALUES,8H OF J0 A,8HND J1 ON,    BESSE036          
     .8HLY ARE R,8HETURNED ,8HIN 1ST T,8HWO ELEME,8HNTS OF A,8HRRAY. FU BESSE037          
     .,8HRTHER JN,8H"S ARE U,8HNDEFINED,8H. IN"S A,8HRE UNDEF,6HINED. ) BESSE038          
300   RETURN                                                            BESSE039          
      END                                                               BESSE040          
