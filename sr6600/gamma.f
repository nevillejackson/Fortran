      FUNCTION GAMMA(C)                                                 GAMMA002          
      INTEGER EXFLTF                                                    GAMMA003          
      DIMENSION B(4)                                                    GAMMA004          
      B(1)=8.333333333E-2 $ B(2)=-2.7777777777E-3                       GAMMA005          
      B(3)=7.936510794E-4 $ B(4)=-5.952380952E-4                        GAMMA006          
      IF(.NOT.(EXFLTF(60)-1.NE.0)) GOTO 70                              GAMMA007          
70    A=C                                                               GAMMA008          
      M=1                                                               GAMMA009          
      T=1.0                                                             GAMMA010          
      IF(A-.5) 110,200,200                                              GAMMA011          
110   M=2                                                               GAMMA012          
      X=A                                                               GAMMA013          
      ICOUNT=0                                                          GAMMA014          
      GOTO 170                                                          GAMMA015          
150   X=X+1E0                                                           GAMMA016          
      ICOUNT=ICOUNT+1                                                   GAMMA017          
170   IF(X) 150,180,180                                                 GAMMA018          
180   U=SIN((X*3.1415926536)*FLOAT(-1)**ICOUNT)                         GAMMA019          
      A=1.-A                                                            GAMMA020          
200   K=6-IFIX(A)                                                       GAMMA021          
      L=1                                                               GAMMA022          
211   IF(L.GT.K) GOTO 219                                               GAMMA023          
      T=T*A                                                             GAMMA024          
      A=A+1.0                                                           GAMMA025          
      L=L+1                                                             GAMMA026          
      GOTO 211                                                          GAMMA027          
219   CONTINUE                                                          GAMMA028          
      S=.9189385332-A+(A-.5)*ALOG(A)                                    GAMMA029          
      CC=A                                                              GAMMA030          
      ASQ=A*A                                                           GAMMA031          
      I=1                                                               GAMMA032          
271   IF(I.GT.4) GOTO 279                                               GAMMA033          
      S=S+B(I)/CC                                                       GAMMA034          
      CC=CC*ASQ                                                         GAMMA035          
      I=I+1                                                             GAMMA036          
      GOTO 271                                                          GAMMA037          
279   CONTINUE                                                          GAMMA038          
      IF(S-709.089) 310,350,350                                         GAMMA039          
310   S=2.7182818285**S/T                                               GAMMA040          
      KQZ001=M                                                          GAMMA041          
      IF(KQZ001.LT.1) KQZ001=1                                          GAMMA042          
      IF(KQZ001.GT.2) KQZ001=2                                          GAMMA043          
      GOTO(410,330),KQZ001                                              GAMMA044          
330   S=3.1415926536/(S*U)                                              GAMMA045          
      IF(EXFLTF(60)-1.NE.0) GOTO 410                                    GAMMA046          
      GOTO 380                                                          GAMMA047          
350   WRITE(61,360) C                                                   GAMMA048          
360   FORMAT(8H1ERROR D,8HETECTED ,8HIN GAMMA/8H ARGUMEN,2HT ,E20.12,   GAMMA049          
     .8H IS TOO ,5HLARGE)                                               GAMMA050          
      GOTO 400                                                          GAMMA051          
380   WRITE(61,390) C                                                   GAMMA052          
390   FORMAT(8H1ERROR D,8HETECTED ,8HIN GAMMA/8H ARGUMEN,2HT ,E20.12,   GAMMA053          
     .8H IS TOO ,8HNEAR ZER,8HO OR NEG,8HATIVE IN,6HTEGER )             GAMMA054          
400   S=9.999999999E306                                                 GAMMA055          
410   GAMMA=S                                                           GAMMA056          
      RETURN                                                            GAMMA057          
      END                                                               GAMMA058          
