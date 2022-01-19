      FUNCTION THETA(K,Z,Q)                                             THETA002          
      EP=1.E-9                                                          THETA003          
      IF(.NOT.(Q.LT.0.)) GOTO 80                                        THETA004          
40    WRITE(61,50) K,Q                                                  THETA005          
50    FORMAT(8H ERROR I,7HN THETA,I1,7H ,NOME=,E19.12)                  THETA006          
      THETA=1.E307                                                      THETA007          
      GOTO 1200                                                         THETA008          
80    IF(.NOT.(Q.LT.1.)) GOTO 40                                        THETA009          
      PI=3.1415926536                                                   THETA010          
      PK=1.5707963268                                                   THETA011          
      KQZ001=K                                                          THETA012          
      IF(KQZ001.LT.1) KQZ001=1                                          THETA013          
      IF(KQZ001.GT.4) KQZ001=4                                          THETA014          
      GOTO(160,120,750,140),KQZ001                                      THETA015          
120   Z=Z+PK                                                            THETA016          
      GOTO 160                                                          THETA017          
140   Z=Z+PK                                                            THETA018          
      GOTO 750                                                          THETA019          
160   IF(.NOT.(Q.EQ.0.)) GOTO 190                                       THETA020          
      THETA=0.                                                          THETA021          
      GOTO 1200                                                         THETA022          
190   IF(.NOT.(Q.LT..1)) GOTO 400                                       THETA023          
      THETA=0.                                                          THETA024          
      C=COS(Z)                                                          THETA025          
      S=SIN(Z)                                                          THETA026          
      R=2.*C                                                            THETA027          
      D=R*C-1.                                                          THETA028          
      T=R*S                                                             THETA029          
      B=-SQRT(SQRT(Q))                                                  THETA030          
      P=Q*Q                                                             THETA031          
      A=-1./Q                                                           THETA032          
290   B=B*A*Q                                                           THETA033          
      TERM=B*S                                                          THETA034          
      THETA=THETA+TERM                                                  THETA035          
      IF(ABS(B).LT.EP) GOTO 380                                         THETA036          
      A=A*P                                                             THETA037          
      S1=S*D+C*T                                                        THETA038          
      C=C*D-S*T                                                         THETA039          
      S=S1                                                              THETA040          
      GOTO 290                                                          THETA041          
380   THETA=2.*THETA                                                    THETA042          
      GOTO 1200                                                         THETA043          
400   N=Z/PI                                                            THETA044          
      I=N-(N/2)*2                                                       THETA045          
      Z=Z-FLOAT(N)*PI                                                   THETA046          
      G=1./ALOG(Q)                                                      THETA047          
      D=2.*PI                                                           THETA048          
      H=Z*D                                                             THETA049          
      F=PI*D                                                            THETA050          
      E=Z+PK                                                            THETA051          
      A=E*E                                                             THETA052          
      E=Z-PK                                                            THETA053          
      B=E*E                                                             THETA054          
      C=F                                                               THETA055          
      M=1                                                               THETA056          
      L=M                                                               THETA057          
      SUM4=0.                                                           THETA058          
      SUM3=SUM4                                                         THETA059          
      SUM2=SUM3                                                         THETA060          
      SUM1=SUM2                                                         THETA061          
540   TERM1=EXP(A*G)                                                    THETA062          
      TERM2=EXP(B*G)                                                    THETA063          
      IF(.NOT.(L.GT.0)) GOTO 590                                        THETA064          
      SUM1=SUM1+TERM1                                                   THETA065          
      GOTO 600                                                          THETA066          
590   SUM3=SUM3+TERM1                                                   THETA067          
600   L=-L                                                              THETA068          
      IF(.NOT.(M.GT.0)) GOTO 640                                        THETA069          
      SUM2=SUM2+TERM2                                                   THETA070          
      GOTO 650                                                          THETA071          
640   SUM4=SUM4+TERM2                                                   THETA072          
650   M=-M                                                              THETA073          
      IF((TERM1+TERM2).LT.EP) GOTO 710                                  THETA074          
      A=A+C+H                                                           THETA075          
      B=B+C-H                                                           THETA076          
      C=C+F                                                             THETA077          
      GOTO 540                                                          THETA078          
710   THETA=SQRT(-PI*G)*(SUM1+SUM4-(SUM2+SUM3))                         THETA079          
      IF(.NOT.(I.EQ.0)) GOTO 740                                        THETA080          
      THETA=-THETA                                                      THETA081          
740   GOTO 1200                                                         THETA082          
750   IF(.NOT.(Q.EQ.0.)) GOTO 780                                       THETA083          
      THETA=1.                                                          THETA084          
      GOTO 1200                                                         THETA085          
780   THETA=0.                                                          THETA086          
      R=1.                                                              THETA087          
      EPD=EP*.5                                                         THETA088          
      IF(.NOT.(Q.LT..1)) GOTO 970                                       THETA089          
      D=COS(2.*Z)                                                       THETA090          
      C=D                                                               THETA091          
      E=SIN(2.*Z)                                                       THETA092          
      S=E                                                               THETA093          
      A=Q                                                               THETA094          
      P=Q*Q                                                             THETA095          
860   TERM=A*C                                                          THETA096          
      R=R*P                                                             THETA097          
      THETA=THETA+TERM                                                  THETA098          
      IF(ABS(A).LT.EPD) GOTO 950                                        THETA099          
      S1=S*D+C*E                                                        THETA100          
      C=C*D-S*E                                                         THETA101          
      S=S1                                                              THETA102          
      A=A*R*Q                                                           THETA103          
      GOTO 860                                                          THETA104          
950   THETA=1.+2.*THETA                                                 THETA105          
      GOTO 1200                                                         THETA106          
970   N=Z/PI                                                            THETA107          
      Z=Z-FLOAT(N)*PI                                                   THETA108          
      G=1./ALOG(Q)                                                      THETA109          
      THETA=EXP(Z*Z*G)                                                  THETA110          
      D=2.*PI*Z                                                         THETA111          
      E=PI*PI                                                           THETA112          
      F=2.*D                                                            THETA113          
      YNC=D-E                                                           THETA114          
      DEC=-D-E                                                          THETA115          
      D=Z+PI                                                            THETA116          
      A=D*D                                                             THETA117          
      D=Z-PI                                                            THETA118          
      B=D*D                                                             THETA119          
      C=4.*E                                                            THETA120          
1110  TERM=EXP(A*G)+EXP(B*G)                                            THETA121          
      THETA=THETA+TERM                                                  THETA122          
      IF(TERM.LT.EP) GOTO 1180                                          THETA123          
      A=A+C+YNC                                                         THETA124          
      B=B+C+DEC                                                         THETA125          
      C=C+F                                                             THETA126          
      GOTO 1110                                                         THETA127          
1180  THETA=SQRT(-PI*G)*THETA                                           THETA128          
1200  RETURN                                                            THETA129          
      END                                                               THETA130          
