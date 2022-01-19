      FUNCTION GAMINC(X,ALPH)                                           GAMIN002          
C                                                                       GAMIN003          
C***   PROGRAMMED BY B. BROWN, R.T. LESLIE AND D.E. SHAW, C.S.I.R.O.    GAMIN004          
C***   NEWTOWN, N.S.W., AUSTRALIA                                       GAMIN005          
C                                                                       GAMIN006          
      IF(X.LT.1E-30) GOTO 390                                           GAMIN007          
      GALMPH=GAMMA(ALPH)                                                GAMIN008          
      IF(.NOT.(X.LT.1.)) GOTO 130                                       GAMIN009          
      PROG=X**ALPH/(ALPH*GALMPH)                                        GAMIN010          
      SUM=PROG                                                          GAMIN011          
      ALPH1=ALPH-1.                                                     GAMIN012          
      J=1                                                               GAMIN013          
81    IF(J.GT.11) GOTO 89                                               GAMIN014          
      PROG=-X*(ALPH1+FLOAT(J))*PROG/(FLOAT(J)*(ALPH+FLOAT(J)))          GAMIN015          
      SUM=SUM+PROG                                                      GAMIN016          
      J=J+1                                                             GAMIN017          
      GOTO 81                                                           GAMIN018          
89    CONTINUE                                                          GAMIN019          
      GAMINC=SUM                                                        GAMIN020          
      GOTO 380                                                          GAMIN021          
130   NALPH=ALPH                                                        GAMIN022          
      RESAL=ALPH-FLOAT(NALPH)                                           GAMIN023          
      IF(RESAL) 160,160,210                                             GAMIN024          
160   RESAL=RESAL+1.                                                    GAMIN025          
      NALPH=NALPH-1                                                     GAMIN026          
      GRAM=GAMMA(RESAL)                                                 GAMIN027          
      GAMINC=(1.-EXP(-X))/GRAM                                          GAMIN028          
      GOTO 310                                                          GAMIN029          
210   GAMINC=1./X                                                       GAMIN030          
      NAIL=40                                                           GAMIN031          
      NAIL1=NAIL+1                                                      GAMIN032          
      ALPH1=FLOAT(NAIL1)-RESAL                                          GAMIN033          
      J=1                                                               GAMIN034          
251   IF(J.GT.NAIL) GOTO 259                                            GAMIN035          
      GAMINC=1./(X+(ALPH1-FLOAT(J))/(GAMINC*FLOAT(NAIL1-J)+1.))         GAMIN036          
      J=J+1                                                             GAMIN037          
      GOTO 251                                                          GAMIN038          
259   CONTINUE                                                          GAMIN039          
      GAMINC=GAMINC*EXP(-X)*X**RESAL                                    GAMIN040          
      GRAM=GAMMA(RESAL)                                                 GAMIN041          
      GAMINC=1.-GAMINC/GRAM                                             GAMIN042          
      IF(NALPH) 380,380,310                                             GAMIN043          
310   PROG=1./ALPH                                                      GAMIN044          
      SUM=0.                                                            GAMIN045          
      ALPH1=ALPH+1.                                                     GAMIN046          
      J=1                                                               GAMIN047          
341   IF(J.GT.NALPH) GOTO 349                                           GAMIN048          
      PROG=PROG*(ALPH1-FLOAT(J))/X                                      GAMIN049          
      SUM=SUM+PROG                                                      GAMIN050          
      J=J+1                                                             GAMIN051          
      GOTO 341                                                          GAMIN052          
349   CONTINUE                                                          GAMIN053          
      GAMINC=GAMINC-EXP(-X)*X**ALPH*SUM/GALMPH                          GAMIN054          
380   IF(GAMINC) 390,400,400                                            GAMIN055          
390   GAMINC=0.                                                         GAMIN056          
400   CONTINUE                                                          GAMIN057          
      RETURN                                                            GAMIN058          
      END                                                               GAMIN059          
