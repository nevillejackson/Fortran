      FUNCTION GAUSINT(TX,MX)                                           GAUSI002          
      DIMENSION F0(1),F(200)                                            GAUSI003          
C EVALUATION OF INTEGRAL OF X**(2*M)*EXP(-T*X*X) FROM X=0 TO X=1        GAUSI004          
C FIRST PARAMETER IS T (REAL), SECOND IS M (INTEGER)                    GAUSI005          
C AUTHOR C.H.J. JOHNSON, CSIRO DIVISION OF APPLIED CHEMISTRY, JULY 1969 GAUSI006          
      T=TX $ MM=MX                                                      GAUSI007          
      MMAX=FLOAT(MM+50)+T                                               GAUSI008          
      IF(.NOT.(MMAX.GT.200)) GOTO 90                                    GAUSI009          
      WRITE(61,80)                                                      GAUSI010          
      STOP                                                              GAUSI011          
80    FORMAT(8H     DIM,8HENSION O,8H/FLOW IN,8H GAUSINT)               GAUSI012          
90    F(MMAX)=1.0E-50                                                   GAUSI013          
      EXPMT=EXP(-T) $ TWOT=T+T                                          GAUSI014          
      M=MMAX                                                            GAUSI015          
130   M=M-1                                                             GAUSI016          
      IF(M.LT.0) GOTO 170                                               GAUSI017          
      F(M)=(TWOT*F(M+1)+EXPMT)/FLOAT(M+M+1)                             GAUSI018          
      GOTO 130                                                          GAUSI019          
170   SUM=F(M+1)                                                        GAUSI020          
      FACT=1.0                                                          GAUSI021          
      M=1                                                               GAUSI022          
191   IF(M.GT.MMAX) GOTO 199                                            GAUSI023          
      FACT=FACT*T/FLOAT(M)                                              GAUSI024          
      SUM=FACT*F(M)+SUM                                                 GAUSI025          
      M=M+1                                                             GAUSI026          
      GOTO 191                                                          GAUSI027          
199   CONTINUE                                                          GAUSI028          
      GAUSINT=F(MM)/SUM                                                 GAUSI029          
      RETURN                                                            GAUSI030          
      END                                                               GAUSI031          
