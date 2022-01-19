      SUBROUTINE SHESORT(X,N,KEY)                                       SHELS002          
      DIMENSION X(1),KEY(1)                                             SHELS003          
C     SHELL, MODIFIED FRANK AND LAZARUS, CACM 3,20 (1960)               SHELS004          
C     TO MAKE KEY TO ORIGINAL ORDER, USE NEGATIVE VALUE OF N            SHELS005          
C     TO SORT INTEGERS, USE    INTEGER X, TEMP                          SHELS006          
      NO=N                                                              SHELS007          
      MO=NO                                                             SHELS008          
      ASSIGN 260 TO KEEPER                                              SHELS009          
      IF(.NOT.(NO.LT.0)) GOTO 90                                        SHELS010          
      NO=-NO                                                            SHELS011          
      MO=NO                                                             SHELS012          
      ASSIGN 230 TO KEEPER                                              SHELS013          
      I=1                                                               SHELS014          
71    IF(I.GT.NO) GOTO 79                                               SHELS015          
      KEY(I)=I                                                          SHELS016          
      I=I+1                                                             SHELS017          
      GOTO 71                                                           SHELS018          
79    CONTINUE                                                          SHELS019          
90    IF(.NOT.(MO.LE.15)) GOTO 130                                      SHELS020          
      IF(.NOT.(MO.GT.1)) GOTO 300                                       SHELS021          
      MO=2*(MO/4)+1                                                     SHELS022          
      GOTO 140                                                          SHELS023          
130   MO=2*(MO/8)+1                                                     SHELS024          
140   KO=NO-MO                                                          SHELS025          
      JO=1                                                              SHELS026          
160   I=JO                                                              SHELS027          
170   IMO=I+MO                                                          SHELS028          
      IF(.NOT.(X(I).GT.X(IMO))) GOTO 280                                SHELS029          
      TEMP=X(I)                                                         SHELS030          
      X(I)=X(IMO)                                                       SHELS031          
      X(IMO)=TEMP                                                       SHELS032          
          GO TO KEEPER(230,260)                                                           
230   KEMP=KEY(I)                                                       SHELS034          
      KEY(I)=KEY(IMO)                                                   SHELS035          
      KEY(IMO)=KEMP                                                     SHELS036          
260   I=I-MO                                                            SHELS037          
      IF(.NOT.(I.LT.1)) GOTO 170                                        SHELS038          
280   JO=JO+1                                                           SHELS039          
      IF(JO.GT.KO) GOTO 90                                              SHELS040          
      GOTO 160                                                          SHELS041          
300   RETURN                                                            SHELS042          
      END                                                               SHELS043          
