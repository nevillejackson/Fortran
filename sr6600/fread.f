      SUBROUTINE FREAD(A,J)                                             FREAD002          
      DIMENSION N(80),A(1000)                                           FREAD003          
C                                                                       FREAD004          
C FORMAT-FREE READING OF REAL NUMBERS - READS CONSECUTIVE VALUES INTO   FREAD005          
C BEGINNING OF ARRAY A AND STORES NUMBER OF VALUES READ IN J.           FREAD006          
C                                                                       FREAD007          
C                                                                       FREAD008          
C                                                                       FREAD009          
C THE EFFECTIVE FIELD CONTINUES, ON TO FURTHER CARDS IF NECESSARY,      FREAD010          
C UNTIL A NEW FIELD IS STARTED BY ANY NUMBER OF BLANKS, A+, -, OR A     FREAD011          
C DECIMAL POINT IN A FIELD ALREADY CONTAINING A POINT. ANY FIELD WITH   FREAD012          
C NO POINT IS ASSUMED TO HAVE A POINT AFTER THE LAST DIGIT. READING     FREAD013          
C CONTINUES TO THE FIRST COLUMN CONTAINING A CHARACTER OTHER THAN A     FREAD014          
C NUMERAL, BLANK, +, -, OR DECIMAL POINT.                               FREAD015          
C                                                                       FREAD016          
C AUTHOR - D.R.SKINNER, D.S.L., MARIBYRNONG.                            FREAD017          
C                                                                       FREAD018          
20    FORMAT(80R1)                                                      FREAD019          
      S=1.                                                              FREAD020          
      J=0                                                               FREAD021          
      K=0                                                               FREAD022          
      L=0                                                               FREAD023          
      AM=0.                                                             FREAD024          
      AMK=0.                                                            FREAD025          
90    READ(60,20) N                                                     FREAD026          
      I=1                                                               FREAD027          
110   IF(.NOT.(N(I).LT.10)) GOTO 210                                    FREAD028          
      AN=N(I)                                                           FREAD029          
      IF(.NOT.(K.EQ.0)) GOTO 180                                        FREAD030          
      AM=10.*AM+AN                                                      FREAD031          
150   L=1                                                               FREAD032          
160   I=I+1                                                             FREAD033          
      IF(I.LE.80) GOTO 110                                              FREAD034          
      GOTO 90                                                           FREAD035          
180   K=K+1                                                             FREAD036          
      AMK=10.*AMK+AN                                                    FREAD037          
      GOTO 150                                                          FREAD038          
210   IF(.NOT.(N(I).EQ.48)) GOTO 370                                    FREAD039          
      IF(L.EQ.0) GOTO 160                                               FREAD040          
      SA=S                                                              FREAD041          
      S=1.                                                              FREAD042          
      KA=K-1                                                            FREAD043          
      K=0                                                               FREAD044          
270   J=J+1                                                             FREAD045          
      AK=1.                                                             FREAD046          
      II=1                                                              FREAD047          
291   IF(II.GT.KA) GOTO 299                                             FREAD048          
      AK=10.*AK                                                         FREAD049          
      II=II+1                                                           FREAD050          
      GOTO 291                                                          FREAD051          
299   CONTINUE                                                          FREAD052          
      AJ=AM+AMK/AK                                                      FREAD053          
      A(J)=SIGN(AJ,SA)                                                  FREAD054          
      L=0                                                               FREAD055          
      AM=0.                                                             FREAD056          
      AMK=0.                                                            FREAD057          
      GOTO 160                                                          FREAD058          
370   IF(.NOT.(N(I).EQ.32)) GOTO 440                                    FREAD059          
      SA=S                                                              FREAD060          
      S=-1.                                                             FREAD061          
      IF(L.EQ.0) GOTO 160                                               FREAD062          
410   KA=K-1                                                            FREAD063          
      K=0                                                               FREAD064          
      GOTO 270                                                          FREAD065          
440   IF(.NOT.(N(I).EQ.16)) GOTO 490                                    FREAD066          
      IF(L.EQ.0) GOTO 160                                               FREAD067          
      SA=S                                                              FREAD068          
      S=1.                                                              FREAD069          
      GOTO 410                                                          FREAD070          
490   IF(.NOT.(N(I).EQ.27)) GOTO 580                                    FREAD071          
      IF(.NOT.(K.NE.0)) GOTO 560                                        FREAD072          
      KA=K-1                                                            FREAD073          
      K=1                                                               FREAD074          
      SA=S                                                              FREAD075          
      S=1.                                                              FREAD076          
      GOTO 270                                                          FREAD077          
560   K=1                                                               FREAD078          
      GOTO 160                                                          FREAD079          
580   IF(L.EQ.0) GOTO 660                                               FREAD080          
      J=J+1                                                             FREAD081          
      KA=K-1                                                            FREAD082          
      AK=1.                                                             FREAD083          
      II=1                                                              FREAD084          
621   IF(II.GT.KA) GOTO 629                                             FREAD085          
      AK=10.*AK                                                         FREAD086          
      II=II+1                                                           FREAD087          
      GOTO 621                                                          FREAD088          
629   CONTINUE                                                          FREAD089          
      AJ=AM+AMK/AK                                                      FREAD090          
      A(J)=SIGN(AJ,S)                                                   FREAD091          
660   CONTINUE                                                          FREAD092          
      RETURN                                                            FREAD093          
      END                                                               FREAD094          
