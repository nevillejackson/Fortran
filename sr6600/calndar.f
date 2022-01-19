      SUBROUTINE JDAY(D,M,Y,J)                                          CALEN002          
      INTEGER D,M,Y,J,DD,MM,YY,JJ,C,YA                                  CALEN003          
C A.L.V. COOK, JULY 1971.                                               CALEN004          
C                                                                       CALEN005          
C JDAY CONVERTS A CALENDAR DATE, GREGORIAN CALENDAR, TO THE             CALEN006          
C  CORRESPONDING JULIAN DAY NUMBER, J.  FROM THE GIVEN DAY D,           CALEN007          
C  MONTH M,AND YEAR Y, ALL INTEGERS,  THE JULIAN DAY NUMBER J           CALEN008          
C  IS COMPUTED WITHOUT USING TABLES. THE PROCEDURE IS VALID             CALEN009          
C  FOR ANY GREGORIAN CALENDAR DATE                                      CALEN010          
C                                                                       CALEN011          
C JDAY IS THE TRANSLATION OF THE ALGOL PROCEDURE JDAY,CACM ALGORITHM    CALEN012          
C  199 - CONVERSIONS BETWEEN CALENDAR DATE AND JULIAN DAY               CALEN013          
C  NUMBER, ROBERT G.TANTZEN.                                            CALEN014          
C                                                                       CALEN015          
C FORMAL PARAMETERS-                                                    CALEN016          
C LOCAL VARIABLES-                                                      CALEN017          
C                                                                       CALEN018          
      YY=Y                                                              CALEN019          
      IF(M.GT.2) GOTO 110                                               CALEN020          
      MM=M+9                                                            CALEN021          
      YY=YY-1                                                           CALEN022          
70    C=YY/100                                                          CALEN023          
      YA=YY-100*C                                                       CALEN024          
      J=(146097*C)/4+(1461*YA)/4+(153*MM+2)/5+D+1721119                 CALEN025          
      GOTO 330                                                          CALEN026          
110   MM=M-3                                                            CALEN027          
      GOTO 70                                                           CALEN028          
C                                                                       CALEN029          
      ENTRY JDATE                                                       CALEN030          
C                                                                       CALEN031          
C JDATE CONVERTS A JULIAN DAY NUMBER TO THE CORRESPONDING CALENDAR      CALEN032          
C  DATE, GREGORIAN CALENDAR. SINCE J IS AN INTEGER FOR THIS             CALEN033          
C  PROCEDURE, IT IS CORRECT ASTRONOMICALLY FOR NOON OF THE              CALEN034          
C  DAY. JDATE COMPUTES THE DAY D, MONTH M, AND YEAR Y, WITHOUT          CALEN035          
C  USING TABLES THE PROCEDURE IS VALID FOR ANY VALID GREGORIAN          CALEN036          
C  CALENDAR DATE                                                        CALEN037          
C                                                                       CALEN038          
C JDATE IS A TRANSLATION OF THE ALGOL PROCEDURE JDATE, CACM ALGORITHM   CALEN039          
C  199.                                                                 CALEN040          
      JJ=J-1721119                                                      CALEN041          
      YY=(4*JJ-1)/146097                                                CALEN042          
      JJ=4*JJ-1-146097*YY                                               CALEN043          
      DD=JJ/4                                                           CALEN044          
      JJ=(4*DD+3)/1461                                                  CALEN045          
      DD=4*DD+3-1461*JJ                                                 CALEN046          
      DD=(DD+4)/4                                                       CALEN047          
      MM=(5*DD-3)/153                                                   CALEN048          
      DD=5*DD-3-153*MM                                                  CALEN049          
      D=(DD+5)/5                                                        CALEN050          
      YY=100*YY+JJ                                                      CALEN051          
      IF(MM.LT.10) GOTO 310                                             CALEN052          
      M=MM-9                                                            CALEN053          
      YY=YY+1                                                           CALEN054          
290   Y=YY                                                              CALEN055          
      GOTO 330                                                          CALEN056          
310   M=MM+3                                                            CALEN057          
      GOTO 290                                                          CALEN058          
330   RETURN                                                            CALEN059          
      END                                                               CALEN060          
