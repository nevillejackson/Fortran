      SUBROUTINE FOURANG(NHAR,NORD,HARMC,HARMS,ORDS)                    FOURA002          
      DIMENSION HARMC(1),HARMS(1),ORDS(1)                               FOURA003          
      M1=NORD/2                                                         FOURA004          
      M2=M1+1                                                           FOURA005          
      M3=(NORD+1)/2                                                     FOURA006          
      IF(NHAR-M2) 90,90,60                                              FOURA007          
60    WRITE(61,70) NHAR,M2                                              FOURA008          
70    FORMAT(/8H TOO MAN,8HY HARMON,8HICS CALL,8HED FOR (,I5,8H),PROGRA FOURA009          
     .,8HM HAS CA,8HLCULATED,8H MAXIMUM,8H NUMBER ,8HPOSSIBLE,1H(,I5,   FOURA010          
     .1H)//)                                                            FOURA011          
      NHAR=M2                                                           FOURA012          
90    ARG=6.283185307/FLOAT(NORD)                                       FOURA013          
      CB=COS(ARG)                                                       FOURA014          
      SB=SIN(ARG)                                                       FOURA015          
      CA=1.                                                             FOURA016          
      SA=0.                                                             FOURA017          
      K=1                                                               FOURA018          
141   IF(K.GT.NHAR) GOTO 149                                            FOURA019          
      UC=0.                                                             FOURA020          
      UB=UC                                                             FOURA021          
      J=NORD                                                            FOURA022          
170   UA=ORDS(J)+2E0*CA*UB-UC                                           FOURA023          
      UC=UB                                                             FOURA024          
      UB=UA                                                             FOURA025          
      J=J-1                                                             FOURA026          
      IF(J-1) 220,220,170                                               FOURA027          
220   HARMC(K)=2E0*(ORDS(1)+CA*UB-UC)/FLOAT(NORD)                       FOURA028          
      HARMS(K)=2E0*SA*UB/FLOAT(NORD)                                    FOURA029          
      UA=CB*CA-SB*SA                                                    FOURA030          
      SA=CB*SA+SB*CA                                                    FOURA031          
      CA=UA                                                             FOURA032          
      K=K+1                                                             FOURA033          
      GOTO 141                                                          FOURA034          
149   CONTINUE                                                          FOURA035          
      HARMC(1)=HARMC(1)/2.                                              FOURA036          
      IF(.NOT.(M1.EQ.M3.AND.NHAR.EQ.M2)) GOTO 310                       FOURA037          
      HARMC(NHAR)=HARMC(NHAR)/2.                                        FOURA038          
      HARMS(NHAR)=0.                                                    FOURA039          
310   GOTO 590                                                          FOURA040          
      ENTRY FOUSYNG                                                     FOURA041          
      ARG=6.283185307/FLOAT(NORD)                                       FOURA042          
      CB=COS(ARG)                                                       FOURA043          
      CA=CB                                                             FOURA044          
      SB=SIN(ARG)                                                       FOURA045          
      SA=SB                                                             FOURA046          
      K=2                                                               FOURA047          
361   IF(K.GT.NHAR) GOTO 369                                            FOURA048          
      VC=0.                                                             FOURA049          
      VB=VC                                                             FOURA050          
      UC=VB                                                             FOURA051          
      UB=UC                                                             FOURA052          
      J=NHAR                                                            FOURA053          
390   UA=HARMC(J)-UC+2E0*CA*UB                                          FOURA054          
      VA=HARMS(J)-VC+2E0*CA*VB                                          FOURA055          
      UC=UB                                                             FOURA056          
      UB=UA                                                             FOURA057          
      VC=VB                                                             FOURA058          
      VB=VA                                                             FOURA059          
      J=J-1                                                             FOURA060          
      IF(J-1) 470,470,390                                               FOURA061          
470   XB=HARMC(1)-UC+CA*UB                                              FOURA062          
      XC=VB*SA                                                          FOURA063          
      ORDS(K)=XB+XC                                                     FOURA064          
      KMOD=NORD-K+2                                                     FOURA065          
      ORDS(KMOD)=XB-XC                                                  FOURA066          
      UA=CB*CA-SB*SA                                                    FOURA067          
      SA=CB*SA+SB*CA                                                    FOURA068          
      CA=UA                                                             FOURA069          
      K=K+1                                                             FOURA070          
      GOTO 361                                                          FOURA071          
369   CONTINUE                                                          FOURA072          
      XBA=0                                                             FOURA073          
      J=1                                                               FOURA074          
561   IF(J.GT.NHAR) GOTO 569                                            FOURA075          
      XBA=XBA+HARMC(J)                                                  FOURA076          
      J=J+1                                                             FOURA077          
      GOTO 561                                                          FOURA078          
569   CONTINUE                                                          FOURA079          
      ORDS(1)=XBA                                                       FOURA080          
590   RETURN                                                            FOURA081          
      END                                                               FOURA082          
