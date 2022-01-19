      SUBROUTINE FOURAN2(NHAR,NORD,HARMC,HARMS,ORDS)                    FOURA002          
      DIMENSION HARMC(1),HARMS(1),ORDS(1),C(401),S(401)                 FOURA003          
      IFLAG=0                                                           FOURA004          
      IF(NORD-401) 70,70,450                                            FOURA005          
      ENTRY FOUSYN2                                                     FOURA006          
      IFLAG=1                                                           FOURA007          
      IF(NORD-401) 70,70,450                                            FOURA008          
70    K1=1                                                              FOURA009          
71    IF(K1.GT.NORD) GOTO 79                                            FOURA010          
      ARG=FLOAT(K1-1)*6.283185307/FLOAT(NORD)                           FOURA011          
      C(K1)=COS(ARG)                                                    FOURA012          
      S(K1)=SIN(ARG)                                                    FOURA013          
      K1=K1+1                                                           FOURA014          
      GOTO 71                                                           FOURA015          
79    CONTINUE                                                          FOURA016          
      K1=K1-1                                                           FOURA017          
      IF(IFLAG) 530,140,530                                             FOURA018          
      ENTRY FOUNTAB                                                     FOURA019          
140   IF(NORD.NE.K1) GOTO 480                                           FOURA020          
      M1=NORD/2                                                         FOURA021          
      M2=M1+1                                                           FOURA022          
      IF(NHAR-M2) 210,210,180                                           FOURA023          
180   WRITE(61,190) NHAR                                                FOURA024          
190   FORMAT(/8H TOO MAN,8HY HARMON,8HICS CALL,6HED FOR,I4)             FOURA025          
      NHAR=M2                                                           FOURA026          
210   M3=(NORD+1)/2                                                     FOURA027          
      XB=0.                                                             FOURA028          
      XA=XB                                                             FOURA029          
      K=1                                                               FOURA030          
231   IF(K.GT.NORD) GOTO 239                                            FOURA031          
      XA=ORDS(K)+XA                                                     FOURA032          
      XB=ORDS(K)-XB                                                     FOURA033          
      K=K+1                                                             FOURA034          
      GOTO 231                                                          FOURA035          
239   CONTINUE                                                          FOURA036          
      HARMC(1)=XA/FLOAT(NORD)                                           FOURA037          
      HARMS(1)=0.                                                       FOURA038          
      L=NHAR                                                            FOURA039          
      IF(.NOT.(M1.EQ.M3.AND.NHAR.EQ.M2)) GOTO 330                       FOURA040          
      HARMC(NHAR)=-XB/FLOAT(NORD)                                       FOURA041          
      HARMS(NHAR)=0.                                                    FOURA042          
      L=L-1                                                             FOURA043          
330   K=2                                                               FOURA044          
331   IF(K.GT.L) GOTO 339                                               FOURA045          
      XB=0                                                              FOURA046          
      XA=XB                                                             FOURA047          
      N=1                                                               FOURA048          
      J=1                                                               FOURA049          
361   IF(J.GT.NORD) GOTO 369                                            FOURA050          
      XA=XA+ORDS(J)*C(N)                                                FOURA051          
      XB=XB+ORDS(J)*S(N)                                                FOURA052          
      IF(N+K-NORD-2) 410,400,400                                        FOURA053          
400   N=N-NORD                                                          FOURA054          
410   N=N+K-1                                                           FOURA055          
      J=J+1                                                             FOURA056          
      GOTO 361                                                          FOURA057          
369   CONTINUE                                                          FOURA058          
      HARMC(K)=2E0*XA/FLOAT(NORD)                                       FOURA059          
      HARMS(K)=2E0*XB/FLOAT(NORD)                                       FOURA060          
      K=K+1                                                             FOURA061          
      GOTO 331                                                          FOURA062          
339   CONTINUE                                                          FOURA063          
      GOTO 470                                                          FOURA064          
450   WRITE(61,460) NORD                                                FOURA065          
460   FORMAT(/I4,8H TOO MAN,8HY ORDINA,8HTES  -  ,8HALTER DI,8HMENSIONS FOURA066          
     .,8H OF C AN,8HD S ARRA,8HYS TO EQ,8HUAL NUMB,8HER OF OR,7HDINATES FOURA067          
     .)                                                                 FOURA068          
470   GOTO 650                                                          FOURA069          
480   WRITE(61,490) NORD                                                FOURA070          
490   FORMAT(/1X,I4,2X,8HNOTE THA,8HT THE NU,8HMBER OF ,8HORDINATE,     FOURA071          
     .8HS CALLED,8H FOR UND,8HER FOURN,6HTAB OR,/,1X,8HFSYNTAB ,        FOURA072          
     .8HMUST BE ,8HTHE SAME,8H AS THE ,8HNUMBER U,8HSED FOR ,8HFOURAN2  FOURA073          
     .,8HOR FOURS,4HYN2.)                                               FOURA074          
      GOTO 650                                                          FOURA075          
      ENTRY FSYNTAB                                                     FOURA076          
      IF(NORD.NE.K1) GOTO 480                                           FOURA077          
530   L=NORD/2+1                                                        FOURA078          
      IF(NHAR-L) 550,560,560                                            FOURA079          
550   L=NHAR                                                            FOURA080          
560   J=1                                                               FOURA081          
561   IF(J.GT.NORD) GOTO 569                                            FOURA082          
      Q=0                                                               FOURA083          
      N=1                                                               FOURA084          
      K=1                                                               FOURA085          
591   IF(K.GT.L) GOTO 599                                               FOURA086          
      Q=Q+HARMC(K)*C(N)+HARMS(K)*S(N)                                   FOURA087          
      IF(N+J-NORD-2) 630,620,620                                        FOURA088          
620   N=N-NORD                                                          FOURA089          
630   N=N+J-1                                                           FOURA090          
      K=K+1                                                             FOURA091          
      GOTO 591                                                          FOURA092          
599   CONTINUE                                                          FOURA093          
      ORDS(J)=Q                                                         FOURA094          
      J=J+1                                                             FOURA095          
      GOTO 561                                                          FOURA096          
569   CONTINUE                                                          FOURA097          
650   RETURN                                                            FOURA098          
      END                                                               FOURA099          
