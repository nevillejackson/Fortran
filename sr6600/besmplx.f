      SUBROUTINE BESMPLX(REZ,AIMZ,N,BESJR,BESJI,BESYR,BESYI)            BESCM002          
      REAL ALOG                                                         BESCM003          
      INTEGER EXFLTF                                                    BESCM004          
      DIMENSION BESJR(1),BESJI(1),BESYR(1),BESYI(1)                     BESCM005          
C   AUTHORS - J.J.RUSSELL, R.W.MUNCEY, C.S.I.R.O. MAY,1967              BESCM006          
CALCULATE BESSEL FUNCTIONS WITH COMPLEX ARGUMENTS. ENTER WITH REAL AND  BESCM007          
C/  IMAGY PARTS OF Z IN REZ AND AIMZ. RESULTS FOR J AND Y TO ORDER N AREBESCM008          
C/  IN BESJR,BESJI ETC.  TECHNIQUE FOLLOWS J.J.RUSSELL.                 BESCM009          
C                                                                       BESCM010          
C IN THE COMPLEX PLANE ERRORS INCREASE EXPONENTIALLY WITH ARGUMENT      BESCM011          
C  ASYMPTOTIC FORMULAE MUST BE USED                                     BESCM012          
C  SEE MICHELS    NASA TN D2141  GIVES UNTESTED ASYMPTOTIC              BESCM013          
C  RECURRENCE FORMULAE                                                  BESCM014          
C                                                                       BESCM015          
      IFLAG=0                                                           BESCM016          
      ZD=REZ*REZ+AIMZ*AIMZ                                              BESCM017          
      ZMOD=SQRT(ZD)                                                     BESCM018          
50    L=2*IFIX(6.5+ZMOD-8.E-4*ZD+8.E-10*ZD*ZD)                          BESCM019          
      IF(L-N-30) 70,80,80                                               BESCM020          
70    L=2*(15+N/2)                                                      BESCM021          
80    IF(.NOT.(N.GT.100.OR.ZMOD.GT.1000..OR.ZMOD.LT.1.E-9)) GOTO 120    BESCM022          
      WRITE(61,100) REZ,AIMZ,N                                          BESCM023          
100   FORMAT(//,10X,8HBESCMPLX,8H REJECT.,8H Z OR N ,8HOFF SCAL,3HE. ,  BESCM024          
     .2E18.8,I9)                                                        BESCM025          
110   GOTO 880                                                          BESCM026          
120   J=EXFLTF(J)                                                       BESCM027          
      ZRR=REZ/ZD                                                        BESCM028          
      ZRI=-AIMZ/ZD                                                      BESCM029          
      ZSQRR=(ZRR*ZRR-ZRI*ZRI)*4E0                                       BESCM030          
      ZSQRI=ZRR*ZRI*8E0                                                 BESCM031          
      IF(IFLAG-1) 230,180,200                                           BESCM032          
180   AD=1.E-250                                                        BESCM033          
      AC=AD $ GOTO 240                                                  BESCM034          
200   WRITE(61,220) $ GOTO 880                                          BESCM035          
220   FORMAT(8H OVERFLO,8HW HAS OC,8HCURRED A,8HFTER TWO,8H STARTIN,    BESCM036          
     .8HG ATTEMP,8HTS, CALL,8H REJECTE,8HD, RESUL,8HTS UNDEF,6HINED  )  BESCM037          
230   AD=1.E-25                                                         BESCM038          
      AC=AD                                                             BESCM039          
240   BD=0                                                              BESCM040          
      BC=BD                                                             BESCM041          
      BB=BC                                                             BESCM042          
      BA=BB                                                             BESCM043          
      AB=BA                                                             BESCM044          
      AA=AB                                                             BESCM045          
      J=L-L/4*4-1                                                       BESCM046          
      GOTO 360                                                          BESCM047          
270   BA=BA+2E0*AE                                                      BESCM048          
      BB=BB+2E0*AF                                                      BESCM049          
      A=4.*FLOAT(J)/FLOAT(L)                                            BESCM050          
      BC=BC+A*AE                                                        BESCM051          
      BD=BD+A*AF                                                        BESCM052          
      AA=AC                                                             BESCM053          
      AB=AD                                                             BESCM054          
      AC=AE                                                             BESCM055          
      AD=AF                                                             BESCM056          
360   A=FLOAT(L)*FLOAT(L-1)*ZSQRR-2.*FLOAT(L)/FLOAT(L+1)                BESCM057          
      B=FLOAT(L)*FLOAT(L-1)*ZSQRI                                       BESCM058          
      C=(1.-FLOAT(L))/FLOAT(1+L)                                        BESCM059          
      AE=A*AC-B*AD+C*AA                                                 BESCM060          
      AF=A*AD+B*AC+C*AB                                                 BESCM061          
      IF(L-3-N) 420,420,440                                             BESCM062          
420   BESJR(L-1)=AE                                                     BESCM063          
      BESJI(L-1)=AF                                                     BESCM064          
440   J=-J                                                              BESCM065          
      L=L-2                                                             BESCM066          
      IF(L.NE.0) GOTO 270                                               BESCM067          
      BA=BA+AE                                                          BESCM068          
      BB=BB+AF                                                          BESCM069          
      A=1E0/(BA*BA+BB*BB)                                               BESCM070          
      AA=A*BA                                                           BESCM071          
      AB=-A*BB                                                          BESCM072          
      BESJR(1)=AA*AE-AB*AF                                              BESCM073          
      BESJI(1)=AA*AF+AB*AE                                              BESCM074          
      IF(.NOT.(ABS(REZ).LT.1.E-20)) GOTO 570                            BESCM075          
      B=SIGN(1.570796327,AIMZ)                                          BESCM076          
      GOTO 590                                                          BESCM077          
570   B=ATAN(AIMZ/REZ)                                                  BESCM078          
      B=B+SIGN(3.141592654,B)*(SIGN(0.5,REZ)-0.5)                       BESCM079          
590   A=ALOG(ZMOD)-0.115931516                                          BESCM080          
      BESYR(1)=(A*BESJR(1)-B*BESJI(1)+AA*BC-AB*BD)*0.6366197724         BESCM081          
      BESYI(1)=(A*BESJI(1)+B*BESJR(1)+AA*BD+AB*BC)*0.6366197724         BESCM082          
      L=1                                                               BESCM083          
      PIZR=-ZRR*0.6366197724                                            BESCM084          
      PIZI=-ZRI*0.6366197724                                            BESCM085          
      IF(EXFLTF(J).EQ.2) GOTO 700                                       BESCM086          
      WRITE(61,670) REZ,AIMZ,N                                          BESCM087          
670   FORMAT(/,10X,8HEXFLT ER,8HROR IN N,8HORMALISI,8HNG SUM B,6HESSEL  BESCM088          
     .,2E15.5,I6/10X,8HSECOND A,8HTTEMPT W,8HITH NEW ,8HSTARTING,       BESCM089          
     .6H POINT)                                                         BESCM090          
      IFLAG=IFLAG+1 $ GOTO 50                                           BESCM091          
700   C=BESJR(L+2)                                                      BESCM092          
      BESJR(L+2)=AA*C-AB*BESJI(L+2)                                     BESCM093          
      BESJI(L+2)=AA*BESJI(L+2)+AB*C                                     BESCM094          
      A=(BESJR(L)+BESJR(L+2))/FLOAT(2*L)                                BESCM095          
      B=(BESJI(L)+BESJI(L+2))/FLOAT(2*L)                                BESCM096          
      BESJR(L+1)=REZ*A-AIMZ*B                                           BESCM097          
      BESJI(L+1)=REZ*B+AIMZ*A                                           BESCM098          
770   C=1E0/(BESJR(L)*BESJR(L)+BESJI(L)*BESJI(L))                       BESCM099          
      A=(BESJR(L+1)*BESYR(L)-BESJI(L+1)*BESYI(L)+PIZR)*C                BESCM100          
      B=(BESJR(L+1)*BESYI(L)+BESJI(L+1)*BESYR(L)+PIZI)*C                BESCM101          
      BESYR(L+1)=(BESJR(L)*A+BESJI(L)*B)                                BESCM102          
      BESYI(L+1)=(BESJR(L)*B-BESJI(L)*A)                                BESCM103          
      L=L+1                                                             BESCM104          
      IF(L.EQ.L/2*2) GOTO 770                                           BESCM105          
      IF(EXFLTF(J).EQ.2) GOTO 870                                       BESCM106          
      WRITE(61,860) L                                                   BESCM107          
860   FORMAT(/,10X,8HERROR EX,8HFLT IN B,8HESSEL RE,8HCURRENCE,5H. L= , BESCM108          
     .I6)                                                               BESCM109          
870   IF(L.GE.N+1) GOTO 110                                             BESCM110          
      GOTO 700                                                          BESCM111          
880   RETURN                                                            BESCM112          
      END                                                               BESCM113          
