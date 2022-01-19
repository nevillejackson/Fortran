      SUBROUTINE PERSPEX(NSQ,SQS,NP,DISP,IPAIRS)                        PERSP002          
      REAL MAX,MIN                                                      PERSP003          
      DIMENSION IPAIRS(2,45),XKN(10,300),Y(300),R(2,30),MAX(2,30),MIN(2 PERSP004          
     .,30),LP(30),FM(30)                                                PERSP005          
      COMMON/GLASS/NEVAL,N,XKN,Y                                        PERSP006          
      COMMON/SZKLO/R,MAX,MIN,LP,FM,LR,S,N1,N2,D                         PERSP007          
C   J.A.B.PALMER AND K.SERKOWSKA, DIV.OF COMP.RES.,CANBERRA.            PERSP008          
C  NSQ  NUMBER OF SQUARES REQUIRED (LE 30).                             PERSP009          
C  SQS  SIZE OF SQUARE IN INCHES.FOR  PL,SQS LE 4.,FOR  PB,LE 12.       PERSP010          
C  NP   NUMBER OF PAIRS OF INDEPENDENT VARIABLES FOR WHICH A MINIMIZING PERSP011          
C  SEQUENCE IS TO BE PLOTTED.                                           PERSP012          
C  DISP DISPLACEMENT,IF DISP=0, PERSPEX WILL SET DISP=0.1 .             PERSP013          
C  IPAIRS  IS AN ARRAY CONTAINING THE PAIRS OF VARIABLES TO BE DRAWN.   PERSP014          
C  THUS IPAIRS(1,3),IPAIRS(2,3) IS THIRD PAIR OF VARIABLES TO BE PLOTTEDPERSP015          
C  IPAIRS MUST BE DIMENSIONED AT LEAST (2,NP).                          PERSP016          
      S=SQS                                                             PERSP017          
      IF(.NOT.(DISP.EQ.0E0)) GOTO 50                                    PERSP018          
      DISP=0.1                                                          PERSP019          
50    D=DISP                                                            PERSP020          
      XSTEP=1.25*S                                                      PERSP021          
      NN=1                                                              PERSP022          
71    IF(NN.GT.NP) GOTO 79                                              PERSP023          
      N1=IPAIRS(1,NN)                                                   PERSP024          
      N2=IPAIRS(2,NN)                                                   PERSP025          
      XMIN=1.E300                                                       PERSP026          
      YMIN=1.E300                                                       PERSP027          
      ST=0                                                              PERSP028          
      J=1                                                               PERSP029          
      CALL PLOT(1.,1.,2)                                                PERSP030          
      CALL PLOT(0,0,1)                                                  PERSP031          
      ST=ST+0.4*S                                                       PERSP032          
      CALL PLOT(ST,1.,3)                                                PERSP033          
      ENCODE(1,250,L1) N1                                               PERSP034          
      CALL TEXT(L1,1,2)                                                 PERSP035          
      ST=ST+0.2*S                                                       PERSP036          
      CALL PLOT(ST,1.,3)                                                PERSP037          
      ENCODE(1,250,L2) N2                                               PERSP038          
      CALL TEXT(L2,1,2)                                                 PERSP039          
      CALL PLOT(0,1.,3)                                                 PERSP040          
250   FORMAT(I1)                                                        PERSP041          
      INC=((NEVAL-1)/NSQ)+1                                             PERSP042          
      IB=1                                                              PERSP043          
271   IF(IB.GT.2) GOTO 279                                              PERSP044          
      LR=3-2*IB                                                         PERSP045          
      XMIN=XKN(N1,NEVAL)                                                PERSP046          
      XMAX=XMIN                                                         PERSP047          
      YMIN=XKN(N2,NEVAL)                                                PERSP048          
      YMAX=YMIN                                                         PERSP049          
      MAX(1,1)=XMAX                                                     PERSP050          
      MIN(1,1)=XMIN                                                     PERSP051          
      MAX(2,1)=YMAX                                                     PERSP052          
      MIN(2,1)=YMIN                                                     PERSP053          
      LP(J)=1                                                           PERSP054          
      R(1,1)=0                                                          PERSP055          
      R(2,1)=0                                                          PERSP056          
      FM(1)=0                                                           PERSP057          
      NOT=NEVAL-INC                                                     PERSP058          
      II=1                                                              PERSP059          
401   IF(II.GT.NSQ) GOTO 409                                            PERSP060          
      IF(NOT.LT.1) GOTO 720                                             PERSP061          
420   FMAX=0                                                            PERSP062          
      I=NOT                                                             PERSP063          
431   IF(I.GT.NEVAL) GOTO 439                                           PERSP064          
      IF(FMAX.GT.ABS(Y(I))) GOTO 460                                    PERSP065          
      FMAX=ABS(Y(I))                                                    PERSP066          
460   CONTINUE                                                          PERSP067          
      I=I+1                                                             PERSP068          
      GOTO 431                                                          PERSP069          
439   CONTINUE                                                          PERSP070          
      I=NOT                                                             PERSP071          
471   IF(I.GT.NEVAL) GOTO 479                                           PERSP072          
      X1=XKN(N1,I)                                                      PERSP073          
      X2=XKN(N2,I)                                                      PERSP074          
      IF(.NOT.(X1.GT.XMAX)) GOTO 510                                    PERSP075          
      XMAX=X1                                                           PERSP076          
510   CONTINUE                                                          PERSP077          
      IF(.NOT.(X1.LE.XMIN)) GOTO 530                                    PERSP078          
      XMIN=X1                                                           PERSP079          
530   CONTINUE                                                          PERSP080          
      IF(.NOT.(X2.GT.YMAX)) GOTO 550                                    PERSP081          
      YMAX=X2                                                           PERSP082          
550   CONTINUE                                                          PERSP083          
      IF(.NOT.(X2.LE.YMIN)) GOTO 570                                    PERSP084          
      YMIN=X2                                                           PERSP085          
570   CONTINUE                                                          PERSP086          
      I=I+1                                                             PERSP087          
      GOTO 471                                                          PERSP088          
479   CONTINUE                                                          PERSP089          
      R1=XMAX-XMIN                                                      PERSP090          
      R2=YMAX-YMIN                                                      PERSP091          
      J=J+1                                                             PERSP092          
      R(1,J)=R1                                                         PERSP093          
      R(2,J)=R2                                                         PERSP094          
      FM(J)=FMAX                                                        PERSP095          
      MAX(1,J)=XMAX                                                     PERSP096          
      MIN(1,J)=XMIN                                                     PERSP097          
      MAX(2,J)=YMAX                                                     PERSP098          
      MIN(2,J)=YMIN                                                     PERSP099          
      LP(J)=NEVAL-NOT+1                                                 PERSP100          
      IF(NOT.EQ.1) GOTO 740                                             PERSP101          
      NOT=NOT-INC                                                       PERSP102          
      II=II+1                                                           PERSP103          
      GOTO 401                                                          PERSP104          
409   CONTINUE                                                          PERSP105          
720   NOT=1                                                             PERSP106          
      GOTO 420                                                          PERSP107          
740   JJ=2                                                              PERSP108          
741   IF(JJ.GT.J) GOTO 749                                              PERSP109          
      CALL SQ(J-JJ+2)                                                   PERSP110          
      JJ=JJ+1                                                           PERSP111          
      GOTO 741                                                          PERSP112          
749   CONTINUE                                                          PERSP113          
      CALL PLOT(1.,1.,2)                                                PERSP114          
      CALL PLOT(0,0,1)                                                  PERSP115          
      IF(IB.EQ.2) GOTO 850                                              PERSP116          
      BB=FLOAT(NSQ)*(S+1.)                                              PERSP117          
      CALL PLOT(XSTEP,-BB,3)                                            PERSP118          
      ST=0                                                              PERSP119          
      J=1                                                               PERSP120          
      IB=IB+1                                                           PERSP121          
      GOTO 271                                                          PERSP122          
279   CONTINUE                                                          PERSP123          
850   CALL PLOT(-XSTEP,2.,3)                                            PERSP124          
      NN=NN+1                                                           PERSP125          
      GOTO 71                                                           PERSP126          
79    CONTINUE                                                          PERSP127          
      RETURN                                                            PERSP128          
      END                                                               PERSP129          
      SUBROUTINE SQ(JJ)                                                 PERSP130          
      REAL MAX,MIN                                                      PERSP131          
      DIMENSION XKN(10,300),Y(300),R(2,30),MAX(2,30),MIN(2,30),LP(30),  PERSP132          
     .FM(30)                                                            PERSP133          
      COMMON/GLASS/NEVAL,N,XKN,Y                                        PERSP134          
      COMMON/SZKLO/R,MAX,MIN,LP,FM,LR,S,N1,N2,D                         PERSP135          
      CALL PLOT(1.,1.,2)                                                PERSP136          
      CALL PLOT(0,-1.,1)                                                PERSP137          
      CALL PLOT(0,0,3)                                                  PERSP138          
      CALL PLOT(0,S,4)                                                  PERSP139          
      CALL PLOT(S,S,4)                                                  PERSP140          
      CALL PLOT(S,0,4)                                                  PERSP141          
      CALL PLOT(0,0,4)                                                  PERSP142          
      CALL PLOT(0,0,3)                                                  PERSP143          
      DIS=D/S                                                           PERSP144          
      SCALEX=R(1,JJ)*(1E0+2E0*DIS)/S                                    PERSP145          
      CALL PLOT(SCALEX,R(2,JJ)/S,2)                                     PERSP146          
      CALL PLOT((MIN(1,JJ)-SCALEX*D),MIN(2,JJ),1)                       PERSP147          
      KZ=NEVAL-LP(JJ)+1                                                 PERSP148          
      DISPEX=SCALEX*D*FLOAT(LR)/FM(JJ)                                  PERSP149          
      CALL PLOT((XKN(N1,KZ)+Y(KZ)*DISPEX),XKN(N2,KZ),3)                 PERSP150          
      I=KZ                                                              PERSP151          
171   IF(I.GT.NEVAL) GOTO 179                                           PERSP152          
      CALL PLOT((XKN(N1,I)+Y(I)*DISPEX),XKN(N2,I),4)                    PERSP153          
      I=I+1                                                             PERSP154          
      GOTO 171                                                          PERSP155          
179   CONTINUE                                                          PERSP156          
      CALL TEXT(1HX,0,1)                                                PERSP157          
      IF(JJ.EQ.2) GOTO 330                                              PERSP158          
      JK=JJ-1                                                           PERSP159          
      RA1=(1E0/(10E0*S))*R(1,JJ)                                        PERSP160          
      RA2=(1E0/(10E0*S))*R(2,JJ)                                        PERSP161          
      IF(.NOT.(R(1,JK).LT.RA1.AND.R(2,JK).LT.RA2)) GOTO 270             PERSP162          
      CALL TEXT(0,0,2)                                                  PERSP163          
      GOTO 330                                                          PERSP164          
270   CALL PLOT(MIN(1,JK),MIN(2,JK),3)                                  PERSP165          
      CALL PLOT(MAX(1,JK),MIN(2,JK),4)                                  PERSP166          
      CALL PLOT(MAX(1,JK),MAX(2,JK),4)                                  PERSP167          
      CALL PLOT(MIN(1,JK),MAX(2,JK),4)                                  PERSP168          
      CALL PLOT(MIN(1,JK),MIN(2,JK),4)                                  PERSP169          
      CALL PLOT(MIN(1,JK),MIN(2,JK),3)                                  PERSP170          
330   CALL PLOT((MIN(1,JJ)-SCALEX*D),MAX(2,JJ),3)                       PERSP171          
      RETURN                                                            PERSP172          
      END                                                               PERSP173          
